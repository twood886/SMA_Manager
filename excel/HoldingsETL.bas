Attribute VB_Name = "HoldingsETL"
Option Explicit

' =============================================================================
' HoldingsETL — Supabase + Enfusion Trades + Bloomberg Data Upload
'
' Setup:
'   1. Create a sheet named "Config" with:
'        A1 = Supabase email
'        A2 = Supabase password
'   2. Fill in SUPABASE_URL, ANON_KEY, and TRADES_URL constants below.
'      SUPABASE_URL / ANON_KEY: Supabase Dashboard > Settings > API
'   3. Call UpdateTrades() to push intraday fills.
'      Call UpdateBloombergData() to push Bloomberg field data.
' =============================================================================

Private Const SUPABASE_URL  As String = "https://YOUR-PROJECT-REF.supabase.co"
Private Const ANON_KEY      As String = "YOUR-ANON-KEY"
Private Const ENFUSION_HOST As String = "http://127.0.0.1:18443"
Private Const CONFIG_SHEET  As String = "Config"
Private Const TRADES_URL    As String = "https://webservices.enfusionsystems.com/mobile/rest/reportservice/exportReport?name=shared%2FTaylor%2FSMA_Mgr_Reports%2FSMA_Trade_Detail.trb"
Private Const BB_SHEET      As String = "BB_Staging"  ' hidden temp sheet for BDP formulas

' Module-level state persisted between Phase 1 and Phase 2 of Bloomberg update
' (Bloomberg BDP only calculates after VBA releases control — we must exit and re-enter)
Private g_bbToken       As String   ' Supabase JWT, valid ~1 hour
Private g_bbNSecs       As Long     ' number of securities with identifiers
Private g_bbNFields     As Long     ' number of fields
Private g_bbMaxAttempts As Long     ' how many times Phase 2 has been called
Private Const BB_MAX_RETRIES As Long = 20  ' max Phase 2 retries (20 x 3s = 60s total)

' =============================================================================
' Public entry point
' =============================================================================

Public Sub UpdateTrades()
    ' --- Credentials ----------------------------------------------------------
    Dim cfgSheet As Worksheet
    On Error Resume Next
    Set cfgSheet = ThisWorkbook.Sheets(CONFIG_SHEET)
    On Error GoTo 0
    If cfgSheet Is Nothing Then
        MsgBox "Could not find '" & CONFIG_SHEET & "' sheet.", vbExclamation
        Exit Sub
    End If

    Dim email As String, password As String
    email    = Trim(cfgSheet.Range("A1").Value)
    password = Trim(cfgSheet.Range("A2").Value)
    If Len(email) = 0 Or Len(password) = 0 Then
        MsgBox "Enter email (A1) and password (A2) in the Config sheet.", vbExclamation
        Exit Sub
    End If

    ' --- Authenticate ---------------------------------------------------------
    Dim token As String
    token = AuthenticateSupabase(email, password)
    If Len(token) = 0 Then Exit Sub

    ' --- Check Enfusion -------------------------------------------------------
    If Not IsEnfusionRunning() Then
        MsgBox "Enfusion is not running. Open Enfusion and try again.", vbExclamation
        Exit Sub
    End If

    ' --- Download trades report from Enfusion ---------------------------------
    Dim localUrl As String
    localUrl = Replace( _
        TRADES_URL, _
        "https://webservices.enfusionsystems.com/mobile/rest/reportservice/", _
        ENFUSION_HOST & "/" _
    )

    Dim csvText As String
    csvText = EnfusionGet(localUrl)
    If Len(csvText) = 0 Then
        MsgBox "Could not download trades from Enfusion.", vbExclamation
        Exit Sub
    End If

    ' --- Parse and upload -----------------------------------------------------
    Dim tradesJson As String
    tradesJson = ParseTradesReport(csvText)

    If tradesJson = "[]" Or Len(tradesJson) = 0 Then Exit Sub

    ' Upsert — Supabase will update quantity_completed on conflict with trade_id
    SupabaseUpsert token, "trades", tradesJson, "trade_id"

    ' Write upload stats to Config sheet
    Dim uploadCount As Long
    If tradesJson = "[]" Then
        uploadCount = 0
    Else
        uploadCount = UBound(SplitJsonArray(tradesJson)) + 1
    End If
    cfgSheet.Range("A5").Value = uploadCount
    cfgSheet.Range("B5").Value = Now()
    cfgSheet.Range("B5").NumberFormat = "YYYY-MM-DD HH:MM:SS"
End Sub

' =============================================================================
' Bloomberg data — Phase 1: write BDP formulas and exit so Bloomberg can calc.
' Phase 2 (UpdateBloombergData_Upload) is scheduled via Application.OnTime.
'
' Sheet layout of BB_SHEET after Phase 1:
'   Row 1  : "BBID" | field_id_1 | field_id_2 | ... | field_id_N | "INST_ID"
'   Row 2+ : bbid   | BDP formula or ""         | ... |            | instrument_id
' =============================================================================

Public Sub UpdateBloombergData()
    ' --- Credentials ----------------------------------------------------------
    Dim cfgSheet As Worksheet
    On Error Resume Next
    Set cfgSheet = ThisWorkbook.Sheets(CONFIG_SHEET)
    On Error GoTo 0
    If cfgSheet Is Nothing Then
        MsgBox "Could not find '" & CONFIG_SHEET & "' sheet.", vbExclamation
        Exit Sub
    End If

    Dim email As String, password As String
    email    = Trim(cfgSheet.Range("A1").Value)
    password = Trim(cfgSheet.Range("A2").Value)
    If Len(email) = 0 Or Len(password) = 0 Then
        MsgBox "Enter email (A1) and password (A2) in the Config sheet.", vbExclamation
        Exit Sub
    End If

    Dim token As String
    token = AuthenticateSupabase(email, password)
    If Len(token) = 0 Then Exit Sub

    ' --- Load fields from bb_field_catalog ------------------------------------
    Dim fieldsResp As String
    fieldsResp = SupabaseGet(token, "/rest/v1/bb_field_catalog?select=field_id,real_time")
    If Len(fieldsResp) = 0 Then
        MsgBox "Could not load field catalog from database.", vbExclamation
        Exit Sub
    End If
    Dim fieldObjs() As String
    fieldObjs = SplitJsonArray(fieldsResp)
    If UBound(fieldObjs) < 0 Then Exit Sub

    ' --- Load all securities (skip those without an identifier) ---------------
    Dim secsResp As String
    secsResp = SupabaseGet(token, "/rest/v1/securities?select=instrument_id,identifier")
    If Len(secsResp) = 0 Then
        MsgBox "Could not load securities from database.", vbExclamation
        Exit Sub
    End If
    Dim secObjs() As String
    secObjs = SplitJsonArray(secsResp)
    Dim nSecsAll As Long
    nSecsAll = UBound(secObjs) + 1
    If nSecsAll = 0 Then Exit Sub

    ' Filter to securities with a non-empty identifier
    Dim instIds() As Long
    Dim bbids()   As String
    ReDim instIds(nSecsAll - 1)
    ReDim bbids(nSecsAll - 1)
    Dim nSecs As Long: nSecs = 0
    Dim si As Long
    For si = 0 To nSecsAll - 1
        Dim tmpBbid As String
        tmpBbid = Trim(JsonGetString(secObjs(si), "identifier"))
        If Len(tmpBbid) = 0 Then GoTo SkipSec
        bbids(nSecs)   = tmpBbid
        instIds(nSecs) = JsonGetLong(secObjs(si), "instrument_id")
        nSecs = nSecs + 1
SkipSec:
    Next si
    If nSecs = 0 Then Exit Sub

    ' --- Set up staging sheet -------------------------------------------------
    Dim bbSheet As Worksheet
    On Error Resume Next
    Set bbSheet = ThisWorkbook.Sheets(BB_SHEET)
    On Error GoTo 0
    If bbSheet Is Nothing Then
        Set bbSheet = ThisWorkbook.Sheets.Add
        bbSheet.Name = BB_SHEET
    End If
    bbSheet.Visible = xlSheetVeryHidden
    bbSheet.Cells.Clear

    Dim todayStr As String
    todayStr = Format(Date, "YYYY-MM-DD")

    Dim nFields As Long
    nFields = UBound(fieldObjs) + 1
    Dim instIdCol As Long           ' column that stores instrument_id (after last field)
    instIdCol = nFields + 2

    ' Row 1 headers
    bbSheet.Cells(1, 1).Value          = "BBID"
    bbSheet.Cells(1, instIdCol).Value  = "INST_ID"

    ' Write bbid (col 1) and instrument_id (last col) — stable for all fields
    For si = 0 To nSecs - 1
        bbSheet.Cells(si + 2, 1).Value         = bbids(si)
        bbSheet.Cells(si + 2, instIdCol).Value = instIds(si)
    Next si

    ' --- Process each field: write header + BDP formulas ----------------------
    Dim fi As Long
    For fi = 0 To nFields - 1
        Dim fieldId  As String
        Dim realTime As Boolean
        fieldId  = JsonGetString(fieldObjs(fi), "field_id")
        realTime = (LCase(JsonGetString(fieldObjs(fi), "real_time")) = "true")
        If Len(fieldId) = 0 Then GoTo NextField

        ' For non-real-time fields: skip securities that already have data today
        Dim skipSecs() As Boolean
        ReDim skipSecs(nSecs - 1)   ' default False = needs update

        If Not realTime Then
            Dim existResp As String
            existResp = SupabaseGet(token, _
                "/rest/v1/security_data" & _
                "?select=instrument_id" & _
                "&field_id=eq." & fieldId & _
                "&date=eq." & todayStr)
            If Len(existResp) > 2 Then
                Dim existObjs() As String
                existObjs = SplitJsonArray(existResp)
                Dim ei As Long
                For ei = 0 To UBound(existObjs)
                    Dim existId As Long
                    existId = JsonGetLong(existObjs(ei), "instrument_id")
                    Dim k As Long
                    For k = 0 To nSecs - 1
                        If instIds(k) = existId Then
                            skipSecs(k) = True
                            Exit For
                        End If
                    Next k
                Next ei
            End If
        End If

        ' Write field header and BDP formulas (row = security index + 2)
        bbSheet.Cells(1, fi + 2).Value = fieldId
        For si = 0 To nSecs - 1
            If Not skipSecs(si) Then
                bbSheet.Cells(si + 2, fi + 2).Formula = _
                    "=BDP(""" & bbids(si) & """,""" & fieldId & """)"
            End If
        Next si

NextField:
    Next fi

    ' --- Store state for Phase 2 and schedule it ------------------------------
    g_bbToken       = token
    g_bbNSecs       = nSecs
    g_bbNFields     = nFields
    g_bbMaxAttempts = 0

    ' Trigger Bloomberg recalculation, then hand control back to Excel.
    ' Bloomberg will populate BDP cells only after VBA is no longer running.
    Application.CalculateFull
    Application.OnTime Now + TimeValue("00:00:05"), "UpdateBloombergData_Upload"
End Sub

' =============================================================================
' Bloomberg data — Phase 2: read BDP values and upload to Supabase.
' Called by Application.OnTime after Bloomberg has had time to calculate.
' Retries up to BB_MAX_RETRIES times (every 3 seconds) if data is still loading.
' =============================================================================

Public Sub UpdateBloombergData_Upload()
    g_bbMaxAttempts = g_bbMaxAttempts + 1

    Dim bbSheet As Worksheet
    On Error Resume Next
    Set bbSheet = ThisWorkbook.Sheets(BB_SHEET)
    On Error GoTo 0
    If bbSheet Is Nothing Then
        MsgBox "BB_Staging sheet not found — Bloomberg upload aborted.", vbExclamation
        Exit Sub
    End If

    Dim nSecs   As Long: nSecs   = g_bbNSecs
    Dim nFields As Long: nFields = g_bbNFields
    Dim instIdCol As Long: instIdCol = nFields + 2

    ' --- Check if Bloomberg is still loading ----------------------------------
    ' Bloomberg returns "#N/A Requesting Data..." as a string before data arrives,
    ' and may also return proper Excel error values. Check both.
    Dim stillLoading As Boolean
    stillLoading = False
    Dim checkR As Long, checkC As Long
    For checkR = 2 To nSecs + 1
        For checkC = 2 To nFields + 1
            ' Only check cells that have a formula (non-empty means BDP was written)
            If bbSheet.Cells(checkR, checkC).HasFormula Then
                Dim cv As Variant
                cv = bbSheet.Cells(checkR, checkC).Value
                If IsError(cv) Then
                    stillLoading = True
                ElseIf VarType(cv) = vbString Then
                    Dim cvStr As String
                    cvStr = CStr(cv)
                    If InStr(cvStr, "Requesting Data") > 0 Or _
                       InStr(cvStr, "#N/A") > 0 Or _
                       InStr(cvStr, "N/A") > 0 Then
                        stillLoading = True
                    End If
                End If
                If stillLoading Then Exit For
            End If
        Next checkC
        If stillLoading Then Exit For
    Next checkR

    ' Retry if not ready yet
    If stillLoading Then
        If g_bbMaxAttempts >= BB_MAX_RETRIES Then
            MsgBox "Bloomberg data did not load after " & BB_MAX_RETRIES & " retries. " & _
                   "Check that Bloomberg Terminal is open and connected.", vbExclamation
            bbSheet.Cells.Clear
        Else
            Application.OnTime Now + TimeValue("00:00:03"), "UpdateBloombergData_Upload"
        End If
        Exit Sub
    End If

    ' --- All cells settled — read values and build JSON -----------------------
    Dim todayStr As String
    todayStr = Format(Date, "YYYY-MM-DD")
    Dim nowStr As String
    nowStr = Format(Now(), "YYYY-MM-DD") & "T" & Format(Now(), "HH:MM:SS") & "Z"

    Dim allRows() As String
    ReDim allRows(0)
    Dim totalRows As Long: totalRows = 0

    Dim fi As Long
    For fi = 0 To nFields - 1
        Dim fieldId As String
        fieldId = Trim(CStr(bbSheet.Cells(1, fi + 2).Value))
        If Len(fieldId) = 0 Then GoTo NextField2

        Dim si As Long
        For si = 0 To nSecs - 1
            ' Skip cells that have no formula (non-real-time field already had data)
            If Not bbSheet.Cells(si + 2, fi + 2).HasFormula Then GoTo NextSec

            Dim cellVal As Variant
            cellVal = bbSheet.Cells(si + 2, fi + 2).Value
            If IsError(cellVal) Or IsEmpty(cellVal) Then GoTo NextSec
            If VarType(cellVal) = vbString Then
                If InStr(CStr(cellVal), "N/A") > 0 Then GoTo NextSec
            End If

            Dim instId As Long
            instId = CLng(bbSheet.Cells(si + 2, instIdCol).Value)
            If instId = 0 Then GoTo NextSec

            ' Determine value type
            Dim valText    As String: valText = "null"
            Dim valNumeric As String: valNumeric = "null"
            Dim valBool    As String: valBool = "null"

            If VarType(cellVal) = vbBoolean Then
                valBool = LCase(CStr(cellVal))
            ElseIf IsNumeric(cellVal) Then
                valNumeric = CStr(CDbl(cellVal))
            Else
                valText = """" & JsonEsc(CStr(cellVal)) & """"
            End If

            Dim jsonRow As String
            jsonRow = "{"
            jsonRow = jsonRow & """instrument_id"":" & instId
            jsonRow = jsonRow & ",""field_id"":""" & fieldId & """"
            jsonRow = jsonRow & ",""date"":""" & todayStr & """"
            jsonRow = jsonRow & ",""value_text"":" & valText
            jsonRow = jsonRow & ",""value_numeric"":" & valNumeric
            jsonRow = jsonRow & ",""value_boolean"":" & valBool
            jsonRow = jsonRow & ",""updated_at"":""" & nowStr & """"
            jsonRow = jsonRow & "}"

            ReDim Preserve allRows(totalRows)
            allRows(totalRows) = jsonRow
            totalRows = totalRows + 1
NextSec:
        Next si
NextField2:
    Next fi

    ' --- Upload in batches of 500 ---------------------------------------------
    Dim batchSize As Long: batchSize = 500
    Dim b As Long
    For b = 0 To totalRows - 1 Step batchSize
        Dim bEnd As Long
        bEnd = b + batchSize - 1
        If bEnd > totalRows - 1 Then bEnd = totalRows - 1

        Dim batchArr() As String
        ReDim batchArr(bEnd - b)
        Dim bi As Long
        For bi = 0 To bEnd - b
            batchArr(bi) = allRows(b + bi)
        Next bi

        SupabaseUpsert g_bbToken, "security_data", "[" & Join(batchArr, ",") & "]", "instrument_id,field_id,date"
    Next b

    ' Clean up
    bbSheet.Cells.Clear
    g_bbToken = ""

    ' Write stats to Config sheet
    Dim cfgSheet As Worksheet
    On Error Resume Next
    Set cfgSheet = ThisWorkbook.Sheets(CONFIG_SHEET)
    On Error GoTo 0
    If Not cfgSheet Is Nothing Then
        cfgSheet.Range("A6").Value        = totalRows
        cfgSheet.Range("B6").Value        = Now()
        cfgSheet.Range("B6").NumberFormat = "YYYY-MM-DD HH:MM:SS"
    End If
End Sub

' =============================================================================
' Parse trades CSV into a JSON array for Supabase
' =============================================================================

Private Function ParseTradesReport(csvText As String) As String
    Dim lines() As String
    lines = Split(Replace(csvText, vbCr, ""), vbLf)
    If UBound(lines) < 1 Then ParseTradesReport = "[]": Exit Function

    Dim headers() As String
    headers = ParseCsvRow(lines(0))

    ' Column indices
    Dim cTradeId    As Integer: cTradeId    = ColIndex(headers, "Trade Id")
    Dim cInstId     As Integer: cInstId     = ColIndex(headers, "Instrument Id")
    Dim cUndInstId  As Integer: cUndInstId  = ColIndex(headers, "Underlying Instrument Id")
    Dim cUndInstTyp As Integer: cUndInstTyp = ColIndex(headers, "Underlying Instrument Type")
    Dim cUndDesc    As Integer: cUndDesc    = ColIndex(headers, "Underlying Description")
    Dim cUndCusip   As Integer: cUndCusip   = ColIndex(headers, "Underlying CUSIP")
    Dim cPortId     As Integer: cPortId     = ColIndex(headers, "Portfolio Id")
    Dim cTradeDate As Integer: cTradeDate = ColIndex(headers, "Trade Date")
    Dim cTxnType   As Integer: cTxnType   = ColIndex(headers, "Txn Type")
    Dim cNotional  As Integer: cNotional  = ColIndex(headers, "Notional Quantity")
    Dim cAllocPct  As Integer: cAllocPct  = ColIndex(headers, "Trade Allocation Percent")
    Dim cParentQty As Integer: cParentQty = ColIndex(headers, "Parent Total Quantity")
    Dim cCustId    As Integer: cCustId    = ColIndex(headers, "Custodian Acct Id")
    Dim cType      As Integer: cType      = ColIndex(headers, "Order Instrument Type")
    Dim cDesc      As Integer: cDesc      = ColIndex(headers, "Description")
    Dim cYK        As Integer: cYK        = ColIndex(headers, "BB Yellow Key")
    Dim cYKUnd     As Integer: cYKUnd     = ColIndex(headers, "Underlying BB Yellow Key")
    Dim cFinanced  As Integer: cFinanced  = ColIndex(headers, "Is Financed")
    Dim cFigi      As Integer: cFigi      = ColIndex(headers, "FIGI")
    Dim cCusip     As Integer: cCusip     = ColIndex(headers, "CUSIP")
    Dim cTrsCust   As Integer: cTrsCust   = ColIndex(headers, "TRS Custodian ID")

    Dim nowStr As String
    nowStr = Format(Now(), "YYYY-MM-DD") & "T" & Format(Now(), "HH:MM:SS") & "Z"

    Dim rows() As String
    ReDim rows(UBound(lines))
    Dim rowCount As Integer: rowCount = 0

    Dim r As Integer
    For r = 1 To UBound(lines)
        Dim line As String
        line = Trim(lines(r))
        If Len(line) = 0 Then GoTo NextRow

        Dim cols() As String
        cols = ParseCsvRow(line)

        ' Skip summary/blank rows — Trade Id is empty on those
        Dim tradeId As String
        tradeId = SafeCol(cols, cTradeId)
        If Len(tradeId) = 0 Or Not IsNumeric(tradeId) Then GoTo NextRow

        ' quantity_completed = Notional Quantity (already signed)
        Dim qtyCompleted As String
        qtyCompleted = SafeNum(SafeCol(cols, cNotional))
        If qtyCompleted = "null" Then GoTo NextRow

        ' quantity_target = sign(notional) * parent_total * (alloc_pct / 100)
        Dim allocPctRaw As String
        Dim parentQty   As String
        allocPctRaw = Replace(SafeCol(cols, cAllocPct), "%", "")
        parentQty   = SafeCol(cols, cParentQty)
        Dim qtyTarget As String
        If IsNumeric(allocPctRaw) And IsNumeric(parentQty) And IsNumeric(qtyCompleted) Then
            Dim sgnVal As Integer
            sgnVal = IIf(CDbl(qtyCompleted) >= 0, 1, -1)
            qtyTarget = CStr(sgnVal * CDbl(parentQty) * (CDbl(allocPctRaw) / 100))
        Else
            qtyTarget = qtyCompleted
        End If

        ' Trade date
        Dim tradeDateRaw As String
        Dim tradeDateStr As String
        tradeDateRaw = SafeCol(cols, cTradeDate)
        On Error Resume Next
        tradeDateStr = IIf(Len(tradeDateRaw) > 0, Format(CDate(tradeDateRaw), "YYYY-MM-DD"), Format(Date, "YYYY-MM-DD"))
        If Err.Number <> 0 Then tradeDateStr = Format(Date, "YYYY-MM-DD")
        On Error GoTo 0

        Dim j As String
        j = "{"
        j = j & """trade_id"":" & tradeId
        j = j & ",""instrument_id"":" & SafeNum(SafeCol(cols, cInstId))
        j = j & ",""portfolio_id"":" & SafeNum(SafeCol(cols, cPortId))
        j = j & ",""trade_date"":""" & tradeDateStr & """"
        j = j & ",""txn_type"":" & JsonNull(SafeCol(cols, cTxnType))
        j = j & ",""quantity_completed"":" & qtyCompleted
        j = j & ",""quantity_target"":" & qtyTarget
        j = j & ",""custodian_acct_id"":" & SafeNum(SafeCol(cols, cCustId))
        j = j & ",""instrument_type"":" & JsonNull(SafeCol(cols, cType))
        j = j & ",""description"":" & JsonNull(SafeCol(cols, cDesc))
        j = j & ",""bb_yellow_key"":" & JsonNull(SafeCol(cols, cYK))
        j = j & ",""bb_yellow_key_underlying"":" & JsonNull(SafeCol(cols, cYKUnd))
        j = j & ",""is_financed"":" & SafeBool(SafeCol(cols, cFinanced))
        j = j & ",""figi"":" & JsonNull(SafeCol(cols, cFigi))
        j = j & ",""cusip"":" & JsonNull(SafeCol(cols, cCusip))
        j = j & ",""underlying_instrument_id"":" & SafeNum(SafeCol(cols, cUndInstId))
        j = j & ",""underlying_instrument_type"":" & JsonNull(SafeCol(cols, cUndInstTyp))
        j = j & ",""underlying_description"":" & JsonNull(SafeCol(cols, cUndDesc))
        j = j & ",""underlying_cusip"":" & JsonNull(SafeCol(cols, cUndCusip))
        j = j & ",""trs_custodian_id"":" & SafeNum(SafeCol(cols, cTrsCust))
        j = j & ",""updated_at"":""" & nowStr & """"
        j = j & "}"

        rows(rowCount) = j
        rowCount = rowCount + 1
NextRow:
    Next r

    If rowCount = 0 Then
        ParseTradesReport = "[]"
    Else
        ReDim Preserve rows(rowCount - 1)
        ParseTradesReport = "[" & Join(rows, ",") & "]"
    End If
End Function

' =============================================================================
' Authentication
' =============================================================================

Private Function AuthenticateSupabase(email As String, password As String) As String
    Dim body As String
    body = "{""email"":""" & email & """,""password"":""" & password & """}"

    Dim resp As String
    resp = HttpPost( _
        SUPABASE_URL & "/auth/v1/token?grant_type=password", _
        "apikey: " & ANON_KEY & "|Content-Type: application/json", _
        body _
    )

    If InStr(resp, "access_token") = 0 Then
        MsgBox "Login failed. Check credentials in Config sheet." & vbLf & _
               IIf(InStr(resp, "message") > 0, JsonGetString(resp, "message"), ""), _
               vbExclamation
        AuthenticateSupabase = ""
    Else
        AuthenticateSupabase = JsonGetString(resp, "access_token")
    End If
End Function

' =============================================================================
' Enfusion
' =============================================================================

Private Function IsEnfusionRunning() As Boolean
    On Error GoTo Fail
    Dim http As Object
    Set http = CreateObject("WinHttp.WinHttpRequest.5.1")
    http.Open "GET", ENFUSION_HOST & "/exportReport", False
    http.setTimeouts 5000, 5000, 10000, 10000
    http.send
    ' Any HTTP response (200, 400, etc.) means Enfusion is running
    IsEnfusionRunning = (http.Status > 0)
    Exit Function
Fail:
    IsEnfusionRunning = False
End Function

Private Function EnfusionGet(url As String) As String
    On Error GoTo Fail
    Dim http As Object
    Set http = CreateObject("WinHttp.WinHttpRequest.5.1")
    http.Open "GET", url, False
    http.setTimeouts 5000, 5000, 30000, 30000
    http.send
    If http.Status = 200 Then EnfusionGet = http.responseText
    Exit Function
Fail:
    EnfusionGet = ""
End Function

' =============================================================================
' HTTP helpers
' =============================================================================

Private Function SupabaseGet(token As String, path As String) As String
    On Error GoTo Fail
    Dim http As Object
    Set http = CreateObject("WinHttp.WinHttpRequest.5.1")
    http.Open "GET", SUPABASE_URL & path, False
    http.setRequestHeader "Authorization", "Bearer " & token
    http.setRequestHeader "apikey", ANON_KEY
    http.setRequestHeader "Accept", "application/json"
    http.send
    If http.Status = 200 Then SupabaseGet = http.responseText
    Exit Function
Fail:
    SupabaseGet = ""
End Function

Private Sub SupabasePost(token As String, tableName As String, jsonBody As String)
    On Error Resume Next
    Dim http As Object
    Set http = CreateObject("WinHttp.WinHttpRequest.5.1")
    http.Open "POST", SUPABASE_URL & "/rest/v1/" & tableName, False
    http.setRequestHeader "Authorization", "Bearer " & token
    http.setRequestHeader "apikey", ANON_KEY
    http.setRequestHeader "Content-Type", "application/json"
    http.setRequestHeader "Prefer", "return=minimal"
    http.send jsonBody
End Sub

' Upsert: insert rows, update on conflict with the given primary key column
Private Sub SupabaseUpsert(token As String, tableName As String, jsonBody As String, pkColumn As String)
    On Error Resume Next
    Dim http As Object
    Set http = CreateObject("WinHttp.WinHttpRequest.5.1")
    http.Open "POST", SUPABASE_URL & "/rest/v1/" & tableName, False
    http.setRequestHeader "Authorization", "Bearer " & token
    http.setRequestHeader "apikey", ANON_KEY
    http.setRequestHeader "Content-Type", "application/json"
    http.setRequestHeader "Prefer", "resolution=merge-duplicates,return=minimal"
    http.send jsonBody
End Sub

Private Sub SupabaseDelete(token As String, tableName As String, filterQs As String)
    On Error Resume Next
    Dim http As Object
    Set http = CreateObject("WinHttp.WinHttpRequest.5.1")
    http.Open "DELETE", SUPABASE_URL & "/rest/v1/" & tableName & "?" & filterQs, False
    http.setRequestHeader "Authorization", "Bearer " & token
    http.setRequestHeader "apikey", ANON_KEY
    http.send
End Sub

Private Function HttpPost(url As String, headers As String, body As String) As String
    On Error GoTo Fail
    Dim http As Object
    Set http = CreateObject("WinHttp.WinHttpRequest.5.1")
    http.Open "POST", url, False
    Dim pairs() As String
    pairs = Split(headers, "|")
    Dim p As Integer
    For p = 0 To UBound(pairs)
        Dim kv() As String
        kv = Split(pairs(p), ": ", 2)
        If UBound(kv) = 1 Then http.setRequestHeader kv(0), kv(1)
    Next p
    http.send body
    HttpPost = http.responseText
    Exit Function
Fail:
    HttpPost = ""
End Function

' =============================================================================
' JSON helpers
' =============================================================================

Private Function JsonGetString(json As String, key As String) As String
    Dim re As Object
    Set re = CreateObject("VBScript.RegExp")
    re.Pattern = """" & key & """\s*:\s*""([^""]*)"""
    Dim m As Object
    Set m = re.Execute(json)
    If m.Count > 0 Then JsonGetString = m(0).SubMatches(0)
End Function

Private Function JsonGetLong(json As String, key As String) As Long
    Dim re As Object
    Set re = CreateObject("VBScript.RegExp")
    re.Pattern = """" & key & """\s*:\s*(\d+)"
    Dim m As Object
    Set m = re.Execute(json)
    If m.Count > 0 Then JsonGetLong = CLng(m(0).SubMatches(0))
End Function

Private Function SplitJsonArray(json As String) As String()
    json = Trim(json)
    If Left(json, 1) = "[" Then json = Mid(json, 2)
    If Right(json, 1) = "]" Then json = Left(json, Len(json) - 1)

    Dim result() As String
    ReDim result(0)
    Dim count   As Long: count = 0
    Dim depth   As Long: depth = 0
    Dim start   As Long: start = 1
    Dim inQuoteJ As Boolean: inQuoteJ = False
    Dim i As Long

    For i = 1 To Len(json)
        Dim c As String
        c = Mid(json, i, 1)
        Dim prevChar As String
        If i = 1 Then
            prevChar = ""
        Else
            prevChar = Mid(json, i - 1, 1)
        End If
        If c = """" And prevChar <> "\" Then
            inQuoteJ = Not inQuoteJ
        ElseIf Not inQuoteJ Then
            If c = "{" Then
                If depth = 0 Then start = i
                depth = depth + 1
            ElseIf c = "}" Then
                depth = depth - 1
                If depth = 0 Then
                    ReDim Preserve result(count)
                    result(count) = Mid(json, start, i - start + 1)
                    count = count + 1
                End If
            End If
        End If
    Next i

    If count = 0 Then ReDim result(0)
    SplitJsonArray = result
End Function

' =============================================================================
' CSV helpers
' =============================================================================

Private Function ParseCsvRow(line As String) As String()
    Dim result() As String
    ReDim result(0)
    Dim count   As Integer: count = 0
    Dim inQuote As Boolean: inQuote = False
    Dim field   As String:  field = ""
    Dim i As Integer

    For i = 1 To Len(line)
        Dim ch As String
        ch = Mid(line, i, 1)
        If ch = """" Then
            If inQuote And Mid(line, i + 1, 1) = """" Then
                field = field & """"
                i = i + 1
            Else
                inQuote = Not inQuote
            End If
        ElseIf ch = "," And Not inQuote Then
            ReDim Preserve result(count)
            result(count) = field
            count = count + 1
            field = ""
        Else
            field = field & ch
        End If
    Next i
    ReDim Preserve result(count)
    result(count) = field
    ParseCsvRow = result
End Function

Private Function ColIndex(headers() As String, colName As String) As Integer
    Dim i As Integer
    For i = 0 To UBound(headers)
        If Trim(headers(i)) = colName Then ColIndex = i: Exit Function
    Next i
    ColIndex = -1
End Function

Private Function SafeCol(cols() As String, idx As Integer) As String
    If idx < 0 Or idx > UBound(cols) Then SafeCol = "" Else SafeCol = Trim(cols(idx))
End Function

Private Function SafeNum(val As String) As String
    val = Trim(val)
    If Len(val) = 0 Or Not IsNumeric(val) Then SafeNum = "null" Else SafeNum = val
End Function

Private Function SafeBool(val As String) As String
    Select Case LCase(Trim(val))
        Case "true", "1", "yes": SafeBool = "true"
        Case Else:                SafeBool = "false"
    End Select
End Function

Private Function JsonNull(val As String) As String
    val = Trim(val)
    If Len(val) = 0 Then JsonNull = "null" Else JsonNull = """" & JsonEsc(val) & """"
End Function

Private Function JsonEsc(val As String) As String
    val = Replace(val, "\", "\\")
    val = Replace(val, """", "\""")
    val = Replace(val, vbCr, "")
    val = Replace(val, vbLf, "")
    val = Replace(val, vbTab, " ")
    JsonEsc = val
End Function
