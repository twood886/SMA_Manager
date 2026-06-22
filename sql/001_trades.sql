-- =============================================================================
-- Migration: trades table + holdings_actual + holdings_target views
-- Run in Supabase SQL Editor
-- =============================================================================

-- -----------------------------------------------------------------------------
-- 1. Trades table
--    One row per allocation leg (portfolio x security x custodian).
--    quantity_completed = Notional Quantity (signed fill to date)
--    quantity_target    = (allocation_pct / 100) * parent_total * sign(notional)
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS trades (
  trade_id                  BIGINT        PRIMARY KEY,
  portfolio_id              INTEGER       NOT NULL,
  trade_date                DATE          NOT NULL,
  txn_type                  TEXT,
  quantity_completed        NUMERIC       NOT NULL,
  quantity_target           NUMERIC       NOT NULL,
  custodian_acct_id         INTEGER,
  instrument_type           TEXT,
  description               TEXT,
  bb_yellow_key             TEXT,
  bb_yellow_key_underlying  TEXT,
  is_financed               BOOLEAN       DEFAULT FALSE,
  figi                      TEXT,
  cusip                     TEXT,
  trs_custodian_id          INTEGER,
  updated_at                TIMESTAMPTZ   DEFAULT now()
);

CREATE INDEX IF NOT EXISTS trades_date_portfolio
  ON trades (trade_date, portfolio_id);

-- -----------------------------------------------------------------------------
-- Helper macro: aggregates today's trade deltas using the requested qty column.
-- Used by both views below to avoid repeating the logic.
-- Joins to EOD holdings via (portfolio_id + lower(description) + custodian_acct_id)
-- to resolve instrument_id — avoids FIGI/yellow-key mismatch for bonds.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- 2. holdings_actual
--    EOD + completed intraday fills (quantity_completed).
--    Use for: compliance checking, current weights, position limits.
-- -----------------------------------------------------------------------------
CREATE OR REPLACE VIEW holdings_actual AS
WITH
eod AS (
  SELECT h.*
  FROM holdings h
  WHERE h.date = (SELECT MAX(date) FROM holdings)
),
-- Aggregate completed fills, resolve instrument_id via description match
trade_deltas AS (
  SELECT
    COALESCE(eod.instrument_id, s.instrument_id) AS instrument_id,
    t.portfolio_id,
    t.custodian_acct_id,
    t.instrument_type,
    t.description,
    t.is_financed,
    t.trs_custodian_id,
    SUM(CASE WHEN t.instrument_type = 'Listed Option'
             THEN t.quantity_completed ELSE 0 END) AS opt_delta,
    SUM(CASE WHEN t.instrument_type != 'Listed Option'
             THEN t.quantity_completed ELSE 0 END) AS stock_delta
  FROM trades t
  -- Primary: match to EOD holdings by description (both from Enfusion)
  LEFT JOIN eod
    ON  eod.portfolio_id                    = t.portfolio_id
    AND lower(eod.description)              = lower(t.description)
    AND COALESCE(eod.custodian_acct_id, -1) = COALESCE(t.custodian_acct_id, -1)
  -- Fallback for brand-new positions: match via securities table
  LEFT JOIN securities s
    ON  eod.instrument_id IS NULL
    AND lower(s.identifier) = lower(t.bb_yellow_key)
  WHERE t.trade_date = CURRENT_DATE
  GROUP BY COALESCE(eod.instrument_id, s.instrument_id),
           t.portfolio_id, t.custodian_acct_id,
           t.instrument_type, t.description,
           t.is_financed, t.trs_custodian_id
),
-- Existing EOD positions with intraday fills applied
updated AS (
  SELECT
    eod.date,
    eod.portfolio_id,
    eod.instrument_id,
    eod.custodian_acct_id,
    eod.instrument_type,
    eod.description,
    eod.bb_yellow_key,
    eod.bb_yellow_key_underlying,
    eod.figi,
    eod.cusip,
    eod.quantity_stock  + COALESCE(td.stock_delta, 0) AS quantity_stock,
    eod.quantity_option + COALESCE(td.opt_delta,   0) AS quantity_option,
    eod.is_financed,
    eod.trs_custodian_id
  FROM eod
  LEFT JOIN trade_deltas td
    ON  td.portfolio_id                     = eod.portfolio_id
    AND td.instrument_id                    = eod.instrument_id
    AND COALESCE(td.custodian_acct_id, -1)  = COALESCE(eod.custodian_acct_id, -1)
  -- Drop positions fully closed intraday
  WHERE eod.quantity_stock  + COALESCE(td.stock_delta, 0) != 0
     OR eod.quantity_option + COALESCE(td.opt_delta,   0) != 0
),
-- Brand-new positions opened intraday (no matching EOD row)
new_positions AS (
  SELECT
    CURRENT_DATE           AS date,
    td.portfolio_id,
    td.instrument_id,
    td.custodian_acct_id,
    td.instrument_type,
    td.description,
    NULL::TEXT             AS bb_yellow_key,
    NULL::TEXT             AS bb_yellow_key_underlying,
    NULL::TEXT             AS figi,
    NULL::TEXT             AS cusip,
    td.stock_delta         AS quantity_stock,
    td.opt_delta           AS quantity_option,
    td.is_financed,
    td.trs_custodian_id
  FROM trade_deltas td
  WHERE td.instrument_id IS NOT NULL
    AND (td.stock_delta != 0 OR td.opt_delta != 0)
    AND NOT EXISTS (
      SELECT 1 FROM eod
      WHERE eod.portfolio_id                    = td.portfolio_id
        AND eod.instrument_id                   = td.instrument_id
        AND COALESCE(eod.custodian_acct_id, -1) = COALESCE(td.custodian_acct_id, -1)
    )
)
SELECT * FROM updated
UNION ALL
SELECT * FROM new_positions;


-- -----------------------------------------------------------------------------
-- 3. holdings_target
--    EOD + full order targets (quantity_target).
--    Use for: replicate_trade, rebalancing — shows pro-forma positions
--    assuming the entire parent order fills.
-- -----------------------------------------------------------------------------
CREATE OR REPLACE VIEW holdings_target AS
WITH
eod AS (
  SELECT h.*
  FROM holdings h
  WHERE h.date = (SELECT MAX(date) FROM holdings)
),
trade_deltas AS (
  SELECT
    COALESCE(eod.instrument_id, s.instrument_id) AS instrument_id,
    t.portfolio_id,
    t.custodian_acct_id,
    t.instrument_type,
    t.description,
    t.is_financed,
    t.trs_custodian_id,
    SUM(CASE WHEN t.instrument_type = 'Listed Option'
             THEN t.quantity_target ELSE 0 END) AS opt_delta,
    SUM(CASE WHEN t.instrument_type != 'Listed Option'
             THEN t.quantity_target ELSE 0 END) AS stock_delta
  FROM trades t
  LEFT JOIN eod
    ON  eod.portfolio_id                    = t.portfolio_id
    AND lower(eod.description)              = lower(t.description)
    AND COALESCE(eod.custodian_acct_id, -1) = COALESCE(t.custodian_acct_id, -1)
  LEFT JOIN securities s
    ON  eod.instrument_id IS NULL
    AND lower(s.identifier) = lower(t.bb_yellow_key)
  WHERE t.trade_date = CURRENT_DATE
  GROUP BY COALESCE(eod.instrument_id, s.instrument_id),
           t.portfolio_id, t.custodian_acct_id,
           t.instrument_type, t.description,
           t.is_financed, t.trs_custodian_id
),
updated AS (
  SELECT
    eod.date,
    eod.portfolio_id,
    eod.instrument_id,
    eod.custodian_acct_id,
    eod.instrument_type,
    eod.description,
    eod.bb_yellow_key,
    eod.bb_yellow_key_underlying,
    eod.figi,
    eod.cusip,
    eod.quantity_stock  + COALESCE(td.stock_delta, 0) AS quantity_stock,
    eod.quantity_option + COALESCE(td.opt_delta,   0) AS quantity_option,
    eod.is_financed,
    eod.trs_custodian_id
  FROM eod
  LEFT JOIN trade_deltas td
    ON  td.portfolio_id                     = eod.portfolio_id
    AND td.instrument_id                    = eod.instrument_id
    AND COALESCE(td.custodian_acct_id, -1)  = COALESCE(eod.custodian_acct_id, -1)
  WHERE eod.quantity_stock  + COALESCE(td.stock_delta, 0) != 0
     OR eod.quantity_option + COALESCE(td.opt_delta,   0) != 0
),
new_positions AS (
  SELECT
    CURRENT_DATE           AS date,
    td.portfolio_id,
    td.instrument_id,
    td.custodian_acct_id,
    td.instrument_type,
    td.description,
    NULL::TEXT             AS bb_yellow_key,
    NULL::TEXT             AS bb_yellow_key_underlying,
    NULL::TEXT             AS figi,
    NULL::TEXT             AS cusip,
    td.stock_delta         AS quantity_stock,
    td.opt_delta           AS quantity_option,
    td.is_financed,
    td.trs_custodian_id
  FROM trade_deltas td
  WHERE td.instrument_id IS NOT NULL
    AND (td.stock_delta != 0 OR td.opt_delta != 0)
    AND NOT EXISTS (
      SELECT 1 FROM eod
      WHERE eod.portfolio_id                    = td.portfolio_id
        AND eod.instrument_id                   = td.instrument_id
        AND COALESCE(eod.custodian_acct_id, -1) = COALESCE(td.custodian_acct_id, -1)
    )
)
SELECT * FROM updated
UNION ALL
SELECT * FROM new_positions;
