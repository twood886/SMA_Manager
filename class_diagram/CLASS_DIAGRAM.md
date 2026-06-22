# SMA Manager — Class Diagram

```mermaid
classDiagram
    direction TB

    class Security {
        +bbid_ : character
        +instrument_type_ : character
        +price_ : numeric
        +delta_ : numeric
        +underlying_security_ : Security
        +rule_data_ : list
        +get_price()
        +get_delta()
        +update_price()
    }

    class Holding {
        +id_ : character
        +sec_id_ : character
        +qty_ : numeric
        +swap_ : logical
        +custodian_ : character
        +custodian_acct_id_ : character
        +get_qty()
        +get_security_id()
    }

    class Position {
        +id_ : character
        +portfolio_short_name_ : character
        +security_ : Security
        +holdings_ : list~Holding~
        +get_security()
        +get_qty()
        +get_mkt_val()
        +get_delta_qty()
        +add_holding()
    }

    class DivisorProvider {
        +kind : character
        +weights_from_data()
        +value_from_data()
        +contrib_vec()
        +gamma()
        +expr()
    }

    class SMARule {
        +sma_name_ : character
        +name_ : character
        +scope_ : character
        +bbfields_ : character[]
        +max_threshold_ : numeric
        +min_threshold_ : numeric
        +swap_only_ : logical
        +relative_to_ : character
        +divisor_ : DivisorProvider
        +exclusions_ : character[]
        +apply_rule_definition()
        +security_impacted()
        +build_constraints()
        +objective_terms()
    }

    class SMARulePosition {
        +check_compliance()
        +get_security_limits()
        +build_constraints()
    }

    class SMARulePortfolio {
        +check_compliance()
        +get_security_limits()
        +build_constraints()
    }

    class SMARuleCount {
        +check_compliance()
        +get_security_limits()
        +build_constraints()
    }

    class VariableFactory {
        +cache : environment
        +vec()
        +scalar()
    }

    class ModelContext {
        +n : integer
        +ids : character[]
        +price : numeric[]
        +nav : numeric
        +t_w : numeric[]
        +w : CVXR~Variable~
        +alpha : CVXR~Parameter~
        +var_factory : VariableFactory
        +index_of()
        +metric()
    }

    class OverflowRule {
        +replacements_ : list
        +build_constraints()
        +objective_terms()
    }

    class TradeConstructor {
        +get_security_position_limits()
        +get_swap_flag_position_rules()
        +make_model_context()
        +optimize_sma()
    }

    class SMAConstructor {
        +calc_target_quantities()
    }

    class OrderConstructor {
        +pb_act_num_ : list
        +pb_act_sel_ : function
        +isda_act_sel_ : function
        +get_holdings()
        +get_long_holdings()
        +get_short_holdings()
        +sell()
    }

    class Portfolio {
        +long_name_ : character
        +short_name_ : character
        +nav_ : numeric
        +positions_ : list~Position~
        +rules_ : list~SMARule~
        +holdings_url_ : character
        +replacements_ : list
        +trade_constructor : TradeConstructor
        +order_constructor_ : OrderConstructor
        +get_position()
        +add_position()
        +add_rule()
        +get_rules()
        +get_trade_constructor()
        +rebalance()
    }

    class SMA {
        +base_portfolio_ : Portfolio
        +get_base_portfolio()
        +get_base_portfolio_position()
        +check_rule_compliance()
        +replicate_trade()
    }

    class Trade {
        +trade_id_ : numeric
        +security_id_ : character
        +total_qty_ : numeric
        +swap_ : logical
        +allocation_shares_ : list
        +allocation_pct_ : list
        +add_trade_qty()
        +remove_trade_qty()
        +allocate_trade()
        +to_df()
    }

    %% ── Inheritance ──────────────────────────────────────────────────────────
    SMARulePosition    --|> SMARule         : extends
    SMARulePortfolio   --|> SMARule         : extends
    SMARuleCount       --|> SMARule         : extends
    SMAConstructor     --|> TradeConstructor : extends
    SMA                --|> Portfolio        : extends

    %% ── Self-reference ───────────────────────────────────────────────────────
    Security           "1" ..o "0..1" Security : underlying_security

    %% ── Composition (strong ownership) ───────────────────────────────────────
    Position           "1"    *--  "1"    Security         : security_
    Position           "1"    *--  "1..*" Holding          : holdings_
    SMARule            "1"    *--  "1"    DivisorProvider  : divisor_
    ModelContext       "1"    *--  "1"    VariableFactory  : var_factory
    Portfolio          "1"    *--  "0..*" Position         : positions_
    Portfolio          "1"    *--  "0..*" SMARule          : rules_
    Portfolio          "1"    *--  "1"    TradeConstructor : trade_constructor

    %% ── Aggregation (referenced, not owned) ──────────────────────────────────
    Portfolio          "1"    o--  "0..1" OrderConstructor : order_constructor_
    SMA                "1"    o--  "1"    Portfolio        : base_portfolio_

    %% ── Dependencies (created transiently during method calls) ───────────────
    TradeConstructor   ..>    ModelContext  : creates in optimize_sma()
    TradeConstructor   ..>    OverflowRule  : creates in optimize_sma()
    TradeConstructor   ..>    Trade         : produces
```

## Legend

| Symbol | Meaning |
|--------|---------|
| `--|>` | Inheritance (is-a) |
| `*--`  | Composition (owns, lifecycle tied) |
| `o--`  | Aggregation (references, independent lifecycle) |
| `..>`  | Dependency (creates/uses transiently) |
| `..o`  | Optional self-reference |

## Quick Reference

| Class | Role |
|-------|------|
| **Security** | Leaf asset with price, delta, Bloomberg data |
| **Holding** | Single custodian lot of a security |
| **Position** | Aggregated view across all lots for one security |
| **Portfolio** | Base container: positions + rules + optimizer |
| **SMA** | Portfolio that tracks a base portfolio and enforces rules |
| **SMARule** | Constraint definition (position, portfolio, or count scope) |
| **DivisorProvider** | Denominator for rule weights (NAV / GMV / long GMV / short GMV) |
| **TradeConstructor** | Builds and runs the CVXR optimization |
| **SMAConstructor** | SMA-specific override of TradeConstructor |
| **ModelContext** | Optimization state: variables, prices, targets |
| **VariableFactory** | Cached CVXR variable allocator |
| **OverflowRule** | Redirects trades to replacement securities |
| **OrderConstructor** | Produces order allocations across broker accounts |
| **Trade** | Output: a security trade with portfolio allocations |
