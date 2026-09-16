#' Define a portfolio decision policy
#'
#' A policy controls which completed market observations may create a
#' target-weight decision. It never changes execution eligibility: every order
#' still fills only on a strictly later, tradable bar for its own asset.
#'
#' @param mode Decision policy. `"complete_universe"` requires a fresh,
#'   tradable completed bar for every allowed asset. `"as_of_valuation"`
#'   permits carried marks for absent assets, subject to `max_staleness`.
#'   `"per_asset_decision"` permits partial decisions, but every named target
#'   must have a fresh, tradable completed bar in the submitted batch.
#' @param max_staleness Maximum age in seconds of a carried valuation for
#'   `"as_of_valuation"`. Defaults to `Inf`; use a finite value in production.
#' @return A validated decision-policy list.
#' @export
sim_portfolio_decision_policy <- function(mode = c(
                                          "complete_universe",
                                          "as_of_valuation",
                                          "per_asset_decision"
                                        ),
                                        max_staleness = Inf) {
  mode <- match.arg(mode)
  max_staleness <- as.numeric(max_staleness)
  if (length(max_staleness) != 1L || is.na(max_staleness) || max_staleness < 0) {
    stop("`max_staleness` must be one non-negative number of seconds or `Inf`.", call. = FALSE)
  }
  list(mode = mode, max_staleness = max_staleness)
}

#' @keywords internal
.portfolio_validate_decision_policy <- function(policy) {
  if (is.null(policy)) return(sim_portfolio_decision_policy())
  if (!is.list(policy)) stop("`decision_policy` must be created by sim_portfolio_decision_policy().", call. = FALSE)
  sim_portfolio_decision_policy(
    mode = as.character(policy$mode %||% "complete_universe"),
    max_staleness = policy$max_staleness %||% Inf
  )
}
