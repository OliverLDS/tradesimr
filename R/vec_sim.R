#' @keywords internal
.vec_factory <- function(func, ...) {
  function(DT) func(DT, ...)
}

#' @keywords internal
.warn_vec_sim_deprecated <- local({
  warned <- new.env(parent = emptyenv())
  function(name) {
    if (isTRUE(warned[[name]])) return(invisible(NULL))
    warned[[name]] <- TRUE
    warning(
      name,
      "() is a legacy approximate vectorized helper. Prefer sim_backtest() for stateful execution.",
      call. = FALSE
    )
  }
})

#' @keywords internal
.vec_sim_form_positions <- function(DT, mode = c("long", "short", "both")) {
  mode <- match.arg(mode)
  
  signal_shifted <- shift(DT[["signal"]], 1L)
  n <- nrow(DT)
  
  pos <- rep(NA_integer_, n)
  
  if (mode == "long") {
    pos[signal_shifted == -1L] <- 0L
    pos[signal_shifted ==  1L] <- 1L
  } else if (mode == "short") {
    pos[signal_shifted == -1L] <- -1L
    pos[signal_shifted ==  1L] <- 0L
  } else {
    pos[signal_shifted == -1L] <- -1L
    pos[signal_shifted ==  1L] <- 1L
  }
  
  pos[1L] <- 0L
  pos <- nafill(pos, type = "locf")
  
  data.table::set(DT, j = "pos", value = pos)
  data.table::setattr(DT, "position_mode", mode)
  DT
}

#' Run a vectorized approximate backtest
#'
#' Lightweight legacy helper that converts a precomputed position column into
#' log returns and an equity curve. It does not model the stateful exchange,
#' order, margin, funding, or liquidation mechanics used by `sim_backtest()`.
#'
#' @param DT A data.table containing at least `datetime`, `close`, and `pos`.
#' @return A list containing equity series and summary statistics.
#' @export
vec_sim_run_backtest <- function(DT) {
  .warn_vec_sim_deprecated("vec_sim_run_backtest")
  
  pos <- DT[["pos"]]
  close <- DT[["close"]]
  log_ret <- shift(pos) * log(close / shift(close))
  log_ret[is.na(log_ret)] <- 0
  data.table::set(DT, j='log_ret', value=log_ret) # assume the order based on previous bar and executed at the price of last close
  
  # Calculate cumulative equity
  data.table::set(DT, j='equity', value=exp(cumsum(log_ret)))

  # Summary
  bg_time <- min(DT$datetime)
  ed_time <- max(DT$datetime)
  total_days <- as.numeric(difftime(ed_time, bg_time, units = "days"))
  total_years <- total_days / 365.25
  last_equity <- tail(DT$equity, 1)
  ann_ret <- (last_equity)^(1 / total_years) - 1
  max_dd <- max(1 - DT$equity / cummax(DT$equity), na.rm = TRUE)

  list(
    inst_id = attr(DT, 'inst_id'),
    bar = attr(DT, 'bar'),
    strat_label = attr(DT, 'label'),
    position_mode = attr(DT, 'position_mode'),
    eq_series = DT[, .SD, .SDcols = c("datetime", "equity")],
    bg_time = bg_time,
    ed_time = ed_time,
    total_years = total_years,
    last_equity = last_equity,
    ann_ret = ann_ret,
    max_dd = max_dd
  )
}

#' Plot vectorized simulation results
#'
#' @param vec_sim_res Result from `vec_sim_run_backtest()`.
#' @param report_mode Plot report mode.
#' @export
vec_sim_gen_plot <- function(vec_sim_res, report_mode = c('simple', 'full')) {
  .warn_vec_sim_deprecated("vec_sim_gen_plot")
  
  report_mode <- match.arg(report_mode)
  
  main_title <- sprintf(
    "%s | %s",
    vec_sim_res$strat_label,
    vec_sim_res$position_mode
  )
  
  sub_title <- sprintf(
    '%s (%.2f%%, %.2f%%)', 
    vec_sim_res$inst_id, 
    vec_sim_res$ann_ret * 100, 
    vec_sim_res$max_dd * 100
  )
  
  par(mar = c(5.1, 4.1, 4.1, 2.1), oma = c(0, 0, 0, 0))
  plot(
    vec_sim_res$eq_series$datetime,
    vec_sim_res$eq_series$equity,
    type = "l", col = "blue", lwd = 2,
    main = main_title,
    cex.main = 0.8, 
    ylab = "", xlab = ""
  )
  mtext(sub_title, side = 3, line = 0.5, cex = 0.7)
}

#' Summarize vectorized simulation results
#'
#' @param vec_sim_res Result from `vec_sim_run_backtest()`.
#' @param report_mode Summary mode.
#' @return Text or a one-row data.table depending on `report_mode`.
#' @export
vec_sim_gen_summary <- function(vec_sim_res, report_mode = c('short_txt', 'long_txt', 'dt')) {
  .warn_vec_sim_deprecated("vec_sim_gen_summary")
  report_mode <- match.arg(report_mode)
  if (report_mode == 'short_txt') {
    sprintf("Ann Ret: %.2f%%; MoD: %.2f%%", vec_sim_res$ann_ret * 100, vec_sim_res$max_dd * 100)
  } else if (report_mode == 'long_txt') {
    sprintf(
      paste0(
        "%s | %s | %s\n",
        "Total Years: %.4f\n",
        "Final Equity: %.4f\n",
        "Annualized Return: %.2f%%\n",
        "Max Drawdown: %.2f%%\n"
      ),
      vec_sim_res$inst_id,
      vec_sim_res$bar,
      vec_sim_res$position_mode,
      vec_sim_res$total_years,
      vec_sim_res$last_equity,
      vec_sim_res$ann_ret * 100,
      vec_sim_res$max_dd * 100
    )
  } else {
    data.table::data.table(
      inst_id = vec_sim_res$inst_id, 
      bar = vec_sim_res$bar, 
      position_mode = vec_sim_res$position_mode, 
      strat_label = vec_sim_res$strat_label,
      bg_time = vec_sim_res$bg_time,
      ed_time = vec_sim_res$ed_time,
      ann_ret = vec_sim_res$ann_ret, 
      max_dd = vec_sim_res$max_dd
    )
  }
}

#' Summarize a batch of vectorized simulations
#'
#' @param DTs List of vectorized simulation data.tables.
#' @return A data.table of summaries.
#' @export
vec_sim_gen_summary_table <- function(DTs) {
  .warn_vec_sim_deprecated("vec_sim_gen_summary_table")
  table_list <- list()
  for (i in 1:length(DTs)) {
    DT <- DTs[[i]]
    vec_sim_res <- vec_sim_run_backtest(DT)
    table_list[[i]] <- vec_sim_gen_summary(vec_sim_res, 'dt')
  }
  data.table::rbindlist(table_list)
}

#' Generate vectorized simulation inputs for multiple instruments
#'
#' Legacy helper that loads candle data through strategyr and applies signal
#' strategies before forming long, short, and both-side position series.
#'
#' @param inst_ids Instrument identifiers.
#' @param bar Bar size passed to strategyr loaders.
#' @param signal_strategies Named list of functions that add a `signal` column.
#' @param root_path Candle data root path.
#' @param bg_time Optional begin time.
#' @param ed_time Optional end time.
#' @return A named list of data.tables.
#' @export
vec_batch_run_simulations <- function(inst_ids, bar, signal_strategies, root_path=Sys.getenv("OKX_Candle_Data_Path"), bg_time = NULL, ed_time = NULL) {
  .warn_vec_sim_deprecated("vec_batch_run_simulations")
  if (!requireNamespace("strategyr", quietly = TRUE)) {
    stop("Package `strategyr` is required for vec_batch_run_simulations().", call. = FALSE)
  }
  vec_load_candle_df <- get("vec_load_candle_df", envir = asNamespace("strategyr"))
  vec_prep_candle_dt <- get("vec_prep_candle_dt", envir = asNamespace("strategyr"))

  results <- list()

  for (i in seq_along(inst_ids)) {
    inst_id <- inst_ids[i]

    candle_df <- vec_load_candle_df(inst_id, bar, root_path)
    DT <- vec_prep_candle_dt(candle_df, bg_time, ed_time)

    for (strat_name in names(signal_strategies)) {
      vec_signal_strategy <- signal_strategies[[strat_name]]

      DT_signal <- vec_signal_strategy(copy(DT))

      for (mode in c("long", "short", "both")) {
        DT_sim <- .vec_sim_form_positions(copy(DT_signal), mode = mode)
        data.table::setattr(DT_sim, "inst_id", inst_id)
        data.table::setattr(DT_sim, "bar", bar)
        data.table::setattr(DT_sim, "strat_label", paste0(strat_name, "_", mode))
        results[[paste(inst_id, strat_name, mode, sep = "_")]] <- DT_sim
      }
    }
  }

  return(results)
}
