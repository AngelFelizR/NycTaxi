#' Calculate Post‑hoc Statistical Power (t‑test or ANOVA)
#'
#' Estimates the power of an observed analysis based on the data.
#' Automatically chooses between a two‑sample t‑test and a one‑way ANOVA
#' depending on the number of groups.
#'
#' @param data A data.table.
#' @param outcome Character, name of the numeric outcome column.
#' @param group Character, name of the categorical grouping column.
#' @param use_min_n Logical; if TRUE, uses the smallest group size for power
#'   calculation; if FALSE (default), uses the mean group size.
#' @param sig_level Numeric, significance level (alpha). Default 0.05.
#' @return An object of class 'power.htest' containing the power calculation details.
#' @importFrom data.table is.data.table as.data.table
#' @export
compute_power <- function(
  data,
  outcome,
  group,
  use_min_n = FALSE,
  sig_level = 0.05
) {
  # 1. Initial checks
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  if (!outcome %chin% names(data) | !group %chin% names(data)) {
    stop("Specified columns do not exist in the dataset.")
  }

  # 2. Clean data (remove rows with NA in either variable)
  dt_clean <- data[
    !is.na(var1) & !is.na(var2),
    env = list(var1 = outcome, var2 = group)
  ]

  # 3. Descriptive statistics per group
  stats <- dt_clean[,
    .(
      mean = mean(.SD[[1]]),
      variance = var(.SD[[1]]),
      n = .N
    ),
    by = group,
    .SDcols = outcome
  ]

  k <- nrow(stats) # number of groups
  n_to_use <-
    if (use_min_n) {
      min(stats$n)
    } else {
      mean(stats$n)
    }

  # 4. Decision based on number of groups
  if (k < 2) {
    stop("At least 2 groups are required to compute power.")
  } else if (k == 2) {
    # --- Two‑sample t‑test case ---
    delta <- abs(stats$mean[1] - stats$mean[2])
    sd_pooled <- sqrt(
      sum((stats$n - 1) * stats$variance) / (sum(stats$n) - 2)
    )

    cat(">>> Running power.t.test (2 groups detected)\n")
    return(power.t.test(
      n = n_to_use,
      delta = delta,
      sd = sd_pooled,
      sig.level = sig_level,
      type = "two.sample"
    ))
  } else {
    # --- One‑way ANOVA case ---
    var_between <- var(stats$mean)
    var_within <- mean(stats$variance) # assumes equal variances

    cat(">>> Running power.anova.test (", k, " groups detected)\n")
    return(power.anova.test(
      groups = k,
      n = n_to_use,
      between.var = var_between,
      within.var = var_within,
      sig.level = sig_level
    ))
  }
}
