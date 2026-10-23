# Reproducible analysis for the CRPI manuscript
#
# This script reproduces the conventional RT/accuracy analyses and the
# pooled-trial assessment-function analysis reported in the manuscript.
# Run it from the repository root, for example:
#
#   Rscript "analysis code/manuscript_reproducible_analysis.R"
#
# Required packages: dplyr, tidyr, ggplot2, afex, sft
# The default bootstrap size is 10,000. For a quick code check only, set the
# environment variable BOOT_B to a smaller integer before running.

options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(afex)
  library(sft)
})

## 1. Configuration ---------------------------------------------------------

INPUT_FILE <- "analysis code/vst_raw_combined.csv"
OUTPUT_DIR <- "output/manuscript_reproducible_analysis"
SEED <- 42L
BOOT_B <- suppressWarnings(as.integer(Sys.getenv("BOOT_B", "10000")))
if (!is.finite(BOOT_B) || BOOT_B < 100L) stop("BOOT_B must be at least 100.")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

write_csv <- function(x, name) {
  write.csv(x, file.path(OUTPUT_DIR, name), row.names = FALSE)
}

## 2. Import and variable definitions --------------------------------------

raw <- read.csv(INPUT_FILE, check.names = FALSE)
expected_names <- c("Group", "Condition", "Resp_Subject", "Target", "Trigger",
                    "XX", "XXX", "FixTime", "Response", "RT", "Correct")
stopifnot(ncol(raw) == length(expected_names))
names(raw) <- expected_names

raw <- raw %>%
  mutate(across(everything(), as.numeric)) %>%
  filter(Group != 3, is.finite(RT), RT > 0) %>%
  mutate(
    SportType = ifelse(Group <= 10 | Group >= 30, "Individual", "Team"),
    DecisionCondition = ifelse(Condition <= 20,
                               "Non-collaborative", "Collaborative"),
    Task = DecisionCondition,
    Channel = ifelse(Task == "Collaborative", 1L,
                     ifelse(Resp_Subject == 1, 2L, 3L)),
    Person = paste(Group, Resp_Subject, sep = "_")
  )

stopifnot(nrow(raw) == 34800L,
          n_distinct(raw$Group) == 30L,
          all(raw$Correct %in% 0:1),
          all(raw$Channel %in% 1:3))

## 3. Conventional mean RT and accuracy analyses ---------------------------
#
# These analyses use the complete valid trial dataset, matching the numerical
# results currently reported in the manuscript. RT is averaged over correct
# trials; accuracy is averaged over all trials. The unit of analysis is dyad.

dyad_condition <- raw %>%
  group_by(Group, SportType, DecisionCondition) %>%
  summarise(
    mean_correct_rt = mean(RT[Correct == 1], na.rm = TRUE),
    accuracy = mean(Correct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Group = factor(Group),
    SportType = factor(SportType, levels = c("Individual", "Team")),
    DecisionCondition = factor(
      DecisionCondition,
      levels = c("Non-collaborative", "Collaborative")
    )
  )

group_descriptives <- dyad_condition %>%
  group_by(SportType, DecisionCondition) %>%
  summarise(
    n_dyads = n(),
    mean_rt = mean(mean_correct_rt),
    sd_rt = sd(mean_correct_rt),
    se_rt = sd_rt / sqrt(n_dyads),
    mean_accuracy = mean(accuracy),
    sd_accuracy = sd(accuracy),
    se_accuracy = sd_accuracy / sqrt(n_dyads),
    .groups = "drop"
  )
write_csv(group_descriptives, "table2_rt_accuracy_descriptives.csv")

run_2x2 <- function(dv) {
  fit <- afex::aov_ez(
    id = "Group", dv = dv, data = dyad_condition,
    between = "SportType", within = "DecisionCondition", type = 3,
    anova_table = list(es = "pes")
  )
  tab <- as.data.frame(afex::nice(fit, es = "pes"))
  tab$Outcome <- dv
  list(fit = fit, table = tab)
}

rt_anova <- run_2x2("mean_correct_rt")
accuracy_anova <- run_2x2("accuracy")
write_csv(bind_rows(rt_anova$table, accuracy_anova$table),
          "anova_sport_by_condition.csv")

## 4. Counterbalancing-order sensitivity analysis --------------------------
#
# Assignment alternated within each sport cohort. In the archived group codes,
# collaborative-first dyads are even-numbered Individual dyads and odd-numbered
# Team dyads. Reversing both labels changes no F test.

dyad_order <- dyad_condition %>%
  mutate(
    group_number = as.integer(as.character(Group)),
    CollaborativeFirst = xor(group_number %% 2 == 0, SportType == "Team"),
    Order = factor(ifelse(CollaborativeFirst,
                          "Collaborative first", "Non-collaborative first"))
  ) %>%
  dplyr::select(-group_number, -CollaborativeFirst)

run_order_anova <- function(dv) {
  fit <- afex::aov_ez(
    id = "Group", dv = dv, data = dyad_order,
    between = c("SportType", "Order"), within = "DecisionCondition",
    type = 3, anova_table = list(es = "pes")
  )
  tab <- as.data.frame(afex::nice(fit, es = "pes"))
  tab$Outcome <- dv
  tab
}

order_results <- bind_rows(
  run_order_anova("mean_correct_rt"),
  run_order_anova("accuracy")
)
write_csv(order_results, "anova_order_sensitivity.csv")

order_descriptives <- dyad_order %>%
  group_by(Order, DecisionCondition) %>%
  summarise(
    n_dyads = n(),
    mean_rt = mean(mean_correct_rt),
    mean_accuracy = mean(accuracy),
    .groups = "drop"
  )
write_csv(order_descriptives, "order_descriptives.csv")

## Figure 2: mean RT and accuracy
condition_labels <- c("Non-collaborative", "Collaborative")
sport_cols <- c(Individual = "#F8766D", Team = "#00BFC4")

p_rt <- ggplot(group_descriptives,
               aes(DecisionCondition, mean_rt, colour = SportType,
                   group = SportType)) +
  geom_point(size = 2.5) + geom_line(linewidth = .7) +
  geom_errorbar(aes(ymin = mean_rt - se_rt, ymax = mean_rt + se_rt),
                width = .08) +
  scale_colour_manual(values = sport_cols) +
  theme_classic(base_size = 12) +
  labs(x = NULL, y = "Response time (ms)", colour = "Sport type")

p_acc <- ggplot(group_descriptives,
                aes(DecisionCondition, mean_accuracy, colour = SportType,
                    group = SportType)) +
  geom_point(size = 2.5) + geom_line(linewidth = .7) +
  geom_errorbar(aes(ymin = mean_accuracy - se_accuracy,
                    ymax = mean_accuracy + se_accuracy), width = .08) +
  scale_colour_manual(values = sport_cols) +
  coord_cartesian(ylim = c(.88, 1)) +
  theme_classic(base_size = 12) +
  labs(x = NULL, y = "Response accuracy", colour = "Sport type")

ggsave(file.path(OUTPUT_DIR, "Figure2_response_time.png"), p_rt,
       width = 6.5, height = 4.5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_DIR, "Figure2_accuracy.png"), p_acc,
       width = 6.5, height = 4.5, dpi = 300, bg = "white")

## 5. Sequential RT cleaning for A_AND(t) ----------------------------------
#
# Within each dyad x responding member x task condition cell:
#   1. remove RTs outside mean +/- 3 SD;
#   2. from the retained trials, remove RTs below the 2.5th percentile or
#      above the 97.5th percentile.
# Target number is pooled in the primary analysis.

sd3 <- raw %>%
  group_by(Group, Resp_Subject, Task) %>%
  filter(abs(RT - mean(RT)) <= 3 * sd(RT)) %>%
  ungroup()

assessment_data <- sd3 %>%
  group_by(Group, Resp_Subject, Task) %>%
  filter(RT >= quantile(RT, .025, type = 7),
         RT <= quantile(RT, .975, type = 7)) %>%
  ungroup() %>%
  as.data.frame()

cleaning_summary <- data.frame(
  original_trials = nrow(raw),
  deleted_3sd = nrow(raw) - nrow(sd3),
  retained_after_3sd = nrow(sd3),
  deleted_tail = nrow(sd3) - nrow(assessment_data),
  deleted_total = nrow(raw) - nrow(assessment_data),
  retained_trials = nrow(assessment_data),
  deleted_percent = 100 * (nrow(raw) - nrow(assessment_data)) / nrow(raw)
)
write_csv(cleaning_summary, "assessment_cleaning_summary.csv")
stopifnot(cleaning_summary$deleted_3sd == 238L,
          cleaning_summary$deleted_total == 2094L,
          cleaning_summary$retained_trials == 32706L)

## 6. Pooled-trial A_AND(t) bootstrap --------------------------------------

time_grid <- 500:4000
L <- length(time_grid)
window_table <- data.frame(start = seq(500, 3500, 50)) %>%
  mutate(end = start + 500)
window_weights <- sapply(seq_len(nrow(window_table)), function(k) {
  as.numeric(time_grid >= window_table$start[k] &
               time_grid <= window_table$end[k]) / 501
})

response_types <- c("correct_fast", "correct_slow",
                    "incorrect_fast", "incorrect_slow")
response_labels <- c(
  correct_fast = "Correct & Fast",
  correct_slow = "Correct & Slow",
  incorrect_fast = "Incorrect & Fast",
  incorrect_slow = "Incorrect & Slow"
)

observed_probabilities <- function(z) {
  lapply(1:3, function(k) {
    q <- z[z$Channel == k, ]
    n <- nrow(q)
    list(
      fc = findInterval(time_grid, sort(q$RT[q$Correct == 1])) / n,
      fi = findInterval(time_grid, sort(q$RT[q$Correct == 0])) / n,
      pc = mean(q$Correct == 1), n = n,
      ncorrect = sum(q$Correct == 1),
      nincorrect = sum(q$Correct == 0)
    )
  })
}

calculate_assessment <- function(p, type) {
  fc <- lapply(p, `[[`, "fc")
  fi <- lapply(p, `[[`, "fi")
  pc <- vapply(p, `[[`, numeric(1), "pc")
  sc <- lapply(1:3, function(k) pc[k] - fc[[k]])
  si <- lapply(1:3, function(k) (1 - pc[k]) - fi[[k]])
  a <- switch(type,
    correct_fast = (log(fc[[2]]) + log(fc[[3]])) / log(fc[[1]]),
    correct_slow = log(sc[[2]] * pc[3] + sc[[3]] * pc[2] -
                         sc[[2]] * sc[[3]]) / log(sc[[1]]),
    incorrect_fast = log(fi[[2]] * pc[3] + fi[[3]] * pc[2] +
                           fi[[2]] * si[[3]] + si[[2]] * fi[[3]] +
                           fi[[2]] * fi[[3]]) / log(fi[[1]]),
    incorrect_slow = log(si[[2]] * pc[3] + si[[3]] * pc[2] +
                           si[[2]] * si[[3]]) / log(si[[1]])
  )
  a[!is.finite(a)] <- NA_real_
  a
}

# Collapse the empirical joint RT-accuracy distribution into categories.
# Multinomial resampling of these categories is exactly equivalent, on the
# evaluation grid, to resampling complete RT-accuracy trial pairs.
make_channel_object <- function(q) {
  bin <- findInterval(q$RT, time_grid, left.open = TRUE) + 1L
  key <- paste(q$Correct, bin, sep = "_")
  tab <- table(key)
  nm <- names(tab)
  outcome <- as.integer(sub("_.*", "", nm))
  bins <- as.integer(sub(".*_", "", nm))
  ord <- order(outcome, bins)
  list(n = nrow(q), prob = as.numeric(tab)[ord] / nrow(q),
       outcome = outcome[ord], bin = bins[ord])
}

draw_probabilities <- function(obj, m) {
  counts <- rmultinom(m, obj$n, obj$prob)
  make_cdf <- function(outcome) {
    take <- which(obj$outcome == outcome)
    bins <- obj$bin[take]
    cc <- counts[take, , drop = FALSE]
    ord <- order(bins)
    bins <- bins[ord]
    cc <- cc[ord, , drop = FALSE]
    cumulative <- apply(cc, 2, cumsum)
    map <- findInterval(seq_len(L), bins)
    rbind(0, cumulative)[map + 1L, , drop = FALSE] / obj$n
  }
  list(
    fc = make_cdf(1L),
    fi = make_cdf(0L),
    pc = colSums(counts[obj$outcome == 1, , drop = FALSE]) / obj$n
  )
}

calculate_assessment_matrix <- function(p, type) {
  fc <- lapply(p, `[[`, "fc")
  fi <- lapply(p, `[[`, "fi")
  pc <- lapply(p, `[[`, "pc")
  sc <- lapply(1:3, function(k) sweep(-fc[[k]], 2, pc[[k]], "+"))
  si <- lapply(1:3, function(k) sweep(-fi[[k]], 2, 1 - pc[[k]], "+"))
  a <- switch(type,
    correct_fast = (log(fc[[2]]) + log(fc[[3]])) / log(fc[[1]]),
    correct_slow = log(sweep(sc[[2]], 2, pc[[3]], "*") +
                         sweep(sc[[3]], 2, pc[[2]], "*") -
                         sc[[2]] * sc[[3]]) / log(sc[[1]]),
    incorrect_fast = log(sweep(fi[[2]], 2, pc[[3]], "*") +
                           sweep(fi[[3]], 2, pc[[2]], "*") +
                           fi[[2]] * si[[3]] + si[[2]] * fi[[3]] +
                           fi[[2]] * fi[[3]]) / log(fi[[1]]),
    incorrect_slow = log(sweep(si[[2]], 2, pc[[3]], "*") +
                           sweep(si[[3]], 2, pc[[2]], "*") +
                           si[[2]] * si[[3]]) / log(si[[1]])
  )
  a[!is.finite(a)] <- NA_real_
  a
}

window_means_from_matrix <- function(a) {
  bad <- !is.finite(a)
  a0 <- a
  a0[bad] <- 0
  value <- crossprod(a0, window_weights)
  invalid <- crossprod(bad * 1, (window_weights > 0) * 1) > 0
  value[invalid] <- NA_real_
  value
}

set.seed(SEED)
observed <- list()
bootstrap_windows <- list()
plot_bands <- list()
support_counts <- list()
plot_index <- seq_along(time_grid)

for (sport in c("Individual", "Team")) {
  z <- assessment_data[assessment_data$SportType == sport, ]
  op <- observed_probabilities(z)
  objects <- lapply(1:3, function(k) make_channel_object(z[z$Channel == k, ]))
  observed[[sport]] <- lapply(response_types, function(tp) {
    calculate_assessment(op, tp)
  })
  names(observed[[sport]]) <- response_types

  # Validate all observed curves against the public sft package.
  for (tp in response_types) {
    is_correct <- startsWith(tp, "correct")
    is_fast <- endsWith(tp, "fast")
    package_curve <- sft::assessment(
      lapply(1:3, function(k) z$RT[z$Channel == k]),
      lapply(1:3, function(k) as.logical(z$Correct[z$Channel == k])),
      stopping.rule = "AND", correct = is_correct,
      fast = is_fast, detection = FALSE
    )(time_grid)
    manual_curve <- observed[[sport]][[tp]]
    ok <- is.finite(package_curve) & is.finite(manual_curve)
    max_difference <- max(abs(package_curve[ok] - manual_curve[ok]))
    if (!is.finite(max_difference) || max_difference >= 1e-8) {
      stop("Observed assessment implementation does not match sft::assessment.")
    }
  }

  bootstrap_windows[[sport]] <- lapply(response_types, function(x) {
    matrix(NA_real_, BOOT_B, nrow(window_table))
  })
  names(bootstrap_windows[[sport]]) <- response_types
  curve_store <- lapply(response_types, function(x) {
    matrix(NA_real_, BOOT_B, length(plot_index))
  })
  names(curve_store) <- response_types

  # Batches reduce memory use while preserving the requested number of draws.
  for (first in seq(1L, BOOT_B, 100L)) {
    rows <- first:min(BOOT_B, first + 99L)
    pp <- lapply(objects, draw_probabilities, m = length(rows))
    for (tp in response_types) {
      aa <- calculate_assessment_matrix(pp, tp)
      bootstrap_windows[[sport]][[tp]][rows, ] <-
        window_means_from_matrix(aa)
      curve_store[[tp]][rows, ] <- t(aa[plot_index, , drop = FALSE])
    }
  }

  for (tp in response_types) {
    m <- curve_store[[tp]]
    plot_bands[[paste(sport, tp)]] <- data.frame(
      Sport = sport, type = tp, time = time_grid,
      A = observed[[sport]][[tp]],
      low = apply(m, 2, quantile, .025, na.rm = TRUE),
      high = apply(m, 2, quantile, .975, na.rm = TRUE),
      valid = colSums(is.finite(m))
    )
  }

  # Outcome support used to determine which candidate windows are eligible.
  support_counts[[sport]] <- lapply(c(correct = 1, incorrect = 0),
                                    function(outcome) {
    bind_rows(lapply(seq_len(nrow(window_table)), function(k) {
      before <- after <- numeric(3)
      for (channel in 1:3) {
        q <- z[z$Channel == channel & z$Correct == outcome, ]
        before[channel] <- sum(q$RT <= window_table$start[k])
        after[channel] <- sum(q$RT > window_table$end[k])
      }
      data.frame(
        start = window_table$start[k], end = window_table$end[k],
        min_before = min(before), min_after = min(after)
      )
    }))
  })
}

## 7. Window contrasts and simultaneous confidence intervals ---------------

all_windows <- list()
key_results <- list()

for (tp in response_types) {
  boot_team <- bootstrap_windows$Team[[tp]]
  boot_individual <- bootstrap_windows$Individual[[tp]]
  boot_difference <- boot_team - boot_individual

  mean_team <- vapply(seq_len(nrow(window_table)), function(k) {
    q <- observed$Team[[tp]][time_grid >= window_table$start[k] &
                              time_grid <= window_table$end[k]]
    if (all(is.finite(q))) mean(q) else NA_real_
  }, numeric(1))
  mean_individual <- vapply(seq_len(nrow(window_table)), function(k) {
    q <- observed$Individual[[tp]][time_grid >= window_table$start[k] &
                                    time_grid <= window_table$end[k]]
    if (all(is.finite(q))) mean(q) else NA_real_
  }, numeric(1))

  estimate <- mean_team - mean_individual
  valid <- colSums(is.finite(boot_difference))
  se <- apply(boot_difference, 2, sd, na.rm = TRUE)
  percentile <- apply(boot_difference, 2, quantile, c(.025, .975),
                      na.rm = TRUE)

  outcome_class <- if (startsWith(tp, "correct")) "correct" else "incorrect"
  support_i <- support_counts$Individual[[outcome_class]]
  support_t <- support_counts$Team[[outcome_class]]
  support_ok <- pmin(support_i$min_before, support_i$min_after,
                     support_t$min_before, support_t$min_after) >= 30
  eligible <- which(support_ok & valid >= .99 * BOOT_B &
                      is.finite(estimate) & se > 0)

  simultaneous_low <- simultaneous_high <- rep(NA_real_, nrow(window_table))
  critical <- NA_real_
  if (length(eligible)) {
    standardized <- sweep(
      sweep(boot_difference[, eligible, drop = FALSE], 2,
            estimate[eligible], "-"),
      2, se[eligible], "/"
    )
    max_statistic <- apply(abs(standardized), 1, max, na.rm = TRUE)
    critical <- unname(quantile(max_statistic, .95, na.rm = TRUE))
    simultaneous_low[eligible] <- estimate[eligible] - critical * se[eligible]
    simultaneous_high[eligible] <- estimate[eligible] + critical * se[eligible]
  }

  result <- data.frame(
    type = tp, window_table,
    team = mean_team, individual = mean_individual,
    difference = estimate, bootstrap_sd = se, valid_boot = valid,
    percentile_low = percentile[1, ], percentile_high = percentile[2, ],
    min_relevant_before = pmin(support_i$min_before, support_t$min_before),
    min_relevant_after = pmin(support_i$min_after, support_t$min_after),
    support_ok = support_ok,
    simultaneous_low = simultaneous_low,
    simultaneous_high = simultaneous_high,
    critical = critical
  )
  all_windows[[tp]] <- result
  key_results[[tp]] <- list(
    significant_team = subset(result, support_ok & simultaneous_low > 0),
    significant_individual = subset(result, support_ok & simultaneous_high < 0)
  )
}

window_results <- bind_rows(all_windows)
curve_bands <- bind_rows(plot_bands)
write_csv(window_results, "assessment_500ms_windows.csv")
write_csv(curve_bands, "assessment_curve_pointwise_bands.csv")

## 8. Manuscript-reported windows and CDF support --------------------------

reported_windows <- data.frame(
  type = c("correct_fast", "correct_fast", "correct_slow", "incorrect_fast"),
  start = c(1100, 2000, 900, 1500),
  end = c(1600, 2500, 1400, 2000)
)

reported_results <- reported_windows %>%
  left_join(window_results,
            by = c("type", "start", "end"))
write_csv(reported_results, "manuscript_reported_windows.csv")

# Mean dyad-level collaborative correct-response CDF at the endpoints of the
# two correct-fast windows. In SFT notation this is the joint probability
# P(T <= t, correct), rather than the unconditional RT CDF. It is descriptive
# support, not the pooled-trial test statistic.
cdf_times <- c(1100, 1600, 2000, 2500)
# Build explicitly to avoid relying on list-columns in public code.
cdf_by_dyad <- bind_rows(lapply(split(
  assessment_data[assessment_data$Task == "Collaborative", ],
  interaction(
    assessment_data$SportType[assessment_data$Task == "Collaborative"],
    assessment_data$Group[assessment_data$Task == "Collaborative"],
    drop = TRUE
  )
), function(z) {
  data.frame(
    Group = z$Group[1], SportType = z$SportType[1],
    time = cdf_times,
    CDF = vapply(
      cdf_times,
      function(tt) mean(z$Correct == 1 & z$RT <= tt),
      numeric(1)
    )
  )
}))
cdf_summary <- cdf_by_dyad %>%
  group_by(SportType, time) %>%
  summarise(mean_dyad_CDF = mean(CDF), sd_dyad_CDF = sd(CDF),
            n_dyads = n(), .groups = "drop")
write_csv(cdf_by_dyad, "correct_fast_cdf_by_dyad.csv")
write_csv(cdf_summary, "correct_fast_cdf_summary.csv")

## 9. Figure 3 --------------------------------------------------------------

curve_bands <- curve_bands %>%
  mutate(
    type_label = factor(response_labels[type], levels = response_labels),
    tail_status = ifelse(type == "incorrect_slow" & time > 2950,
                         "Low tail support", "Supported")
  )
ribbon_data <- curve_bands %>% filter(tail_status == "Supported")
solid_data <- curve_bands %>%
  mutate(A = ifelse(tail_status == "Supported", A, NA_real_))
dashed_data <- curve_bands %>%
  mutate(A = ifelse(tail_status == "Low tail support", A, NA_real_))
figure3 <- ggplot(curve_bands, aes(time, A, colour = Sport, fill = Sport)) +
  geom_ribbon(data = ribbon_data, aes(ymin = low, ymax = high),
              alpha = .22, colour = NA, na.rm = TRUE) +
  geom_line(data = solid_data, linewidth = .65, na.rm = TRUE) +
  geom_line(data = dashed_data, linewidth = .65, linetype = "dashed",
            alpha = .48, na.rm = TRUE) +
  geom_hline(yintercept = 1, linetype = 2, colour = "red") +
  facet_wrap(~type_label, ncol = 2, scales = "free_y") +
  scale_colour_manual(values = sport_cols) +
  scale_fill_manual(values = sport_cols) +
  scale_x_continuous(limits = c(0, 4000), breaks = seq(0, 4000, 1000)) +
  theme_bw(base_size = 12) +
  theme(legend.position = "top") +
  labs(
    x = "Time (ms)", y = expression(A[AND](t)),
    colour = NULL, fill = NULL,
    caption = paste(
      "Pointwise percentile 95% bootstrap bands.",
      "For Incorrect & Slow, bands end at 2950 ms;",
      "dashed curves thereafter have insufficient tail support."
    )
  )

ggsave(file.path(OUTPUT_DIR, "Figure3_assessment_functions.png"), figure3,
       width = 11, height = 8, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_DIR, "Figure3_assessment_functions.pdf"), figure3,
       width = 11, height = 8, device = cairo_pdf)

## 10. Human-readable result summary and verification ----------------------

pick_window <- function(type_value, start_value, end_value) {
  x <- window_results %>%
    filter(.data$type == .env$type_value,
           .data$start == .env$start_value,
           .data$end == .env$end_value)
  stopifnot(nrow(x) == 1L)
  x
}

cf_early <- pick_window("correct_fast", 1100, 1600)
cf_late <- pick_window("correct_fast", 2000, 2500)
cs_early <- pick_window("correct_slow", 900, 1400)
if_early <- pick_window("incorrect_fast", 1500, 2000)
is_early_unsupported <- pick_window("incorrect_slow", 500, 1000)
is_significant_supported <- window_results %>%
  filter(type == "incorrect_slow", support_ok,
         (simultaneous_low > 0 | simultaneous_high < 0))

summary_lines <- c(
  "CRPI manuscript analysis summary",
  sprintf("Bootstrap seed = %d; replicates = %d", SEED, BOOT_B),
  "",
  sprintf("Assessment cleaning: %d original; %d removed by 3 SD; %d additional tail removals; %d retained.",
          cleaning_summary$original_trials, cleaning_summary$deleted_3sd,
          cleaning_summary$deleted_tail, cleaning_summary$retained_trials),
  "",
  sprintf("Correct & fast 1100-1600 ms: Team %.3f; Individual %.3f; difference %.3f; simultaneous 95%% CI [%.3f, %.3f].",
          cf_early$team, cf_early$individual, cf_early$difference,
          cf_early$simultaneous_low, cf_early$simultaneous_high),
  sprintf("Correct & fast 2000-2500 ms: Team %.3f; Individual %.3f; difference %.3f; simultaneous 95%% CI [%.3f, %.3f].",
          cf_late$team, cf_late$individual, cf_late$difference,
          cf_late$simultaneous_low, cf_late$simultaneous_high),
  sprintf("Correct & slow 900-1400 ms: Team %.3f; Individual %.3f; difference %.3f; simultaneous 95%% CI [%.3f, %.3f].",
          cs_early$team, cs_early$individual, cs_early$difference,
          cs_early$simultaneous_low, cs_early$simultaneous_high),
  sprintf("Incorrect & fast 1500-2000 ms: Team %.3f; Individual %.3f; difference %.3f; simultaneous 95%% CI [%.3f, %.3f].",
          if_early$team, if_early$individual, if_early$difference,
          if_early$simultaneous_low, if_early$simultaneous_high),
  sprintf("Incorrect & slow: %d adequately supported windows had a search-corrected CI excluding zero.",
          nrow(is_significant_supported)),
  sprintf("Descriptive early Incorrect & slow 500-1000 ms: difference %.3f; ordinary percentile CI [%.3f, %.3f], but minimum relevant trials before the window = %d; therefore the window was ineligible for formal search-corrected inference.",
          is_early_unsupported$difference,
          is_early_unsupported$percentile_low,
          is_early_unsupported$percentile_high,
          is_early_unsupported$min_relevant_before)
)
writeLines(summary_lines, file.path(OUTPUT_DIR, "RESULTS_SUMMARY.txt"))
writeLines(capture.output(sessionInfo()), file.path(OUTPUT_DIR, "session_info.txt"))

# Exact manuscript checks are intentionally limited to estimates; bootstrap
# limits can differ slightly if BOOT_B or the R random-number implementation is
# changed.
if (BOOT_B == 10000L) {
  stopifnot(
    abs(cf_early$team - 2.212) < .002,
    abs(cf_early$individual - 2.368) < .002,
    abs(cf_late$team - 4.282) < .002,
    abs(cf_late$individual - 3.585) < .002,
    nrow(is_significant_supported) == 0L
  )
}

cat("Analysis completed successfully. Outputs written to:", OUTPUT_DIR, "\n")
