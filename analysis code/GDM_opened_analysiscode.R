rm(list = ls())

## Load Packages
# install.packages("sft")
library(pacman)
p_load(
  tidyverse, psyphy, lubridate, dplyr, tidyr, data.table, ggpubr, ERP, mnormt,
  fdrtool, tidyverse, gridExtra, crayon, boot, reshape2, ggthemes, devtools,
  sft, BayesFactor, magrittr
)

#############################################
# EXP 1:
# This part is for data aggregation 

# Read file path
sft_path <- list.files(
  path = "C:/Users/waes8/Desktop/SFT",
  pattern = ".csv",
  full.names = TRUE
)

# Read file content
sft_data <- sft_path %>%
  lapply(read.csv, header = FALSE)

# Unlist and make it as data frame
sft_result <- rbindlist(sft_data, fill = TRUE)

dta <- sft_result %>%
  na.omit(.) %>%
  as.data.frame(.)

# Column 1: Group
# Column 2: Condition (1: non-Collaborative condition, 2: Collaborative condition)
# Column 3: Response Subject (which Subject in the group 1: A, 2: B)
# Column 4: Target (how many target of the trial 0: target absent, 1: single target, 2: double target)
# Column 8: Fixation Time (ms unit)
# Column 9: Response (0: no response, 9: target absent, 1: single target, 2: double target)
# Column 10: Response Time (ms unit)
# Column 11: Accuracy (0: incorrect, 1: correct)

names(dta) <- c(
  "Group", "Condition", "Resp_Subject", "Target", "Trigger", "XX", "XXX",
  "Fixtime", "Response", "RT", "Correct"
)

# Create subject ID by combining Group and Resp_Subject
dta$Subject <- interaction(dta$Group, dta$Resp_Subject, sep = "_")

# Individual sport group: 0–10, 41–45; Team sport group: 11–30
dta$SportType <- ifelse(
  dta$Group <= 10, "Individual",
  ifelse(dta$Group >= 30, "Individual", "Team")
)

# Collaboration condition
dta$Condition <- ifelse(dta$Condition <= 20, "Non-verbal", "Verbal")

dta$RT <- as.integer(round(dta$RT, 0))

library(dplyr)
# Remove group 3 (accuracy < .60)

dta <- dta %>%
  filter(Group != 3)

####### Data filtering
# Group by Subject, Condition, SportType, Target and compute SD and upper/lower limits
dta_cleaned <- dta %>%
  group_by(Subject, Condition, SportType, Target) %>%
  mutate(
    RT_mean     = mean(RT, na.rm = TRUE),
    RT_sd       = sd(RT, na.rm = TRUE),
    lower_limit = RT_mean - 3 * RT_sd,  # 3 SD lower limit
    upper_limit = RT_mean + 3 * RT_sd   # 3 SD upper limit
  ) %>%
  ungroup()

# Count the number of rows before filtering
before_filter <- nrow(dta_cleaned)

# Remove outliers beyond the upper/lower limits
dta_cleaned <- dta_cleaned %>%
  filter(RT >= lower_limit & RT <= upper_limit)

after_filter <- nrow(dta_cleaned)

# Compute the number of removed rows
removed_data_count <- before_filter - after_filter

# Display the number of removed rows
cat("Removed", removed_data_count, "trial。")

# Remove auxiliary columns that are no longer needed
dta_cleaned <- dta_cleaned %>%
  dplyr::select(-RT_mean, -RT_sd, -lower_limit, -upper_limit)

# Inspect filtered data
head(dta_cleaned)

##########################RT Plot
dta_n <- dta %>%
  group_by(Subject, Condition, SportType) %>%
  summarize(n_Trials = n()) 

# 計算平均 RT
dta_mRT <- dta %>%
  group_by(Subject, Condition, SportType) %>%
  filter(Correct != "0") %>%
  summarize(RT_m = round(mean(RT, na.rm = TRUE),0), .groups = "drop") 

dta_mRT$Condition <- factor(dta_mRT$Condition, 
                            levels = c("Non-verbal", "Verbal"),
                            labels = c("Non-Collaborative", "Collaborative"))

dta_mRT$SportType <- factor(dta_mRT$SportType, 
                            levels = c("Individual", "Team"),
                            labels = c("Individual Sport", "Team Sport"))

##
#plot
dta_mRT %>% 
  group_by(Condition, SportType) %>%
  mutate(m_Value=mean(RT_m), 
         se_Value=sd(RT_m)/sqrt(n())) %>%
  ggplot() +
  aes(Condition, m_Value, color=Condition, fill=Condition) +
  geom_bar(stat="identity", position=position_dodge(0.7), width = 0.6,
           size=0.8) +
  geom_errorbar(aes(ymin=m_Value - se_Value,
                    ymax=m_Value + se_Value),width=.25, size=1, 
                position=position_dodge(.7)) +
  geom_point(aes(x=Condition, y=RT_m, color=Condition, fill=Condition), position=position_jitterdodge(jitter.width = 0.4), 
             alpha = 0.7, shape = 21, size = 2, stroke=1) +
  scale_color_manual(values = c("#845116", "#9b71aa"))+
  scale_fill_manual(values = c("#ebd94c", "#dbc6e0"))+
  facet_grid(.~SportType)+
  xlab("Condition") +
  ylab("Mean RTs (msec)") +
  theme_bw() +
  theme(text = element_text(size = 12), legend.position = 'none')



###################### SFT
################### Capacity Coefficient
library(dplyr)
library(tidyr)
library(ggplot2)

# Filter data and relabel
dtaforsft <- dta %>%
  filter(RT != 0) %>%
  dplyr::select(-Subject) %>%
  mutate(Subject = Group)

# Relabel factors
dtaforsft$Channel1 <- 0
dtaforsft$Channel1[dtaforsft$Condition == "Verbal"]      <- 1
dtaforsft$Channel1[dtaforsft$Resp_Subject == "1"]        <- 1
dtaforsft$Channel2 <- 0
dtaforsft$Channel2[dtaforsft$Condition == "Verbal"]      <- 1
dtaforsft$Channel2[dtaforsft$Resp_Subject == "2"]        <- 1

dtaforsft <- dplyr::select(
  dtaforsft,
  Subject, Condition, SportType, Correct, RT, Channel1, Channel2
)

## C(t) analysis
dtaforsft$Condition <- dtaforsft$SportType
cap.res <- capacityGroup(
  dtaforsft,
  acc.cutoff   = .7,
  stopping.rule = "AND",
  plotCt       = FALSE
)

###########
ass.res_cf <- assessmentGroup(
  dtaforsft,
  stopping.rule = "AND",
  correct       = TRUE,
  fast          = TRUE,
  detection     = FALSE,
  plotAt        = FALSE
)
ass.res_cs <- assessmentGroup(
  dtaforsft,
  stopping.rule = "AND",
  correct       = TRUE,
  fast          = FALSE,
  detection     = FALSE,
  plotAt        = FALSE
)
ass.res_if <- assessmentGroup(
  dtaforsft,
  stopping.rule = "AND",
  correct       = FALSE,
  fast          = TRUE,
  detection     = FALSE,
  plotAt        = FALSE
)
ass.res_is <- assessmentGroup(
  dtaforsft,
  stopping.rule = "AND",
  correct       = FALSE,
  fast          = FALSE,
  detection     = FALSE,
  plotAt        = FALSE
)

##########

# C(t) plot
ct <- cap.res$Ct.fn %>%
  as.data.frame(.)
time_pt <- cap.res$times
Time <- seq(1, 1000, 1)
time_matrix <- cbind(time_pt, Time) %>%
  as.data.frame(.)

Sall <- cap.res$overview$Subject
Call <- cap.res$overview$Condition
Call <- Call[Sall != "Group"]
Sall <- Sall[Sall != "Group"]
ct$Subject   <- Sall
ct$Condition <- Call
ct$Channel   <- "Cand(t)"

dta_ct <- ct %>%
  gather(Time, Value, V1:V1000) %>%
  separate(Time, c("V", "Time"), sep = "(?<=[A-Za-z])(?=[0-9])") %>%
  dplyr::select(-V) %>%
  mutate(Time = as.numeric(Time))

dta_ct <- inner_join(dta_ct, time_matrix)

# Relabel Condition
dta_ct <- dta_ct %>%
  mutate(
    Condition = recode(
      Condition,
      "Individual" = "Individual sport",
      "Team"       = "Team sport"
    )
  )

# Plot C(t)
ggplot(dta_ct, aes(x = time_pt, y = Value, group = Condition)) +
  stat_summary(
    fun  = mean,
    geom = "line",
    size = 1,
    aes(color = Condition)
  ) +
  stat_summary(
    fun.data = mean_cl_boot,
    geom     = "ribbon",
    alpha    = 0.3,
    aes(fill = Condition)
  ) +
  geom_hline(
    yintercept = 1,
    col        = "red",
    linetype   = "dashed",
    size       = 1
  ) +
  xlab("Time (msec)") +
  xlim(0, 6000) +
  ylab(expression(~italic("CAND(t)"))) +
  theme_bw() +
  theme(
    text          = element_text(size = 12),
    legend.position = "top",
    legend.title  = element_blank()
  )

# Assessment function
# Cf plot
cf <- ass.res_cf$At.fn %>%
  as.data.frame(.)
time_pt <- ass.res_cf$times
Time <- seq(1, 1000, 1)
time_matrix <- cbind(time_pt, Time) %>%
  as.data.frame(.)

Sall <- ass.res_cf$Subject
Call <- ass.res_cf$Condition
Call <- Call[Sall != "Group"]
Sall <- Sall[Sall != "Group"]
cf$Subject   <- Sall
cf$Condition <- Call
cf$Channel   <- "Aand(t)"

dta_cf <- cf %>%
  gather(Time, Value, V1:V1000) %>%
  separate(Time, c("V", "Time"), sep = "(?<=[A-Za-z])(?=[0-9])") %>%
  dplyr::select(-V) %>%
  mutate(Time = as.numeric(Time))

dta_cf <- inner_join(dta_cf, time_matrix)
str(dta_cf)

# Plot Cf
ggplot(dta_cf, aes(x = time_pt, y = Value, group = Condition)) +
  stat_summary(
    fun  = mean,
    geom = "line",
    size = 1,
    aes(color = Condition)
  ) +
  stat_summary(
    fun.data = mean_cl_boot,
    geom     = "ribbon",
    alpha    = 0.3,
    aes(fill = Condition)
  ) +
  geom_hline(
    yintercept = 1,
    col        = "red",
    linetype   = "dashed",
    size       = 1
  ) +
  xlab("Time (msec)") +
  ylab(expression(~italic("Aand(t)"))) +
  theme_bw() +
  theme(
    text           = element_text(size = 12),
    legend.position = "top"
  )

# Cs plot
cs <- ass.res_cs$At.fn %>%
  as.data.frame(.)
time_pt <- ass.res_cs$times
Time <- seq(1, 1000, 1)
time_matrix <- cbind(time_pt, Time) %>%
  as.data.frame(.)

Sall <- ass.res_cs$Subject
Call <- ass.res_cs$Condition
Call <- Call[Sall != "Group"]
Sall <- Sall[Sall != "Group"]
cs$Subject   <- Sall
cs$Condition <- Call
cs$Channel   <- "Aand(t)"

dta_cs <- cs %>%
  gather(Time, Value, V1:V1000) %>%
  separate(Time, c("V", "Time"), sep = "(?<=[A-Za-z])(?=[0-9])") %>%
  dplyr::select(-V) %>%
  mutate(Time = as.numeric(Time))

dta_cs <- inner_join(dta_cs, time_matrix)
str(dta_cs)

# Plot Cs
ggplot(dta_cs, aes(x = time_pt, y = Value, group = Condition)) +
  stat_summary(
    fun  = mean,
    geom = "line",
    size = 1,
    aes(color = Condition)
  ) +
  stat_summary(
    fun.data = mean_cl_boot,
    geom     = "ribbon",
    alpha    = 0.3,
    aes(fill = Condition)
  ) +
  geom_hline(
    yintercept = 1,
    col        = "red",
    linetype   = "dashed",
    size       = 1
  ) +
  xlab("Time (msec)") +
  ylab(expression(~italic("Aand(t)"))) +
  theme_bw() +
  theme(
    text           = element_text(size = 12),
    legend.position = "top"
  )

# If plot
If <- ass.res_if$At.fn %>%
  as.data.frame(.)
time_pt <- ass.res_if$times
Time <- seq(1, 1000, 1)
time_matrix <- cbind(time_pt, Time) %>%
  as.data.frame(.)

Sall <- ass.res_if$Subject
Call <- ass.res_if$Condition
Call <- Call[Sall != "Group"]
Sall <- Sall[Sall != "Group"]
If$Subject   <- Sall
If$Condition <- Call
If$Channel   <- "Aand(t)"

dta_if <- If %>%
  gather(Time, Value, V1:V1000) %>%
  separate(Time, c("V", "Time"), sep = "(?<=[A-Za-z])(?=[0-9])") %>%
  dplyr::select(-V) %>%
  mutate(Time = as.numeric(Time))

dta_if <- inner_join(dta_if, time_matrix)
str(dta_if)

# Plot If
ggplot(dta_if, aes(x = time_pt, y = Value, group = Condition)) +
  stat_summary(
    fun  = mean,
    geom = "line",
    size = 1,
    aes(color = Condition)
  ) +
  stat_summary(
    fun.data = mean_cl_boot,
    geom     = "ribbon",
    alpha    = 0.3,
    aes(fill = Condition)
  ) +
  geom_hline(
    yintercept = 1,
    col        = "red",
    linetype   = "dashed",
    size       = 1
  ) +
  xlab("Time (msec)") +
  ylab(expression(~italic("Aand(t)"))) +
  theme_bw() +
  theme(
    text           = element_text(size = 12),
    legend.position = "top"
  )

# Is plot
Is <- ass.res_is$At.fn %>%
  as.data.frame(.)
time_pt <- ass.res_is$times
Time <- seq(1, 1000, 1)
time_matrix <- cbind(time_pt, Time) %>%
  as.data.frame(.)

Sall <- ass.res_is$Subject
Call <- ass.res_is$Condition
Call <- Call[Sall != "Group"]
Sall <- Sall[Sall != "Group"]
Is$Subject   <- Sall
Is$Condition <- Call
Is$Channel   <- "Aand(t)"

dta_is <- Is %>%
  gather(Time, Value, V1:V1000) %>%
  separate(Time, c("V", "Time"), sep = "(?<=[A-Za-z])(?=[0-9])") %>%
  dplyr::select(-V) %>%
  mutate(Time = as.numeric(Time))

dta_is <- inner_join(dta_is, time_matrix)
str(dta_is)

# Plot Is
ggplot(dta_is, aes(x = time_pt, y = Value, group = Condition)) +
  stat_summary(
    fun  = mean,
    geom = "line",
    size = 1,
    aes(color = Condition)
  ) +
  stat_summary(
    fun.data = mean_cl_boot,
    geom     = "ribbon",
    alpha    = 0.3,
    aes(fill = Condition)
  ) +
  geom_hline(
    yintercept = 1,
    col        = "red",
    linetype   = "dashed",
    size       = 1
  ) +
  xlab("Time (msec)") +
  ylab(expression(~italic("Aand(t)"))) +
  xlim(0, 3000) +
  theme_bw() +
  theme(
    text           = element_text(size = 12),
    legend.position = "top"
  )

# Combine four plots
library(ggplot2)
library(cowplot)  # For combining plots
library(dplyr)

# Ensure Condition is a factor with defined levels
all_conditions <- c("Individual sport", "Team sport")

dta_cf <- dta_cf %>%
  mutate(
    Condition = factor(
      recode(Condition, "Individual" = "Individual sport", "Team" = "Team sport"),
      levels = all_conditions
    )
  )

dta_cs <- dta_cs %>%
  mutate(
    Condition = factor(
      recode(Condition, "Individual" = "Individual sport", "Team" = "Team sport"),
      levels = all_conditions
    )
  )

dta_if <- dta_if %>%
  mutate(
    Condition = factor(
      recode(Condition, "Individual" = "Individual sport", "Team" = "Team sport"),
      levels = all_conditions
    )
  )

dta_is <- dta_is %>%
  mutate(
    Condition = factor(
      recode(Condition, "Individual" = "Individual sport", "Team" = "Team sport"),
      levels = all_conditions
    )
  )

# Plotting function (default: no x-axis limit)
plot_aand <- function(data, title, xlim_max = NULL) {
  p <- ggplot(
    data,
    aes(
      x     = time_pt,
      y     = Value,
      group = Condition,
      color = Condition,
      fill  = Condition
    )
  ) +
    stat_summary(
      fun  = mean,
      geom = "line",
      size = 1
    ) +
    stat_summary(
      fun.data = mean_cl_boot,
      geom     = "ribbon",
      alpha    = 0.3
    ) +
    geom_hline(
      yintercept = 1,
      col        = "red",
      linetype   = "dashed",
      size       = 1
    ) +
    xlab("Time (msec)") +
    ylab(expression(~italic("AAND(t)"))) +
    ggtitle(title) +
    theme_bw() +
    theme(
      text           = element_text(size = 12),
      legend.position = "none"
    )
  
  # Apply xlim when x-axis range is constrained
  if (!is.null(xlim_max)) {
    p <- p + xlim(0, xlim_max)
  }
  
  return(p)
}

# Create individual plots
p1 <- plot_aand(dta_cf, "Correct & Fast")
p2 <- plot_aand(dta_cs, "Correct & Slow")
p3 <- plot_aand(dta_if, "Incorrect & Fast")
p4 <- plot_aand(dta_is, "Incorrect & Slow", xlim_max = 3000)  # **Only this plot uses x-axis limits 0–3000**

# **Legend settings**
legend_plot <- ggplot(
  bind_rows(dta_cf, dta_cs, dta_if, dta_is),
  aes(
    x     = time_pt,
    y     = Value,
    group = Condition,
    color = Condition,
    fill  = Condition
  )
) +
  geom_line() +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.title    = element_blank()
  )

legend <- get_legend(legend_plot)

# **Combine plots and add legend**
combined_plot <- plot_grid(
  legend,  # **Legend at the top**
  plot_grid(
    p1, p2, p3, p4,
    labels = c("A", "B", "C", "D"),
    ncol   = 2
  ),
  ncol        = 1,
  rel_heights = c(0.1, 1)  # **Legend with smaller height**
)

# Display the final combined plot
print(combined_plot)

##########################
##########################
##########################
############### At bootstrap for Cf

# Filter data and relabel
dta_boot <- dta_cleaned %>%
  filter(RT != 0) %>%
  dplyr::select(-Subject) %>%
  mutate(Subject = Group)

# Recode channels
dta_boot$Channel1 <- 0
dta_boot$Channel1[dta_boot$Condition == "Verbal"]       <- 1
dta_boot$Channel1[dta_boot$Resp_Subject == "1"]         <- 1
dta_boot$Channel2 <- 0
dta_boot$Channel2[dta_boot$Condition == "Verbal"]       <- 1
dta_boot$Channel2[dta_boot$Resp_Subject == "2"]         <- 1

dta_boot <- dplyr::select(
  dta_boot,
  Subject, Condition, SportType, Correct, RT, Channel1, Channel2
)

# Use SportType as Condition for SFT assessment
dta_boot$Condition <- dta_boot$SportType

########### assessment objects (renamed to avoid conflicts)
ass_boot_cf <- assessmentGroup(
  dta_boot,
  stopping.rule = "AND",
  correct       = TRUE,
  fast          = TRUE,
  detection     = FALSE,
  plotAt        = FALSE
)


########## build Cf time-series object (renamed) ##########
cf_boot <- ass_boot_cf$At.fn %>%
  as.data.frame(.)

# Map time index (1–1000) to actual time points
time_index_boot <- seq(1, 1000, 1)
time_values_boot <- ass_boot_cf$times
time_map_boot <- data.frame(
  TimeIndex = time_index_boot,
  time_pt   = time_values_boot
)

subj_boot <- ass_boot_cf$Subject
cond_boot <- ass_boot_cf$Condition
cond_boot <- cond_boot[subj_boot != "Group"]
subj_boot <- subj_boot[subj_boot != "Group"]

cf_boot$Subject   <- subj_boot
cf_boot$Condition <- cond_boot
cf_boot$Channel   <- "Aand(t)"

dta_cf_boot <- cf_boot %>%
  gather(TimeIndex, Value, V1:V1000) %>%  # TimeIndex = "V1"..."V1000"
  separate(TimeIndex, c("V", "TimeIndex"), sep = "(?<=[A-Za-z])(?=[0-9])") %>%
  dplyr::select(-V) %>%
  mutate(TimeIndex = as.numeric(TimeIndex))

# Join actual time values (time_pt) based on index
dta_cf_boot <- inner_join(
  dta_cf_boot,
  time_map_boot,
  by = c("TimeIndex" = "TimeIndex")
)

str(dta_cf_boot)

############### Bootstrap AUC for Cf ###############

# Load packages (for safety if this chunk is run standalone)
library(dplyr)
library(pracma)
library(boot)

# 1) Compute AUC for each subject (full time range; same as previous logic)
subj_auc_boot <- dta_cf_boot %>%
  filter(!is.na(Value)) %>%
  mutate(adj = pmax(Value - 1, 0)) %>%
  group_by(Subject, Condition) %>%
  arrange(time_pt) %>%                                # use column time_pt
  summarise(
    auc    = trapz(time_pt, adj),
    .groups = "drop"
  )

# 1b) Compute AUC for each subject (only 2000–3000 ms)
subj_auc_boot <- dta_cf_boot %>%
  filter(
    !is.na(Value),
    time_pt >= 2000,
    time_pt <= 3000
  ) %>%
  mutate(adj = pmax(Value - 1, 0)) %>%
  group_by(Subject, Condition) %>%
  arrange(time_pt) %>%                                # use column time_pt
  summarise(
    auc    = trapz(time_pt, adj),
    .groups = "drop"
  )

# 2) Define statistic function for bootstrap: Team_mean - Individual_mean
boot_diff_auc <- function(data, indices) {
  d <- data[indices, ]  # Resampled data
  means <- d %>%
    group_by(Condition) %>%
    summarise(m = mean(auc), .groups = "drop")
  
  diff <- means$m[means$Condition == "Team"] -
    means$m[means$Condition == "Individual"]
  
  return(diff)
}

# 3) Bootstrap on subj_auc_boot
set.seed(42)  # Fix random seed for reproducibility
boot_out_auc <- boot(
  data      = subj_auc_boot,
  statistic = boot_diff_auc,
  R         = 5000  # 5000 resamples
)

# 4) Compute 95% confidence interval (BCa method)
ci_auc <- boot.ci(boot_out_auc, type = "bca")

# 5) Inspect output
print(boot_out_auc)
print(ci_auc)
