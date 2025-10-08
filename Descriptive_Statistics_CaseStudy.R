#-------------------------------
# Descriptive Statistics for TBI Case Study
#-------------------------------

# Background: In this case study, ten healthy control participants (ages 10–21) and one experimental participant with persistent post-concussive symptoms following mTBI 

library(readxl)
CIPARTICIPANTS <- read_excel("CIPARTICIPANTS.xlsx")
View(CIPARTICIPANTS)

#################################################################
#         DATA PREPARATION AND GROUP SPLITTING
###############################################################

# Load required packages
library(dplyr)
library(tidyr)
library(stringr)

# Define the variables of interest
target_vars <- c("Age", "PHQ9", "GAD7", "CGS", "WASIVocab", "WASIMatrix")

# Check which of those variables actually exist in the dataset
have_vars <- intersect(target_vars, names(CIPARTICIPANTS))
missing_vars <- setdiff(target_vars, have_vars)
if (length(missing_vars) > 0) {
  message("⚠️ Missing columns (skipped): ", paste(missing_vars, collapse = ", "))
}

# Convert Date column to proper Date format if it isn't already
if (!inherits(CIPARTICIPANTS$Date, "Date")) {
  suppressWarnings(
    CIPARTICIPANTS$Date <- as.Date(CIPARTICIPANTS$Date)
  )
}

# Some datasets have “TBI_pre” and “TBI_post” in the Group column;
# this code splits those into separate Group and Timepoint columns
if (any(grepl("^TBI_", CIPARTICIPANTS$Group))) {
  CIPARTICIPANTS <- CIPARTICIPANTS %>%
    mutate(
      Timepoint = case_when(
        Group == "TBI_pre"  ~ "Pre",
        Group == "TBI_post" ~ "Post",
        TRUE ~ NA_character_
      ),
      Group = if_else(grepl("^TBI_", Group), "TBI", Group)
    )
}

###############################################################
#           DEFINE COHORTS
###############################################################

# Separate control participants
controls <- CIPARTICIPANTS %>% filter(Group == "Control")

# Isolate the TBI case (ID 1001, Pre & Post)
tbi_case <- CIPARTICIPANTS %>%
  filter(Group == "TBI", ID == 1001) %>%
  arrange(Date)

###############################################################
#           CONTROL DESCRIPTIVE STATISTICS
###############################################################

# Compute descriptive statistics (mean, SD, median, IQR)
# for the selected numeric variables among controls
controldescriptives <- controls %>%
  summarise(
    across(all_of(have_vars),
           list(
             mean   = ~mean(.x, na.rm = TRUE),
             sd     = ~sd(.x,   na.rm = TRUE),
             median = ~median(.x, na.rm = TRUE),
             IQR    = ~IQR(.x, na.rm = TRUE)
           ),
           .names = "{.col}.{.fn}")
  ) %>%
  # Convert from wide to long format for readability
  pivot_longer(everything(),
               names_to = c("Variable", "Stat"),
               names_sep = "\\.",
               values_to = "Value") %>%
  arrange(Variable, Stat)

# Display results
cat("\n📊 Descriptive Statistics for Control Participants (",
    paste(have_vars, collapse = ", "), ")\n")
print(controldescriptives)

###############################################################
#           CASE PRE/POST COMPARISON
###############################################################

# If both pre and post measurements exist, compute the change
if (nrow(tbi_case) >= 2) {
  
  # If Timepoint column is missing, create it based on Date order
  if (!"Timepoint" %in% names(tbi_case) || any(is.na(tbi_case$Timepoint))) {
    tbi_case <- tbi_case %>%
      arrange(Date) %>%
      mutate(Timepoint = c("Pre", "Post")[seq_len(n())])
  }
  
  # Pivot to show Pre, Post, and Change for each measure
  case_change <- tbi_case %>%
    select(Timepoint, all_of(have_vars)) %>%
    pivot_longer(-Timepoint, names_to = "Measure", values_to = "Value") %>%
    pivot_wider(names_from = Timepoint, values_from = Value) %>%
    mutate(Change = Post - Pre) %>%
    arrange(Measure)
  
  cat("\n🧠 Case Participant (ID 1001): Pre/Post Values and Change Scores\n")
  print(case_change)
} else {
  message("Case participant does not have two timepoints; skipping change computation.")
}

###############################################################
#           VISUALIZE WITH BOX PLOTS
###############################################################

# Create boxplots of Controls for each variable
par(mfrow = c(ceiling(length(have_vars)/2), 2))  # Arrange plots in a grid

for (v in have_vars) {
  ctrl_vals <- controls [[v]]
  
  # Skip if both groups are missing data
  if (all(is.na(ctrl_vals))) next
  
  # Single-group boxplot : Controls Only
  boxplot(ctrl_vals,
          names = "Controls",
          main  = paste("Controls:", v),
          ylab  = v,
          col   = "lightblue")
  
  # Jittered individual control points
  good <- !is.na(ctrl_vals)
  points(jitter(rep(1, sum(good)), amount = 0.08),
         ctrl_vals[good],
         pch = 16, col = "blue")
}

# Reset plotting layout
par(mfrow = c(1, 1))

###############################################################
# CASE (ID 1001) SPAGHETTI: PRE → POST ACROSS MEASURES
###############################################################
library(dplyr)
library(tidyr)
library(ggplot2)

# Long format for the chosen variables
case_long <- tbi_case %>%
  select(Timepoint, all_of(have_vars)) %>%
  pivot_longer(-Timepoint, names_to = "Measure", values_to = "Value")

# Build a wide version to draw Pre→Post segments
case_wide <- case_long %>%
  pivot_wider(names_from = Timepoint, values_from = Value) %>%
  filter(!(is.na(Pre) & is.na(Post))) %>%
  mutate(Measure = factor(Measure, levels = have_vars))  # keep your preferred order

# Plot: each measure on x-axis; vertical segment Pre→Post; points at both ends
ggplot(case_wide, aes(x = Measure)) +
  geom_segment(aes(xend = Measure, y = Pre,  yend = Post), linewidth = 1, color = "grey40") +
  geom_point(aes(y = Pre,  color = "Pre"),  size = 3) +
  geom_point(aes(y = Post, color = "Post"), size = 4) +
  scale_color_manual(values = c("Pre" = "#1f77b4", "Post" = "#ff7f0e")) +
  coord_flip() +
  labs(title = "TBI Participant: Pre → Post by Measure",
       x = NULL, y = "Score", color = "Timepoint") +
  theme_minimal(base_size = 12)

#Finish
