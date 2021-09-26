# update.packages(ask = FALSE)
if (!require(readxl)) install.packages("readxl") # survey (excel) reading
if (!require(likert)) install.packages("likert") # likert visualization
if (!require(ggpubr)) install.packages("ggpubr") # ggscatter
if (!require(tm)) install.packages("tm")  # for text mining
if (!require(BlandAltmanLeh)) install.packages("BlandAltmanLeh") # for comparison of daily and weekly surveys

library("tm")
library("readxl")
library("likert")
library("ggpubr")
library("BlandAltmanLeh")

source("misc/utility.r")
source("misc/metadata.r")
source("survey/final_survey.r")
source("survey/daily_survey.r")
source("survey/weekly_survey.r")
source("survey/load_all.r")
source("survey/aggregate.r")
source("analysis/0_pre_processing.r")
source("analysis/rq2_tool_feedback.r")
source("analysis/rq3_descriptive_stats.r")
source("analysis/rq4_accuracy_bias.r")
source("analysis/rq5_fkm_waste.r")
source("analysis/rq6_participant_report.r")

# Load all the data from excel. This will populate some global variables:
# daily, weekly, fkm, final, tool_feedback
load_data()

# Pre processing
find_participants_without_final(daily, weekly, final)
find_wrongly_reported_weeks(final, daily)
weekly <- find_missing_reportings(daily, weekly, final)

# Combine the available data into one data set for further processing
aggregated_data <- aggregated_daily_and_weekly_data(daily, weekly)

# RQ2
generate_feedback_likert_plots(tool_feedback)
generate_not_reported_waste_summary(daily)

# RQ3
generate_descriptive_statistics(daily, weekly)

# RQ4
generate_daily_weekly_correlation(aggregated_data)
generate_daily_weekly_difference_boxplots(aggregated_data)
generate_daily_weekly_bias(aggregated_data)

# RQ5
generate_regression_fkm_waste(daily, fkm)

# Participant Reports
generate_participant_reports(aggregated_data)