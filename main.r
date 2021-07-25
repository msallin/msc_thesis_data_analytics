# update.packages(ask = FALSE)
if (!require(readxl)) install.packages("readxl")
if (!require(dplyr)) install.packages("dplyr")
if (!require(psych)) install.packages("psych")
if (!require(likert)) install.packages("likert")
if (!require(plyr)) install.packages("plyr")

library("dplyr")
library("readxl")
library("likert")
library("plyr")

source("misc/utility.r")
source("misc/metadata.r")
source("survey/final_survey.r")
source("survey/daily_survey.r")
source("survey/weekly_survey.r")
source("survey/load_all.r")
source("analysis/rqx_reporting_interval_analysis.r")
source("analysis/descriptive_stats.r")
source("analysis/rq1_tool_feedback.r")
source("analysis/rq2_xy.r")
source("analysis/rq3_xy.r")

# Load all the data from excel.
# This will populate some global variables:
# daily, weekly, fkm, final, tool_feedback
load_data()

# Descriptive Stats
generate_descriptive_statistics(daily)

# RQ1
generate_feedback_likert_plots(tool_feedback)

# RQ2
generate_daily_weekly_difference_boxplots(daily, weekly)

# RQ3
generate_interval_analysis_lm(daily, weekly)