# update.packages(ask = FALSE)
if (!require(readxl)) install.packages("readxl") # survey (excel) reading
# if (!require(dplyr)) install.packages("dplyr") # survey (excel) reading
if (!require(psych)) install.packages("psych") # likert visualization
if (!require(likert)) install.packages("likert") # likert visualization
# if (!require(plyr)) install.packages("plyr")
if (!require(ggpubr)) install.packages("ggpubr") # ggscatter


library("readxl")
library("likert")
library("ggpubr")

source("misc/utility.r")
source("misc/metadata.r")
source("survey/final_survey.r")
source("survey/daily_survey.r")
source("survey/weekly_survey.r")
source("survey/load_all.r")
source("analysis/rqx_reporting_interval_analysis.r")
source("analysis/descriptive_stats.r")
source("analysis/rq1_tool_feedback.r")
source("analysis/rq2_accuracy.r")
source("analysis/rq3_bias.r")

# Load all the data from excel.
# This will populate some global variables:
# daily, weekly, fkm, final, tool_feedback
load_data()

# Descriptive Stats
generate_descriptive_statistics(daily)

# RQ1
generate_feedback_likert_plots(tool_feedback)

# RQ2: How accurate is self-reporting daily vs. weekly?
generate_daily_weekly_difference_boxplots(daily, weekly)
generate_daily_weekly_correlation(daily, weekly)

# RQ3
generate_interval_analysis_lm(daily, weekly)