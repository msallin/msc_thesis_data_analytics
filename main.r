# TODO-List
# 1. Clean up data
# 1.1 Has a participant filled out the daily or weekly two times? If yes, take the latest
# 1.2 Exclude weeks which were not reported fully
# 2. Finish descriptive statistics (include cleanup stuff into this)

# update.packages(ask = FALSE)
if (!require(readxl)) install.packages("readxl") # survey (excel) reading
if (!require(likert)) install.packages("likert") # likert visualization
if (!require(ggpubr)) install.packages("ggpubr") # ggscatter
if (!require(tm))install.packages("tm")  # for text mining
if (!require(wordcloud)) install.packages("wordcloud") # word-cloud generator 
if (!require(RColorBrewer)) install.packages("RColorBrewer") # color palettes

library("tm")
# library("wordcloud")
# library("RColorBrewer")
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
source("analysis/rq21_tool_feedback.r")
source("analysis/rq22_accuracy.r")
source("analysis/rq23_bias.r")
source("analysis/rq3_fkm_waste.r")

# Load all the data from excel.
# This will populate some global variables:
# daily, weekly, fkm, final, tool_feedback
load_data()

# Descriptive Stats
generate_descriptive_statistics(daily, weekly)

# RQ2
generate_feedback_likert_plots(tool_feedback) # 2.1
generate_not_reported_waste_summary(daily) # 2.1
generate_daily_weekly_correlation(daily, weekly) # 2.2
generate_daily_weekly_difference_boxplots(daily, weekly) # 2.3

# RQ3
generate_regression_fkm_waste(daily, fkm)