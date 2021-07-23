# update.packages(ask = FALSE)
# install.packages("readxl")
# install.packages("dplyr")

library("dplyr")
library("readxl")

source("utility.r")
source("final_survey.r")
source("daily_survey.r")
source("weekly_survey.r")
source("reporting_interval_analysis.r")

load_data <- function() {
    result_file <- "C:\\Users\\marcs\\OneDrive\\Documents\\School\\6 - FHNW MSc Engineering\\Project 9 Thesis\\results.xlsx" # file.choose()
    daily_sheet <- read_excel(result_file, sheet = "Daily Survey")
    weekly_sheet <- read_excel(result_file, sheet = "Weekly Survey")
    final_sheet <- read_excel(result_file, sheet = "Final Survey")

    daily <<- get_daily(daily_sheet)
    weekly <<- get_weekly(weekly_sheet)

    fkm <<- get_fkm(final_sheet)
}

load_data()

# ***********************************************************
# RQ1: How can a self-reporting tool help to identify waste?
# ***********************************************************
# TODO

aggregated_data <- interval_analysis_data_prep(daily, weekly)

# ***********************************************************
# RQ2: How accurate is self-reporting daily vs. weekly?
# ***********************************************************
column_names <- get_data_column_names()
results <- calculate_weekly_daily_difference(aggregated_data, column_names)
print_weekly_daily_difference_overview(results, column_names)

# Generate histogram of different waste types
# hist(unlist(results["rework"], use.names = FALSE))

# Generate boxplot for each waste type
# par(mfrow = c(4, 4)) # Create a 4 x 4 plotting matrix
# for (i in seq_len(length(results))) {
#     name <- names(results[i])
# 1. Open jpeg file
# jpeg("RQ3/" %&% name %&% ".jpg", width = 350, height = 350)
# 2. Create the plot
# boxplot(results[i], main = name, notch = FALSE)
# 3. Close the file
# dev.off()
# }

# ***********************************************************
# RQ3: Is there a systematic bias when self-reporting weekly?
# ***********************************************************
interval_analysis_lm(aggregated_data)