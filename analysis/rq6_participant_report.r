# ***********************************************************
# Participant Report:
# ***********************************************************
# NOT PART OF STUDY!
# Generates the individual candidate report.
# ***********************************************************

generate_participant_reports <- function(aggregated_data) {
    daily_time_waste_columns <- paste0("daily_", get_waste_time_spent_column_names())
    daily_time_delay_columns <- paste0("daily_", get_waste_delay_data_column_names())

    # id <- "ce1ce7e7-07ef-413c-adec-64378049a128"
    for (id in unique(aggregated_data$participant_id)) {
        participant <- subset(aggregated_data, participant_id == id)

        file_name <- "participant_reports/" %&% id %&% ".md"
        full_name <- recreate_results_file(file_name)

        writeLine("# Personal Waste Report", full_name)

        writeLine("## Overview", full_name, emptyLine = TRUE)
        writeLine("Reported calendar weeks: " %&% paste(participant$calendar_week, collapse = ", ") %&% "  ", full_name, emptyLine = TRUE)
        writeLine("Total time waste: " %&% sum(participant$daily_time_waste) %&% "  ", full_name)
        writeLine("Total delay: " %&% sum(participant$daily_time_delay) %&% "  ", full_name)
        writeLine("Average customer certainty: " %&% round(mean(participant$daily_customer), 1) %&% "  ", full_name)
        writeLine("Average stress score: " %&% round(mean(participant$daily_stress), 1) %&% "  ", full_name)

        writeLine("## Waste by category", full_name, emptyLine = TRUE)

        writeLine("Time spent", full_name, emptyLine = TRUE)
        time_spent <- participant[, daily_time_waste_columns]
        sum_time_spent <- sort(colSums(time_spent[, 1:length(time_spent)]), decreasing = TRUE)
        total_sum_time_spent <- sum(time_spent)
        for (i in seq(1, length(time_spent), +1)) {
            daily_avg_min <- round(sum_time_spent[i] / nrow(time_spent) * 60, 0)
            if(total_sum_time_spent == 0) {
                fraction <- 0
            } else {
                fraction <- round((sum_time_spent[i] / total_sum_time_spent) * 100, 2)
            }
            txt <- "  " %&% i %&% ". " %&% waste_title[substring(labels(sum_time_spent[i]), nchar("daily_") + 1)] %&% " " %&% fraction %&% "% (" %&% format_time_span(sum_time_spent[i] * 60) %&% ", "  %&% format_time_span(daily_avg_min) %&% "/day" %&% ")"
            writeLine(txt, full_name)
        }

        writeLine("Delay", full_name, emptyLine = TRUE)
        delay <- participant[, daily_time_delay_columns]
        sum_delay <- sort(colSums(delay[, 1:length(delay)]), decreasing = TRUE)
        total_sum_delay <- sum(sum_delay)
        for (i in seq(1, length(delay), + 1)) {
            daily_avg_min <- round(sum_delay[i] / nrow(delay) * 60, 0)
            if(total_sum_delay == 0) {
                fraction <- 0
            } else {
                fraction <- round((sum_delay[i] / total_sum_delay) * 100, 2)
            }
            txt <- "  " %&% i %&% ". " %&%  waste_title[substring(labels(sum_delay[i]), nchar("daily_") + 1)] %&% " " %&% fraction %&% "% (" %&% format_time_span(sum_delay[i] * 60) %&% ", "  %&% format_time_span(daily_avg_min) %&% "/day" %&% ")"
            writeLine(txt, full_name)
        }

        writeLine("## Calendar Weeks", full_name, emptyLine = TRUE)

        writeLine(paste(c("| ", participant$calendar_week), collapse = " | ") %&% " | ", full_name, emptyLine = TRUE)
        writeLine(strrep("|---", length(participant$calendar_week) + 1) %&% "|", full_name)
        writeLine(paste(c("| Missing daily", participant$missing_data), collapse = " | ") %&% " | ", full_name)
        writeLine(paste(c("| Hours wasted daily", participant$daily_time_waste), collapse = " | ") %&% " | ", full_name)
        writeLine(paste(c("| Hours wasted weekly", participant$weekly_time_waste), collapse = " | ") %&% " | ", full_name)
        writeLine(paste(c("| Hours delay daily", participant$daily_time_delay), collapse = " | ") %&% " | ", full_name)
        writeLine(paste(c("| Hours delay weekly", participant$weekly_time_delay), collapse = " | ") %&% " | ", full_name)
        writeLine(paste(c("| Stress mean daily", participant$daily_stress), collapse = " | ") %&% " | ", full_name)
        writeLine(paste(c("| Stress weekly", participant$weekly_stress), collapse = " | ") %&% " | ", full_name)
        writeLine(paste(c("| Customer confidence mean daily", participant$daily_customer), collapse = " | ") %&% " | ", full_name)
        writeLine(paste(c("| Customer confidence mean weekly", participant$weekly_customer), collapse = " | ") %&% " | ", full_name)
    }
}

format_time_span <- function(time_in_min) {
    if(is.na(time_in_min)) {
        return("-")
    }
    if(time_in_min > 59) {
        result <- round(time_in_min/60, 1) %&% "h"
        return(result)
    } else {
        result <- time_in_min %&% "min"
        return(result)
    }
}