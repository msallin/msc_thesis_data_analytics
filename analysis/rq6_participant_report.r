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
    # id <- "3af8086e-b320-4c3e-bc8a-8edff40fec93"
    for (id in unique(aggregated_data$participant_id)) {
        participant <- subset(aggregated_data, participant_id == id)

        file_name <- "participant_reports/" %&% id %&% ".md"
        full_name <- recreate_results_file(file_name)

        writeLine("# Personal Waste Report", full_name)

        writeLine("## Overview", full_name, emptyLine = TRUE)
        writeLine("Provides an overview of your reporting period.  ", full_name)
        writeLine("Reported calendar weeks: " %&% paste(participant$calendar_week, collapse = ", ") %&% "  ", full_name, emptyLine = TRUE)
        writeLine("Total time waste: " %&% sum(participant$daily_time_waste) %&% "h  ", full_name)
        writeLine("Total delay: " %&% sum(participant$daily_time_delay) %&% "h  ", full_name)
        writeLine("Average customer confidence*: " %&% round(mean(participant$daily_customer), 1) %&% "  ", full_name)
        writeLine("Average stress score**: " %&% round(mean(participant$daily_stress), 1) %&% "  ", full_name)

        writeLine("## Waste by category", full_name, emptyLine = TRUE)
        writeLine("Shows how much waste measured in hours you reported by category.  ", full_name)

        writeLine("Time spent", full_name, emptyLine = TRUE)
        time_spent <- participant[, daily_time_waste_columns]
        sum_time_spent <- sort(colSums(time_spent[, 1:length(time_spent)]), decreasing = TRUE)
        total_sum_time_spent <- sum(time_spent)
        for (i in seq(1, length(time_spent), +1)) {
            daily_avg_min <- round(sum_time_spent[i] / sum(participant$number_of_days_reported) * 60, 0)
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
            daily_avg_min <- round(sum_delay[i] / sum(participant$number_of_days_reported) * 60, 0)
            if(total_sum_delay == 0) {
                fraction <- 0
            } else {
                fraction <- round((sum_delay[i] / total_sum_delay) * 100, 2)
            }
            txt <- "  " %&% i %&% ". " %&%  waste_title[substring(labels(sum_delay[i]), nchar("daily_") + 1)] %&% " " %&% fraction %&% "% (" %&% format_time_span(sum_delay[i] * 60) %&% ", "  %&% format_time_span(daily_avg_min) %&% "/day" %&% ")"
            writeLine(txt, full_name)
        }

        writeLine("## Calendar Weeks", full_name, emptyLine = TRUE)
        writeLine("Shows how much waste per category you reported per calendar week.  ", full_name)
        writeLine("The table contains your daily and weekly reports to enable reasoning about accuracy.  ", full_name)

        writeLine(paste(c("| ", participant$calendar_week), collapse = " | ") %&% " | ", full_name, emptyLine = TRUE)
        writeLine(strrep("|---", length(participant$calendar_week) + 1) %&% "|", full_name)
        writeLine(paste(c("| Missing daily reports", ifelse(participant$missing_data, "Yes", "No")), collapse = " | ") %&% " | ", full_name)
        writeLine(paste(c("| Wasted daily (h)", participant$daily_time_waste), collapse = " | ") %&% " | ", full_name)
        writeLine(paste(c("| Wasted weekly (h)", participant$weekly_time_waste), collapse = " | ") %&% " | ", full_name)
        writeLine(paste(c("| Delay daily (h)", participant$daily_time_delay), collapse = " | ") %&% " | ", full_name)
        writeLine(paste(c("| Delay weekly (h)", participant$weekly_time_delay), collapse = " | ") %&% " | ", full_name)
        writeLine(paste(c("| Stress* mean daily", participant$daily_stress), collapse = " | ") %&% " | ", full_name)
        writeLine(paste(c("| Stress* weekly", participant$weekly_stress), collapse = " | ") %&% " | ", full_name)
        writeLine(paste(c("| Customer** confidence mean daily", participant$daily_customer), collapse = " | ") %&% " | ", full_name)
        writeLine(paste(c("| Customer** confidence mean weekly", participant$weekly_customer), collapse = " | ") %&% " | ", full_name)

        writeLine(" ## Averages   ", full_name, emptyLine = TRUE)
        writeLine("Shows averages of reported waste, to enable you to compare against others.  ", full_name)
        writeLine("  ", full_name)
        writeLine("Time waste by category  ", full_name)
        writeLine("1. Rework 20.31% (217h, 57min/day)", full_name)
        writeLine("2. Other Duties 14.69% (157h, 41min/day)", full_name)
        writeLine("3. Knowledge 13.48% (144h, 38min/day)", full_name)
        writeLine("4. Cognitive Load 11.61% (124h, 32min/day)", full_name)
        writeLine("5. Communication 10.62% (113.5h, 30min/day)", full_name)
        writeLine("6. Admin Demands 10.48% (112h, 29min/day)", full_name)
        writeLine("7. Manual Work 9.41% (100.5h, 26min/day)", full_name)
        writeLine("8. Complex Solution 9.41% (100.5h, 26min/day)", full_name)
        writeLine("  ", full_name)
        writeLine("Time waste by category  ", full_name)
        writeLine("1. Admin Demands Delay 84.4% (822.6h, 3.6h/day)", full_name)
        writeLine("2. Automation Delay 15.6% (152.1h, 40min/day)", full_name)
        writeLine("  ", full_name)
        writeLine("Stress  ", full_name)
        writeLine("Mean: 2.3, Median: 2; Standard deviation: 2.3; Min: 0; Max: 8  ", full_name)
        writeLine("  ", full_name)
        writeLine("Customer confidence  ", full_name)
        writeLine("Mean: 3.9; Median: 4; Standard deviation: 1  ", full_name)
        writeLine("Distribution:  ", full_name)
        writeLine("1. 2.8%  ", full_name)
        writeLine("2. 4.7%  ", full_name)
        writeLine("3. 21.9%  ", full_name)
        writeLine("4. 44.2%  ", full_name)
        writeLine("5. 26.5%  ", full_name)

        writeLine("## Remarks  ", full_name, emptyLine = TRUE)
        writeLine("\\* 0 = Not stressed at all, 10 = Extremely stressed  ", full_name)
        writeLine("** Confidence doing the right things. 1 = Completely insecure, 5 = Completely confident  ", full_name)
    }
}
