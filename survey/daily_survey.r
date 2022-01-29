# ***********************************************************
# Daily Survey
# ***********************************************************
# This script reads and prepares the daily survey data for further processing.
# ***********************************************************

get_daily <- function(daily_sheet) {

    names <- c("productivity", "stress", "rework", "manual", "missing_automation_delay", "communication", "administrative_demands", "administrative_demands_delay", "other_duties", "customer", "cognitive_load", "complex_solution", "knowledge")

    daily_sheet_new <- as.data.frame(daily_sheet)
    colnames(daily_sheet_new)[1] <- "record_id"
    colnames(daily_sheet_new)[2] <- "date"
    colnames(daily_sheet_new)[6] <- names[1]
    colnames(daily_sheet_new)[7] <- names[2]
    colnames(daily_sheet_new)[8] <- names[3]
    colnames(daily_sheet_new)[9] <- names[4]
    colnames(daily_sheet_new)[10] <- names[5]
    colnames(daily_sheet_new)[11] <- names[6]
    colnames(daily_sheet_new)[12] <- names[7]
    colnames(daily_sheet_new)[13] <- names[8]
    colnames(daily_sheet_new)[14] <- names[9]
    colnames(daily_sheet_new)[15] <- names[10]
    colnames(daily_sheet_new)[16] <- names[11]
    colnames(daily_sheet_new)[17] <- names[12]
    colnames(daily_sheet_new)[18] <- names[13]
    colnames(daily_sheet_new)[19] <- "stress_cause"
    colnames(daily_sheet_new)[20] <- "main_waste_cause"
    colnames(daily_sheet_new)[21] <- "not_reported"
    colnames(daily_sheet_new)[22] <- "id"

    duration_recode <- c(
        "0h" = 0,
        "0" = 0, # Error in survey which was corrected during the study.
        "<1h" = 1,
        "1-2h" = 2,
        "2-4h" = 3,
        "4-6h" = 4,
        "6-8h" = 5,
        ">8h" = 6
    )
    daily_sheet_new$rework <- duration_recode[daily_sheet_new$rework]
    daily_sheet_new$manual <- duration_recode[daily_sheet_new$manual]
    daily_sheet_new$communication <- duration_recode[daily_sheet_new$communication]
    daily_sheet_new$administrative_demands <- duration_recode[daily_sheet_new$administrative_demands]
    daily_sheet_new$other_duties <- duration_recode[daily_sheet_new$other_duties]
    daily_sheet_new$cognitive_load <- duration_recode[daily_sheet_new$cognitive_load]
    daily_sheet_new$knowledge <- duration_recode[daily_sheet_new$knowledge]
    daily_sheet_new$complex_solution <- duration_recode[daily_sheet_new$complex_solution]

    delay_recode <- c(
        "0h" = 0,
        "Up to 2h" = 1,
        "Up to 4h" = 2,
        "Up to 8h" = 3,
        "Up to a few days" = 4,
        "Up to a week" = 5,
        "Up to a month" = 6,
        "Multiple months" = 7
    )
    daily_sheet_new$missing_automation_delay <- delay_recode[daily_sheet_new$missing_automation_delay]
    daily_sheet_new$administrative_demands_delay <- delay_recode[daily_sheet_new$administrative_demands_delay]

    customer_recode <- c(
        "N/A" = 0,
        "Completely confident" = 1,
        "Somewhat confident" = 2,
        "Neutral" = 3,
        "Somewhat insecure" = 4,
        "Completely insecure" = 5
    )
    daily_sheet_new$customer <- customer_recode[daily_sheet_new$customer]

    daily_sheet_new$totalWaste <- rowSums(daily_sheet_new[6:18])
    daily_sheet_new$calendarWeek <- strftime(daily_sheet_new$date, format = "%V")

    return(daily_sheet_new)
}
