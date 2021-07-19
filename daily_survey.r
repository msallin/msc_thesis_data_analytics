
get_daily <- function(daily_sheet) {
    daily_sheet_new <- as.data.frame(daily_sheet)
    colnames(daily_sheet_new)[2] <- "date"
    colnames(daily_sheet_new)[6] <- "productivity"
    colnames(daily_sheet_new)[7] <- "stress"
    colnames(daily_sheet_new)[8] <- "rework"
    colnames(daily_sheet_new)[9] <- "manual"
    colnames(daily_sheet_new)[10] <- "delay_missing_automation"
    colnames(daily_sheet_new)[11] <- "communication"
    colnames(daily_sheet_new)[12] <- "administrative_demands"
    colnames(daily_sheet_new)[13] <- "administrative_demands_delay"
    colnames(daily_sheet_new)[14] <- "other_duties"
    colnames(daily_sheet_new)[15] <- "customer"
    colnames(daily_sheet_new)[16] <- "cognitive_load"
    colnames(daily_sheet_new)[17] <- "complex_solution"
    colnames(daily_sheet_new)[18] <- "knowledge"
    colnames(daily_sheet_new)[19] <- "stress_cause"
    colnames(daily_sheet_new)[20] <- "main_waste_cause"
    colnames(daily_sheet_new)[21] <- "not_reported"
    colnames(daily_sheet_new)[22] <- "id"

    duration_recode <- c(
        "0h" = 0,
        "0" = 0,
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
    daily_sheet_new$delay_missing_automation <- delay_recode[daily_sheet_new$delay_missing_automation]
    daily_sheet_new$administrative_demands_delay <- delay_recode[daily_sheet_new$administrative_demands_delay]


    customer_recode <- c(
        "N/A" = 0,
        "Completely confident" = 5,
        "Somewhat confident" = 4,
        "Neutral" = 3,
        "Somewhat insecure" = 2,
        "Completely insecure" = 1
    )
    daily_sheet_new$customer <- customer_recode[daily_sheet_new$customer]

    daily_sheet_new$totalWaste <- rowSums(daily_sheet_new[6:18])
    daily_sheet_new$calendarWeek <- strftime(daily_sheet_new$date, format = "%V")

    return(daily_sheet_new)
}