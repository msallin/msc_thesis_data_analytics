
get_weekly <- function(weekly_sheet) {
    weekly_sheet_new <- as.data.frame(weekly_sheet)
    colnames(weekly_sheet_new)[2] <- "date"
    colnames(weekly_sheet_new)[6] <- "productivity"
    colnames(weekly_sheet_new)[7] <- "stress"
    colnames(weekly_sheet_new)[8] <- "rework"
    colnames(weekly_sheet_new)[9] <- "manual"
    colnames(weekly_sheet_new)[10] <- "missing_automation_delay"
    colnames(weekly_sheet_new)[11] <- "communication"
    colnames(weekly_sheet_new)[12] <- "administrative_demands"
    colnames(weekly_sheet_new)[13] <- "administrative_demands_delay"
    colnames(weekly_sheet_new)[14] <- "other_duties"
    colnames(weekly_sheet_new)[15] <- "customer"
    colnames(weekly_sheet_new)[16] <- "cognitive_load"
    colnames(weekly_sheet_new)[17] <- "complex_solution"
    colnames(weekly_sheet_new)[18] <- "knowledge"
    colnames(weekly_sheet_new)[19] <- "stress_cause"
    colnames(weekly_sheet_new)[20] <- "main_waste_cause"
    colnames(weekly_sheet_new)[21] <- "id"

    customer_recode <- c(
        "N/A" = 0,
        "Completely confident" = 5,
        "Somewhat confident" = 4,
        "Neutral" = 3,
        "Somewhat insecure" = 2,
        "Completely insecure" = 1
    )
    weekly_sheet_new$customer <- customer_recode[weekly_sheet_new$customer]

    weekly_sheet_new[6:18] <- lapply(weekly_sheet_new[6:18], as.numeric)

    weekly_sheet_new$totalWaste <- rowSums(weekly_sheet_new[6:18])
    weekly_sheet_new$calendarWeek <- strftime(weekly_sheet_new$date, format = "%V")

    return(weekly_sheet_new)
}