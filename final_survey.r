get_fkm <- function(final) {
    final_new <- as.data.frame(final)
    colnames(final_new)[6] <- "dropout"
    colnames(final_new)[7] <- "not_working"
    colnames(final_new)[8] <- "skipped_weeks"
    colnames(final_new)[9] <- "deployment_frequency"
    colnames(final_new)[10] <- "leadtime"
    colnames(final_new)[11] <- "time_to_restore"
    colnames(final_new)[12] <- "change_failure_rate"
    colnames(final_new)[13] <- "representative"
    colnames(final_new)[14] <- "helped_to_identify_waste"
    colnames(final_new)[15] <- "helped_to_recall"
    colnames(final_new)[16] <- "fill_out_daily_okay"
    colnames(final_new)[17] <- "fill_out_weekly_okay"
    colnames(final_new)[18] <- "day_was_more_accurate"
    colnames(final_new)[19] <- "daily_had_influence_on_weekly"
    colnames(final_new)[20] <- "interest_in_report"
    colnames(final_new)[21] <- "id"

    metadata_columns <- c(6, 7, 8, 21)
    fkm_columns <- c(9, 10, 11, 12, 21)
    metadata <- final_new[metadata_columns]
    fkm <- final_new[fkm_columns]

    df_recode <- c(
        "I don't know / NA" = 0,
        "Between once per month and once every 6 months" = 1,
        "Between once per week and once per month" = 2,
        "Between once per day and once per week" = 3,
        "Between once per hour and once per day" = 4,
        "On demand (multiple deploys per day)" = 5
    )
    fkm$deployment_frequency <- df_recode[fkm$deployment_frequency]

    ttr_lt_recode <- c(
        "I don't know / NA" = 0,
        "More than six moths" = 1,
        "Between one and six moths" = 2,
        "Between one week and one moth" = 3,
        "Between one day and one week" = 4,
        "Less than one day" = 5,
        "Less than one hour" = 6
    )
    fkm$leadtime <- ttr_lt_recode[fkm$leadtime]
    fkm$time_to_restore <- ttr_lt_recode[fkm$time_to_restore]

    cfr_recode <- c(
        "I don't know / NA" = 0,
        "0-15%" = 6,
        "16-30%" = 5,
        "31-45%" = 4,
        "46-60%" = 3,
        "61-75%" = 2,
        "76-100%" = 1
    )
    fkm$change_failure_rate <- cfr_recode[fkm$change_failure_rate]

    fkm$fkm_score <- fkm$change_failure_rate + fkm$time_to_restore + fkm$leadtime + fkm$deployment_frequency
    return(fkm)
}
