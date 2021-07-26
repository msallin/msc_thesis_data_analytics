analysis_something <- function(daily, fkm) {
    filtered_fkm <- subset(fkm, fkm_score > 0)

    op <- par(mfrow = c(2, 2))
    delay_columns <- daily[, c("id", get_waste_delay_data_column_names())]
    aggregated_delay <- aggregate(. ~ id, delay_columns, sum)
    fit_lm(filtered_fkm, aggregated_delay, "Delay")

    time_spent_columns <- daily[, c("id", get_waste_time_spent_column_names())]
    aggregated_time_spent <- aggregate(. ~ id, time_spent_columns, sum)
    fit_lm(filtered_fkm, aggregated_time_spent, "Time Spent")

    stress_column <- daily[, c("id", "stress")]
    aggregated_stress <- aggregate(. ~ id, stress_column, sum)
    fit_lm(filtered_fkm, aggregated_stress, "Stress")

    productivity_column <- daily[, c("id", "productivity")]
    aggregated_productivity <- aggregate(. ~ id, productivity_column, sum)
    fit_lm(filtered_fkm, aggregated_productivity, "Productivity")
}

fit_lm <- function(fkm, waste, title) {
    fkm_waste <- merge(fkm, waste, by = "id")
    
    # The first six columns are fkm or metadata releated
    total <- rowSums(fkm_waste[, 6:length(fkm_waste)])
    score <- fkm_waste$fkm_score 

    # We are only interested in the fkm score and the summed waste
    lm_data_frame <- data.frame(score, total)

    lm <- lm(score ~ ., data = lm_data_frame)
    summary(lm)
    plot(score ~ ., data = lm_data_frame, 
        main = title,
        xlab = "Waste", ylab = "FKM Score",
        col = "grey",
        pch = 20, cex = 1.5)
    abline(lm, lwd = 1)
}