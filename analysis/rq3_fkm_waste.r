generate_regression_fkm_waste <- function(daily, fkm) {
    filtered_fkm <- subset(fkm, fkm_score > 0)

    correlation_plots <- list()

    #plot_to_file_start("rq_3_waste_regression")
    #op <- par(mfrow = c(2, 2))

    delay_columns <- daily[, c("id", get_waste_delay_data_column_names())]
    aggregated_delay <- aggregate(. ~ id, delay_columns, sum)
    correlation_plots[["Delay"]] <- spearman_correlation(filtered_fkm, aggregated_delay, "Delay")
    # fit_lm(filtered_fkm, aggregated_delay, "Delay")

    time_spent_columns <- daily[, c("id", get_waste_time_spent_column_names())]
    aggregated_time_spent <- aggregate(. ~ id, time_spent_columns, sum)
    correlation_plots[["Time Spent"]] <- spearman_correlation(filtered_fkm, aggregated_time_spent, "Time Spent")
    # fit_lm(filtered_fkm, aggregated_time_spent, "Time Spent")

    stress_column <- daily[, c("id", "stress")]
    aggregated_stress <- aggregate(. ~ id, stress_column, sum)
    correlation_plots[["Stress"]] <- spearman_correlation(filtered_fkm, aggregated_stress, "Stress")
    # fit_lm(filtered_fkm, aggregated_stress, "Stress")

    productivity_column <- daily[, c("id", "productivity")]
    aggregated_productivity <- aggregate(. ~ id, productivity_column, sum)
    correlation_plots[["Productivity"]] <- spearman_correlation(filtered_fkm, aggregated_productivity, "Productivity")
    # fit_lm(filtered_fkm, aggregated_productivity, "Productivity")

    # par(op)
    # plot_to_file_end()
    arranged_plot <- ggarrange(plotlist = correlation_plots)
    suppressMessages(ggsave(plot = arranged_plot, "results/rq_3_fkm_waste_correlation.pdf", device = "pdf"))
}

prepare_data <- function(fkm, waste) {
   fkm_waste <- merge(fkm, waste, by = "id")
    
    # The first six columns are fkm or metadata releated
    total <- rowSums(fkm_waste[, 6:length(fkm_waste)])
    score <- fkm_waste$fkm_score 

    # We are only interested in the fkm score and the summed waste
    lm_data_frame <- data.frame(score, total)

    return(lm_data_frame)
}

fit_lm <- function(fkm, waste, title) {
    lm_data_frame <- prepare_data(fkm, waste)

    lm <- lm(score ~ ., data = lm_data_frame)
    summary(lm)
    plot(score ~ ., data = lm_data_frame, 
        main = title,
        xlab = "Waste", ylab = "FKM Score",
        col = "grey",
        pch = 20, cex = 1.5)
    abline(lm, lwd = 1)
}

spearman_correlation <- function(fkm, waste, title) {
    lm_data_frame <- prepare_data(fkm, waste)
    plot <- ggscatter(lm_data_frame, x = "score", y = "total", conf.int = TRUE,
        cor.coef = TRUE, cor.method = "spearman", title = title,
        size=1) + geom_smooth(formula = y ~ x, method = "lm")
    return(plot)
}
