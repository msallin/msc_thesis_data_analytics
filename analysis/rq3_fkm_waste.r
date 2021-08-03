# ***********************************************************
# RQ3: Is the reported amount of waste correlated with the software delivery performance?
# ***********************************************************
# This script does calculate the total reported waste per quantification unit
# And performs a correlation analysis (using spearman, visual and fitting lm)
# between the total reported waste per unit and the reported FKM.
# ***********************************************************

generate_regression_fkm_waste <- function(daily, fkm) {
    filtered_fkm <- subset(fkm, fkm_score > 0)

    correlation_plots <- list()

    delay_columns <- daily[, c("id", get_waste_delay_data_column_names())]
    aggregated_delay <- aggregate(. ~ id, delay_columns, sum)
    correlation_plots[["Delay"]] <- spearman_correlation(filtered_fkm, aggregated_delay, "Delay")

    time_spent_columns <- daily[, c("id", get_waste_time_spent_column_names())]
    aggregated_time_spent <- aggregate(. ~ id, time_spent_columns, sum)
    correlation_plots[["Time Spent"]] <- spearman_correlation(filtered_fkm, aggregated_time_spent, "Time Spent")

    stress_column <- daily[, c("id", "stress")]
    aggregated_stress <- aggregate(. ~ id, stress_column, sum)
    correlation_plots[["Stress"]] <- spearman_correlation(filtered_fkm, aggregated_stress, "Stress")

    customer_column <- daily[, c("id", "customer")]
    aggregated_customer <- aggregate(. ~ id, customer_column, sum)
    correlation_plots[["Productivity"]] <- spearman_correlation(filtered_fkm, aggregated_customer, "Customer")

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
    plot <- ggscatter(lm_data_frame, x = "score", y = "total",
        conf.int = TRUE, title = title, size=1) +
        geom_smooth(formula = y ~ x, method = "lm", colour = "blue", size = 0.5) +
        stat_cor(method = "spearman", cor.coef.name = "rho")
    return(plot)
}
