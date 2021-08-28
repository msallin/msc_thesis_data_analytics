# ***********************************************************
# RQ5: Is the reported amount of waste correlated with the software delivery performance?
# ***********************************************************
# This script does calculate the total reported waste per quantification unit
# And performs a correlation analysis (using spearman, visual and fitting lm)
# between the total reported waste per unit and the reported FKM.
# ***********************************************************

generate_regression_fkm_waste <- function(daily, fkm) {
    filtered_fkm <- subset(fkm, fkm_score > 0)

    correlation_plots <- list()

    delay_columns <- daily[, c("id", get_waste_delay_data_column_names())]
    aggregated_delay <- aggregate(. ~ id, delay_columns, mean)
    correlation_plots[["Delay"]] <- fit_lm(filtered_fkm, aggregated_delay, "Delay")

    time_spent_columns <- daily[, c("id", get_waste_time_spent_column_names())]
    aggregated_time_spent <- aggregate(. ~ id, time_spent_columns, mean)
    correlation_plots[["Time Spent"]] <- fit_lm(filtered_fkm, aggregated_time_spent, "Time Spent")

    stress_column <- daily[, c("id", "stress")]
    aggregated_stress <- aggregate(. ~ id, stress_column, mean)
    correlation_plots[["Stress"]] <- fit_lm(filtered_fkm, aggregated_stress, "Stress")

    customer_column <- daily[, c("id", "customer")]
    aggregated_customer <- aggregate(. ~ id, customer_column, mean)
    correlation_plots[["Productivity"]] <- fit_lm(filtered_fkm, aggregated_customer, "Customer")

    arranged_plot <- ggarrange(plotlist = correlation_plots)
    suppressMessages(ggsave(plot = arranged_plot, "results/rq5_fkm_waste_correlation.pdf", device = "pdf"))
}

fit_lm <- function(fkm, waste, title) {
    lm_data_frame <- prepare_data(fkm, waste)
    plot <- ggscatter(lm_data_frame, x = "score", y = "total",
        conf.int = TRUE, title = title, size=1) +
        geom_smooth(formula = y ~ x, method = "lm", colour = "blue", size = 0.5) +
        stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.x = 10) # Show R^2 of lm fit instead of rho
        # stat_cor(method = "pearson", cor.coef.name = "rho") # Show rho of pearson correlation
    return(plot)
}

prepare_data <- function(fkm, waste) {
    fkm_waste <- merge(fkm, waste, by = "id")
    
    total <- 0
    # If there is only one column, now rowSum is necessary but only select the column
    if(length(fkm_waste) == 7) {
        total <- fkm_waste[, 7]
    } else {
        # The first six columns are fkm or metadata releated
       total <- rowSums(fkm_waste[, 7:length(fkm_waste)])
    }
    
    score <- fkm_waste$fkm_score

    # We are only interested in the fkm score and the summed waste
    lm_data_frame <- data.frame(score, total)
    return(lm_data_frame)
}
