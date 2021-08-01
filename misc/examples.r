# TODO: Delete after finishing the analysis...

analysis_something <- function() {
    hist(daily$stress)
    hist(daily$productivity)
    pairs(~ totalWaste + stress + productivity, data = daily, main = "Simple Scatterplot Matrix")

    fit <- lm(stress ~ log(totalWaste), data = daily)
    summary(fit)

    plot(stress ~ log(totalWaste),
        data = daily, col = "grey", pch = 20, cex = 1.5,
        main = "Stress at Swiss Post, By Experienced Waste"
    )
    abline(fit, col = "darkorange", lwd = 2)

    # compare models
    fit1 <- lm(totalWaste ~ stress + productivity, data = daily)
    fit2 <- lm(totalWaste ~ stress, data = daily)
    fit3 <- lm(totalWaste ~ productivity, data = daily)
    anova(fit1, fit2, fit3)

    fkm_waste <- merge(fkm, waste, by = "id")
    # plot(fkm_waste$km_score, fkm_waste$delay)
    pairs(~ fkm_score + delay + stress + time_lost + customer_focus,
        data = fkm_waste,
        main = "Simple Scatterplot Matrix"
    )
}


generate_interval_analysis_lm <- function(aggregated_data) {
   model <- lm(daily_time_waste ~ weekly_time_waste, data = aggregated_data)
   newx <- seq(min(aggregated_data$weekly_time_waste), max(aggregated_data$weekly_time_waste), by = 0.05)
   conf_interval <- predict(model,
      newdata = data.frame(weekly_time_waste = newx), interval = "confidence",
      level = 0.95
   )
   summary(model)

   plot_to_file_start("rq_2_3_weekly_daily_lm")

   plot(daily_time_waste ~ weekly_time_waste,
      data = aggregated_data, col = "grey", pch = 20, cex = 1.5,
      main = "Weekly Aggregated vs Reported"
   )
   abline(model, col = "red", lwd = 2)
   matlines(newx, conf_interval[, 2:3], col = "blue", lty = 2)

   plot_to_file_end()

   plot_to_file_start("rq_2_3_weekly_daily_lm_residuals")

   op <- par(mfrow = c(2, 2))
   for (i in 1:4) {
      plot(model, which = i)
   }

   par(op)

   plot_to_file_end()
}