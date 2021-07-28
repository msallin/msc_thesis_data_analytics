# ***********************************************************
# RQ2.3: Is there a systematic bias when self-reporting weekly?
# ***********************************************************
# TODO: Describe file
# ***********************************************************

generate_interval_analysis_lm <- function(daily, weekly) {
   week_aggreations <- interval_analysis_data_prep(daily, weekly)
   model <- lm(daily_time_waste ~ weekly_time_waste, data = week_aggreations)
   newx <- seq(min(week_aggreations$weekly_time_waste), max(week_aggreations$weekly_time_waste), by = 0.05)
   conf_interval <- predict(model,
      newdata = data.frame(weekly_time_waste = newx), interval = "confidence",
      level = 0.95
   )
   summary(model)

   plot_to_file_start("rq_2_3_weekly_daily_lm")

   plot(daily_time_waste ~ weekly_time_waste,
      data = week_aggreations, col = "grey", pch = 20, cex = 1.5,
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