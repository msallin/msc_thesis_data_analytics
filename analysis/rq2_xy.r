# ***********************************************************
# RQ2: How accurate is self-reporting daily vs. weekly?
# ***********************************************************


generate_daily_weekly_difference_boxplots <- function(daily, weekly) {
   aggregated_data <- interval_analysis_data_prep(daily, weekly)
   column_names <- get_data_column_names()
   results <- calculate_weekly_daily_difference(aggregated_data, column_names)
   print_weekly_daily_difference_overview(results, column_names)
}

calculate_weekly_daily_difference <- function(aggregated_data, column_names) {
   results <- list()
   for (name in get_data_column_names()) {
      columns <- c("weekly_" %&% name, "daily_" %&% name)
      one_waste_category <- aggregated_data[, columns]

      # Daily - Weekly
      results[name] <- one_waste_category[1] - one_waste_category[2]
   }

   return(results)
}

print_weekly_daily_difference_overview <- function(results, column_names) {
   plot_to_file_start("weekly_daily_diff_overview")

   # Outcomment to generate a a stripchart above the boxplot
   # par(mfrow = c(2, 1))
   # stripchart(results, group.names = column_names, vertical = TRUE)

   boxplot(results,
      main = "Weekly vs Daily Aggregate",
      ylab = "Difference from daily aggregate (h)",
      las = 2
      # notch = TRUE
   )

   abline(h = 0, col = "red", lwd = 0.5, lty = 2)

   plot_to_file_end()
}

# Generate histogram of different waste types
# hist(unlist(results["rework"], use.names = FALSE))

# Generate boxplot for each waste type
# par(mfrow = c(4, 4)) # Create a 4 x 4 plotting matrix
# for (i in seq_len(length(results))) {
#     name <- names(results[i])
# 1. Open jpeg file
# jpeg("RQ3/" %&% name %&% ".jpg", width = 350, height = 350)
# 2. Create the plot
# boxplot(results[i], main = name, notch = FALSE)
# 3. Close the file
# dev.off()
# }
