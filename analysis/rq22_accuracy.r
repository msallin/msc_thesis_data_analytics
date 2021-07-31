# ***********************************************************
# RQ2.2: How accurate is self-reporting daily vs. weekly?
# ***********************************************************
# TODO: Describe file
# ***********************************************************

# Spearman's correlation measures the strength and direction of monotonic association between two variables.
# R: 1.0 (a perfect positive correlation) and -1.0 (a perfect negative correlation).
# An Rs of 0 indicates no association between ranks.
# H0: No correlation, H1: Correlation
generate_daily_weekly_correlation <- function(daily, weekly) {
   aggregated_data <- interval_analysis_data_prep(daily, weekly)
   column_names <- get_waste_data_column_names()

   plots <- list()
   for (column_name in column_names) {
      one_type_column <- c("weekly_" %&% column_name, "daily_" %&% column_name)
      one_type <- aggregated_data[, one_type_column]
      names(one_type)[1] <- "weekly"
      names(one_type)[2] <- "daily"
      # one_type$daily = log(one_type$daily)
      plot <- ggscatter(
         one_type,
         x = "weekly", y = "daily", conf.int = TRUE,
         cor.coef = TRUE, cor.method = "spearman",
         xlab = FALSE, ylab = FALSE, title = column_name,
         size = 1
      ) +
      geom_smooth(formula = y ~ x, method = "loess", size = 0.5, colour = "red") +
      geom_smooth(formula = y ~ x, method = "lm", size = 0.5, colour = "blue")
      plots[[column_name]] <- plot + rremove("x.text") + rremove("y.text")
   }

   arranged_plot <- ggarrange(plotlist = plots)
   suppressMessages(ggsave(plot = arranged_plot, "results/rq_2_2_daily_weekly_correlation.pdf", device = "pdf"))
}

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
      one_type <- aggregated_data[, columns]
      names(one_type) <- c("daily", "weekly")
      one_type$mean <- (one_type$daily + one_type$weekly) / 2
      one_type$difference <- one_type$daily - one_type$weekly
      one_type$percentage_difference <- (one_type$difference / one_type$mean) * 100

      # TODO: Write to file
      mean(one_type$difference)
      mean(one_type$percentage_difference)
      sd(one_type$difference)
      sd(one_type$percentage_difference)

      results[name] <- one_type["difference"]
   }

   return(results)
}

print_weekly_daily_difference_overview <- function(results, column_names) {
   plot_to_file_start("rq_2_2_weekly_daily_diff_overview")

   # Outcomment to generate a a stripchart above the boxplot
   # par(mfrow = c(2, 1))
   # stripchart(results, group.names = column_names, vertical = TRUE)

   boxplot(results,
      main = "Weekly vs Daily Aggregate",
      ylab = "Difference from daily aggregate (h)",
      las = 2
   )

   abline(h = 0, col = "red", lwd = 0.5, lty = 2)

   plot_to_file_end()
}
