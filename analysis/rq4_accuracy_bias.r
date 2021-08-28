# ***********************************************************
# RQ4: Is weekly self-reporting sufficient?
# ***********************************************************

# ***********************************************************
# RQ4.1: How accurate is self-reporting daily vs. weekly?
# ***********************************************************
# Calculates some statistics about the self-reporting accuracy
# and generates scatter plots inclusive LOESS and LM as well as spearman correlation coefficient
# to get a first idea about the daily vs weekly reporting correlation.
# ***********************************************************

# Spearman's correlation measures the strength and direction of monotonic association between two variables.
# R: 1.0 (a perfect positive correlation) and -1.0 (a perfect negative correlation).
# An Rs of 0 indicates no association between ranks.
# H0: No correlation, H1: Correlation
generate_daily_weekly_correlation <- function(aggregated_data) {
   column_names <- get_waste_data_column_names()

   plots <- list()
   for (column_name in column_names) {
      one_type_column <- c("weekly_" %&% column_name, "daily_" %&% column_name, "missing_data")
      one_type <- aggregated_data[, one_type_column]
      names(one_type)[1] <- "weekly"
      names(one_type)[2] <- "daily"
      names(one_type)[3] <- "missing_data"
      plot <- ggscatter(
         one_type,
         x = "weekly", y = "daily", conf.int = TRUE,
         color = "missing_data", palette = c("black", "red"),
         xlab = FALSE, ylab = FALSE, title = waste_title[column_name],
         size = 1
      ) +
      font("title", size = 10) +
      geom_smooth(formula = y ~ x, method = "loess", size = 0.5, colour = "red") +
      geom_smooth(formula = y ~ x, method = "lm", size = 0.5, colour = "blue") +
      stat_cor(method = "spearman", cor.coef.name = "rho") + # Add correlation coefficient w/o distingish between missing_data
      rremove("x.text") +
      rremove("y.text") +
      theme(legend.position='none')
      plots[[column_name]] <- plot
   }

   arranged_plot <- ggarrange(plotlist = plots)
   suppressMessages(ggsave(plot = arranged_plot, "results/rq4_1_daily_weekly_correlation.pdf", device = "pdf"))
}

generate_daily_weekly_difference_boxplots <- function(aggregated_data) {
   column_names <- get_data_column_names()
   results <- calculate_weekly_daily_difference(aggregated_data, column_names)
   print_weekly_daily_difference_overview(results, column_names)
}

calculate_weekly_daily_difference <- function(aggregated_data, column_names) {
   results <- list()

   file_name <- "rq4_1_weekly_daily_diff.txt"
   full_name <- recreate_results_file(file_name)

   for (name in get_waste_data_column_names()) {
      columns <- c("daily_" %&% name, "weekly_" %&% name)
      one_type <- aggregated_data[, columns]
      names(one_type) <- c("daily", "weekly")
      one_type$mean <- (one_type$daily + one_type$weekly) / 2
      one_type$difference <- one_type$daily - one_type$weekly
      one_type$percentage_difference <- (one_type$difference / one_type$mean) * 100
      one_type$percentage_difference[ is.na(one_type$percentage_difference) ] <- 0

      writeLine("Weekly Daily Diff: " %&% name, full_name, emptyLine = TRUE)
      writeLine("Mean diff: " %&% mean(one_type$difference), full_name)
      writeLine("Mean %: " %&% mean(one_type$percentage_difference), full_name)
      writeLine("SD diff: " %&% sd(one_type$difference), full_name)
      writeLine("SD %: " %&% sd(one_type$percentage_difference), full_name)

      results[waste_title[name]] <- one_type["difference"]
   }

   return(results)
}

print_weekly_daily_difference_overview <- function(results, column_names) {
   plot_to_file_start("rq4_1_weekly_daily_diff_overview")
   par(mar=c(9,4,1,1))

   par(cex.lab=0.85) # is for y-axis
   par(cex.axis=0.85) # is for x-axis

   boxplot(results,
      main = "",
      ylab = "",
      las = 2
   )

   abline(h = 0, col = "black", lwd = 0.5, lty = 2)

   plot_to_file_end()
}

# ***********************************************************
# RQ4.2: Is there a systematic bias when self-reporting weekly?
# ***********************************************************
# Does a bland-altman analysis in combination with linear
# regression based on the bland altman data to check for a bias.
# ***********************************************************

generate_daily_weekly_bias <- function(aggregated_data) {
   file_name <- "rq4_2_bland_altman.txt"
   full_name <- recreate_results_file(file_name)

   plot_to_file_start("rq4_2_bias_bland_altman")
   op <- par(mfrow = c(3, 4))

   for (column_name in get_waste_data_column_names()) {
      one_type_column <- c("daily_" %&% column_name, "weekly_" %&% column_name)
      one_type <- aggregated_data[, one_type_column]
      names(one_type) <- c("daily", "weekly")
      title <- waste_title[column_name]
      bland_altman_analysis(one_type, title, full_name)
   }

   par(op)
   plot_to_file_end()
}

# https://www-users.york.ac.uk/~mb55/meas/ba.htm - original
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4470095/ - Understanding Bland Altman analysis
# https://ncss-wpengine.netdna-ssl.com/wp-content/themes/ncss/pdf/Procedures/NCSS/Bland-Altman_Plot_and_Analysis.pdf
# https://cran.r-project.org/web/packages/BlandAltmanLeh/vignettes/Intro.html
bland_altman_analysis <- function(measurements, title, file) {
   A <- measurements[,1] # daily (gold standard)
   B <- measurements[,2] # weekly (estimate)
   stats <- bland.altman.stats(A, B, conf.int = 0.95)

   writeLine(title, file, emptyLine = TRUE)
   writeLine("Mean diff: " %&% round(stats$mean.diffs, 1), file)
   writeLine("Lower limit: " %&% round(stats$lower.limit, 1), file)
   writeLine("Upper limit: " %&% round(stats$upper.limit, 1), file)

   # If missing data points should be drawn in a different color, create own graphic:
   # https://mran.microsoft.com/snapshot/2014-10-25/web/packages/BlandAltmanLeh/vignettes/Intro.html
   bland.altman.plot(A, B, main="", xlab="Means", ylab="Differences", pch=19)
   title(main=title, cex.main=1)

   # K. M. Ho, “Using linear regression to assess dose-dependent bias on a Bland-Altman plot,” J. Emerg. Crit. Care Med., vol. 2, pp. 68–68, 2018.
   df <- data.frame(stats$means, stats$diffs)
   names(df) <- c("A", "B")
   fit <- lm(B ~ A, data = df)
   fit_summary <- summary(fit)
   slope_p_value <- fit_summary$coef[,"Pr(>|t|)"][2]
   writeLine("Slope p-value: " %&% round(slope_p_value, 5), file)

   abline(coef(fit)[1], coef(fit)[2], col="red")
}