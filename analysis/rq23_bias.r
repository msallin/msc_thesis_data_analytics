# ***********************************************************
# RQ2.3: Is there a systematic bias when self-reporting weekly?
# ***********************************************************
# This script does a bland-altman analysis in combination with linear
# regression based on the bland altman data to check for a bias.
# ***********************************************************

generate_daily_weekly_bias <- function(aggregated_data) {
   file_name <- "rq_2_3_bland_altman.txt"
   full_name <- recreate_results_file(file_name)

   plot_to_file_start("rq_2_3_bias_bland_altman")
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
   writeLine("Mean diff: " %&%stats$mean.diffs, file)
   writeLine("Lower limit: " %&% stats$lower.limit, file)
   writeLine("Upper limit: " %&%stats$upper.limit, file)

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
   writeLine("Slope p-value: " %&% slope_p_value, file)

   abline(coef(fit)[1], coef(fit)[2], col="red")
}