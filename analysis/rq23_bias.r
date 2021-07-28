# ***********************************************************
# RQ2.3: Is there a systematic bias when self-reporting weekly?
# ***********************************************************
# TODO: Describe file
# ***********************************************************

generate_daily_weekly_bias <- function(daily, weekly) {
   aggregated_data <- interval_analysis_data_prep(daily, weekly)

   plot_to_file_start("rq_2_3_bias_bland_altman")
   op <- par(mfrow = c(3, 3))

   for (column_name in get_waste_time_spent_column_names()) {
      one_type_column <- c("daily_" %&% column_name, "weekly_" %&% column_name)
      one_type <- aggregated_data[, one_type_column]
      names(one_type) <- c("daily", "weekly")
      print_bland_altman_plots(one_type, column_name)
   }

   par(op)
   plot_to_file_end()
}

# https://www-users.york.ac.uk/~mb55/meas/ba.htm - original
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4470095/ - Understanding Bland Altman analysis
# https://ncss-wpengine.netdna-ssl.com/wp-content/themes/ncss/pdf/Procedures/NCSS/Bland-Altman_Plot_and_Analysis.pdf
# https://cran.r-project.org/web/packages/BlandAltmanLeh/vignettes/Intro.html
print_bland_altman_plots <- function(measurements, title) {
   A <- measurements[,1] # daily (gold standard)
   B <- measurements[,2] # weekly (estimate)
   stats <- bland.altman.stats(A, B)
   bland.altman.plot(A, B,
      main=title, xlab="Means", ylab="Differences")
      # Maybe add , conf.int=.95, pch=19
}