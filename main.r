# update.packages(ask = FALSE)
# install.packages("readxl")
# install.packages("dplyr")

library("dplyr")
library("readxl")
source("final_survey.r")
source("daily_survey.r")
source("weekly_survey.r")
source("reporting_interval_analysis.r")

analysis_something <- function() {
    hist(daily$stress)
    hist(daily$productivity)
    pairs(~ totalWaste + stress + productivity, data = daily, main = "Simple Scatterplot Matrix")

    fit <- lm(stress ~ log(totalWaste), data = daily)
    summary(fit)
    # layout(matrix(c(1, 2, 3, 4), 2, 2)) # optional 4 graphs/page

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

load_data <- function() {
    get_waste <- function(final_sheet) {
        id <- c(55, 255)
        stress <- c(2, 3)
        delay <- c(5, 7)
        time_lost <- c(2, 5)
        customer_focus <- c(1, 4)
        return(data.frame(id, stress, delay, time_lost, customer_focus))
    }

    result_file <- "C:\\Users\\marcs\\OneDrive\\Documents\\School\\6 - FHNW MSc Engineering\\Project 9 Thesis\\results.xlsx" # file.choose()
    daily_sheet <- read_excel(result_file, sheet = "Daily Survey")
    weekly_sheet <- read_excel(result_file, sheet = "Weekly Survey")
    final_sheet <- read_excel(result_file, sheet = "Final Survey")

    daily <<- get_daily(daily_sheet)
    weekly <<- get_weekly(weekly_sheet)

    fkm <<- get_fkm(final_sheet)
    waste <<- get_waste(final_sheet)
}

load_data()
interval_analysis(daily, weekly)