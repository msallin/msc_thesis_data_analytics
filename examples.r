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