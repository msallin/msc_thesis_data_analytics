# ***********************************************************
# RQ2.1: How can a self-reporting tool help to identify waste?
# ***********************************************************
# TODO: Describe file
# ***********************************************************

generate_feedback_likert_plots <- function(tool_feedback) {
    likert_one_columns <- c(
        "helped_to_recall",
        "helped_to_identify_waste",
        "representative"
    )

    likert_two_columns <- c(
        "day_was_more_accurate",
        "daily_had_influence_on_weekly",
        "fill_out_daily_okay",
        "fill_out_weekly_okay"
    )

    likert_one_full_text <- tool_feedback[, likert_one_columns]
    names(likert_one_full_text) <- c(
        "helped_to_recall" = "The survey helped me to recall/remember where I encountered waste",
        "helped_to_identify_waste" = "The surveys helped me to identify waste",
        "representative" = "The waste reported in the reporting period was representative for the waste I encounter during my usual workdays"
    )

    likert_two_full_text <- tool_feedback[, likert_two_columns]
    names(likert_two_full_text) <- c(
        "day_was_more_accurate" = "My daily reporting was more accurate than my weekly reporting",
        "daily_had_influence_on_weekly" = "What I reported in my daily surveys influenced what I reported in my weekly surveys",
        "fill_out_daily_okay" = "If I'm asked to, I would fill out a daily survey for waste identification for a few weeks every year",
        "fill_out_weekly_okay" = "If I'm asked to, I would fill out a weekly survey for waste identification for a few weeks every year"
    )

    p <- plot(likert(likert_one_full_text), wrap=40, text.size=2.5)
    p$layers[[2]]$geom_params$width = 0.8
    p$layers[[3]]$geom_params$width = 0.8
    p <- p + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), aspect.ratio=0.2, legend.text = element_text(color="black",size=8), axis.text=element_text(color="black",size=8))
    suppressMessages(ggsave(plot = p, "results/rq_2_1_likert_one.pdf", device = "pdf", height = 2.3, width = 11))

    p <- plot(likert(likert_two_full_text), wrap=40, text.size=2.5)
    p$layers[[2]]$geom_params$width = 0.8
    p$layers[[3]]$geom_params$width = 0.8
    p <- p + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), aspect.ratio=0.2, legend.text = element_text(color="black",size=8), axis.text=element_text(color="black",size=8))
    suppressMessages(ggsave(plot = p, "results/rq_2_1_likert_two.pdf", device = "pdf", height = 4, width = 11))
}

generate_not_reported_waste_summary <- function(daily) {
    reports_with_content <- subset(daily, 
        !is.na(daily$not_reported) &
        daily$not_reported != "<NA>" &
        daily$not_reported != "-" &
        daily$not_reported != "No" &
        daily$not_reported != "no")
    reports_with_content <- reports_with_content$not_reported

    file_name <- "rq_2_1_not_able_to_report.txt"
    full_name <- recreate_results_file(file_name)
    writeLine("Participants not able to report the following waste:", full_name)
    writeLine(reports_with_content, full_name)
}