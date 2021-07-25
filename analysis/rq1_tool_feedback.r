# ***********************************************************
# RQ1: How can a self-reporting tool help to identify waste?
# ***********************************************************

generate_feedback_likert_plots <- function(tool_feedback) {
    likert_one <- c(
        "helped_to_recall",
        "helped_to_identify_waste",
        "representative"
    )

    likert_two <- c(
        "day_was_more_accurate",
        "daily_had_influence_on_weekly",
        "fill_out_daily_okay",
        "fill_out_weekly_okay"
    )

    feedback_likert_one <- likert(tool_feedback[, likert_one])
    feedback_likert_two <- likert(tool_feedback[, likert_two])

    plot_to_file_start("likert_one")
    plot(feedback_likert_one)
    plot_to_file_end()

    plot_to_file_start("likert_two")
    plot(feedback_likert_two)
    plot_to_file_end()
}
