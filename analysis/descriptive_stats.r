# ***********************************************************
# Descriptive:
# - Daily answers
# - Weekly answers
# - How much waste was reported?
# - Which was the category with the most waste?
# ***********************************************************

generate_descriptive_statistics <- function(daily) {
    file_name <- "descriptive.txt"
    full_name <- recreate_results_file(file_name)

    write("Daily reportings: " %&% nrow(daily), full_name, append = TRUE)
    write("Weekly reportings: " %&% nrow(weekly), full_name, append = TRUE)

    time_spent <- daily[, c(get_waste_time_spent_column_names())]

    mean_of_time_spent <- sort(colMeans(time_spent[, 1:length(time_spent)]), decreasing = TRUE)
    for (i in seq(1, length(time_spent), +1)) {
        txt <- i %&% ". " %&% labels(mean_of_time_spent[i]) %&% "(" %&% mean_of_time_spent[i] %&% ")"
        write(txt, full_name, append = TRUE)
    }
}