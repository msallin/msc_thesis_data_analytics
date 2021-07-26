# ***********************************************************
# Descriptive Statistics
# ***********************************************************
# This script generates a bunch of descriptive statistics based on the reported daily & weekly data.
# 1. Daily answers
# 2. Weekly answers
# 3. How much waste was reported (total and daily mean)?
# 4. Which was the category with the most waste?
# 5. How much delay was reported?
# 6. Which was the category with the most delay  (total and daily mean)?
# 7. What is the distribution of the differently reported waste types?
# 8. What caused the most stress (text cloud)?
# 9. What caused the most waste (text cloud)?
# ***********************************************************

generate_descriptive_statistics <- function(daily, weekly) {
    file_name <- "descriptive.txt"
    full_name <- recreate_results_file(file_name)

    # 1. Daily answers
    # 2. Weekly answers
    write("Daily reportings: " %&% nrow(daily), full_name, append = TRUE)
    write("Weekly reportings: " %&% nrow(weekly), full_name, append = TRUE)

    # 3. How much waste was reported (total and daily mean)?
    time_spent <- daily[, c(get_waste_time_spent_column_names())]

    # 4. Which was the category with the most waste?
    mean_of_time_spent <- sort(colMeans(time_spent[, 1:length(time_spent)]), decreasing = TRUE)
    for (i in seq(1, length(time_spent), +1)) {
        txt <- i %&% ". " %&% labels(mean_of_time_spent[i]) %&% "(" %&% mean_of_time_spent[i] %&% ")"
        write(txt, full_name, append = TRUE)
    }

    # 5. How much delay was reported?
    # TODO

    # 6. Which was the category with the most delay (total and daily mean)?
    time_spent <- daily[, c(get_was())]
    mean_of_time_spent <- sort(colMeans(time_spent[, 1:length(time_spent)]), decreasing = TRUE)
    for (i in seq(1, length(time_spent), +1)) {
        txt <- i %&% ". " %&% labels(mean_of_time_spent[i]) %&% "(" %&% mean_of_time_spent[i] %&% ")"
        write(txt, full_name, append = TRUE)
    }


    # 7. What is the distribution of the differently reported waste types?
    # TODO

    # 8. What caused the most stress (text cloud)?
    # TODO

    # 9. What caused the most waste (text cloud)?
    # TODO
}