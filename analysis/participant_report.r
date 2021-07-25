
# ***********************************************************
# Participant Report:
# 1. How much waste did you experience?
# 2. How much delay did you experience?
# 3. What was your averge stress score?
# 4. What was your average productivity score?
# 5. Does your feeling of productivity correlate with waste experienced?
# 6. Does your feeling of stress correlate with waste experienced?
# - What caused you the most waste?
# ***********************************************************
bla <- function(daily) {
    participant <- subset(daily, id == "ce1ce7e7-07ef-413c-adec-64378049a128")

    # 1. How much waste did you experience?
    participant_time_spent <- participant[, c("date", get_waste_time_spent_column_names())]
    sum_of_time_spent <- sort(colSums(participant_time_spent[, 1:length(participant_time_spent)]), decreasing = TRUE)
    for (i in seq(1, length(participant_time_spent), +1)) {
        print(i %&% ". " %&% labels(sum_of_time_spent[i]) %&% "(" %&% sum_of_time_spent[i] %&% ")")
    }

    mean_of_time_spent <- sort(colMeans(participant_time_spent[, 1:length(participant_time_spent)]), decreasing = TRUE)
    for (i in seq(1, length(participant_time_spent), +1)) {
        print(i %&% ". " %&% labels(mean_of_time_spent[i]) %&% "(" %&% mean_of_time_spent[i] %&% ")")
    }

    print("Total: " %&% sum(sum_of_time_spent))
    pie(sum_of_time_spent)

    # barplot(t(as.matrix(participant_time_spent$rework, participant_time_spent$administrative_demands)))

    # 2. How much delay did you experience?
    # TODO

    # 3. What was your averge stress score?
}