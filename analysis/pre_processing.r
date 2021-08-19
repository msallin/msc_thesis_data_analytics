# ***********************************************************
# Pre-Processing
# ***********************************************************
# This script does some pre-processing on the data (e.g. detect missing reportings).
# Some methods are only used one time and corrected in the results and some enricht/transform the data.
# ***********************************************************

find_participants_without_final <- function(daily, weekly, final) {
    d <- unique(daily$id)
    w <- unique(weekly$id)
    f <- unique(final$id)

    print("Diff Daily & Weekly:")
    print(setdiff(d, w)) # reported daily but not weekly
    print("Diff Weekly & Final:")
    print(setdiff(w, f)) # reported weekly but not final
    print("Diff Daily & Final:")
    print(setdiff(d, f)) # reported daily but not final
}

# This code was used one time to do manual data clean up because it was much easiert to do it manually than do it with code.
find_wrongly_reported_weeks <- function(final, daily) {
    participants <- unique(final$id)
    for (participant_id in participants) {
        participant_daily <- subset(daily, id == participant_id)
        cw <- unique(participant_daily$calendarWeek)
        if(length(cw) > 3) {
            print(length(cw))
            print(participant_id)
            print("Too much reports...")
        }
    }
}

find_missing_reportings <- function(daily, weekly, final) {
    weekly$missing_data <- FALSE
    file_name <- "pre_processing.txt"
    full_name <- recreate_results_file(file_name)

    participant_ids <- unique(final$id)
    for (participant_id in participant_ids) {
    
        participant_weekly <- subset(weekly, id == participant_id)
        weeks <- unique(participant_weekly$calendarWeek)

        for (week in weeks) {
            # All days for this week which were reported
            daily_results_for_cw <- subset(daily, calendarWeek == week & id == participant_id)
            days_reported <- format(daily_results_for_cw$date, "%d.%m")

            # The days the participant stated that he didn't work 
            # Always add the days not worked no matter to which CW they belong.
            # Works and is easier than check for every CW.
            final_survey_row <- subset(final, id == participant_id)
            days_not_worked <- list()
            if(!is.na(final_survey_row$not_working)) {
                days_not_worked <- unlist(strsplit(final_survey_row$not_working, ";"))
                normalized_days_not_worked <- as.Date(days_not_worked, "%d.%m")
                days_not_worked_string <- format(normalized_days_not_worked, "%d.%m")
                days_reported <- c(days_reported, days_not_worked_string)
            }

            # For every day in this week
            first_day_of_year <- as.Date("2021/01/01")
            first_day_of_reporting <- first_day_of_year + ((as.numeric(week) - 1) * 7) + 3
            for (i in 0:4) {
                day_to_check <- first_day_of_reporting + i
                day_to_check_string <- format(day_to_check, "%d.%m")
                if(!any(day_to_check_string == days_reported)) {
                    writeLine(participant_id %&% " missed: " %&% day_to_check %&% " cw: " %&% week, full_name)
                    writeLine(days_reported, full_name)
                    weekly <- transform(weekly, missing_data = ifelse(!missing_data, calendarWeek == week & id == participant_id, missing_data))
                }
            }
        }
    }
    return(weekly)
}
