interval_analysis <- function(daily, weekly) {
   calendarWeeksReported <- unique(weekly$calendarWeek)
   participantsReported <- unique(weekly$id)

   # TODO: Remove
   calendar_week <- 27
   participant_id <- "4e10a96a-9a1b-4c41-b410-f7e593932e1f"

   aggregated_participant_daily <- data.frame()
   for (calendar_week in calendarWeeksReported) {
      print(calendar_week)
      participants_for_week <- unique(weekly[which(weekly$calendarWeek == calendar_week)]$id)
      weekly_for_calendar_week <- subset(weekly, calendarWeek == calendar_week)
      participants_for_this_week <- unique(weekly_for_calendar_week$id)
      for (participant_id in participants_for_this_week) {
         daily_participant <- subset(daily, id == participant_id & calendarWeek == calendar_week)
         avg_stress <- mean(daily_participant$stress)
         avg_productivity <- mean(daily_participant$productivity)
         sum_rework <- sum(daily_participant$rework)
         sum_manual <- sum(daily_participant$manual)
         sum_communication <- sum(daily_participant$communication)
         sum_administrative_demands <- sum(daily_participant$administrative_demands)
         sum_administrative_demands_delay <- sum(daily_participant$administrative_demands_delay)
         sum_other_duties <- sum(daily_participant$other_duties)
         mean_customer <- mean(daily_participant$customer)
         sum_cognitive_load <- sum(daily_participant$cognitive_load)
         sum_complex_solution <- sum(daily_participant$complex_solution)
         sum_knowledge <- sum(daily_participant$knowledge)
         aggregated_participant_daily_row <- data.frame(
            participant_id,
            calendar_week,
            avg_stress,
            avg_productivity,
            sum_rework,
            sum_manual,
            sum_communication,
            sum_administrative_demands,
            sum_administrative_demands_delay,
            sum_other_duties,
            mean_customer,
            sum_cognitive_load,
            sum_complex_solution,
            sum_knowledge
         )
         aggregated_participant_daily <- rbind(aggregated_participant_daily, aggregated_participant_daily_row)
      }
   }
}