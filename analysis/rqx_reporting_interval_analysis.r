interval_analysis_data_prep <- function(daily, weekly) {
   calendar_weeks_reported <- unique(weekly$calendarWeek)

   # TODO: Remove
   calendar_week <- 27
   participant_id <- "4e10a96a-9a1b-4c41-b410-f7e593932e1f"
   participant_id <- "2c8f98b5-dc6d-4082-b1a3-6dc70b0deda3"

   week_aggreations <- data.frame()
   for (calendar_week in calendar_weeks_reported) {
      weekly_for_calendar_week <- subset(weekly, calendarWeek == calendar_week)
      participants_for_this_week <- unique(weekly_for_calendar_week$id)

      for (participant_id in participants_for_this_week) {
         weekly_for_calendar_week_for_participant <- subset(weekly, calendarWeek == calendar_week & id == participant_id)

         # Aggregate daily values for one week
         daily_participant <- subset(daily, id == participant_id & calendarWeek == calendar_week)
         daily_stress <- mean(daily_participant$stress)
         daily_productivity <- mean(daily_participant$productivity)
         daily_rework <- sum(daily_participant$rework)
         daily_manual <- sum(daily_participant$manual)
         daily_communication <- sum(daily_participant$communication)
         daily_administrative_demands <- sum(daily_participant$administrative_demands)
         daily_administrative_demands_delay <- sum(daily_participant$administrative_demands_delay)
         daily_missing_automation_delay <- sum(daily_participant$missing_automation_delay)
         daily_other_duties <- sum(daily_participant$other_duties)
         daily_customer <- mean(daily_participant$customer)
         daily_cognitive_load <- sum(daily_participant$cognitive_load)
         daily_complex_solution <- sum(daily_participant$complex_solution)
         daily_knowledge <- sum(daily_participant$knowledge)
         daily_time_waste <- daily_rework + daily_manual + daily_communication + daily_administrative_demands + daily_other_duties + daily_cognitive_load + daily_knowledge + daily_complex_solution
         daily_time_delay <- daily_administrative_demands_delay + daily_missing_automation_delay

         weekly_stress <- weekly_for_calendar_week_for_participant$stress
         weekly_productivity <- weekly_for_calendar_week_for_participant$productivity
         weekly_rework <- weekly_for_calendar_week_for_participant$rework
         weekly_manual <- weekly_for_calendar_week_for_participant$manual
         weekly_communication <- weekly_for_calendar_week_for_participant$communication
         weekly_administrative_demands <- weekly_for_calendar_week_for_participant$administrative_demands
         weekly_administrative_demands_delay <- weekly_for_calendar_week_for_participant$administrative_demands_delay
         weekly_missing_automation_delay <- weekly_for_calendar_week_for_participant$missing_automation_delay
         weekly_other_duties <- weekly_for_calendar_week_for_participant$other_duties
         weekly_customer <- weekly_for_calendar_week_for_participant$customer
         weekly_cognitive_load <- weekly_for_calendar_week_for_participant$cognitive_load
         weekly_complex_solution <- weekly_for_calendar_week_for_participant$complex_solution
         weekly_knowledge <- weekly_for_calendar_week_for_participant$knowledge
         weekly_time_waste <- weekly_rework + weekly_manual + weekly_communication + weekly_administrative_demands + weekly_other_duties + weekly_cognitive_load + weekly_knowledge + weekly_complex_solution
         weekly_time_delay <- weekly_administrative_demands_delay + weekly_missing_automation_delay

         week_aggreations_row <- data.frame(
            participant_id,
            calendar_week,
            daily_stress,
            daily_productivity,
            daily_rework,
            daily_manual,
            daily_communication,
            daily_administrative_demands,
            daily_administrative_demands_delay,
            daily_missing_automation_delay,
            daily_other_duties,
            daily_customer,
            daily_cognitive_load,
            daily_complex_solution,
            daily_knowledge,
            daily_time_waste,
            daily_time_delay,
            weekly_stress,
            weekly_productivity,
            weekly_rework,
            weekly_manual,
            weekly_communication,
            weekly_administrative_demands,
            weekly_administrative_demands_delay,
            weekly_missing_automation_delay,
            weekly_other_duties,
            weekly_customer,
            weekly_cognitive_load,
            weekly_complex_solution,
            weekly_knowledge,
            weekly_time_waste,
            weekly_time_delay
         )
         week_aggreations <- rbind(week_aggreations, week_aggreations_row)
      }
   }

   return(week_aggreations)
}