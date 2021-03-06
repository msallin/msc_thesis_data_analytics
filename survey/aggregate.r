# ***********************************************************
# Aggregate
# ***********************************************************
# This script aggregates the daily and weekly data into one data set.
# ***********************************************************

aggregated_daily_and_weekly_data <- function(daily, weekly) {
   calendar_weeks_reported <- unique(weekly$calendarWeek)
   week_aggreations <- data.frame()

   for (calendar_week in calendar_weeks_reported) {
      weekly_for_calendar_week <- subset(weekly, calendarWeek == calendar_week)
      participants_for_this_week <- unique(weekly_for_calendar_week$id)

      for (participant_id in participants_for_this_week) {
         weekly_for_calendar_week_for_participant <- subset(weekly, calendarWeek == calendar_week & id == participant_id)

         # Aggregate daily values for one week
         daily_participant <- subset(daily, id == participant_id & calendarWeek == calendar_week)
         number_of_days_reported <- nrow(daily_participant)
         
         daily_stress <- mean(daily_participant$stress)
         daily_productivity <- mean(daily_participant$productivity)

         # The factors used represent timespans and hence calculating the mean is possible.
         # It is the same as taking the mean amount of hours represented in a span, sum and average this.
         daily_rework <- sum(recode_daily_factor_to_mean(daily_participant$rework))
         daily_rework <- sum(recode_daily_factor_to_mean(daily_participant$rework))
         daily_manual <- sum(recode_daily_factor_to_mean(daily_participant$manual))
         daily_communication <- sum(recode_daily_factor_to_mean(daily_participant$communication))
         daily_administrative_demands <- sum(recode_daily_factor_to_mean(daily_participant$administrative_demands))
         daily_other_duties <- sum(recode_daily_factor_to_mean(daily_participant$other_duties))
         daily_knowledge <- sum(recode_daily_factor_to_mean(daily_participant$knowledge))
         daily_cognitive_load <- sum(recode_daily_factor_to_mean(daily_participant$cognitive_load))
         daily_complex_solution <- sum(recode_daily_factor_to_mean(daily_participant$complex_solution))
         
         daily_customer <- round(mean(daily_participant$customer), digits=0)

         daily_administrative_demands_delay <- sum(recode_daily_delay_factor_to_mean(daily_participant$administrative_demands_delay))
         daily_missing_automation_delay <- sum(recode_daily_delay_factor_to_mean(daily_participant$missing_automation_delay))

         daily_time_waste <- daily_rework + daily_manual + daily_communication + daily_administrative_demands + daily_other_duties + daily_cognitive_load + daily_knowledge + daily_complex_solution
         daily_time_delay <- daily_administrative_demands_delay + daily_missing_automation_delay

         # Metadata
         missing_data <- weekly_for_calendar_week_for_participant$missing_data

         # Scale
         weekly_stress <- weekly_for_calendar_week_for_participant$stress # Scale from 1-10, its expected to be the mean.
         weekly_productivity <- weekly_for_calendar_week_for_participant$productivity # Scale from 1-10, its expected to be the mean.

         # Factor
         weekly_customer <- weekly_for_calendar_week_for_participant$customer # Its a factor expressing how sure one is to do the right thins, its expected to be the mean.

         # Durations
         weekly_rework <- recode_weekly_duration(weekly_for_calendar_week_for_participant$rework, number_of_days_reported)
         weekly_manual <- recode_weekly_duration(weekly_for_calendar_week_for_participant$manual, number_of_days_reported)
         weekly_communication <- recode_weekly_duration(weekly_for_calendar_week_for_participant$communication, number_of_days_reported)
         weekly_administrative_demands <- recode_weekly_duration(weekly_for_calendar_week_for_participant$administrative_demands, number_of_days_reported)
         weekly_other_duties <- recode_weekly_duration(weekly_for_calendar_week_for_participant$other_duties, number_of_days_reported)
         weekly_cognitive_load <- recode_weekly_duration(weekly_for_calendar_week_for_participant$cognitive_load, number_of_days_reported)
         weekly_complex_solution <- recode_weekly_duration(weekly_for_calendar_week_for_participant$complex_solution, number_of_days_reported)
         weekly_knowledge <- recode_weekly_duration(weekly_for_calendar_week_for_participant$knowledge, number_of_days_reported)

         # Delays
         weekly_administrative_demands_delay <- recode_weekly_duration(weekly_for_calendar_week_for_participant$administrative_demands_delay, number_of_days_reported)
         weekly_missing_automation_delay <- recode_weekly_duration(weekly_for_calendar_week_for_participant$missing_automation_delay, number_of_days_reported)

         # Aggregations
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
            weekly_time_delay,
            missing_data,
            number_of_days_reported
         )
         week_aggreations <- rbind(week_aggreations, week_aggreations_row)
      }
   }

   return(week_aggreations)
}

recode_daily_delay_factor_to_mean <- function(x) {
   unlist(lapply(x, function(x) {
      if(x == "0") return(0) #0h
      if(x == "1") return(1) #Up to 2h (0+2 /2)
      if(x == "2") return(3) # ...4h (2+4 /2)
      if(x == "3") return(6) # ...8h (4+8 / 2)
      if(x == "4") return(16.6) # ...few days (8+25.2 / 2)
      if(x == "5") return(29.3) # ...week (16.6+32 / 2)
      if(x == "6") return(98.65) # ...month (29.3+168 / 2)
      if(x == "7") return(295.95) # ...multiple months (98.65*3)
   }))
}

recode_daily_factor_to_mean <- function(x) {
   unlist(lapply(x, function(x) {
      if(x == "0") return(0)
      if(x == "1") return(0.5)
      if(x == "2") return(1.5)
      if(x == "3") return(3)
      if(x == "4") return(5)
      if(x == "5") return(7)
      if(x == "6") return(8)
   }))
}

recode_weekly_duration <- function(weekly_reporting, number_of_days_reported) {
   return(weekly_reporting)
   # Use this for normalizing on the day for week/day comparison
   # x <- weekly_reporting/number_of_days_reported
   # if(x == 0) return(0)
   # if(x < 1) return(1)
   # if(x <= 2) return(2)
   # if(x <= 4) return(3)
   # if(x <= 6) return(4)
   # if(x <= 8) return(5)
   # if(x > 8) return(6)
}