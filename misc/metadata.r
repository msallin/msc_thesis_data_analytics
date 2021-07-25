get_data_column_names <- function() {
    n <- c(get_productivity_data_column_name(), get_waste_data_column_names())
    return(n)
}

get_productivity_data_column_name <- function() {
    n <- c("productivity")
    return(n)
}

get_waste_data_column_names <- function() {
    n <- c(get_waste_scale_data_column_names(), get_waste_delay_data_column_names(), get_waste_time_spent_column_names())
    return(n)
}

get_waste_scale_data_column_names <- function() {
    n <- c("stress", "customer")
    return(n)
}

get_waste_delay_data_column_names <- function() {
    n <- c("missing_automation_delay", "administrative_demands_delay")
    return(n)
}

get_waste_time_spent_column_names <- function() {
    n <- c("rework", "manual", "communication", "administrative_demands", "other_duties", "cognitive_load", "complex_solution", "knowledge")
    return(n)
}
