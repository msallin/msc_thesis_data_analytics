load_data <- function() {
    result_file <- "C:\\Users\\marcs\\OneDrive\\Documents\\School\\6 - FHNW MSc Engineering\\Project 9 Thesis\\results.xlsx" # file.choose()
    daily_sheet <- read_excel(result_file, sheet = "Daily Survey")
    weekly_sheet <- read_excel(result_file, sheet = "Weekly Survey")
    final_sheet <- read_excel(result_file, sheet = "Final Survey")

    daily <<- get_daily(daily_sheet)
    weekly <<- get_weekly(weekly_sheet)

    fkm <<- get_fkm(final_sheet)
    final <<- get_final(final_sheet)
    tool_feedback <<- get_tool_feedback(final_sheet)
}
