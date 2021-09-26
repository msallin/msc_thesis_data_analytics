# ***********************************************************
# Pre-Processing
# ***********************************************************
# Provide shared utilities used all over the place.
# ***********************************************************

plot_to_file_start <- function(file_name) {
    file_name <- "results/" %&% file_name %&% ".pdf"
    pdf(file_name, onefile = FALSE)
    return(file_name)
}

writeLine <- function(text, file_path, emptyLine = FALSE) {
    if(emptyLine) {
        write("", file_path, append = TRUE)
    }
    write(text, file_path, append = TRUE)
}

plot_to_file_end <- function() {
    garbage <- dev.off()
}

recreate_results_file <- function(file_name) {
    file_name <- "results/" %&% file_name
    if (file.exists(file_name)) file.remove(file_name)
    return(file_name)
}

format_time_span <- function(time_in_min) {
    if(is.na(time_in_min)) {
        return("-")
    }
    if(time_in_min > 59) {
        result <- round(time_in_min/60, 1) %&% "h"
        return(result)
    } else {
        result <- time_in_min %&% "min"
        return(result)
    }
}

"%&%" <- function(x, y) paste0(x, y)