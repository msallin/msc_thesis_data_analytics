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

"%&%" <- function(x, y) paste0(x, y)