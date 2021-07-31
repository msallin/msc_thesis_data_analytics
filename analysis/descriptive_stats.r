# ***********************************************************
# Descriptive Statistics
# ***********************************************************
# This script generates a bunch of descriptive statistics based on the reported daily & weekly data.
# 1. Daily answers
# 2. Weekly answers
# 3. Which was the category with the most waste?
# 4. Which was the category with the most delay?
# 5. How stressful is the work environment?
# 6. What caused the most stress?
# 7. What caused the most waste?
# ***********************************************************

generate_descriptive_statistics <- function(daily, weekly) {
    file_name <- "rq_0_descriptive.txt"
    full_name <- recreate_results_file(file_name)

    writeLine("1. Number of daily reports: " %&% nrow(daily), full_name)
    writeLine("2. Number of weekly reports: " %&% nrow(weekly), full_name, emptyLine = TRUE)

    time_spent <- daily[, c(get_waste_time_spent_column_names())]
    sum_time_spent <- sort(colSums(time_spent[, 1:length(time_spent)]), decreasing = TRUE)
    writeLine("3. Which was the category with the most waste?", full_name, emptyLine = TRUE)
    for (i in seq(1, length(time_spent), +1)) {
        txt <- "  " %&% i %&% ". " %&% labels(sum_time_spent[i]) %&% "(" %&% sum_time_spent[i] %&% ")"
        writeLine(txt, full_name)
    }

    # par(mar=c(5,10,4,1)+1)
    # barplot(
    #     sum_time_spent,
    #     horiz=TRUE, las=1,
    #     beside=TRUE, 
    #     xlim=range(pretty(c(0, sum_time_spent))))

    delay <- daily[, c(get_waste_delay_data_column_names())]
    sum_time_spent <- sort(colSums(delay[, 1:length(delay)]), decreasing = TRUE)

    writeLine("4. Which was the category with the most delay?", full_name, emptyLine = TRUE)
    for (i in seq(1, length(delay), +1)) {
        txt <- "  " %&% i %&% ". " %&% labels(sum_time_spent[i]) %&% " (" %&% sum_time_spent[i] %&% ")"
        writeLine(txt, full_name)
    }

    writeLine("5. How stressful is the work environment?", full_name, emptyLine = TRUE)
    stress <- daily[, c("stress")]
    writeLine("  Mean: " %&% mean(stress), full_name)
    writeLine("  Median: " %&% median(stress), full_name)
    writeLine("  Sd: " %&% sd(stress), full_name)
    writeLine("  Min: " %&% min(stress), full_name)
    writeLine("  Max: " %&% max(stress), full_name)

    writeLine("6. What was the subjective productivitiy feeling?", full_name, emptyLine = TRUE)
    productivity <- daily[, c("productivity")]
    writeLine("  Mean: " %&% mean(productivity), full_name)
    writeLine("  Sd: " %&% sd(productivity), full_name)
    writeLine("  Min: " %&% min(productivity), full_name)
    writeLine("  Max: " %&% max(productivity), full_name)

    writeLine("7. What caused the most stress?", full_name, emptyLine = TRUE)
    stress_causes_subset <- subset(daily, (!is.na(daily$stress_cause) & daily$stress_cause != "<NA>") & daily$stress_cause != "-")
    stress_causes <- stress_causes_subset$stress_cause
    wf <- word_frequency(stress_causes)
    writeLine(head(wf, 10)$word, full_name)

    writeLine("8. What caused the most waste?", full_name, emptyLine = TRUE)
    main_waste_subset <- subset(daily, (!is.na(daily$main_waste) & daily$main_waste != "<NA>") & daily$main_waste != "-")
    main_waste <- main_waste_subset$main_waste
    wf <- word_frequency(main_waste)
    writeLine(head(wf, 10)$word, full_name)

    # What is the distribution of the differently reported waste types?
    # par(mfrow=c(2,4))
    # for (col in 1:ncol(time_spent)) {
    #     hist(time_spent[,col])
    # }
}

word_frequency <- function(input) {
    splitted_input <- unlist(strsplit(input, split = ","))
    splitted_input <- unlist(strsplit(splitted_input, split = ";"))

    docs <- VCorpus(VectorSource(splitted_input))
    docs <- tm_map(docs, content_transformer(tolower))
    docs <- tm_map(docs, removeNumbers)
    docs <- tm_map(docs, removeWords, stopwords("english"))
    docs <- tm_map(docs, removePunctuation)
    docs <- tm_map(docs, stripWhitespace)

    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    wordFreq <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(wordFreq), freq=wordFreq)
    return(d)

    # grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) ) # sets number of gray shades to use
    # wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=2, random.order=F, colors=grayLevels)
}
