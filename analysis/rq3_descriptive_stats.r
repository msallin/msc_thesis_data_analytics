# ***********************************************************
# RQ3: What are the insights for Swiss Post?
# ***********************************************************
# This script generates a bunch of descriptive statistics based on the reported daily & weekly data.
# 1. Survey stats
# 2. Which was the category with the most waste?
# 3. Which was the category with the most delay?
# 4. How stressful is the work environment?
# 5. What was the subjective productivitiy feeling?
# 6. What caused the most stress?
# 7. What caused the most waste?
# 8. How many week data points have missing daily surveys?
# 9. How confidente were the participants to work on the right things?
# ***********************************************************

generate_descriptive_statistics <- function(daily, weekly) {
    file_name <- "rq3_descriptive.txt"
    full_name <- recreate_results_file(file_name)

    writeLine("1. Survey stats", full_name)
    with_missing <- unique(weekly[weekly$missing_data == TRUE, ]$id)
    without_missing <- unique(weekly[weekly$missing_data == FALSE, ]$id)
    nevery_missed_weekly <- setdiff(without_missing, with_missing)
    writeLine("  Candidates (with missing): " %&% length(with_missing), full_name)
    writeLine("  Candidates (without missing): " %&% length(nevery_missed_weekly), full_name)
    writeLine("  Number of daily reports: " %&% nrow(daily), full_name)
    writeLine("  Number of weekly reports: " %&% nrow(weekly), full_name)

    writeLine("2. Which was the category with the most waste?", full_name, emptyLine = TRUE)
    time_spent <- daily[, c(get_waste_time_spent_column_names())]
    # Recode to hours
    for (name in get_waste_time_spent_column_names()) {
        time_spent[,name] <- recode_daily_factor_to_mean(time_spent[,name])
    }
    sum_time_spent <- sort(colSums(time_spent[, 1:length(time_spent)]), decreasing = TRUE)
    total_sum_time_spent <- sum(time_spent)
    for (i in seq(1, length(time_spent), +1)) {
        daily_avg_min <- round(sum_time_spent[i] / nrow(time_spent) * 60, 0)
        fraction <- round((sum_time_spent[i] / total_sum_time_spent) * 100, 2)
        txt <- "  " %&% i %&% ". " %&% waste_title[labels(sum_time_spent[i])] %&% " " %&% fraction %&% "% (" %&% format_time_span(sum_time_spent[i] * 60) %&% ", "  %&% format_time_span(daily_avg_min) %&% "/day" %&% ")"
        writeLine(txt, full_name)
    }
    writeLine("  Total: " %&% total_sum_time_spent %&% "h", full_name, emptyLine = TRUE)
    writeLine("  Statistical significant group differences", full_name, emptyLine = TRUE)
    check_group_difference(time_spent, full_name)

    writeLine("3. Which was the category with the most delay?", full_name, emptyLine = TRUE)
    delay <- daily[, c(get_waste_delay_data_column_names())]
    for (name in get_waste_delay_data_column_names()) {
        delay[,name] <- recode_daily_delay_factor_to_mean(delay[,name])
    }
    sum_delay <- sort(colSums(delay[, 1:length(delay)]), decreasing = TRUE)
    total_sum_delay <- sum(sum_delay)
    for (i in seq(1, length(delay), + 1)) {
        daily_avg_min <- round(sum_delay[i] / nrow(delay) * 60, 0)
        fraction <- round((sum_delay[i] / total_sum_delay) * 100, 2)
        txt <- "  " %&% i %&% ". " %&%  waste_title[labels(sum_delay[i])] %&% " " %&% fraction %&% "% (" %&% format_time_span(sum_delay[i] * 60) %&% ", "  %&% format_time_span(daily_avg_min) %&% "/day" %&% ")"
        writeLine(txt, full_name)
    }
    writeLine("  Total: " %&% total_sum_delay %&% "h", full_name, emptyLine = TRUE)
    writeLine("  Statistical significant group differences", full_name, emptyLine = TRUE)
    check_group_difference(delay, full_name)

    writeLine("4. How stressful is the work environment?", full_name, emptyLine = TRUE)
    stress <- daily[, c("stress")]
    writeLine("  Mean: " %&% round(mean(stress), 1), full_name)
    writeLine("  Median: " %&% round(median(stress), 1), full_name)
    writeLine("  Sd: " %&% round(sd(stress), 1), full_name)
    writeLine("  Min: " %&% round(min(stress), 1), full_name)
    writeLine("  Max: " %&% round(max(stress), 1), full_name)

    writeLine("5. What was the subjective productivitiy feeling?", full_name, emptyLine = TRUE)
    productivity <- daily[, c("productivity")]
    writeLine("  Mean: " %&% round(mean(productivity), 1), full_name)
    writeLine("  Sd: " %&% round(sd(productivity), 1), full_name)
    writeLine("  Min: " %&% round(min(productivity), 1), full_name)
    writeLine("  Max: " %&% round(max(productivity), 1), full_name)

    writeLine("6. What caused the most stress?", full_name, emptyLine = TRUE)
    stress_causes_subset <- subset(daily, (!is.na(daily$stress_cause) & daily$stress_cause != "<NA>") & daily$stress_cause != "-")
    stress_causes <- stress_causes_subset$stress_cause
    wf <- word_frequency(stress_causes)
    stress_top <- head(wf, 15)
    for (i in 1:nrow(stress_top)) {
       to_print <- "  " %&% toString(stress_top$word[i]) %&% " " %&% toString(stress_top$freq[i])
       writeLine(to_print, full_name)
    }

    writeLine("7. What caused the most waste?", full_name, emptyLine = TRUE)
    writeLine("7.1 Daily?", full_name, emptyLine = TRUE)
    main_waste_subset <- subset(daily, (!is.na(daily$main_waste) & daily$main_waste != "<NA>") & daily$main_waste != "-")
    main_waste <- main_waste_subset$main_waste
    wf <- word_frequency(main_waste)
    waste_daily_top <- head(wf, 15)
    for (i in 1:nrow(waste_daily_top)) {
       to_print <- "  " %&% toString(waste_daily_top$word[i]) %&% " " %&% toString(waste_daily_top$freq[i])
       writeLine(to_print, full_name)
    }

    writeLine("7.2 Weekly?", full_name, emptyLine = TRUE)
    main_waste_subset <- subset(weekly, (!is.na(weekly$main_waste) & weekly$main_waste != "<NA>") & weekly$main_waste != "-")
    main_waste <- main_waste_subset$main_waste
    wf <- word_frequency(main_waste)
    waste_weekly_top <- head(wf, 15)
    for (i in 1:nrow(waste_weekly_top)) {
       to_print <- "  " %&% toString(waste_weekly_top$word[i]) %&% " " %&% toString(waste_weekly_top$freq[i])
       writeLine(to_print, full_name)
    }

    writeLine("8. How many weekly data points have missing daily survey?", full_name, emptyLine = TRUE)
    writeLine("  Missing data (#weeks): " %&% nrow(weekly[weekly$missing_data == TRUE, ]), full_name)
    writeLine("  Total (#weeks): " %&% nrow(weekly), full_name)

    writeLine("9. Customer", full_name, emptyLine=TRUE)
    answers_only <- table(daily[daily[,"customer"]>0,]$customer)
    all <- sum(answers_only)
    for (i in 1:length(answers_only)) {
        percentage <- (answers_only[i]/all) * 100
        writeLine("  " %&% i %&% ": " %&% round(percentage, 1), full_name)
    }
    customer_records <- daily[daily[,"customer"]>0,]$customer
    writeLine("  Mean: " %&% round(mean(customer_records), 1) %&% "; Median: " %&% round(median(customer_records), 1) %&% "; Standard deviation: " %&% round(sd(customer_records), 1), full_name)

    names = c("Completely insecure", "Somewhat insecure", "Neutral", "Somewhat confident", "Completely confident")
    plot_to_file_start("rq3_customer")
    par(mar=c(4,10,20,5)) # bottom, left, top, right
    barplot(answers_only, names.arg=names, horiz=T, las=1, xlab="Frequency", xlim=range(pretty(c(0,answers_only))))
    plot_to_file_end()
}

check_group_difference <- function(groups, file) {
    combinations <- list()
    if(length(names(groups)) > 2) {
        combinations <- combn(names(groups), 2, simplify=FALSE)
    } else {
        combinations <- list(names(groups))
    }
    
    for (i in 1:length(combinations)) {
       name1 <- combinations[[i]][1]
       name2 <- combinations[[i]][2]
       result <- wilcox.test(groups[,name1], groups[,name2])
       if(result$p.value < 0.05) {
           writeLine("   " %&% name1 %&% " & " %&% name2 %&% " (p=" %&% round(result$p.value, 5) %&% ")", file)
       }
    }
}

word_frequency <- function(input) {
    splitted_input <- unlist(strsplit(input, split = ","))
    splitted_input <- unlist(strsplit(splitted_input, split = ";"))
    splitted_input <- removeWords(splitted_input, stopwords("english"))
    splitted_input <- removeNumbers(splitted_input)
    splitted_input <- removePunctuation(splitted_input)
    splitted_input <- gsub("    ", " ", splitted_input)
    splitted_input <- gsub("   ", " ", splitted_input)
    splitted_input <- gsub("  ", " ", splitted_input)
    splitted_input <- tolower(splitted_input)
    splitted_input <- trimws(splitted_input)
    splitted_input <- gsub(" ", "_", splitted_input)
    docs <- VCorpus(VectorSource(splitted_input))

    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    wordFreq <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(wordFreq), freq=wordFreq)
    return(d)

    # Tried but doesn't make up a good picture.
    # Maybe try it again after all data is collected.
    # grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) ) # sets number of gray shades to use
    # wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=2, random.order=F, colors=grayLevels)
}
