#######################################################################
###################                                 ###################
###################                                 ###################
###################              SETUP              ###################
###################                                 ###################
###################                                 ###################
#######################################################################

# Set working directory to the directory which holds this script.
# setwd('your_working_directory_here')


####################
####################
##                ##
##    LIBRARIES   ##
##                ##
####################
####################


# libraries
library(tidyverse)
library(lubridate)
library(tm)
library(pdftools)
library(tidytext)
library(textdata)
library(data.table)
library(scales)
library(ggiraph)
library(ggpubr)


####################
####################
##      SET       ##
##      FILE      ##
##      PATHS     ##
####################
####################


# file path for voting records
vote_records_csv_path <- 'data/raw/2019_vote_records/SCDB_2020_01_justiceCentered_Docket.csv'

# get file paths and names of all oral argument files in the data folder
pdf_filenames <- list.files(path = 'data/raw/2019_cases/', full.names = TRUE)



#######################################################################
###################                                 ###################
###################         READ AND SUMMARIZE      ###################
###################           ORAL ARGUMENT         ###################
###################            TRANSCRIPTS          ###################
###################                                 ###################
#######################################################################



####################
####################
##    FUNCTION    ##
##   TO PROCESS   ##
##  A SINGLE CASE ##
####################
####################



## -----> define constants outside the function


# all justices on the bench in the 2019 term
justices <- c("CHIEF JUSTICE ROBERTS",
              "JUSTICE THOMAS",
              "JUSTICE BREYER",
              "JUSTICE ALITO",
              "JUSTICE SOTOMAYOR",
              "JUSTICE KAGAN",
              "JUSTICE GORSUCH",
              "JUSTICE KAVANAUGH",
              "JUSTICE GINSBURG")

# custom stop words to withhold from some parts of the analysis
custom_stop_words <- data.frame(word = c('chief', 
                                         'justice',
                                         'roberts',
                                         'thomas',
                                         'breyer',
                                         'alito',
                                         'sotomayor',
                                         'kagan',
                                         'gorsuch',
                                         'kavanaugh',
                                         'ginsburg',
                                         'counsel',
                                         'argument',
                                         'ms'))

# expressions to filter out when counting questions - justices politely referring to each other ("Justice Kagan?" = it's your turn to speak, Justice Kagan)
justices_referring_to_each_other <- str_to_title(paste(justices, '\\?', sep = '', collapse = "|"))


## -----> define function for processing each PDF, and each justice in the text


# function takes one PDF file path and returns a dataframe with stats for the justices in that case
process_single_case <- function(filename){
  
  
  ## -----> load and parse document
  
  
  # extract the docket number from the filename
  docket <- str_extract(filename, "\\d+-\\d+")
  
  # reads each page into an element of the vector
  pdf_all_text <- pdf_text(filename)
  
  # grab date from text before cleaning
  date <- str_extract(pdf_all_text, 'Date:\\s+\\w+\\s\\d+,\\s\\d\\d\\d\\d')
  date <- date[!is.na(date)]
  date <- lubridate::parse_date_time(date, "b d Y")
  
  # steps to remove unwanted artifacts (line breaks and numbers) + boilerplate content on each page
  pdf_all_text <-
    pdf_all_text %>% 
    str_replace_all("Official\n\n\n", "") %>%  # cut boilerplate header/footer
    str_replace_all("\\n\\d+", '') %>% # cut line numbers (linebreaks before them)
    str_replace_all("\\d+\\n", "") %>%  # cut line numbers (linebreaks after them)
    str_replace_all("\\n", "") %>% # cut any linebreaks remaining
    str_replace_all("Official - Subject to Final Review", "") %>% # cut boilerplate header/footer
    str_replace_all("Heritage Reporting Corporation", "") %>% #cut boilerplate header/footer
    str_flatten(collapse = "") %>% # combine vector elements into one big text
    str_squish() # remove excess whitespace in the text
  
  # extract the part of the transcript which is the oral arguments of the petitioner(s)
  # lookbehind = start of argument for petitioners, lookahead = start of argument for respondent, capture all text in between
  # CAPITALIZATION MATTERS! these phrases mark the correct parts of the document only when capitalized
  oral_argument_of_petitioner <-
    str_extract(pdf_all_text, "(?<=ON BEHALF OF THE PETITIONER).*(?=ON BEHALF OF .* RESPONDENT)")
  
  
  ## -----> function to get stats for each justice who speaks in this document
  
  
  # this function takes a justice's name as an input, and returns a list containing summary stats for that justice
  single_judge_data <- function(selected_judge){
    
    # print current docket and justice to console so users can see the process running
    print(paste0('Docket: ', docket, ' Judge: ', selected_judge))
    
    # pipeline for each judge wrapped in tryCatch to handle absent judges (example: abstention)
    tryCatch({  
      
      
      ## -----> define regex patterns for this justice
      
      
      # a pattern that indicates this justice is now speaking
      # example: "CHIEF JUSTICE ROBERTS: " "his words here" "CAPITAL LETTERS MARKING THE NEXT SPEAKER" appears whenever Roberts speaks
      # negative lookahead marks the end by identify a series of capital letters (the next speaker or document section is written in all caps)
      justice_is_speaking_pattern <- paste0(selected_judge,": .+?(?=\\s[A-Z][A-Z]+\\.?\\s)")
      
      # a pattern in the text which indicates that the justice has interrupted another speaker
      justice_is_interrupting_pattern <- paste0("-- ", selected_judge)
      
      
      ## -----> extract this justice's speech from the document
      
      
      # get all the text where the selected justice is speaking
      justice_speech <- str_extract_all(oral_argument_of_petitioner, justice_is_speaking_pattern, simplify = TRUE)
      
      # transform the text so - now each row represents one uninterrupted segment of spoken words from the justice
      justice_speech <-
        justice_speech %>% 
        as.data.frame() %>% 
        pivot_longer(everything()) %>% 
        select('value') %>% 
        rename('spoken_words' = 'value')
      
      
      ## -----> sentenced-based sentiment analysis with sentimentr
      
      
      # get sentences and compute sentiment score with sentimentr
      justice_sentences_with_sentimentr_scores <-
        justice_speech %>% 
        sentimentr::get_sentences() %>% 
        sentimentr::sentiment() %>% 
        mutate(spoken_words = str_remove_all(spoken_words, paste0(selected_judge, ": "))) # clean justice's name from text
      
      # store most positive sentence in a vector
      justice_most_positive_sentence <-
        justice_sentences_with_sentimentr_scores %>% 
        arrange(sentiment) %>% 
        slice_tail() %>% 
        pull(spoken_words)
      
      # store most negative sentence in a vector
      justice_most_negative_sentence <-
        justice_sentences_with_sentimentr_scores %>% 
        arrange(sentiment) %>% 
        slice_head() %>% 
        pull(spoken_words)
      
      # calculate mean sentiment of all sentences, weighted by word count of each sentence
      justice_mean_sentiment_sentimentr <- weighted.mean(justice_sentences_with_sentimentr_scores$sentiment, 
                                                         w = justice_sentences_with_sentimentr_scores$word_count)
      
      
      ## -----> token-based sentiment analysis with afinn lexicon
      
      
      # get all the words and count them
      justice_speech_tokens <-
        justice_speech %>% 
        unnest_tokens(word, spoken_words) %>% 
        anti_join(stop_words) %>% 
        anti_join(custom_stop_words) %>% 
        group_by(word) %>% 
        summarize(count = n()) %>% 
        arrange(desc(count))
      
      # get sentiments from 'afinn' because we want a numeric score so we can compare sentiments between justices, cases
      justice_sentiments_afinn <- 
        justice_speech_tokens %>% 
        inner_join(get_sentiments('afinn')) %>% 
        mutate(count_times_value = count * value)
      
      # get mean sentiment score of the justice's word
      justice_mean_sentiment_afinn <- 
        justice_sentiments_afinn %>% 
        summarize(mean = mean(count_times_value)) %>% 
        pull(mean)
      
      
      ## -----> other analysis on the justice's speech
      
      
      # count the number of time this justice interrupted another speaker
      justice_interruptions <- 
        str_extract_all(oral_argument_of_petitioner, justice_is_interrupting_pattern) %>% 
        unlist() %>% 
        length()
      
      # count the number of questions asked by this justice
      justice_questions <-
        justice_speech %>% 
        pull(spoken_words) %>% 
        str_flatten(collapse = "") %>% 
        str_remove_all(justices_referring_to_each_other) %>% # remove polite conversation between justices
        str_extract_all(pattern = "\\?") %>% 
        unlist() %>% 
        length()
      
      # count total words spoken (in this case, we'll also count stop words to get an accurate picture of how much they talked)
      justice_count_words_spoken <- 
        justice_speech %>% 
        unnest_tokens(word, spoken_words) %>% 
        pull(word) %>% 
        length()
      
      # top word spoken by the justice in this case
      justice_top_word <- 
        justice_speech_tokens %>% 
        head(1) %>% 
        pull(word)
      
      # store all the unigrams in a string so that we can put it in one cell in the dataframe
      justice_all_unigrams_as_a_string <-
        justice_speech %>% 
        unnest_tokens(word, spoken_words) %>% 
        anti_join(stop_words) %>% 
        anti_join(custom_stop_words) %>% 
        pull(word) %>% 
        str_flatten(collapse = " ")
      
      
      ## -----> store data in list
      
      
      # create a list of the information gathered
      summary_row <- list(docket_number = docket,
                          date_argued = date,
                          justice = selected_judge,
                          sentiment_score_afinn = justice_mean_sentiment_afinn,
                          sentiment_score_sentimentr = justice_mean_sentiment_sentimentr,
                          questions = justice_questions,
                          interruptions = justice_interruptions,
                          words_spoken = justice_count_words_spoken,
                          top_word = justice_top_word,
                          unigrams = justice_all_unigrams_as_a_string,
                          most_positive_sentence = justice_most_positive_sentence,
                          most_negative_sentence = justice_most_negative_sentence
                          )
      
    }, 
    
    # error condition for tryCatch - does nothing except allow the process to continue
    error = function(e){})
    
    # tryCatch on returning collected data - return a special NA list if there was an error
    tryCatch(return(summary_row), error = function(e){
      summary_row <- list(docket_number = docket,
                          date_argued = date,
                          justice = selected_judge,
                          sentiment_score_afinn = NA,
                          sentiment_score_sentimentr = NA,
                          questions = NA,
                          interruptions = NA,
                          words_spoken = NA,
                          top_word = NA,
                          unigrams = NA,
                          most_positive_sentence = NA,
                          most_negative_sentence = NA)
      
      return(summary_row)
      
    })
  }
  
  # get list of stats for each justice in the case, and bind lists to a dataframe
  single_case <- rbindlist(lapply(justices, single_judge_data))
  
  return(single_case)
}


####################
####################
##     PROCESS    ##
##       ALL      ##
##      CASES     ##
####################
####################


# for every file in pdf_filenames, call the process_single_case() function which returns a dataframe, then use rbindlist to bind rows for all dataframes
all_cases <- rbindlist(lapply(pdf_filenames, process_single_case))


####################
####################
##   READ & JOIN  ##
##  JUSTICE VOTE  ##
##     RECORDS    ##
####################
####################


# data.table's fread() handles the docket column correctly by default - read_csv() may have errors
vote_records <- fread(vote_records_csv_path)

# columns to keep for the vote records table
vote_records_columns_to_keep <- c('docket', 'justiceName', 'caseName', 'petitioner_wins', 'justice_in_majority', 'voted_for_petitioner')

# get TRUE/FALSE values for whether the petitioner won and whether the justice voted with the  majority or not
# then use boolean logic to determine if the justice voted in the petitioners favor or not
vote_records <-
  vote_records %>% 
  mutate(petitioner_wins = ifelse(partyWinning == 1, TRUE, FALSE)) %>% # if the petitioner "won" (val = 1) then TRUE, else (val = 0 (lose) or 2 (unclear)) FALSE
  mutate(justice_in_majority = ifelse(majority == 2, TRUE, FALSE)) %>% # if majority == 2, the justice voted with the majority, otherwise we consider it a dissent
  mutate(voted_for_petitioner = petitioner_wins & justice_in_majority) %>% # combinations of TRUE & FALSE tell us whether the judge voted for the petitioner or not
  select(all_of(vote_records_columns_to_keep))

# rename justices to match format from the other table
# since there are only 9 justices, it's easier just to do this manually
vote_records <-
  vote_records %>% 
  mutate(justiceName = ifelse(justiceName == "JGRoberts", "CHIEF JUSTICE ROBERTS", justiceName)) %>% 
  mutate(justiceName = ifelse(justiceName == "CThomas", "JUSTICE THOMAS", justiceName)) %>%
  mutate(justiceName = ifelse(justiceName == "SGBreyer", "JUSTICE BREYER", justiceName)) %>%
  mutate(justiceName = ifelse(justiceName == "SAAlito", "JUSTICE ALITO", justiceName)) %>%
  mutate(justiceName = ifelse(justiceName == "SSotomayor", "JUSTICE SOTOMAYOR", justiceName)) %>%
  mutate(justiceName = ifelse(justiceName == "EKagan", "JUSTICE KAGAN", justiceName)) %>%
  mutate(justiceName = ifelse(justiceName == "NMGorsuch", "JUSTICE GORSUCH", justiceName)) %>%
  mutate(justiceName = ifelse(justiceName == "BMKavanaugh", "JUSTICE KAVANAUGH", justiceName)) %>%
  mutate(justiceName = ifelse(justiceName == "RBGinsburg", "JUSTICE GINSBURG", justiceName))

# join vote records to the analysis table
all_cases_with_votes <-
  left_join(all_cases, vote_records, by = c("justice" = "justiceName", "docket_number" = "docket"))

# interesting NAs in docket 18-217: there were no votes, because the petition was dismissed with consent of both parties
# leave most NAs alone, but fill in the caseName variable so we can use it plot tooltips
all_cases_with_votes$caseName[all_cases_with_votes$docket_number == "18-217"] <- "MATHENA v. MALVO"


####################
####################
##      WRITE     ##
##   CLEAN TABLE  ##
##     TO CSV     ##
####################
####################


fwrite(all_cases_with_votes, file = 'data/clean/all_cases_with_votes.csv')



#######################################################################
###################                                 ###################
###################             VISUALIZE           ###################
###################                AND              ###################
###################              ANALYZE            ###################
###################                                 ###################
#######################################################################


# ------------------------------------------------------------------------------ section on Thomas's graphs

#### address in report: JUSTICE THOMAS is not a data error, *he really doesn't talk that much*

# Justice Thomas has a reputation for being quiet. He literally went 10 years without asking a question, [ending his silent streak in 2016].
# link to this article from 2016 to establish his reputation: https://www.nytimes.com/2016/03/01/us/politics/supreme-court-clarence-thomas.html

# However, [he has gotten more chatty during the pandemic]. This analysis might look different if it were run on the current court session. link to this article: https://www.nytimes.com/2021/05/03/us/politics/clarence-thomas-supreme-court.html

# I'm addressing this up front because you may notice that his graphs are markedly different from those of other justices. This missing data for REORDER(questions, interruptions, sentiment) is not an error - *he just didn't say anything during the oral argument of the petitioner in those cases*.

### ---------------------------------------------------------------------------- interruptions


# GOOD
# interactivity: label case name, `Interruptions given: `
# BOX PLOT
plot_interruptions_by_vote <-
all_cases_with_votes %>% 
  drop_na(voted_for_petitioner) %>% 
  mutate(voted_for_petitioner = factor(voted_for_petitioner, levels = c("TRUE", "FALSE"))) %>% 
  ggplot(aes(x = voted_for_petitioner, y = interruptions, fill = justice)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_jitter_interactive(aes(color = justice, 
                              tooltip = paste0("Case: ", caseName,
                                               "\n",
                                               "Interruptions by justice: ", interruptions)),
                          width = 0.2, alpha = 0.35) +
  theme(legend.position = "none") +
  labs(x = "Voted For Petitioner", y = "Number of Interruptions") +
  facet_wrap(~ justice)

# show interactive plot
ggiraph(ggobj = plot_interruptions_by_vote)

# Do justices interrupt more or less on average, depending on how they vote?
# H0: There is **no difference** in the mean number of interruptions by the justices, depending on whether they voted FOR or AGAINST the petitioner.
# HA: The mean number of interruptions by the justices **is different**, depending on whether they voted FOR or AGAINST the petitioner.
# Conclusion: We reject the null hypothesis. The average number of interruptions in a case is higher when they voted AGAINST the petitioner, {val mean a} VS {val mean b}, with a p-value of {p-value}.
# Unfortunately, the sample size is too small to hypothesis test each justice individually.
# However, we can take a look at the boxplots below to take some (statistically unsound!) guess about for which justices this pattern might hold true.
ttest_interruptions_justices <- t.test(interruptions ~ voted_for_petitioner, data = all_cases_with_votes)

# not enough evidence to associate higher mean with losing the case (p = 0.14)
ttest_interruptions_win_the_case <- t.test(interruptions ~ petitioner_wins, data = all_cases_with_votes)

######### ---------------------------------------------------------------------- words

# GOOD ENOUGH
# interactivity: label case name
# words spoken by justices over time 
plot_words_spoken_over_time <-
all_cases_with_votes %>% 
  ggplot(aes(x = date_argued, y = words_spoken, color = justice)) +
  geom_smooth() + 
  geom_point_interactive(alpha = 0.5, aes(tooltip = paste0("Case: ", caseName,
                                                           "\n",
                                                           "Words spoken: ", words_spoken))) +
  labs(x = 'Date of Oral Argument', y = 'Spoken Word Count') +
  theme(legend.position = "none") +
  facet_wrap(~ justice)

# show plot
ggiraph(ggobj = plot_words_spoken_over_time)

# GOOD
# interactivity: for point, label mean value, for segment label range or difference of values
# barbell plot with some awkward coding to circumvent issues with multiple guides in ggplot
plot_words_spoken_by_vote_type <-
all_cases_with_votes %>% 
  drop_na(voted_for_petitioner) %>% 
  group_by(justice, voted_for_petitioner) %>% 
  summarize(mean_words_spoken = mean(words_spoken, na.rm = TRUE)) %>%
  rename(`Vote Type` = voted_for_petitioner) %>% 
  mutate(`Vote Type` = ifelse(`Vote Type` == TRUE, "For the Petitioner", "Against the Petitioner")) %>% 
  ggplot(aes(x = reorder(justice, -mean_words_spoken), y = mean_words_spoken)) +
  geom_line_interactive(size = 1.5, color = "grey30") +
  geom_point_interactive(aes(shape = `Vote Type`, fill = `Vote Type`, 
                             tooltip = paste0(round(mean_words_spoken, 0), " words on average")), 
                         size = 6) + 
  scale_shape_manual(values = c(25, 24)) +
  scale_fill_manual(values = c("orangered1", "dodgerblue4")) +
  labs(x = "", y = "Spoken Word Count") +
  coord_flip() +
  theme(legend.position = 'top')


# show plot
ggiraph(ggobj = plot_words_spoken_by_vote_type)

#### --------------------------------------------------------------------------- questions


# p value of 0.01... yeah, they ask more questions when they vote against
questions_t_test <- t.test(questions ~ voted_for_petitioner, data = all_cases_with_votes)

# GOOD
# interactivity: label case name, `questions given: `
# BOX PLOT
plot_questions_asked_by_vote_type <-
all_cases_with_votes %>% 
  drop_na(voted_for_petitioner) %>% 
  mutate(voted_for_petitioner = factor(voted_for_petitioner, levels = c("TRUE", "FALSE"))) %>% 
  ggplot(aes(x = voted_for_petitioner, y = questions, fill = justice)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_jitter_interactive(aes(color = justice,
                              tooltip = paste0("Case: ", caseName,
                                               "\n",
                                               "Questions: ", questions)), 
                          width = 0.2, alpha = 0.35) +
  theme(legend.position = "none") +
  labs(x = "Voted For Petitioner", y = "Number of Questions Asked") +
  facet_wrap(~ justice)

# show plot
ggiraph(ggobj = plot_questions_asked_by_vote_type)

# function takes a justice as an argument, and performs a t-test on the data filtered to that justice
# t-test: is the mean number of questions different depending on whether the justice voted for or against the petitioner
mean_questions_t_test <- function(one_judge){
  
  # filter data to the the judge we want to examine
  filtered_data <- all_cases_with_votes[justice == one_judge]
  
  # students t-test: any difference in means when voted_for_petitioner is TRUE vs FALSE?
  questions_t_test_result <- t.test(questions ~ voted_for_petitioner, data = filtered_data)
  
  # label test results as significant/insignificant based on p-value cutoff of 0.05
  significance <- ifelse(questions_t_test_result$p.value < 0.05, "Significant", "Not Significant")
  
  # store results in list
  one_judge_t_questions_t_test_result <- list(Justice = one_judge,
                                              `Voted for Petitioner` = round(questions_t_test_result$estimate[['mean in group TRUE']], 2),
                                              `Voted against Petitioner` = round(questions_t_test_result$estimate[['mean in group FALSE']], 2),
                                              `Statistical Significance` = significance,
                                              `P-Value` = round(questions_t_test_result$p.value, 3))
  
}

# do t-test for all justices
questions_table <- rbindlist(lapply(justices, mean_questions_t_test))

# arrange by p value to show relevant justices first
questions_table %>% 
  arrange(`P-Value`)


### ---------------------------------------------------------------------------- sentiment


#SHOW PLOTS SIDE BY SIDE TO ILLUSTRATE DIFFERENT MEANS (positive leaning VS negative leaning)
plot_sentiment_density_afinn <-
all_cases_with_votes %>% 
  ggplot(aes(x = sentiment_score_afinn)) +
  geom_density(fill = "lightsteelblue3") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_x_continuous(limits = c(-10,10)) +
  labs(title = "afinn", x = "Score", y = "Density")

plot_sentiment_density_sentimentr <-
all_cases_with_votes %>% 
  ggplot(aes(x = sentiment_score_sentimentr)) +
  geom_density(fill = "tomato2") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_x_continuous(limits = c(-1,1)) +
  labs(title = "sentimentr", x = "Score", y = "Density")

# show plots side-by-side
ggarrange(plot_sentiment_density_afinn, 
          plot_sentiment_density_sentimentr,
          ncol = 2,
          nrow = 1)

# END SIDE-BY-SIDE PLOTS 

# OKAY - interesting
# scatter of z-scores for afinn and sentimentr
# the scores are weakly correlated within 2 standard deviations of their respective means
# and very different at the extremes
plot_sentiment_z_scores <- 
all_cases_with_votes %>%
  drop_na(sentiment_score_afinn, sentiment_score_sentimentr) %>% 
  mutate(afinn_scaled = (sentiment_score_afinn - mean(sentiment_score_afinn)) / sd(sentiment_score_afinn),
         sentimentr_scaled = (sentiment_score_sentimentr - mean(sentiment_score_sentimentr)) / sd(sentiment_score_sentimentr)) %>% 
  ggplot(aes(x = afinn_scaled, y = sentimentr_scaled)) +
  geom_point_interactive(aes(tooltip = paste0("sentimentr z-score: ", round(sentimentr_scaled, 2),
                                              "\n",
                                              "afinn z-score: ", round(afinn_scaled, 2)))) +
  geom_smooth(color = "red") +
  labs(title = "Scaled afinn & sentimentr scores", x = "afinn z-score", y = "sentimentr z-score")

# show plot
ggiraph(ggobj = plot_sentiment_z_scores)

# I will use afinn for the analysis
# prefer a method which provides numeric scores
# find afinn to be more accurate when inspecting examples **in this particular data**


# GOOD ENOUGH
# tooltip: mean sentiment score?
# as density
plot_sentiment_density_all_justices <-
all_cases_with_votes %>% 
  ggplot(aes(x = sentiment_score_afinn, fill = justice)) + 
  geom_density() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_x_continuous(limits = c(-10,10)) +
  facet_wrap(~ justice) +
  labs(x = "Mean Sentiment Score (afinn)", y = "Density") +
  theme(legend.position =  "none")

# show plot
plot_sentiment_density_all_justices

# we can see some interesting patterns visually...
# the Breyer, Ginsburg, Kagan, and Sotomayor seem to lean a bit negative
# these are the liberal justices
# confirm the negativity with the table
sentiment_table <-
all_cases_with_votes %>% 
  group_by(justice) %>% 
  summarize(`Mean Sentiment Score` = mean(sentiment_score_afinn, na.rm = TRUE)) %>% 
  rename(Justice = justice) %>% 
  arrange(`Mean Sentiment Score`)

sentiment_table

# density conditioned on vote type
plot_sentiment_density_vote_type <-
all_cases_with_votes %>%
  drop_na(voted_for_petitioner) %>% 
  ggplot(aes(sentiment_score_afinn, fill = voted_for_petitioner)) +
  geom_density(alpha = 0.7) +
  labs(x = 'Mean Sentiment Score (afinn)', y = "Density") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_x_continuous(limits = c(-10,10)) +
  scale_fill_manual(name = "Vote Type", labels = c("Against the Petitioner", "For the Petitioner"), values = c("orangered1", "dodgerblue4")) +
  facet_wrap(~ justice) +
  theme(legend.position = "top")

# show plot
plot_sentiment_density_vote_type

## ----------------------------------------------------------------------------> token analysis

## -----> a few more stop words 

# a few more stop words - these words were making visualizations less informative
ggplot_stop_word_list <- data.frame(word = c("person", "court", "law", "question", "read", "people"))

## -----> roll your own td-idf solution.... (talk about it)


# GOOD? - sanity check needed
# interactivity: word, count
# Ginsburg and Gorsuch
plot_top_unigrams_by_vote_type_ginsburg_gorsuch <- 
all_cases_with_votes %>% 
  filter(justice %in% c("JUSTICE GINSBURG", "JUSTICE GORSUCH")) %>% 
  drop_na(voted_for_petitioner, unigrams) %>% 
  group_by(justice, voted_for_petitioner) %>% 
  unnest_tokens(word, unigrams) %>% 
  anti_join(ggplot_stop_word_list) %>% 
  count(word) %>% 
  arrange(justice, voted_for_petitioner, -n) %>% 
  rename(count = n) %>% 
  slice_max(n = 5, order_by = count) %>% ### NUMBER of words per group here
  mutate(directional_count = ifelse(voted_for_petitioner == TRUE, count, count * -1)) %>%
  ggplot(aes(x = reorder(word, directional_count), y = directional_count)) +
  geom_col(aes(fill = voted_for_petitioner)) +
  geom_label(aes(label = word)) +
  labs(title = "Top Words by Vote Type", x = "", y = "Number of Times Word Spoken") +
  scale_x_discrete(labels = NULL, breaks = NULL) +
  scale_y_continuous(breaks = c(-20, -10, 0, 10, 20), limits = c(-20, 20), labels = c("20", "10", "0", "10", "20")) +
  theme(legend.position = "top") +
  scale_fill_manual(name = "", labels = c("Voted Against the Petitioner", "Voted For the Petitioner"), values = c("orangered1", "dodgerblue4")) +
  coord_flip() +
  facet_wrap(~ justice, scales = "free")

# show plot
plot_top_unigrams_by_vote_type_ginsburg_gorsuch


## -----> tf-idf done the right way


# GOOD ENOUGH
# tf-idf for all justices # not conditioned on vote type
# explains each justices unique vocabulary
plot_tf_idf_all_justices <-
all_cases_with_votes %>% 
  drop_na(unigrams) %>% 
  unnest_tokens(word, unigrams) %>% 
  count(justice, word, sort = TRUE) %>% 
  bind_tf_idf(word, justice, n) %>% 
  group_by(justice) %>% 
  slice_max(tf_idf, n = 7) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(tf_idf, word, fill = justice)) +
  geom_col(show.legend = FALSE) +
  labs(x = "tf-idf: Distintive Vocabulary Among the Justices", y = NULL) +
  facet_wrap(~justice, ncol = 3, scales = "free")

# show plot
plot_tf_idf_all_justices

## talk about "Dah", show "dah" screenshots




# GOOD GOOD GOOD
# tf-idf for Roberts FOR vs AGAINST
# with special bar directions and absolute value scale
# tf-idf for Roberts #conditioned on FOR vs AGAINST
# explains unique language from roberts when based on how he feels about the petitioner's argument
plot_tf_idf_roberts_vote_type <-
all_cases_with_votes %>% 
  drop_na(unigrams) %>% 
  filter(justice == "CHIEF JUSTICE ROBERTS") %>% 
  mutate(voted_for_petitioner = ifelse(voted_for_petitioner == TRUE, "Voted For Petitioner", "Voted Against Petitioner")) %>% 
  unnest_tokens(word, unigrams) %>% 
  count(voted_for_petitioner, word, sort = TRUE) %>% 
  bind_tf_idf(word, voted_for_petitioner, n) %>% 
  group_by(voted_for_petitioner) %>% 
  slice_max(tf_idf, n = 5) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, tf_idf)) %>%
  mutate(tf_idf = ifelse(voted_for_petitioner == "Voted For Petitioner", tf_idf, tf_idf * -1)) %>%  #flip direction of bars for "voted against"
  ggplot(aes(tf_idf, word)) +
  geom_col(aes(fill = voted_for_petitioner), show.legend = FALSE) +
  geom_label(aes(label = word)) +
  scale_fill_manual(values = c("orangered1", "dodgerblue4")) +
  scale_y_discrete(labels = NULL, breaks = NULL) +
  labs(x = "tf-idf of Chief Justice Roberts", y = NULL) +
  facet_wrap(~ voted_for_petitioner, ncol = 2, scales = "free") +
  scale_x_continuous(labels = abs) # must be the last line

# show plot
plot_tf_idf_roberts_vote_type
