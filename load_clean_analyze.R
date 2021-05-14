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
library(ggiraph)
library(ggimage)


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


## ------------------------------------- ##
## define constants outside the function ##
## ------------------------------------- ##

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

# custom stop words to withhold from text analysis
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
                                         'argument'))

# expressions to filter out when counting questions - justices politely referring to each other ("Justice Kagan?" = it's your turn to speak, Justice Kagan)
justices_referring_to_each_other <- str_to_title(paste(justices, '\\?', sep = '', collapse = "|"))

## --------------- ##
## define function ##
## --------------- ##

# function takes one PDF file path and returns a dataframe with stats for the justices in that case
process_single_case <- function(filename){
  
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
  
  # this code has a positive look behind for the start of the petitioner's opening oral argument...
  # a capture group which simply takes all text...
  # and a positive lookahead for the beginning of the respondent's opening oral argument (this indicates the end of the petitioner's)
  # CAPITALIZATION MATTERS! these phrases mark the correct parts of the document only when capitalized
  oral_argument_of_petitioner <-
    str_extract(pdf_all_text, "(?<=ON BEHALF OF THE PETITIONER).*(?=ON BEHALF OF .* RESPONDENT)")
  
  # this function takes a justice's name as an input, and returns a list containing summary stats for that justice
  single_judge_data <- function(selected_judge){
    
    # print current docket and justice to console so users can see the process running
    print(paste0('Docket: ', docket, ' Judge: ', selected_judge))
    
    # pipeline for each judge wrapped in tryCatch to handle absent judges (example: abstention)
    tryCatch({  
      
      # a pattern recognized in the text that indicates this justice is now speaking
      # example: "CHIEF JUSTICE ROBERTS: " appears whenever Roberts speaks
      # negative lookahead marks the end by identify a series of capital letters (the next speaker or document section is written in all caps)
      justice_is_speaking_pattern <- paste0(selected_judge,": .+?(?=\\s[A-Z][A-Z]+\\.?\\s)")
      
      # a pattern in the text which indicates that the justice has interrupted another speaker
      justice_is_interrupting_pattern <- paste0("-- ", selected_judge)
      
      # get all the text where the selected justice is speaking
      justice_speech <- str_extract_all(oral_argument_of_petitioner, justice_is_speaking_pattern, simplify = TRUE)
      
      # transform the text so - now each row represents one uninterrupted segment of spoken words from the justice
      justice_speech <-
        justice_speech %>% 
        as.data.frame() %>% 
        pivot_longer(everything()) %>% 
        select('value') %>% 
        rename('spoken_words' = 'value')
      
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
      justice_sentiments <- 
        justice_speech_tokens %>% 
        inner_join(get_sentiments('afinn')) %>% 
        mutate(count_times_value = count * value)
      
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
      
      # get mean sentiment score of the justice's word
      justice_mean_sentiment <- 
        justice_sentiments %>% 
        summarize(mean = mean(count_times_value)) %>% 
        pull(mean)
      
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
      
      # create a list of the information gathered
      summary_row <- list(docket_number = docket,
                          date_argued = date,
                          justice = selected_judge,
                          sentiment_score = justice_mean_sentiment,
                          questions = justice_questions,
                          interruptions = justice_interruptions,
                          words_spoken = justice_count_words_spoken,
                          top_word = justice_top_word,
                          unigrams = justice_all_unigrams_as_a_string)
      
    }, 
    
    # error condition for tryCatch - does nothing except allow the process to continue
    error = function(e){})
    
    # tryCatch on returning collected data - return a special NA list if there was an error
    tryCatch(return(summary_row), error = function(e){
      summary_row <- list(docket_number = docket,
                          date_argued = date,
                          justice = selected_judge,
                          sentiment_score = NA,
                          questions = NA,
                          interruptions = NA,
                          words_spoken = NA,
                          top_word = NA,
                          unigrams = NA)
      
      return(summary_row)
      
    })
  }
  
  # get list of stats for ech justice in the case, and bind lists to a dataframe
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
###################                                 ###################
###################                                 ###################
###################                                 ###################
###################                                 ###################
#######################################################################

### TODO: add interactivity with ggiraph?
## geom_point() can be labeled with case names

# ------------------------------------------------------------------------------ section on Thomas's graphs

#### address in report: JUSTICE THOMAS is not a data error, *he really doesn't talk that much*

# Justice Thomas has a reputation for being quiet. He literally went 10 years without asking a question, [ending his silent streak in 2016].
# link to this article from 2016 to establish his reputation: https://www.nytimes.com/2016/03/01/us/politics/supreme-court-clarence-thomas.html

# However, [he has gotten more chatty during the pandemic]. This analysis might look different if it were run on the current court session. link to this article: https://www.nytimes.com/2021/05/03/us/politics/clarence-thomas-supreme-court.html

# I'm addressing this up front because you may notice that his graphs are markedly different from those of other justices. This missing data for REORDER(questions, interruptions, sentiment) is not an error - *he just didn't say anything during the oral argument of the petitioner in those cases*.

### ---------------------------------------------------------------------------- interruptions


# interruptions committed by justices over time
all_cases_with_votes %>% 
  ggplot(aes(x = date_argued, y = interruptions, color = justice)) +
  geom_smooth() + 
  geom_point(alpha = 0.5) +
  labs(x = 'Date of Oral Argument', y = 'Interruptions') +
  theme(legend.position = "none") +
  # scale_y_continuous()
  facet_wrap(~ justice)

# facet by voted with petitioner
# interactivity: statistical significance?
# BAR PLOT
all_cases_with_votes %>% 
  drop_na(voted_for_petitioner) %>% 
  group_by(justice, voted_for_petitioner) %>% 
  summarize(mean_interruptions = mean(interruptions,  na.rm = TRUE)) %>% 
  mutate(voted_for_petitioner = factor(voted_for_petitioner, levels = c("TRUE", "FALSE"))) %>% 
  ggplot(aes(x = voted_for_petitioner, y = mean_interruptions, fill = justice)) +
  geom_col() +
  theme(legend.position = "none") +
  facet_wrap(~ justice)

# BOX PLOT
all_cases_with_votes %>% 
  drop_na(voted_for_petitioner) %>% 
  mutate(voted_for_petitioner = factor(voted_for_petitioner, levels = c("TRUE", "FALSE"))) %>% 
  ggplot(aes(x = voted_for_petitioner, y = interruptions, fill = justice)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.35) +
  theme(legend.position = "none") +
  facet_wrap(~ justice)

# Do justices interrupt more or less on average, depending on how they vote?
# H0: There is **no difference** in the mean number of interruptions by the justices, depending on whether they voted FOR or AGAINST the petitioner.
# HA: The mean number of interruptions by the justices **is different**, depending on whether they voted FOR or AGAINST the petitioner.
# Conclusion: We reject the null hypothesis. The average number of interruptions in a case is higher when they voted AGAINST the petitioner, {val mean a} VS {val mean b}, with a p-value of {p-value}.
# Unfortunately, the sample size is too small to hypothesis test each justice individually.
# However, we can take a look at the boxplots below to take some (statistically unsound!) guess about for which justices this pattern might hold true.
ttest_interruptions <- t.test(interruptions ~ voted_for_petitioner, data = all_cases_with_votes)

# density graph
all_cases_with_votes %>% 
  drop_na(voted_for_petitioner) %>% 
  mutate(voted_for_petitioner = factor(voted_for_petitioner, levels = c("TRUE", "FALSE"))) %>% 
  ggplot(aes(x = interruptions, fill = voted_for_petitioner)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ justice)

######### ---------------------------------------------------------------------- words

# words spoken by justices over time 
all_cases_with_votes %>% 
  ggplot(aes(x = date_argued, y = words_spoken, color = justice)) +
  geom_smooth() + 
  geom_point(alpha = 0.5) +
  labs(x = 'Date of Oral Argument', y = 'Word Count') +
  theme(legend.position = "none") +
  # scale_y_continuous()
  facet_wrap(~ justice)

# words spoken by justices bar means 
all_cases_with_votes %>% 
  group_by(justice, voted_for_petitioner) %>% 
  summarize(mean_words_spoken = mean(words_spoken, na.rm = TRUE)) %>% 
  ggplot(aes(x = voted_for_petitioner, y = mean_words_spoken, color = justice)) +
  geom_col() +
  # labs(x = 'Date of Oral Argument', y = 'Word Count') +
  theme(legend.position = "none") +
  # scale_y_continuous()
  facet_wrap(~ justice)

# words spoken by justices heatmap 
all_cases_with_votes %>% 
  drop_na(voted_for_petitioner) %>% 
  mutate(voted_for_petitioner = factor(voted_for_petitioner, levels = c("TRUE", "FALSE"))) %>% 
  group_by(justice, voted_for_petitioner) %>% 
  summarize(mean_words_spoken = mean(words_spoken, na.rm = TRUE)) %>% 
  ggplot(aes(x = voted_for_petitioner, y = justice, fill = mean_words_spoken)) +
  geom_tile() +
  theme(legend.position = "none") 

# lollipop graph
all_cases_with_votes %>% 
  drop_na(voted_for_petitioner) %>% 
  mutate(voted_for_petitioner = factor(voted_for_petitioner, levels = c("TRUE", "FALSE"))) %>% 
  group_by(justice, voted_for_petitioner) %>% 
  summarize(mean_words_spoken = mean(words_spoken, na.rm = TRUE)) %>% 
  ggplot(aes(x = reorder(justice, -mean_words_spoken), y = mean_words_spoken)) +
  geom_segment(aes(xend = justice,
                   y = min(mean_words_spoken),
                   yend = mean_words_spoken), size = 2) +
  geom_point(size = 5) +
  coord_flip()

# all_cases_with_votes %>% 
#   drop_na(voted_for_petitioner) %>% 
#   group_by(justice, voted_for_petitioner) %>% 
#   summarize(mean_words_spoken = mean(words_spoken, na.rm = TRUE)) %>% 
#   pivot_wider(id_cols = justice, names_from = voted_for_petitioner, values_from = mean_words_spoken) %>% 
#   rename("against_petitioner" = `FALSE`, "for_petitioner" = `TRUE`) %>% 
#   ggplot(aes(x = for_petitioner, xend = against_petitioner, y = reorder(justice, -against_petitioner))) +
#   geom_dumbbell(size=2, color="#0B0A09", 
#                 colour_x = "#5b8124", colour_xend = "#bad744",
#                 dot_guide=FALSE)

# custom barbell plot
# tooltip: value on hover
all_cases_with_votes %>% 
  drop_na(voted_for_petitioner) %>% 
  group_by(justice, voted_for_petitioner) %>% 
  summarize(mean_words_spoken = mean(words_spoken, na.rm = TRUE)) %>% 
  pivot_wider(id_cols = justice, names_from = voted_for_petitioner, values_from = mean_words_spoken) %>% 
  ungroup() %>% 
  rename("against_petitioner" = `FALSE`, "for_petitioner" = `TRUE`) %>% 
  ggplot(aes(x = reorder(justice, -against_petitioner), y = against_petitioner)) +
  geom_segment(aes(y = for_petitioner, yend = against_petitioner, xend = justice)) +
  geom_point(aes(y = for_petitioner), shape = 24, fill = '#2e40b8', size = 5) +
  geom_point(aes(y = against_petitioner), shape = 25, fill = '#f58442', size = 5) +
  labs(x = "", y = "Word Count") +
  coord_flip()

# barbell plot with some awkward coding to circumvent issues with multiple guides in ggplot
all_cases_with_votes %>% 
  drop_na(voted_for_petitioner) %>% 
  group_by(justice, voted_for_petitioner) %>% 
  summarize(mean_words_spoken = mean(words_spoken, na.rm = TRUE)) %>%
  rename(`Vote Type` = voted_for_petitioner) %>% 
  mutate(`Vote Type` = ifelse(`Vote Type` == TRUE, "For the Petitioner", "Against the Petitioner")) %>% 
  ggplot(aes(x = reorder(justice, -mean_words_spoken), y = mean_words_spoken)) +
  geom_line(size = 1.5) +
  geom_point(aes(shape = `Vote Type`, fill = `Vote Type`), size = 6) + 
  scale_shape_manual(values = c(25, 24)) +
  scale_color_manual(values = c("#f58442", "#2e40b8"))+
  labs(x = "", y = "Word Count") +
  coord_flip() +
  theme(legend.position = 'top')


#### --------------------------------------------------------------------------- questions

# questions asked by justices over time 
all_cases_with_votes %>% 
  ggplot(aes(x = date_argued, y = questions, color = justice)) +
  geom_smooth() + 
  geom_point(alpha = 0.5) +
  labs(x = 'Date of Oral Argument', y = 'Questions') +
  theme(legend.position = "none") +
  # scale_y_continuous()
  facet_wrap(~ justice)

