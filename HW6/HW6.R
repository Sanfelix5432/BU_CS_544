library(stringr)
library(tidyverse)
#part1
#a
file <- "https://people.bu.edu/kalathur/datasets/mlk.txt"
words <- scan(file, what = character())

# Detect words with punctuation symbols
punct_words <- words[str_detect(words, "[[:punct:]]")]
print(punct_words)
#b
# Replace punctuation symbols with empty string and convert to lowercase
new_words <- str_replace_all(words, "[[:punct:]]", "") %>% tolower()
new_words
#c
new_words <- str_replace_all(words, "[[:punct:]]", "") %>% tolower()
# find top 5 frequent words
top_words <- sort(table(new_words), decreasing = TRUE)[1:5]
top_words
stopfile <- "https://people.bu.edu/kalathur/datasets/stopwords.txt"
stopwords <- scan(stopfile, what=character())
# remove stopwords
new_words_no_stopwords <- new_words[!new_words %in% stopwords]
# find top 5 frequent words
top_words_no_stopwords <- sort(table(new_words_no_stopwords), decreasing = TRUE)[1:5]
top_words_no_stopwords
#d
library(ggplot2)
# find word lengths
word_lengths <- str_length(new_words)
# create frequency table
freq_table <- as.data.frame(table(word_lengths))
freq_table <- setNames(freq_table, c("Word_Length", "Frequency"))
# plot frequency distribution
ggplot(freq_table, aes(x = Word_Length, y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Word Length", y = "Frequency", title = "Word Length Distribution")
# create frequency table
word_lengths_no_stopwords <- str_length(new_words_no_stopwords)
freq_table_no_stopwords <- as.data.frame(table(word_lengths_no_stopwords))
freq_table_no_stopwords <- setNames(freq_table_no_stopwords, c("Word_Length", "Frequency"))

# plot frequency distribution
ggplot(freq_table_no_stopwords, aes(x = Word_Length, y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Word Length", y = "Frequency", title = "Word Length Distribution without Stopwords")



#e
# Words with longest length
longest_words <- new_words[which.max(nchar(new_words))]
print(longest_words)
#f
# Words starting with "c"
c_words <- new_words[startsWith(new_words, "c")]
print(c_words)
#g
# Words ending with "r"
r_words <- new_words[endsWith(new_words, "r")]
print(r_words)
#h
# Words starting with "c" and ending with "r"
cr_words <- new_words[startsWith(new_words, "c") & endsWith(new_words, "r")]
print(cr_words)

#part2
#a
url <- "https://people.bu.edu/kalathur/usa_daily_avg_temps.csv"
download.file(url, destfile = "usa_daily_avg_temps.csv", mode = "wb")
usaDailyTemps <- read.csv("usa_daily_avg_temps.csv", header = TRUE) %>%
  as_tibble()
usaDailyTemps
#b
maxTempsByYear <- usaDailyTemps %>%
  group_by(year) %>%
  summarise(max_temp = max(avgtemp)) %>%
  ungroup()
maxTempsByYear
ggplot(maxTempsByYear, aes(x = year, y = max_temp)) +
  geom_col()
#c
maxTempsByState <- usaDailyTemps %>%
group_by(state) %>%
  summarise(max_temp = max(avgtemp)) %>%
  ungroup()
maxTempsByState
ggplot(maxTempsByState, aes(x = state, y = max_temp)) +
  geom_col() +
  xlab("State") +
  ylab("Maximum Temperature")
#d
bostonDailyTemps <- usaDailyTemps %>%
  filter(city == "Boston")
bostonDailyTemps
#e
avgTempsByMonth <- bostonDailyTemps %>%
  group_by(month) %>%
  summarise(avg_temp = mean(avgtemp)) %>%
  ungroup()
avgTempsByMonth
ggplot(avgTempsByMonth, aes(x = month, y = avg_temp)) +
  geom_line() +
  xlab("Month") +
  ylab("Average Temperature")

