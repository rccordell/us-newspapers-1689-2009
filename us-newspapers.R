library(tidyverse)
library(tidytext)
library(plotly)

papers <- read_csv("./us-newspapers/data/US-Newspapers.csv") %>%
  select(title, state, city, start, end, frequency, language) %>%
  filter(start != 9999) %>% 
  mutate(end = replace(end, end == 9999, 2014)) %>%
  mutate(startDecade = paste(substring(start, 1,3))) %>%
  mutate(startDecade = as.numeric(paste(startDecade, 0, sep=""))) %>%
  mutate(endDecade = paste(substring(end, 1,3))) %>%
  mutate(endDecade = as.numeric(paste(endDecade, 0, sep=""))) %>%
  unique()

# top words in newspaper titles, sorted by frequency
papers %>% 
  unnest_tokens(word, title) %>% View()
  anti_join(stop_words) %>%
  group_by(start, word) %>% 
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  View()
  
stopWords <- as_data_frame(c("the", "an", "and", "der", 
                             "die", "das", "und", "of",
                             "in","aus","dem","or")) %>%
  rename(word = value)

# top n words in newspaper titles, sorted by start decade
papers %>% 
  unnest_tokens(word, title) %>%
  anti_join(stopWords) %>%
  group_by(startDecade, word) %>% 
  summarize(count = n()) %>%
  arrange(startDecade,desc(count)) %>%
  top_n(10) %>%
  View()

titleWords <- papers %>% 
  unnest_tokens(word, title) %>%
  anti_join(stopWords) %>%
  group_by(startDecade, word) %>% 
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count)) %>%
  mutate(decadeCount = length(startDecade)) %>%
  arrange(startDecade,desc(count))

# plot top words
ggplot(titleWords) +
  aes(x=percentage,y=startDecade,label=word) + 
  geom_point(alpha=.3) + 
  geom_text(check_overlap = TRUE)

# by raw count
plot <- ggplot(titleWords %>% 
                 filter(startDecade >= 1800 & startDecade <= 1900) %>%
                 top_n(5), 
               aes(x=start, y=count, color = word)) +
  geom_line() +
  geom_point()

ggplotly(plot)

# by percentage
newPapers <- papers %>% 
  group_by(startDecade) %>%
  summarise(newPapers = n())

titleWords <- titleWords %>%
  left_join(newPapers, by = "startDecade") %>%
  mutate(percentage = count/newPapers) %>%
  arrange(startDecade, desc(percentage))

plot <- ggplot(titleWords %>%
                 filter(startDecade >= 1800 & startDecade <= 1950) %>%
                 top_n(3) %>% 
                 filter(percentage >= .02)) +
  aes(x=startDecade, y=percentage, color = word) +
  geom_line() +
  geom_point(size = .3) +
  ggtitle("Most Used Words in New Newspaper Titles by Decade, 1800-1950") +
  labs(x="Decades",y="Percentage of Titles",fill="Word",caption="The top words used in the titles of new newspapers during the nineteenth century by decade") + 
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=18, hjust=0.5)) +
  theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) + 
  theme(legend.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) +
  theme(legend.background = element_rect(color = "#efefef")) +
  theme(plot.caption = element_text(family = "Trebuchet MS", color="#666666", size=10, hjust = 0.5, margin = margin(15, 0, 15, 0))) +
  theme(axis.text = element_text(family = "Trebuchet MS", color="#aaaaaa", face="bold", size=10)) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.major = element_line(color = "#efefef")) +
  theme(axis.ticks = element_line(color = "#efefef"))

ggplotly(plot)

# By Year

stopWords <- as_data_frame(c("the", "an", "and", "der", "die", "das", "und", "of","in","aus","dem","or")) %>%
  rename(word = value)

titleWords <- papers %>% 
  unnest_tokens(word, title) %>%
  anti_join(stopWords) %>%
  group_by(start, word, startDecade, end) %>% 
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(start) %>%
  arrange(start,desc(count)) 

newPapers <- papers %>% 
  group_by(start) %>%
  summarise(newPapers = n())

titleWords <- titleWords %>%
  left_join(newPapers, by = "start") %>%
  mutate(percentage = count/newPapers) %>%
  arrange(start, desc(percentage))

plot <- ggplot(titleWords %>%
                 filter(start >= 1800 & start <= 1950) %>%
                 arrange(start,desc(percentage)) %>%
                 top_n(3)) + 
               aes(x=start, y=percentage, color = word) +
  geom_line() +
  geom_point(size = .3) +
  ggtitle("Most Used Words in New Newspaper Titles by Year, 1800-1950") +
  labs(x="Years",y="Percentage of Titles",fill="Word",caption="The top words used in the titles of new newspapers during the nineteenth century by decade") + 
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=18, hjust=0.5)) +
  theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) + 
  theme(legend.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) +
  theme(legend.background = element_rect(color = "#efefef")) +
  theme(plot.caption = element_text(family = "Trebuchet MS", color="#666666", size=10, hjust = 0.5, margin = margin(15, 0, 15, 0))) +
  theme(axis.text = element_text(family = "Trebuchet MS", color="#aaaaaa", face="bold", size=10)) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.major = element_line(color = "#efefef")) +
  theme(axis.ticks = element_line(color = "#efefef"))

ggplotly(plot)


# Extant Titles by Year
extantPapers <- papers %>%
  rowwise() %>%
  do(data.frame(title = .$title, year = seq(.$start, .$end)))

extantTitleWords <- extantPapers %>% 
  unnest_tokens(word, title) %>% 
  anti_join(stopWords) %>%
  group_by(year, word) %>% 
  summarize(count = n()) %>%
  arrange(year,desc(count))

extantPapersByYear <- extantPapers %>% 
  group_by(year) %>%
  summarise(extantPapers = n())

extantTitleWords <- extantTitleWords %>%
  left_join(extantPapersByYear, by = "year") %>%
  mutate(percentage = count/extantPapers)

plot <- ggplot(extantTitleWords %>%
                 filter(year >= 1800 & year <= 1950) %>%
                 arrange(year,desc(percentage)) %>%
                 top_n(10)) +
  aes(x=year, y=percentage, color = word) +
  geom_line() +
  geom_point(size = .3) +
  ggtitle("Most Used Words in Extant Newspaper Titles by Year, 1800-1950") +
  labs(x="Years",y="Percentage of Titles",fill="Word",caption="The top words used in the titles of extant newspapers during the nineteenth century by decade") + 
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=18, hjust=0.5)) +
  theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) + 
  theme(legend.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) +
  theme(legend.background = element_rect(color = "#efefef")) +
  theme(plot.caption = element_text(family = "Trebuchet MS", color="#666666", size=10, hjust = 0.5, margin = margin(15, 0, 15, 0))) +
  theme(axis.text = element_text(family = "Trebuchet MS", color="#aaaaaa", face="bold", size=10)) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.major = element_line(color = "#efefef")) +
  theme(axis.ticks = element_line(color = "#efefef"))

ggplotly(plot)


# Top 10 words by Date

decadeToSearch <- 1850

titleWords %>% 
  filter(startDecade >= 1840 & startDecade < 1900) %>%
  group_by(startDecade) %>%
  top_n(5) %>%
  ggplot() + 
  geom_col(aes(x=word, y=percentage, fill=word)) + 
  coord_flip() +
  facet_wrap(~ startDecade, ncol=2) 

extantTitleWords %>% 
  filter(year >= decadeToSearch & year < decadeToSearch + 9) %>%
  top_n(5) %>%
  ggplot() + 
  geom_col(aes(x=word, y=percentage, fill=word)) + 
  coord_flip()
