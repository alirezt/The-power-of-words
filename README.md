# The-power-of-words
How our selection of words impacts other people's emotion and unconsciousness? (Machine learning in computational social science)

When we read a text or web page, we receive a particular feeling or provocation of a thought that is caused by the text's opinion and emotion. Using the text mining method, we can derive so-called **sentiments** of the text.

Here I use The gutenbergr package to illustrate the positive and negative sentiments of **The Brothers Karamazov**, the last novel by Russian author Fyodor Dostoevsky. As you can see from the graphs below, "**afraid**" and "**like**" were the most frequent negative and positive words which were used by Dostoevsky. 

![po](https://user-images.githubusercontent.com/89996099/141172971-23a48056-b256-4977-b35e-de444f40a184.jpeg)
![ne](https://user-images.githubusercontent.com/89996099/141172978-c2a16eff-ca10-4e49-b6c1-99cfbf81eb0b.jpeg)

Code I use in R:

#using gutenberg_download() and 
#the Project Gutenberg ID numbers for each novel from David Robinson

install.packages("gutenbergr")
install.packages("vroom")
install.packages(c("janeaustenr","stringr"))

library(gutenbergr)
library(dplyr)
library(vroom)
library(tidyr)
library(scales)
library(janeaustenr)
library(dplyr)
library(stringr)
library(hrbrthemes)

brkar <- gutenberg_works(title == "The Brothers Karamazov") %>%
  gutenberg_download()

tidy_brkar <- brkar %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords())
tidy_brkar

tidy_brkar %>%
  count(word, sort = TRUE) 

original_brkar <- brkar %>%
  mutate(line = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()
original_brkar
__________________________________

positive <- get_sentiments("bing") %>%
  filter(sentiment == "positive")

negative <- get_sentiments("bing") %>%
  filter(sentiment == "negative")

brkar_negative <-
  tidy_brkar%>%
  semi_join(negative) %>%
  count(word, sort = TRUE)

brkar_positive <-
  tidy_brkar%>%
  semi_join(positive) %>%
  count(word, sort = TRUE)

brkar_positive %>%
  filter(n > 100) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "#84b4b6", color = "#1d3131") +
  coord_flip() +
  labs(y = "Positive Sentiments")+
  ggtitle("The Brothers Karamazov Positive Sentiments") +
  theme_ipsum()

brkar_negative %>%
  filter(n > 100) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "#887881", color = "black") +
  coord_flip() +
  labs(y = "Negative Sentiments")+
  ggtitle("The Brothers Karamazov Negative Sentiments") +
  theme_ipsum()
