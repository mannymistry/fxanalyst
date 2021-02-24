# Load Libraries
library(riingo)
library(tidyverse)
library(tidyquant)
library(tidytext)
library(BIS)
library(zoo)

# Function to get all news
get_all_news <- function(tags, days_back){
  PRE_ALLOC_MAX <- 30
  results <- vector(mode = "list", length = PRE_ALLOC_MAX)
  i <- 1
  from <- 0
  repeat {
    res <-riingo::riingo_news(tags=tags, start_date=Sys.Date()-days_back,
                              end_date=Sys.Date(), limit=1000, offset = from)
    # res <- cbind(res, tags)
    results <- rbind(res, results)
    if (nrow(res) == 1000) {
      message("Fetching next 1000 records...")
      i <- i + 1
      from <-  from + 1000
    } else {
      break
    }
  }
  results
}

# Get your data

# Most trade currencies last 180 day prices by 
fx_tickers_traded <- c("eurusd", "usdjpy", "usdchf", "gbpusd")
start <- Sys.Date() - 180
fx_prices <- riingo_fx_prices(fx_tickers_traded, start, resample_frequency = "2hour")

# Read in the cash balances
cash_balances <- read_csv("cashbalances.csv")

# Currency Tags 
tags <- c("usd", "eur", "jpy", "gbp", "chf")

# Read in News data about each currency over the last 180 days
news_eurusd <- get_all_news(tags = "eurusd", 365)
news_usdjpy <- get_all_news(tags = "usdjpy", 365)
news_usdchf <- get_all_news(tags = "usdchf", 365)
news_gbpusd <- get_all_news(tags = "gbpusd", 365)

# Sentiment for the last 30, 60, 90 days
generate_fx_tokens <- function(news_df, to_date){
  news_df %>% 
  unnest_tokens(word, description) %>% 
  anti_join(stop_words) %>% 
  select(publishedDate, id, word) %>%
  filter(publishedDate > to_date) %>%
  count(word) %>%
  inner_join(get_sentiments("loughran"), by = "word") %>%
  group_by(sentiment)
}


# Create the tokens 30,60,90
eurusdtokens90 <- generate_fx_tokens(news_eurusd, to_date = Sys.Date()-90) %>% 
  mutate(fx = "eurusd")
usdjpytokens90 <- generate_fx_tokens(news_usdjpy, to_date = Sys.Date()-90) %>% 
  mutate(fx = "usdjpy")
usdchftokens90 <- generate_fx_tokens(news_usdchf, to_date = Sys.Date()-90) %>% 
  mutate(fx = "usdchf")
gbpusdtokens90 <- generate_fx_tokens(news_gbpusd, to_date = Sys.Date()-90) %>% 
  mutate(fx = "gbpusd")

eurusdtokens60 <- generate_fx_tokens(news_eurusd, to_date = Sys.Date()-60) %>% 
  mutate(fx = "eurusd")
usdjpytokens60 <- generate_fx_tokens(news_usdjpy, to_date = Sys.Date()-60) %>% 
  mutate(fx = "usdjpy")
usdchftokens60 <- generate_fx_tokens(news_usdchf, to_date = Sys.Date()-60) %>% 
  mutate(fx = "usdchf")
gbpusdtokens60 <- generate_fx_tokens(news_gbpusd, to_date = Sys.Date()-60) %>% 
  mutate(fx = "gbpusd")

eurusdtokens30 <- generate_fx_tokens(news_eurusd, to_date = Sys.Date()-30) %>% 
  mutate(fx = "eurusd")
usdjpytokens30 <- generate_fx_tokens(news_usdjpy, to_date = Sys.Date()-30) %>% 
  mutate(fx = "usdjpy")
usdchftokens30 <- generate_fx_tokens(news_usdchf, to_date = Sys.Date()-30) %>% 
  mutate(fx = "usdchf")
gbpusdtokens30 <- generate_fx_tokens(news_gbpusd, to_date = Sys.Date()-30) %>% 
  mutate(fx = "gbpusd")

# Combine all the Tables
fx_tokens90 <- rbind(eurusdtokens90, usdjpytokens90, usdchftokens90, gbpusdtokens90)
fx_tokens60 <- rbind(eurusdtokens60, usdjpytokens60, usdchftokens60, gbpusdtokens60)
fx_tokens30 <- rbind(eurusdtokens30, usdjpytokens30, usdchftokens30, gbpusdtokens30)


# Display the 30 60 and 90 Positvity Scores for each pair
# FX Sentiment Count
fx_sentiment_count <- fx_tokens90 %>%
  count(sentiment, fx) %>%
  spread(sentiment, n, fill = 0)

positivity_score <- fx_sentiment_count %>%
  mutate(score = (positive - negative) / (positive + negative)) %>%
  mutate(fx = reorder(fx, score))

positivity_score %>% 
  ggplot(aes(score, fx, fill = score > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = " Positivity score among news articles spanning 90 days", y = NULL)

# Get monthly policy rates
bis_datasets <- get_datasets()
dailyrates_all <- get_bis(bis_datasets$url[24], quiet = TRUE)


dailyrates <- dailyrates_all %>% 
  mutate(date = as.Date(as.yearmon(date))) %>% 
  filter(reference_area %in% c("CH:Switzerland", "GB:United Kingdom", 
                              "US:United States", "XM:Euro area", "JP:Japan"))

dailyrates %>% 
  group_by(reference_area) %>%
  # tq_transmute(select = obs_value, 
  #              mutate_fun = periodReturn,
  #              period = "daily", 
  #              col_rename = "Ra") %>% 
  ggplot(aes(date, obs_value, color = reference_area))+
  geom_line(show.legend = TRUE) +
  facet_wrap(~reference_area) +
  theme_light() +
  theme(panel.grid = element_blank()) +
  labs(x = NULL, y = NULL,
       title = "Daily Return FX Pairs")

  
# Downward Spiral in Central Bank Policy Rates for the last 50 years
# So what does this mean?
ggplot(dailyrates, aes(date, obs_value, color = reference_area)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey70", size = 0.02) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~reference_area) +
  theme_light() +
  theme(panel.grid = element_blank()) +
  labs(x = NULL, y = NULL,
       title = "Central bank policy rates")


# Some graphs 
fx_prices %>% 
  group_by(ticker) %>% 
  tq_transmute(select = open, 
               mutate_fun = periodReturn,
               period = "daily", 
               col_rename = "Ra") %>% 
  ggplot(aes(date, Ra, color = ticker))+
  geom_line(show.legend = FALSE) +
  facet_wrap(~ticker) +
  theme_light() +
  theme(panel.grid = element_blank()) +
  labs(x = NULL, y = NULL,
       title = "Daily Return FX Pairs")+
  theme_tq()



fx_prices %>% 
  filter(ticker == "eurusd") %>% 
  ggplot(aes(x = date, y = open)) +
  geom_line(color = palette_light()[[1]]) + 
  scale_y_log10() +
  geom_smooth(method = "lm") +
  labs(title = "EURUSD", 
       subtitle = "EUR", 
       y = "Open Price", x = "") + 
  theme_tq()

fx_prices %>% 
  filter(ticker == "gbpusd") %>% 
  ggplot(aes(x = date, y = open)) +
  geom_line(color = palette_light()[[1]]) + 
  # scale_y_log10() +
  geom_smooth(method = "lm") +
  labs(title = "GBPUSD", 
       subtitle = "Log Scale, Applying Linear Trendline", 
       y = "Adjusted Closing Price", x = "") + 
  theme_tq()

fx_prices %>% 
  filter(ticker == "usdjpy") %>% 
  ggplot(aes(x = date, y = open)) +
  geom_line(color = palette_light()[[1]]) + 
  # scale_y_log10() +
  geom_smooth(method = "lm") +
  labs(title = "JPYUSD", 
       subtitle = "Log Scale, Applying Linear Trendline", 
       y = "Adjusted Closing Price", x = "") + 
  theme_tq()

fx_prices %>% 
  filter(ticker == "usdchf") %>% 
  ggplot(aes(x = date, y = open)) +
  geom_line(color = palette_light()[[1]]) + 
  # scale_y_log10() +
  geom_smooth(method = "lm") +
  labs(title = "USDCHF", 
       subtitle = "Log Scale, Applying Linear Trendline", 
       y = "Adjusted Closing Price", x = "") + 
  theme_tq()



