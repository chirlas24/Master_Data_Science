library(tidyverse)
library(dslabs)
library(lubridate)

############## polls_us_election_2016: Clinton vs Trump

data("polls_us_election_2016")
polls_us_election_2016$startdate %>% head

class(polls_us_election_2016$startdate)

as.numeric(polls_us_election_2016$startdate) %>% head

as.Date("1970-01-01") %>% as.numeric

polls_us_election_2016 %>% filter(pollster == "Ipsos" & state =="U.S.") %>%
  ggplot(aes(startdate, rawpoll_trump)) +
  geom_line()

############## The lubridate package {#lubridate}

set.seed(2002)
dates <- sample(polls_us_election_2016$startdate, 10) %>% sort
dates

data_frame(date = dates, 
           month = month(dates),
           day = day(dates),
           year = year(dates))

month(dates, label = TRUE)

x <- c(20090101, "2009-01-02", "2009 01 03", "2009-1-4",
       "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")
ymd(x)

x <- "09/01/02"

ymd(x)

mdy(x)

ydm(x)
myd(x)
dmy(x)
dym(x)

make_date(2019, 8, 3)

make_date(1980:1989)

polls_us_election_2016 %>% 
  mutate(week = round_date(startdate, "week")) %>%
  group_by(week) %>%
  summarize(margin = mean(rawpoll_clinton - rawpoll_trump)) %>%
  qplot(week, margin, data = .)

