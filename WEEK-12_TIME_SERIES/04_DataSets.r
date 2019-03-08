

###############################################################################
# Dataset1: Edad de los reyes de UK desde William 
###############################################################################
kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kingstimeseries <- ts(kings)
head(kings)
kingstimeseries
ts.plot(kingstimeseries)

###############################################################################
# Dataset2: Número de nacimientos por mes en NY 
###############################################################################
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
birthstimeseries
ts.plot(birthstimeseries)

###############################################################################
# Dataset3: tienda de souvenirs
###############################################################################
souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))
souvenirtimeseries
ts.plot(souvenirtimeseries)

################################################################################
# Dataset4:
################################################################################
data("polls_us_election_2016")
is.ts(polls_us_election_2016)
polls_Clinton_ts<-polls_us_election_2016 %>% 
  filter(pollster == "Ipsos" & state =="U.S.") %>% 
  select(startdate,rawpoll_clinton) %>%
  mutate(rawpoll=rawpoll_clinton) %>% select(startdate,rawpoll)

polls_trump_ts<-polls_us_election_2016 %>% 
  filter(pollster == "Ipsos" & state =="U.S.") %>% 
  select(startdate,rawpoll_trump)%>%
  mutate(rawpoll=rawpoll_trump) %>% select(startdate,rawpoll)


polls_both<-bind_rows(polls_Clinton_ts,polls_trump_ts) %>%
  mutate(name=c(rep("clinton",length(polls_Clinton_ts$rawpoll)),
                rep("trump",length(polls_trump_ts$rawpoll))))

ggplot(polls_both,aes(startdate, rawpoll,group=name)) +geom_line()
ggplot(polls_both,aes(startdate, rawpoll,color=name)) +geom_line()

ggplot(data = polls_both, mapping = aes(startdate, rawpoll)) +
  geom_line() +
  facet_wrap(~ name)

ymd(polls_both$startdate)-min(ymd(polls_both$startdate))
# no es una frecuencia regular, por lo tanto no podemos convertirlo en una serie temporal
# el objeto ts() precisa de: datos, fecha de inicio y frecuencia. 
# Nos imaginamos que sí que se han medido cada dia para poder trabajar con ello:

polls_clinton_ts=ts(polls_both$rawpoll[which(polls_both$name=="clinton")], frequency = 1, 
                    start=1)
ts.plot(polls_clinton_ts)
polls_trump_ts=ts(polls_both$rawpoll[which(polls_both$name=="trump")], start = 1, frequency = 1)
ts.plot(polls_trump_ts)



