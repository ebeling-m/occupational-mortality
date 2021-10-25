# Data preparation
# Note that this code only works within the secured network for the 
# analysis of register data 

mapvalues <- function (x, from, to, warn_missing = TRUE) 
{
  if (length(from) != length(to)) {
    stop("`from` and `to` vectors are not the same length.")
  }
  if (!is.atomic(x)) {
    stop("`x` must be an atomic vector.")
  }
  if (is.factor(x)) {
    levels(x) <- mapvalues(levels(x), from, to, warn_missing)
    return(x)
  }
  mapidx <- match(x, from)
  mapidxNA <- is.na(mapidx)
  from_found <- sort(unique(mapidx))
  if (warn_missing && length(from_found) != length(from)) {
    message("The following `from` values were not present in `x`: ", 
            paste(from[!(1:length(from) %in% from_found)], collapse = ", "))
  }
  x[!mapidxNA] <- to[mapidx[!mapidxNA]]
  x
}

library(dplyr)
library(lubridate)

# Load data
setwd("I:/EPI/Data/MarcusEbeling/occupationProject/data")

dat1 <- read.csv("StudPop_MarcusMaxPlanck.csv", header = TRUE, sep = ",",
                 stringsAsFactors = FALSE, na.strings = '', colClasses = "character")
dat2 <- read.csv("StudPopUpdate20210609.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, na.strings = '', 
                 colClasses = "character")

# Mark all cases that are in dat2
dat1$risk17 <- ifelse(dat1$LopNr %in% dat2$LopNr, 1, 0)
head(dat1)

# Replace death date, last date and event from dat2 with those in dat1
datTot <- 
  dat2 %>% 
  select(LopNr, DODSDATn, lastDate, Event) %>% 
  right_join(dat1, by = "LopNr") %>% 
  mutate(deathDate = if_else(risk17 == 1, DODSDATn.x, DODSDATn.y), 
         Event = if_else(risk17 == 1, Event.x, Event.y), 
         lastDate = if_else(risk17 == 1, lastDate.x, lastDate.y)) %>% 
  select(LopNr, Kon, deathDate, Event, lastDate, firstDate, Yrke80FoB1985, FODDATn) %>% 
  as_tibble() %>% 
  mutate(BirthCohort = as.numeric(substr(FODDATn, 1, 4))) %>% 
  filter(BirthCohort > 1924 & BirthCohort < 1940 & !Event %in% c(30, 40))

recodeAge <- cbind(1925:1939, rep(c(61, 56, 51), each = 5))
datTot$stAge <- mapvalues(datTot$BirthCohort, from=recodeAge[,1], to=recodeAge[,2])


datTot <- 
  datTot %>% 
  mutate(lifespan = interval(start = as.Date(FODDATn), end =  as.Date(lastDate)) /
           duration(num = 1, units = "years"),
         dateAge = paste(BirthCohort+stAge, substr(FODDATn, 5, 10), sep =""))

# Paste Start Date in 1986
datTot1 <- 
  datTot %>% 
  filter(as.Date(lastDate) > as.Date(dateAge))

# Test Estimation Cohort 1930
# fitDat <- 
#   datTot1 %>% 
#   filter(BirthCohort == 1930 & Kon == 1)


pylong <- function(x){
  start <- as.numeric(x["stAge"])
  end <- as.integer(x["lifespan"])
  lifespan <- as.numeric(x["lifespan"])
  Event <- as.numeric(x["Event"])
  ages <- seq(start, end, by = 1)
  
  if(length(ages) > 1 & Event == 20){
    py <- c(rep(1, length(ages)-1), lifespan-end)
  }
  
  if(length(ages) > 1 & Event == 50){
    py <- c(rep(1, length(ages)))
  }
  
  if(length(ages) == 1 & Event == 20){
    py <- lifespan-end
  }
  
  n <- length(ages)
  out <- data.frame(LopNr = rep(x["LopNr"], n),
                    Sex = rep(x["Kon"], n), 
                    Occu = rep(x["Yrke80FoB1985"], n), 
                    startAge = rep(x["stAge"], n),
                    Event = rep(x["Event"], n),
                    PY = py, 
                    birthCohort = rep(x["BirthCohort"], n),
                    lifespan = rep(lifespan, n),
                    Age = ages)
  return(out)
}

# Make Long data PY
pyLong <-  
  apply(datTot1, MARGIN = 1, FUN = pylong) %>% 
  bind_rows() %>% as_tibble()

# Aggregate Person Years Total
pyLongT <- 
  pyLong %>%
  group_by(Sex, Age, startAge) %>% 
  summarise(PYtot = sum(PY)) %>% 
  mutate(startAge = as.numeric(startAge))


# Aggregate Deaths Total and merge
dAgg <- 
  datTot1 %>% 
  filter(Event == 20) %>% 
  mutate(AgeD = as.integer(lifespan), 
         Nr = 1) %>% 
  group_by(Kon, AgeD, stAge) %>%
  summarise(Dtot = sum(Nr)) %>% 
  rename(Sex = Kon, Age = AgeD, startAge = stAge) %>% 
  right_join(pyLongT) %>% 
  mutate(mx = Dtot/PYtot)

# Make aggregation for occupations
digitRecode <- cbind(c(0:8, 90:94, 98, 99), c(0:4, 99, 6, 7, 7, 90, rep(91, 4), 99, 99))

pyOcc <- 
  pyLong %>%
  filter(!is.na(Occu)) %>% 
  mutate(oneDigit = case_when(substr(Occu, 1, 1) == 9 ~ substr(Occu, 1, 2),
                              !substr(Occu, 1, 1) == 9 ~ substr(Occu, 1, 1))) %>% 
  mutate(digRe = as.numeric(mapvalues(oneDigit, from = digitRecode[,1], to = digitRecode[,2]))) %>% 
  mutate(digRe = if_else(Sex == 2 & digRe == 90, 99, digRe)) %>% 
  group_by(Sex, Age, startAge, digRe) %>% 
  summarise(PYtot = sum(PY)) %>% 
  mutate(startAge = as.numeric(startAge))

dOcc <- 
  datTot1 %>%
  rename(Occu = Yrke80FoB1985, Sex = Kon) %>% 
  filter(Event == 20 & !is.na(Occu)) %>%
  mutate(oneDigit = case_when(substr(Occu, 1, 1) == 9 ~ substr(Occu, 1, 2),
                              !substr(Occu, 1, 1) == 9 ~ substr(Occu, 1, 1))) %>% 
  mutate(digRe = as.numeric(mapvalues(oneDigit, from = digitRecode[,1], to = digitRecode[,2]))) %>% 
  mutate(digRe = if_else(Sex == 2 & digRe == 90, 99, digRe)) %>%
  mutate(AgeD = as.integer(lifespan), 
         Nr = 1) %>% 
  group_by(Sex, AgeD, stAge, digRe) %>%
  summarise(Dtot = sum(Nr)) %>% 
  rename(Age = AgeD, startAge = stAge) %>% 
  right_join(pyOcc) %>% 
  mutate(mx = Dtot/PYtot)

  
write.table(dAgg, file = "totalAgg_20210621.txt", row.names = FALSE)
write.table(dOcc, file = "occupationAgg_20210621.txt", row.names = FALSE)

