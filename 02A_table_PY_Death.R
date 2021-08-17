# Summary table with person year & deaths for supplemental materials
source("00_packages_Fct.R")

# Load data
occDat <- read.table("occupationAgg_20210621.txt", 
                     header = TRUE, colClasses = "character") %>% as_tibble()


occDat$mx %<>% as.numeric()
occDat$Age %<>% as.numeric()
occDat$Dtot %<>% as.numeric()
occDat$PYtot %<>% as.numeric()

# Reduce to age range for each group: 
#   startAge = 51 (51-81)
#   startAge = 56 (56-86)
#   startAge = 61 (61-91)

occDat1 <- 
  occDat %>% 
  filter(case_when(startAge == 51 ~ Age %in% c(51:81),
                   startAge == 56 ~ Age %in% c(56:86),
                   startAge == 61 ~ Age %in% c(61:91)), 
         !digRe == 99)

# Sum death and person years over age by occupation and sex
sumTab <- 
  occDat1 %>% 
  group_by(startAge, digRe, Sex) %>% 
  summarize(Dx = sum(Dtot), PY = round(sum(PYtot),1))

# Males
cbind(sumTab %>% filter(Sex == 1 & startAge == 61) %>% ungroup() %>% select(Dx, PY),
sumTab %>% filter(Sex == 1 & startAge == 56) %>% ungroup() %>% select(Dx, PY),
sumTab %>% filter(Sex == 1 & startAge == 51) %>% ungroup() %>% select(Dx, PY))

# females
cbind(sumTab %>% filter(Sex == 2 & startAge == 61) %>% ungroup() %>% select(Dx, PY),
      sumTab %>% filter(Sex == 2 & startAge == 56) %>% ungroup() %>% select(Dx, PY),
      sumTab %>% filter(Sex == 2 & startAge == 51) %>% ungroup() %>% select(Dx, PY))
