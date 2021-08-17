# Differences in age specific death rates 
source("00_packages_Fct.R")

# Load data
occDat <- read.table("occupationAgg_20210621.txt", 
                     header = TRUE, colClasses = "character") %>% as_tibble()

occDat$mx %<>% as.numeric()
occDat$Age %<>% as.numeric()
occDat$Dtot %<>% as.numeric()
occDat$PYtot %<>% as.numeric()
occDat$startAge %<>% as.numeric()


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


# Smooth death rates
smoothMx <- function(Sex, Age, startAge, digRe, Dtot, PYtot, mx){
  fitMod <- gam(Dtot ~ s(Age, bs="ps") + offset(log(PYtot)), 
                family = poisson, method = "GCV.Cp")
  sMx  <- exp(predict(fitMod, newdata = data.frame(Age=Age, PYtot = PYtot)))/PYtot 
  
  out <- data.frame(Sex = Sex, Age = Age, startAge = startAge, 
                    digRe = digRe, Dtot = Dtot, PYtot = PYtot, mx = mx, 
                    sMx = sMx)
  return(out)
}

occDat1 <- 
  occDat1 %>%
  group_by(Sex, startAge, digRe) %>% 
  group_map(~smoothMx(Sex = .$Sex, Age = .$Age, startAge = .$startAge,
                      digRe = .$digRe, Dtot = .$Dtot, PYtot = .$PYtot,
                      mx = .$mx), .keep = TRUE) %>% 
  bind_rows() %>% as_tibble()

# Calculate partial life expectancy 
pLE <- 
  occDat1 %>% 
  group_by(startAge, digRe, Sex) %>% 
  group_map(~partialLe(mx=.$sMx, 
                       occ=.$digRe,
                       sex=.$Sex,
                       age=.$Age,
                       startAge=.$startAge), .keep = TRUE) %>% 
  bind_rows() %>% as_tibble()

# Function to calculate age specific differences 
distRange <- function(digRe, Sex, Age, startAge, value){
  distMinMax <- max(value)-min(value)
  distMin <- value-min(value)
  distRel <- (value-min(value))/(max(value)-min(value))
  out <- data.frame(digRe = digRe, 
                    Sex = Sex, 
                    startAge = startAge, 
                    Age = Age,
                    minLE = rep(min(value), length(digRe)),
                    maxLE = rep(max(value), length(digRe)),
                    MinMax = rep(max(value)/min(value), length(digRe)),
                    distMin = distMin,
                    distRel = distRel,
                    pE = value)
  return(out)
}

distDat <- 
  pLE %>% 
  group_by(sex, startAge, age) %>% 
  group_map(~distRange(Sex = .$sex, Age = .$age, startAge = .$startAge,
                      digRe = .$occ,
                      value = .$pE), .keep = TRUE) %>% 
    bind_rows() %>% as_tibble()

colOcc <- c('#88CCEE', '#44AA99', '#117733', '#332288', '#DDCC77', '#999933','#CC6677', '#882255', '#AA4499')
distDat$colOcc <- mapvalues(distDat$digRe, from = c(0:4, 6, 7, 90, 91), to = colOcc)


occLab <- c("Professional & technical work",
            "Health, nursing & social work",
            "Administrative & clerical work",
            "Sales work",
            "Agricultural, forestry & fishing work", 
            "Transport & communications work",
            "Production work",
            "Civilian protective service work",
            "Catering, caretaking & care work")

distDat$occLab <- mapvalues(distDat$digRe, from = c(0:4, 6, 7, 90, 91), to = occLab)


# Cut at five years prior end 

distDat1 <-
  distDat %>% 
  filter(case_when(startAge == 51 ~ Age %in% seq(51, 76, by = 5),
                   startAge == 56 ~ Age %in% seq(56, 81, by = 5),
                   startAge == 61 ~ Age %in% seq(61, 86, by = 5))) %>% 
  mutate(maxYears = (startAge+30)-Age+1)

# Plot results

pdf("dist_PLE_1925_29.pdf", family = "Times", width = 12, height = 9)
layout(rbind(1,2))
par(xpd = TRUE, mar = c(3,3,2,0), oma =c(2,2,1,0))

# Males
plot(x=0:7, y=0:7, xaxt = "n", yaxt = "n", xaxs = "i", 
     yaxs = "i", bty = "n", typ ="n", ylim = c(0, 1), 
     xlab = NA, ylab = NA)

segments(x0=0, y0= seq(0, 1, by = 0.2), x1=5, y1 =seq(0, 1, by = 0.2),
         col = "gray", lwd = 1)
segments(x0=0, y0= seq(0.1, 0.9, by = 0.2), x1=5, y1 =seq(0.1, 0.9, by = 0.2),
         col = "gray", lwd = .5, lty =2)
for(i in 1:5){
  segments(x0=i, y0= 0, x1=i, y1 = 1, col = "black", lwd = 2)
}
par(las = 1)
axis(2, at = seq(0, 1, by = 0.2), 
     labels = c("0%", "20%", "40%", "60%", "80%", "100%"), lwd = 3)
axis(2, at = seq(0.1, 0.9, by = 0.2), labels = FALSE, lwd = 1)
par(las = 0)

# par(las = 2)
# axis(1, at = 0:5, labels = paste(seq(61, 86, by = 5), "-91", sep = ""), 
#      pos = -0.04, lwd = 0, srt =90)
# par(las = 0)

for(j in unique(distDat1$digRe)){
plotDat <- 
  distDat1 %>% 
  filter(Sex == 1 & startAge == 61 & digRe == j)
lines(x=0:5, y=plotDat$distRel, lwd = 3, 
      col =plotDat$colOcc[1])
lines(x=0:5, y=plotDat$distRel, typ = "p", pch = 16, 
      col =plotDat$colOcc[1], cex = 2)
text(x=5, y=plotDat$distRel[6], plotDat$occLab[1], 
     col = plotDat$colOcc[1], pos = 4)
if(j == 0){
  axis(1, at = 0:5, labels = round(plotDat$minLE,2),lwd = 0, pos = 0.04)
  axis(1, at = 0:5, labels = round(plotDat$MinMax,2),lwd = 0, pos = -0.02, cex = 0.2)
  axis(1, at = 0:5, labels = round(plotDat$maxLE,2),lwd = 0, pos = 1.15)
  axis(1, at = 0.28, labels = "(Min)",lwd = 0, pos = 0.04)
  axis(1, at = 0.35, labels = "(Max/Min)",lwd = 0, pos = -0.02, cex = 0.2)
  axis(1, at = 0.28, labels = "(Max)",lwd = 0, pos = 1.15)}
}
mtext("Males", 3, adj = 0, line = 1.5, cex = 1.6)
# Females
plot(x=0:7, y=0:7, xaxt = "n", yaxt = "n", xaxs = "i", 
     yaxs = "i", bty = "n", typ ="n", ylim = c(0, 1), 
     xlab = NA, ylab = NA)


segments(x0=0, y0= seq(0, 1, by = 0.2), x1=5, y1 =seq(0, 1, by = 0.2),
         col = "gray", lwd = 1)
segments(x0=0, y0= seq(0.1, 0.9, by = 0.2), x1=5, y1 =seq(0.1, 0.9, by = 0.2),
         col = "gray", lwd = .5, lty =2)
for(i in 1:5){
  segments(x0=i, y0= 0, x1=i, y1 = 1, col = "black", lwd = 2)
}
par(las = 1)
axis(2, at = seq(0, 1, by = 0.2), 
     labels = c("0%", "20%", "40%", "60%", "80%", "100%"), lwd = 3)
axis(2, at = seq(0.1, 0.9, by = 0.2), labels = FALSE, lwd = 1)
par(las = 0)

# par(las = 2)
# axis(1, at = 0:5, labels = paste(seq(61, 86, by = 5), "-91", sep = ""), 
#      pos = -0.04, lwd = 0, srt =90)
# par(las = 0)

for(j in c(0:4, 6, 7, 91)){
  plotDat <- 
    distDat1 %>% 
    filter(Sex == 2 & startAge == 61 & digRe == j)
  lines(x=0:5, y=plotDat$distRel, lwd = 3, 
        col =plotDat$colOcc[1])
  lines(x=0:5, y=plotDat$distRel, typ = "p", pch = 16, 
        col =plotDat$colOcc[1], cex = 2)
  text(x=5, y=plotDat$distRel[6], plotDat$occLab[1], 
       col = plotDat$colOcc[1], pos = 4)
  if(j == 0){axis(1, at = 0:5, labels = round(plotDat$minLE,2),lwd = 0, pos = 0.04)
    axis(1, at = 0:5, labels = round(plotDat$maxLE,2),lwd = 0, pos = 1.15)
    axis(1, at = 0:5, labels = round(plotDat$MinMax,2),lwd = 0, pos = -0.02, cex = 0.2)
  }
}

axis(1, at = seq(0,5, by = 1), labels = c("Ages 61-91", "Ages 66-91", "Ages 71-91", "Ages 76-91", "Ages 81-91", "Ages 86-91"),
     lwd =0, pos = -0.08, font =2)

mtext("Females", 3, adj = 0, line = 1.5, cex = 1.4)
mtext("Proportion of range between minimum and maximum value (in %)", 2, adj = -0.25, line = 3.5, cex = 1.4)

dev.off()
 
# Cohort 1930 - 34
pdf("dist_PLE_1930_34.pdf", family = "Times", width = 12, height = 9)
layout(rbind(1,2))
par(xpd = TRUE, mar = c(3,3,2,0), oma =c(2,2,1,0))

# Males
plot(x=0:7, y=0:7, xaxt = "n", yaxt = "n", xaxs = "i", 
     yaxs = "i", bty = "n", typ ="n", ylim = c(0, 1), 
     xlab = NA, ylab = NA)

segments(x0=0, y0= seq(0, 1, by = 0.2), x1=5, y1 =seq(0, 1, by = 0.2),
         col = "gray", lwd = 1)
segments(x0=0, y0= seq(0.1, 0.9, by = 0.2), x1=5, y1 =seq(0.1, 0.9, by = 0.2),
         col = "gray", lwd = .5, lty =2)
for(i in 1:5){
  segments(x0=i, y0= 0, x1=i, y1 = 1, col = "black", lwd = 2)
}
par(las = 1)
axis(2, at = seq(0, 1, by = 0.2), 
     labels = c("0%", "20%", "40%", "60%", "80%", "100%"), lwd = 3)
axis(2, at = seq(0.1, 0.9, by = 0.2), labels = FALSE, lwd = 1)
par(las = 0)

# par(las = 2)
# axis(1, at = 0:5, labels = paste(seq(61, 86, by = 5), "-91", sep = ""), 
#      pos = -0.04, lwd = 0, srt =90)
# par(las = 0)

for(j in unique(distDat1$digRe)){
  plotDat <- 
    distDat1 %>% 
    filter(Sex == 1 & startAge == 56 & digRe == j)
  lines(x=0:5, y=plotDat$distRel, lwd = 3, 
        col =plotDat$colOcc[1])
  lines(x=0:5, y=plotDat$distRel, typ = "p", pch = 16, 
        col =plotDat$colOcc[1], cex = 2)
  if(j %in% 91){text(x=5, y=plotDat$distRel[6]+0.03, plotDat$occLab[1], 
                     col = plotDat$colOcc[1], pos = 4)}else{
    text(x=5, y=plotDat$distRel[6], plotDat$occLab[1], 
         col = plotDat$colOcc[1], pos = 4)}
  if(j == 0){
    axis(1, at = 0:5, labels = round(plotDat$minLE,2),lwd = 0, pos = 0.04)
    axis(1, at = 0:5, labels = round(plotDat$MinMax,2),lwd = 0, pos = -0.02, cex = 0.2)
    axis(1, at = 0:5, labels = round(plotDat$maxLE,2),lwd = 0, pos = 1.15)
    axis(1, at = 0.28, labels = "(Min)",lwd = 0, pos = 0.04)
    axis(1, at = 0.35, labels = "(Max/Min)",lwd = 0, pos = -0.02, cex = 0.2)
    axis(1, at = 0.28, labels = "(Max)",lwd = 0, pos = 1.15)}
}
mtext("Males", 3, adj = 0, line = 1.5, cex = 1.6)
# Females
plot(x=0:7, y=0:7, xaxt = "n", yaxt = "n", xaxs = "i", 
     yaxs = "i", bty = "n", typ ="n", ylim = c(0, 1), 
     xlab = NA, ylab = NA)

segments(x0=0, y0= seq(0, 1, by = 0.2), x1=5, y1 =seq(0, 1, by = 0.2),
         col = "gray", lwd = 1)
segments(x0=0, y0= seq(0.1, 0.9, by = 0.2), x1=5, y1 =seq(0.1, 0.9, by = 0.2),
         col = "gray", lwd = .5, lty =2)
for(i in 1:5){
  segments(x0=i, y0= 0, x1=i, y1 = 1, col = "black", lwd = 2)
}
par(las = 1)
axis(2, at = seq(0, 1, by = 0.2), 
     labels = c("0%", "20%", "40%", "60%", "80%", "100%"), lwd = 3)
axis(2, at = seq(0.1, 0.9, by = 0.2), labels = FALSE, lwd = 1)
par(las = 0)

# par(las = 2)
# axis(1, at = 0:5, labels = paste(seq(61, 86, by = 5), "-91", sep = ""), 
#      pos = -0.04, lwd = 0, srt =90)
# par(las = 0)

for(j in c(0:4, 6, 7, 91)){
  plotDat <- 
    distDat1 %>% 
    filter(Sex == 2 & startAge == 56 & digRe == j)
  lines(x=0:5, y=plotDat$distRel, lwd = 3, 
        col =plotDat$colOcc[1])
  lines(x=0:5, y=plotDat$distRel, typ = "p", pch = 16, 
        col =plotDat$colOcc[1], cex = 2)
  if(j %in% 2){text(x=5, y=plotDat$distRel[6]+0.03, plotDat$occLab[1], 
                     col = plotDat$colOcc[1], pos = 4)}else{
                       text(x=5, y=plotDat$distRel[6], plotDat$occLab[1], 
                            col = plotDat$colOcc[1], pos = 4)}
  if(j == 0){
    axis(1, at = 0:5, labels = round(plotDat$minLE,2),lwd = 0, pos = 0.04)
    axis(1, at = 0:5, labels = round(plotDat$maxLE,2),lwd = 0, pos = 1.15)    
    axis(1, at = 0:5, labels = round(plotDat$MinMax,2),lwd = 0, pos = -0.02, cex = 0.2)
    }}


axis(1, at = seq(0,5, by = 1), labels = c("Ages 56-86", "Ages 61-86", "Ages 66-86", "Ages 71-86", "Ages 76-86", "Ages 81-86"),
     lwd =0, pos = -0.08, font =2)

mtext("Females", 3, adj = 0, line = 1.5, cex = 1.4)
mtext("Proportion of range between minimum and maximum value (in %)", 2, adj = -0.25, line = 3.5, cex = 1.4)

dev.off()


# Cohort 1935 - 39
pdf("dist_PLE_1935_39.pdf", family = "Times", width = 12, height = 9)
layout(rbind(1,2))
par(xpd = TRUE, mar = c(3,3,2,0), oma =c(2,2,1,0))

# Males
plot(x=0:7, y=0:7, xaxt = "n", yaxt = "n", xaxs = "i", 
     yaxs = "i", bty = "n", typ ="n", ylim = c(0, 1), 
     xlab = NA, ylab = NA)

segments(x0=0, y0= seq(0, 1, by = 0.2), x1=5, y1 =seq(0, 1, by = 0.2),
         col = "gray", lwd = 1)
segments(x0=0, y0= seq(0.1, 0.9, by = 0.2), x1=5, y1 =seq(0.1, 0.9, by = 0.2),
         col = "gray", lwd = .5, lty =2)
for(i in 1:5){
  segments(x0=i, y0= 0, x1=i, y1 = 1, col = "black", lwd = 2)
}
par(las = 1)
axis(2, at = seq(0, 1, by = 0.2), 
     labels = c("0%", "20%", "40%", "60%", "80%", "100%"), lwd = 3)
axis(2, at = seq(0.1, 0.9, by = 0.2), labels = FALSE, lwd = 1)
par(las = 0)

# par(las = 2)
# axis(1, at = 0:5, labels = paste(seq(61, 86, by = 5), "-91", sep = ""), 
#      pos = -0.04, lwd = 0, srt =90)
# par(las = 0)

for(j in unique(distDat1$digRe)){
  plotDat <- 
    distDat1 %>% 
    filter(Sex == 1 & startAge == 51 & digRe == j)
  lines(x=0:5, y=plotDat$distRel, lwd = 3, 
        col =plotDat$colOcc[1])
  lines(x=0:5, y=plotDat$distRel, typ = "p", pch = 16, 
        col =plotDat$colOcc[1], cex = 2)
  if(j %in% 2){text(x=5, y=plotDat$distRel[6]+0.04, plotDat$occLab[1], 
                     col = plotDat$colOcc[1], pos = 4)}else{
                       text(x=5, y=plotDat$distRel[6], plotDat$occLab[1], 
                            col = plotDat$colOcc[1], pos = 4)}
  if(j == 0){
    axis(1, at = 0:5, labels = round(plotDat$minLE,2),lwd = 0, pos = 0.04)
    axis(1, at = 0:5, labels = round(plotDat$MinMax,2),lwd = 0, pos = -0.02, cex = 0.2)
    axis(1, at = 0:5, labels = round(plotDat$maxLE,2),lwd = 0, pos = 1.15)
    axis(1, at = 0.28, labels = "(Min)",lwd = 0, pos = 0.04)
    axis(1, at = 0.35, labels = "(Max/Min)",lwd = 0, pos = -0.02, cex = 0.2)
    axis(1, at = 0.28, labels = "(Max)",lwd = 0, pos = 1.15)}
}
mtext("Males", 3, adj = 0, line = 1.5, cex = 1.6)
# Females
plot(x=0:7, y=0:7, xaxt = "n", yaxt = "n", xaxs = "i", 
     yaxs = "i", bty = "n", typ ="n", ylim = c(0, 1), 
     xlab = NA, ylab = NA)

segments(x0=0, y0= seq(0, 1, by = 0.2), x1=5, y1 =seq(0, 1, by = 0.2),
         col = "gray", lwd = 1)
segments(x0=0, y0= seq(0.1, 0.9, by = 0.2), x1=5, y1 =seq(0.1, 0.9, by = 0.2),
         col = "gray", lwd = .5, lty =2)
for(i in 1:5){
  segments(x0=i, y0= 0, x1=i, y1 = 1, col = "black", lwd = 2)
}
par(las = 1)
axis(2, at = seq(0, 1, by = 0.2), 
     labels = c("0%", "20%", "40%", "60%", "80%", "100%"), lwd = 3)
axis(2, at = seq(0.1, 0.9, by = 0.2), labels = FALSE, lwd = 1)
par(las = 0)

# par(las = 2)
# axis(1, at = 0:5, labels = paste(seq(61, 86, by = 5), "-91", sep = ""), 
#      pos = -0.04, lwd = 0, srt =90)
# par(las = 0)

for(j in c(0:4, 6, 7, 91)){
  plotDat <- 
    distDat1 %>% 
    filter(Sex == 2 & startAge == 51 & digRe == j)
  lines(x=0:5, y=plotDat$distRel, lwd = 3, 
        col =plotDat$colOcc[1])
  lines(x=0:5, y=plotDat$distRel, typ = "p", pch = 16, 
        col =plotDat$colOcc[1], cex = 2)
  if(j %in% 100){text(x=5, y=plotDat$distRel[6]+0.03, plotDat$occLab[1], 
                    col = plotDat$colOcc[1], pos = 4)}else{
                      text(x=5, y=plotDat$distRel[6], plotDat$occLab[1], 
                           col = plotDat$colOcc[1], pos = 4)}
  if(j == 0){
    axis(1, at = 0:5, labels = round(plotDat$minLE,2),lwd = 0, pos = 0.04)
    axis(1, at = 0:5, labels = round(plotDat$maxLE,2),lwd = 0, pos = 1.15)
    axis(1, at = 0:5, labels = round(plotDat$MinMax,2),lwd = 0, pos = -0.02, cex = 0.2)}}

axis(1, at = seq(0,5, by = 1), labels = c("Ages 51-81", "Ages 56-81", "Ages 61-81", "Ages 66-81", "Ages 71-81", "Ages 76-81"),
     lwd =0, pos = -0.08, font =2)

mtext("Females", 3, adj = 0, line = 1.5, cex = 1.4)
mtext("Proportion of range between minimum and maximum value (in %)", 2, adj = -0.25, line = 3.5, cex = 1.4)

dev.off()
