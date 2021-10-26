# Raw vs smooth death rates
setwd("C:/occupation/analysisJune2021/occupational-mortality")
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

plotDeathRates <- function(mx, smx, age, occ, startAge){
plot(x=51:91, y=51:91, typ = "n", xlab = "Age", ylab = "Log death rates", 
     xaxs = "i", yaxs = "i", log = "y", ylim = c(0.001, 0.23), xlim = c(50, 91), bty = "n", 
     main = occLab[paste(occ[1])])

  coh51 <- startAge == 51
  coh56 <- startAge == 56
  coh61 <- startAge == 61

  lines(x=age[coh51], y=mx[coh51], typ ="p", pch = 16)
  lines(x=age[coh51], y=smx[coh51], typ ="l", lwd = 2)
  
  lines(x=age[coh56], y=mx[coh56], typ ="p", pch = 16, col = "steelblue")
  lines(x=age[coh56], y=smx[coh56], typ ="l", lwd = 2, col = "steelblue")
  
  lines(x=age[coh61], y=mx[coh61], typ ="p", pch = 16, col = "tomato")
  lines(x=age[coh61], y=smx[coh61], typ ="l", lwd = 2, col = "tomato")
  
  legend("topleft", legend = c("1925-29", "1930-34", "1935-39"), col = c("tomato", "steelblue", "black"), pch = 16)
  
}

pdf("deathRates_raw_smooth_fem.pdf", family = "Times", height = 14, width = 14)
par(mfrow=c(3,3))
occDat1 %>% filter(Sex == 2) %>% 
  group_by(digRe) %>% 
  group_map(~plotDeathRates(mx= .$mx, smx = .$sMx, 
                            age = .$Age, occ = .$digRe,
                            startAge = .$startAge), .keep = TRUE)
dev.off()

pdf("deathRates_raw_smooth_male.pdf", family = "Times", height = 14, width = 14)
par(mfrow=c(3,3))
occDat1 %>% filter(Sex == 1) %>% 
  group_by(digRe) %>% 
  group_map(~plotDeathRates(mx= .$mx, smx = .$sMx, 
                            age = .$Age, occ = .$digRe,
                            startAge = .$startAge), .keep = TRUE)
dev.off()

# Create panel plot for age-specific death rates
# Assign colors

colOcc <- c('#88CCEE', '#44AA99', '#117733', '#332288', '#DDCC77', '#999933','#CC6677', '#882255', '#AA4499')
colOccT <- c('#CC88CCEE', '#CC44AA99', '#CC117733', '#CC332288', '#CCDDCC77', '#CC999933','#CCCC6677', '#CC882255', '#CCAA4499')
names(colOcc) <- c(0:4, 6, 7, 90, 91)
occDat1$colOcc <- mapvalues(occDat1$digRe, from = c(0:4, 6, 7, 90, 91), to = colOcc)
occDat1$colOccT <- mapvalues(occDat1$digRe, from = c(0:4, 6, 7, 90, 91), to = colOccT)


occLab <- c("Professional & technical work",
            "Health, nursing & social work",
            "Administrative & clerical work",
            "Sales work",
            "Agricultural, forestry & fishing work", 
            "Transport & communications work",
            "Production work",
            "Civilian protective service work",
            "Catering, caretaking & care work")
names(occLab) <- c(0:4, 6, 7, 90, 91) 


pdf("deathRates_Panel.pdf", family = "Times", width = 14, pointsize = 15, height =10)
naturePlot <- layout(matrix(1:6, byrow = FALSE, ncol = 3))
par(xpd = FALSE, mar = c(3,3,2,1), oma =c(2,2,1,0))

# Plot Coh 1935-39
for(sex in 1:2){
    plot(x=51:91, y=51:91, typ = "n", xlab = NA, ylab = NA, 
     xaxs = "i", yaxs = "i", log = "y", ylim = c(0.002, 0.1), 
     xlim = c(50, 95), bty = "n", xaxt = "n", yaxt = "n")

segments(x0=50, y0=c(0.002, 0.004, 0.008, 0.016, 0.032, 0.064, 0.1), 
         x1=95, y1=c(0.002, 0.004, 0.008, 0.016, 0.032, 0.064, 0.1), 
         col = "lightgray")

segments(x0=seq(50, 95, by = 5), y0=0.002, 
         x1=seq(50, 95, by = 5), y1=0.1, 
         col = "lightgray")

axis(1, at = seq(50, 95, by = 5), labels = TRUE, 
     lwd = 3)
axis(2, at = c(0.002, 0.004, 0.008, 0.016, 0.032, 0.064, 0.1), 
     labels = paste(c(0.002, 0.004, 0.008, 0.016, 0.032, 0.064, 0.1)), 
     lwd = 3)
 
# occDat1 %>% filter(Sex == sex & startAge == sAge) %>% 
#   group_by(digRe) %>% 
#   group_map(~lines(x= .$Age, y = .$mx,
#                    col = .$colOccT[1], typ = "p", pch = 16, cex = 0.5), .keep = TRUE)

occDat1 %>% filter(Sex == sex & startAge == 51) %>% 
  group_by(digRe) %>% 
  group_map(~lines(x= .$Age, y = .$sMx,
                   col = .$colOcc[1], typ = "l", lwd  = 2), .keep = TRUE)
if(sex == 1){
  mtext("Males", side = 3, adj = 0, line = 0.5, cex = 1.3)
  mtext("Birth cohorts 1935-1939", side = 3, adj = 0.02, line = -1.5)
  mtext("Death rates (log-scale)", side = 2, adj = -1.75, line = 2.5, cex = 1.3)
  legend("bottomright", legend =occLab, col =
           colOcc, bty = "n", cex = 1, pch = 15, horiz = FALSE)
  
}
if(sex == 2){
  mtext("Females", side = 3, adj = 0, line = 0.5, cex = 1.3)
}


}

# Plot Coh 1930-34
for(sex in 1:2){
  plot(x=51:91, y=51:91, typ = "n", xlab = NA, ylab = NA, 
       xaxs = "i", yaxs = "i", log = "y", ylim = c(0.002, 0.128), 
       xlim = c(50, 95), bty = "n", xaxt = "n", yaxt = "n")
  
  segments(x0=50, y0=c(0.002, 0.004, 0.008, 0.016, 0.032, 0.064, 0.128), 
           x1=95, y1=c(0.002, 0.004, 0.008, 0.016, 0.032, 0.064, 0.128), 
           col = "lightgray")
  
  segments(x0=seq(50, 95, by = 5), y0=0.002, 
           x1=seq(50, 95, by = 5), y1=0.128, 
           col = "lightgray")
  
  axis(1, at = seq(50, 95, by = 5), labels = TRUE, 
       lwd = 3)
  axis(2, at = c(0.002, 0.004, 0.008, 0.016, 0.032, 0.064, 0.128), 
       labels = paste(c(0.002, 0.004, 0.008, 0.016, 0.032, 0.064, 0.128)), 
       lwd = 3)
  
  # occDat1 %>% filter(Sex == sex & startAge == sAge) %>% 
  #   group_by(digRe) %>% 
  #   group_map(~lines(x= .$Age, y = .$mx,
  #                    col = .$colOccT[1], typ = "p", pch = 16, cex = 0.5), .keep = TRUE)
  
  occDat1 %>% filter(Sex == sex & startAge == 56) %>% 
    group_by(digRe) %>% 
    group_map(~lines(x= .$Age, y = .$sMx,
                     col = .$colOcc[1], typ = "l", lwd  = 2), .keep = TRUE)
  if(sex == 1){
    mtext("Birth cohorts 1930-1934", side = 3, adj = 0.02, line = -1.5)
  }
  mtext("Age", 1, line = 2.5, cex = 1.3)
}


# Plot Coh 1925-29
sex <- 1
for(sex in 1:2){
  plot(x=51:91, y=51:91, typ = "n", xlab = NA, ylab = NA, 
       xaxs = "i", yaxs = "i", log = "y", ylim = c(0.004, 0.256), 
       xlim = c(50, 95), bty = "n", xaxt = "n", yaxt = "n")
  
  segments(x0=50, y0=c(0.004, 0.008, 0.016, 0.032, 0.064, 0.128, 0.256), 
           x1=95, y1=c(0.004, 0.008, 0.016, 0.032, 0.064, 0.128, 0.256), 
           col = "lightgray")
  
  segments(x0=seq(50, 95, by = 5), y0=0.004, 
           x1=seq(50, 95, by = 5), y1=0.256, 
           col = "lightgray")
  
  axis(1, at = seq(50, 95, by = 5), labels = TRUE, 
       lwd = 3)
  axis(2, at = c(0.004, 0.008, 0.016, 0.032, 0.064, 0.128, 0.256), 
       labels = paste(c(0.004, 0.008, 0.016, 0.032, 0.064, 0.128, 0.256)), 
       lwd = 3)
  
  # occDat1 %>% filter(Sex == sex & startAge == sAge) %>% 
  #   group_by(digRe) %>% 
  #   group_map(~lines(x= .$Age, y = .$mx,
  #                    col = .$colOccT[1], typ = "p", pch = 16, cex = 0.5), .keep = TRUE)
  
  occDat1 %>% filter(Sex == sex & startAge == 61) %>% 
    group_by(digRe) %>% 
    group_map(~lines(x= .$Age, y = .$sMx,
                     col = .$colOcc[1], typ = "l", lwd  = 2), .keep = TRUE)
  if(sex == 1){
    mtext("Birth cohorts 1925-1929", side = 3, adj = 0.02, line = -1.5)
  }
}
# 
# # Legend
# ## Legend
# par(xpd = TRUE, mar = c(1,1,1,1))
# plot(x=1:7, y=1:7, xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n",
#      bty = "n", xlab = NA, ylab = NA, typ = "n", xlim =
#        c(1,6))
# 
# legend("top", legend =occLab[1:5], col =
#          colOcc[1:5], bty = "n", cex = 2, pch = 15, horiz = TRUE)
# # 
# # legend("bottom", legend =occLab[1:5], col =
# #          colOcc[1:5], bty = "n", cex = 2, pch = 15, horiz = TRUE)
# 

dev.off()
