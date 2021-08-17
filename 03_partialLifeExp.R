# Calculation of Partial Life expectancy
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

# Calculate pLE
pLE <- 
  occDat1 %>% 
  group_by(startAge, digRe, Sex) %>% 
  group_map(~partialLe(mx=.$sMx, 
                       occ=.$digRe,
                       sex=.$Sex,
                       age=.$Age,
                       startAge=.$startAge), .keep = TRUE) %>% 
  bind_rows() %>% as_tibble()

# starting Values of pLE
startPLE <- 
  pLE %>%
  arrange(pE) %>% 
  filter(age == startAge)

# Labels occupation
unique(bsp$occ)
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

colOcc <- c('#88CCEE', '#44AA99', '#117733', '#332288', '#DDCC77', '#999933','#CC6677', '#882255', '#AA4499')
names(colOcc) <- c(0:4, 6, 7, 90, 91)
startPLE$colOcc <- mapvalues(startPLE$occ, from = c(0:4, 6, 7, 90, 91), to = colOcc)

# Make PLot for PAR LE 

pdf("barPlot_initialPLE.pdf", family = "Times", width = 14, pointsize = 16, height =10)
naturePlot <- layout(matrix(1:6, byrow = TRUE, ncol = 3))
par(xpd = FALSE, mar = c(3,4,2,2), oma =c(2,2,1,0))
for(sex_var in c(2,1)){
  for(coh_var in c(61, 56, 51)){
    plotDat <- startPLE %>% filter(sex == sex_var & startAge == coh_var) %>% arrange(pE)
    plot(x=1:9, y=1:9, xaxt = "n", yaxt = "n",
         xaxs = "i", yaxs = "i", bty = "n", 
         xlab = NA, ylab = NA, typ = "n", xlim = c(0,14), ylim = c(1, 10))
    
    if(sex_var == 1){

        segments(x0=c(0, seq(2, 14, by = 2)), y0 =1, 
             x1=c(0, seq(2, 14, by = 2)), y1 =10, lwd = 3, col = "lightgray")
    
        segments(x0=c(0, seq(3, 13, by = 2)), y0 =1, 
             x1=c(0, seq(3, 13, by = 2)), y1 =10, lwd = 1, col = "lightgray")
    
        axis(1, at = c(0, seq(2, 14, by = 2)), labels =
           c(0,seq(19, 31, by = 2)), lwd = 3, cex.axis = 1.4)
    
        axis(1, at = seq(3, 13, by = 2), labels = FALSE,
         lwd = 1, cex.axis = 1.2)
    
        axis.break(1, 1, style = "slash")
        
        if(coh_var == 61){mtext("Males", 3, cex = 1.4, adj = 0, line = 0)}
        if(coh_var == 56){mtext("Partial life expectancy (in years)", 1, cex = 1.4, line = 3.5)}
        
        
    }
    
    if(sex_var == 2){
      
      segments(x0=c(0, seq(2, 14, by = 2)), y0 =1, 
               x1=c(0, seq(2, 14, by = 2)), y1 =9, lwd = 3, col = "lightgray")
      
      segments(x0=c(0, seq(3, 13, by = 2)), y0 =1, 
               x1=c(0, seq(3, 13, by = 2)), y1 =9, lwd = 1, col = "lightgray")
      
      axis(1, at = c(0, seq(2, 14, by = 2)), labels =
             c(0,seq(19, 31, by = 2)), lwd = 3, cex.axis = 1.4)
      
      axis(1, at = seq(3, 11, by = 2), labels = FALSE,
           lwd = 1, cex.axis = 1.2)
      
      axis.break(1, 1, style = "slash")
      
      if(coh_var == 61){mtext("Females", 3, cex = 1.4, adj = 0, line = 0)}
      
      if(coh_var == 61){mtext("Birth cohorts 1925-29, Ages 61-91", 3, line = -1.5, cex = 1.1, adj = 0)}
      if(coh_var == 56){mtext("Birth cohorts 1930-34, Ages 56-86", 3, line = -1.5, cex = 1.1, adj = 0)}
      if(coh_var == 51){mtext("Birth cohorts 1925-29, Ages 51-81", 3, line = -1.5, cex = 1.1, adj = 0)}
      
      
    }
    
        addFac <- 1:9
    
        names(addFac) <- plotDat$occ
    
    for(i in plotDat$occ){
      pE <- plotDat %>% filter(occ == i) %>% select(pE)
      colpE <- plotDat %>% filter(occ == i) %>% select(colOcc)
      xx <- c(0, unlist(rep(pE,2))-17,0)
      yy <- c(rep(0.2, 2), rep(0.8, 2))+addFac[paste(i)]
      polygon(x=xx, y=yy, col = as.character(colpE), border = "white")
      if(coh_var == 61 & sex_var == 1){
      text(x=pE-17, y=0.5+addFac[paste(i)],occLab[paste(i)], pos = 4, cex = 1.1)
      }
    }
  }
}

dev.off()
