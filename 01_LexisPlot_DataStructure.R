# Creating a plot that shows the structure of the analysed data

pdf("data_Layout.pdf", family = "Times", width = 5.35)
plot(x=1985:2025, y=1985:2025, typ = "n", xlab = NA, ylab = NA, bty =
       "n", xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", ylim = c(50,
                                                                     100)) 
par(xpd = TRUE)
segments(x0=seq(1985, 2025, by = 5), y0=50, x1=seq(1985, 2025, by =
                                                     5), y1= 100, col = "gray")
segments(x0=1985, y0=seq(50, 100, by = 5), x1=2025, y1= seq(50, 100,
                                                            by = 5), col =
           "gray")
par(las = 2)
axis(1, at = seq(1985, 2025, by = 5), labels = TRUE, lwd = 3)
par(las = 1)
axis(2, at = seq(50, 100, by = 5), labels = TRUE, lwd = 3)


segments(x0=2021, y0=50, x1=2021, y1= 100, col =
           "lightgray", lwd = 3)

text(x=2021, y=100, "End of follow-up \n 2020/12/31", col = "black", pos = 3)


text(x=1985, y=97.5, "Occupation reported in Census 1985", col = "black", pos = 4)

xcords <- c(1985, 2016, 2021, 1990)
ycords <- c(60, 91, 91, 60)

polygon(x=xcords, y=ycords, col =grey(level = 0.2, alpha = 0.4),
        border = grey(level = 0.2, alpha = 0.4))

xcords <- c(1985, 2016, 2021, 1990)
ycords <- c(55, 86, 86, 55)

polygon(x=xcords, y=ycords, col =grey(level = 0.2, alpha = 0.4),
        border = grey(level = 0.2, alpha = 0.4))

xcords <- c(1985, 2016, 2021, 1990)
ycords <- c(50, 81, 81, 50)

polygon(x=xcords, y=ycords, col =grey(level = 0.2, alpha = 0.4),
        border = grey(level = 0.2, alpha = 0.4))

text(x=1987.5, y=61, "Cohorts 1925-1929, Ages 60-91", pos = 4, srt = 45)
text(x=1987.5, y=56, "Cohorts 1930-1934, Ages 55-86", pos = 4, srt = 45)
text(x=1987.5, y=51, "Cohorts 1935-1939, Ages 50-81", pos = 4, srt = 45)

par(las = 0)
mtext("Year", 1, line = 2.8)
mtext("Age", 2, line = 2.8)

dev.off()
