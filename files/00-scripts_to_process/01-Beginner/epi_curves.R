
```{r epi-curve1, eval=F, echo = F, message=F, warning=F, error=F, fig.height=6*9/16*2, fig.cap = "Epidemic Curve of COVID-19 in Athens-Clarke County, GA, USA"}

date.labels <- seq(min(covid$date), max(covid$date), by="months")

at.points <- seq(0.5, nrow(covid)-0.5, by=1)[which(covid$date%in%date.labels)]

at.points2 <- seq(0.5, nrow(covid)-0.5, by=1)[which(covid$date%in%c(min(wbe$sample_date), max(wbe$sample_date)))]

layout(matrix(c(1,2,1,3), nrow = 2), widths = c(1,1), heights = c(3,1))

par(mar=c(2.1,3.1,2.1,1.1))
# barplot(cases.reported~date, data=covid)
barplot(cases.reported~date, data=covid, las=2, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', width = 1, space = 0, xaxs = 'i', yaxs = 'i', axes=F, col = "gainsboro", border = "gainsboro")
axis(1, at=at.points, tick=T, labels = paste(format(date.labels, "%b %y"),' '), xpd = TRUE, cex.axis=0.7, line = 0, padj = -1)
axis(2, labels = T, tick = T, cex=0.7, cex.axis = 0.7, las = 1)
title(ylab = "Cases", line = 2)
axis(3, at = at.points2, labels = format(c(min(wbe$sample_date), max(wbe$sample_date)), "%d %b %y"), tick = T, line = 0, tck = 0.02, cex.axis = 0.7)
axis(3, at = mean(at.points2), labels = "Study Period", line = 0, padj = -0.25, tck = -0.04)


barplot(cases.symptom.onset~date, data=covid, las=2, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', width = 1, space = 0, xaxs = 'i', yaxs = 'i', axes=F, add = T, col = viridis::viridis(100, alpha = 0.75)[55], border = viridis::viridis(100, alpha = 0.75)[55])

legend("topleft", fill = c("gainsboro", viridis::viridis(100, alpha = 0.75)[55]), legend = c("Reported", "Symptom Onset"))
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "A", adj = c(0,1), xpd = T, cex = 1, font = 2)


par(mar = c(2.1, 1.1, 1.1, 1.1))
barplot(cases.reported~date, data=covid, las=2, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', width = 1, space = 0, xaxs = 'i', yaxs = 'i', axes=F, col = "black", ylim = c(0, 300))
axis(1, at=at.points, tick=T, labels = paste(format(date.labels, "%b %y"),' '), xpd = TRUE, cex.axis=0.7, line = 0, padj = -2, tck = -0.02)
axis(2, labels = F, tick = T, cex=0.7, cex.axis = 0.7, las = 1)
title(xlab = "Date of Report", line = 0.75)
# axis(3, at = at.points2, labels = format(c(min(wbe$sample_date), max(wbe$sample_date)), "%d %b %y"), tick = T, line = 0, tck = 0.02, cex.axis = 0.7)
# axis(3, at = mean(at.points2), labels = "Study Period", line = 0, padj = -0.25, tck = -0.04)
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "B", adj = c(0,1), xpd = T, cex = 1, font = 2)



barplot(cases.symptom.onset~date, data=covid, las=2, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', width = 1, space = 0, xaxs = 'i', yaxs = 'i', axes=F, col = viridis::viridis(100, alpha = 0.75)[55], border = viridis::viridis(100, alpha = 0.75)[55], ylim = c(0, 300))
axis(1, at=at.points, tick=T, labels = paste(format(date.labels, "%b %y"),' '), xpd = TRUE, cex.axis=0.7, line = 0, padj = -2, tck = -0.02)
axis(2, labels = F, tick = T, cex=0.7, cex.axis = 0.7, las = 1)
title(xlab = "Date of Symptom Onset", line = 0.75)
# axis(3, at = at.points2, labels = format(c(min(wbe$sample_date), max(wbe$sample_date)), "%d %b %y"), tick = T, line = 0, tck = 0.02, cex.axis = 0.7, hadj = 1)
# axis(3, at = mean(at.points2), labels = "Study Period", line = 0, padj = -0.25, tck = -0.04)
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "C", adj = c(0,1), xpd = T, cex = 1, font = 2)


```






```{r epi-curve2, eval = F, echo = F, message=F, warning=F, error=F, fig.height=6*9/16*2, fig.cap = "Epidemic Curve of COVID-19 in Athens-Clarke County, GA, USA"}

date.labels <- seq(min(covid$date), max(covid$date), by="months")

at.points <- seq(0.5, nrow(covid)-0.5, by=1)[which(covid$date%in%date.labels)]

at.points2 <- seq(0.5, nrow(covid)-0.5, by=1)[which(covid$date%in%c(min(wbe$sample_date), max(wbe$sample_date)))]

par(mar=c(2.1,3.6,2.1,1.1), mfrow = c(2,1))
# # barplot(cases.reported~date, data=covid)
# barplot(cases.reported~date, data=covid, las=2, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', width = 1, space = 0, xaxs = 'i', yaxs = 'i', axes=F, col = "gainsboro", border = "gainsboro")
# axis(1, at=at.points, tick=T, labels = paste(format(date.labels, "%b %y"),' '), xpd = TRUE, cex.axis=0.7, line = 0, padj = -1)
# axis(2, labels = T, tick = T, cex=0.7, cex.axis = 0.7, las = 1)
# title(ylab = "Cases", line = 2)
# axis(3, at = at.points2, labels = format(c(min(wbe$sample_date), max(wbe$sample_date)), "%d %b %y"), tick = T, line = 0, tck = 0.02, cex.axis = 0.7)
# axis(3, at = mean(at.points2), labels = "Study Period", line = 0, padj = -0.25, tck = -0.04)
# 
# 
# barplot(cases.symptom.onset~date, data=covid, las=2, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', width = 1, space = 0, xaxs = 'i', yaxs = 'i', axes=F, add = T, col = viridis::viridis(100, alpha = 0.75)[55], border = viridis::viridis(100, alpha = 0.75)[55])
# 
# legend("topleft", fill = c("gainsboro", viridis::viridis(100, alpha = 0.75)[55]), legend = c("Reported", "Symptom Onset"))
# text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
#      par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
#      labels = "A", adj = c(0,1), xpd = T, cex = 1, font = 2)


# par(mar = c(2.1, 1.1, 1.1, 1.1))
barplot(cases.reported~date, data=covid, las=2, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', width = 1, space = 0, xaxs = 'i', yaxs = 'i', axes=F, col = "black", ylim = c(0, 300))
axis(1, at=at.points, tick=T, labels = paste(format(date.labels, "%b %y"),' '), xpd = TRUE, cex.axis=0.7, line = 0, padj = -2, tck = -0.02)
axis(2, labels = F, tick = T, cex=0.7, cex.axis = 0.7, las = 1)
title(xlab = "Date of Report", line = 1)
# axis(3, at = at.points2, labels = format(c(min(wbe$sample_date), max(wbe$sample_date)), "%d %b %y"), tick = T, line = 0, tck = 0.02, cex.axis = 0.7)
# axis(3, at = mean(at.points2), labels = "Study Period", line = 0, padj = -0.25, tck = -0.04)
# text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
#      par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
#      labels = "B", adj = c(0,1), xpd = T, cex = 1, font = 2)


axis(2, labels = T, tick = T, cex=0.7, cex.axis = 0.7, las = 1)
title(ylab = "Cases", line = 2)
axis(3, at = at.points2, labels = format(c(min(wbe$sample_date), max(wbe$sample_date)), "%d %b %y"), tick = T, line = 0, tck = 0.02, cex.axis = 0.7)
axis(3, at = mean(at.points2), labels = "Study Period", line = 0, padj = -0.25, tck = -0.04)

abline(v = at.points, lty = 3, col = "gainsboro", xpd = T)

text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "A", adj = c(0,1), xpd = T, cex = 1, font = 2)



barplot(cases.symptom.onset~date, data=covid, las=2, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', width = 1, space = 0, xaxs = 'i', yaxs = 'i', axes=F, col = viridis::viridis(100, alpha = 1)[55], border = viridis::viridis(100, alpha = 1)[55], ylim = c(0, 300))
axis(1, at=at.points, tick=T, labels = paste(format(date.labels, "%b %y"),' '), xpd = TRUE, cex.axis=0.7, line = 0, padj = -2, tck = -0.02)
axis(2, labels = F, tick = T, cex=0.7, cex.axis = 0.7, las = 1)
title(xlab = "Date of Symptom Onset", line = 1)
# axis(3, at = at.points2, labels = format(c(min(wbe$sample_date), max(wbe$sample_date)), "%d %b %y"), tick = T, line = 0, tck = 0.02, cex.axis = 0.7, hadj = 1)
# axis(3, at = mean(at.points2), labels = "Study Period", line = 0, padj = -0.25, tck = -0.04)
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "B", adj = c(0,1), xpd = T, cex = 1, font = 2)
axis(2, labels = T, tick = T, cex=0.7, cex.axis = 0.7, las = 1)
title(ylab = "Cases", line = 2)

abline(v = at.points, lty = 3, col = "gainsboro", xpd = T)
# legend("topleft", fill = c("gainsboro", viridis::viridis(100, alpha = 0.75)[55]), legend = c("Reported", "Symptom Onset"))
```







```{r epi-curve, echo = F, message=F, warning=F, error=F, fig.height=6*9/16*2, fig.cap = "Epidemic Curve of COVID-19 in Athens-Clarke County, GA, USA"}

date.labels <- seq(min(covid$date), max(covid$date), by="months")

at.points <- seq(0.5, nrow(covid)-0.5, by=1)[which(covid$date%in%date.labels)]

at.points2 <- seq(0.5, nrow(covid)-0.5, by=1)[which(covid$date%in%c(min(wbe$sample_date), max(wbe$sample_date)))]




# png(filename = "./consult/03-output/for-pres/epi-curves.png", width = 16, height = 9, units = "in", res = 300, pointsize = 16)
par(mar=c(2.1,3.6,2.1,1.1), mfrow = c(3,1))






barplot(cases.symptom.onset~date, data=covid, las=2, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', width = 1, space = 0, xaxs = 'i', yaxs = 'i', axes=F, col = viridis::viridis(100, alpha = 1)[1], border = viridis::viridis(100, alpha = 1)[1], ylim = c(0, 300))
axis(1, at=at.points, tick=T, labels = paste(format(date.labels, "%b %y"),' '), xpd = TRUE, cex.axis=0.7, line = 0, padj = -2, tck = -0.02)
axis(2, labels = F, tick = T, cex=0.7, cex.axis = 0.7, las = 1)
title(xlab = "Date of Symptom Onset", line = 1)
# axis(3, at = at.points2, labels = format(c(min(wbe$sample_date), max(wbe$sample_date)), "%d %b %y"), tick = T, line = 0, tck = 0.02, cex.axis = 0.7, hadj = 1)
# axis(3, at = mean(at.points2), labels = "Study Period", line = 0, padj = -0.25, tck = -0.04)
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "A", adj = c(0,1), xpd = T, cex = 1, font = 2)
axis(2, labels = T, tick = T, cex=0.7, cex.axis = 0.7, las = 1)
title(ylab = "Cases", line = 2)

abline(v = at.points, lty = 3, col = "grey60", xpd = T)
# legend("topleft", fill = c("gainsboro", viridis::viridis(100, alpha = 0.75)[55]), legend = c("Reported", "Symptom Onset"))

axis(3, at = at.points2, labels = format(c(min(wbe$sample_date), max(wbe$sample_date)), "%d %b %y"), tick = T, line = 0, tck = 0.02, cex.axis = 0.7)
axis(3, at = mean(at.points2), labels = "Study Period", line = 0, padj = -0.25, tck = -0.04)



legend("topleft", fill = c(viridis::viridis(100, alpha = 1)[1], viridis::viridis(100, alpha = 1)[50], viridis::viridis(100, alpha = 1)[100]), border = c(viridis::viridis(100, alpha = 1)[1], viridis::viridis(100, alpha = 1)[50], viridis::viridis(100, alpha = 1)[100]), legend = c("Symptom Onset", "Specimen Collection", "Reported"))













barplot(pcr_pos~date, data=covid, las=2, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', width = 1, space = 0, xaxs = 'i', yaxs = 'i', axes=F, col = viridis::viridis(100, alpha = 1)[50], border = viridis::viridis(100, alpha = 1)[50], ylim = c(0, 300))
axis(1, at=at.points, tick=T, labels = paste(format(date.labels, "%b %y"),' '), xpd = TRUE, cex.axis=0.7, line = 0, padj = -2, tck = -0.02)
axis(2, labels = F, tick = T, cex=0.7, cex.axis = 0.7, las = 1)
title(xlab = "Date of Specimen Collection", line = 1)
# axis(3, at = at.points2, labels = format(c(min(wbe$sample_date), max(wbe$sample_date)), "%d %b %y"), tick = T, line = 0, tck = 0.02, cex.axis = 0.7, hadj = 1)
# axis(3, at = mean(at.points2), labels = "Study Period", line = 0, padj = -0.25, tck = -0.04)
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "B", adj = c(0,1), xpd = T, cex = 1, font = 2)
axis(2, labels = T, tick = T, cex=0.7, cex.axis = 0.7, las = 1)
title(ylab = "Cases", line = 2)

abline(v = at.points, lty = 3, col = "grey60", xpd = T)
# legend("topleft", fill = c("gainsboro", viridis::viridis(100, alpha = 0.75)[55]), legend = c("Reported", "Symptom Onset"))







# par(mar = c(2.1, 1.1, 1.1, 1.1))
barplot(cases.reported~date, data=covid, las=2, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', width = 1, space = 0, xaxs = 'i', yaxs = 'i', axes=F, col = viridis::viridis(100, alpha = 1)[100], border = viridis::viridis(100, alpha = 1)[100], ylim = c(0, 300))
axis(1, at=at.points, tick=T, labels = paste(format(date.labels, "%b %y"),' '), xpd = TRUE, cex.axis=0.7, line = 0, padj = -2, tck = -0.02)
axis(2, labels = F, tick = T, cex=0.7, cex.axis = 0.7, las = 1)
title(xlab = "Date of Report", line = 1)


axis(2, labels = T, tick = T, cex=0.7, cex.axis = 0.7, las = 1)
title(ylab = "Cases", line = 2)

abline(v = at.points, lty = 3, col = "grey60", xpd = T)




text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "C", adj = c(0,1), xpd = T, cex = 1, font = 2)





# dev.off()
```



















```{r epi-curve-decon, echo = F, message=F, warning=F, error=F, fig.height=6*9/16*2, fig.cap = "Comparison of Epidemic Curves from Dates of Report and Symptom Onset to a Deconvoluted Incidence Curve"}

# png(filename = "./consult/03-output/for-pres/deconvolution.png", width = 16*3/5, height = 9*3/5, units = "in", res = 300, pointsize = 16)

par(mar=c(3.1,3.1,2.1,1.1))
# barplot(cases.reported~date, data=covid)
barplot(cases.reported~date, data=covid, las=2, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', width = 1, space = 0, xaxs = 'i', yaxs = 'i', axes=F, col = "gainsboro", border = "gainsboro")
axis(1, at=at.points, tick=T, labels = paste(format(date.labels, "%b %y"),' '), xpd = TRUE, cex.axis=0.7, line = 0, padj = -1)
axis(2, labels = T, tick = T, cex=0.7, cex.axis = 0.7, las = 1)
title(xlab = "Date", ylab = "Cases", line = 2)
axis(3, at = at.points2, labels = format(c(min(wbe$sample_date), max(wbe$sample_date)), "%d %b %y"), tick = T, line = 0, tck = 0.02, cex.axis = 0.7)
axis(3, at = mean(at.points2), labels = "Study Period", line = 0, padj = -0.25, tck = -0.04)

barplot(cases.symptom.onset~date, data=covid, las=2, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', width = 1, space = 0, xaxs = 'i', yaxs = 'i', axes=F, add = T, col = viridis::viridis(100, alpha = 0.25)[1], border = viridis::viridis(100, alpha = 0.25)[1])


lines(seq(0.5, nrow(covid)-0.5, by=1), decon.report$Chat, lty = 3, lwd = 2)
lines(seq(0.5, nrow(covid)-0.5, by=1), decon.report$Ihat, lty = 5, lwd = 4)

# lines(seq(0.5, nrow(covid)-0.5, by=1), covid$cases.reported.7dma)
# lines(seq(0.5, nrow(covid)-0.5, by=1), covid$cases.symptom.onset.7dma)


legend("topleft", 
       bty = 'n',
       pt.bg = c("gainsboro", viridis::viridis(100, alpha = 0.25)[1], NA, NA), 
       pt.cex = 2,
       pch = c(22,22,NA,NA),
       lty = c(0, 0, 3, 5), 
       lwd = c(0, 0, 1, 2), 
       col = c("gainsboro", viridis::viridis(100, alpha = 0.25)[1], "black", "black"), 
       legend = c("Reported", "Symptom Onset", "Fitted Convolution", "Estimated Deconvolution"))

# dev.off()

```








