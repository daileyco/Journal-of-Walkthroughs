x = rnorm(5)
e = rnorm(5, sd = 0.2)
x.lb = x-e
x.ub = x+e
plot(NA, type = "n", ylim = c(min(x.lb)*1.5, max(x.ub)*1.5), xlim = c(floor(min(test)), ceiling(max(test))))
barplot(x, add=T)
arrows(test, x.lb, test, x.ub, angle = 90, code = 3, xpd = T)



plot(1:length(x)/2, x, ylim = c(min(x.lb)*1.5, max(x.ub)*1.5))
arrows(1:length(x)/2, x.lb, 1:length(x)/2, x.ub, length = 0.05, angle = 90, code = 3, xpd = T)















library(readxl)

coef.ests <- read_xlsx("./COVID_Estimates.xlsx")

coef.ests <- read_xlsx("./Hospregion_Estimates.xlsx")
coef.ests <- read_xlsx("./LOR_Estimates.xlsx")
coef.ests <- read_xlsx("./Ownership_Estimates.xlsx")
coef.ests <- read_xlsx("./Teachingstatus_Estimates.xlsx")

# coef.ests <- coef.ests[which(coef.ests$ind==1),]

coef.ests <- sapply(coef.ests, trimws)

coef.ests <- as.data.frame(coef.ests)

coef.ests[,3:5] <- sapply(coef.ests[,3:5], as.numeric)

coef.ests <- coef.ests[c(1:nrow(coef.ests), rep(which(coef.ests$variable=="spacer"), 2))[order(c(1:nrow(coef.ests), rep(which(coef.ests$variable=="spacer"), 2)))],]


coef.ests$y <- (1:nrow(coef.ests))*-1

coef.ests <- coef.ests[-which(coef.ests$variable=="spacer"),]

coef.ests$level <- ifelse(coef.ests$point==1, paste0(coef.ests$level, " (REF)"), coef.ests$level)


xlimits <- c(min(log(coef.ests$lb), na.rm=T)*1.2, max(log(coef.ests$ub), na.rm = T)*1.2)

xlimits <- log(c(0.25, 4))


ylimits <- c(min(coef.ests$y, na.rm = T)-1, max(coef.ests$y, na.rm = T)+1)





png(filename = "./Hospregion_coef_plot.png", width = 16, height = 9, units = "in", pointsize = 12, res = 300, family = "sans")
# png(filename = "./LOR_coef_plot.png", width = 16, height = 9, units = "in", pointsize = 12, res = 300, family = "sans")
# png(filename = "./Ownership_coef_plot.png", width = 16, height = 9, units = "in", pointsize = 12, res = 300, family = "sans")
# png(filename = "./Teachingstatus_coef_plot.png", width = 16, height = 9, units = "in", pointsize = 12, res = 300, family = "sans")





par(mar = c(4.1, 20.1, 0, 0))
plot(NA, type = "n", xlim = xlimits, ylim = ylimits, xlab = "", ylab = "", axes = F)
abline(v = log(1), lty = 3)
abline(v = log(c(0.5, 2)), lty = 5, col = "gainsboro")

points(x = log(coef.ests$point[which(coef.ests$point!=1)]), y = coef.ests$y[which(coef.ests$point!=1)], pch = 19)
arrows(x0 = log(coef.ests$lb), y0 = coef.ests$y, x1 = log(coef.ests$ub), y1 = coef.ests$y, angle = 90, length = 0.05, code = 3)

axis(1, at = log(c(0.25,0.5,1,2,4)), labels= exp(log(c(0.25,0.5,1,2,4))), line = 0, cex.axis = 24/28)
text(x = mean(range(xlimits)), y = min(ylimits)*1.125, labels = "Odds Ratio \nw/ 95% Confidence Interval", xpd = T, cex = 1)

axis(2, at = coef.ests$y[which(coef.ests$point!=1)], labels= coef.ests$level[which(coef.ests$point!=1)], line = 0, las = 1, cex.axis = 24/28)
axis(2, at = coef.ests$y[which(coef.ests$point==1)], labels= coef.ests$level[which(coef.ests$point==1)], line = 0, las = 1, cex.axis = 24/28, font = 3, tick = F)

axis(2, at = coef.ests$y[which(!duplicated(coef.ests$variable))]+1.2, labels = coef.ests$variable[which(!duplicated(coef.ests$variable))], line = 17.5, hadj = 0, las = 1, cex.axis = 1, tick = F, tck = -0.075, font = 2)
axis(2, at = coef.ests$y[which(!duplicated(coef.ests$variable))]+0.5, labels = F, line = 0, tick = T, tcl = -18.5, lwd.ticks = 2)



segments(x0 = par('usr')[1], y0 = min(coef.ests$y), x1 = par('usr')[1], y1 = max(coef.ests$y))


dev.off()


dir("Data")
getwd()








coef_plot <- function(filepath){
  
  require(readxl)
  
  coef.ests <- read_xlsx(filepath)
  
  coef.ests <- sapply(coef.ests, trimws)
  coef.ests <- as.data.frame(coef.ests)
  coef.ests[,3:5] <- sapply(coef.ests[,3:5], as.numeric)
  coef.ests <- coef.ests[c(1:nrow(coef.ests), rep(which(coef.ests$variable=="spacer"), 2))[order(c(1:nrow(coef.ests), rep(which(coef.ests$variable=="spacer"), 2)))],]
  
  
  coef.ests$y <- (1:nrow(coef.ests))*-1
  coef.ests <- coef.ests[-which(coef.ests$variable=="spacer"),]
  coef.ests$level <- ifelse(coef.ests$point==1, paste0(coef.ests$level, " (REF)"), coef.ests$level)
  
  
  xlimits <- c(min(log(coef.ests$lb), na.rm=T)*1.2, max(log(coef.ests$ub), na.rm = T)*1.2)
  xlimits <- log(c(0.25, 4))
  
  ylimits <- c(min(coef.ests$y, na.rm = T)-1, max(coef.ests$y, na.rm = T)+1)
  
  
  
  par(mar = c(4.1, 20.1, 0, 0))
  plot(NA, type = "n", xlim = xlimits, ylim = ylimits, xlab = "", ylab = "", axes = F)
  abline(v = log(1), lty = 3)
  abline(v = log(c(0.5, 2)), lty = 5, col = "gainsboro")
  
  points(x = log(coef.ests$point[which(coef.ests$point!=1)]), y = coef.ests$y[which(coef.ests$point!=1)], pch = 19)
  arrows(x0 = log(coef.ests$lb), y0 = coef.ests$y, x1 = log(coef.ests$ub), y1 = coef.ests$y, angle = 90, length = 0.05, code = 3)
  
  axis(1, at = log(c(0.25,0.5,1,2,4)), labels= exp(log(c(0.25,0.5,1,2,4))), line = 0, cex.axis = 24/28)
  text(x = mean(range(xlimits)), y = min(ylimits)*1.125, labels = "Odds Ratio \nw/ 95% Confidence Interval", xpd = T, cex = 1)
  
  axis(2, at = coef.ests$y[which(coef.ests$point!=1)], labels= coef.ests$level[which(coef.ests$point!=1)], line = 0, las = 1, cex.axis = 24/28)
  axis(2, at = coef.ests$y[which(coef.ests$point==1)], labels= coef.ests$level[which(coef.ests$point==1)], line = 0, las = 1, cex.axis = 24/28, font = 3, tick = F)
  
  axis(2, at = coef.ests$y[which(!duplicated(coef.ests$variable))]+1.2, labels = coef.ests$variable[which(!duplicated(coef.ests$variable))], line = 17.5, hadj = 0, las = 1, cex.axis = 1, tick = F, tck = -0.075, font = 2)
  axis(2, at = coef.ests$y[which(!duplicated(coef.ests$variable))]+0.5, labels = F, line = 0, tick = T, tcl = -18.5, lwd.ticks = 2)
  
  
  
  segments(x0 = par('usr')[1], y0 = min(coef.ests$y), x1 = par('usr')[1], y1 = max(coef.ests$y))

}



png(filename = "./Hospregion_coef_plot.png", width = 16, height = 9, units = "in", pointsize = 12, res = 300, family = "sans")
coef_plot("./Hospregion_Estimates.xlsx")
dev.off()

png(filename = "./LOR_coef_plot.png", width = 16, height = 9, units = "in", pointsize = 12, res = 300, family = "sans")
coef_plot("./LOR_Estimates.xlsx")
dev.off()

png(filename = "./Ownership_coef_plot.png", width = 16, height = 9, units = "in", pointsize = 12, res = 300, family = "sans")
coef_plot("./Ownership_Estimates.xlsx")
dev.off()

png(filename = "./Teachingstatus_coef_plot.png", width = 16, height = 9, units = "in", pointsize = 12, res = 300, family = "sans")
coef_plot("./Teachingstatus_Estimates.xlsx")
dev.off()



















