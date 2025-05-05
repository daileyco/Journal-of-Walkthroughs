# figures


# png(filename = "./03-Output/02-Figures/figure3.png", width = 16*2/5, height = 9*2/5*2, units = "in", res = 300, pointsize = 16)

par(mfcol = c(2,1), mar = c(2.1, 2.1, 1.1, 0))

# layout.show(2)
plot()

# put "A" in top left
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "A", adj = c(0,1), xpd = T, cex = 1, font = 2)


plot()
box()
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "B", adj = c(0,1), xpd = T, cex = 1, font = 2)

# dev.off()




