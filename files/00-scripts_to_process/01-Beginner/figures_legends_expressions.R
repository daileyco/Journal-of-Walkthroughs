# figures
## legends and expressions

par(mfrow = c(2,2), mar = c(2.1,2.1,1.1,0.1))
plot()
legend("topright", lwd = c(2,0.5,2,0.5), lty = c(1,1,3,3), legend = c(expression(LOD[~N1]), expression(LOQ[~N1]), expression(frac(1,2) %*% LOD[~N1]),expression(frac(1,2) %*% LOQ[~N1])), bty = "n", y.intersp = 1.25, cex = 0.7)
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "A", adj = c(0,1), xpd = T, cex = 1, font = 2)


plot()
legend("topright", lwd = c(2,0.5,2,0.5), lty = c(1,1,3,3), legend = c(expression(LOD[~N2]), expression(LOQ[~N2]), expression(frac(1,2) %*% LOD[~N2]),expression(frac(1,2) %*% LOQ[~N2])), bty = "n", y.intersp = 1.25, cex = 0.7)
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "B", adj = c(0,1), xpd = T, cex = 1, font = 2)




plot()
legend("topright", lwd = c(2,0.5,2,0.5), lty = c(1,1,3,3), legend = c(expression(LOD[~N1]), expression(LOQ[~N1]), expression(frac(1,2) %*% LOD[~N1]),expression(frac(1,2) %*% LOQ[~N1])), bty = "n", y.intersp = 1.25, cex = 0.7)
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "C", adj = c(0,1), xpd = T, cex = 1, font = 2)


plot()
legend("topright", lwd = c(2,0.5,2,0.5), lty = c(1,1,3,3), legend = c(expression(LOD[~N2]), expression(LOQ[~N2]), expression(frac(1,2) %*% LOD[~N2]),expression(frac(1,2) %*% LOQ[~N2])), bty = "n", y.intersp = 1.25, cex = 0.7)
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "D", adj = c(0,1), xpd = T, cex = 1, font = 2)

