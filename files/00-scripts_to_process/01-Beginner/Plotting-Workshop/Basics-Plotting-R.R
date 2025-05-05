


## Set up

mypath <- "C:/Users/daileyco/Desktop/Plotting-Workshop"
setwd(mypath)




## Vocab

# Graphical Parameters
# par()
# ?par()
par('usr')
par('mar')
par('mfrow')




## Plot Basic Example


plot(x=1:10, y=1:10, 
     type = "o", 
     main = "Woah")
par('usr')



### "Deconstructed" / Show individual functions used to build whole
plot.new()
plot.window(
  xlim = c(1,10),
  ylim = c(1,10)
)
points(x=1:10, y=1:10, 
       pch=1)
lines(x=1:10, y=1:10,
      lty=1)
axis(side=1,
     at=seq(2,10,by=2),
     labels=TRUE,
     line=0 #,pos=c()
     # ,padj=0.5,hadj=0.5
     )
axis(side=1,
     at=mean(par('usr')[1:2]), 
     labels = c("Vector from one to ten"), 
     tick = FALSE, 
     line = 2)
axis(side=2,
     at=seq(2,10,by=2),
     labels=TRUE,
     line=0 #,pos=c()
     # ,padj=0.5,hadj=0.5
     , las = 2
     )
axis(side=2,
     at=mean(par('usr')[1:2]), 
     labels = c("Vector from one to ten"), 
     tick = FALSE, 
     line = 2)
title(main="Radical")
box()
par('usr')



## Add random point

xr = runif(1,par('usr')[1],par('usr')[2])
yr = runif(1,par('usr')[3],par('usr')[4])
points(x=xr, 
       y=yr, 
       bg = "red", 
       col = "black", 
       pch = 23)

arrows(x0=8,
       y0=2, 
       x1=xr,
       y1=yr)

text(x=8,y=2,
     labels=c("Look here, silly!"),
     adj=c(0.5,1))

## Add legend
legend("topleft", 
       legend = c("IDK", "Random"), 
       pch = c(1,23), 
       pt.bg = c(NA,"red"), 
       col = c("black", "black"))











##Others
# barplot()
# rect()
# polygon()




barplot(1:10)





plot.new()
plot.window(xlim=c(0,10),ylim=c(0,10))

xlefts<-0:9
xrights<-xlefts+1

rect(xleft=xlefts,
     ybottom=xlefts,
     xright=xrights,
     ytop=xrights,
     density = 12:21, 
     col = "red", 
     border="purple")












## Fun function

plot_My_Holiday_Tree <- function(myseed = .Random.seed[1]){
  
  
  set.seed(myseed)
  
  par(mar=c(1,1,1,1))
  plot.new()
  plot.window(xlim=c(-10,10),ylim=c(-10,10))
  
  mid <- 0
  bot <- -8
  top <- 8
  left <- bot
  right <- top
  
  
  rect(xleft=mid-2,
       ybottom=bot, 
       xright=mid+2,
       ytop=bot+3,
       col=colorRampPalette(c("brown", "black"))(5)[3],
       border="brown")
  
  
  myxs <- mid+
    c(runif(1,-1,1),
      -7+runif(1,-0.1,0.1), 
      7+runif(1,-0.1,0.1))
  myys <- c(bot+5+3,
            bot+3+runif(1,-0.1,0),
            bot+3+runif(1,-0.1,0))
  
  polygon(x=myxs, 
          y=myys, 
          col="darkgreen", 
          border="darkgreen")
  
  myxs <- mid+
    c(runif(1,-0.25,0.25),
      -4+runif(1,-0.1,0.1), 
      4+runif(1,-0.1,0.1))
  
  myys <- myys+4+c(runif(1,-1,1),runif(2,-0.2,0))
  
  polygon(x=myxs, 
          y=myys, 
          col="darkgreen", 
          border="darkgreen")
  
  myxs <- mid+
    c(runif(1,-0.25,0.25),
      -2+runif(1,-0.1,0.1), 
      2+runif(1,-0.1,0.1))
  
  myys <- myys+4+c(runif(1,-1,1),runif(2,-0.2,0))
  
  polygon(x=myxs, 
          y=myys, 
          col="darkgreen", 
          border="darkgreen")
  
  points(myxs[1],myys[1],adj=c(0),pch=21,bg="white",cex=3)
  points(myxs[1],myys[1]+0.675,adj=c(0.5),pch=21,bg="white",cex=2.2)
  points(myxs[1],myys[1]+1.2,adj=c(1),pch=21,bg="white",cex=1.5)
  
  
  myxs<-mid+c(
    runif(2,-6,6), 
    runif(2,-5,5),
    runif(2,-4,4),
    runif(2,-3,3),
    runif(2,-2,2),
    runif(2,-1,1)
  )
  
  myys<-rep(c(-5,-3,-1,2,4,6), each=2)+runif(12,-0.1,0.1)
  
  
  points(myxs,
         myys,
         
         pch = 21, 
         bg=sample(colors()[c(1:151,362:657)],length(myxs)), 
         cex=runif(length(myxs),1,3))
  
  
  
  
  points(x=runif(1000,min=par('usr')[1],max=par('usr')[2]), 
         y=runif(1000,min=par('usr')[3],max=par('usr')[4]), 
         pch=8,
         cex=runif(1000,0.1,0.5),
         col="steelblue")
  
  
  title(main = expression(paste(Eta, alpha, rho, rho, psi,~Eta, omicron, zeta, iota, delta, alpha, psi, sigma)), font = 4,
        xlab = "Cody", line = 0)
  
  
  box("fig", lwd = 7, lty = sample(1:6,1))
  
  
  
  
  
}







## Saving pretty figures



png(filename = "holidaytree%03d.png", width = 5, height = 5, units = "in", res = 300, pointsize = 12, family = "sans")

for(i in 1:3){
  plot_My_Holiday_Tree(myseed=.Random.seed[i], main=paste(i))
}

dev.off()


svg(filename = "holidaytree%03d.svg", width = 5, height = 5, pointsize = 12, family = "sans")

for(i in 4:6){
  plot_My_Holiday_Tree(myseed=.Random.seed[i])
}

dev.off()











###tweaks
vplotMHT <- Vectorize("plot_My_Holiday_Tree")




## Arranging plots

### Regular Grid

par(mfrow=c(2,2))
vplotMHT(myseed=1:4)

dev.off()

### More complex

layout(mat = matrix(c(1,2,1,3,1,4), byrow = T, ncol=2), widths = c(3,1), heights = c(1,1,1))
layout.show(n=4)

vplotMHT(myseed=5:8)

dev.off()





