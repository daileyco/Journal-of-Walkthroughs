# Map with arrows connecting two places





# 
# ft.ew <- full_join(
#   full_join(
#     b.ew, 
#     cbind(mp = {wa.ew$mp.ew%>%toupper()}[which(wa.ew$has.seqs==1)], wa.ew.centroids.coords)%>%as.data.frame(), 
#     by = c("From" = "mp")),
#   cbind(mp = {wa.ew$mp.ew%>%toupper()}[which(wa.ew$has.seqs==1)], wa.ew.centroids.coords)%>%as.data.frame(), 
#   by = c("To" = "mp"), 
#   suffix = c(".from", ".to")
# ) %>% 
#   filter(BAYES_FACTOR>3)
# ft.ew$med.lwd <- if(is.na(sd(ft.ew$median))){
#   4
# }else{
#   ((ft.ew$median-mean(ft.ew$median))/sd(ft.ew$median)*2)+abs(min((ft.ew$median-mean(ft.ew$median))/sd(ft.ew$median)*2))+1
# }        
# 
# 
# 
# library(diagram)
# 
# 
# png(filename = "./output/sequence_sampling/metapop_ew_tr2.png", height = 5.63, width = 10, units = "in", res = 300, pointsize = 12, family = "sans")
# 
# layout(matrix(1:2, ncol = 2), widths = c(1, lcm(3)))
# par(mar=c(0,0,0,0))
# plot(wa.ew$geometry, border="grey")
# # plot(wa.ew["mp.ew"], col=viridis(2), add=T, border = "grey", lwd = 2.5)
# plot(wa.ew$geometry[which(wa.ew$mp.ew=="West")], col = viridis(2)[1], add = TRUE, lwd = 2.5, border = "grey")
# plot(wa.ew$geometry[which(wa.ew$mp.ew=="East")], col = viridis(2)[2], add = TRUE, lwd = 2.5, border = "grey")
# plot(metapop.categories$geometry[which(!metapop.categories$county2%in%metadata$county)], col = rgb(220/255, 220/255, 220/255, alpha = 1), add = TRUE, lwd = 2.5, border = "grey")
# 
# plot(wa.ew.centroids, pch=19, col="white", add=T, cex=2)
# 
# # points(wa.ew.centroids.coords[1,1], wa.ew.centroids.coords[1,2], col = "white", pch = 19, cex = 2)
# # points(wa.ew.centroids.coords[1,1], wa.ew.centroids.coords[1,2], col = viridis(2)[2], pch = 19, cex = 1)
# # 
# # points(wa.ew.centroids.coords[2,1], wa.ew.centroids.coords[2,2], col = "white", pch = 19, cex = 2)
# # points(wa.ew.centroids.coords[2,1], wa.ew.centroids.coords[2,2], col = viridis(2)[1], pch = 19, cex = 1)
# 
# curvedarrow(ft.ew[,which(grepl(".from", names(ft.ew)))]%>%as.numeric(), ft.ew[,which(grepl(".to", names(ft.ew)))]%>%as.numeric(), lwd = ft.ew$med.lwd, lty = 1, lcol = "white", curve = 0.1, arr.pos = 0.8, segment = c(0.2, 0.8))
# par(mar=c(0,0,0,2.1))
# .image_scale_factor(c("", "", ""), col = c(viridis(2), rgb(220/255, 220/255, 220/255, alpha = 1)), key.length = lcm(2), key.width = lcm(3), key.pos = 4)
# text(x=c(2,2,2), y=c(1, 2, 3), labels = c("West", "East", "No Seqs"), xpd=TRUE, cex=10/12, adj = 0)
# 
# 
# dev.off()

