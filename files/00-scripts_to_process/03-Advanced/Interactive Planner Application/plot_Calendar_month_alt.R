
plot.new()
plot.window(xlim = c(1,8), ylim = c(1,6))

x=c(1,2,2,1)
y=c(6,6,5,5)

polygon(x,y)
polygon(x+1, y)
polygon(x, y-1)


shift.x=rep(0:6, 5)
shift.y=rep(0:4, each=7)

grid.cells <- list(list(x=x, y=y))

for(i in 1:5){
  for(ii in 1:7){
    grid.cells[[(i-1)*7+ii]] <- list(x=x+shift.x[(i-1)*7+ii], y=y-shift.y[(i-1)*7+ii])
  }
}

par(mar = c(0.1,0.1,3.1,0.1), oma = c(1.1,1.1,1.1,1.1))
plot.new()
plot.window(xlim = c(1,8), ylim = c(1,6))

# box("plot")
# box("outer")
box("inner")

title(main = "JANUARY")


lapply(grid.cells, function(.gc){polygon(.gc$x, .gc$y)})

lapply(1:35, function(.gc){text(grid.cells[[.gc]]$x[1], grid.cells[[.gc]]$y[1], adj = c(-0.2,1.2), labels = .gc)})








the.dates <- seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by="days")

all.days.2021 <- data.frame(Date=the.dates, Weekday=weekdays(the.dates), Month=months(the.dates))



classic.week <- seq(as.Date("2021-03-01"), as.Date("2021-03-07"), by="days")

weekday.relational <- data.frame(Weekday=weekdays(classic.week), Day.code=1:7)



current.month <- months(Sys.Date())
current.month <- "January"
current.month <- "February"


current.month.days <- all.days.2021[which(all.days.2021$Month==current.month),]
current.month.days <- merge(current.month.days, weekday.relational, by = "Weekday", sort = F)
current.month.days <- current.month.days[order(current.month.days$Date),]

x.start <- current.month.days$Day.code[1]
x.end <- current.month.days$Day.code[nrow(current.month.days)]

# current.month.grid <- grid.cells[x.start:{length(grid.cells)-(7-x.end)}]
current.month.grid <- grid.cells[x.start:{x.start+nrow(current.month.days)-1}]



par(mar = c(0.1,0.1,3.1,0.1), oma = c(1.1,1.1,1.1,1.1))
plot.new()
plot.window(xlim = c(1,8), ylim = c(1,6))

# box("plot")
# box("outer")
box("inner")

title(main = current.month)
text(x=2:8-0.5, y=6, adj = c(0.5, -0.2), labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

lapply(1:length(current.month.grid), function(.index){polygon(current.month.grid[[.index]]$x, current.month.grid[[.index]]$y); text(current.month.grid[[.index]]$x[1], current.month.grid[[.index]]$y[1], adj = c(-0.2,1.2), labels = .index)})


# how to highlight important days
## with some kind of color coding? shading? hashing? symbols?

#useful to save png to print? 
##import some kind of list of dates to show


































