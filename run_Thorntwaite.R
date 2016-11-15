#rm(list = ls())

# 1. Load data:
Tdata = read.table("metdata.txt",header = TRUE, sep = "")
day = Tdata[,c("day")]
Tair = Tdata[,c("tavg")]
year = Tdata[,c("year")]
data_tmdh = read.table("TMDH.txt",header = TRUE, sep = "")
TMDH = data_tmdh[,c("TMDH")]
month_tmdh = data_tmdh[,c("month")]

Ep = Thornthwaite(day, year, Tair, TMDH)

# Results: 

#plot graphs:
#plot(x, y1,y2,y3, ylim = range(c(y1,y2,y3)),xlim = range(x), type = "l", col = "red")
#lines(x,y2,col = "green")
#lines(x,y3,col = "blue")
#plot(month_tmdh[4:15], Ep[4:15])

#plot(x,y1, ylim = range(c(y1,y2,y3)))
#par(new = TRUE)
#plot(x,y2,ylim = range(c(y1,y2,y3)),axes = FALSE, xlab = "", ylab = "")
#dev.off()
#library(ggplot2)
#library(reshape2)

x = month_tmdh[4:15]
y1 = Ep[4:15]
y2 = Ep[28:39]
y3 = Ep[88:99]
#df = data.frame(x,y1,y2,y3)

plot(x, y1, type = "p", col = "red", xlab = "month", ylab = "Ep (cm)")
par(new = TRUE)
plot(x, y2, type = "p", col = "green")
par(new = TRUE)
plot(x, y3, type = "p", col = "blue")
# Create a title with a red, bold/italic font
title(main="Monthly potential ET for 2001, 2003, 2008", col.main="black", font.main=4)
legend("topleft", legend=c("2001", "2003", "2008"),
       col=c("red", "green", "blue"), pch = 1, cex=0.5)
#legend(2000,9.5, c("2001","2003","2008"),lty=c(1,1), lwd=c(2.5,2.5),col=c("red", "green", "blue"))

#df = data.frame(x = rep(x,3), y = c(y1,y2,y3))
  #ggplot(df, aes(x=x)) +
    #geom_line(aes(y = y1, colours = "red")) +
    #geom_line(aes(y = y2, colours = "blue"))+
    #geom_line(aes(y = y3, colours = "green")) +
    #geom_line()           
#require(ggplot2)
#ggplot(df, aes(x = x)) +

# 
# 
#g <- ggplot(df, aes(x))
#g <- g + geom_line(aes(y =y1), colour = "red")
#g <- g + geom_line(aes(y =y2), colour = "blue")
#g <- g + geom_line(aes(y =y3), colour = "green")
#g <- g + ylab("month") + xlab("Ep (cm)")
#g 

#df2 <- melt(data = df, id.vars = "x")
#ggplot(data = df2, aes(x = "month", y = "Ep (cm)", colour = variable)) + geom_line()
  
#plot(day[1:31], Ep,type = "l", col = "black", xlab = "dayes", ylab = "Ep (cm)")
# Create a title with a red, bold/italic font
#title(main="Monthly potential ET for 2001, 2003, 2008", col.main="black", font.main=4)
