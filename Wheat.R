library(HistData)
library(plotrix)

data(Wheat)
data(Wheat.monarchs)

Wheat1 <- na.omit(Wheat) 

# Plot wheat price and years in line(stair steps)
plot(Wheat$Year, Wheat$Wheat, type="s", lwd=2,
     ylim=c(0,104), xlim=c(1565,1820),
     xlab="Year(1565-1821)",
     ylab="Price of Wheat (shillings)",
     main="Time series graph of prices, wages, and ruling monarch over a 250-year period",
     panel.first = grid(55,28,lty = 1,lwd = 2))
text(1650, 70,
     paste("Shewing at One View",
           "The Price of the Quarter of Wheat",
           "& Wages of Labor by the Week",
           "from the Year 1565 to 1821",
           "by William Playfair",
           sep="\n"), font=4)
for (i in 1:nrow(Wheat)){
  gradient.rect(Wheat$Year[i],0,Wheat$Year[i+1],Wheat$Wheat[i],
                col=smoothColors("grey",(Wheat$Wheat[i]-0)/5,"black"),
                gradient="y",border=NA)
}
# polygon(c(1565,Wheat$Year[Wheat$Year>=1565 & Wheat$Year<=1570],1570),
#         c(20,Wheat$Wheat[Wheat$Year==1565],Wheat$Wheat[Wheat$Year==1565],20), 
#         border =NA, col = brewer.pal(11, "BrBG")[3:8])

# Plot wages and years in line
lines(Wheat$Year, Wheat$Wages, lwd=2,col="white")
text(1640,10, "Weekly wages of a Good Mechanic(Shillings)", cex=1, font=4,col="white")
polygon(c(1565,Wheat1$Year,1810),c(0,Wheat1$Wages,0), border =NA, col = rgb(1,1,1,0.5))

# Plot monarchs on the top 
p <- ifelse((!seq_along(Wheat.monarchs$start) %% 2) & !Wheat.monarchs$commonwealth, 102, 103)
segments(Wheat.monarchs$start, p, Wheat.monarchs$end, p, 
         col = ifelse(Wheat.monarchs$commonwealth, "grey", "black"), 
         lwd = 10, lend=1)
text((Wheat.monarchs$start+Wheat.monarchs$end)/2, p-1.2, Wheat.monarchs$name, cex=0.7,font=2)
text(1575,106,"Ruling Monarch",cex=1,font=4)

#polygon(c(1565,Wheat$Year[Wheat$Year>=1565 & Wheat$Year<=1570],1570),c(20,Wheat$Wheat[Wheat$Year==1565],Wheat$Wheat[Wheat$Year==1565],20), border =NA, col = brewer.pal(11, "BrBG")[3:8])

#polygon(c(1565,Wheat$Year[Wheat$Year>=1565 & Wheat$Year<=1570],1570),c(20,Wheat$Wheat[Wheat$Year==1565],Wheat$Wheat[Wheat$Year==1565],20), border =NA, col = colfunc(10))
