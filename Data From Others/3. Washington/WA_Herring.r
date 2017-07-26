## Plotting WA herring stock data-- all stocks.  
## Trying to make a spark plot... 5.8.2017.

##ENDED UP NOT USING THIS ONE

WAherring <- read.csv("C:/Users/jagil/Documents/School/699/Herring Data/Data From Others/3. Washington/WA_Herring.csv",
                      header = TRUE, sep=",")
WAherring
##Test to see if data is working the way I want
plot(x=WAherring$ï..YEAR, y=WAherring$SQUAXIN, type="l")
##Is this going to work? Plot them each individually then collate?
par(mfrow=c(4,1), mar=c(1,0,0,0), oma=c(4,5,4,4))
plot( x=WAherring$ï..YEAR, y=WAherring$SQUAXIN,
      axes=F, ylab="", xlab="", main="",
      ylim=c(0,3000), type="l"); axis(2, cex.axis=1)
plot( x=WAherring$ï..YEAR, y=WAherring$PURDY,
      axes=F, ylab="", xlab="", main="",
      ylim=c(0,3000), type="l"); axis(2, cex.axis=1)
plot( x=WAherring$ï..YEAR, y=WAherring$WOLLOCHET.BAY,
      axes=F, ylab="", xlab="", main="",
      ylim=c(0,3000), type="l"); axis(2, cex.axis=1)
plot( x=WAherring$ï..YEAR, y=WAherring$QM,
      ylab="", xlab="Year", main="",
      bty="n", xlim=c(1973,2016), ylim=c(0,3000), type="l"); axis(2, cex.axis=1)
axis(1, labels=TRUE)

## it's only sort of working like this... The x-axis won't go all the way from 1973-2016, 
## despite having that xlim on there for the last graph... Grr.

##Trying something new:
WAstocks <- read.csv("C:/Users/jagil/Documents/School/699/Herring Data/Data From Others/3. Washington/WA_Herring.dotchart.csv",
                     header=TRUE, sep=",")
WAstocks
library(ggplot2)
par(mfrow=c(1,1))
dotchart( x=as.matrix(WAstocks[,3]), labels=WAstocks[,2], groups=WAstocks[,1])



##SparkChart help from Michelle 

##MOVED TO OWN .r file ("sparkplots.r")
# WAspark <- read.csv("C:/Users/jagil/Documents/School/699/Herring Data/Data From Others/3. Washington/WA_Herring_spark.csv")
# head(WAspark)
# nrow(WAspark)
# ##getting rid of the first row of data
# index <- WAspark[1,] 
# SparkData2 <- WAspark[2:45,]
# head(SparkData2)
# SparkData3 <- SparkData2[,-1]
# ##from sparky.r
# #sparkStats<- t( apply( as.matrix(SparkData3) ,2,calcStats ) ) 
# plt.spark( x=c(1:nrow(SparkData3)),y=SparkData3,plt.lines,
#            sparkStats[,c("Min.","Mean","Max.")],
#            col="red", lwd=2,
#            labelPct=0.1, plotPct=0.3 )

