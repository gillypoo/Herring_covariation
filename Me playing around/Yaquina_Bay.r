Yaquina <- read.csv("C:/Users/jagil/Documents/School/699/Herring Data/Me playing around/Yaquina_Bay_Proc_dev.csv")
plot(x=Yaquina$ï..Year, y=Yaquina$proc_dev_yr1,
     type="l", col="red", lwd=2 )
abline(h=0.0, col="grey")
PDO <- read.csv(file="C:/Users/jagil/Documents/School/699/Oceanographic Data/PDO Data/3.29.17 PDO Data.csv",
                header=TRUE,sep = "")
Means <- rowMeans(PDO[2:13])
Means
par(mfrow=c(1,1))
par( new=TRUE )
plot( x=PDO$YEAR, y=Means, 
      type="l", axes=FALSE, bty="n",
      xlab="", ylab="", col="blue", lwd=2)
axis( side=4, ylim=c(-2,2), las=1, col="blue")
mtext("PDO", side=4, line = 1.5, col="blue")
legend(1965,2,c("Herring", "PDO"), lty=c(1,1), 
       lwd=c(2.5,2.5), col=c("red","blue"), bty="n" )
