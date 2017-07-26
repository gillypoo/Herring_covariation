##BC biomass data
Herring <- read.csv("C:/Users/jagil/Documents/School/699/Herring Data/Published Stock Assessments/2.British Columbia/2015_BC_StockAssessment.csv",
                    header = TRUE,sep = ",")
Herring
plot(Herring$ï..Year, Herring$Totals/1000, 
     type="l", las=1, 
     ylab="Biomass ('000s tons)", xlab="Year")

##Stocks across Pacific 
Stocks <- read.csv("C:/Users/jagil/Documents/School/699/Herring Data/Me playing around/Current_Available_Herring2.csv")
Stocks
#plot(Stocks$ï..Stock,Stocks$MT/1000, type="o", xlab="Stock Number (North to South)", 
 #    ylab="Stock Biomass ('000s tons)", las=1)
#legend(12,150, c("1-6: Alaska","7-11: British Columbia",
 #                  "12-15: Washington", "16: Oregon",
 #                 "17-19: California", bty="o") 


par(xpd=FALSE)
par(mfrow=c(1,1),mar=c(5,4,4,2)+0.1)
dotchart( x= as.matrix(Stocks[,3])/1000, labels=Stocks[,1],
          groups=Stocks[,2],
          cex=1, pch=19,
          gcolor="blue",
          main="Pacific herring biomass", 
          xlab="Biomass ('000s MT)" )
#  mtext("Biomass ('000 MT)",side=1, line=1, adj=0.65, outer=TRUE)          

