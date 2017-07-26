compare<-read.csv("C:/Users/jagil/Documents/School/699/Herring Data/Comparisons.csv",stringsAsFactors = FALSE)
colnames(compare)<-c("year","sitka","sfbay")
compare
plot(compare$sitka)
compare$sitka
plot(compare$year,compare$sitka,las=1,type="l")
plot(compare$year,compare$sfbay,las=1,type="l")
linmod<-lm(compare$sitka~compare$sfbay)
plot(linmod)
