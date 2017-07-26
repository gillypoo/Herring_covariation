###########################################################
#     MDS playing around, code from:                      #
#     http://www.statmethods.net/advstats/mds.html        #
#     July 6, 2017                                        #
###########################################################


# Classical MDS
# N rows (objects) x p columns (variables)
# each row identified by a unique row name


setwd("C:/Users/jagil/Documents/School/699/Herring Data/Figures")
biomass <- read.csv("C:/Users/jagil/Documents/School/699/Herring Data/Figures/Biomass_all.csv")

d <- dist(biomass) # euclidean distances between the rows
fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
fit # view results

# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Metric	MDS",	type="n")
text(x, y, labels = row.names(biomass), cex=.7)

# Nonmetric MDS
# N rows (objects) x p columns (variables)
# each row identified by a unique row name

library(MASS)
d2 <- dist(biomass) # euclidean distances between the rows
##Having some errors here!!

fit2 <- isoMDS(2, biomass) # k is the number of dim
fit2 # view results

# plot solution 
x <- fit2$points[,1]
y <- fit2$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Nonmetric MDS", type="n")
text(x, y, labels = row.names(biomass), cex=.7)