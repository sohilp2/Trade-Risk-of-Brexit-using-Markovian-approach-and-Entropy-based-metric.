library(readxl)
library(MASS)
library(xlsx)
setwd("E:/Texas A&M/660- QRA/Brexit/data/Product 29-END")
rawdata <- read.csv("58Publish-Import-2013.csv", header = TRUE)
mat<- as.data.frame(rawdata)
summary(mat)
#Matrix by d
levels(mat$REPORTER) <- 1:28
levels(mat$PARTNER) <- 1:28
P=matrix(data = 0, nrow = 28, ncol = 28)
ratio=matrix(data = 0, nrow = 28, ncol = 28)
trade_zi=matrix(data = 0, nrow = 28, ncol = 28)
uplim <- dim(mat)[1]
for (i in 1:uplim) {
  k=mat$REPORTER[i]
  for (j in 1:uplim) {
    l=mat$PARTNER[j]
    if(i==j){
      P[l,k]=mat$VALUE_IN_EUROS[i]
      }
  }
}
for (i in 1:28) {
  ratio[i,]=P[i,]/sum(P[i,])
}
# steady state eigenvalue
eig=eigen(ratio)
rvect=eig$vectors
lvect=ginv(eig$vectors)
lam<-eig$values
rvect%*%diag(lam)%*%ginv(rvect)
Pinf_1<-Re(lvect[1,]/sum(lvect[1,]))


for (i in 1:28) {
  for (j in 1:28) {
    trade_zi[i,j] <- P[i,j]*sum(P)/(sum(P[i,]*sum(P[,j])))
   }
}

#risk of portfolio
inequality_vector <- matrix(0, 28, 1)
for(i in 1:28){
  for (j in 1:28){
    inequality_vector[i] <- inequality_vector[i]+ ((trade_zi[i,j]-1)^2)/28
  }
}
datacombine <- data.frame(t(sort(unique(rawdata$REPORTER))),Pinf_1, inequality_vector)
write.xlsx(datacombine, "E:/Texas A&M/660- QRA/Brexit/data/Product 29-END/Results/R58Publish-Import-2013.xlsx")
