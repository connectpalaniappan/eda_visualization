nydata = read.csv(file.choose());
nydata$age_group <- cut(nydata$Age, c(-Inf,0,18,24,34,44,54,64,Inf))
nydata$ctr = nydata$Clicks/nydata$Impressions
nydataClicksgz = subset(nydata,Clicks>0)
nydataImpsgz = subset(nydataClicksgz,Impressions>0)
install.packages("ggplot2")
library("ggplot2")
library("doBy")
ggplot(nydataClicksgz, aes(x =ctr, colour=age_group)) + geom_density() + 
  xlab('Clicks') + ggtitle('Clicks (Age Group Classification)') +
  ylab('Quantity')
nydata$scode[nydata$Impressions==0] <- "NoImps"
nydata$scode[nydata$Impressions >0] <- "Imps"
nydata$scode[nydata$Clicks > 0] <- "Clicks"
head(nydata)
nydata$scode <- factor(nydata$scode)
nydata$genExp = factor(nydata$Gender, labels=c("Female","Male"))
nydata$signedInExp = factor(nydata$Signed_In, labels=c("Not LoggedIn","LoggedIn"))
clen <- function(x){c(length(x))}
summaryBy(Impressions~scode+Gender+age_group, data = subset(nydata,Age>0 & Age<18), FUN=clen)
ggplot(subset(nydata,Age>0 & Age<18), aes(x=genExp, fill=scode)) +
geom_histogram(binwidth=1) + 
  xlab('Sex') + ggtitle('Male vs Female Clicks') +
  ylab('Clicks')
summaryBy(Signed_In~age_group+Signed_In,data = nydata, FUN=clen)
summaryBy(Impressions~scode+signedInExp, data = nydata, FUN=clen)
ggplot(subset(nydata), aes(x=signedInExp, fill=scode)) + geom_histogram(binwidth=1) + 
  xlab('User Status') + ggtitle('Loggedin vs NotLoggedin Clicks') +
  ylab('Clicks')
rangesummarize <- function(x){c(sum=sum(x),len=length(x), min=min(x), mean=mean(x), median=median(x), max=max(x), var=var(x), quant=quantile(x))}



