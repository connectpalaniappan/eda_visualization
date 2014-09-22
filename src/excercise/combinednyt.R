setwd("C:/Pals/Graduate_Course/Subjects/Data_Intensive_Computing/Projects/Project1/Excercise_2/dds_ch2_nyt/")
nydata = list.files(path=getwd(), pattern="*.csv");
combData = read.csv(nydata[1])
combData$day <- 1
for (i in 2:length(nydata)) {
  temp <- read.csv(nydata[i])
  temp$day <- i
  combData <- rbind(combData, temp)
}

summary(combData)
length(combData$Age)


combData$age_group <- cut(combData$Age, c(-Inf,0,18,24,34,44,54,64,Inf))
combData$ctr = combData$Clicks/combData$Impressions
combDataClicksgz = subset(combData,Clicks>0)
combDataImpsgz = subset(combDataClicksgz,Impressions>0)
library("ggplot2")
ggplot(subset(combDataImpsgz,Age>0), aes(x =ctr, colour=age_group)) + geom_density() + 
  xlab('Clicks') + ggtitle('Clicks (Age Group Classification) For a Month') +
  ylab('Quantity')
combData$scode[combData$Impressions==0] <- "NoImps"
combData$scode[combData$Impressions >0] <- "Imps"
combData$scode[combData$Clicks > 0] <- "Clicks"
head(combData)
library("doBy")
combData$scode <- factor(combData$scode)
combData$genExp = factor(combData$Gender, labels=c("Female","Male"))
combData$signedInExp = factor(combData$Signed_In, labels=c("Not LoggedIn","LoggedIn"))
clen <- function(x){c(length(x))}
sumCombData <- summaryBy(Impressions~scode+Gender+age_group+day, data = subset(combData,Age>0 & Age<18), FUN=clen)
#ggplot(subset(combData,Age>0 & Age<18), aes(x=genExp, fill=scode)) + geom_histogram(binwidth=1)
grpCombData <- cbind(subset(sumCombData, scode=="Clicks")[c(-1)],subset(sumCombData, scode=="Imps")[c(-1:-4)],subset(sumCombData, scode=="NoImps")[c(-1:-4)])
colnames(grpCombData)[4] <- "clicks"
colnames(grpCombData)[5] <- "Imp"
colnames(grpCombData)[6] <- "NoImp"

maleGrpCombData <- subset(grpCombData, Gender==1)
femaleGrpCombData <- subset(grpCombData, Gender==0)
colors <- c("red", "darkblue","skyblue")

par(mfrow=c(2, 1),oma = c(0, 0, 0, 2))
barplot(rbind(femaleGrpCombData$clicks, femaleGrpCombData$Imp, femaleGrpCombData$NoImp),
        beside = TRUE, names.arg = femaleGrpCombData$day, cex.names = 0.40, 
        xlab="Days", ylab= "Density", col=colors,
                          main = "Clicks vs Imps vs No Imps (DayWise- Females)"
)  

barplot(rbind(maleGrpCombData$clicks, maleGrpCombData$Imp, maleGrpCombData$NoImp),
        beside = TRUE, names.arg = maleGrpCombData$day, cex.names = 0.40, 
        xlab="Days", ylab= "Density", col=colors,
        main = "Clicks vs Imps vs No Imps (DayWise - Males)"
)
par(xpd=NA)
legend(x=-29.0,y=1000000, 
       legend = c("Clicks", "Imps","NoImps"), 
       fill = colors,
       bty="n")

#summaryBy(Impressions~scode+signedInExp, data = nydata, FUN=clen)
####################################################3

sumCombData <- summaryBy(Impressions~scode+signedInExp+age_group+day, data = combData, FUN=clen)
#ggplot(subset(combData,Age>0 & Age<18), aes(x=genExp, fill=scode)) + geom_histogram(binwidth=1)
grpCombData <- cbind(subset(sumCombData, scode=="Clicks")[c(-1)],subset(sumCombData, scode=="Imps")[c(-1:-4)],subset(sumCombData, scode=="NoImps")[c(-1:-4)])
colnames(grpCombData)[4] <- "clicks"
colnames(grpCombData)[5] <- "Imp"
colnames(grpCombData)[6] <- "NoImp"

maleGrpCombData <- subset(grpCombData, Gender==1)
femaleGrpCombData <- subset(grpCombData, Gender==0)
colors <- c("red", "darkblue","skyblue")

par(mfrow=c(2, 1),oma = c(0, 0, 0, 2))
barplot(rbind(femaleGrpCombData$clicks, femaleGrpCombData$Imp, femaleGrpCombData$NoImp),
        beside = TRUE, names.arg = femaleGrpCombData$day, cex.names = 0.40, 
        xlab="Days", ylab= "Density", col=colors,
        main = "Clicks vs Imps vs No Imps (DayWise- Females)"
)  

barplot(rbind(maleGrpCombData$clicks, maleGrpCombData$Imp, maleGrpCombData$NoImp),
        beside = TRUE, names.arg = maleGrpCombData$day, cex.names = 0.40, 
        xlab="Days", ylab= "Density", col=colors,
        main = "Clicks vs Imps vs No Imps (DayWise - Males)"
)
par(xpd=NA)
legend(x=-29.0,y=1000000, 
       legend = c("Clicks", "Imps","NoImps"), 
       fill = colors,
       bty="n")
