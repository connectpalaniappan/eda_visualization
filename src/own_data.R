#Authors
#Palaniappan Meiyappan       
#UB# 50097597  email : palaniap@buffalo.edu
#Arun Nagendran A M	          
#UB# 50096549  email : arunnage@buffalo.edu


#####################################################
#install Libraries

install.packages("gridBase")
install.packages("maps")
install.packages("mapproj")
library(gridBase)
library(doBy)
library(cluster)
library(rgeos)
library(maptools)
library("ggplot2")
library("doBy")
require(maps)


#####################################################
#Import data and check
allcrimedata = read.csv(file.choose());

crimedata = subset(allcrimedata, instsize>0)

summary(crimedata$stabbr)
head(crimedata)


crimedata$crimeSum <- rowSums(crimedata[15:92])
summary(crimedata$crimeSum)
crimedata$crimeSum
  
stateWiseCrimes <- summaryBy(crimeSum+instsize~stabbr, data=crimedata, FUN=sum);


#####################################################
#criminal cases life threatenig vs property damage

crimedata$lifeThreatening <- rowSums(cbind(crimedata[33:36],crimedata[38],
                                           crimedata[42:45],crimedata[47],
                                           crimedata[51:54],crimedata[56],
                                           crimedata[60:63],crimedata[65],
                                           crimedata[69:72],crimedata[74]
                                           ))
crimedata$propertyDamages <- rowSums(cbind(crimedata[33:77])) - crimedata$lifeThreatening
summary(crimedata$propertyDamages)
crimeLvsP <- summaryBy(propertyDamages+lifeThreatening~OBEREG, data=crimedata, FUN=sum);
summary(crimeLvsP)

par(mfrow=c(1, 4),oma = c(0, 0, 0, 2))
plot(crimeLvsP$OBEREG, crimeLvsP$lifeThreatening.sum/(crimeLvsP$lifeThreatening.sum+crimeLvsP$propertyDamages.sum)
     , xlab = "Geographical Regions", ylab = "Lifethreatening Crimes : Total Crimes Ratio")
axis(1, 0:9)
plot(crimeLvsP$OBEREG, crimeLvsP$lifeThreatening.sum
     , xlab = "Geographical Regions", ylab = "Total Life threatening Crimes")
axis(1, 0:9)
plot(crimeLvsP$OBEREG, crimeLvsP$propertyDamages.sum
     , xlab = "Geographical Regions", ylab = "Property Damage Crimes")
axis(1, 0:9)
par(xpd=NA)
mtext('Comparison of Life Threathening and Property Related Crimes (Region Wise)', outer=T, line=-1)
legend(x=9.6,y=150000,legend = c("Y-Axis Labels",
"0 - US Service schools",
"1 - New England",
"2 - Mid East",
"3 - Great Lakes",
"4 - Plains",
"5 - Southeast",
"6 - Southwest",
"7 - Rocky Mountains",
"8 - Far West",
"9 - Outlying areas"
), 
  bty="n")

#########################################################
#Arrests vs Disciplinary
crimedata$totalArrests <- rowSums(crimedata[18:32])
crimedata$totalDisciplinary <- rowSums(crimedata[78:92])

plot(crimedata$totalArrests,crimedata$totalDisciplinary)

plot(log(crimedata$totalArrests),log(crimedata$totalDisciplinary))

validCrime = subset(crimedata,totalArrests>0 & totalDisciplinary>0)



m = qplot(log(totalArrests),log(totalDisciplinary),data=validCrime, 
           geom=c("point", "smooth"), method="lm", formula=y~ns(x, 2))
m + scale_x_continuous(breaks=c(seq(0,10,by=1)))
m + scale_y_continuous(breaks=c(seq(0,10,by=1)))
m + xlab('Log of Total Arrest') + ggtitle('Scatter Plot of Disciplinary Action vs Arrests') +
  ylab('Log of Disciplinary Action')



##############################################################
# Campus Vs Non Campus - Time Series plot

crimedata$onCampus <- rowSums(cbind(crimedata[24:26],crimedata[51:59],crimedata[84:86]))
crimedata$onCampusShf <- rowSums(cbind(crimedata[27:29],crimedata[60:68],crimedata[87:89]))
crimedata$nonCampus <- rowSums(cbind(crimedata[21:23],crimedata[42:50],crimedata[81:83]))

TimeSeriesCrimes <- summaryBy(onCampus+nonCampus+onCampusShf~surveyyear, data=crimedata, FUN=mean);


ggplot(data=TimeSeriesCrimes, aes(x = surveyyear, color ="Crimes Occured at." )) + 
  geom_line(aes( y = onCampus.mean, color = "On Campus"), size=2) +
  geom_line(aes( y = onCampusShf.mean, color = "On Campus Housing Facility"), size=2) +
  geom_line(aes( y = nonCampus.mean, color = "Off Campus"), size=2)  +

  xlab('surveyyear') + ggtitle('Time Series of US State Crimes') +
  ylab('Crimes (Avg.)') + scale_x_continuous(breaks=c(seq(2001,2012,by=1)))

#########################################################
#Statewise vs no of crimes vs year

StateTimeSeriesCrimes <- summaryBy(crimeSum~surveyyear+stabbr, data=crimedata, FUN=sum);

ggplot(data=subset(StateTimeSeriesCrimes,surveyyear>2009)  , 
       aes(x=stabbr, y=crimeSum.sum, fill=factor(crimeSum.sum))) + 
  geom_bar(stat="identity") + coord_flip() + 
  facet_wrap( ~ surveyyear, ncol=3) + theme(legend.position="none") + 
  ggtitle("StateWise Crime distribution for years 2010-12") +
  ylab(c("Survey Year"))  + xlab(c("States in US"))

#########################################################

#statewise fires and live saved
crimedata$TotalFireAccidents <- crimedata$Firerelateddeaths+crimedata$Firerelatedinjuries

StateFires <- summaryBy(TotalFires+TotalFireAccidents~stabbr, data=subset(crimedata,TotalFires>0), FUN=sum);

colors <- c("red", "darkblue")
StateFiresPlot <- barplot(rbind(StateFires$TotalFires.sum,StateFires$TotalFireAccidents.sum),
        beside = TRUE,  names.arg = StateFires$stabbr, cex.names = 0.40, col=colors,
        xlab="State in US", ylab= "Number of Fires",
        main = "Fires in US Institutions StateWise (2010-12)"
        )  
legend("topright", 
       legend = c("Total Fires", "Lifes Lost in Fires"), 
       fill = colors,
       bty="n")


#########################################################

#Sectorwise crimes

sectorSummary <- summaryBy(crimeSum~sector, data=crimedata, FUN=sum);


desc <- c(
  "Public, 4-year or above",
  "Private not-for-profit, 4-year or above",
  "Private for-profit, 4-year or above",
  "Public, 2-year",
  "Private not-for-profit, 2-year",
  "Private for-profit, 2-year",
  "Public, less-than 2-year",
  "Private not-for-profit, less-than 2-year",
  "Private for-profit, less-than 2-year"
)

colors <- c("red", "grey70", "grey30","white","#76ED66","#B4F5AB","skyblue","#CC0000","#660000")
par(mai = c(0,0,0,0))
layout(c(1,2),heights=c(0.3,1))
plot.new()
pie(sectorSummary$crimeSum.sum, col=colors, labels = sectorSummary$sector, main="Pie Chart of Sectors");
legend("left", legend = desc, fill=colors, bty="n")

#########################################################
#Police Action Density in Boc Plot
crimedata$localpoliceaction <- rowSums(cbind(crimedata[18:20],crimedata[33:41],crimedata[78:80]))

ggplot(subset(crimedata,crimeSum>0 & localpoliceaction > 0), 
       aes(x=factor(stabbr), y=localpoliceaction/crimeSum, fill=factor(stabbr)), 
       ) +geom_boxplot() +
     labs(x = "State in US", 
          y = "Police Action(Density)", 
          title = "Statewise density of Police Action in Student Crimes")
