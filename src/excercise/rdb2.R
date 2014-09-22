install.packages("gdata")
require(gdata)
#PATTERN --> searches for the particular word and start reading from it
setwd("C:/Pals/Graduate_Course/Subjects/Data_Intensive_Computing/Projects/Project1/Excercise_2/dds_ch2_rollingsales/")
bk <- read.xls(xls="rollingsales_manhattan.xls",perl="C:/Perl64/bin/perl.exe",pattern="BOROUGH") 
head(bk)
summary(bk)
bk$SALE.PRICE.N <- as.numeric(gsub("[^[:digit:]]","",bk$SALE.PRICE))
#count(is.na(bk$SALE.PRICE.N))
#length(is.na(bk$SALE.PRICE.N)==TRUE)
#length(subset(bk,is.na(bk$SALE.PRICE.N)==TRUE))
bk$SALESBOOL = is.na(bk$SALE.PRICE.N)
#bk$SALESVALID = factor(bk$SALESBOOL, labels=c("TRUE","FALSE"))
count = function(x) {c(count=length(x))}
summaryBy(SALESBOOL~SALESBOOL, data = bk, FUN=count)
names(bk) <-tolower(names(bk))
bk$gross.sqft <- as.numeric(gsub("[^[:digit:]]","",bk$gross.square.feet))
bk$land.sqft <- as.numeric(gsub("[^[:digit:]]","",bk$land.square.feet))
bk$sale.date <- as.Date(bk$sale.date)
bk$yer.built <- as.numeric(as.character(bk$year.built))
#class(bk$year.built)
head(bk)
attach(bk)
summary(bk$sale.price.n)
hist(sale.price.n, plot=TRUE)
summaryBy(sale.price.n~1, data = subset(bk,sale.price.n>0 & sale.price.n<50000000), FUN=count)
hist(sale.price.n[sale.price.n>0], plot=TRUE,axes=TRUE)
hist(gross.sqft[sale.price.n==0], plot=TRUE)
detach(bk)
bk$salepricegrp = cut(bk$sale.price.n, c(-Inf,0,100000,200000,300000,400000,500000,600000,700000,800000,900000,1000000,Inf))
ggplot(subset(bk), aes(x= salepricegrp)) + geom_histogram()

bk.sale <- bk[bk$sale.price.n!=0,]
plot(bk.sale$gross.sqft,bk.sale$sale.price.n)
plot(log(bk.sale$gross.sqft),log(bk.sale$sale.price.n))



