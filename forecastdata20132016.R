setwd("~/RFB")
require(dplyr)
require(tidyr)

sales1<-read.csv2("ndc_sales1.csv", header = TRUE, sep = ";")
sales2<-read.csv2("SalesTransactions 20160702.csv", header = TRUE, sep = "|")
s1<-select(sales1,factuurdatum,aantal_producten,productCode,nettobedrag=netto)
s2<-select(sales2,factuurdatum,aantal_producten,productCode,nettobedrag)
## zet data goed

s2[ , 4] <- gsub(".", "", s2[ , 4],fixed=TRUE)
s2[ , 4] <- as.numeric(gsub(",", ".", s2[ , 4],fixed=TRUE))
## set NA aantal producten op 1 en datum in YMD format
s1$aantal_producten[is.na(s1$aantal_producten)]<-1       
s2$factuurdatum<-as.Date(s2$factuurdatum)
s1$factuurdatum<-as.Date(s1$factuurdatum)
s1<-s1[!is.na(s1$factuurdatum),]
sales<-rbind(s1,s2)


## Zet datum in juiste format
sales$factuurdatum<- as.Date(sales$factuurdatum, "%Y-%m-%d")
### Shorten data naar maanden
sales$maand = strftime(sales$factuurdatum, "%Y/%m")

sales <- sales[order(sales$factuurdatum), ]

### Gebruik alleen data van 1-1-2014 tot en met 1-7-2016
sales <- subset(sales, factuurdatum > "2014-1-1" & factuurdatum < "2016-7-1")
range(sales$factuurdatum)

### select data time frame ###
time <- subset(sales, factuurdatum > "2014-1-1" & factuurdatum < "2016-1-1")
timem = subset(sales, maand > "2014-1" & maand < "2016-1")


#monthorder <- lastYear[order(lastYear$datum), ]


## Aggregate and spread - maak df met producten in columns per DAG
td<-aggregate(time$aantal_producten,by=list(product=time$productCode,datum=time$factuurdatum),FUN = sum)
ppd<-spread(td,product,x)
ppd[is.na(ppd)]<-0

## Aggregate and spread - maak df met producten in columns per MAAND
tm<-aggregate(time$aantal_producten,by=list(product=time$productCode,datum=time$maand),FUN = sum)
ppm<-spread(tm,product,x)
ppm[is.na(ppm)]<-0

## gehele tijd
ta<-aggregate(sales$aantal_producten,by=list(product=sales$productCode,datum=sales$maand),FUN = sum)
ppa<-spread(ta,product,x)
ppa[is.na(ppa)]<-0

## make timeseries
k<-ts(pp[,2:83])

## Start with forecast
require(forecast)




## Voorspelde waarde komende maanden
mn = naive(k[,1],h=12,level=c(90,95),fan=FALSE,lambda=NULL) 
plot(mn) 
mf = meanf(k[,1],h=12,level=c(90,95),fan=FALSE,lambda=NULL)
md = rwf(k[,1],h=12,drift=T,level=c(90,95),fan=FALSE,lambda=NULL)


## Stationary of non-s
kpss = kpss.test(k[,1])

library(tseries)
adf = adf.test(k[,1])
adf


### Echte data januari tot juli 2016
lastmonths <- subset(sales, factuurdatum > "2016-1-1" & factuurdatum < "2016-7-1")
lastmonths$datum = strftime(lastmonths$factuurdatum, "%Y/%m")
lastmonthsorder <- lastmonths[order(lastmonths$datum), ]

t6<-aggregate(lastmonthsorder$aantal_producten,by=list(product=lastmonthsorder$productCode,datum=lastmonthsorder$datum),FUN = sum)
pp6<-spread(t6,product,x)
pp6[is.na(pp6)]<-0





## ggplot 
#reshape data
require(reshape)
pp1 <- melt(pp ,  id.vars = 'datum', variable.name = 'producten')
ggplot(df, aes(time,value)) + geom_line(aes(colour = series))
ggplot(pp1, aes(datum,value)) + geom_line(aes(colour = value))

ndiffs(k[,1])
[1] 1
diff_data = diff(k[,1])
diff_data

## auto arima

auto.arima(pp[,3])
forecast(auto.arima(k[,8]))
plot(forecast(auto.arima(k[,8])))

## omzet per maand by;
omzetpm<-as.data.frame(group_by(monthorder, datum) %>%
                         summarise(
                           totaalnetto=sum(nettobedrag),
                           totaal=n(),
                           gemnettopp=totaalnetto/totaal
                           
                           
                         ))
