#1
install.packages("timeSeriesDataSets")  # Instalujemy pakiet z danymi
library(timeSeriesDataSets)
data(milk_ts)

is.ts(milk_ts)
summary(milk_ts)

#2
plot(milk_ts, main = "Miesięczna produkcja mleka w latach '62-'75", 
     xlab = "Rok",ylab="Funty")

#3
install.packages("forecast")
library(forecast)
monthplot(milk_ts,main="Wykres sezonowości produkcji mleka - miesiące")
seasonplot(milk_ts,col=rainbow(13),
           main="Wykres sezonowości produkcji mleka",
           year.labels=TRUE,pch=19)

tsdisplay(milk_ts,main="Wykres szeregu, ACF i PACF" )
Acf(milk_ts,lag.max=75,main="Wykres Acf") 
Pacf(milk_ts,lag.max=75,main="Wykres Pacf")

#4
srednia=365.25/12
liczba=monthdays(milk_ts)
milk.adj=milk_ts/liczba*srednia
ts.plot(milk_ts,milk.adj,main="Szereg-skorygowanie",
        col=c("black","green"),lty=c(1,2))
legend("bottomright",legend=c("dane oryginalne", "dane skorygowane"),
       col=c("black","green"),lty=c(1,2))
x =milk.adj - milk_ts


milk.sqrt=BoxCox(milk.adj,lambda= 0.7847705)
milk.log=BoxCox(milk.adj,lambda=0)
plot(milk.adj,main="Dane oryginalne")
plot(milk.sqrt,main="Lambda opytmalna")

milk.diff.trend=diff(milk.adj,differences=1)
tsdisplay(milk.diff.trend,main="Różnicowanie trendu")
milk.diff.sez=diff(milk.adj,lag=12)
tsdisplay(milk.diff.sez,main="Różnicowanie sezonowości")
milk.diff=diff(diff(milk.adj, lag=12),differences=1)
tsdisplay(milk.diff, main="Różnicowanie trendu i sezonowości")

#5
#ruchoma średnia
m21=stats::filter(milk_ts,filter=rep(1/21,21),sides=2)
m3=stats::filter(milk_ts,filter=rep(1/3,3),sides=2)
m11=stats::filter(milk_ts,filter=rep(1/11,11),sides=2)

plot(milk_ts,main="Metoda ruchomej średniej") 
lines(m3,col="blue",lty=2) 
lines(m11,col="red",lty=2)
lines(m21,col="green",lty=2)
legend("bottomright",legend =c("wyjściowy szereg", "ruchoma średnia rzędu 3", "ruchoma średnia rzędu 11","ruchoma średnia rzędu 21"), col=c("black","blue","red",  "green"),lty=c(1,2,2,2))

#ważonej ruchomej średniej
spenser=stats::filter(milk_ts,filter=(1/320)*c(-3,-6,-5,3,21,46,67,74,67,46,21,3,-5,-6,-3),sides=2)
plot(milk_ts)
lines(spenser,col="red",lty=2)

#wygładzania wykładniczego
milk.1=ses(milk_ts,alpha=0.5,initial="simple") 
milk.2=ses(milk_ts,alpha=0.2,initial="simple") 
milk.3=ses(milk_ts,alpha=0.2,initial="optimal")

milk.close.1=stats::lag(fitted(milk.1))
milk.close.2=stats::lag(fitted(milk.2))
milk.close.3=stats::lag(fitted(milk.3))

plot(milk_ts,main="Wygładzanie wykładnicze")
lines(milk.close.1,col="red",lty=2) 
lines(milk.close.2,col="blue",lty=2)
lines(milk.close.3,col="green", lty=2)
legend("bottomright",legend= c("wyjściowy szereg","alpha= 0.5","alpha=0.2",
                               "alpha=0.2, warunek początkowy = optimal"),
       col=c("black","red","blue", "green"),lty=c(1,2,2,2))

#dekompozycja addytywna
milk.ad=decompose(milk_ts,type="additive") 
plot(milk.ad) 
milk.ad.trend=milk.ad$trend  
milk.ad.sez=milk.ad$seasonal  
milk.ad.ind.sez=milk.ad$figure  
milk.ad.reszty=milk.ad$random 

barplot(milk.ad.ind.sez,names.arg=month.abb, main="indeksy sezonowe") 
tsdisplay(milk.ad.reszty,main="reszty") 

#dekompozycja multiplikatywna
milk.mul=decompose(milk_ts,type="multiplicative") 
plot(milk.mul) 
milk.mul.trend=milk.mul$trend  
milk.mul.sez=milk.mul$seasonal  
milk.mul.ind.sez=milk.mul$figure  
milk.mul.reszty=milk.mul$random 

barplot(milk.mul.ind.sez,names.arg=month.abb, main="indeksy sezonowe") 
tsdisplay(milk.mul.reszty,main="reszty") 

#trend liniowy
milk.tlsm.trend= tslm(milk_ts~trend)
summary(milk.tlsm.trend)
plot(milk_ts,main="Trend liniowy")
lines(fitted(milk.tlsm.trend), col="red",lty=2)
legend("bottomright",legend =c("oryginalne dane","trend liniowy"),
       col=c("black", "red"),lty=c(1,2))
tsdisplay(residuals(milk.tlsm.trend),main="reszty")

# uwzględniamy sezonowość
milk.tlsm.trend.sez=tslm(milk_ts~trend+season) 
summary(milk.tlsm.trend.sez)
plot(milk_ts,main="Metoda 2") 
lines(fitted(milk.tlsm.trend.sez),col="red",lty=2) 
legend("bottomright",legend=c("oryginalne dane", "trend+sezonowość"),col=c("black","red"),lty=c(1,2)) 
tsdisplay(residuals(milk.tlsm.trend.sez),main="reszty")
