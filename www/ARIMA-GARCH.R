## Author: Rainer Stollhoff
## Datum: 14.12.2020
## Titel: Skript zur Analyse von Finanzzeitreihen mit ARIMA-GARCH Modellen


# Pakete einbinden
library(Quandl)
library(xts)
library(forecast)


# Datensatz laden

sap_ql <- Quandl("FSE/SAP_X",type="xts")
sap_ql <- sap_ql$Close
sap_ql_2017 <- sap_ql["2012"]


# Analyse

## Manuelle

### Box-Jenkins

#### Linearer Trend

plot(sap_ql_2017)
sap_ql_2017_data <- data.frame("Close" = sap_ql_2017$Close,"t"=time(sap_ql_2017))
sap_ql_2017_lm <- lm(formula = Close ~ t, data = sap_ql_2017_data)
summary(sap_ql_2017_lm)

sap_ql_2017_d <- diff(sap_ql_2017,na.pad=F)
plot(sap_ql_2017_d)
sap_ql_2017_data_d <- data.frame("Close" = sap_ql_2017_d$Close,"t"=time(sap_ql_2017_d))
sap_ql_2017_lm_d <- lm(formula = Close ~ t, data = sap_ql_2017_data_d)
summary(sap_ql_2017_lm_d)

#### ARMA-Metapameter
acf(na.omit(sap_ql_2017_d),lag.max =10)
pacf(na.omit(sap_ql_2017_d),lag.max =10)

####### ARIMA(0,1,0) Modell erscheint angebracht
sap_ql_2017_arima010 <- arima(sap_ql_2017, order=c(0,1,0)) # Einfache Differenzzeitreihe als Basismodell


## Automatische Modellschätzung
sap_ql_2017_arima_auto1 <- auto.arima(sap_ql_2017, d=1, seasonal = F, stepwise =F) # ARIMA(p,1,q) Modell


## GARCH Modell
sap_ql_2017d_garch10 <- garchFit(data = sap_ql_2017_d, formula = ~ garch(1,0), trace =F) # der Parameter trace=F reduziert die Ausgabe

## ARMA-GARCH Modell
sap_ql_2017d_FINAL <- garchFit(data = sap_ql_2017_d, formula = ~ arma(0,0) + garch(1,1), trace =F) # der Parameter trace=F reduziert die Ausgabe
