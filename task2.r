library(PerformanceAnalytics)
IMOEX<- read.csv2("/Users/Nikon/Desktop/CMC MSU/MC/6 sem/R/2/IMOEX.csv",
                sep = ";", dec = ".", header = TRUE)

ASSET <- read.csv2("/Users/Nikon/Desktop/CMC MSU/MC/6 sem/R/2/ASSET.csv",
                   sep=';',header=TRUE,dec=".")
head(IMOEX)
head(ASSET)
tail(ASSET)
tail(IMOEX)
Rf <-0.04
r2 <- diff(log(IMOEX$X.CLOSE.))
R1 <- diff(log(ASSET$X.CLOSE.))

Beta <- cov(R1,r2)/var(r2)
Beta

Alpha <- mean(R1)-Rf- Beta*(mean(r2)- Rf)
Alpha

#Sharp Koeff
Sharp <- mean(R1-Rf)/sd(R1-Rf)
Sharp

#VAR Исторический 
VAR_History<-VaR(R1, p=.95, method="historical")[1,1]
VAR_History
#VAR Нормальынй 
VAR_Normal<-VaR(R1, p=.95)[1,1]
VAR_Normal
#Shortfall Исторический
Shortfall_Historical <- ES(R1, p = 0.95, method = "historical")[1,1]
#Shortfall Нормальынй 
Shortfall_Normal <- ES(R1, p = 0.95, method = "gaussian")[1,1]
#ВЫВОД
Beta
Alpha
Sharp
VAR_Normal
VAR_History
Shortfall_Historical
Shortfall_Normal

