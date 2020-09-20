bibliotecas = c("forecast","fpp2","readxl")
install.packages(bibliotecas)

library(forecast)
library(fpp2)
library(readxl)
autoplot(JohnsonJohnson)

autoplot(co2)

decompco2 <- decompose(JohnsonJohnson)
autoplot(decompco2)
ggseasonplot(JohnsonJohnson)


mdl1ad <- hw(JohnsonJohnson,seasonal='additive', h=16)
autoplot(mdl1ad)


dados_excel <- read_xlsx("vendascarros_2020.xlsx",sheet = "Dados")
head(dados_excel)
ts <- ts(dados_excel$VendasCarros,start = c(2018,1), frequency=12)
autoplot(ts)


decompco2 <- decompose(ts)
autoplot(decompco2)
ggseasonplot(JohnsonJohnson)



mdl1ts <- hw(ts,seasonal='additive', h=48)
autoplot(mdl1ts)


mdl2 <- holt(austres, h = 12)
autoplot(mdl2)

autoplot(a10) + ylab("fdsfs")
decompa10 <- decompose(a10)
autoplot(decompa10)

mdladitivo_a10 <-hw(a10, seasonal = "additive",h=36, PI=F)
mdlmultiplicativo_a10 <- hw(a10, seasonal = "multiplicative", h = 36, PI = F)

autoplot(a10)+ ylab("Despesas governamentais mensais")+
  autolayer(mdladitivo_a10, series="HW Add.")+
  autolayer(mdlmultiplicativo_a10, series="HW mult.")
