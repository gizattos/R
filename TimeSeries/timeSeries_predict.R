#### carregando bibliotecas
bibliotecas = c("forecast","fpp2","readxl")
install.packages(bibliotecas)
library(forecast)
library(fpp2)
library(readxl)

## visualizando o time series da JohnsonJohnson
autoplot(JohnsonJohnson)

## decompando para visualizar de maneira mais clara
decompco2 <- decompose(JohnsonJohnson)
autoplot(decompco2)

## aplicando outra forma de visualizar esse time series
ggseasonplot(JohnsonJohnson)

## prevendo os pr�ximos 4 anos utilizando o m�todo hw indicado para time series com sazonalidade
## o h = 16 � utilizado pois a base � trimestral, sendo 4 per�odos ao ano
## ent�o para prever os 4 pr�ximos anos fica 4x4 = 16
mdl1ad <- hw(JohnsonJohnson,seasonal='additive', h=16)
autoplot(mdl1ad)

## carregando base de vendas de carros
dados_excel <- read_xlsx("vendascarros_2020.xlsx",sheet = "Dados")
## visualizando os primeiros registros
head(dados_excel)
## transformando a base em time series, come�ando no ano de 2018
##e com frequ�ncia de 12, j� que s�o 12 meses por ano.
ts <- ts(dados_excel$VendasCarros,start = c(2018,1), frequency=12)
autoplot(ts)

## decompando para uma melhor visualiza��o
decompco2 <- decompose(ts)
autoplot(decompco2)
## prevendo pr�ximos 4 anos
mdl1ts <- hw(ts,seasonal='additive', h=48)
autoplot(mdl1ts)

## prevendo a base austres
mdl2 <- holt(austres, h = 12)
autoplot(mdl2)

## demonstrando o time series a10
autoplot(a10) + ylab("crescimento")
## decompando e apresentando o resultado
decompa10 <- decompose(a10)
autoplot(decompa10)

## criando as previs�es aditivas e multiplicativas e plotando no mesmo gr�fico
mdladitivo_a10 <-hw(a10, seasonal = "additive",h=36, PI=F)
mdlmultiplicativo_a10 <- hw(a10, seasonal = "multiplicative", h = 36, PI = F)

autoplot(a10)+ ylab("Despesas governamentais mensais")+
  autolayer(mdladitivo_a10, series="HW Add.")+
  autolayer(mdlmultiplicativo_a10, series="HW mult.")
