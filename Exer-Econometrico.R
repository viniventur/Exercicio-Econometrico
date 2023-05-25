# Script da Questão 2 do Exercício Econométrico - 2022.2
# Por Vinícius Ventura

# Baixando os pacotes e carregando
install.packages("dplyr") 
install.packages("ggplot2")
install.packages("readxl")
install.packages("psych")
install.packages("openxlsx")
install.packages("gmodels")
install.packages("tseries")
library(tseries)
library(openxlsx)  
library(dplyr)
library(psych)
library(ggplot2)
library(readxl)
library(gmodels)

# carregando os dados
df <- read_excel("C:/Users/Robson Ventura/OneDrive/Trabalhos/Econometria - R/Atividade AB2/data_prova.xls")
View(df)
head(df)

# Filtrando por Alagoas
al <- subset(df, df$uf == 27 & df$ano == 2010)
View(al)

# tabela descritiva varias variáveis
summary(al[c('analf','pobre', 'poptot', 'popurb', 'rpc', 'esgag_inad')])

# Criar excel com a tabela descritiva
wb1 <- createWorkbook()
addWorksheet(wb1, "Summary")
writeData(wb1, "Summary", summary(al[c('analf','pobre', 'poptot', 'popurb',
                                       'rpc', 'esgag_inad')]))
saveWorkbook(wb1, "C:/Users/Robson Ventura/OneDrive/Trabalhos/Econometria - R/Atividade AB2/Resultados/summarytotal.xls")

# Análise mais detalhada da variável analf
describe(al$analf)
quantile(al$analf)
hist(al$analf, 
     main = "", 
     xlab = "Taxa de Analfabetismo",
     ylab = "Frequência")


# salvar em tabela
wb2 <- createWorkbook()
addWorksheet(wb2, "detalh analf")
writeData(wb2, "detalh analf", describe(al$analf))
saveWorkbook(wb2, "C:/Users/Robson Ventura/OneDrive/Trabalhos/Econometria - R/Atividade AB2/Resultados/describeanalf.xls")

# Criação de uma variável categórica para taxa de analfabetismmo
cortes <- quantile(al$analf, probs = c(0, 0.05, 0.5, 1))
cortes
al$sitanalf <- cut(al$analf, 
                   cortes, labels=c("bom", "regular", "ruim"),
                   right=FALSE)
View(al)

# Analisando os melhores municípios para analfabetismo
analfbom <- subset(al, al$sitanalf=='bom')
View(analfbom)

# Exportando analfbom
wb3 <- createWorkbook()
addWorksheet(wb3, "analfbom")
writeData(wb3, "analfbom", subset(al, al$sitanalf=='bom'))
saveWorkbook(wb3, "C:/Users/Robson Ventura/OneDrive/Trabalhos/Econometria - R/Atividade AB2/Resultados/analfbom.xls")

# Analisando os piores para analfabetismo
analfruim <- subset(al, al$sitanalf=='ruim')
View(analfruim)

# Selecionando as 5 primeiras linhas do dataframe ordenado
top5analfruim <- head(analfruim, 5)
View(top5analfruim)


# Exportando analfruim
wb4 <- createWorkbook()
addWorksheet(wb4, "analfbom")
writeData(wb4, "analfbom", head(analfruim, 5))
saveWorkbook(wb4, "C:/Users/Robson Ventura/OneDrive/Trabalhos/Econometria - R/Atividade AB2/Resultados/analfruim.xls")


# Variável de faixa populacional
cortespop <- c(-Inf, 5000, 20000, 50000, 100000, Inf)
al$fxpop <- cut(al$poptot, cortespop, labels=c(1,2,3,4,5), right=FALSE)

# Tabulação cruzada - sitanalf e fxpop
CrossTable(al$sitanalf, al$fxpop, prop.chisq = FALSE, digits=2)

# Exportando a tabulação cruzada
cross <- xtabs(~al$sitanalf + al$fxpop)
cross
popcross <- prop.table(cross, margin=1)*100
round(popcross,2)

# Estatística descritiva da variável pobre por faixa populacional
describeBy(al$pobre, al$fxpop, digits = 2)

cvpobre <- al %>%
  group_by(fxpop) %>%
  summarise(sd(pobre)/mean(pobre)*100)
  cvpobre
cvpobre  
  
# Plotagem
plot(al$pobre, al$poptot, ylab="População", xlab="Taxa de Pobreza")

# Box plot de pobreza por faixa populacional
boxplot(pobre~fxpop, al, xlab='Faixa populacional', ylab='Taxa de Pobreza')

# Variável de urbanização
al$urb <- (al$popurb/al$poptot)*100
al$rur <- (al$poprur/al$poptot)*100
View(al)

# Regressão com pobre como variável dependende
reg <- lm(pobre~urb, al)
print(summary(reg))

# Resíduos
uhat <- reg$residuals
hist(reg$residuals, xlab='Resíduos', ylab='Frequência', main='')
qqnorm(reg$residuals, pch = 1, frame = FALSE, main='')
qqline(reg$residuals, col = "red", lwd = 2)

# Jarque-Bera
testejb <- jarque.bera.test(uhat)
print(testejb)


