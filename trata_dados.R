#######################################
###DEFLACIONA PHARMA####################
########################################

#import list of countries
selectedCountries <- data.frame(read.xlsx("selectedCountries.xlsx", sheetIndex = 1))

#importa dados deflatores
importsDeflatorIndex <- WDI(countr = 'all', indicator = "TM.VAL.MRCH.XD.WD", start = "1995", end = "2018")
exportsDeflatorIndex <- WDI(countr = 'all', indicator = "TX.VAL.MRCH.XD.WD", start = "1995", end = "2018")


#verificando consistencia do banco de dados
paisesFaltando <- data.frame(matrix(nrow = length(levels(as.factor(exportsDeflatorIndex$year))), ncol =3))
colnames(paisesFaltando) <- c("Ano","PaisExp","PaisImp")
paisesFaltando$Ano <- as.numeric(levels(as.factor(exportsDeflatorIndex$year)))


#tira paises que nao tem dados 
paisesExcluir <- (exportsDeflatorIndex$iso2c[which(
  exportsDeflatorIndex$iso2c%in%selectedCountries$ISO_2_IMF&
    is.na(exportsDeflatorIndex$TX.VAL.MRCH.XD.WD))])
#View(selectedCountries[which(selectedCountries$ISO_2_IMF%in%paisesExcluir),])
paisesExcluir <- paisesExcluir[-which(paisesExcluir=="CH")]
selectedCountriesDef <- selectedCountries[which(selectedCountries$ISO_2_IMF%!in%paisesExcluir),]

#aplica índice de 2016 para a suíça
exportsDeflatorIndex$TX.VAL.MRCH.XD.WD[which(exportsDeflatorIndex$country=="Switzerland"&
                                               is.na(exportsDeflatorIndex$TX.VAL.MRCH.XD.WD))] <- 
  exportsDeflatorIndex$TX.VAL.MRCH.XD.WD[which(exportsDeflatorIndex$year==2016&
                                                 exportsDeflatorIndex$country=="Switzerland")]

importsDeflatorIndex$TM.VAL.MRCH.XD.WD[which(importsDeflatorIndex$country=="Switzerland"&
                                               is.na(importsDeflatorIndex$TM.VAL.MRCH.XD.WD))] <-
  importsDeflatorIndex$TM.VAL.MRCH.XD.WD[which(importsDeflatorIndex$year==2016&
                                                 importsDeflatorIndex$country=="Switzerland")]




# View(exportsDeflatorIndex[which(is.na(exportsDeflatorIndex$TX.VAL.MRCH.XD.WD)),])
# View(importsDeflatorIndex[which(is.na(importsDeflatorIndex$TX.VAL.MRCH.XD.WD)),])

indExpAtual <- exportsDeflatorIndex[which(exportsDeflatorIndex$iso2c%in%selectedCountriesDef$ISO_2_IMF),]
indImpAtual <- importsDeflatorIndex[which(importsDeflatorIndex$iso2c%in%selectedCountriesDef$ISO_2_IMF),]


#coloca os dados na base de 2018
for (year in 1995:2018){
  for (pais in selectedCountriesDef$ISO_2_IMF) {
    indExpAtual$TX.VAL.MRCH.XD.WD[which(indExpAtual$iso2c==pais&indExpAtual$year==year)] <- 
      indExpAtual$TX.VAL.MRCH.XD.WD[which(indExpAtual$iso2c==pais&
                                            indExpAtual$year==2018)] /
      indExpAtual$TX.VAL.MRCH.XD.WD[which(
        indExpAtual$iso2c==pais&
          indExpAtual$year==year)]
    
    indImpAtual$TM.VAL.MRCH.XD.WD[which(indImpAtual$iso2c==pais&indImpAtual$year==year)] <- 
      indImpAtual$TM.VAL.MRCH.XD.WD[which(indImpAtual$iso2c==pais&
                                            indImpAtual$year==2018)] /
      indImpAtual$TM.VAL.MRCH.XD.WD[which(
        indImpAtual$iso2c==pais&
          indImpAtual$year==year)]
  }
}

#verifica se há 'NA's
TRUE%in%is.na(indImpAtual)
TRUE%in%is.na(indExpAtual)
#tira tudo antes de 1995
PharmaOriginal <- read.csv("PharmaAllCod30.csv")
PharmaAllCod30 <- PharmaOriginal[which(PharmaOriginal$year>1994&
                                         PharmaOriginal$reporter_iso%in%selectedCountriesDef$ISO_3_UN&
                                         PharmaOriginal$trade_flow_code%in%(c(1,2))),]

#aplicadeflatores nas bases
for (linha in 1:length(PharmaAllCod30$X)) {
  if (PharmaAllCod30$trade_flow_code[linha]%in%c(1)) {tp <- "Imp"}
  else if (PharmaAllCod30$trade_flow_code[linha]%in%c(2)) {tp <- "Exp"}
  PharmaAllCod30$trade_value_usd[linha] <- deflaciona(PharmaAllCod30$trade_value_usd[linha],
                                                      PharmaAllCod30$year[linha],
                                                      as.character(PharmaAllCod30$reporter_iso[linha]),tp,3)
}

write.csv(PharmaAllCod30,"PharmaAllCod30Def.csv")
write.xlsx(selectedCountriesDef,"selectedCountriesDef.xlsx")







#####################################
###CRIA SERIES HISTORICAS CHARGES###
#####################################
############HISTORICO DE CHARGES POR VARIOS AGREGADOS NAO DEFLACIONADOS###########################
selectedCountries <- read.xlsx("selectedCountries.xlsx", sheetIndex = 1)
#histórico de charges por nivel de desenvolvimento
histChargesDevstCredit <- data.frame(matrix(nrow = 2,ncol = (2019-1980)))
rownames(histChargesDevstCredit) <- c("Developed","Developing")
colnames(histChargesDevstCredit) <- c(1980:2018)
histChargesDevstDebit <- data.frame(matrix(nrow = 2, ncol = (2019-1980)))
rownames(histChargesDevstDebit) <- c("Developed","Developing")
colnames(histChargesDevstDebit) <- c(1980:2018)
for (ano in 1980:2018) {
  histChargesDevstCredit["Developed",(ano-1979)] <- sum(chargesIPCredits$BXSORL_BP6_USD[which(
    chargesIPCredits$year==ano&chargesIPCredits$iso2c%in%(ctrNivDes("Developed","iso2")))],na.rm = TRUE)
  histChargesDevstCredit["Developing",(ano-1979)] <- sum(chargesIPCredits$BXSORL_BP6_USD[which(
    chargesIPCredits$year==ano&chargesIPCredits$iso2c%in%(ctrNivDes("Developing","iso2")))],na.rm = TRUE)
  histChargesDevstDebit["Developed",(ano-1979)] <- sum(chargesIPDebits$BMSORL_BP6_USD[which(
    chargesIPDebits$year==ano&chargesIPDebits$iso2c%in%(ctrNivDes("Developed","iso2")))], na.rm = TRUE)
  histChargesDevstDebit["Developing",(ano-1979)] <- sum(chargesIPDebits$BMSORL_BP6_USD[which(
    chargesIPDebits$year==ano&chargesIPDebits$iso2c%in%(ctrNivDes("Developing","iso2")))], na.rm = TRUE)
}

# histChargesNet <- histChargesDevstCredit - histChargesDevstDebit
# histChargesFlux <- histChargesDevstCredit + histChargesDevstDebit

# write.xlsx(histChargesNet,"Historico_charges_net.xlsx")
# write.xlsx(histChargesFlux,"Historico_charges_Flux.xlsx")
# write.xlsx(histChargesDevstCredit,"hiscred.xlsx")
# write.xlsx(histChargesDevstDebit,"hisdeb.xlsx")

#historico de charges por nivel de renda
histChargesRendaCredit <- data.frame(matrix(nrow = length(levels(as.factor(selectedCountries$Income_Level))),ncol = (2019-1980)))
rownames(histChargesRendaCredit) <- c("High income","Upper middle income","Lower middle income","Low income")
colnames(histChargesRendaCredit) <- c(1980:2018)
histChargesRendaDebit <- data.frame(matrix(nrow = length(levels(as.factor(selectedCountries$Income_Level))),ncol = (2019-1980)))
rownames(histChargesRendaDebit) <- c("High income","Upper middle income","Lower middle income","Low income")
colnames(histChargesRendaDebit) <- c(1980:2018)
for (ano in 1980:2018) {
  for (item in (rownames(histChargesRendaCredit))) {
    histChargesRendaCredit[item,(ano-1979)] <- sum(chargesIPCredits$BXSORL_BP6_USD[which(
      chargesIPCredits$year==ano&chargesIPCredits$iso2c%in%(listAgreg("Renda",item,2)))],na.rm = TRUE)
    histChargesRendaDebit[item,(ano-1979)] <- sum(chargesIPDebits $BMSORL_BP6_USD[which(
      chargesIPDebits$year==ano&chargesIPDebits$iso2c%in%(listAgreg("Renda",item,2)))],na.rm = TRUE)
    #print(item)
  }
}

#historico de charges por regiao
histChargesRegiaoCredit <- data.frame(matrix(nrow = length(levels(as.factor(selectedCountries$Region ))),ncol = (2019-1980)))
rownames(histChargesRegiaoCredit) <- levels(as.factor(selectedCountries$Region))
colnames(histChargesRegiaoCredit) <- c(1980:2018)
histChargesRegiaoDebit <- data.frame(matrix(nrow = length(levels(as.factor(selectedCountries$Region))),ncol = (2019-1980)))
rownames(histChargesRegiaoDebit) <- levels(as.factor(selectedCountries$Region))
colnames(histChargesRegiaoDebit) <- c(1980:2018)
for (ano in 1980:2018) {
  for (item in (rownames(histChargesRegiaoCredit))) {
    histChargesRegiaoCredit[item,(ano-1979)] <- sum(chargesIPCredits$BXSORL_BP6_USD[which(
      chargesIPCredits$year==ano&chargesIPCredits$iso2c%in%(listAgreg("Regiao",item,2)))],na.rm = TRUE)
    histChargesRegiaoDebit[item,(ano-1979)] <- sum(chargesIPDebits$BMSORL_BP6_USD[which(
      chargesIPDebits$year==ano&chargesIPDebits$iso2c%in%(listAgreg("Regiao",item,2)))],na.rm = TRUE)
    #print(item)
  }
}

# histChargesRegiaoCredit <- histChargesRegiaoCredit[with(histChargesRegiaoCredit,order(-`2018`)),]
#View(histChargesRegiaoCredit)
# histChargesRegiaoDebit <- histChargesRegiaoDebit[with(histChargesRegiaoDebit,order(-`2018`)),]
# #View(histChargesRegiaoDebit)


###Por sub regiao
histChargesSubRegiaoCredit <- data.frame(matrix(nrow = length(levels(as.factor(selectedCountries$Sub_Region))),ncol = (2019-1980)))
rownames(histChargesSubRegiaoCredit) <- levels(as.factor(selectedCountries$Sub_Region))
colnames(histChargesSubRegiaoCredit) <- c(1980:2018)
histChargesSubRegiaoDebit <- data.frame(matrix(nrow = length(levels(as.factor(selectedCountries$Sub_Region))),ncol = (2019-1980)))
rownames(histChargesSubRegiaoDebit) <- levels(as.factor(selectedCountries$Sub_Region))
colnames(histChargesSubRegiaoDebit) <- c(1980:2018)
for (ano in 1980:2018) {
  for (item in (rownames(histChargesSubRegiaoCredit))) {
    histChargesSubRegiaoCredit[item,(ano-1979)] <- sum(chargesIPCredits$BXSORL_BP6_USD[which(
      chargesIPCredits$year==ano&chargesIPCredits$iso2c%in%(listAgreg("SubRegiao",item,2)))],na.rm = TRUE)
    histChargesSubRegiaoDebit[item,(ano-1979)] <- sum(chargesIPDebits$BMSORL_BP6_USD[which(
      chargesIPDebits$year==ano&chargesIPDebits$iso2c%in%(listAgreg("SubRegiao",item,2)))],na.rm = TRUE)
    #print(item)
  }
}

# histChargesSubRegiaoCredit <- histChargesSubRegiaoCredit[with(histChargesSubRegiaoCredit,order(-`2018`)),]
# #View(histChargesSubRegiaoCredit)
# histChargesSubRegiaoDebit <- histChargesSubRegiaoDebit[with(histChargesSubRegiaoDebit,order(-`2018`)),]
# #View(histChargesSubRegiaoDebit)







# item <- "Regiao"
# tipo <- "Credit"
# ottipo <- "net"
# 
# rbind(tudo,linha,get(paste("histCharges",item,tipo, sep = "")))

tudo <- matrix(ncol = ncol(histChargesDevstCredit))
colnames(tudo) <- colnames(histChargesDevstCredit)
linha <- matrix(ncol = ncol(histChargesDevstCredit), nrow = 1)
colnames(linha) <- colnames(histChargesDevstCredit)


for (item in c("Regiao","SubRegiao","Devst","Renda")) {
  for (tipo in c("Credit","Debit")) {
    rownames(linha) <- paste("histórico Charges",item,tipo, sep = " ")
    tudo <- rbind(tudo,linha,
                  get(paste("histCharges",item,tipo, sep = "")))}
  for (ottipo in c("net","flow")){
    rownames(linha) <- paste("histórico Charges",item,ottipo, sep = " ")
    if (ottipo == "net") {
      tudo <- rbind(tudo,linha, 
                    get(paste("histCharges",item,"Credit", sep = ""))-
                      get(paste("histCharges",item,"Debit", sep = "")))}
    else {
      tudo <- rbind(tudo,linha, 
                    get(paste("histCharges",item,"Credit", sep = ""))+
                      get(paste("histCharges",item,"Debit", sep = "")))}
  }
}

tudo <- cbind(rownames(tudo),tudo)
tudo$`rownames(tudo)` <- removeNumbers(as.character(tudo$`rownames(tudo)`))
rownames(tudo) <- c(1:length(tudo$`rownames(tudo)`))
write.xlsx(tudo,"agregadosCharges.xlsx", row.names = FALSE)

############################
#FIM SERIE HISTORICA CHARGES
############################















########################################################
######CRIA SERIES HISTORICAS PHARMA#####################
########################################################
selectedCountries <- read.xlsx("selectedCountriesDef.xlsx", sheetIndex = 1)
Pharma <- read.csv("PharmaAllCod30Def.csv")
#write.xlsx(selectedCountries,"selectedCountries.xlsx")

############HISTORICO DE Pharma POR VARIOS AGREGADOS NAO DEFLACIONADOS###########################

#histórico de Pharma por nivel de desenvolvimento
histPharmaDevstCredit <- data.frame(matrix(nrow = 2,ncol = (2019-1995)))
rownames(histPharmaDevstCredit) <- c("Developed","Developing")
colnames(histPharmaDevstCredit) <- c(1995:2018)
histPharmaDevstDebit <- data.frame(matrix(nrow = 2, ncol = (2019-1995)))
rownames(histPharmaDevstDebit) <- c("Developed","Developing")
colnames(histPharmaDevstDebit) <- c(1995:2018)
for (ano in 1995:2018) {
  histPharmaDevstCredit["Developed",(ano-1994)] <- sum(Pharma$trade_value_usd[which(
    Pharma$year==ano&Pharma$trade_flow_code==2&Pharma$reporter_iso%in%(ctrNivDes("Developed","iso3")))],
    na.rm = TRUE)
  histPharmaDevstCredit["Developing",(ano-1994)] <- sum(Pharma$trade_value_usd[which(
    Pharma$year==ano&Pharma$trade_flow_code==2&Pharma$reporter_iso%in%(ctrNivDes("Developing","iso3")))],
    na.rm = TRUE)
  histPharmaDevstDebit["Developed",(ano-1994)] <- sum(Pharma$trade_value_usd[which(
    Pharma$year==ano&Pharma$trade_flow_code==1&Pharma$reporter_iso%in%(ctrNivDes("Developed","iso3")))],
    na.rm = TRUE)
  histPharmaDevstDebit["Developing",(ano-1994)] <- sum(Pharma$trade_value_usd[which(
    Pharma$year==ano&Pharma$trade_flow_code==1&Pharma$reporter_iso%in%(ctrNivDes("Developing","iso3")))],
    na.rm = TRUE)
}


#limite <-3* max(histPharmaDevstCredit["Developing",]-histPharmaDevstDebit["Developing",])



#View(histPharmaDevstCredit)
# histPharmaNet <- histPharmaDevstCredit - histPharmaDevstDebit
# histPharmaFlux <- histPharmaDevstCredit + histPharmaDevstDebit

# write.xlsx(histPharmaNet,"Historico_Pharma_net.xlsx")
# write.xlsx(histPharmaFlux,"Historico_Pharma_Flux.xlsx")
# write.xlsx(histPharmaDevstCredit,"hiscred.xlsx")
# write.xlsx(histPharmaDevstDebit,"hisdeb.xlsx")

#historico de Pharma por nivel de renda
histPharmaRendaCredit <- data.frame(matrix(nrow = length(levels(as.factor(selectedCountries$Income_Level))),ncol = (2019-1995)))
rownames(histPharmaRendaCredit) <- c("High income","Upper middle income","Lower middle income","Low income")
colnames(histPharmaRendaCredit) <- c(1995:2018)
histPharmaRendaDebit <- data.frame(matrix(nrow = length(levels(as.factor(selectedCountries$Income_Level))),ncol = (2019-1995)))
rownames(histPharmaRendaDebit) <- c("High income","Upper middle income","Lower middle income","Low income")
colnames(histPharmaRendaDebit) <- c(1995:2018)
for (ano in 1995:2018) {
  for (item in (rownames(histPharmaRendaCredit))) {
    histPharmaRendaCredit[item,(ano-1994)] <- sum(Pharma$trade_value_usd[which(
      Pharma$year==ano&Pharma$trade_flow_code==2&Pharma$reporter_iso%in%(listAgreg("Renda",item,3)))],
      na.rm = TRUE)
    histPharmaRendaDebit[item,(ano-1994)] <- sum(Pharma $trade_value_usd[which(
      Pharma$year==ano&Pharma$trade_flow_code==1&Pharma$reporter_iso%in%(listAgreg("Renda",item,3)))],
      na.rm = TRUE)
    #print(item)
  }
}

#historico de Pharma por regiao
histPharmaRegiaoCredit <- data.frame(matrix(nrow = length(levels(as.factor(selectedCountries$Region ))),ncol = (2019-1995)))
rownames(histPharmaRegiaoCredit) <- levels(as.factor(selectedCountries$Region))
colnames(histPharmaRegiaoCredit) <- c(1995:2018)
histPharmaRegiaoDebit <- data.frame(matrix(nrow = length(levels(as.factor(selectedCountries$Region))),ncol = (2019-1995)))
rownames(histPharmaRegiaoDebit) <- levels(as.factor(selectedCountries$Region))
colnames(histPharmaRegiaoDebit) <- c(1995:2018)
for (ano in 1995:2018) {
  for (item in (rownames(histPharmaRegiaoCredit))) {
    histPharmaRegiaoCredit[item,(ano-1994)] <- sum(Pharma$trade_value_usd[which(
      Pharma$year==ano&Pharma$trade_flow_code==2&Pharma$reporter_iso%in%(listAgreg("Regiao",item,3)))],na.rm = TRUE)
    histPharmaRegiaoDebit[item,(ano-1994)] <- sum(Pharma$trade_value_usd[which(
      Pharma$year==ano&Pharma$trade_flow_code==1&Pharma$reporter_iso%in%(listAgreg("Regiao",item,3)))],na.rm = TRUE)
    #print(item)
  }
}

histPharmaRegiaoCredit <- histPharmaRegiaoCredit[with(histPharmaRegiaoCredit,order(-`2018`)),]
#View(histPharmaRegiaoCredit)
histPharmaRegiaoDebit <- histPharmaRegiaoDebit[with(histPharmaRegiaoDebit,order(-`2018`)),]
#View(histPharmaRegiaoDebit)


###Por sub regiao
histPharmaSubRegiaoCredit <- data.frame(matrix(nrow = length(levels(as.factor(selectedCountries$Sub_Region))),ncol = (2019-1995)))
rownames(histPharmaSubRegiaoCredit) <- levels(as.factor(selectedCountries$Sub_Region))
colnames(histPharmaSubRegiaoCredit) <- c(1995:2018)
histPharmaSubRegiaoDebit <- data.frame(matrix(nrow = length(levels(as.factor(selectedCountries$Sub_Region))),ncol = (2019-1995)))
rownames(histPharmaSubRegiaoDebit) <- levels(as.factor(selectedCountries$Sub_Region))
colnames(histPharmaSubRegiaoDebit) <- c(1995:2018)
for (ano in 1995:2018) {
  for (item in (rownames(histPharmaSubRegiaoCredit))) {
    histPharmaSubRegiaoCredit[item,(ano-1994)] <- sum(Pharma$trade_value_usd[which(
      Pharma$year==ano&Pharma$trade_flow_code==2&Pharma$reporter_iso%in%(listAgreg("SubRegiao",item,3)))],na.rm = TRUE)
    histPharmaSubRegiaoDebit[item,(ano-1994)] <- sum(Pharma$trade_value_usd[which(
      Pharma$year==ano&Pharma$trade_flow_code==1&Pharma$reporter_iso%in%(listAgreg("SubRegiao",item,3)))],na.rm = TRUE)
    #print(item)
  }
}

histPharmaSubRegiaoCredit <- histPharmaSubRegiaoCredit[with(histPharmaSubRegiaoCredit,order(-`2018`)),]
#View(histPharmaSubRegiaoCredit)
histPharmaSubRegiaoDebit <- histPharmaSubRegiaoDebit[with(histPharmaSubRegiaoDebit,order(-`2018`)),]
#View(histPharmaSubRegiaoDebit)

# tipo <- "Credit"
# item <- "Regiao"
# ottipo <- "net"

save.image("atualizadaDadosPharma.Rdata")

tudo <- matrix(ncol = ncol(histPharmaDevstCredit))
colnames(tudo) <- colnames(histPharmaDevstCredit)
linha <- matrix(ncol = ncol(histPharmaDevstCredit), nrow = 1)
colnames(linha) <- colnames(histPharmaDevstCredit)


for (item in c("Regiao","SubRegiao","Devst","Renda")) {
  for (tipo in c("Credit","Debit")) {
    rownames(linha) <- paste("historico Pharma",item,tipo, sep = " ")
    tudo <- rbind(tudo,linha,
                  get(paste("histPharma",item,tipo, sep = "")))}
  for (ottipo in c("net","flow")){
    rownames(linha) <- paste("histórico Pharma",item,ottipo, sep = " ") 
    if (ottipo == "net") {
      tudo <- rbind(tudo, linha,
                    get(paste("histPharma",item,"Credit", sep = ""))-
                      get(paste("histPharma",item,"Debit", sep = "")))}
    else {
      tudo <- rbind(tudo, linha,
                    get(paste("histPharma",item,"Credit", sep = ""))+
                      get(paste("histPharma",item,"Debit", sep = "")))}
  }
}
#View(tudo)

tudo <- cbind(rownames(tudo),tudo)
tudo$`rownames(tudo)` <- removeNumbers(as.character(tudo$`rownames(tudo)`))
rownames(tudo) <- c(1:length(tudo$`rownames(tudo)`))
write.xlsx(tudo,"agregadosPharma.xlsx")


##########################################
###########FIM SERIE HISTORICA PHARMA
##########################################











#############################
#TABELAS E SUMARIOS
##############################
tabela2 <- data.frame(matrix(nrow = length(levels(as.factor(selectedCountries$Region))),
                             ncol = length(c("Total Flow","Net Flow","World Share","World Flow of other services share"))))
rownames(tabela2) <- levels(as.factor(selectedCountries$Region))
colnames(tabela2) <- c("Total Flow","Net Flow","World Share","World Flow of other services share")
totalFlow <- (sum(histChargesRegiaoCredit[,37:39]) + sum(histChargesRegiaoDebit[,37:39]))/3

for (item in rownames(tabela2)) {
  tabela2[item,1] <- mean(as.numeric(histChargesRegiaoCredit[item,37:39])) +
    mean(as.numeric(histChargesRegiaoDebit[item,37:39]))
  tabela2[item,2] <- mean(as.numeric(histChargesRegiaoCredit[item,37:39])) -
    mean(as.numeric(histChargesRegiaoDebit[item,37:39]))
  tabela2[item,3] <- tabela2[item,1] / totalFlow
  tabela2[item,4] <- (
    (sum(servicesCredits$BXS_BP6_USD[which(servicesCredits$iso2c%in%
                                             listAgreg("Regiao",item,2)&
                                             servicesCredits$year%in%c(2016:2018))],na.rm = TRUE)+
       (sum(servicesDebits$BMS_BP6_USD[which(servicesDebits$iso2c%in%
                                               listAgreg("Regiao",item,2)&
                                               servicesDebits$year%in%c(2016:2018))],na.rm = TRUE)))-
      (tabela2[item,1]*3)
  )/
    (
      (
        sum(servicesCredits$BXS_BP6_USD[which(servicesCredits$iso2c%in%
                                                selectedCountries$ISO_2_IMF&
                                                servicesCredits$year%in%c(2016:2018))],na.rm = TRUE)+
          sum(servicesDebits$BMS_BP6_USD[which(servicesDebits$iso2c%in%
                                                 selectedCountries$ISO_2_IMF&
                                                 servicesDebits$year%in%c(2016:2018))],na.rm = TRUE)
      )-
        (totalFlow * 3)
    )
}



#TABELA 3, PHARMACEUTICAL POR REGIAO
#load("atualizadaDadosPharma.Rdata")
tabela3 <- data.frame(matrix(nrow = length(levels(as.factor(selectedCountries$Region))),
                             ncol = length(c("Total Flow","Net Flow","World Share","World Flow of other goods share"))))
rownames(tabela3) <- levels(as.factor(selectedCountries$Region))
colnames(tabela3) <- c("Total Flow","Net Flow","World Share","World Flow of other goods share")
totalFlow <- (sum(histPharmaRegiaoCredit[,22:24]) + sum(histPharmaRegiaoDebit[,22:24]))/3


for (item in rownames(tabela3)) {
  tabela3[item,1] <- mean(as.numeric(histPharmaRegiaoCredit[item,22:24])) +
    mean(as.numeric(histPharmaRegiaoDebit[item,22:24]))
  tabela3[item,2] <- mean(as.numeric(histPharmaRegiaoCredit[item,22:24])) -
    mean(as.numeric(histPharmaRegiaoDebit[item,22:24]))
  tabela3[item,3] <- tabela3[item,1] / totalFlow
  tabela3[item,4] <- (
    ((sum(goodsCredits$BXG_BP6_USD[which(goodsCredits$iso2c%in%
                                           listAgreg("Regiao",item,2)&
                                           goodsCredits$year%in%c(2016:2018))],na.rm = TRUE))+
       (sum(goodsDebits$BMG_BP6_USD[which(goodsDebits$iso2c%in%
                                            listAgreg("Regiao",item,2)&
                                            goodsDebits$year%in%c(2016:2018))],na.rm = TRUE))-((tabela3[item,1]*3)/10^6)
    )/
      (sum(goodsCredits$BXG_BP6_USD[which(goodsCredits$iso2c%in%
                                            selectedCountries$ISO_2_IMF&
                                            goodsCredits$year%in%c(2016:2018))],na.rm = TRUE)+
         sum(goodsDebits$BMG_BP6_USD[which(goodsDebits$iso2c%in%
                                             selectedCountries$ISO_2_IMF&
                                             goodsDebits$year%in%c(2016:2018))],na.rm = TRUE)- 
         ((totalFlow*3)/10^6)
      ) 
  )
}




#TABELA 4 CHARGES FOR THE USE OF IP POR PAIS

############################################################
full_list <- data.frame(matrix(nrow = length(selectedCountries$ISO_2_IMF),ncol = 2))
colnames(full_list) <- c("Country","Value")
full_list$Country <- selectedCountries$ISO_2_IMF
full_list$Value <- as.numeric(full_list$Value)
total_flow_charges <- sum(chargesIPCredits$BXSORL_BP6_USD[which(chargesIPCredits$iso2c%in%selectedCountries$ISO_2_IMF&
                                                                  chargesIPCredits$year==2018)],
                          na.rm = TRUE) +
  sum(chargesIPDebits$BMSORL_BP6_USD[which(chargesIPDebits$iso2c%in%selectedCountries$ISO_2_IMF&
                                             chargesIPDebits$year==2018)],
      na.rm = TRUE)

total_services_year <- (sum(servicesCredits$BXS_BP6_USD[which(servicesCredits$iso2c%in%
                                                                selectedCountries$ISO_2_IMF&
                                                                servicesCredits$year==2018)],na.rm = TRUE)+
                          sum(servicesDebits$BMS_BP6_USD[which(servicesDebits$iso2c%in%
                                                                 selectedCountries$ISO_2_IMF&
                                                                 servicesDebits$year==2018)],na.rm = TRUE))

total_goods_and_services_year <- (sum(servicesCredits$BXS_BP6_USD[which(servicesCredits$iso2c%in%
                                                                          selectedCountries$ISO_2_IMF&
                                                                          servicesCredits$year==2018)],na.rm = TRUE)+
                                    sum(servicesDebits$BMS_BP6_USD[which(servicesDebits$iso2c%in%
                                                                           selectedCountries$ISO_2_IMF&
                                                                           servicesDebits$year==2018)],na.rm = TRUE))+
  (sum(goodsCredits$BXG_BP6_USD[which(goodsCredits$iso2c%in%
                                        selectedCountries$ISO_2_IMF&
                                        goodsCredits$year==2018)],na.rm = TRUE)+
     sum(goodsDebits$BMG_BP6_USD[which(goodsDebits$iso2c%in%
                                         selectedCountries$ISO_2_IMF&
                                         goodsDebits$year==2018)],na.rm = TRUE))

total_gdp_year <- sum(GDP$NY.GDP.MKTP.CD[which(GDP$iso2c%in%selectedCountries$ISO_2_IMF&GDP$year==2018)], na.rm = TRUE)
#creating table
Biggest_Charges_Flow_2018 <- data.frame(matrix(nrow = 10, ncol = 9))
colnames(Biggest_Charges_Flow_2018) <- c("Country","Total Flow",
                                         "Net Flow","% of Goods and Services Flows",
                                         "% of Services Flows","% of the GDP","World Share","Word Share in Services",
                                         "World Share of the GDP")

for (item in full_list$Country) {
  full_list$Value[which(full_list$Country==item)] <- sum(chargesIPCredits$BXSORL_BP6_USD[which(
    chargesIPCredits$iso2c==item&chargesIPCredits$year==2018)],na.rm = TRUE) + 
    sum(chargesIPDebits$BMSORL_BP6_USD[which(
      chargesIPDebits$iso2c==item&chargesIPDebits$year==2018)],na.rm = TRUE)
}
full_list <- full_list[with(full_list,order(-Value)),]
# full_list[1:10,]
Biggest_Charges_Flow_2018$Country <- as.character(full_list$Country[1:10])
Biggest_Charges_Flow_2018$`Total Flow`<- full_list$Value[1:10]


#############################

for (item in Biggest_Charges_Flow_2018$Country) {
  
  #terceira coluna
  Biggest_Charges_Flow_2018$`Net Flow`[which(Biggest_Charges_Flow_2018$Country==item)] <- 
    sum(chargesIPCredits$BXSORL_BP6_USD[which(
      chargesIPCredits$iso2c==item&chargesIPCredits$year==2018)],na.rm = TRUE) - 
    sum(chargesIPDebits$BMSORL_BP6_USD[which(
      chargesIPDebits$iso2c==item&chargesIPDebits$year==2018)],na.rm = TRUE)
  #quarta coluna
  Biggest_Charges_Flow_2018$`% of Goods and Services Flows` [which(Biggest_Charges_Flow_2018$Country==item)] <-
    Biggest_Charges_Flow_2018$`Total Flow`[which(Biggest_Charges_Flow_2018$Country==item)] / (
      (sum(servicesCredits$BXS_BP6_USD[which(servicesCredits$iso2c==item&
                                               servicesCredits$year==2018)],
           na.rm = TRUE) +
         sum(servicesDebits$BMS_BP6_USD[which(servicesDebits$iso2c==item&
                                                servicesDebits$year==2018)],
             na.rm = TRUE)) +
        (sum(goodsCredits$BXG_BP6_USD[which(goodsCredits$iso2c==item&
                                              goodsCredits$year==2018)],
             na.rm = TRUE) +
           sum(goodsDebits$BMG_BP6_USD[which(goodsDebits$iso2c==item&
                                               goodsDebits$year==2018)],
               na.rm = TRUE))
    )
  #quinta coluna
  Biggest_Charges_Flow_2018$`% of Services Flows`[which(Biggest_Charges_Flow_2018$Country==item)] <-
    Biggest_Charges_Flow_2018$`Total Flow`[which(Biggest_Charges_Flow_2018$Country==item)] / (
      (sum(servicesCredits$BXS_BP6_USD[which(servicesCredits$iso2c==item&
                                               servicesCredits$year==2018)],
           na.rm = TRUE) +
         sum(servicesDebits$BMS_BP6_USD[which(servicesDebits$iso2c==item&
                                                servicesDebits$year==2018)],
             na.rm = TRUE)))
  #sexta coluna
  Biggest_Charges_Flow_2018$`% of the GDP`[which(Biggest_Charges_Flow_2018$Country==item)] <-
    Biggest_Charges_Flow_2018$`Total Flow`[which(Biggest_Charges_Flow_2018$Country==item)] / 
    (GDP$NY.GDP.MKTP.CD[which(GDP$iso2c==item&GDP$year==2018)]/10^6)
  #setima coluna
  Biggest_Charges_Flow_2018$`World Share`[which(Biggest_Charges_Flow_2018$Country==item)] <-
    Biggest_Charges_Flow_2018$`Total Flow`[which(Biggest_Charges_Flow_2018$Country==item)] /
    total_flow_charges
  #oitava coluna
  Biggest_Charges_Flow_2018$`Word Share in Services`[which(Biggest_Charges_Flow_2018$Country==item)] <-
    (sum(servicesCredits$BXS_BP6_USD[which(servicesCredits$iso2c==item&
                                             servicesCredits$year==2018)],
         na.rm = TRUE) +
       sum(servicesDebits$BMS_BP6_USD[which(servicesDebits$iso2c==item&
                                              servicesDebits$year==2018)],
           na.rm = TRUE) - Biggest_Charges_Flow_2018 [which(Biggest_Charges_Flow_2018$Country==item),2] ) /
    (total_services_year - total_flow_charges)
  #Nona
  Biggest_Charges_Flow_2018$`World Share of the GDP`[which(Biggest_Charges_Flow_2018$Country==item)] <-
    (GDP$NY.GDP.MKTP.CD[which(GDP$iso2c==item&GDP$year==2018)]) / total_gdp_year
  #primeira coluna
  Biggest_Charges_Flow_2018$Country[which(Biggest_Charges_Flow_2018$Country==item)] <- 
    as.character(selectedCountries$Country[which(selectedCountries$ISO_2_IMF==item)])
}

#sum(Biggest_Charges_Flow_2018$`World Share`)
#View(Biggest_Charges_Flow_2018)
#getwd()

##################################################



#CHARGES IP THE REST
selectedCountries <- read.xlsx("selectedCountries.xlsx", sheetIndex = 1)
theRest <- c("CN","KR","IN","BR","MY","TH","AR","ID","ZA",
             "CL","TR","MX")


FlowsChargesRest <- data.frame(matrix(nrow = length(theRest), ncol = 9))
colnames(FlowsChargesRest) <- c("Country","Total Flow",
                                "Net Flow","% of Goods and Services Flows","% of Services Flows",
                                "% of the GDP","World Share","Word Share in Services",
                                "World Share of the GDP")
FlowsChargesRest[,1] <- theRest
#View(FlowsPharmaRest)
for (item in FlowsChargesRest$Country) {
  #Segunda Coluna
  FlowsChargesRest$`Total Flow` [which(FlowsChargesRest$Country==item)] <- sum(chargesIPCredits$BXSORL_BP6_USD[which(
    chargesIPCredits$iso2c==item&chargesIPCredits$year==2018)],na.rm = TRUE) + 
    sum(chargesIPDebits$BMSORL_BP6_USD[which(
      chargesIPDebits$iso2c==item&chargesIPDebits$year==2018)],na.rm = TRUE)
  #terceira coluna
  FlowsChargesRest$`Net Flow`[which(FlowsChargesRest$Country==item)] <- 
    sum(chargesIPCredits$BXSORL_BP6_USD[which(
      chargesIPCredits$iso2c==item&chargesIPCredits$year==2018)],na.rm = TRUE) - 
    sum(chargesIPDebits$BMSORL_BP6_USD[which(
      chargesIPDebits$iso2c==item&chargesIPDebits$year==2018)],na.rm = TRUE)
  #quarta coluna
  FlowsChargesRest$`% of Goods and Services Flows` [which(FlowsChargesRest$Country==item)] <-
    FlowsChargesRest$`Total Flow`[which(FlowsChargesRest$Country==item)] / (
      (sum(servicesCredits$BXS_BP6_USD[which(servicesCredits$iso2c==item&
                                               servicesCredits$year==2018)],
           na.rm = TRUE) +
         sum(servicesDebits$BMS_BP6_USD[which(servicesDebits$iso2c==item&
                                                servicesDebits$year==2018)],
             na.rm = TRUE)) +
        (sum(goodsCredits$BXG_BP6_USD[which(goodsCredits$iso2c==item&
                                              goodsCredits$year==2018)],
             na.rm = TRUE) +
           sum(goodsDebits$BMG_BP6_USD[which(goodsDebits$iso2c==item&
                                               goodsDebits$year==2018)],
               na.rm = TRUE))
    )
  #quinta coluna
  FlowsChargesRest$`% of Services Flows`[which(FlowsChargesRest$Country==item)] <-
    FlowsChargesRest$`Total Flow`[which(FlowsChargesRest$Country==item)] / (
      (sum(servicesCredits$BXS_BP6_USD[which(servicesCredits$iso2c==item&
                                               servicesCredits$year==2018)],
           na.rm = TRUE) +
         sum(servicesDebits$BMS_BP6_USD[which(servicesDebits$iso2c==item&
                                                servicesDebits$year==2018)],
             na.rm = TRUE)))
  #sexta coluna
  FlowsChargesRest$`% of the GDP`[which(FlowsChargesRest$Country==item)] <-
    FlowsChargesRest$`Total Flow`[which(FlowsChargesRest$Country==item)] / 
    (GDP$NY.GDP.MKTP.CD[which(GDP$iso2c==item&GDP$year==2018)]/10^6)
  #setima coluna
  FlowsChargesRest$`World Share`[which(FlowsChargesRest$Country==item)] <-
    FlowsChargesRest$`Total Flow`[which(FlowsChargesRest$Country==item)] /
    total_flow_charges
  #oitava coluna
  FlowsChargesRest$`Word Share in Services`[which(FlowsChargesRest$Country==item)] <-
    (sum(servicesCredits$BXS_BP6_USD[which(servicesCredits$iso2c==item&
                                             servicesCredits$year==2018)],
         na.rm = TRUE) +
       sum(servicesDebits$BMS_BP6_USD[which(servicesDebits$iso2c==item&
                                              servicesDebits$year==2018)],
           na.rm = TRUE) - FlowsChargesRest$`Total Flow` [which(FlowsChargesRest$Country==item)] ) /
    (total_services_year - total_flow_charges)
  #Nona
  FlowsChargesRest$`World Share of the GDP`[which(FlowsChargesRest$Country==item)] <-
    (GDP$NY.GDP.MKTP.CD[which(GDP$iso2c==item&GDP$year==2018)]) / total_gdp_year
  #primeira coluna
  FlowsChargesRest$Country[which(FlowsChargesRest$Country==item)] <- 
    as.character(selectedCountries$Country[which(selectedCountries$ISO_2_IMF==item)])
}



# View(Biggest_Charges_Flow_2018)
# View(FlowsChargesRest)









############################################################
#TABELA 6 PHARMA POR PAIS

full_list <- data.frame(matrix(nrow = length(selectedCountries$ISO_2_IMF),ncol = 2))
colnames(full_list) <- c("Country","Value")
full_list$Country <- selectedCountries$ISO_3_UN
total_flow_Pharma <- sum(Pharma$trade_value_usd[which(Pharma$reporter_iso%in%selectedCountries$ISO_3_UN&
                                                        Pharma$year==2018)],
                         na.rm = TRUE)

total_goods_year <- (sum(goodsCredits$BXG_BP6_USD[which(goodsCredits$iso2c%in%
                                                          selectedCountries$ISO_2_IMF&
                                                          goodsCredits$year==2018)],na.rm = TRUE)+
                       sum(goodsDebits$BMG_BP6_USD[which(goodsDebits$iso2c%in%
                                                           selectedCountries$ISO_2_IMF&
                                                           goodsDebits$year==2018)],na.rm = TRUE))


total_goods_and_services_year <- (sum(servicesCredits$BXS_BP6_USD[which(servicesCredits$iso2c%in%
                                                                          selectedCountries$ISO_2_IMF&
                                                                          servicesCredits$year==2018)],na.rm = TRUE)+
                                    sum(servicesDebits$BMS_BP6_USD[which(servicesDebits$iso2c%in%
                                                                           selectedCountries$ISO_2_IMF&
                                                                           servicesDebits$year==2018)],na.rm = TRUE))+
  (sum(goodsCredits$BXG_BP6_USD[which(goodsCredits$iso2c%in%
                                        selectedCountries$ISO_2_IMF&
                                        goodsCredits$year==2018)],na.rm = TRUE)+
     sum(goodsDebits$BMG_BP6_USD[which(goodsDebits$iso2c%in%
                                         selectedCountries$ISO_2_IMF&
                                         goodsDebits$year==2018)],na.rm = TRUE))

total_gdp_year <- sum(GDP$NY.GDP.MKTP.CD[which(GDP$iso2c%in%selectedCountries$ISO_2_IMF&GDP$year==2018)], na.rm = TRUE)
#creating table
Biggest_Pharma_Flow_2018 <- data.frame(matrix(nrow = 10, ncol = 9))
colnames(Biggest_Pharma_Flow_2018) <- c("Country","Total Flow",
                                        "Net Flow",
                                        "% of Goods and Services Flows","% of Goods Flows",
                                        "% of the GDP","World Share","Word Share in Goods",
                                        "World Share of the GDP")

for (item in full_list$Country) {
  full_list$Value[which(full_list$Country==item)] <- 
    sum(Pharma$trade_value_usd[which(Pharma$reporter_iso==item&
                                       Pharma$year==2018&
                                       Pharma$trade_flow_code%in%c(1,2))]
        ,na.rm = TRUE)
}

full_list <- full_list[with(full_list,order(-Value)),]
# full_list[1:10,]
Biggest_Pharma_Flow_2018$Country <- as.character(full_list$Country[1:10])
Biggest_Pharma_Flow_2018$`Total Flow`<- full_list$Value[1:10]/10^6
#View(Biggest_Pharma_Flow_2018)

for (item in Biggest_Pharma_Flow_2018$Country) {
  
  #terceira coluna
  Biggest_Pharma_Flow_2018$`Net Flow`[which(Biggest_Pharma_Flow_2018$Country==item)] <- 
    (sum(Pharma$trade_value_usd[which(Pharma$year==2018&
                                        Pharma$reporter_iso==item&
                                        Pharma$trade_flow_code==2)],na.rm = TRUE) - 
       sum(Pharma$trade_value_usd[which(Pharma$year==2018&
                                          Pharma$reporter_iso==item&
                                          Pharma$trade_flow_code==1)],na.rm = TRUE))/10^6
  #  quarta coluna
  Biggest_Pharma_Flow_2018$`% of Goods and Services Flows`[which(Biggest_Pharma_Flow_2018$Country==item)] <-
    (Biggest_Pharma_Flow_2018$`Total Flow`[which(Biggest_Pharma_Flow_2018$Country==item)]) / (
      (sum(servicesCredits$BXS_BP6_USD[which(servicesCredits$iso2c==converteIso3(item)&
                                               servicesCredits$year==2018)],
           na.rm = TRUE) +
         sum(servicesDebits$BMS_BP6_USD[which(servicesDebits$iso2c==converteIso3(item)&
                                                servicesDebits$year==2018)],
             na.rm = TRUE)) +
        (sum(goodsCredits$BXG_BP6_USD[which(goodsCredits$iso2c==converteIso3(item)&
                                              goodsCredits$year==2018)],
             na.rm = TRUE) +
           sum(goodsDebits$BMG_BP6_USD[which(goodsDebits$iso2c==converteIso3(item)&
                                               goodsDebits$year==2018)],
               na.rm = TRUE))
    )
  #quinta coluna
  Biggest_Pharma_Flow_2018$`% of Goods Flows`[which(Biggest_Pharma_Flow_2018$Country==item)] <-
    Biggest_Pharma_Flow_2018$`Total Flow`[which(Biggest_Pharma_Flow_2018$Country==item)] / (
      sum(goodsCredits$BXG_BP6_USD[which(goodsCredits$iso2c==converteIso3(item)&
                                           goodsCredits$year==2018)],
          na.rm = TRUE) +
        sum(goodsDebits$BMG_BP6_USD[which(goodsDebits$iso2c==converteIso3(item)&
                                            goodsDebits$year==2018)],
            na.rm = TRUE)
    )
  #sexta coluna
  Biggest_Pharma_Flow_2018$`% of the GDP`[which(Biggest_Pharma_Flow_2018$Country==item)] <-
    Biggest_Pharma_Flow_2018$`Total Flow`[which(Biggest_Pharma_Flow_2018$Country==item)] / 
    (GDP$NY.GDP.MKTP.CD[which(GDP$iso2c==converteIso3(item)&GDP$year==2018)]/10^6)
  #setima coluna
  Biggest_Pharma_Flow_2018$`World Share`[which(Biggest_Pharma_Flow_2018$Country==item)] <-
    Biggest_Pharma_Flow_2018$`Total Flow`[which(Biggest_Pharma_Flow_2018$Country==item)] /
    (total_flow_Pharma/10^6)
  #oitava coluna
  Biggest_Pharma_Flow_2018$`Word Share in Goods`[which(Biggest_Pharma_Flow_2018$Country==item)] <-
    ((sum(goodsCredits$BXG_BP6_USD[which(goodsCredits$iso2c==converteIso3(item)&
                                           goodsCredits$year==2018)],
          na.rm = TRUE) +
        sum(goodsDebits$BMG_BP6_USD[which(goodsDebits$iso2c==converteIso3(item)&
                                            goodsDebits$year==2018)],
            na.rm = TRUE))- Biggest_Pharma_Flow_2018$`Total Flow`[which(Biggest_Pharma_Flow_2018$Country==item)]) /
    (total_goods_year - total_flow_Pharma)
  #Nona
  Biggest_Pharma_Flow_2018$`World Share of the GDP`[which(Biggest_Pharma_Flow_2018$Country==item)] <-
    (GDP$NY.GDP.MKTP.CD[which(GDP$iso2c==converteIso3(item)&GDP$year==2018)]) / total_gdp_year
  #primeira coluna
  Biggest_Pharma_Flow_2018$Country[which(Biggest_Pharma_Flow_2018$Country==item)] <- 
    as.character(selectedCountries$Country[which(selectedCountries$ISO_2_IMF==converteIso3(item))])
}




##########################################




#creating table THE REST
########################################
theRest <- c("CN","KR","IN","BR","MY","TH","AR","ID","ZA",
             "CL","TR","MX")


FlowsPharmaRest <- data.frame(matrix(nrow = length(theRest), ncol = 9))
colnames(FlowsPharmaRest) <- c("Country","Total Flow",
                               "Net Flow","% of Goods and Services Flows",
                               "% of Goods Flows",
                               "% of the GDP","World Share","Word Share in Goods",
                               "World Share of the GDP")
FlowsPharmaRest[,1] <- theRest
#View(FlowsPharmaRest)

for (item in FlowsPharmaRest$Country) {
  #segunda coluna
  FlowsPharmaRest$`Total Flow`[which(FlowsPharmaRest$Country==item)] <-
    (sum(Pharma$trade_value_usd[which(Pharma$year==2018&
                                        Pharma$reporter_iso==converteIso2(item)&
                                        Pharma$trade_flow_code==2)],na.rm = TRUE) + 
       sum(Pharma$trade_value_usd[which(Pharma$year==2018&
                                          Pharma$reporter_iso==converteIso2(item)&
                                          Pharma$trade_flow_code==1)],na.rm = TRUE))/10^6
  #terceira coluna
  FlowsPharmaRest$`Net Flow`[which(FlowsPharmaRest$Country==item)] <- 
    (sum(Pharma$trade_value_usd[which(Pharma$year==2018&
                                        Pharma$reporter_iso==converteIso2(item)&
                                        Pharma$trade_flow_code==2)],na.rm = TRUE) - 
       sum(Pharma$trade_value_usd[which(Pharma$year==2018&
                                          Pharma$reporter_iso==converteIso2(item)&
                                          Pharma$trade_flow_code==1)],na.rm = TRUE))/10^6
  #  quarta coluna
  FlowsPharmaRest$`% of Goods and Services Flows` [which(FlowsPharmaRest$Country==item)] <-
    FlowsPharmaRest$`Total Flow`[which(FlowsPharmaRest$Country==item)] / (
      (sum(servicesCredits$BXS_BP6_USD[which(servicesCredits$iso2c==item&
                                               servicesCredits$year==2018)],
           na.rm = TRUE) +
         sum(servicesDebits$BMS_BP6_USD[which(servicesDebits$iso2c==item&
                                                servicesDebits$year==2018)],
             na.rm = TRUE)) +
        (sum(goodsCredits$BXG_BP6_USD[which(goodsCredits$iso2c==item&
                                              goodsCredits$year==2018)],
             na.rm = TRUE) +
           sum(goodsDebits$BMG_BP6_USD[which(goodsDebits$iso2c==item&
                                               goodsDebits$year==2018)],
               na.rm = TRUE))
    )
  #quinta coluna
  FlowsPharmaRest$`% of Goods Flows`[which(FlowsPharmaRest$Country==item)] <-
    FlowsPharmaRest$`Total Flow`[which(FlowsPharmaRest$Country==item)] / (
      sum(goodsCredits$BXG_BP6_USD[which(goodsCredits$iso2c==item&
                                           goodsCredits$year==2018)],
          na.rm = TRUE) +
        sum(goodsDebits$BMG_BP6_USD[which(goodsDebits$iso2c==item&
                                            goodsDebits$year==2018)],
            na.rm = TRUE)
    )
  #sexta coluna
  FlowsPharmaRest$`% of the GDP`[which(FlowsPharmaRest$Country==item)] <-
    FlowsPharmaRest$`Total Flow`[which(FlowsPharmaRest$Country==item)] / 
    (GDP$NY.GDP.MKTP.CD[which(GDP$iso2c==item&GDP$year==2018)]/10^6)
  #setima coluna
  FlowsPharmaRest$`World Share`[which(FlowsPharmaRest$Country==item)] <-
    FlowsPharmaRest$`Total Flow`[which(FlowsPharmaRest$Country==item)] /
    (total_flow_Pharma/10^6)
  #oitava coluna
  FlowsPharmaRest$`Word Share in Goods`[which(FlowsPharmaRest$Country==item)] <-
    ((sum(goodsCredits$BXG_BP6_USD[which(goodsCredits$iso2c==item&
                                           goodsCredits$year==2018)],
          na.rm = TRUE) +
        sum(goodsDebits$BMG_BP6_USD[which(goodsDebits$iso2c==item&
                                            goodsDebits$year==2018)],
            na.rm = TRUE))-FlowsPharmaRest$`Total Flow`[which(FlowsPharmaRest$Country==item)]) /
    (total_goods_year - total_flow_Pharma)
  #Nona
  FlowsPharmaRest$`World Share of the GDP`[which(FlowsPharmaRest$Country==item)] <-
    (GDP$NY.GDP.MKTP.CD[which(GDP$iso2c==item&GDP$year==2018)]) / total_gdp_year
  #primeira coluna
  FlowsPharmaRest$Country[which(FlowsPharmaRest$Country==item)] <- 
    as.character(selectedCountries$Country[which(selectedCountries$ISO_2_IMF==item)])
}




write.xlsx(tabela2,"chargesRegiao.xlsx")
write.xlsx(tabela3,"PharmaRegiao.xlsx")
write.xlsx(Biggest_Charges_Flow_2018,"Top10Charges.xlsx")
write.xlsx(FlowsChargesRest,"TheRestCharges.xlsx")
write.xlsx(Biggest_Pharma_Flow_2018,"Top10Pharma.xlsx")
write.xlsx(FlowsPharmaRest,"TheRestPharma.xlsx")

save.image("estudoCharges.RData")
