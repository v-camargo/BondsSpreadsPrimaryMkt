#Esta versão considera a regressão com a Razão Inversa de Mills da emissão externa
#e a Razão Inversa de Mills da presença de garantia



### Instalação das Bibliotecas
#install.packages("tidyverse")
#install.packages("sampleSelection")
#install.packages("plm")
#install.packages("lubridate")

### Bibliotecas Importadas -----------------------------------------------------

library(tidyverse)
library(sampleSelection)
library(plm)
library(lubridate)

### Tratamento dos Dados -------------------------------------------------------
##Base2------------------------------------------------------------------------
base2 <- readxl::read_xlsx("Base2vf.xlsx")


heckitbase2 <- base2 |> select(ID,`Empresa (j)`, βLP, βExtAccess, `Volume R$ 100 MM`, βHighGrade, 
                `Volume Médio Mercado Bonds R$ 100 MM`,`LP Mercado Bonds (>5 anos)`,
                `Volume Médio Debs R$ 100 MM`,`LP Debs (>5 anos)`, Ativo)
heckitbase2 <- heckitbase2 |> rename(empresaId=2,
                                BLP=3,
                                BExtAcess=4,
                                volEmissao=5,
                                BHG=6,
                                volMedBonds=7,
                                percLPAnoBonds=8,
                                volMedDebs=9,
                                percLPAnoDebs=10,
                                BExt=11)

#heckitbase2 <- heckitbase2 |> mutate(empresaId = empresaId |> as.factor(),
#                                BLP = BLP |> as.factor(),
#                                BExtAcess = BExtAcess |> as.factor(),
#                               BHG = BHG |> as.factor(),
#                                BExt = if_else(BExt == "Bonds", 1,0) %>% as.factor()
#                                )


##Base1 ------------------------------------------------------------------------
base1 <- readxl::read_xlsx("Base1vf.xlsx")


heckitbase1 <- base1 |> select (ID,`Empresa (j)`,`βExt`,`Emissao (i)`, `βLP`,
                                `βGarantia`,`Volume R$ (100 MM)`, `Spread DI+`,
                                `βDebInc`,`βCall`, `βHighGrade`, `RiscoPaís (%)`) 

heckitbase1 <- heckitbase1 |> rename(empresaId=2,
                                     BExt=3,
                                     emissao=4,
                                     BLP=5,
                                     BGarantia=6,
                                     volEmissao=7,
                                     spread=8,
                                     BDebInc=9,
                                     BCall=10,
                                     BHG=11,
                                     riscoPais=12)
                                     
                                     
                                     
#heckitbase1 <- heckitbase1 |> mutate(empresaId = empresaId |> as.factor(),
#                                     BExt = BExt |> as.factor(),
#                                     emissao = emissao %>% as.factor(),
#                                     BLP = BLP |> as.factor(),
#                                     BGarantia = ifelse(BGarantia == "S",1,0) %>% as.factor(),
#                                     BDebInc = BDebInc |> as.factor(),
#                                     BCall = BCall %>% as.factor(),
#                                     BHG = BHG |> as.factor()
#)


##Base3 -----------------------------------------------------------------------

base3 <- readxl::read_xlsx("Base3vf.xlsx")

heckitbase3 <- base3 |> select(ID, `PL R$ (100 MM)`,`TotalDebt R$ (100 MM)`)
heckitbase3 <-  heckitbase3 |> rename(pl = 2,
                                      totalDebt = 3)

##Base Final -------------------------------------------------------------------
heckitbase2f <- heckitbase2 |> mutate(BExt = if_else(BExt == "Bonds", 1,0))
                                      

heckitbase1f <- heckitbase1 %>% select(ID, emissao, BGarantia, spread, BDebInc, BCall, riscoPais) |> 
                mutate(riscoPais=riscoPais/100,
                       BGarantia = BGarantia |> as.double())
basefinalheckit <- merge(heckitbase2f, heckitbase1f, by="ID") 
basefinalheckit <- merge(basefinalheckit,heckitbase3, by ="ID")
basefinalheckit1 <- basefinalheckit |> select(ID,
                                              BExt,
                                              BLP,
                                              BExtAcess,
                                              volEmissao,
                                              BHG,
                                              volMedBonds,
                                              percLPAnoBonds,
                                              volMedDebs,
                                              percLPAnoDebs,
                                              spread,
                                              empresaId,
                                              emissao,
                                              BGarantia,
                                              BDebInc,
                                              BCall,
                                              riscoPais,
                                              pl,
                                              totalDebt) 
                                              
basefinalheckit1 |> glimpse()
### Modelo Heckit --------------------------------------------------------------
#modelHeckit <- summary(heckit(BExt>0 ~ BLP + BExtAcess + volEmissao + BHG + volMedBonds + percLPAnoBonds + volMedDebs + percLPAnoDebs,
#                             spread ~ empresaId  + emissao + BLP + BGarantia + volEmissao + BDebInc +	BCall + BHG + riscoPais,
#                             basefinalheckit
#                             ))


### Heckit através do Selection-------------
#summary(selection(selection= BExt>0 ~ BLP + BExtAcess + volEmissao + BHG + volMedBonds + percLPAnoBonds + volMedDebs + percLPAnoDebs,
#          outcome=spread ~ empresaId  + emissao + BLP + BGarantia + volEmissao + BDebInc +	BCall + BHG + riscoPais,
#          method="2step",
#          data=basefinalheckit,
#          ))

### probit e InvMills-------------------------------------------------------------
#Inversa de Mills de BExt = 1
modelprobitExt <- probit(BExt>0 ~ BLP + BExtAcess + volEmissao + BHG + volMedBonds + percLPAnoBonds + volMedDebs + percLPAnoDebs,
                      data=basefinalheckit1)
invMillsExt<-invMillsRatio(modelprobitExt)$IMR1


#Inversa de Mills de BGarantia = 1
modelprobitGar <- probit(BGarantia>0 ~ BExt + BLP + volEmissao +BCall +BHG + riscoPais + pl + totalDebt,
                      data=basefinalheckit1)
invMillsGar <- invMillsRatio(modelprobitGar)$IMR1


### Regressão em Painel---------------------------------------------------------
##base para regressão
baseAnos <- base1 %>% select(ID, `Data de Emissao`) %>% 
                      rename(anoEmissao=2) %>% 
                      mutate(anoEmissao = year(anoEmissao))
 
basepainel <- basefinalheckit |> mutate(invMillsExt = invMillsExt,
                                        invMillsGar = invMillsGar,
                                        volEmissao=volEmissao/100,
                                        volMedBonds=volMedBonds/100,
                                        volMedDebs=volMedDebs/100,
                                        )
basepainel <- merge(basepainel,baseAnos, by="ID")
basepainel <- basepainel |> mutate(emissao = emissao %>% as.factor(),
                                   BLP = BLP |> as.factor(),
                                   BGarantia=BGarantia |> as.factor(),
                                   BDebInc=BDebInc |> as.factor(),
                                   BCall=BCall |> as.factor(),
                                   BHG=BHG |> as.factor()
                                   )
                                  

basepainel |> glimpse()

#Sem efeitos fixo por emissor e sem controle de ano-----------------------------

pooling <- plm(spread ~  BLP + BGarantia + volEmissao + BDebInc + BCall + BHG + riscoPais+invMillsExt+invMillsGar,
              basepainel,
              model="pooling",
              index = "emissao")
resultPool <- summary(pooling)



#Com efeito fixo do emissor e sem controle de ano -------------------------------
fixoEmissor <- plm(spread ~ BLP + BGarantia + volEmissao + BDebInc + BCall + BHG + riscoPais+invMillsExt+invMillsGar,
               basepainel,
               model="within",
               effect = "twoways",
               index =c("emissao","empresaId"))
resultEmissor <- summary(fixoEmissor)


#outra visualização
#summary(lm(spread ~ emissao + BLP + BGarantia + volEmissao + BDebInc + BCall + BHG + riscoPais+invMills +as.factor(empresaId),
#           basepainel)) 


#Sem efeito fixo do emissor e com controle de ano
fixoAno <- plm(spread ~  BLP + BGarantia + volEmissao + BDebInc + BCall + BHG + riscoPais+invMillsExt+invMillsGar,
               basepainel,
               model="within",
               effect = "twoways",
               index =c("emissao","anoEmissao"))
resultAno <- summary(fixoAno)



#Com efeito do emissor e com controle de ano -------------------------------------------

fixoEmissorAno <-  plm(spread ~ factor(anoEmissao) + BLP + BGarantia + volEmissao + BDebInc + BCall + BHG + riscoPais+invMillsExt+invMillsGar,
                       basepainel,
                       model="within",
                       effect = "twoways",
                       index =c("emissao","empresaId"))

resultEmissorAno <- summary(fixoEmissorAno)



##Consolidando os resultados -----------------------------------------------------

dfPool <- resultPool$coefficients |> as.data.frame()
dfEmissor <- resultEmissor$coefficients |> as.data.frame()
dfAno <- resultAno$coefficients |> as.data.frame()
dfEmissorAno <- resultEmissorAno$coefficients |> as.data.frame()

#Todos os resultados do summary:

resultPool
resultEmissor
resultAno
resultEmissorAno

#somente os valores dos coefs:


dfPool |> filter(row.names(dfPool) %in% c('BLP1','BGarantia1','volEmissao','BDebInc1',
                                  'BCall1','BHG1','riscoPais','invMillsExt','invMillsGar' ))

dfEmissor |> filter(row.names(dfEmissor) %in% c('BLP1','BGarantia1','volEmissao','BDebInc1',
                                          'BCall1','BHG1','riscoPais','invMills'))

dfAno |> filter(row.names(dfAno) %in% c('BLP1','BGarantia1','volEmissao','BDebInc1',
                                                'BCall1','BHG1','riscoPais','invMills'))

dfEmissorAno |> filter(row.names(dfEmissorAno) %in% c('BLP1','BGarantia1','volEmissao','BDebInc1',
                                        'BCall1','BHG1','riscoPais','invMills'))


