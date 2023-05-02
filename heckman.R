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
base2 <- readxl::read_xlsx("Base2PreliminarTratada.xlsx")

heckitbase2 <- base2 |> select(ID,`Empresa (j)`, βLP, βExtAccess, `Volume R$ MM`, βHighGrade, 
                `Volume Médio Mercado Bonds R$ MM`,`LP Mercado Bonds (>5 anos)`,
                `Volume Médio Debs R$ MM`,`LP Debs (>5 anos)`, Ativo)
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
base1 <- readxl::read_xlsx("BasePreliminarTratada.xlsx")

heckitbase1 <- base1 |> select (ID,`Empresa (j)`,`βExt`,`Emissao (i)`, `βLP`,
                                `βGarantia`,`Volume R$ MM`, `Spread DI+`,
                                `βDebInc`,`βCall`, `βHighGrade`, RiscoPaís) 

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


##Base Final -------------------------------------------------------------------
heckitbase2f <- heckitbase2
heckitbase1f <- heckitbase1 %>% select(ID, emissao, BGarantia, spread, BDebInc, BCall, riscoPais)
basefinalheckit <- merge(heckitbase2f, heckitbase1f, by="ID") 
basefinalheckit %>% glimpse()


### Modelo Heckit --------------------------------------------------------------
modelHeckit <- summary(heckit(BExt ~ BLP + BExtAcess + volEmissao + BHG + volMedBonds + percLPAnoBonds + volMedDebs + percLPAnoDebs,
                             spread ~ empresaId  + emissao + BLP + BGarantia + volEmissao + BDebInc +	BCall + BHG + riscoPais,
                             basefinalheckit
                             ))
modelHeckit
##Output de interesse: Razão Inversa de Mills
invMillsRatio <- modelHeckit$estimate["invMillsRatio", "Estimate"]


### Modelo tobit ---------------------------------------------------------------
basefinaltobit <- basefinalheckit %>% mutate(invMillsRatio1 = invMillsRatio,
                                             BExt = if_else(BExt=="Bonds", 1,0),
                                             BGarantia = if_else(BGarantia =="S", 1,0) ) 


summary(selection(BExt<1 ~ BLP + BExtAcess + volEmissao + BHG + volMedBonds + percLPAnoBonds + volMedDebs + percLPAnoDebs,
                  spread ~ empresaId  + emissao + BLP + BGarantia + volEmissao + BDebInc + BCall + BHG + riscoPais,
                  basefinaltobit
                  ))

#como p-valor da Razão inversa de mills é alto, 
#não conseguimos refutar a hipótes nula de que não há viés de seleção, 
#então rodaremos a regressão em painel em a RIM


### Regressão em Painel---------------------------------------------------------
#Sem efeitos fixos--------------------------------------------------------------
pooling <- plm(spread ~ emissao + BLP + BGarantia + volEmissao + BDebInc + BCall + BHG + riscoPais,
              basefinaltobit,
              model="pooling")
summary(pooling)


#Considerando efeitos fixos somente das empresas -------------------------------
fixo <- plm(spread ~ emissao + BLP + BGarantia + volEmissao + BDebInc + BCall + BHG + riscoPais,
               basefinaltobit,
               model="within",
               index ="empresaId" )
summary(fixo)
summary(fixef(fixo))

#outra visualização
summary(lm(spread ~ emissao + BLP + BGarantia + volEmissao + BDebInc + BCall + BHG + riscoPais + as.factor(empresaId),
           basefinaltobit)) 

#considerando efeitos fixos dos anos -------------------------------------------

base1ano <- base1 %>% select(ID, `Data de Emissao`) %>% 
                      rename(anoEmissao=2) %>% 
                      mutate(anoEmissao = year(anoEmissao))
basepainelano <- merge(basefinaltobit,base1ano, by="ID") 
basepainelano %>% glimpse()

summary(lm(spread ~ emissao + BLP + BGarantia + volEmissao + BDebInc + BCall + BHG + riscoPais + as.factor(empresaId) + as.factor(anoEmissao),
           basepainelano)) 



