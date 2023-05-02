### Bibliotecas Importadas -----------------------------------------------------

library(tidyverse)
library(sampleSelection)

### Tratamento dos Dados -------------------------------------------------------
## Base2
base2 <- readxl::read_xlsx("Base2PreliminarTratada.xlsx")

heckitbase2 <- base2 |> select(`Empresa (j)`, βLP, βExtAccess, `Volume R$ MM`, βHighGrade, 
                `Volume Médio Mercado Bonds R$ MM`,`LP Mercado Bonds (>5 anos)`,
                `Volume Médio Debs R$ MM`,`LP Debs (>5 anos)`, Ativo)
heckitbase2 <- heckitbase2 |> rename(empresaId=1,
                                BLP=2,
                                BExtAcess=3,
                                volEmissao=4,
                                BHG=5,
                                volMedBonds=6,
                                percLPAnoBonds=7,
                                volMedDebs=8,
                                percLPAnoDebs=9,
                                ativo=10)
heckitbase2 <- heckitbase2 |> mutate(empresaId = empresaId |> as.factor(),
                                BLP = BLP |> as.factor(),
                                BExtAcess = BExtAcess |> as.factor(),
                                BHG = BHG |> as.factor(),
                                ativo = if_else(ativo == "Bonds", 1,0) 
                                )


##Base1
base1 <- readxl::read_xlsx("BasePreliminarTratada.xlsx")
base1 |> glimpse()

heckitbase1 <- base1 |> select (`Empresa (j)`,`βExt`,`Emissao (i)`, `βLP`,
                                `βGarantia`,`Volume R$ MM`, `Spread DI+`,
                                `βDebInc`,`βCall`, `βHighGrade`, RiscoPaís) 


###Base Final
basefinalheckit <- merge(heckitbase2, heckitbase1) 
basefinalheckit |> glimpse()

### Modelos --------------------------------------------------------------------
basefinalheckit <- heckitbase2 |> select(-empresaId)
summary(heckit(ativo ~ BLP + BExtAcess + volEmissao + BHG + volMedBonds + percLPAnoBonds + volMedDebs + percLPAnoDebs,
                          `Spread DI+` ~ `Volume R$ MM` + RiscoPaís  + `Emissao (i)` + volEmissao + βGarantia,
                          basefinalheckit
                          ))

heckit5fit()
tobit5fit()
