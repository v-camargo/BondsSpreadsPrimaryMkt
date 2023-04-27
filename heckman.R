### Bibliotecas Importadas -----------------------------------------------------

library(tidyverse)
library(sampleSelection)

### Tratamento dos Dados -------------------------------------------------------

base2 <- readxl::read_xlsx("Base2PreliminarTratada.xlsx")

heckit <- base2 |> select(`Empresa (j)`, βLP, βExtAccess, `Volume R$ MM`, βHighGrade, 
                `Volume Médio Mercado Bonds R$ MM`,`LP Mercado Bonds (>5 anos)`,
                `Volume Médio Debs R$ MM`,`LP Debs (>5 anos)`, Ativo)
heckit <- heckit |> rename(empresaId=1,
                 BLP=2,
                 BExtAcess=3,
                 volEmissao=4,
                 BHG=5,
                 volMedBonds=6,
                 percLPAnoBonds=7,
                 volMedDebs=8,
                 percLPAnoDebs=9,
                 ativo=10)
heckit <- heckit |> mutate(empresaId = empresaId |> as.factor(),
                 BLP = BLP |> as.factor(),
                 BExtAcess = BExtAcess |> as.factor(),
                 BHG = BHG |> as.factor(),
                 ativo = if_else(ativo == "Bonds", 1,0) 
                 )
baseheckit <- heckit |> select(-empresaId)
baseheckit |> heckit(selection(ativo ~ BLP + BExtAcess + volEmissao + BHG + volMedBonds + percLPAnoBonds + volMedDebs + percLPAnoDebs),
                     ativo ~ BLP + BExtAcess + volEmissao + BHG + volMedBonds + percLPAnoBonds + volMedDebs + percLPAnoDebs)
