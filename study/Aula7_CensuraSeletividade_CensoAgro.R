# CE731-Econometria II
# Prof. Alexandre Gori Maia
# Instituto de Economia - UNICAMP
# Aula 7: Censura e Seletividade

# Amostra de 333 produtores do Censo Agropecuário 2006
# As análises baseiam-se nos resultados da publicação:
#     MAIA, A. G., CESANO, D., MIYAMOTO, B. C, EUSEBIO, G. SILVA, P. O. 
#     Climate change and farm-level adaptation: the Braziian Sertão.
#     International Journal of Climate Change Strategies and Management, v.10, n. 5, pp. 729-751,  2018.
#     Disponível em: https://doi.org/10.1108/IJCCSM-04-2017-0088 

# Variáveis de interesse:
#  VT_PRODUCAO - valor total da produção agrícola no ano (R$)
#  AREA_TOTAL - área total do estabelecimentos (hectares)
#  TOTAL_FAMILIARES - total de membros familiares ocupados na atividade agrícola
library(tidyverse)
install.packages('plotly')
library(plotly)

# biblioteca para modelo TOBIT
library(VGAM)

# bibliotecas para método Heckit
library(sampleSelection)

# Leitura de arquivo de produtores
censo <- read.csv("Data_AgriculturalCensus06.csv")
censotest <- censo

# análise descritiva de todas as variáveis do arquivo
summary(censo)
censo |> glimpse()
censo |> head()

# Ajusta modelo linear para produção total por MQO 
# Há propriedades com valor zero da produção: censura à esquerda no zero
mqocensura <- lm(VT_PRODUCAO ~ AREA_TOTAL + TOTAL_FAMILIARES, data=censo)
summary(mqocensura)

#testando gráficos
plot(mqocensura)
yhat <- predict(mqocensura) |> as.data.frame()
censotest <- censotest |>  mutate(predict = predict(mqocensura))
p <- censotest |> select(AREA_TOTAL, TOTAL_FAMILIARES, predict)
plot_ly(p, x = ~AREA_TOTAL, y = ~TOTAL_FAMILIARES, z = ~predict, type = "scatter3d")


# Ajusta modelo Tobit para produção total: censura à esquerda no zero
modelotobit <- vglm(VT_PRODUCAO ~ AREA_TOTAL + TOTAL_FAMILIARES, tobit(Lower=0), data=censo)
summary(modelotobit)


# Ajusta modelo semi logaritmico para produção total por MQO 
# Elimina observações com valor zero para produção total: truncamento no zero
censo.truncamento <- subset(censo, VT_PRODUCAO>0)
mqotruncamento <- lm(log(VT_PRODUCAO) ~ log(AREA_TOTAL) + TOTAL_FAMILIARES, data=censo.truncamento)
summary(mqotruncamento)

# binária para modelo de seleção: 1 se valor da produção > 0
censo$produziu <- (censo$VT_PRODUCAO>0)

# Ajusta modelo Heckit para produção total
modeloheckit <- heckit(produziu ~ log(AREA_TOTAL) + TOTAL_FAMILIARES, 
                      log(VT_PRODUCAO) ~ log(AREA_TOTAL) + TOTAL_FAMILIARES, 
                      data=censo)
summary(modeloheckit)


