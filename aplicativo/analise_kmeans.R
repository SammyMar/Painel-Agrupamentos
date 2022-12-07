library(modelsummary)
require(writexl)
library(caTools)
library(dplyr)
library(rpart)
library(rpart.plot)
require(Lahman)
require(maptree)
library(readxl)

source("global.R")
source("script_mapas.R")
####################### K médias ##############################################
## ajustando k-médias com o número de grupos escolhido

set.seed(1122)
k_medias <- kmeans(dados_norm,
                   centers = 7)
table(k_medias$cluster)


dados$Grupo <- as.factor(k_medias$cluster)

socioeconomicos <- read_excel("indic_censo_2010_muni.xlsx")
socioeconomicos <- socioeconomicos %>%
  clean_names %>%
  mutate(codmun6 = as.factor(codmun6)) %>%
  select("ano",
         "uf",
         "codmun6",
         "municipio",
         "fectot",
         "gini",
         "rdpc",
         "idhm"
         )

socioeconomicos <- left_join(
  socioeconomicos ,
  dados[, c("codigo", "Grupo")],
  by = c("codmun6" = "codigo")
  )

# saveRDS(dados, "dados_tabelas_descr.rds")
# saveRDS(socioeconomicos, "dados_socioeconomicos.rds")

media <- function(x)
  mean(x, na.rm = TRUE)
mediana <- function(x)
  median(x, na.rm = TRUE)
dp <- function(x)
  sd(x, na.rm = TRUE)
minimo <- function(x)
  min(x, na.rm = TRUE)
maximo <- function(x)
  max(x, na.rm = TRUE)
n <- function(x)
  sum(!is.na(x))

datasummary(
  Grupo ~ (
    nascidos_vivos + porc_premat + porc_gesta_multipla + porc_cesarea +
      porc_0_consulta + porc_7mais_consulta + porc_apgar1_menor_7 +
      porc_apgar5_menor_7 + porc_anomalia + porc_peso_menor_2500 + porc_fem
  ) * (n + media + dp + minimo + mediana + maximo),
  data = dados
)

# Prematuridade
datasummary(
  Grupo ~ (porc_premat)* ((`n` = N) +
                          (`Média` = Mean) +
                          (`Desvio Padrão` = SD) +
                          (`Mínimo` = Min) +
                          (`Mediana` = Median) +
                          (`Máximo` = Max)),
  data = dados
)

((Avg. = Mean) + (Std.Dev. = SD))

# Gestação múltipla
datasummary(
  Grupo ~ (porc_gesta_multipla)* ((`n` = N) +
                                    (`Média` = Mean) +
                                    (`Desvio Padrão` = SD) +
                                    (`Mínimo` = Min) +
                                    (`Mediana` = Median) +
                                    (`Máximo` = Max)),
  data = dados
)

# Parto Cesária
datasummary(
  Grupo ~ (porc_cesarea)* ((`n` = N) +
                             (`Média` = Mean) +
                             (`Desvio Padrão` = SD) +
                             (`Mínimo` = Min) +
                             (`Mediana` = Median) +
                             (`Máximo` = Max)),
  data = dados
)

# Nenhuma consulta pré natal
datasummary(
  Grupo ~ (porc_0_consulta)* ((`n` = N) +
                                (`Média` = Mean) +
                                (`Desvio Padrão` = SD) +
                                (`Mínimo` = Min) +
                                (`Mediana` = Median) +
                                (`Máximo` = Max)),
  data = dados
)

# 7 ou mais consultas pré natal
datasummary(
  Grupo ~ (porc_7mais_consulta)* ((`n` = N) +
                                    (`Média` = Mean) +
                                    (`Desvio Padrão` = SD) +
                                    (`Mínimo` = Min) +
                                    (`Mediana` = Median) +
                                    (`Máximo` = Max)),
  data = dados
)

# Apgar no primeiro minuto menor que 7
datasummary(
  Grupo ~ (porc_apgar1_menor_7)* ((`n` = N) +
                                    (`Média` = Mean) +
                                    (`Desvio Padrão` = SD) +
                                    (`Mínimo` = Min) +
                                    (`Mediana` = Median) +
                                    (`Máximo` = Max)),
  data = dados
)

# Apgar no quinto minuto menor que 7
datasummary(
  Grupo ~ (porc_apgar5_menor_7)* ((`n` = N) +
                                    (`Média` = Mean) +
                                    (`Desvio Padrão` = SD) +
                                    (`Mínimo` = Min) +
                                    (`Mediana` = Median) +
                                    (`Máximo` = Max)),
  data = dados
)

# Anomalia congênita
datasummary(
  Grupo ~ (porc_anomalia)* ((`n` = N) +
                              (`Média` = Mean) +
                              (`Desvio Padrão` = SD) +
                              (`Mínimo` = Min) +
                              (`Mediana` = Median) +
                              (`Máximo` = Max)),
  data = dados
)

# Peso menor que 2500g
datasummary(
  Grupo ~ (porc_peso_menor_2500)* ((`n` = N) +
                                     (`Média` = Mean) +
                                     (`Desvio Padrão` = SD) +
                                     (`Mínimo` = Min) +
                                     (`Mediana` = Median) +
                                     (`Máximo` = Max)),
  data = dados
)

# Sexo feminino
datasummary(
  Grupo ~ (porc_fem)* ((`n` = N) +
                         (`Média` = Mean) +
                         (`Desvio Padrão` = SD) +
                         (`Mínimo` = Min) +
                         (`Mediana` = Median) +
                         (`Máximo` = Max)),
  data = dados
)
################ árvore de classificação ######################################

dados_arv <- dados %>%
  rename(
    premat = porc_premat,
    multipla = porc_gesta_multipla,
    cesarea = porc_cesarea,
    cons0 = porc_0_consulta,
    cons7 = porc_7mais_consulta,
    ap1 =  porc_apgar1_menor_7,
    ap5 = porc_apgar5_menor_7,
    anomalia = porc_anomalia,
    peso = porc_peso_menor_2500,
    fem = porc_fem
  )

arvore <- rpart(Grupo ~ ., data = dados_arv[, -c(1:4)])
prp(arvore)

################ construção mapa ##############################################
mapa_grups <-
  left_join(mapa_dados_df, dados[, c("codigo", "Grupo")])

#memory.limit(size = 100000)
mapa_cluster <-
  ggplot(mapa_grups %>%
           filter(codigo %in% dados$codigo)) +
  theme_minimal() +
  geom_polygon(aes(
    x = long,
    y = lat,
    group = group,
    fill = Grupo
  )) +
  coord_equal() +
  theme_minimal() +
  scale_fill_viridis_d("") +
  theme(legend.position = 'bottom') +
  ggtitle("Grupos") +
  geom_path(
    aes(long, lat, group = group),
    color = "black",
    size = 0.05,
    data = mapa_uf_df
  )

png(file = "mapa_cluster.png")
mapa_cluster
dev.off()

################## capitais ################################################### 
capitais <- dados %>%
  filter(
    codigo %in% c(110020,
                  130260,
                  120040,
                  500270,
                  160030,
                  530010,
                  140010,
                  510340,
                  172100,
                  355030,
                  221100,
                  330455,
                  150140,
                  520870,
                  292740,
                  420540,
                  211130,
                  270430,
                  431490,
                  410690,
                  310620,
                  230440,
                  261160,
                  250750,
                  280030,
                  240810,
                  320530)
  ) %>%
  mutate(
    regiao = ifelse(
      uf %in% c("RO", "AC", "AM", "RR", "PA", "AP", "TO"), "norte",
    ifelse(uf %in% c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA"),
           "nordeste", 
           ifelse(uf %in% c("MG", "ES", "RJ", "SP"), "sudeste",
                    ifelse(uf %in% c("PR", "SC", "RS"), "sul",
                           "centro-oeste")))))
###############################################################################

# Boxplot Nascidos vivos
box_nasc <- ggplot(dados[dados$codigo %in% socioeconomicos$codmun6, ]) +
  geom_boxplot(aes(Grupo, log(nascidos_vivos)), fill = "aquamarine4") +
  labs(y = "",
       x = "Grupo",
       title = "Nascidos Vivos (log)") +
  theme_classic()

# Boxplot Fecundidade Total
box_fectot <- ggplot( socioeconomicos ) +
  geom_boxplot(aes(Grupo, fectot), fill = "aquamarine4") +
  labs(y = "", 
       x = "Grupo",
       title = "Fecundidade Total") + 
  theme_classic()

# Boxplot Índice de Gini
box_gini <- ggplot( socioeconomicos ) +
  geom_boxplot(aes(Grupo, gini), fill = "aquamarine4") +
  labs(y = "", 
       x = "Grupo",
       title = " Índice de Gini") +
  theme_classic()

# Boxplot Renda per Capita Média
box_rdpc <- ggplot( socioeconomicos ) +
  geom_boxplot(aes(Grupo, rdpc), fill = "aquamarine4") +
  labs(y = "", 
       x = "Grupo",
       title = "Renda per Capita Média") + 
  theme_classic()

# Boxplot Índice de Desenvolvimento Humano Municipal
box_idhm <- ggplot( socioeconomicos ) +
  geom_boxplot(aes(Grupo, idhm), fill = "aquamarine4") +
  labs(y = "", 
       x = "Grupo", 
       title = " Índice de Desenvolvimento Humano Municipal") + 
  theme_classic()