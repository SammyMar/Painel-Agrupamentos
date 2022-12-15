source("global.R")

############ criando base de dados dos mapas ##################################

library(rgdal)
library(leaflet)
library(highcharter)
library(mapview)
library(htmlwidgets)
library(geobr)

### base de dados para construção dos mapas
caminho_mapas <-
  "br_municipios_20200807"

mapa_brasil <- readOGR(dsn = caminho_mapas,
                       layer = 'BR_Municipios_2019',
                       verbose = FALSE)

## ajustando formatação dos dados
#mapa_brasil@data$municipio <-
#  iconv(mapa_brasil@data$NM_MUN, from = 'UTF-8', to = 'ASCII//TRANSLIT') %>%
#  toupper()

mapa_brasil@data$codigo <-
  substring(mapa_brasil@data$CD_MUN, 1, nchar(mapa_brasil@data$CD_MUN) -
              1) %>%
  as.factor()

## municípios da base dos mapas não contidos na base de dados
#municipios_nao_encontrados <- mapa_brasil@data %>%
#  anti_join(dados, by = "codigo") %>%
#  select(codigo, municipio)

### cruzando dados dos mapas com a base de dados no estudo
mapa_dados <- mapa_brasil
mapa_dados@data <- mapa_brasil@data %>%
  left_join(dados[,-c(1:2)])

## criando variável identificadora do município
mapa_dados@data$id = rownames(mapa_dados@data)

## criando base de dados municipais
mapa_dados_pontos = ggplot2::fortify(mapa_dados, region = "id")

mapa_dados_df = dplyr::inner_join(mapa_dados_pontos,
                                  mapa_dados@data,
                                  by = "id")
saveRDS(mapa_dados_df, "mapa_dados_df.rds")

### gerando data frame para UF

caminho_uf <-
  "estados_2010/estados_2010"

mapa_uf <- readOGR(dsn = caminho_uf,
                   layer = 'estados_2010',
                   verbose = FALSE)
## ajustando formatação dos dados
mapa_uf@data$nome <-
  iconv(mapa_uf@data$nome, from = 'UTF-8', to = 'ASCII//TRANSLIT')

mapa_uf@data$uf <- mapa_uf@data$sigla %>%
  as.factor()

## criando base de dados estaduais
mapa_uf_pontos = ggplot2::fortify(mapa_uf, region = "id")

mapa_uf_df = dplyr::inner_join(mapa_uf_pontos,
                               mapa_uf@data,
                               by = "id")
saveRDS(mapa_uf_df, "mapa_uf_df.rds")