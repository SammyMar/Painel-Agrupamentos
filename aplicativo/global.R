### carregando pacotes necessários
loadlibrary <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = T)
    if (!require(x, character.only = TRUE))
      stop("Package not found")
  }
}
packages <- c(
  "readxl",
  "janitor",
  "dplyr",
  "tidyverse",
  "skimr",
  "forcats",
  "stringr",
  "lubridate",
  "readr",
  "ggplot2",
  "summarytools",
  "factoextra",
  "psych",
  "gridExtra",
  "Rcpp"
)
lapply(packages, loadlibrary)

### carregando dados
dados <- readRDS("dados_2019.rds")

### limpando dados
municipios_ignorados <- dados %>%
  filter(str_detect(municipio, "MUNICIPIO IGNORADO"))

dados <- dados %>%
  filter(!municipio %in% municipios_ignorados$municipio) %>%
  select(
    uf,
    municipio,
    codigo,
    nascidos_vivos,
    starts_with("porc")
  ) %>%
  mutate(
    uf = as.factor(uf),
    codigo = as.factor(codigo),
    porc_peso_menor_2500 = ifelse(is.na(porc_peso_menor_2500) == TRUE,
                                  0,
                                  porc_peso_menor_2500)
  )


## dados normalizados
dados_norm <- as.data.frame(scale(dados[,-c(1:4)]))

## matriz de dissimilaridade
dados_diss <- dist(scale(dados[, -c(1:4)]), method = "euclidean")