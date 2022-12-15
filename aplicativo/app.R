library(shiny)
library(dplyr)
library(magrittr)
library(readxl)
library(shinydashboard)
library(questionr)
library(kableExtra)
library(ggplot2)
library(highcharter)
library(summarytools)
library(modelsummary)
library(leaflet)
library(leaflet.extras)
library(stringr)
library(reactable)
library(htmltools)
library(zoo)
library(plotly)
library(lubridate)
library(googlesheets4)
library(shinyjs)
library(spsComps)

require(writexl)
library(caTools)
library(rpart)
library(rpart.plot)
require(Lahman)
require(maptree)


dados <- readRDS("dados_tabelas_descr.rds")
# socioeconomicos <- readRDS("dados_socioeconomicos.rds")
# mapa_dados_df <- readRDS("mapa_dados_df.rds")
# mapa_uf_df <- readRDS("mapa_uf_df.rds")
# 
# mapa_grups <-
#   left_join(mapa_dados_df, dados[, c("codigo", "Grupo")])

sticky_style <-
  list(
    position = "sticky",
    left = 0,
    background = "#fff",
    zIndex = 1,
    borderRight = "1px solid #eee"
  )

hoje <- Sys.Date()


# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# get current Epoch time
epochTime <- function() {
  return(as.integer(Sys.time()))
}

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

table <- "responses"

saveData <- function(data) {
  data <- data  %>% as.list() %>% data.frame()
  Selfie <- gs4_get('https://docs.google.com/spreadsheets/d/1Zs3jMI3CKr637QBGaveP8_doGEVW-ZaGtw97KnAqa7E/edit?usp=sharing')
  sheet_append(Selfie, data)
}

# directory where responses get stored
responsesDir <- file.path("responses")

# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "


# User interface ----

ui <-  dashboardPage(
  title = "Agrupamento por K-médias dos municípios brasileiros que se
assemelham quanto a indicadores obstétricos",
  dashboardHeader(
    title = strong('OOBr')
    ),
  dashboardSidebar(
    disable = TRUE,
    sidebarMenu(
    style = "position: fixed; overflow: visible",
    menuItem("Medidas descritivas", tabName = "ac")
  )),
  dashboardBody(
    tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #0A1E3C;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #0A1E3C;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #0A1E3C;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #0A1E3C;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #32A0FF;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #0A1E3C;
                              color: #FFFFFF;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #32A0FF;
                              }
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #32A0FF;
                              }
                              '),
                         HTML("hr {border-top: 1px solid #0A1E3C;}")
                         )),
    tabItems(
      tabItem(tabName = "ac",
              h1(strong(
              "Agrupamento por K-médias dos municípios brasileiros que se assemelham quanto a indicadores obstétricos"
              )),
              br(),
            fluidRow(
              align = "center",
              box(
                width = 12,
                status = "primary",
                solidHeader = FALSE,
                collapsible = TRUE,
                h4(strong("Mapas dos municípios para o número de nascidos vivos e para os indicadores obstétricos"),
                   style = "font-size:30px;",
                   align = "center"),
                selectInput(
                  "chart0",
                  "",
                  c(
                    "Nascidos vivos" = "nascidos_vivos",
                    "Prematuridade" = "porc_premat",
                    "Gestação múltipla" = "porc_gesta_multipla",
                    "Parto Cesária" = "porc_cesarea",
                    "Nenhuma consulta pré natal" = "porc_0_consulta",
                    "7 ou mais consultas pré natal" = "porc_7mais_consulta",
                    "Apgar no primeiro minuto menor que 7" = "porc_apgar1_menor_7",
                    "Apgar no quinto minuto menor que 7" = "porc_apgar5_menor_7",
                    "Anomalia congênita" = "porc_anomalia",
                    "Peso menor que 2500g" = "porc_peso_menor_2500",
                    "Sexo feminino" = "porc_fem"
                  )
                ),
                tags$head(tags$style(HTML(
                  ".selectize-input {height: 40px; width: 550px; font-size: 25px;}"
                ))),
                shiny::conditionalPanel(
                  condition = "input.chart0 == 'nascidos_vivos'",
                  column(
                    offset = 3,
                    align = "center",
                    imageOutput("img0"), 
                    width = 6)
                ),
                shiny::conditionalPanel(
                  condition = "input.chart0 == 'porc_premat'",
                  column(
                    offset = 3,
                    align = "center",
                    imageOutput("img1"), 
                    width = 6)
                ),
                shiny::conditionalPanel(
                  condition = "input.chart0 == 'porc_gesta_multipla'",
                  column(
                    offset = 3,
                    align = "center",
                    imageOutput("img2"), 
                    width = 6)
                ),
                shiny::conditionalPanel(
                  condition = "input.chart0 == 'porc_cesarea'",
                  column(
                    offset = 3,
                    align = "center",
                    imageOutput("img3"), 
                    width = 6)
                ),
                shiny::conditionalPanel(
                  condition = "input.chart0 == 'porc_0_consulta'",
                  column(
                    offset = 3,
                    align = "center",
                    imageOutput("img4"), 
                    width = 6)
                ),
                shiny::conditionalPanel(
                  condition = "input.chart0 == 'porc_7mais_consulta'",
                  column(
                    offset = 3,
                    align = "center",
                    imageOutput("img5"), 
                    width = 6)
                ),
                shiny::conditionalPanel(
                  condition = "input.chart0 == 'porc_apgar1_menor_7'",
                  column(
                    offset = 3,
                    align = "center",
                    imageOutput("img6"), 
                    width = 6)
                ),
                shiny::conditionalPanel(
                  condition = "input.chart0 == 'porc_apgar5_menor_7'",
                  column(
                    offset = 3,
                    align = "center",
                    imageOutput("img7"), 
                    width = 6)
                ),
                shiny::conditionalPanel(
                  condition = "input.chart0 == 'porc_anomalia'",
                  column(
                    offset = 3,
                    align = "center",
                    imageOutput("img8"), 
                    width = 6)
                ),
                shiny::conditionalPanel(
                  condition = "input.chart0 == 'porc_peso_menor_2500'",
                  column(
                    offset = 3,
                    align = "center",
                    imageOutput("img9"), 
                    width = 6)
                ),
                shiny::conditionalPanel(
                  condition = "input.chart0 == 'porc_fem'",
                  column(
                    offset = 3,
                    align = "center",
                    imageOutput("img10"), 
                    width = 6)
                )
              )),
            fluidRow(
              #align = "center",
                box(
                  width = 6,
                  id = "box_l",
                  status = "primary",
                  solidHeader = FALSE,
                  collapsible = TRUE,
                  h4(strong("Indicadores obstétricos por cluster"),
                     style = "font-size:30px;"),
                  selectInput(
                    "tabbox1",
                    "",
                    c(
                      "Prematuridade" = "porc_premat",
                      "Gestação múltipla" = "porc_gesta_multipla",
                      "Parto Cesária" = "porc_cesarea",
                      "Nenhuma consulta pré natal" = "porc_0_consulta",
                      "7 ou mais consultas pré natal" = "porc_7mais_consulta",
                      "Apgar no primeiro minuto menor que 7" = "porc_apgar1_menor_7",
                      "Apgar no quinto minuto menor que 7" = "porc_apgar5_menor_7",
                      "Anomalia congênita" = "porc_anomalia",
                      "Peso menor que 2500g" = "porc_peso_menor_2500",
                      "Sexo feminino" = "porc_fem"
                    )
                  ),
                  tags$head(tags$style(HTML(
                    ".selectize-input {height: 40px; width: 500px; font-size: 25px;}"
                    ))),
                  column(
                      #verbatimTextOutput("table1"),
                      tabBox(tabPanel(htmlOutput("table1"))),
                      #tableOutput("table1"), 
                      width = 6)
                ),
                  # column(
                  #   plotOutput("plot0"), width = 6)
                  box(
                    width = 6,
                    id= "box_r",
                    status = "primary",
                    solidHeader = FALSE,
                    collapsible = TRUE,
                    h4(strong("Mapa dos clusters obtidos"),
                       style = "font-size:30px;",
                       align = "center"),
                    imageOutput("img12")
                    ),
                spsComps::heightMatcher("box_r", "box_l")
                ),
            fluidRow(
              #align = "center",
              box(
                width = 6,
                id = "box_j",
                status = "primary",
                solidHeader = FALSE,
                collapsible = TRUE,
                h4(strong("Análise socioeconômica por cluster"),
                   style = "font-size:30px;" #,
                   #align = "center"
                   ),
                selectInput(
                  "chart1",
                  "",
                  c(
                    "Nascidos vivos" = "nascidos_vivos",
                    "Fecundidade Total" = "fectot",
                    "Índice de Gini" = "gini",
                    "Renda per Capita Média" = "rdpc",
                    "Índice de Desenvolvimento Humano Municipal" = "idhm"
                    )
                  ),
                  tags$head(tags$style(HTML(
                    ".selectize-input {height: 40px; width: 550px; font-size: 25px;}"
                  ))),
                  shiny::conditionalPanel(
                    condition = "input.chart1 == 'nascidos_vivos'",
                    column(
                      #offset = 3,
                      #align = "center",
                      #plotOutput("plot1"), 
                      imageOutput("plot1"),
                      width = 6)
                  ),
                  shiny::conditionalPanel(
                    condition = "input.chart1 == 'fectot'",
                    column(
                      #offset = 3,
                      #align = "center",
                      #plotOutput("plot2"), 
                      imageOutput("plot2"),
                      width = 6)
                  ),
                  shiny::conditionalPanel(
                    condition = "input.chart1 == 'gini'",
                    column(
                      #offset = 3,
                      #align = "center",
                      #plotOutput("plot3"), 
                      imageOutput("plot3"),
                      width = 6)
                  ),
                  shiny::conditionalPanel(
                    condition = "input.chart1 == 'rdpc'",
                    column(
                      #offset = 3,
                      #align = "center",
                      #plotOutput("plot4"), 
                      imageOutput("plot4"),
                      width = 6)
                  ),
                  shiny::conditionalPanel(
                    condition = "input.chart1 == 'idhm'",
                    column(
                      #offset = 3,
                      #align = "center",
                      #plotOutput("plot5"),
                      imageOutput("plot5"),
                      width = 6)
                  )
                ),
              box(
                width = 6,
                id = "box_f",
                status = "primary",
                solidHeader = FALSE,
                collapsible = TRUE,
                h4(strong("Árvore de decisão dos clusters"),
                   style = "font-size:30px;",
                   align = "center"),
                imageOutput("img13")
              ),
              spsComps::heightMatcher("box_f", "box_j")
              )
                ))
              ))
    



############################# Server ########################################

server <- function(input, output, session) {


################ Tabelas e graficos ####################

output$img0 <- renderImage({
    list(src = "mapa_nascidos_vivos.png", 
         height = 400, 
         width = 550,
         style = "display: block; margin-left: auto; margin-right: auto;")
}, deleteFile = FALSE)

output$img1 <- renderImage({
  list(src = "mapa_porc_premat.png", 
       height = 400, 
       width = 550,
       style = "display: block; margin-left: auto; margin-right: auto;")
}, deleteFile = FALSE)

output$img2 <- renderImage({
  list(src = "mapa_porc_gesta_multipla.png", 
       height = 400, 
       width = 550,
       style = "display: block; margin-left: auto; margin-right: auto;")
}, deleteFile = FALSE)

output$img3 <- renderImage({
  list(src = "mapa_porc_cesarea.png", 
       height = 400, 
       width = 550,
       style = "display: block; margin-left: auto; margin-right: auto;")
}, deleteFile = FALSE)

output$img4 <- renderImage({
  list(src = "mapa_porc_0_consulta.png", 
       height = 400, 
       width = 550,
       style = "display: block; margin-left: auto; margin-right: auto;")
}, deleteFile = FALSE)

output$img5 <- renderImage({
  list(src = "mapa_porc_7mais_consulta.png", 
       height = 400, 
       width = 550,
       style = "display: block; margin-left: auto; margin-right: auto;")
}, deleteFile = FALSE)

output$img6 <- renderImage({
  list(src = "mapa_porc_apgar1_menor_7.png", 
       height = 400, 
       width = 550,
       style = "display: block; margin-left: auto; margin-right: auto;")
}, deleteFile = FALSE)

output$img7 <- renderImage({
  list(src = "mapa_porc_apgar5_menor_7.png", 
       height = 400, 
       width = 550,
       style = "display: block; margin-left: auto; margin-right: auto;")
}, deleteFile = FALSE)

output$img8 <- renderImage({
  list(src = "mapa_porc_anomalia.png", 
       height = 400, 
       width = 550,
       style = "display: block; margin-left: auto; margin-right: auto;")
}, deleteFile = FALSE)

output$img9 <- renderImage({
  list(src = "mapa_porc_peso_menor_2500.png", 
       height = 400, 
       width = 550,
       style = "display: block; margin-left: auto; margin-right: auto;")
}, deleteFile = FALSE)

output$img10 <- renderImage({
  list(src = "mapa_porc_fem.png", 
       height = 400, 
       width = 550,
       style = "display: block; margin-left: auto; margin-right: auto;")
}, deleteFile = FALSE)
  
dado1 <- reactive({
    dados_aux <- dados
    var <- input$tabbox1
    names(dados_aux)[names(dados_aux) == var] <- 'var'
    dados_aux
 })
  
output$table1 <- renderPrint({
  var <- input$tabbox1
  st_options(headings = FALSE, display.labels = FALSE, use.x11 = FALSE)
  modelsummary::datasummary(
    Grupo ~ (var)* ((`n` = N) +
                            (`Média` = Mean) +
                            ( DP = SD) +
                            (`Mínimo` = Min) +
                            (`Mediana` = Median) +
                            (`Máximo` = Max)),
    data = dado1(),
    output = 'html'
    ) 
})

output$img12 <- renderImage({
  list(src = "mapa_cluster.png", 
       height = 450, 
       width = 525,
       style = "display: block; margin-left: auto; margin-right: auto;")
  }, deleteFile = FALSE)

output$img13 <- renderImage({
  list(src = "arvore.png", 
       height = 480, 
       width = 600,
       style = "display: block; margin-left: auto; margin-right: auto;")
}, deleteFile = FALSE)

# output$plot0 <- renderPlot({
#   ggplot(mapa_grups %>%
#            filter(codigo %in% dados$codigo)) +
#     theme_minimal() +
#     geom_polygon(aes(
#       x = long,
#       y = lat,
#       group = group,
#       fill = Grupo
#     )) +
#     coord_equal() +
#     theme_minimal() +
#     scale_fill_viridis_d("") +
#     theme(legend.position = 'bottom') +
#     ggtitle("Grupos") +
#     geom_path(
#       aes(long, lat, group = group),
#       color = "black",
#       size = 0.05,
#       data = mapa_uf_df
#     )
# })

##################### Boxplot ##########################

output$plot1 <- renderImage({
  list(src = "box_nasc.png", 
       height = 400, 
       width = 625,
       style = "display: block; margin-left: auto; margin-right: auto;")
}, deleteFile = FALSE)

output$plot2 <- renderImage({
  list(src = "box_fectot.png", 
       height = 400, 
       width = 625,
       style = "display: block; margin-left: auto; margin-right: auto;")
}, deleteFile = FALSE)

output$plot3 <- renderImage({
  list(src = "box_gini.png", 
       height = 400, 
       width = 625,
       style = "display: block; margin-left: auto; margin-right: auto;")
}, deleteFile = FALSE)

output$plot4 <- renderImage({
  list(src = "box_rdpc.png", 
       height = 400, 
       width = 625,
       style = "display: block; margin-left: auto; margin-right: auto;")
}, deleteFile = FALSE)

output$plot5 <- renderImage({
  list(src = "box_idhm.png", 
       height = 400, 
       width = 625,
       style = "display: block; margin-left: auto; margin-right: auto;")
}, deleteFile = FALSE)

# output$plot1 <- renderPlot({
#   ggplot(dados[dados$codigo %in% socioeconomicos$codmun6, ]) +
#     geom_boxplot(aes(Grupo, log(nascidos_vivos)), fill = "aquamarine4") +
#     labs(y = "",
#          x = "Grupo",
#          title = "Nascidos Vivos (log)") +
#     theme_classic()
# })
# 
# output$plot2 <- renderPlot({
#   ggplot( socioeconomicos ) +
#     geom_boxplot(aes(Grupo, fectot), fill = "aquamarine4") +
#     labs(y = "", 
#          x = "Grupo",
#          title = "Fecundidade Total") + 
#     theme_classic()
# })
# 
# output$plot3 <- renderPlot({
#   ggplot( socioeconomicos ) +
#     geom_boxplot(aes(Grupo, gini), fill = "aquamarine4") +
#     labs(y = "", 
#          x = "Grupo",
#          title = " Índice de Gini") +
#     theme_classic()
# })
# 
# output$plot4 <- renderPlot({
#   ggplot( socioeconomicos ) +
#     geom_boxplot(aes(Grupo, rdpc), fill = "aquamarine4") +
#     labs(y = "", 
#          x = "Grupo",
#          title = "Renda per Capita Média") + 
#     theme_classic()
# })
# 
# output$plot5 <- renderPlot({
#   ggplot( socioeconomicos ) +
#     geom_boxplot(aes(Grupo, idhm), fill = "aquamarine4") +
#     labs(y = "", 
#          x = "Grupo", 
#          title = " Índice de Desenvolvimento Humano Municipal") + 
#     theme_classic()
# })





######## Configuration ######## 
# Gather all the form inputs (and add timestamp)
formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- c(data, timestamp = humanTime())
    data <- t(data)
    data
})    
  
# When the Submit button is clicked, submit the response
observeEvent(input$submit, {
    
    # User-experience stuff
    shinyjs::disable("submit")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    
    # Save the data (show an error message in case of error)
    tryCatch({
      saveData(formData())  
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    },
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error", anim = TRUE, animType = "fade")
    },
    finally = {
      shinyjs::enable("submit")
      shinyjs::hide("submit_msg")
    })
})
  
# submit another response
observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
})
  
  
}

shinyApp(ui, server)
