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

require(writexl)
library(caTools)
library(rpart)
library(rpart.plot)
require(Lahman)
require(maptree)


dados <- readRDS("dados_tabelas_descr.rds")
socioeconomicos <- readRDS("dados_socioeconomicos.rds")


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
  title = "TCC MARIANA",
  dashboardHeader(
    title = strong('TCC MARIANA')
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
            fluidRow(
                box(
                  width = 12,
                  status = "primary",
                  solidHeader = FALSE,
                  collapsible = TRUE,
                  h4(strong("Medidas descritivas dos indicadores obstétricos por grupo"),
                     style = "font-size:50px;"),
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
                      tabBox(tabPanel(htmlOutput("table1"))), width = 6)
                ),
                box(
                  width = 12,
                  status = "primary",
                  solidHeader = FALSE,
                  collapsible = TRUE,
                  h4(strong("Análise socioeconômica por grupo"),
                     style = "font-size:50px;"),
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
                    column(
                      plotOutput("plot1"), width = 6)
                )
                ))
              ))
    )



############################# Server ########################################

server <- function(input, output, session) {


################ Tabelas descritivas ####################
dado1 <- reactive({
    dados_aux <- dados
    var <- input$tabbox1
    names(dados_aux)[names(dados_aux) == var] <- 'var'
    dados_aux
 })
  
output$table1 <- renderPrint({
  var <- input$tabbox1
  st_options(headings = FALSE, display.labels = FALSE)
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

  
##################### Boxplot ##########################

output$plot1 <- renderPlot({
  ggplot(dados[dados$codigo %in% socioeconomicos$codmun6, ]) +
    geom_boxplot(aes(Grupo, log(nascidos_vivos)), fill = "aquamarine4") +
    labs(y = "",
         x = "Grupo",
         title = "Nascidos Vivos (log)") +
    theme_classic()
})

output$plot2 <- renderPlot({
  ggplot( socioeconomicos ) +
    geom_boxplot(aes(Grupo, fectot), fill = "aquamarine4") +
    labs(y = "", 
         x = "Grupo",
         title = "Fecundidade Total") + 
    theme_classic()
})

output$plot3 <- renderPlot({
  ggplot( socioeconomicos ) +
    geom_boxplot(aes(Grupo, gini), fill = "aquamarine4") +
    labs(y = "", 
         x = "Grupo",
         title = " Índice de Gini") +
    theme_classic()
})

output$plot4 <- renderPlot({
  ggplot( socioeconomicos ) +
    geom_boxplot(aes(Grupo, rdpc), fill = "aquamarine4") +
    labs(y = "", 
         x = "Grupo",
         title = "Renda per Capita Média") + 
    theme_classic()
})

output$plot5 <- renderPlot({
  ggplot( socioeconomicos ) +
    geom_boxplot(aes(Grupo, idhm), fill = "aquamarine4") +
    labs(y = "", 
         x = "Grupo", 
         title = " Índice de Desenvolvimento Humano Municipal") + 
    theme_classic()
})





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
