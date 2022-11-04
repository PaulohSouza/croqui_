library(shiny)
library(shinythemes) 
library(tidyverse)
library(readxl)
library(AgroR)
library(DT)
library(shinycssloaders)
library(colourpicker)
library(shinyWidgets)
library(agricolae)
library(gridExtra)
library(grid)
library(dplyr)
library(agricolae)
library(tidyverse)
library(stringr)

rm(list=ls())


options(shiny.sanitize.errors = FALSE)


######################################################
#### Modelo base para relatório com input de dados ###
######################################################
source('global.R')

ui <-navbarPage("FMT Croqui", windowTitle = 'FMT Croqui', collapsible = TRUE, theme = shinytheme("paper"),
                
                
                tabPanel
                ("INÍCIO",
                  includeCSS("palatino.css"),
                  " ",
                  sidebarLayout (
                    sidebarPanel (
                      tags$hr(),
                      
                      ######################################################
                      #              scripts dos botoes avancar            #
                      ######################################################
                      (tags$head(tags$script('
                                           Shiny.addCustomMessageHandler("myCallbackHandler",
                                           function(typeMessage) {console.log(typeMessage)
                                           
                                           if(typeMessage == "ACT_CONJUNTO_TO_BD"){
                                           $("a:contains(Definir delineamento)").click();
                                           }
                                           
                                                                     
                                           if(typeMessage == "DELINEAMENTO_PARA_CROQUI"){
                                           $("a:contains(Delineamento inteiramente casualizado (DIC))").click();
                                           }
                                           
                                                                                      
                                           if(typeMessage == "DADOS_PARA_INICIO"){
                                           $("a:contains(INÍCIO)").click();
                                           }
                                           
                                           if(typeMessage == "FDBC_PARA_INICIO"){
                                           $("a:contains(INÍCIO)").click();
                                           }
                                           
                                           if(typeMessage == "DBC_PARA_INICIO"){
                                           $("a:contains(INÍCIO)").click();
                                           }
                                           
                                           
                                           if(typeMessage == "Delineamento inteiramente casualizado (DIC)"){
                                           $("a:contains(Delineamento inteiramente casualizado (DIC))").click();
                                           }
                                           
                                                                                      
                                           if(typeMessage == "Delineamento em blocos casualizados (DBC)"){
                                           $("a:contains(Delineamento em blocos casualizados (DBC))").click();
                                           }


                                           if(typeMessage == "Delineamento em quadrado latino (DQL)"){
                                           $("a:contains(Delineamento em quadrado latino (DQL))").click();
                                           }

                                           
                                           if(typeMessage == "Fatorial duplo em DIC"){
                                           $("a:contains(Fatorial duplo em DIC)").click();
                                           }
                                           
                                                                                      
                                           if(typeMessage == "Fatorial duplo em DBC"){
                                           $("a:contains(Fatorial duplo em DBC)").click();
                                           }


                                           if(typeMessage == "Parcela subdividida em DIC"){
                                           $("a:contains(Parcela subdividida em DIC)").click();
                                           }
                                           
                                           
                                           if(typeMessage == "Parcela subdividida em DBC"){
                                           $("a:contains(Parcela subdividida em DBC)").click();
                                           }
                                           
                                           });
                                           '))
                      ),
                      
                      actionButton("ACT_CONJUNTO_TO_BD", label = "SORTEAR CROQUI", width = "100%"),
                      br(),
                      br(),
                      actionButton("IN_INSTRUCOES", label = "INSTRUÇÕES DE USO", width = "100%", onclick ="window.open('https://drive.google.com/file/d/19IRQNPIu3BBCQcgFag5FK1gPYvA8ih5_/view?usp=sharing', '_blank')"),
                      br(),
                      br(),
                      actionButton("IN_AJUDA", label = "AJUDA COM DELINEAMENTOS", width = "100%"),
                      br(),
                      br(),
                      
                      # Linha horizontal ----
                      tags$hr(),
                      
                      ######################################################
                      #     botao avancar e seus alinhamentos em html      #
                      ######################################################
                      
                      #tags$div(align="center", border="0",
                      #actionButton("ACT_CONJUNTO_TO_BD", label = "Avançar", class="btn btn-success", icon= icon("angle-right"), width="100")
                      #),
                      
                      tags$head(tags$style(HTML('
                          #AREA_DADOS{
                           background-color: #cee6cf !important;
                           border: none;
                           }')))
                    ),
                    mainPanel( renderPlot("GRAFICO"),
                               
                               tags$div(align="center", valign="top", img(src='https://raw.githubusercontent.com/PaulohSouza/imagens/main/logo.png', width='560'), br()),
                               br()
                    )
                  )
                  
                ),
                
                #### Pagina 2
                navbarMenu("DELINEAMENTO",
                           tabPanel
                           ("Definir delineamento", 
                             sidebarLayout
                             (
                               sidebarPanel
                               (
                                 ######################################################
                                 #              scripts dos botoes avancar            #
                                 ######################################################
                                 
                                 
                                 radioButtons("IN_DELINEAMENTO", label = h5("Escolha o delinemento experimental"),
                                              choices = list(#"Delineamento inteiramente casualizado (DIC)" = 1,
                                                "Delineamento em blocos casualizados (DBC)" = 2,
                                                #"Delineamento em quadrado latino (DQL)" = 3,
                                                #"Fatorial duplo em DIC" = 4,
                                                "Fatorial duplo em DBC" = 5#,
                                                #"Parcela subdividida em DIC" = 6,
                                                #"Parcela subdividida em DBC" = 7
                                              ),
                                              selected = 2),
                                 
                                 tags$div(align="center",  border="0",
                                          actionButton("DADOS_PARA_INICIO", label = "< Voltar", class="btn btn-success", width="100"),
                                          actionButton("DELINEAMENTO_PARA_CROQUI", label = "Avançar >", class="btn btn-success", width="100")
                                 ),
                                 
                                 br()
                                 
                               ),
                               
                               mainPanel(
                                 tabsetPanel(
                                   tabPanel(
                                     DT::dataTableOutput("tabela")
                                   )
                                 )
                               )
                             )
                           )
                ),
                
                navbarMenu("CROQUI",
                           
                           # tabPanel
                           # ("Delineamento inteiramente casualizado (DIC)",
                           #   sidebarLayout
                           #   (
                           #     sidebarPanel
                           #     (
                           #     ),
                           #     mainPanel(
                           #       withSpinner(verbatimTextOutput("CROQUI_DIC"), color = "green"))
                           #   ),
                           #   # FOOTER CREDITOS
                           #   hr(),
                           #   tags$div(align="center", valign="bottom", img(src='https://raw.githubusercontent.com/PaulohSouza/imagens/main/logo.png', width='160'), br()),
                           #   tags$div( align="center",
                           #             HTML("FMT Croqui - 2022")
                           #   ),
                           # ),
                           
                           tabPanel
                           ("Delineamento em blocos casualizados (DBC)",
                             sidebarLayout
                             (
                               sidebarPanel 
                               (
                                 sliderInput("IN_TRATAMENTOS", "Quantidade de tratamentos", 
                                             min = 4, max = 100, value = 4),
                                 sliderInput("IN_REPETICOES", "Quantidade de blocos", 
                                             min = 3, max = 10, value = 4),
                                 textInput("IN_SIGLA", "Sigla dos tratamentos", 
                                           value = "T"),
                                 
                                 checkboxInput("IN_TRANSPOR", "Transpor o croqui", FALSE),
                                 
                                 tags$div(align="center", downloadButton("IN_DOWNLOAD", "Download", class="btn btn-success")),
                                 
                                 tags$div(align="left",  border="0",
                                          actionButton("DBC_PARA_INICIO", label = "< Voltar", class="btn btn-success", width="100")
                                 ),
                                 
                                 #checkboxInput("IN_TRAT_AD", "inserir tratamento adicional", FALSE),
                                 
                                 #conditionalPanel(condition = "input.IN_TRAT_AD",
                                 #                numericInput("IN_QT_TRAT_AD", ("Quantidade de tratamentos adicionais"), value ="")),
                                 
                                 ###################################################### 
                                 #     botao avancar e seus alinhamentos em html      #
                                 ######################################################
                                 
                               ),
                               
                               mainPanel
                               (      
                                 withSpinner(verbatimTextOutput("CROQUI_DBC"), color = "green"),
                               )
                             )
                           ),
                           
                           # tabPanel
                           # ("Delineamento em quadrado latino (DQL)",
                           #   sidebarLayout
                           #   (
                           #     sidebarPanel
                           #     (
                           #     ),
                           #     mainPanel
                           #     (
                           #       withSpinner(verbatimTextOutput("CROQUI_DQL"), color = "green"),
                           #     )
                           #   )
                           # ), # FIM TAB PANEL
                           
                           # tabPanel
                           # ("Fatorial duplo em DIC",
                           #   sidebarLayout
                           #   (
                           #     sidebarPanel
                           #     (
                           #     ),
                           #     mainPanel
                           #     (
                           #     )
                           #   )
                           # ), # FIM TAB PANEL
                           
                           tabPanel
                           ("Fatorial duplo em DBC",
                             sidebarLayout
                             (
                               sidebarPanel 
                               (
                                 textInput("IN_SIGLA1_FDBC", "Sigla do primeiro fator", 
                                           value = "T"),
                                 sliderInput("IN_TRATAMENTOS1_FDBC", "Quantidade de tratamentos do primeiro fator", 
                                             min = 2, max = 15, value = 3),
                                 textInput("IN_SIGLA2_FDBC", "Sigla do segundo fator", 
                                           value = "S"),
                                 sliderInput("IN_TRATAMENTOS2_FDBC", "Quantidade de tratamentos do segundo fator", 
                                             min = 2, max = 15, value = 3),
                                 sliderInput("IN_REPETICOES_FDBC", "Quantidade de blocos", 
                                             min = 2, max = 10, value = 4),
                                 # textInput("IN_SIGLA_FDBC", "Sigla dos tratamentos", 
                                 #           value = "T"),
                                 
                                 checkboxInput("IN_TRANSPOR_FDBC", "Transpor o croqui", FALSE),
                                 
                                 tags$div(align="center", downloadButton("IN_DOWNLOAD_FDBC", "Download", class="btn btn-success", align="center")),
                                 
                                 tags$div(align="left",  border="0",
                                          actionButton("FDBC_PARA_INICIO", label = "< Voltar", class="btn btn-success", width="100")
                                 ),
                                 
                                 
                               ),
                               mainPanel
                               (
                                 withSpinner(verbatimTextOutput("CROQUI_FDBC"), color = "green"),
                                 
                               )
                             )
                           ), # FIM TAB PANEL
                           
                           # tabPanel
                           # ("Parcela subdividida em DIC",
                           #   sidebarLayout
                           #   (
                           #     sidebarPanel 
                           #     (
                           #     ),
                           #     mainPanel
                           #     (      
                           #     )
                           #   )
                           # ), # FIM TAB PANEL
                           
                           # tabPanel
                           # ("Parcela subdividida em DBC",
                           #   sidebarLayout
                           #   (
                           #     sidebarPanel 
                           #     (
                           #     ),
                           #     mainPanel
                           #     (      
                           #       withSpinner(verbatimTextOutput("CROQUI_SUBDBC"), color = "green"),
                           #     )
                           #   )
                           # ), # FIM TAB PANEL
                           
                           
                ) # fim da navbar RESULTADOS
)