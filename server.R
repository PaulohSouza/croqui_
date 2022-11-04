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

shinyServer(function(input, output, session) {
  
  #### Variávei globais reactivas
  
  vals <- reactiveValues(x = NULL)
  
  ############################################################
  #     Ações e eventos                                      #
  ############################################################
  
  observe({
    if(input$ACT_CONJUNTO_TO_BD > 0){
      print("ACT_CONJUNTO_TO_BD")
      session$sendCustomMessage("myCallbackHandler", "ACT_CONJUNTO_TO_BD")
    }
  })
  
  observe({
    if(input$DADOS_PARA_INICIO > 0){
      print("DADOS_PARA_INICIO")
      session$sendCustomMessage("myCallbackHandler", "DADOS_PARA_INICIO")
    }
  })
  
  observe({
    if(input$FDBC_PARA_INICIO > 0){
      print("FDBC_PARA_INICIO")
      session$sendCustomMessage("myCallbackHandler", "FDBC_PARA_INICIO")
    }
  })
  
  observe({
    if(input$DBC_PARA_INICIO > 0){
      print("DBC_PARA_INICIO")
      session$sendCustomMessage("myCallbackHandler", "DBC_PARA_INICIO")
    }
  })
  
  ######## 
  
  observeEvent(input$DELINEAMENTO_PARA_CROQUI,{
    
    if(input$IN_DELINEAMENTO == 1){
      print("Delineamento inteiramente casualizado (DIC)")
      session$sendCustomMessage("myCallbackHandler", "Delineamento inteiramente casualizado (DIC)")
    }  
    
    
    if(input$IN_DELINEAMENTO == 2){
      print("Delineamento em blocos casualizados (DBC)")
      session$sendCustomMessage("myCallbackHandler", "Delineamento em blocos casualizados (DBC)")
    } 
    
    if(input$IN_DELINEAMENTO == 3){
      print("Delineamento em quadrado latino (DQL)")
      session$sendCustomMessage("myCallbackHandler", "Delineamento em quadrado latino (DQL)")
    }
    
    if(input$IN_DELINEAMENTO == 4){
      print("Fatorial duplo em DIC")
      session$sendCustomMessage("myCallbackHandler", "Fatorial duplo em DIC")
    }
    
    if(input$IN_DELINEAMENTO == 5){
      print("Fatorial duplo em DBC")
      session$sendCustomMessage("myCallbackHandler", "Fatorial duplo em DBC")
    }
    
    if(input$IN_DELINEAMENTO == 6){
      print("Parcela subdividida em DIC")
      session$sendCustomMessage("myCallbackHandler", "Parcela subdividida em DIC")
    }
    
    if(input$IN_DELINEAMENTO == 7){
      print("Parcela subdividida em DBC")
      session$sendCustomMessage("myCallbackHandler", "Parcela subdividida em DBC")
    }
    
  })
  
  
  
  
  # observe({
  #   if(input$ANALISE_PARA_DADOS > 0){
  #     print("ANALISE_PARA_DADOS")
  #     session$sendCustomMessage("myCallbackHandler", "ANALISE_PARA_DADOS")
  #   }
  # })
  
  # observe({
  #   if(input$ANALISE_PARA_GRAFICOS > 0){
  #     print("ANALISE_PARA_GRAFICOS")
  #     session$sendCustomMessage("myCallbackHandler", "ANALISE_PARA_GRAFICOS")
  #   }
  # })
  
  # observe({
  #   if(input$GRAFICOS_PARA_ANALISE > 0){
  #     if(deli == "sei la")
  #       print("GRAFICOS_PARA_ANALISE")
  #     session$sendCustomMessage("myCallbackHandler", "GRAFICOS_PARA_ANALISE")
  #     
  #   }
  # })
  
  
  ######################################################################
  ############# DIC
  ######################################################################
  
  output$CROQUI_DIC <- renderPrint({
    
    if(input$IN_DELINEAMENTO == 1){
      print("Delineamento inteiramente casualizado (DIC)")
    }
  })
  
  ######################################################################
  ############# DBC
  ######################################################################
  
  output$CROQUI_DBC <- renderPrint({
    
    
    if(input$IN_DELINEAMENTO == 2){
      
      print("Delineamento em blocos casualizados (DBC)")
      
      NUMERO_TRATAMENTOS = input$IN_TRATAMENTOS ###INPUT
      BLOCO = input$IN_REPETICOES ## INPUT NUMERO DE REPETICOES
      SIGLA = input$IN_SIGLA
      
      if(BLOCO > 4){
        croqui <- VIZINHO_PROXIMO(NUMERO_TRATAMENTOS, BLOCO, SIGLA)
        #print("Algoritmo Vizinho próximo")
      }else{
        croqui <- SORTEIO_PERFEITO(NUMERO_TRATAMENTOS, BLOCO, SIGLA)
        #print("Sorteio Perfeito")
      }
      
      #print(croqui)
      
      if(input$IN_TRANSPOR == FALSE){
        print(croqui)
      }
      
      if(input$IN_TRANSPOR == TRUE){
        
        croqui <- as.data.frame(t(croqui), row.names = "Bloco")
        
        cat("Croqui transposto \n")
        print(croqui)
      }
      
      output$IN_DOWNLOAD <- downloadHandler("Croqui.xlsx",
                                            content = function(file){
                                              write.xlsx(croqui, file, row.names = T)
                                            },
                                            contentType = "text/xlsx")
    }
  }) 
  
  ######################################################################
  ############# DQL
  ######################################################################
  
  output$CROQUI_DQL <- renderPrint({
    
    if(input$IN_DELINEAMENTO == 3){
      print("Delineamento em quadrado latino (DQL)")
    }
  })
  
  
  
  ######################################################################
  ############# FDBC
  ######################################################################
  
  output$CROQUI_FDBC <- renderPrint({
    
    if(input$IN_DELINEAMENTO == 5){
      
      print("Fatorial duplo em DBC")
      
      F1_FDBC = input$IN_TRATAMENTOS1_FDBC ###INPUT
      F2_FDBC = input$IN_TRATAMENTOS2_FDBC ###INPUT
      BLOCOS_FDBC = input$IN_REPETICOES_FDBC ## INPUT NUMERO DE REPETICOES
      SIGLA1_FDBC = input$IN_SIGLA1_FDBC
      SIGLA2_FDBC = input$IN_SIGLA2_FDBC
      
      
      if(BLOCOS_FDBC > 4){
        croqui <- FATORIALDBC(F1_FDBC, F2_FDBC, BLOCOS_FDBC, SIGLA1_FDBC, SIGLA2_FDBC)
        #print("Algoritmo Vizinho próximo")
      }else{
        croqui <- FATORIALDBC_PERFEITO(F1_FDBC, F2_FDBC, BLOCOS_FDBC, SIGLA1_FDBC, SIGLA2_FDBC)
        #print("Sorteio Perfeito")
      }
      
      ######## TRANSPOR 
      
      if(input$IN_TRANSPOR_FDBC == FALSE){
        print(croqui)
      }
      
      if(input$IN_TRANSPOR_FDBC == TRUE){
        
        croqui <- as.data.frame(t(croqui), row.names = "Bloco", colnames = "Linha")
        
        
        cat("Croqui transposto \n")
        print(croqui)
      }
      
      output$IN_DOWNLOAD_FDBC <- downloadHandler("Croqui.xlsx",
                                                 content = function(file){
                                                   write.xlsx(croqui, file, row.names = T)
                                                 },
                                                 contentType = "text/xlsx")
    }
  })
  
  
  
  ######################################################################
  ############# SUBDBC
  ######################################################################
  
  output$CROQUI_SUBDBC <- renderPrint({
    
    if(input$IN_DELINEAMENTO == 7){
      print("Parcela subdividida em DBC")
    }
  })
  
  
  
  
  
  
})







