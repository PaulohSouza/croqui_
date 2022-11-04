############################################################################### 
#################### DBC
###############################################################################

VIZINHO_PROXIMO <- function(NUMERO_TRATAMENTOS, BLOCO, SIGLA){
  
  TRATAMENTOS <- NULL ### INPUT NUMERO DE TRATAMENTOS 
  LINHAS <- NULL
  BLOCOS <- NULL
  
  ### FOR PARA CRIAR VETOR DE TRATAMENTOS
  k=1
  
  for(k in 1:NUMERO_TRATAMENTOS){
    TRATAMENTOS <- c(TRATAMENTOS,(paste0(SIGLA,k)))
    LINHAS <- c(LINHAS,(paste0("Linha ",k)))
  }
  
  for(b in 1:BLOCO){
    BLOCOS <- c(BLOCOS,(paste0("Bloco ",b)))
  }
  LINHAS <- as.vector(LINHAS)
  BLOCOS <- as.vector(BLOCOS)
  
  ### VARIAVEIS DE INICIALIZACAO
  FLAG = 0
  STATUS = "INICIANDO"
  TENTATIVA = 1
  
  while(FLAG == 0){
    i = 1
    j = 1
    TENTATIVA = TENTATIVA+1
    outdesign <-design.rcbd(TRATAMENTOS,BLOCO,serie=2)
    book <-outdesign$book
    croqui <- data.frame(outdesign$sketch)
    #print(paste("TENTATIVA = ", TENTATIVA, "\n"))
    IGUAIS = 0
    
    for(i in 1:length(croqui)){
      for(j in 1:(nrow(croqui)-1)){
        if(croqui[j,i] == croqui[j+1,i]){
          STATUS = "IGUAL"
          IGUAIS = IGUAIS +1
          break
        }else{
          if((i == length(croqui)) && (IGUAIS == 0) && (j == nrow(croqui)-1)){
            STATUS = "DIFERENTE"
            colnames(croqui) <- LINHAS
            rownames(croqui) <- BLOCOS
            break
          }
        }
      }
    }
    #print(croqui)
    if(STATUS == "DIFERENTE"){
      colnames(croqui) <- LINHAS
      rownames(croqui) <- BLOCOS
      FLAG = 1
    }else{
      FLAG = 0
    }
  }
  #names(croqui)
  #colnames(croqui) <- LINHAS 
  return(croqui)
  
}

############################################################################### 
#################### DBC
###############################################################################

SORTEIO_PERFEITO <- function(NUMERO_TRATAMENTOS, BLOCO, SIGLA){
  
  TRATAMENTOS <- NULL ### INPUT NUMERO DE TRATAMENTOS
  LINHAS <- NULL
  BLOCOS <- NULL
  
  ### FOR PARA CRIAR VETOR DE TRATAMENTOS
  for(k in 1:NUMERO_TRATAMENTOS){
    TRATAMENTOS <- c(TRATAMENTOS,(paste0(SIGLA,k)))
    LINHAS <- c(LINHAS,(paste0("Linha ",k)))
  }
  
  for(b in 1:BLOCO){
    BLOCOS <- c(BLOCOS,(paste0("Bloco ",b)))
  }
  
  LINHAS <- as.vector(LINHAS)
  BLOCOS <- as.vector(BLOCOS)
  
  ### VARIAVEIS DE INICIALIZACAO
  FLAG = 0
  STATUS = "INICIANDO"
  TENTATIVA = 1
  croqui <- NULL
  
  while(FLAG == 0){
    i = 1
    
    TENTATIVA = TENTATIVA+1
    outdesign <-design.rcbd(TRATAMENTOS,BLOCO,serie=2)
    book <-outdesign$book
    croqui <- data.frame(outdesign$sketch)
    #print(paste("TENTATIVA = ", TENTATIVA, "\n"))
    IGUAIS = 0
    
    for(i in 1:NUMERO_TRATAMENTOS){
      SOMA <-tapply(croqui[,i], croqui[,i], FUN = function(x) length(unique(x)))
      if((length(SOMA)) < BLOCO){
        STATUS = "IGUAL"
        IGUAIS = IGUAIS +1
        break
      }else{
        if((i == NUMERO_TRATAMENTOS) && (IGUAIS == 0)){
          #cat("Encontrou croqui sem tratamentos iguais próximos \n")
          STATUS = "DIFERENTE"
          #cat("Croqui Final \n")
          colnames(croqui) <- LINHAS
          rownames(croqui) <- BLOCOS
          #print(croqui)
          break
        }
      }
    }
    
    if(STATUS == "DIFERENTE"){
      colnames(croqui) <- LINHAS
      rownames(croqui) <- BLOCOS
      FLAG = 1
    }else{
      FLAG = 0
    }
    
  }
  return(croqui)
}

############################################################################### 
#################### FDBC
###############################################################################

FATORIALDBC <- function(F1_FDBC, F2_FDBC, BLOCOS_FDBC, SIGLA1_FDBC, SIGLA2_FDBC){
  
  FLAG = 0
  STATUS = "INICIANDO"
  TENTATIVA = 1
  
  while(FLAG == 0){
    
    TRATAMENTOS = c(F1_FDBC, F2_FDBC)
    TENTATIVA = TENTATIVA + 1
    
    outdesign <- design.ab(TRATAMENTOS,BLOCOS_FDBC,design="rcbd",serie=0)
    book <- as.vector(matrix(paste(outdesign$book$A,outdesign$book$B),nrow=BLOCOS_FDBC,byrow=T))
    
    croqui <- data.frame(book)
    croqui <- as.vector(croqui)
    
    i = 1
    j = 1
    k = 1
    
    TR <- as.data.frame(NULL)
    
    NUMERO_COLUNAS<- as.integer(F1_FDBC * F2_FDBC)
    NUMERO_LINHAS <- BLOCOS_FDBC
    
    for(i in 1: NUMERO_COLUNAS){
      for(j in 1:NUMERO_LINHAS){
        TR[j,i] <- croqui[k,1]
        k = k+1
      }
    }
    
    croqui <- TR
    
    IGUAIS = 0
    
    for(i in 1:length(croqui)){
      for(j in 1:(nrow(croqui)-1)){
        if(croqui[j,i] == croqui[j+1,i]){
          STATUS = "IGUAL"
          IGUAIS = IGUAIS +1
          break
          
        }else{
          if((i == length(croqui)) && (IGUAIS == 0) && (j == nrow(croqui)-1)){
            STATUS = "DIFERENTE"
            break
          }
        }
      }
    }
    
    if(STATUS == "DIFERENTE"){
      FLAG = 1
      
      CROQUI_VETOR <- NULL
      
      for(i in 1: length(croqui)){
        for(j in 1: nrow(croqui)){
          CROQUI_VETOR <- c(CROQUI_VETOR, croqui[j,i])
        }
      }
      
      CROQUI_DF <-  data.frame(strsplit(CROQUI_VETOR, " "))
      
      NOVOS_TRATAMENTOS <- NULL
      
      for(i in 1:length(CROQUI_VETOR)){
        F1 <- paste(SIGLA1_FDBC, CROQUI_DF[1,i], SIGLA2_FDBC,CROQUI_DF [2,i])
        NOVOS_TRATAMENTOS <- c(NOVOS_TRATAMENTOS, str_replace_all(F1, " ",""))
      }
      
      CROQUI_FINAL <- data.frame(NULL)
      
      k = 1
      i = 1
      j = 1
      
      for(i in 1:NUMERO_COLUNAS){
        for(j in 1:NUMERO_LINHAS){
          CROQUI_FINAL[j,i] <- NOVOS_TRATAMENTOS[k]
          k = k + 1
        } 
      }
      
      ###############################################################        
      
      LINHAS <- NULL
      BLOCOS <- NULL
      
      k=1
      ### FOR PARA CRIAR VETOR DE TRATAMENTOS
      for(k in 1:NUMERO_COLUNAS){
        LINHAS <- c(LINHAS,(paste0("Linha ",k)))
      }
      
      for(b in 1:NUMERO_LINHAS){
        BLOCOS <- c(BLOCOS,(paste0("Bloco ",b)))
      }
      LINHAS <- as.vector(LINHAS)
      BLOCOS <- as.vector(BLOCOS)
      
      colnames(CROQUI_FINAL) <- LINHAS
      rownames(CROQUI_FINAL) <- BLOCOS
      
      ###############################################################  
      
    }else{
      FLAG = 0
    }
  }
  
  return(CROQUI_FINAL)
  
}

############################################################################### 
#################### FDBC                                              PERFEITO
###############################################################################

FATORIALDBC_PERFEITO <- function(F1_FDBC, F2_FDBC, BLOCOS_FDBC, SIGLA1_FDBC, SIGLA2_FDBC){
  
  FLAG = 0
  STATUS = "INICIANDO"
  TENTATIVA = 1
  
  while(FLAG == 0){
    
    TRATAMENTOS = c(F1_FDBC,F2_FDBC)
    TENTATIVA = TENTATIVA+1
    
    outdesign <- design.ab(TRATAMENTOS,BLOCOS_FDBC,design="rcbd",serie=0)
    book <- as.vector(matrix(paste(outdesign$book$A,outdesign$book$B),nrow=BLOCOS_FDBC,byrow=T))
    croqui <- data.frame(book)
    croqui <- as.vector(croqui)
    
    i = 1
    j = 1
    k = 1
    
    TR <- as.data.frame(NULL)
    
    NUMERO_COLUNAS<- as.integer(F1_FDBC * F2_FDBC)
    NUMERO_LINHAS <- BLOCOS_FDBC
    
    for(i in 1: NUMERO_COLUNAS){
      for(j in 1:NUMERO_LINHAS){
        TR[j,i] <- croqui[k,1]
        k = k+1
      }
    }
    
    croqui <- TR
    
    IGUAIS = 0
    
    for(i in 1:NUMERO_COLUNAS){
      SOMA <-tapply(croqui[,i], croqui[,i], FUN = function(x) length(unique(x)))
      if((length(SOMA)) < BLOCOS_FDBC){
        STATUS = "IGUAL"
        IGUAIS = IGUAIS +1
        break
      }else{
        if((i == NUMERO_COLUNAS) && (IGUAIS == 0)){
          STATUS = "DIFERENTE"
          break
        }
      }
    }
    
    if(STATUS == "DIFERENTE"){
      FLAG = 1
      
      CROQUI_VETOR <- NULL
      
      for(i in 1: length(croqui)){
        for(j in 1: nrow(croqui)){
          CROQUI_VETOR <- c(CROQUI_VETOR, croqui[j,i])
        }
      }
      
      CROQUI_DF <-  data.frame(strsplit(CROQUI_VETOR, " "))
      
      NOVOS_TRATAMENTOS <- NULL
      
      for(i in 1:length(CROQUI_VETOR)){
        F1 <- paste(SIGLA1_FDBC, CROQUI_DF[1,i], SIGLA2_FDBC,CROQUI_DF [2,i])
        NOVOS_TRATAMENTOS <- c(NOVOS_TRATAMENTOS, str_replace_all(F1, " ",""))
      }
      
      CROQUI_FINAL <- data.frame(NULL)
      
      k = 1
      i = 1
      j = 1
      
      for(i in 1:NUMERO_COLUNAS){
        for(j in 1:NUMERO_LINHAS){
          CROQUI_FINAL[j,i] <- NOVOS_TRATAMENTOS[k]
          k = k + 1
        } 
      }
      
      ###############################################################        
      
      LINHAS <- NULL
      BLOCOS <- NULL
      
      k=1
      ### FOR PARA CRIAR VETOR DE TRATAMENTOS
      for(k in 1:NUMERO_COLUNAS){
        LINHAS <- c(LINHAS,(paste0("Linha ",k)))
      }
      
      for(b in 1:NUMERO_LINHAS){
        BLOCOS <- c(BLOCOS,(paste0("Bloco ",b)))
      }
      LINHAS <- as.vector(LINHAS)
      BLOCOS <- as.vector(BLOCOS)
      
      colnames(CROQUI_FINAL) <- LINHAS
      rownames(CROQUI_FINAL) <- BLOCOS
      
      ############################################################### 
      
    }else{
      FLAG = 0
    }
  }
  
  return(CROQUI_FINAL)
  
}

