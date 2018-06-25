####################################
###--- MODULO DE DEPENDENCIAS ---###
####################################
#
#--- Declarar las librerias de R necesarias
dependencias<-c("shiny",
                "shinydashboard",
                "shinycssloaders",
                "shinyjs",
                "shinyBS",
                "mailR",
                "odbc",
                "sqldf",
                "dplyr",
                "tidyr",
                "xlsx",
                "png",
                "ggplot2",
                "lubridate",
                "scales",
                "DT")
#--- Verificar que todas las librerias de R necesarias esten instaladas
if (!all(dependencias %in% installed.packages())){
  faltantes<-which(!(dependencias %in% installed.packages()))
  faltantes<-dependencias[faltantes]
  lapply(faltantes, install.packages, character.only=TRUE)
}
#--- Verficar que las librerías de R necesarias esten cargadas
if(!all(dependencias %in% loadedNamespaces())){
  faltantes<-which(!(dependencias %in% loadedNamespaces()))
  faltantes<-dependencias[faltantes]
  lapply(dependencias, require, character.only=TRUE)
}
#
#--- Cargar dependencias definidas por el usuario
#- Funciones
source("Dependencias/FuncionesGenerarQueries.R")
if (!exists("con")){
  source("Dependencias/ConnectToDB.R")
}
source("Dependencias/FuncionesGenerarGraficas.R")
source("Dependencias/FuncionesProcesarTablas.R")
#
#- Datos
#tablaWaitTimes<-read.delim("Dependencias/EstimatedTimes.csv")
tablaTodasProfesiones<-read.delim("Dependencias/Profesiones.tsv", sep="\t") #Tabla1 para mapear profesiones
tablaProfesionesAgrupadas<-read.delim("Dependencias/GruposProfesiones.tsv", sep="\t") #Tabla2 para mapear profesiones 
tablaEspecialidades<-read.delim("Dependencias/Especialidades.tsv", sep="\t") #Tabla de especialidades mapeadas
catalogoLabs<-read.delim("Dependencias/MapeoLaboratorios.csv", header = T, sep="\t")
listaPaises<-read.delim("Dependencias/ListaPaises.csv", sep = "\t", header = T) #Lista de paises
listaLogos<-read.delim("Dependencias/Logos.csv", header = T, sep="\t")
