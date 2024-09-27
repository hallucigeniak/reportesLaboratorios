########################################
###--- PROCESAR TABLAS DE QUERIES ---###
########################################
#
#dependencias<-c("dplyr")
#
mapeoProfesiones<-function(tablaQuery){
  tablaTodasProfesiones<-read.delim("Dependencias/Profesiones.tsv", sep="\t")
  tablaProfesionesAgrupadas<-read.delim("Dependencias/GruposProfesiones.tsv", sep="\t")
  tablaEspecialidades<-read.delim("Dependencias/Especialidades.tsv", sep="\t")
  #
  tablaMerge<-merge(tablaTodasProfesiones, tablaProfesionesAgrupadas, by = "ProfesionID", all.x = T)[,-1]
  #tablaMerge<-tablaMerge[-which(is.na(tablaMerge$ProfesionID.1)), -1]
  colnames(tablaMerge)<-c("ProfessionName", "ProfessionNameStd")
  x<-merge(tablaQuery, tablaMerge, by = "ProfessionName", all.x = T)
  OtherProfessions<-x$ProfessionName[!(x$ProfessionName %in% tablaMerge$ProfessionName)]<-"OTRAS PROFESIONES"
  x<-merge(x, tablaEspecialidades, by.x = "SpecialityName", by.y="Especialidad", all.x = T)
  return(x)
}
#
procesarTablas<-function(tablaQuery, Actual=T){
  ptm<-proc.time()
  tablaQuery$searchdate<-as.Date(tablaQuery$searchdate)
  tablaQuery$searchdateMonths<-format(tablaQuery$searchdate, "%Y-%m")
  tablaQuery$ConsultasTotales<-1
  
  if(Actual){
    #--- TABLA MAESTRA ---#
    tablaQuery$ClickOrPrint<-0
    tablaQuery$ClickOrPrint[which(tablaQuery$Consultas==0)]<-"Clicks"
    tablaQuery$ClickOrPrint[which(tablaQuery$Consultas!=0)]<-"Prints"
    #tablaQuery$Clicks<-0
    #tablaQuery$Clicks[which(tablaQuery$Consultas==0)]<-1
    #tablaQuery$Prints<-0
    #tablaQuery$Prints[which(tablaQuery$Consultas!=0)]<-1
    listaTablas<-list()
    #--- GENERAR TABLA PARA GRAFICAR RESUMEN DE CONSULTAS TOTALES ---#
    consultasTotales<-tablaQuery %>% group_by(searchdateMonths) %>% summarise(ConsultasTotales=sum(ConsultasTotales))
    fechasFormateadas<-paste0(consultasTotales$searchdateMonths, "-01")
    consultasTotales$searchdateMonths<-as.Date(fechasFormateadas)
    #consultasTotales$Periodo<-year(consultasTotales$searchdateMonths)
    consultasTotales$Periodo<-"Actual"
    grafConsultasTotales<-consultasTotales
    listaTablas$grafPeriodosActual<-grafConsultasTotales
    #--- GENERAR TABLA PARA RENDEREAR CONSULTAS TOTALES ---#
    tablaSpread <- consultasTotales %>% spread(searchdateMonths, ConsultasTotales, fill=0)
    fechasColNames<-format(as.Date(colnames(tablaSpread)[-1]), "%b")
    colnames(tablaSpread)<-c("Periodo", fechasColNames)
    TOTAL<-unlist(apply(tablaSpread[,-1], 1, function(x) sum(x)))
    tablaSpread<-cbind(tablaSpread, TOTAL)
    listaTablas$tablaSpreadPeriodos<-tablaSpread
    listaTablas$consultasPeriodos<-formatCurrency(datatable(tablaSpread), columns = 2:NCOL(tablaSpread), currency = "", mark = ",", digits = 0)
    #--- GENERAR TABLA PARA GRAFICAR CLICKS Y PRINTS DEL PERIODO ---#
    tablaGrafClicksPrints<-tablaQuery %>% count(searchdateMonths, ClickOrPrint)
    listaTablas$grafClicksPrintsPeriodo<-tablaGrafClicksPrints
    #--- GENERAR TABLA PARA RENDEREAR CLICKS Y PRINTS DEL PERIODO ---#
    tblClicksPrints<-spread(tablaGrafClicksPrints, searchdateMonths, n, fill=0)
    TOTAL<-unlist(apply(tblClicksPrints[,-1], 1, function(x) sum(x)))
    tblClicksPrints<-cbind(tblClicksPrints, TOTAL)
    colnames(tblClicksPrints)[1]<-"Tipo"
    listaTablas$tblClicksPrints<-formatCurrency(datatable(tblClicksPrints), columns = 2:NCOL(tblClicksPrints), currency = "", mark = ",", digits = 0)
    #--- GENERAR TABLAS DE CONSULTAS POR MARCA ---#
    consultasXproductoMensual<-tablaQuery %>% group_by(Brand, searchdateMonths) %>% summarise(Consultas=sum(ConsultasTotales))
    consultasXproductoMensual$searchdateMonths<-paste(consultasXproductoMensual$searchdateMonths, "01", sep="-")
    consultasXproductoMensual$searchdateMonths<-as.Date(consultasXproductoMensual$searchdateMonths)
    consultasMarcasMensual<-consultasXproductoMensual
    colnames(consultasMarcasMensual)<-c("Marca", "Fecha", "Consultas")
    listaTablas$consultasMarcasMensual<-consultasMarcasMensual
    #--- Tabla TOP MARCAS: Ordenada por consultas ---#
    consultasTopMarcas<- consultasXproductoMensual %>% group_by(Brand) %>% summarise(Consultas=sum(Consultas))
    consultasTopMarcas<-consultasTopMarcas[order(consultasTopMarcas$Consultas, decreasing = T),]
    colnames(consultasTopMarcas)<-c("Marca", "Consultas")
    listaTablas$grafTopMarcas<-consultasTopMarcas
    consultasTopMarcas<-cbind(consultasTopMarcas, Porcentaje=paste(round((consultasTopMarcas$Consultas*100)/sum(consultasTopMarcas$Consultas), 2), "%"))
    colnames(consultasTopMarcas)<-c("Marca", "Consultas", "Porcentaje")
    listaTablas$TopMarcas<-consultasTopMarcas
    listaTablas$tblTopMarcas<-formatCurrency(datatable(consultasTopMarcas), columns = 2, currency = "", mark = ",", digits = 0)
    #--- Tabla de Consultas por marca por mes ---#
    todasLasMarcas<-spread(consultasXproductoMensual, searchdateMonths, Consultas, fill=0)
    fechasColNames<-colnames(todasLasMarcas)[-1]
    fechasColNames<-format(as.Date(fechasColNames), "%b")
    todasLasMarcas<-cbind(todasLasMarcas, TOTAL=rowSums(todasLasMarcas[,-1]))
    todasLasMarcas[,-1]<-sapply(todasLasMarcas[,-1], as.integer)
    colnames(todasLasMarcas)<-c("Marca", fechasColNames, "TOTAL")
    listaTablas$TopMarcasMes<-formatCurrency(datatable(todasLasMarcas), columns = 2:NCOL(todasLasMarcas), currency = "", mark = ",", digits = 0)
    #
    #--- Tabla de Consultas por Dispositivo ---#
    consultasDispositivos<-count(tablaQuery, TargetName)
    colnames(consultasDispositivos) <- c("Dispositivo", "Consultas")
    consultasDispositivos <- cbind(consultasDispositivos, Porcentaje=paste(round((consultasDispositivos$Consultas*100)/sum(consultasDispositivos$Consultas), 2), "%"))
    listaTablas$consultasDispositivos<-consultasDispositivos
    listaTablas$tblConsultasDispositivos<-formatCurrency(datatable(consultasDispositivos), columns = 2, currency = "", mark = ",", digits = 0)
    #
    #--- Consultas por Distribución para grafica ---#
    consultasTopDistributions<- tablaQuery %>% count(PrefixDescription) %>% arrange(desc(n))
    consultasTopDistributions<- cbind(consultasTopDistributions, Porcentaje=(round((consultasTopDistributions$n/sum(consultasTopDistributions$n))*100, 2)))
    listaTablas$grafTopDistributions<-consultasTopDistributions
    #--- COnsultas por Distribucion par tabla ---#
    consultasDistribucionTbl<- tablaQuery %>% count(PrefixDescription, TargetName) %>% spread(TargetName, n, fill=0)
    consultasDistribucionTbl<-cbind(consultasDistribucionTbl, TOTAL=rowSums(consultasDistribucionTbl[,-1])) %>% arrange(desc(TOTAL))
    consultasDistribucionTbl<-consultasDistribucionTbl %>% cbind(consultasDistribucionTbl, Porcentaje=(round((consultasDistribucionTbl$TOTAL/sum(consultasDistribucionTbl$TOTAL))*100, 2)))
    listaTablas$tblDistributions<-formatCurrency(datatable(consultasDistribucionTbl), columns = 2, currency = "", mark = ",", digits = 0)
    #--- TABLAS DE CONSULTAS POR PROFESION ---#
    #
    #--- Tabla de consultas totales por profesion ---#
    tablaQueryMapeada<-mapeoProfesiones(tablaQuery)
    tablaQueryMapeada <- tablaQueryMapeada[!is.na(tablaQueryMapeada$SpecialityNameStd),]
    if (NROW(tablaQueryMapeada) == 0){
      listaTablas$consultasTotalesProfesion<-NULL
      listaTablas$tblConsultasTotalesProfesion<-NULL
      listaTablas$consultasProfesionMes<-NULL
      listaTablas$grafProfesionMes<-NULL
      listaTablas$consultasTotalesEspecialidades<-NULL
      listaTablas$tblConsultasTotalesEspecialidades<-NULL
      listaTablas$grafEspecialidadesMes<-NULL
      listaTablas$consultasEspecialidadMes<-NULL
      listaTablas$grafEspecialidadesMes<-NULL
    } else{
      consultasProfesion <- tablaQueryMapeada %>% group_by(ProfessionNameStd) %>% summarise(Consultas = sum(ConsultasTotales))
      consultasProfesion <- transform(consultasProfesion, Porcentaje=paste(round((Consultas*100)/sum(Consultas), 2), "%"))
      colnames(consultasProfesion)[1]<-"Profesión"
      listaTablas$consultasTotalesProfesion<-consultasProfesion
      listaTablas$tblConsultasTotalesProfesion<-formatCurrency(datatable(consultasProfesion), columns = 2, currency = "", mark = ",", digits = 0)
      #
      #--- Tabla de consultas mensuales de cada profesiorenderDataTable({listaTablas$consultasDispositivos})n ---#
      consultasProfesionFecha <- group_by(tablaQueryMapeada, ProfessionNameStd, searchdateMonths)
      consultasProfesionFecha <- consultasProfesionFecha %>% summarise(Consultas = sum(ConsultasTotales)) %>% spread(searchdateMonths, Consultas, fill=0)
      fechasColNames<-paste0(colnames(consultasProfesionFecha)[-1], "-01")
      fechasColNames<-format(as.Date(fechasColNames), "%b")
      consultasProfesionFecha<-cbind(consultasProfesionFecha, TOTAL=rowSums(consultasProfesionFecha[,-1]))
      consultasProfesionFecha[,-1]<-sapply(consultasProfesionFecha[,-1], as.integer)
      colnames(consultasProfesionFecha)<-c("Profesión", fechasColNames, "TOTAL")
      listaTablas$consultasProfesionMes<-formatCurrency(datatable(consultasProfesionFecha), columns = 2:NCOL(consultasProfesionFecha), currency = "", mark = ",", digits = 0)
      #
      # Generar la tabla que se usara para graficar las consultas por mes de cada profesion.
      tablaProfesionFecha <- tablaQueryMapeada %>% group_by(ProfessionNameStd, searchdateMonths) %>% summarize(Consultas = sum(ConsultasTotales))
      colnames(tablaProfesionFecha) = c("Profesion", "Fecha", "Consultas")
      fechasFormateadas<-paste0(tablaProfesionFecha$Fecha, "-01")
      tablaProfesionFecha$Fecha<-as.Date(fechasFormateadas)
      listaTablas$grafProfesionMes<-tablaProfesionFecha
      #
      #--- GENERAR TABLAS DE ESPECIALIDADES ---#
      tablaEspecialidadesMed <- tablaQueryMapeada[tablaQueryMapeada$ProfessionNameStd == 'MÉDICO',]
      tablaEspecialidadesMed <- tablaEspecialidadesMed[!is.na(tablaEspecialidadesMed$SpecialityNameStd),]
      #--- Tabla Todas las especialidades ---#
      consultasTotalesEspecialidad <- group_by(tablaEspecialidadesMed, SpecialityNameStd)
      consultasTotalesEspecialidad <- summarize(consultasTotalesEspecialidad, Consultas = sum(ConsultasTotales))
      consultasTotalesEspecialidad <- transform(consultasTotalesEspecialidad, Porcentaje=paste(round((Consultas*100)/sum(Consultas), 2), "%"))
      consultasTotalesEspecialidad<-consultasTotalesEspecialidad[order(-consultasTotalesEspecialidad$Consultas),]
      colnames(consultasTotalesEspecialidad)[1]<-"Especialidad"
      listaTablas$consultasTotalesEspecialidades<-consultasTotalesEspecialidad
      listaTablas$tblConsultasTotalesEspecialidades<-formatCurrency(datatable(consultasTotalesEspecialidad), columns = 2, currency = "", mark = ",", digits = 0)
      #
      #--- Tabla para graficar consultas por especialidad por mes ---#
      tablaEspecialidad <- tablaEspecialidadesMed %>% group_by(SpecialityNameStd, searchdateMonths) %>% summarize(Consultas = sum(ConsultasTotales))
      tablaEspecialidad<-tablaEspecialidad[order(-tablaEspecialidad$Consultas),]
      fechasFormateadas<-paste0(tablaEspecialidad$searchdateMonths, "-01")
      tablaEspecialidad$searchdateMonths<-as.Date(fechasFormateadas)
      tablaEspecialidades<-tablaEspecialidad
      colnames(tablaEspecialidad)<-c("Especialidad", "Fecha", "Consultas")
      listaTablas$grafEspecialidadesMes<-tablaEspecialidad
      #
      #--- Tabla de Consultas por especialidades por mes ---#
      consultasEspecialidadFecha<-tablaEspecialidades %>% spread(searchdateMonths, Consultas, fill=0)
      fechasColNames<-paste0(colnames(consultasEspecialidadFecha)[-1], "-01")
      fechasColNames<-format(as.Date(fechasColNames), "%b")
      consultasEspecialidadFecha<-cbind(consultasEspecialidadFecha, TOTAL=rowSums(consultasEspecialidadFecha[,-1]))
      consultasEspecialidadFecha[,-1]<-sapply(consultasEspecialidadFecha[,-1], as.integer)
      colnames(consultasEspecialidadFecha)<-c("Especialidad", fechasColNames, "TOTAL")
      listaTablas$consultasEspecialidadMes<-formatCurrency(datatable(consultasEspecialidadFecha), columns = 2:NCOL(consultasEspecialidadFecha), currency = "", mark = ",", digits = 0)
    }
    print("Tiempo transcurrido en el procesamiento de la tabla")
    print(proc.time() - ptm)
    return(listaTablas)
    #
    #--- Generar tabla para las consultas del periodo anterior  
  } else{
    consultasTotalesLastYear<-tablaQuery %>% group_by(searchdateMonths) %>% summarise(ConsultasTotales=sum(ConsultasTotales))
    fechasFormateadas<-paste0(consultasTotalesLastYear$searchdateMonths, "-01")
    consultasTotalesLastYear$searchdateMonths<-as.Date(fechasFormateadas)
    #consultasTotalesLastYear$Periodo<-year(consultasTotalesLastYear$searchdateMonths)
    consultasTotalesLastYear$Periodo<-"Anterior"
    tablaSpread <- consultasTotalesLastYear %>% spread(searchdateMonths, ConsultasTotales, fill=0)
    fechasColNames<-format(as.Date(colnames(tablaSpread)[-1]), "%b")
    colnames(tablaSpread)<-c("Periodo", fechasColNames)
    TOTAL<-unlist(apply(tablaSpread[,-1], 1, function(x) sum(x)))
    tablaSpread<-cbind(tablaSpread, TOTAL)
    tablaSpreadP<-rbind(listaTablas$tablaSpreadPeriodos, tablaSpread)
    listaTablas$consultasPeriodos<<-formatCurrency(datatable(tablaSpreadP), columns = 2:NCOL(tablaSpreadP), currency = "", mark = ",", digits = 0) 
    
    
    return(consultasTotalesLastYear)
  }
}
#