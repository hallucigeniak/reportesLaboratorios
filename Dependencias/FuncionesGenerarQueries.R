##########################################################
###--- DEFINICION DE FUNCIONES PARA GENERAR QUERIES ---###
##########################################################
#
#dependencias<-c("sqldf", "dplyr", "dbplyr")
#
fullQuery<-function(divisionId, fechaInicial, fechaFinal){
  if (as.Date(fechaInicial) > as.Date(fechaFinal))
  {
    return("fechas")
  } 
  else{
    ptm<-proc.time()
    out<-tryCatch(
      {
        aa<-unique(c(year(fechaInicial), year(fechaFinal))) #Validar años del periodo
        print(aa)
        tt<-data.frame()
        for (iter in aa){
          DBobjectString<-paste0("plmtracking_", iter, ".dbo.vw_Tracking_Laboratorio_Medicamentos")
          fullQueryString<-paste("SELECT t.TargetName, t.ProfessionName, t.SpecialityName, t.Brand, t.ISBN, t.searchdate, t.Consultas, p.DistributionDescription, p.PrefixDescription FROM (SELECT Rlab.TargetName, Rlab.ProfessionName, Rlab.SpecialityName, Rlab.Brand, Rlab.ISBN, Rlab.searchdate, Rlab.Consultas FROM (SELECT TargetName, ProfessionName, SpecialityName, Brand, ISBN, searchdate, parentid AS 'Consultas' FROM ", DBobjectString, " WHERE Brand IS NOT NULL AND DivisionId IN (", divisionId, ") AND searchdate BETWEEN '", fechaInicial, "' AND '", fechaFinal, "') as Rlab inner join  Medinet.dbo.Editions e on Rlab.ISBN = e.ISBN) as t inner join PLMClients.dbo.plm_vwClientsByApplication p on t.codestring = p.CodeString", sep="")
          print(fullQueryString)
          tt<-rbind(tt, sqldf(fullQueryString, connection= con))
        }
        return(tt)
      },
      error=function(err) {
        SqlState<-grep(": \\w{5}:", err$message, value=T)
        if (grepl("08...", SqlState)){
          errCode<-"Error de conexión. Inténtelo nuevamente"
        }
        if (grepl("42S02", SqlState)){
          errCode<-"No se encuentran datos con los criterios de selección. Intente con otro periodo"
        }
        print(err)
        dbDisconnect(con)
        source("Dependencias/ConnectToDB.R")
        return(errCode)
      },
      warning=function(war) {
        print(war)
        return("failed")
      },
      finally={
        message("Query usado")
        #print(fullQueryString)
        message("Fetch Time")
        print(proc.time() - ptm)
      }
    )
    return(out)
  }
}

#lastYearQuery<-function(DBsource, divisionId, fechaInicial, fechaFinal){
#  fullQueryString<-paste("SELECT Rlab.TargetName, Rlab.ProfessionName, Rlab.SpecialityName, Rlab.Brand, Rlab.ISBN, Rlab.searchdate, Rlab.Consultas FROM (SELECT TargetName, ProfessionName, SpecialityName, Brand, ISBN, searchdate, parentid AS 'Consultas' FROM ", DBsource, " WHERE Brand IS NOT NULL AND DivisionId IN (", divisionId, ") AND searchdate BETWEEN '", fechaInicial, "' AND '", fechaFinal, "') as Rlab inner join  Medinet.dbo.Editions e on Rlab.ISBN = e.ISBN", sep="")
#  print(fullQueryString)
#  t.fullQuery<-sqldf(fullQueryString, connection=con)
#  
#  return(t.fullQuery)
#}
#
# dateQuery<-function(DBsource, divisionId){
#   out<-tryCatch(
#     {
#       dateQueryString<-paste('SELECT MIN(searchdate) AS Minima, MAX(searchdate) AS Maxima FROM', DBsource, 'WHERE DivisionId =', divisionId, sep=" ")
#       fechas<-sqldf(dateQueryString, connection=con)
#       lista.fechas<-list(min=fechas$Minima, max=fechas$Maxima)
#       return(lista.fechas)
#     },
#     error=function(cond){
#       message(paste("Error: No existe la BD ", DBsource, sep=""))
#       message("Mensaje Original:")
#       message(cond)
#       return(FALSE)
#     },
#     warning=function(cond){
#       message(cond)
#       return(FALSE)
#     },
#     finally = {
#       message(paste("Intentando hacer query a ", DBsource, sep=""))
#     }
#   )
#   return(out)
# }