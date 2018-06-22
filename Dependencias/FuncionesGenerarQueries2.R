library(dplyr)
library(dbplyr)
library(tidyr)
library(lubridate)
DBase<-"vw_Tracking_Laboratorio_Medicamentos"
#
getClicksPrints<-function(DBase, divisionId, fechaInicial, fechaFinal){
  t<-tbl(con, DBase)
  m<-tbl(src_dbi(con), in_schema("Medinet.dbo", "Editions"))
  m<- m %>% filter(!is.na(ISBN)) %>% distinct(ISBN)
  listaTablas<-list()
  
  #--- Clicks por mes
  clicks<-t %>% filter(DivisionId == divisionId & !is.na(Brand) & between(searchdate, fechaInicial, fechaFinal) & parentid == 0) %>% select(ISBN, searchdate) %>% inner_join(m, by=c("ISBN" = "ISBN")) %>%  mutate(year=year(searchdate), month=month(searchdate))  %>% count(year, month)
  #--- Prints por mes
  prints<-t %>% filter(DivisionId == divisionId & !is.na(Brand) & between(searchdate, fechaInicial, fechaFinal) & parentid != 0) %>% select(ISBN, searchdate) %>% inner_join(m, by=c("ISBN" = "ISBN")) %>%  mutate(year=year(searchdate), month=month(searchdate))  %>% count(year, month)
  #--- Suma de clicks y prints
  clicksPrints<-inner_join(clicks, prints, by=c("month" = "month",  "year" = "year")) %>% collect()
  clicksPrints$total<-as.integer(rowSums(clicksPrints[,-c(1,2)]))
  clicksPrints$yearMonth<-as.Date(paste(clicksPrints$year, clicksPrints$month, "01", sep="-"))
  clicksPrints<-clicksPrints[,c(6,3,4,5)]
  
  clicksPrints<-clicksPrints[order(clicksPrints$yearMonth),]
  colnames(clicksPrints)<-c("Periodo", "Clicks", "Prints", "total")
  clicksPrintsSpread<-gather(clicksPrints, variable, value, Clicks, Prints, total) %>% spread(Periodo, value)
  clicksPrintsSpread$total<-as.integer(rowSums(clicksPrintsSpread[,-1]))
  #
  listaTablas$tblPeriodosActual<-clicksPrintsSpread
  listaTablas$grafPeriodosActual<-gather(as.data.frame(clicksPrints), key, value, Clicks, Prints, total)
  #
  #--- Top Marcas Clicks
  topMarcasClicks<- t %>% filter(DivisionId == divisionId & !is.na(Brand) & between(searchdate, fechaInicial, fechaFinal) & parentid == 0) %>% select(ISBN, searchdate, Brand) %>% inner_join(m, by=c("ISBN" = "ISBN"))  %>% mutate(year=year(searchdate), month=month(searchdate), Brand)  %>% count(year, month, Brand)
  #
  topBrandClicks<-topMarcasClicks %>% group_by(Brand) %>% summarise(Clicks= sum(n))
  #--- Top Marcas Prints
  topMarcasPrints<- t %>% filter(DivisionId == divisionId & !is.na(Brand) & between(searchdate, fechaInicial, fechaFinal) & parentid != 0) %>% select(ISBN, searchdate, Brand) %>% inner_join(m, by=c("ISBN" = "ISBN")) %>% mutate(year=year(searchdate), month=month(searchdate), Brand)  %>% count(year, month, Brand)
  #
  topBrandPrints<-topMarcasPrints %>% group_by(Brand) %>% summarise(Prints=sum(n, na.rm = T))
  #--- Top Marcas Join Clicks y Prints
  TopBrandClicksPrints<-left_join(topBrandPrints, topBrandClicks, by=c("Brand" = "Brand")) %>% collect()
  #
  TopBrandClicksPrints$Clicks<-TopBrandClicksPrints$Clicks %>% replace_na(0)
  TopBrandClicksPrints$Prints<-TopBrandClicksPrints$Prints %>% replace_na(0)
  TopBrandClicksPrints$total<-as.integer(rowSums(TopBrandClicksPrints[,-1]))
  TopBrandClicksPrints<-TopBrandClicksPrints %>% arrange(desc(total))
  TopBrandClicksPrints$Clicks<-as.integer(TopBrandClicksPrints$Clicks)
  TopBrandClicksPrints$Prints<-as.integer(TopBrandClicksPrints$Prints)
  colnames(TopBrandClicksPrints)<-c("Marca", "Prints", "Clicks", "total")
  #
  listaTablas$tblTopBrands<-TopBrandClicksPrints
  listaTablas$grafTopBrands<-TopBrandClicksPrints %>% gather(variable, value, Clicks, Prints) %>% arrange(desc(total), desc(variable))
  #
  #--- Top Marcas Por Mes
  topBrandClicksMonths<-topMarcasClicks %>% group_by(Brand, year, month) %>% summarise(Clicks= sum(n, na.rm = T))
  topBrandPrintsMonths<-topMarcasPrints %>% group_by(Brand, year, month) %>% summarise(Prints= sum(n, na.rm = T))
  topBrandClicksPrintsMonth<-left_join(topBrandPrintsMonths, topBrandClicksMonths, by=c("Brand"="Brand", "year"="year", "month"="month")) %>% collect()
  topBrandClicksPrintsMonth$Clicks<-topBrandClicksPrintsMonth$Clicks %>% replace_na(0)
  topBrandClicksPrintsMonth$Prints<-topBrandClicksPrintsMonth$Prints %>% replace_na(0)
  topBrandClicksPrintsMonth$total<-as.integer(rowSums(topBrandClicksPrintsMonth[,-(1:3)]))
  topBrandClicksPrintsMonth$Prints<-as.integer(topBrandClicksPrintsMonth$Prints)
  topBrandClicksPrintsMonth$Clicks<-as.integer(topBrandClicksPrintsMonth$Clicks)
  topBrandClicksPrintsMonth$yearMonth<-as.Date(paste(topBrandClicksPrintsMonth$year, topBrandClicksPrintsMonth$month, "01", sep="-"))
  #
  grafClicksMonth<-gather(topBrandClicksPrintsMonth[,c(1,5,7)], variable, value, Clicks) %>% arrange(yearMonth)
  colnames(grafClicksMonth)<-c("Marca", "Periodo", "Tipo", "Valor")
  #
  listaTablas$grafClicksMonth<-grafClicksMonth
  #
  tblClicksMonth<-gather(topBrandClicksPrintsMonth[,c(1,5,7)], variable, value, Clicks) %>% spread(yearMonth, value)
  tblClicksMonth<-tblClicksMonth[,-2]
  colNamesMonths<-format(as.Date(colnames(tblClicksMonth)[-1]), "%b")
  colnames(tblClicksMonth)<-c("Marca", colNamesMonths)
  tblClicksMonth[is.na(tblClicksMonth)]<-0
  tblClicksMonth$total<-rowSums(tblClicksMonth[,-1])
  #
  listaTablas$tblClicksMonth<-tblClicksMonth
  #
  grafPrintsMonth<-gather(topBrandClicksPrintsMonth[,c(1,4,7)], variable, value, Prints) %>% arrange(yearMonth)
  colnames(grafPrintsMonth)<-c("Marca", "Periodo", "Tipo", "Valor")
  #
  listaTablas$grafPrintsMonth<-grafPrintsMonth
  #
  tblPrintsMonth<-gather(topBrandClicksPrintsMonth[,c(1,4,7)], variable, value, Prints) %>% spread(yearMonth, value)
  tblPrintsMonth<-tblPrintsMonth[,-2]
  colNamesMonths<-format(as.Date(colnames(tblPrintsMonth)[-1]), "%b")
  colnames(tblPrintsMonth)<-c("Marca", colNamesMonths)
  tblPrintsMonth[is.na(tblPrintsMonth)]<-0
  tblPrintsMonth$total<-rowSums(tblPrintsMonth[,-1])
  #
  listaTablas$tblPrintsMonth<-tblPrintsMonth
  #
  grafTotalMonth<-gather(topBrandClicksPrintsMonth[,c(1,6,7)], variable, value, total) %>% arrange(yearMonth)
  colnames(grafTotalMonth)<-c("Marca", "Periodo", "Tipo", "Valor")
  #
  listaTablas$graftotalMonth<-grafTotalMonth
  tblTotalMonth<-gather(topBrandClicksPrintsMonth[,c(1,6,7)], variable, value, total) %>% spread(yearMonth, value)
  tblTotalMonth<-tblTotalMonth[,-2]
  colNamesMonths<-format(as.Date(colnames(tblTotalMonth)[-1]), "%b")
  colnames(tblTotalMonth)<-c("Marca", colNamesMonths)
  tblTotalMonth[is.na(tblTotalMonth)]<-0
  tblTotalMonth$total<-rowSums(tblTotalMonth[,-1])
  #
  listaTablas$tblTotalMonth<-tblTotalMonth
  #
  #--- Dispositivos ---
  totalDispositivos<-t %>% filter(DivisionId == divisionId & !is.na(Brand) & between(searchdate, fechaInicial, fechaFinal)) %>% select(ISBN, TargetName) %>% inner_join(m, by=c("ISBN" = "ISBN")) %>% count(TargetName) %>% collect()
  colnames(totalDispositivos) <- c("Dispositivo", "Consultas totales")
  totalDispositivos <- cbind(totalDispositivos, Porcentaje=paste(round((totalDispositivos$`Consultas totales`*100)/sum(totalDispositivos$`Consultas totales`), 2), "%"))
  totalDispositivos<-totalDispositivos[order(totalDispositivos$`Consultas totales`, decreasing = T),]
  listaTablas$totalDispositivos<-totalDispositivos
  #
  #--- Profesiones
  totalProfesion<-t %>% filter(DivisionId == divisionId & !is.na(Brand) & between(searchdate, fechaInicial, fechaFinal) & !is.na(ProfessionName)) %>% select(ISBN, searchdate, ProfessionName) %>% inner_join(m, by=c("ISBN" = "ISBN")) %>% count(ProfessionName) %>% collect()
  totalProfesion<-mapeoProfesiones(totalProfesion)
  totalProfesion<-totalProfesion %>% group_by(ProfessionNameStd) %>% summarise(`Consultas totales`=sum(n, na.rm = T))
  colnames(totalProfesion)[1]<-"Profesión"
  consultasProfesion <- transform(totalProfesion, Porcentaje=paste(round((`Consultas totales`*100)/sum(`Consultas totales`), 2), "%"))
  listaTablas$totalProfesion<-consultasProfesion
  return(listaTablas)
}

listaTablas<-getClicksPrints(DBase, 170, "2016", "2018")

