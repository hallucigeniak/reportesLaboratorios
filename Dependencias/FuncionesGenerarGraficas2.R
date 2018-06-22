#####################################################
###--- GENERAR GRAFICAS DE REPORTE DE MINERIAS ---###
#####################################################
#
#dependencias<-c("ggplot2", "scales")
#
graficarResumenMarcas<-function(tabla){
  ggplot(tabla, aes(x=Periodo, y=value, group=key, colour=key)) + geom_line(size=1) + geom_point(size=3) + geom_text(aes(label=format(value, big.mark = ",", scientific=F), vjust=-1)) + labs(x="Mes", y="") + expand_limits(y=0) + theme(axis.title = element_text(size=16), axis.text.x=element_text(size=12), axis.text.y=element_text(hjust=1, size=10), axis.line = element_line(colour = "black"), legend.title=element_blank()) +  scale_y_continuous(labels = comma, expand = c(0.05, 10)) + scale_x_date(labels = date_format("%b"), date_breaks = "1 month")
  #ggplot(data = tabla, aes(x=searchdateMonths, y=ConsultasTotales, group=Periodo, colour=Periodo)) + geom_line(size=1) + geom_point(size=3) + geom_text(aes(label = format(ConsultasTotales, big.mark = ",", scientific = F), vjust= -1), size = 4) + labs(x = "Mes", y = "Consultas") + expand_limits(y=0) + theme(axis.title = element_text(size=16), axis.text.x=element_text(size=12), axis.text.y=element_text(hjust=1, size=10), legend.title=element_text(hjust=0.5, size=14, face="bold"), axis.line = element_line(colour = "black"), legend.title=element_blank()) + scale_x_date(labels = date_format("%b"), date_breaks = "1 month") + scale_y_continuous(labels = comma, expand = c(0.05, 10))
}
#
graficarTopMarcas<-function(tabla, Nbrands){
  topBrands<-unique(tabla$Marca)[1:Nbrands]
  tablaTopBrands<-as.data.frame(tabla[which(tabla$Marca %in% topBrands),])
  tablaTopBrands<tablaTopBrands[order(tablaTopBrands$total, -xtfrm(tablaTopBrands$Marca), xtfrm(tablaTopBrands$variable), decreasing = T),]
  #  
  ggplot(tablaTopBrands, aes(x=reorder(Marca, -total), y=value, fill=factor(variable, levels = c("Prints", "Clicks")) )) + geom_bar(stat="identity") + geom_text(aes(label=format(round(value), big.mark = ",", scientific = F)), position = "stack", color="black", size=4) + theme(legend.title=element_blank(), axis.title = element_text(size=16), axis.text.y=element_text(hjust=1, size=12), axis.text.x=element_text(angle=45, hjust=1, size=12), axis.line = element_line(colour = "black")) + labs(x = "Marca", y = "") + scale_y_continuous(labels = comma, expand = c(0.1, 0))
  #ggplot(data=tabla[1:Nbrands,], aes(x=reorder(Marca, -Consultas), y=Consultas, fill=Consultas)) + geom_bar(stat="identity") + geom_text(aes(label=format(Consultas, big.mark = ",", scientific = F)), vjust=-0.5, color="black", size=4) + theme(legend.position="none", axis.title = element_text(size=16), axis.text.x=element_text(angle=45, hjust=1, size=12), axis.text.y=element_text(hjust=1, size=12), axis.line = element_line(colour = "black")) + theme(legend.position="none") + labs(x = "Marca", y = "Consultas") + scale_y_continuous(labels = comma, expand = c(0.1, 0))
}
#
graficarTopMarcasMensual<-function(tabla, Nbrands){
  topMarcas<-listaTablas$tblTopBrands[1:Nbrands,]$Marca
  tablaTopBrand<-tabla[which(tabla$Marca %in% topMarcas),]
  yLabel<-unique(tablaTopBrand$Tipo)
  
  ggplot(tablaTopBrand, aes(x=Periodo, y=Valor, colour=Marca)) + geom_line(aes(group = Marca), size=1) + geom_point(size=2) + labs(x = "Mes", y = yLabel) + theme(axis.title = element_text(size=16), axis.text.x=element_text(size=12), axis.text.y=element_text(hjust=1, size=10), legend.title=element_text(hjust=0.5, size=14, face="bold"), axis.line = element_line(colour = "black")) + scale_y_continuous(labels = comma) + scale_x_date(labels = date_format("%b"), date_breaks = "1 month")
}
#
graficaPastelDispositivos<-function(tabla){
  Dispositivos<-paste(tabla$Dispositivo, tabla$Porcentaje, " ")
  ggplot(tabla, aes(x="", y=`Consultas totales`, fill=Dispositivos)) + geom_bar(width=1, stat="identity", color="black") + coord_polar(theta="y", start=0) + theme_void() + labs(x="", y="") + theme(legend.title=element_text(hjust=0.5, size=20, face="bold"), legend.text = element_text(size = 18)) + guides(fill=guide_legend(override.aes=list(colour=NA)))
}
#
graficarTopEspecialidades<-function(tabla, Nespecialidades){
  # Grafica de barras TOP "Consultas por especialidad"
  ggplot(tabla[1:Nespecialidades,], aes(x=reorder(Especialidad, -Consultas), y=Consultas, fill=Consultas)) + geom_bar(stat="identity") + geom_text(aes(label=Porcentaje), vjust=-0.7, size=4) + theme(legend.position="none", axis.title = element_text(size=16), axis.text.x=element_text(angle=45, hjust=1, size=12), axis.text.y=element_text(hjust=1, size=10),axis.line = element_line(colour = "black")) + labs(x="Especialidad", y="Consultas") + scale_y_continuous(labels = comma, expand = c(0.1, 0))
}
#
graficarTopEspMensual<-function(tabla, Nespecialidades){
  topEsp<-listaTablas$consultasTotalesEspecialidades[1:Nespecialidades,]$Especialidad
  tablaEspecialidad<-tabla[which(tabla$Especialidad %in% topEsp),]
  ggplot(tablaEspecialidad, aes(x=Fecha, y=Consultas, colour=Especialidad)) + geom_line(aes(group = Especialidad), size=1) + geom_point(size=2) + labs(x="Mes", y="Consultas") + theme(plot.title = element_text(hjust=0.5, size=15), legend.title=element_text(hjust=0.5, size=14, face="bold"), axis.title = element_text(size=16), axis.text.x=element_text(size=12), axis.text.y=element_text(hjust=1, size=10), axis.line = element_line(colour = "black")) + scale_x_date(labels = date_format("%b"), date_breaks = "1 month") + theme() + scale_y_continuous(labels = comma)
  
}
#
graficarProfesiones<-function(tabla){
  ggplot(tabla, aes(x=reorder(Profesión, -`Consultas totales`), y=`Consultas totales`, fill=`Consultas totales`)) + geom_bar(stat="identity") + geom_text(aes(label=Porcentaje), vjust=-0.7, size=5) + theme(legend.position="none", axis.title = element_text(size=16), axis.line = element_line(colour = "black"), axis.text.x=element_text(size=12), axis.text.y=element_text(hjust=1, size=10)) + labs(x="Profesión", y="Consultas") + scale_y_continuous(labels = comma, expand = c(0.1, 0))
}
#
graficarProfesionesMes<-function(tabla){
  ggplot(tabla, aes(x=Fecha, y= `Consultas totales`, colour=Profesión)) + geom_line(aes(group = Profesión), size=1) + geom_point(size=2) + labs(x="Mes", y="Consultas") + theme(axis.title = element_text(size=16), axis.text.x=element_text(size=12), axis.text.y=element_text(hjust=1, size=10), legend.title=element_text(hjust=0.5, size=14, face="bold"), axis.line = element_line(colour = "black")) + scale_x_date(labels = date_format("%b"), date_breaks = "1 month") + scale_y_continuous(labels = comma)
}


ggplot(clicksPrintsTotal, aes(Mes))
