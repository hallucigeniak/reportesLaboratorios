###########################################################################
###--- APLICACION PARA VISUALIZAR Y GENERAR REPORTES POR LABORATORIO ---###
###########################################################################
#
#
#--- OPCIONES DEL SERVIDOR ----
#options(shiny.host = "195.192.2.126")
#options(shiny.port = 5070)
#
#--- CARGAR DEPENDENCIAS ----
source("Dependencias/loadDependencies.R", echo = F)
#--- USER INTERFACE ----
ui <- dashboardPage(
  dashboardHeader(title = "REPORTES POR LABORATORIO",
                  titleWidth = 350
  ),
  dashboardSidebar(width = 350,
                   sidebarMenu(
                     menuItem(strong("BÚSQUEDA"), tabName = "sqlQueries", icon=icon("search")#,
                              #menuItem("Búsqueda", tabName = "sqlQueries")
                     ),
                     menuItem(strong("RESUMEN GLOBAL"), tabName = "resumenGlobal", icon=icon("eye")#,
                              #menuItem("Resumen", tabName = "resumenGlobal")
                       
                     ),
                     menuItem(strong("REPORTE"), icon=icon("line-chart"),
                              menuItem("Consultas por marcas", tabName = "resumenMarcas"),
                              menuItem("Consultas por dispositivos", tabName = "resumenDispositivos"),
                              menuItem("Conusltas por profesiones", tabName = "resumenProfesiones"),
                              menuItem("Consultas por especialidades", tabName = "resumenEspecialidades")
                     ),
                     menuItem(strong("DESCARGAR REPORTE"), tabName = "downloadReport", icon = icon("download")#,
                              #menuItem("Descarga", tabName = "downloadReport")
                     ),
                     br(),
                     br(),
                     br(),
                     br(),
                    withSpinner(uiOutput("logo"))
                   )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(
        tabName="sqlQueries",
        h1("Buscar:"),
        fluidRow(
          column(width=6,
                 box(
                   title=h3("Fechas"), width = NULL, status = "primary",
                   uiOutput("rangoFechas")
                 )
          ),
          column(width=6,
                 box(
                   title=h3("Comparar con años anteriores:"), width = NULL, status="primary",
                   uiOutput("comparePeriod")
                 )
          )
        ),
        fluidRow(
          column(width=6,
                 box(
                   title = h3("País"), width = NULL, status = "primary",
                   uiOutput("listaPaises")
                 )
          ),
          column(width=6,
                 box(
                   title= h3("Laboratorio"), width = NULL, status = "primary",
                   uiOutput("listaDivisions")
                 )
          )
        ),
        fluidRow(
          box(
            title=h3("GENERAR REPORTE"), width = NULL, status = "primary",
            tags$head(
              tags$style(HTML('#generarReporte{background-color:lightgreen}'))
            ),
            actionButton("generarReporte", label = "Iniciar", style='padding:8px; font-size:150%')
          )
        )
      ),
      tabItem(tabName = "resumenGlobal",
              h1("Resumen Global"),
              h4(textOutput("textoPeriodoResGlobal")),
              br(),
              br(),
              fluidRow(
                column(width=12, valueBoxOutput("valueBoxTotales", width = 12))
              ),
              fluidRow(
                column(width=12, valueBoxOutput("valueBoxTopMarca", width = 12))
              ),
              fluidRow(
                column(width=12, valueBoxOutput("valueBoxDispositivos", width=12))
              ),
              fluidRow(
                column(width=12, valueBoxOutput("valueBoxProfesiones", width=12))
              ),
              fluidRow(
                column(width=12, valueBoxOutput("valueBoxEspecialidades", width=12))
              )
      ),
      tabItem(tabName = "resumenMarcas",
              h1("Consultas a marcas"),
              h4(textOutput("textoPeriodoMarca")),
              fluidRow(
                box(
                  title=h2("Resumen de consultas totales"), width = NULL, status = "primary",
                  plotOutput("grafResumen"),
                  dataTableOutput("tablaConsultasTotales"),
                  textOutput("textoIncremento")
                  ) 
              ),
              fluidRow(
                box(
                  title=h2("Marcas más consultadas del periodo"), width = NULL, status = "primary",
                  uiOutput("selectTopBrands"),
                  plotOutput("grafTopBrands"),
                  dataTableOutput("resumenMarcas")
                ),
                box(
                  title=h2("Marcas más consultadas por mes"), width= NULL, status = "primary",
                  plotOutput("grafTopBrandsMensual"),
                  h2("Tabla de consultas por marca por mes"),
                  dataTableOutput("tablaTopBrandsMensual")
                )
              )
      ),
      tabItem(tabName = "resumenDispositivos",
              h1("Consultas por dispositivo"),
              h4(textOutput("textoPeriodoDispositivos")),
              fluidRow(
                column(width = 12, align="center",
                       box(
                         width = NULL, status = "primary",
                         plotOutput("grafDispositivos"),
                         dataTableOutput("tablaDispositivos")
                       )
                )
              )
      ),
      tabItem(tabName = "resumenProfesiones",
              h1("Consultas por profesión"),
              h4(textOutput("textoPeriodoProfesion")),
              fluidRow(
                column(width = 12, align="center",
                       box(
                         width = NULL, status = "primary",
                         popify(plotOutput("grafTopProfesiones"), title="Otras profesiones puede incluir:", "Agrónomo, Asociación, Biólogo, Distribuidor, Dueño De Clínica Veterinaria, Dueño De Farmacia Veterinaria, Dueño De Mascota, Estudiante, Farmacia Veterinaria, Ingeniero Biomédico, Ingeniero En Alimentos, Ingeniero Químico, Médico Veterinario, Otra, Paciente, Personal Industria Farmacéutica, Productor, Representante Médico, Universidad"),
                         dataTableOutput("tablaTopProfesiones")
                       )
                )
              ),
              h1("Consultas por profesión por mes"),
              fluidRow(
                column(width = 12, align="center",
                       box(
                         width = NULL, status = "primary",
                         plotOutput("grafTopProfesionesMensual"),
                         dataTableOutput("tablaTopProfesionesMensual")
                       )
                )
              )
      ),
      tabItem(tabName = "resumenEspecialidades",
              h1("Consultas por especialidad"),
              h4(textOutput("textoPeriodoEspecialidad")),
              fluidRow(
                column(width = 12, align="center",
                       box(width = NULL, status = "primary",
                           #uiOutput("selectTopEspecialidades"),
                           sliderInput("sliderNespecialidades", "Ver número de especialidades:", min=5, max=20, value = 10, step=5),
                           popify(plotOutput("grafTopEspecialidades"), title= "En OTRAS ESPECIALIDADES se pueden incuir:", "Animales Exóticos, Aves, Bovinos Carne, Bovinos Leche, Cerdos, Dueño De Mascota, Equinos, Otra, Ovejas Y Cabras, Pequeñas Especies, Reproducción Animal"),
                           dataTableOutput("tablaTopEspecialidades")
                       )
                )
              ),
              h1("Consultas por especialidad por mes"),
              fluidRow(
                column(width = 12, align="center",
                       box(width = NULL, status = "primary",
                           plotOutput("grafTopEspecialidadesMensual"),
                           dataTableOutput("tablaTopEspecialidadesMensual")
                       )
                )
              )
      ),
      tabItem(tabName= "downloadReport",
              h1("Descargar reporte"),
              h4(textOutput("textoPeriodoDescarga")),
              fluidRow(
                column(width = 12,
                       box(title=h2("Secciones en el Reporte"), status = "primary",
                         checkboxGroupInput("reportSections", "Incluir secciones:", choices = list(Resumen="SECCION RESUMEN", Dispositivos="SECCION DISPOSITIVOS", Profesiones="SECCION PROFESIONES", Especialidades="SECCION ESPECIALIDADES"), selected = c("SECCION RESUMEN", "SECCION DISPOSITIVOS","SECCION PROFESIONES", "SECCION ESPECIALIDADES")),
                         tags$head(
                           tags$style(HTML('#saveData{background-color:lightgreen}'))
                         ),
                         actionButton("saveData", "Guardar datos")
                       ) 
                )
              )
      )
    )
  )
)

####---- SERVIDOR ----
server <- function(input, output) {
  #
  flag<-reactiveValues()
  flag$start<-TRUE
  flag$inProcess<-FALSE
  #---- RENDER CALENDARIO ----
  output$rangoFechas<-renderUI({
    years<-unique(catalogoLabs$TrackingYear)
    minDate<-paste(min(years), "01", "01", sep="-")
    maxDate<-paste(max(years), "12", "31", sep="-")
    dateRangeInput("selectFechas", "", startview= "year", language = "es", separator = "hasta", min=minDate, max=maxDate, start=minDate, end=maxDate)
  })
  #
  #--- RENDER LISTA PAISES ----
  observeEvent(input$selectFechas,
               {
                 periodos<-unique(year(input$selectFechas))
                 catPaises<-unique(catalogoLabs[which(catalogoLabs$TrackingYear%in%periodos), c("CountryNameLab", "CountryLabId")])
                 output$listaPaises<-renderUI({
                   selectInput("selectPais", "", choices = split(catPaises$CountryLabId, catPaises$CountryNameLab), selected = 11)
                 })
               }
  )
  #
  #---- RENDER LISTA LABORATORIOS ----
  observeEvent({input$selectPais
    input$selectFechas},
               {
                 periodos<-unique(year(input$selectFechas))
                 listaDivisions<<-unique(catalogoLabs[which(catalogoLabs$CountryLabId==input$selectPais & catalogoLabs$TrackingYear%in%periodos), c("DivisionIdNorm", "DivisionNameNorm")])
                 colnames(listaDivisions)<<-c("DivisionIdNorm", "DivisionNameNorm")
                 output$listaDivisions<-renderUI({
                   selectInput("listaDivisions", "", choices= split(listaDivisions$DivisionIdNorm, listaDivisions$DivisionNameNorm))
                 })
               }
  )
  #
  #--- RENDER CHECKBOX COMPARAR PERIODOS ANTERIORES ----
  observeEvent(input$selectFechas,
               {
                 periodos<-year(input$selectFechas)
                 YearBehindPeriodos<-periodos-1
                 if (all(YearBehindPeriodos%in%catalogoLabs$TrackingYear)){

                   output$comparePeriod<-renderUI({
                     input$focusYear
                     checkboxInput("compareYears", "Comparar con periodo anterior")
                   })
                 } else {
                   output$comparePeriod<-renderText("No disponible")
                 }
               }
  )
  ###--- RENDER LOGO ----
  whichLogo<-reactive({
    if (flag$start){
        plmLogo<-'<center><img src="http://www.plmconnection.com/plmservices/PharmaSearchEngine/Mexico/DEF/SIDEF/400x400/plm.png" width="50%" height="50%" align="middle"></center>'
        startFlag<-FALSE
        plmLogo
    }else{
        validate(need(!is.null(input$generarReporte), ''))
        labId<-isolate(input$listaDivisions)
        labId<-as.numeric(labId)
        fileName<-listaLogos$FileName[which(listaLogos$DivisionId==labId)]
        paisId<-isolate(input$selectPais)
        paisName<-listaPaises$countryName[which(listaPaises$countryId==paisId)]
        logo.url<<-paste0("http://www.plmconnection.com/plmservices/PharmaSearchEngine/", paisName, "/DEF/Logos/400x400/", fileName)
        size<-50+runif(1)
        htmlImage<-paste0('<center><img src="',logo.url,'"width="', size, '%" height="',size,'%" align="middle"></center>')
        htmlImage
    }
  })
    output$logo<-renderText(whichLogo())

  #---- REACT TO Generar Reporte ----
  observeEvent(input$generarReporte,
               {
                 flag$start<-FALSE
                 showModal(modalDialog(title = h3("Petición enviada"), h4("Por favor espera..."), size="m", easyClose = F, footer = NULL,  fade=T ))
                 #--- Armar query ----
                 fechaInicial<-input$selectFechas[1]
                 fechaFinal<-input$selectFechas[2]
                 divisionId<-input$listaDivisions
                 divisionId<-paste(unique(catalogoLabs$DivisionId[which(catalogoLabs$DivisionIdNorm==divisionId)]), collapse = ", ")
                 #--- Consultas actuales
                 tablaResultados<-fullQuery(divisionId, fechaInicial, fechaFinal)
                 #
                 #--- Validacion fechaInicial < FechaFinal
                 if (class(tablaResultados) == "character" ){
                   if (tablaResultados == "fechas"){
                     showModal(modalDialog(
                       title = h3("Rango de fechas incorrecto"),
                       h4(paste0("Asegurate de que la fecha inicial sea anterior a la fecha final")),
                       easyClose = TRUE,
                       footer = NULL
                     ))
                   } 
                   else if (nchar(tablaResultados) > 10){ #tablaREsultados trae el Mensaje de error
                     showModal(modalDialog(
                       title = h3("Error:"),
                       h4(paste0(tablaResultados)),
                       easyClose = TRUE,
                       footer = NULL
                     ))
                   }
                 } 
                 else if (class(tablaResultados) == "data.frame"){
                 listaTablas<<-procesarTablas(tablaResultados, Actual=T)
                 #--- Consultas de hace un año
                 print(input$compareYears)
                 if (!is.null(input$compareYears)){
                   if (input$compareYears){
                     print("Comparar año")
                     yearBehindDates<-as.Date(input$selectFechas) %m-% years(1)
                     tablaLastYear<-fullQuery(divisionId, yearBehindDates[1], yearBehindDates[2])
                     consultasTotalesLastYear<<-procesarTablas(tablaLastYear, Actual = F)
                     listaTablas$grafPeriodosActual<<-rbind(listaTablas$grafPeriodosActual, consultasTotalesLastYear)
                   }
                 }
                 ###--- GENERAR VALUE BOX TOTALES ---###
                 output$valueBoxTotales<-renderValueBox({
                   s<-sum(listaTablas$TopMarcas$Consultas)
                   valueBox(format(s, big.mark = ",", scientific = F), "Consultas TOTALES", color= "navy")
                 })
                 output$valueBoxTopMarca<-renderValueBox({
                   a<-listaTablas$grafTopMarcas$Consultas[1]
                   s<-format(a, big.mark = ",", scientific = F)
                   s<-paste(s, "consultas a \n", listaTablas$grafTopMarcas$Marca[1])
                   valueBox(s, "Marca más consultada", color= "blue")
                 })
                 output$valueBoxDispositivos<-renderValueBox({
                   dispositivo<-listaTablas$consultasDispositivos[order(listaTablas$consultasDispositivos$Consultas, decreasing = T),][1,1]
                   no.consultas<-listaTablas$consultasDispositivos[order(listaTablas$consultasDispositivos$Consultas, decreasing = T),][1,2]
                   no.consultas<-format(no.consultas, big.mark = ",", scientific = F)
                   s<-paste(no.consultas, "consultas a través de", dispositivo)
                   valueBox(s, "Plataforma más usada", color="aqua")
                 })
                 output$valueBoxProfesiones<-renderValueBox({
                   validate(need(listaTablas$consultasTotalesProfesion, "NO HAY DATOS DE PROFESIONES"))
                   profesion<-listaTablas$consultasTotalesProfesion[1,1]
                   no.consultas<-listaTablas$consultasTotalesProfesion[1,2]
                   no.consultas<-format(no.consultas, big.mark = ",", scientific = F)
                   s<-paste(no.consultas, "consultas hechas por", profesion)
                   valueBox(s, "Profesionistas que más consultaron", color="light-blue")
                 })
                 output$valueBoxEspecialidades<-renderValueBox({
                   validate(need(listaTablas$consultasTotalesEspecialidades, "NO HAY DATOS DE ESPECIALIDADES"))
                   especialidad<-listaTablas$consultasTotalesEspecialidades[1,1]
                   no.consultas<-listaTablas$consultasTotalesEspecialidades[1,2]
                   no.consultas<-format(no.consultas, big.mark = ",", scientific = F)
                   s<-paste(no.consultas, "consultas hechas por", especialidad)
                   valueBox(s, "Especialidad con más consultas", color="teal")
                 })
                 ###--- GENERAR TEXTO DEL PERIODO ---###
                 fechaIni<-format(as.Date(input$selectFechas[1]), "%d-%b-%Y")
                 fechaFin<-format(as.Date(input$selectFechas[2]), "%d-%b-%Y")
                 output$textoPeriodoResGlobal<-renderText({periodo<-paste0("del periodo: ", fechaIni, " a ", fechaFin)
                 periodo
                 })
                 output$textoPeriodoMarca<-renderText({periodo<-paste0("del periodo: ", fechaIni, " a ", fechaFin)
                   periodo
                 })
                 output$textoPeriodoDispositivos<-renderText({periodo<-paste0("del periodo: ", fechaIni, " a ", fechaFin)
                 periodo
                 })
                 output$textoPeriodoProfesion<-renderText({periodo<-paste0("del periodo: ", fechaIni, " a ", fechaFin)
                 periodo
                 })
                 output$textoPeriodoEspecialidad<-renderText({periodo<-paste0("del periodo: ", fechaIni, " a ", fechaFin)
                 periodo
                 })
                 output$textoPeriodoDescarga<-renderText({periodo<-paste0("del periodo: ", fechaIni, " a ", fechaFin)
                 periodo
                 })
                 ###--- GENERAR GRAFICA DE RESUMEN ---###
                 listaGraficas<<-list()
                 listaGraficas$resumenPeriodo<<-graficarResumenMarcas(listaTablas$grafPeriodosActual)
                 output$grafResumen<-renderPlot({
                   listaGraficas$resumenPeriodo
                 })
                 ###--- GENERAR TABLA DE RESUMEN ---###
                 output$tablaConsultasTotales<-renderDataTable(
                   listaTablas$consultasPeriodos
                   #options = list(pageLength=NROW(listaTablas$consultasPeriodos))
                 )
                 ###--- GENERAR TABLA DE MARCAS DEL LABORATORIO ---###
                 output$tablaTopBrandsMensual<-renderDataTable(
                   listaTablas$TopMarcasMes
                   #options = list(pageLength=10)
                 )
                 ###--- GENERAR SLIDER TOP MARCAS ---###
                 output$selectTopBrands<-renderUI({
                   sliderInput("selectNumBrands", "Ver número de marcas:", min=5, max=20, value = 10, step=5) 
                 })
                 output$resumenMarcas<-renderDataTable(
                   listaTablas$tblTopMarcas
                   #options = list(pageLength=10)
                 )
                 ###--- GENERAR TABLA Y GRAFICA DE DISPOSITIVOS ---###
                 listaGraficas$consultasDsipositivos<<-graficaPastelDispositivos(listaTablas$consultasDispositivos)
                 output$grafDispositivos<-renderPlot({
                   listaGraficas$consultasDsipositivos
                 })
                 output$tablaDispositivos<-renderDataTable(
                   listaTablas$tblConsultasDispositivos
                 )
                 ###--- GENERAR TABLA Y GRAFICA POR PROFESIONES ---###
                 #if (!is.na(listaTablas$consultasTotalesProfesion) && !is.na(listaTablas$tblConsultasTotalesProfesion)){
                   listaGraficas$consultasProfesiones<<-graficarProfesiones(listaTablas$consultasTotalesProfesion)
                   #--- Grafica de Barras Profesiones
                   output$grafTopProfesiones<-renderPlot({
                     validate(need(listaTablas$consultasTotalesProfesion, "NO HAY DATOS DISPONIBLES"))
                     listaGraficas$consultasProfesiones
                   })
                   output$tablaTopProfesiones<-renderDataTable({
                     validate(need(listaTablas$consultasTotalesProfesion, "NO HAY DATOS DISPONIBLES"))
                     listaTablas$tblConsultasTotalesProfesion
                   })
                   listaGraficas$profesionesMes<<-graficarProfesionesMes(listaTablas$grafProfesionMes)
                   output$grafTopProfesionesMensual<-renderPlot({
                     validate(need(listaTablas$grafProfesionMes, "NO HAY DATOS DISPONIBLES"))
                     listaGraficas$profesionesMes
                   })
                   output$tablaTopProfesionesMensual<-renderDataTable({
                     validate(need(listaTablas$consultasProfesionMes, "NO HAY DATOS DISPONIBLES"))
                     listaTablas$consultasProfesionMes
                   })
                   #--- Especialidades
                   nEspecialidades<-reactive(input$sliderNespecialidades)
                   output$grafTopEspecialidades<-renderPlot({
                     validate(need(listaTablas$consultasTotalesEspecialidades, "NO HAY DATOS DISPONIBLES"))
                     graficarTopEspecialidades(listaTablas$consultasTotalesEspecialidades, nEspecialidades())
                   })
                   output$tablaTopEspecialidades<-renderDataTable({
                     validate(need(listaTablas$tblConsultasTotalesEspecialidades, "NO HAY DATOS DISPONIBLES"))
                     listaTablas$tblConsultasTotalesEspecialidades
                   })
                   output$grafTopEspecialidadesMensual<-renderPlot({
                     validate(need(listaTablas$grafEspecialidadesMes, "NO HAY DATOS DISPONIBLES"))
                     graficarTopEspMensual(listaTablas$grafEspecialidadesMes, nEspecialidades())
                   })
                   output$tablaTopEspecialidadesMensual<-renderDataTable({
                     validate(need(listaTablas$consultasEspecialidadMes, "NO HAY DATOS DISPONIBLES"))
                     listaTablas$consultasEspecialidadMes
                   })
                   #--- REACT TO select top 10 Brands ----
                   observeEvent(input$selectNumBrands,
                                {
                                  #--- Consultas por TOP Producto en el periodo
                                  nBrands<-input$selectNumBrands
                                  output$grafTopBrands<-renderPlot({
                                    graficarTopMarcas(listaTablas$grafTopMarcas, nBrands)
                                  })
                                  #--- Consultas por TOP Producto X mes
                                  output$grafTopBrandsMensual<-renderPlot({
                                    graficarTopMarcasMensual(listaTablas$consultasMarcasMensual, nBrands)
                                  })
                                }
                   )
                 #} else{
                  # output$grafTopProfesiones<-renderText("No hay datos disponibles")
                  # output$tablaTopProfesiones<-renderText("No hay datos disponibles")
                  # output$grafTopProfesionesMensual<-renderText("No hay datos disponibles")
                  # output$tablaTopProfesionesMensual<-renderText("No hay datos disponibles")
                  # output$grafTopEspecialidades<-renderText("No hay datos disponibles")
                  # output$tablaTopEspecialidades<-renderText("No hay datos disponibles")
                  # output$grafTopEspecialidadesMensual<-renderText("No hay datos disponibles")
                  # output$tablaTopEspecialidadesMensual<-renderText("No hay datos disponibles")
                  # output$grafTopBrands<-renderText("No hay datos disponibles")
                  # output$grafTopBrandsMensual<-renderText("No hay datos disponibles")
                 #}
                 }
                 removeModal()
               }
  )

  ###--- SELECCIONAR  SECCIONES Y SUBSECCIONES DEL REPORTE PARA DESCARGAR ----
  includedSections<-reactive({
    paste(input$reportSections, collapse = "|")
  })
  
  includedSubSections<-reactive({
    a<-input$checkResumenSubsections
    b<-input$checkProfesionSubsections
    c<-input$checkEspecialidadesSubsections
    d<-c(a, b, c)
    paste(paste("SUBSECCION", d, sep="-"), collapse="|")
  })
  
  ###--- REACT TO Descargar Reporte ----
  
  observeEvent(input$saveData,
               {
                 #--- Crear directirio para el reporte
                 labId<-isolate(input$listaDivisions)
                 lab<-listaDivisions$DivisionName[which(listaDivisions$DivisionId==labId)]
                 RDataName<-paste0(Sys.Date(), ".RData")
                 pathDir<-file.path(getwd(), "Reportes", lab, Sys.Date())
                 dir.create(pathDir, showWarnings = F, recursive = T)
                 filePath<-file.path(pathDir, RDataName)
                 save(listaTablas, listaGraficas, file = filePath)
                 #--- Copiar logo plm.png
                 logoFilePath<-file.path(pathDir, "plm.png")
                 file.copy("Dependencias/plm.png", logoFilePath)
                 #--- Procesar archivo Rmd
                 copiaReporte<-readLines("Dependencias/plantillaReportes.Rmd")
                 copiaReporte[10]<-gsub('NAME-LABORATORIO', lab, copiaReporte[10], perl = T)
                 periodo<-paste(isolate(input$selectFechas), collapse = " a ")
                 copiaReporte[11]<-gsub("fecha1-fecha2", periodo, copiaReporte[11])
                 copiaReporte[17]<-gsub("objetosReporte.RData", RDataName, copiaReporte[17])
                 #--- Descomentar secciones del reporte para que aparezcan en el reporte
                 copiaReporte<-copiaReporte[grep(includedSections(), copiaReporte, invert = T)]
                 #--- Descomentar subsecciones para que aparezcan en el reporte
                 #copiaReporte<-copiaReporte[grep(includedSubSections(), copiaReporte, invert = T)]
                 rmdFilePath<-file.path(pathDir, "Reporte.Rmd")
                 writeLines(copiaReporte, rmdFilePath)
                 #--- Escribir tablas .xlsx
                 xlsxFilePath<-file.path(pathDir, "Tablas.xlsx")
                 for (i in 1:length(listaTablas)){
                   tabName<-names(listaTablas)[i]
                   if (grepl("graf|Mensual", tabName)){
                     next
                   }
                   if (file.exists(xlsxFilePath)){
                     file.remove(xlsxFilePath)
                   }
                   #write.xlsx2(as.data.frame(listaTablas[[i]]), sheet=tabName, file=xlsxFilePath, append = T, row.names = F)
                 }
                 #--- Escribir graficas en .pdf
                 pdfFilePath<-file.path(pathDir, "Graficas.pdf")
                 pdf(pdfFilePath, width = 16 , height = 10)
                 for (i in 1:length(listaGraficas)){
                   grafName<-names(listaGraficas)[i]
                   plot(listaGraficas[[i]])
                 }
                 dev.off()
                 #--- Guardar logo.png
                 pngFilePath<-file.path(pathDir, "Logo.png")
                 download.file(logo.url, pngFilePath, method = "wget")
                 #--- Subir reporte a servidor de Shiny
                 showModal(modalDialog(
                   title = h3("Guardando reporte"),
                   h4(paste0("El reporte se está guardando en el servidor")),
                   easyClose = FALSE,
                   footer = NULL
                 ))
                 rsconnect::deployApp(appDir = pathDir, appPrimaryDoc = "Reporte.Rmd", appSourceDoc = file.path(pathDir, "Reporte.rmd"),      account = "eniak", server = "shinyapps.io", appName = paste0("Reporte-", lab,"-", Sys.Date()),      appTitle = paste0("Reporte-", lab,"-", Sys.Date()), launch.browser = function(url) {message("Deployment completed: ", url)}, lint = FALSE, metadata = list(asMultiple = FALSE, asStatic = FALSE),      logLevel = "verbose")
                 removeModal()
                 reportUrl<-paste0("https://eniak.shinyapps.io/Reporte-",lab, "-", Sys.Date())
                 showModal(urlModal(reportUrl, 
                   title = h3("Reporte guardado"),
                   subtitle=h4("Ahora puedes consultar el reporte con el url proporcionado")
                 ))
               })
  
}
#
#--- Run the application ---#
shinyApp(ui = ui, server = server, options=options(shiny.host = "195.192.2.126", shiny.port=5070))
