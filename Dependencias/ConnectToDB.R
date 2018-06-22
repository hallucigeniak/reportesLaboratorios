#################################################
###--- GENERAR CONEXION A LA BASE DE DATOS ---###
#################################################
#
#dependencias<-c("odbc")
#
#con <- dbConnect(odbc(),
#                 Driver = "ODBC Driver 17 for SQL Server",
#                 Server = "195.192.2.252",
#                 Database = "plmtracking_2017",
#                 UID = "eniak.hernandez",
#                 PWD = "DMTemporal*")

con <- dbConnect(odbc(),
                 Driver = "ODBC Driver 17 for SQL Server",
                 Server = "10.77.230.184,5496",
                 Database = "plmtracking_2018",
                 UID = "eniak.hernandez",
                 PWD = "EnHLidm19LC")
