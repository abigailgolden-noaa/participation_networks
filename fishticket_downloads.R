source("C:/Program Files/R/connectioninfoROracle.R")


#connectioninfo file:
  
library(DBI)
library(odbc)


pacfin <- DBI::dbConnect(odbc::odbc(),
                         host   = "pacfindb.psmfc.org",
                         UID    = 'agolden',
                         PWD    = 'pacfinpass',
                         dsn    = 'pacfin',
                         port   = 2045
                         )

pacfin <- DBI::dbConnect(odbc::databricks(),
                         httpPath = "")

# to query the DB:
  
  dat <- dbGetQuery(pacfin, "your query")
