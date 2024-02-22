library("RPostgres")
require("dbplyr")
require("dplyr")

wbbData <- readRDS("/Volumes/Secomba/matthiassoot/Boxcryptor/Ohne\ Titel/Kaufpreise+oz.rds")
wbbData$HAUS <- trimws(wbbData$HAUS, which = "left", whitespace = "[ \t\r\n]")


pw <- "!$MSOM2+3?"
################ Programmablauf ###############################

# loads the PostgreSQL driver
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- DBI::dbConnect(RPostgres::Postgres(), dbname = "aks-oga",
                      host = "localhost", port=5432,
                      user = "ben-oga",
                      password = pw
)


lod2 <- data.frame()
##################### 1. Datensatz Selektieren
for (s in seq(51,3951,50)) {
  lod2_temp <- read.csv(paste("~/Desktop/GML/",s,".csv", sep=""))
  lod2 <- rbind(lod2, lod2_temp)
}
for (s in seq(4001,10101,50)) {
  lod2_temp <- read.csv(paste("~/Desktop/GML/GML",s,".csv", sep=""))
  lod2 <- rbind(lod2, lod2_temp)
}
for (s in seq(1,1051,50)) {
  lod2_temp <- read.csv(paste("~/Downloads/GML/",s,".csv", sep=""))
  lod2 <- rbind(lod2, lod2_temp)
}


LOD2Strase <- subset(lod2, !is.na(lod2$LABE) & !is.na(lod2$HAUS))
LOD2Strase <- LOD2Strase[!duplicated(LOD2Strase$id),]
LOD2Strase$GEME <- as.character(paste("0",LOD2Strase$GEME,sep=""))

names(LOD2Strase) <- c("id" ,  "dqd" , "dqb" , "dql",  "geme", "ah"  , "mth"  ,"fh"  , "rt"  , "fu"  , "ho" ,  "gemn", "haus", "labe", "gruf", "volu")
dbWriteTable(con, "lod", LOD2Strase, overwrite=TRUE)
