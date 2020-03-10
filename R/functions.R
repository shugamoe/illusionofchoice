getData  <- function(dataType){
  rv <- switch(dataType, "proto" = getDataProto(),
               )
  (rv)
}

getDataProto  <- function(dataDir = "data/"){
  require(data.table)

  (dataProto <- fread(paste0(dataDir, 'protodat.csv')))
}
