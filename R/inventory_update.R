#' inventory update
#'
#' Takes in the unedited vial inventory and outputs the updated inventory as a .csv saved to disk.
#' @param inventoryFile character; character string of the .csv file that includes alphanumeric barcodes and any additional vial information of inventory to be updated
#' @param removalRecord character; character string of the .csv file that includes alphanumeric barcodes of vials to remove from inventory file
#' @param removeVials logical; should rows be removed from inventory? Default TRUE. If FALSE, then rows will not be removed. A new column will be added indicating the vials' inventory status.
#' @param columnName character; name of the column that contains the alphanumeric codes. inventoryFile and removalRecord must both have a column with this name containing the alplhanumeric codes. Default "barcode".
#' @param writeToFile logical; should updated inventory data be written to disk? Default TRUE. If FALSE, output will not be written to file.
#' @return An updated inventory file in .csv format.
#' @examples
#' list.files(pattern="\\.csv$")
#' [1] "current inventory.csv" [2] "removed vials.csv
#' inventory_update(inventoryFile = "current inventory.csv", removalRecord = "removed vials.csv",filename = "current inventory 11Nov2020.csv")
#' [1] "current inventory 11Nov2020.csv"
#' @export
inventory_update <- function(inventoryFile, removalRecord, removeVials = TRUE, columnName = "barcode", writeToFile = TRUE){
  inventory.file <- read.csv(inventoryFile,stringsAsFactors=FALSE,check.names=F)
  removal.file <- read.csv(removalRecord,stringsAsFactors=FALSE,check.names=F)
  if(regexpr(pattern="\\.csv$",inventoryFile)==-1){
    stop("incorrect inventory file filetype; must be .csv format")
  }
  if(regexpr(pattern="\\.csv$",removalRecord)==-1){
    stop("incorrect removal record filetype; must be .csv format")
  }
  if(!is.logical(removeVials)){
    stop("incorrect remove vials format; must be logical (TRUE or FALSE)")
  }
  if(typeof(columnName)!="character"){
    stop("incorrect alphanumeric column name; must be type character")
  }
  if(!columnName %in% colnames(inventory.file)){
    stop("columnName not a column in inventoryFile")
  }
  if(!columnName %in% colnames(removal.file)){
    stop("columnName not a column in removalRecord")
  }
  if(dim(removal.file)[1]>dim(inventory.file)[1]){
    stop("inventoryFile has more rows than removalRecord. Cannot remove more vials than are in inventory. Check that arguments are correctly defined.")
  }
  current <- inventory.file[,columnName]
  removed <- removal.file[,columnName]
  updated <- inventory.file[-which(current %in% removed),]
  date. <- strftime(Sys.time(), "%d-%m-%Y")
  date.split <- strsplit(date.,"-")[[1]]
  date.split[2] <- month.abb[as.numeric(strsplit(date.,"-")[[1]][2])]
  date.correct <- paste0(date.split,collapse="")
  if(writeToFile==TRUE){
    write.csv(updated,file=paste0("vial inventory ",date.correct,".csv"),row.names=F)
  }
  return(paste0("vial inventory ",date.correct,".csv"))
}
