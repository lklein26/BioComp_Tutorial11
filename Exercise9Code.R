coeffvar <- function(dir, y){
  files<- list.files(path="dir")
  dataset <-data.frame()
  for (i in 1:length(file_list)){
    temp_data <- read_excel(file_list[i], range = cell_cols("y"))
    temp_data$Class <- sapply(strsplit(gsub(".xlsx", "", files[i]), "_"), function(x){x[2]}) 
    dataset <- rbind(dataset, temp_data) 
  }
  read <- lapply(dataset, sd)
  if nrow(y)<50 {
    warning("There are less than 50 values in this column; not recommended")
  } else (print(read))
}


#go through all files (use for loop)
#calculate mean and save in variable
#calculate sd and save in variable
#divide 2 variables
#display warning message if fewer than 50 entries 

function(dir, column, requiredobs){
files<- list.files(path="C:/Users/Lauren K/Desktop/Biocomptutorials/BioComp_Tutorial11/check")
temp_data <-data.frame(length=length(files))
for (i in 1:length(files)){
  temp_data[i] <- files[i]
}

coeffvar<-function(x, column){
  name <<- read.csv("x")
  stdev<-sd(name[,column], na.rm = TRUE)
  meanf<-mean(name[,column], na.rm = TRUE)
  coeff<- stdev / meanf
  
  return(coeff)

  if numrow(name1)<50 {
    warning("less than 50")
  } else (print(coeff))


load_data <- function(path) { 
  files <- dir(path)
  tables <- lapply(files, summary)
}
  
load_data("C:/Users/Lauren K/Desktop/Biocomptutorials/BioComp_Tutorial11/check")

coeffvar<-function(x, column){
  name <<- read.csv("x")
  stdev<-sd(name[,column], na.rm = TRUE)
  meanf<-mean(name[,column], na.rm = TRUE)
  coeff<- stdev / meanf

  return(coeff)
}

coeffvar(File1.csv, 2)
name1<-read.csv("File1.csv")
1:nrow(name1)


for 
if nrow(name1)<50 {
  warning("less than 50")
} else (print(sd))


files <- list.files(path="path/to/dir", pattern="*.txt", full.names=TRUE, recursive=FALSE)
lapply(files, function(x) {
  t <- read.table(x, header=TRUE) # load file
  # apply function
  out <- function(t)
    # write to file
    write.table(out, "path/to/output", sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)
})




files <- list.files(path="C:/Users/Lauren K/Desktop/Biocomptutorials/BioComp_Tutorial11/check")
lapply(files, function(x) {
  t <- read.table(x, header=TRUE) # load file
  # apply function
  out <- function(t)
    # write to file
    write.table(out, "path/to/output", sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)
})



