#Creates function for finding the coefficient of variation of all files
#in a specified directory. Contains the following arguments:
#dir="specify path to directory in which files are located"
#col=#; identify the column number of data
#narm=TRUE/FALSE; TRUE to remove NAs in data, FALSE to keeps NAs in original data for calculation
#req=TRUE/FALSE; TRUE sets a standard of 50 observations to calculate coeff of var
#if 50 obs are not present, calculation is completed with a warning
#FALSE simply calculates coeff of var for any number of observations 

coeffvar<- function(dir, col=1, narm=TRUE, req=TRUE){
  files<-list.files(path=dir)
  files<-files[grepl(".csv", files, fixed = TRUE)]
  results<-numeric(length(files))
  #for loop, loop through files and calculate coeff of var
  for (i in 1:length(files)){
    read<-read.csv(files[i], header=TRUE, stringsAsFactors = FALSE)
    #if else statement; users can override required obs warning by specifying req=FALSE
    if(req==TRUE){
      if(sum(! is.na(read[,col]))>=50){
        results[i]<-sd(read[,col], na.rm=narm)/mean(read[,col], na.rm=narm)
      }else {
        print(paste(files[i],"has less than 50 observations."))
        results[i]<-sd(read[,col], na.rm=narm)/mean(read[,col], na.rm=narm)
      }
    }else{
      results[i]<-sd(read[,col], na.rm=narm)/mean(read[,col], na.rm=narm)
    }
  }
  return(results)
}

#Example function use
coeffvar(dir="C:/Users/Lauren K/Desktop/Biocomptutorials/BioComp_Tutorial11/check", col=2, narm=TRUE, req=FALSE)

