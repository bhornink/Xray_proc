#Created by: Bruna Hornink
#Data: 004.03.2024
#Objective: To join all the files by species


#Clean workspace
rm(list = ls())

#1.Set Directory####

wd <- "C:/Documentos/PhD/Projeto/Abordagem 1.b/mensuração densidade"
setwd(wd)
dir()

#2.Packages required####
#install.packages("")
library(tidyverse)
library(lme4)

#3.Reading database####

#reading all the file with "_output" in the name inside of the Output file
dir_files <- file.path(wd,"Output")
list_files <- list.files(dir_files)
length(list_files)

#create an empty data frame
base_total <- data.frame()

#test the code
#name_data <- list.files(file.path(dir_files,list_files[1]), pattern = "\\.txt$")
# file_path <- file.path(dir_files,list_files[1],name_data[1])
# data_file <-  read.table(file_path, sep = ";", header = TRUE)
# data_file$species <- paste(sub("\\_output$", "", file))
# data_file$position <- substr(data_file$Cod, nchar(data_file$Cod) - 1, nchar(data_file$Cod)-1) 
# data_file$ray <- substr(data_file$Cod, nchar(data_file$Cod), nchar(data_file$Cod))
# data_file$cod_arv <- substr(data_file$Cod,1,nchar(data_file$Cod) - 2)

for (file in 1:length(list_files)) {
    
    #select a file and list all the file .txt inside of it  
    name_data <-  list.files(file.path(dir_files,list_files[file]), pattern = "\\.txt$") 
  
  #select the each database inside of the file
  for (data in 1:length(name_data)) { 
    
    # database path
    data_path <- file.path(dir_files,list_files[file],name_data[data])
    #read the database 
    data_file <-  read.table(data_path, sep = ";", header = TRUE) 
   
    #add specie name using the file name
    data_file$species <- paste(sub("\\_output$", "", list_files[file])) 
    
    #add height position (base, middle, top)
    data_file$position <- substr(data_file$Cod, nchar(data_file$Cod) - 1, nchar(data_file$Cod)-1) 
    
    #add the number of the ray 
    data_file$ray <- substr(data_file$Cod, nchar(data_file$Cod), nchar(data_file$Cod))
    
    data_file$cod_arv <- substr(data_file$Cod,1,nchar(data_file$Cod) - 2)#o código da árvore vai vir do código da amostr obtida no script medição_raiosX
    
    base_total <- rbind(base_total, data_file) #criei a base final juntando todas as bases na base vazia

  }
}

str(base_total)
unique(base_total$species)
length(unique(base_total$Cod)) 

#check if any position has na
base_total$Cod[which(is.na(base_total$position))]

#saving database
write.table(base_total,file.path(wd,"base_total.txt"), sep = ";")

