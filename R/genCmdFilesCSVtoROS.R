genCmdFilesCSVtoROS <- function (dir_file, file_name){
  
  fn <- paste(file_name,"_toRosCmd.txt", sep="")
  if (file.exists(fn)) unlink(file.path(dir_file, fn), recursive=TRUE)
  
  #dir.create(dir_file)
  lista_out=NULL
  if(.Platform$OS.type == "unix")
  {
  lista_out[1] <- "StructureCreator"
  lista_out[2] <- "{OUTPUT = DecisionTable}"
  lista_out[3] <- "MyDecisionTableImporter"
  lista_out[4] <- paste("{FILENAME=",dir_file,"/",file_name,".txt}", sep="")
  lista_out[5] <- "Saver"
  lista_out[6] <- paste("{FILENAME=",dir_file,"/",file_name, ".ros}", sep="")
  write.table(lista_out,file=paste(dir_file,"/",fn, sep=""), quote=F, col.names = F, row.names = F)
  }else{
  lista_out[1] <- "StructureCreator"
  lista_out[2] <- "{OUTPUT = DecisionTable}"
  lista_out[3] <- "MyDecisionTableImporter"
  lista_out[4] <- paste("{FILENAME=",dir_file,"\\",file_name,".txt}", sep="")
  lista_out[5] <- "Saver"
  lista_out[6] <- paste("{FILENAME=",dir_file,"\\",file_name, ".ros}", sep="")
  write.table(lista_out,file=paste(dir_file,"\\",fn, sep=""), quote=F, col.names = F, row.names = F)
  }
  return(fn)
}
