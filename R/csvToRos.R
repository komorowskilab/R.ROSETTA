##################### CSV to ROS ###################
csvToRos <- function(dirList){

############## serching for CSV file ###############
#if(.Platform$OS.type=="unix")
#{  
#file_name=gsub(paste0(dirList,"/"),"", list.files(dirList))}else{
#file_name=gsub(paste0(dirList,"\\"),"", list.files(dirList)) 
# }
file_name <- list.files(dirList)
file_name <- gsub(".csv","",file_name[grep('.csv',file_name)])
#storing name of found csv file 
  
#creating output folder
f_out_dir <- genCmdFilesCSVtoROS(dirList,file_name)

if(.Platform$OS.type=="unix")
{
pathExe <- paste(system.file(package="R.ROSETTA"), "exec/clrosetta.exe", sep="/")
comm <- sprintf('wine %s SerialExecutor "FILENAME.COMMANDS=%s/%s; FILENAME.LOG=%s/log.txt"',pathExe,dirList,f_out_dir,dirList)
try(system(command=comm, ignore.stdout = TRUE), silent=TRUE)
}
else{ ## win
  pathExe <- paste(gsub("/","\\",system.file(package="R.ROSETTA"),fixed=T), "exec","clrosetta.exe", sep="\\")
  comm <- paste0('cmd /K ','"','"', pathExe,'"',' SerialExecutor ','"','FILENAME.COMMANDS=',dirList,'\\',f_out_dir,'; FILENAME.LOG=',dirList,'\\log.txt','"','"')
  #try(system(command=comm, ignore.stdout = TRUE, intern=TRUE), silent=TRUE)
  cout <- capture.output(system(command=comm, ignore.stdout = TRUE))
}


}

