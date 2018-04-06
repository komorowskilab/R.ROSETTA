##################### CSV to ROS ###################
csvToRos <- function(dirList){

############## serching for CSV file ###############
  
file_name=gsub(paste0(dirList,"/"),"", list.files(dirList))
file_name=gsub(".csv","",file_name[grep('.csv',file_name)])
#storing name of found csv file 
  
#creating output folder
f_out=c()

f_out = c(f_out, genCmdFilesCSVtoROS(dirList,file_name))
f_out_dir= genCmdFilesCSVtoROS(dirList,file_name)
f_out=f_out_dir
f_out_dir=paste(file_name,"_toRosCmd.txt", sep="")
f_out_l=f_out

#setwd(system.file(package="rROSETTA"))

#file.copy(pathExe, dirList)

if(.Platform$OS.type=="unix")
{
pathExe <- paste(system.file(package="R.ROSETTA"), "exec/clrosetta.exe", sep="/")
comm=sprintf('wine %s SerialExecutor "FILENAME.COMMANDS=%s/%s; FILENAME.LOG=%s/log.txt"',pathExe,dirList,f_out_dir,dirList)
}
else{
  pathExe <- paste(gsub("/","\\",system.file(package="R.ROSETTA"),fixed=T), "exec","clrosetta.exe", sep="\\")
  comm=sprintf('%s SerialExecutor "FILENAME.COMMANDS=%s\\%s; FILENAME.LOG=%s\\log.txt"',pathExe,dirList,f_out_dir,dirList)
  }

 #try(system(command=comm, ignore.stdout = TRUE, show.output.on.console=FALSE), silent=TRUE)
 try(system2(command=comm), silent=TRUE)


}

