##################### CSV to ROS ###################
csvToRos <- function(dirList){

############## serching for CSV file ###############
#if(.Platform$OS.type=="unix")
#{  
#file_name=gsub(paste0(dirList,"/"),"", list.files(dirList))}else{
#file_name=gsub(paste0(dirList,"\\"),"", list.files(dirList)) 
# }
file_name=list.files(dirList)
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
try(system(command=comm, ignore.stdout = TRUE), silent=TRUE)
}
else{
  pathExe <- paste(gsub("/","\\",system.file(package="R.ROSETTA"),fixed=T), "exec","clrosetta.exe", sep="\\")
  #pathExe <- paste(system.file(package="R.ROSETTA"), "exec","clrosetta.exe", sep="/")
  comm=sprintf('cmd /C %s SerialExecutor "FILENAME.COMMANDS=%s\\%s; FILENAME.LOG=%s\\log.txt"',pathExe,dirList,f_out_dir,dirList)
#comm=paste0(pathExe," SerialExecutor")
#argms=paste0("FILENAME.COMMANDS=",dirList,"\\",f_out_dir,"; ", "FILENAME.LOG=",dirList,"\\log.txt")
  try(system(command=comm, ignore.stdout = TRUE, intern=TRUE), silent=TRUE)
}

 
#system(command=comm)

}

