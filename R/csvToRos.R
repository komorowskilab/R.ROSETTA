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
pathExe <- paste(system.file(package="RROSETTA"), "exec/clrosetta.exe", sep="/")
file.copy(pathExe, dirList)

if(.Platform$OS.type=="unix")
{
comm=sprintf('wine %s SerialExecutor "FILENAME.COMMANDS=%s/%s; FILENAME.LOG=%s/log.txt"',pathExe,dirList,f_out_dir,dirList)
}
else{
  comm=sprintf('%s SerialExecutor "FILENAME.COMMANDS=%s/%s; FILENAME.LOG=%s/log.txt"',pathExe,dirList,f_out_dir,dirList)
  }
system(command=comm, show.output.on.console = FALSE)


}

