######################################################################
####################### R.ROSETTA main function ######################
######################################################################

rosetta <- function(dt,
                    classifier="StandardVoter",
                    cvNum=10,
                    discrete=FALSE,
                    discreteMethod="EqualFrequencyScaler",
                    discreteParam=3,
                    discreteMask=TRUE,
                    reducer="JohnsonReducer",
                    reducerDiscernibility="Object", #or Full
                    roc=FALSE,
                    clroc="autism",
                    fallBack=TRUE,
                    fallBackClass="autism",
                    maskFeatures=FALSE,
                    maskFeaturesNames=c(),
                    underSample=FALSE,
                    underSampleNum=0,
                    underSampleSize=0,
                    ruleFiltration=FALSE,
                    ruleFiltrSupport=c(1,3),
                    ruleFiltrAccuracy=c(0,0.5),
                    ruleFiltrCoverage=c(0,0),
                    ruleFiltrStability=c(0,0),
                    JohnsonParam=c(Modulo=TRUE,BRT=FALSE,BRTprec=0.9,Precompute=TRUE,Approximate=TRUE,Fraction=0.9),
                    GeneticParam=c(Modulo=TRUE,BRT=FALSE,BRTprec=0.9,Precompute=TRUE,Approximate=TRUE,Fraction=0.9,Algorithm="Simple"),
                    ManualNames=c(),
                    pAdjust=TRUE,
                    pAdjustMethod="BH",
                    seed=0,
                    invert=FALSE
                    )
{
  #df<-dt
  # setting paths, creating temp directory where the analysis will go
  firstPath<-tempdir()
  fname<-"data"
  
  if(discrete){
  discrete=FALSE}else
  {
  discrete=TRUE
  }

  if(.Platform$OS.type=="unix")
  {
  tempDirNam=paste(firstPath,paste0(format(Sys.time(), "%b_%d_%Y_%H%M%S"),"_RROS"),sep="/")
  dir.create(tempDirNam)
  dir.create(paste0(tempDirNam,"/data"))
  dir.create(paste0(tempDirNam,"/results"))}else
  {
  tempDirNam=paste(firstPath,paste0(format(Sys.time(), "%b_%d_%Y_%H%M%S"),"_RROS"),sep="\\")
  dir.create(tempDirNam)
  dir.create(paste0(tempDirNam,"\\data"))
  dir.create(paste0(tempDirNam,"\\results"))
  }


  
  # training pipline length
  if(discrete==TRUE)
  {
  pipeLen=5}else
  {
  pipeLen=4
  }
  

  ##############################
  ######## undersampling #######
  
  if(underSample==TRUE)
  {
   if(underSampleNum == 0)
   {
    #tab=df
    n=min(unname(table(as.character(dt[,length(dt)])))) #min
    k=max(unname(table(as.character(dt[,length(dt)])))) #max
    rep=1000
    out=c()
     for(j in 1:rep){
     vec=rep(0,k)
     i=0
      while(length(which(vec==0))>0){
       vec[sample(k,n)]<-1
       i=i+1
       }
     out[j]=i
     }
    underSampleNum=round(mean(unlist(out)))
   }
    
  # vector of the classes
  clvec=as.character(dt[,length(dt)])
  # number of the classes
  clnum=length(table(clvec))
  # names of the classes
  clnames=names(table(clvec))
  classL=list()
    
    # choose the number of objects in undersampled groups
   if(underSampleSize==0){
    minC=min(table(clvec))}else###if you want the minimum class
   {
    minC=underSampleSize ###if you want to choose the number
   }
    #choosing class
   for(i in 1:clnum){
   classL[[i]]=which(clvec%in%clnames[i]) #
   }
    
   for(j in 1:underSampleNum){
   samp=list()
    for(i in 1:clnum){
    samp[[i]]=sample(classL[[i]], minC)
    }
   df2=dt[c(1,unlist(samp)),]
   dfToCsv(df2, paste0(fname,"_",j), tempDirNam, disc=discrete)
   }
  
  ## without undersampling
  }else{
  dfToCsv(dt, fname, tempDirNam, disc=discrete)
  }
  
  ##############################
  
  
    if(.Platform$OS.type=="unix")
  {
  csvFileName <- list.files(path=paste0(tempDirNam,"/data"), pattern = "\\.csv$")}else{
  csvFileName <- list.files(path=paste0(tempDirNam,"\\data"), pattern = "\\.csv$")
  }
  
  #loop by files
 for(i in 1:length(csvFileName)){

  if(.Platform$OS.type=="unix")
  {
  dir.create(paste0(tempDirNam,"/results/",csvFileName[i]))
  dir.create(paste0(tempDirNam,"/results/",csvFileName[i],"/outPrep"))
  file.copy(paste0(tempDirNam,"/data/",csvFileName[i]), paste0(tempDirNam,"/results/",csvFileName[i],"/outPrep"))
  dirList=paste0(tempDirNam,"/results/",csvFileName[i],"/outPrep")
  }else{
  dir.create(paste0(tempDirNam,"\\results\\",csvFileName[i]))
  dir.create(paste0(tempDirNam,"\\results\\",csvFileName[i],"/outPrep"))
  file.copy(paste0(tempDirNam,"\\data\\",csvFileName[i]), paste0(tempDirNam,"\\results\\",csvFileName[i],"\\outPrep"))
  dirList=paste0(tempDirNam,"\\results\\",csvFileName[i],"\\outPrep")
  }
  
  
  
  
  ###### convert CSV to ROS ######
  csvToRos(dirList)
  # check ROS filename
  rosFileName <- list.files(path=dirList, pattern = "\\.ros$")
  
   if(.Platform$OS.type=="unix"){
   dirList2=paste0(tempDirNam,"/results/",csvFileName[i],"/outRosetta")
   pathExe <- paste(system.file(package="R.ROSETTA"), "exec/clrosetta.exe", sep="/")
   }else{
   dirList2=paste(tempDirNam,"results",csvFileName[i],"outRosetta", sep="\\")  
   pathExe <- paste(gsub("/","\\",system.file(package="R.ROSETTA"),fixed=T), "exec","clrosetta.exe", sep="\\")
   }
   
   dir.create(dirList2)
   
   ## masking the attributes
   maskAttribute(maskFeaturesNames, dirList2)
   if(.Platform$OS.type=="unix"){
   file.copy(paste(dirList,"/",rosFileName,sep=""), dirList2)
   }else{
   file.copy(paste(dirList,"\\",rosFileName,sep=""), dirList2)
   }
   
   
   IDGfnam="maIDG.txt"
   FoldNam="objects"
   
   ## to mask the features set parameter IDGfn to TRUE
   ## transmite parameters to generate command files to rosetta
   genCmdFilesRosetta(dir_file3=dirList2,
                      classifier=classifier,
                      discMethod=discreteMethod,
                      discParam=discreteParam,
                      discMask=discreteMask,
                      IDGlog=maskFeatures,
                      IDGfn=IDGfnam,
                      maskFeaturesNames=maskFeaturesNames,
                      ruleMeth=reducer,
                      proNam=FoldNam,
                      LogVerb=T,
                      disc=discrete,
                      fallBack=fallBack,
                      fallBackClass=fallBackClass,
                      ruleFiltration=ruleFiltration,
                      ruleFiltrSupport=ruleFiltrSupport,
                      ruleFiltrAccuracy=ruleFiltrAccuracy,
                      ruleFiltrCoverage=ruleFiltrCoverage,
                      ruleFiltrStability=ruleFiltrStability,
                      reducerDiscernibility=reducerDiscernibility,
                      JohnsonParam=JohnsonParam,
                      GeneticParam=GeneticParam,
                      ManualNames=ManualNames,
                      roc=roc,
                      clroc=clroc,)
    
  #seed=0
  # check the platform. For unix you have to have wine installed!
  if(.Platform$OS.type=="unix")
  {
  comm=sprintf('wine %s CVSerialExecutor "INVERT = %s; NUMBER = %i; SEED = %i; LENGTH = %i; FILENAME.COMMANDS = %s; FILENAME.LOG = %s" %s',
                pathExe,
                substr(as.character(invert),1,1),
                cvNum,
                seed,
                pipeLen,
                paste0(dirList2,"/","OUT_cmdCV.txt"),
                paste0(dirList2,"/","logMain.txt"),
                paste0(dirList2,"/",rosFileName)
               )
  }else{
  comm=sprintf('%s CVSerialExecutor "INVERT = %s; NUMBER = %i; SEED = %i; LENGTH = %i; FILENAME.COMMANDS = %s; FILENAME.LOG = %s" %s',
                pathExe,
                substr(as.character(invert),1,1),
                cvNum,
                seed,
                pipeLen,
                paste(dirList2,"OUT_cmdCV.txt",sep="\\"),
                paste(dirList2,"logMain.txt",sep="\\"),
                paste(dirList2,rosFileName,sep="\\")
               )
  }
  try(system(command=comm, ignore.stdout = TRUE, intern=TRUE), silent=TRUE) # supress warnings and comunicates
 }
  
  # prepare all results
  if(.Platform$OS.type=="unix")
  {
  LFout=list.files(paste0(tempDirNam,"/results"))}else{
  LFout=list.files(paste0(tempDirNam,"\\results"))
  }
  dfRes_rocAucSE=c()
  dfRes_rocAuc=c()
  dfRes_accMean=c()
  dfRes_accMedian=c()
  dfRes_accStdDev=c()
  dfRes_accMin=c()
  dfRes_accMax=c()
  dfRes_rocMean=c()
  dfRes_rocMedian=c()
  dfRes_rocStdDev=c()
  dfRes_rocMin=c()
  dfRes_rocMax=c()
  dfRes_rocseMean=c()
  dfRes_rocseMedian=c()
  dfRes_rocseStdDev=c()
  dfRes_rocseMin=c()
  dfRes_rocseMax=c()
  dfRes_mccMean=c()
  
  # statistic
  if(roc){
   for(i in 1:length(LFout)){
     if(.Platform$OS.type=="unix")
   {
   path=paste0(tempDirNam,"/results","/",LFout[i],"/outRosetta")}else{
   path=paste0(tempDirNam,"\\results","\\",LFout[i],"\\outRosetta")
   }
   
   # ROC AUC
   dfRes_rocAuc[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[1])))
   dfRes_rocAucSE[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[2])))
   # ACCURACY
   dfRes_accMean[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[3])))
   dfRes_accMedian[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[4])))
   dfRes_accStdDev[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[5])))
   dfRes_accMin[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[6])))
   dfRes_accMax[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[7])))
   # ROC
   dfRes_rocMean[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[8])))
   dfRes_rocMedian[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[9])))
   dfRes_rocStdDev[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[10])))
   dfRes_rocMin[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[11])))
   dfRes_rocMax[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[12])))
   # ROC SE
   dfRes_rocseMean[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[13])))
   dfRes_rocseMedian[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[14])))
   dfRes_rocseStdDev[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[15])))
   dfRes_rocseMin[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[16])))
   dfRes_rocseMax[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[17])))
   }
   
   outRos=data.frame(mean(dfRes_accMean),mean(dfRes_accMedian),mean(dfRes_accStdDev),mean(dfRes_accMin),
                     mean(dfRes_accMax),mean(dfRes_rocAuc),mean(dfRes_rocAucSE),mean(dfRes_rocMean), 
                     mean(dfRes_rocMedian),mean(dfRes_rocStdDev), mean(dfRes_rocMin), mean(dfRes_rocMax),
                     mean(dfRes_rocseMean), mean(dfRes_rocseMedian),mean(dfRes_rocseStdDev), mean(dfRes_rocseMin),
                     mean(dfRes_rocseMax))
   
   colnames(outRos)<-c("Accuracy.Mean","Accuracy.Median","Accuracy.Std","Accuracy.Min","Accuracy.Max",
                        "ROC.AUC","ROC.AUC.SE","ROC.AUC.MEAN","ROC.AUC.MEDIAN","ROC.AUC.STDEV","ROC.AUC.MIN","ROC.AUC.MAX",
                        "ROC.AUC.SE.MEAN","ROC.AUC.SE.MEDIAN","ROC.AUC.SE.STDEV","ROC.AUC.SE.MIN","ROC.AUC.SE.MAX")
   rownames(outRos)<-""
    
  }else{ # just accuracy values
   for(i in 1:length(LFout)){
   
   if(.Platform$OS.type=="unix")
   {
   path=paste0(tempDirNam,"/results","/",LFout[i],"/outRosetta")}else{
   path=paste0(tempDirNam,"\\results","\\",LFout[i],"\\outRosetta")
   }
   # ACCURACY
   dfRes_accMean[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[1])))
   dfRes_accMedian[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[2])))
   dfRes_accStdDev[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[3])))
   dfRes_accMin[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[4])))
   dfRes_accMax[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[5])))
   dfRes_mccMean[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[6])))
   }
      
   outRos=data.frame(mean(dfRes_accMean),mean(dfRes_accMedian),mean(dfRes_accStdDev),mean(dfRes_accMin),mean(dfRes_accMax), mean(dfRes_mccMean))
   colnames(outRos)<-c("Accuracy.Mean","Accuracy.Median","Accuracy.Std","Accuracy.Min","Accuracy.Max","MCC.mean")
   rownames(outRos)<-""
  }

  # make mean accuracy for CV and undersampled files
  dataset_merged=data.frame()
  for(i in 1:length(LFout)){
     if(.Platform$OS.type=="unix")
   {
  path2=paste0(tempDirNam,"/results/",LFout[i],"/outRosetta/rules")
  file_list <- paste0(path2,"/",list.files(path=path2))}else{
  path2=paste0(tempDirNam,"\\results\\",LFout[i],"\\outRosetta\\rules")
  file_list <- paste0(path2,"\\",list.files(path=path2))
  }
    
   for(file in file_list){
   temp_dataset <-read.table(file, header=TRUE, sep="\t")
   dataset_merged<-rbind(dataset_merged, temp_dataset)
   rm(temp_dataset)}
  }
  
  colnames(dataset_merged)<-"rules"
  rules2=dataset_merged
  
  # filter out comments
  rl=rules2
  rl2=as.matrix(rl[!grepl("%", rl, fixed = T)]) #deleting comments
  rl_r=which(grepl("=>", rl2, fixed = T)) #choosing rules
  rules=rl2[rl_r]
  
  # choose proper lines
  supp_lhs=rl2[which(grepl("Supp. (LHS) =", rl2, fixed = T))]
  supp_rhs=rl2[which(grepl("Supp. (RHS) =", rl2, fixed = T))]
  acc_rhs=rl2[which(grepl("Acc.  (RHS) =", rl2, fixed = T))]
  cov_lhs=rl2[which(grepl("Cov.  (LHS) =", rl2, fixed = T))]
  cov_rhs=rl2[which(grepl("Cov.  (RHS) =", rl2, fixed = T))]
  stab_lhs=rl2[which(grepl("Stab. (LHS) =", rl2, fixed = T))]
  stab_rhs=rl2[which(grepl("Stab. (RHS) =", rl2, fixed = T))]
  
  # choose only values from lists
  supp_lhs2=gsub(" object(s)","",unlist(regmatches(supp_lhs, gregexpr("\\[\\K[^\\]]+(?=\\])", supp_lhs, perl=TRUE))), fixed = T)
  supp_rhs2=gsub(" object(s)","",unlist(regmatches(supp_rhs, gregexpr("\\[\\K[^\\]]+(?=\\])", supp_rhs, perl=TRUE))), fixed = T)
  acc_rhs2=unlist(regmatches(acc_rhs, gregexpr("\\[\\K[^\\]]+(?=\\])", acc_rhs, perl=TRUE)))
  cov_lhs2=unlist(regmatches(cov_lhs, gregexpr("\\[\\K[^\\]]+(?=\\])", cov_lhs, perl=TRUE)))
  cov_rhs2=unlist(regmatches(cov_rhs, gregexpr("\\[\\K[^\\]]+(?=\\])", cov_rhs, perl=TRUE)))
  stab_lhs2=unlist(regmatches(stab_lhs, gregexpr("\\[\\K[^\\]]+(?=\\])", stab_lhs, perl=TRUE)))
  stab_rhs2=unlist(regmatches(stab_rhs, gregexpr("\\[\\K[^\\]]+(?=\\])", stab_rhs, perl=TRUE)))
  
  ## convert everything to numeric values 
  ## and choosing max if there is two option
  
  # SUPPORT LHS
  supp_lhs3=unlist(lapply(lapply(strsplit(supp_lhs2, ","),as.numeric),max))
  # SUPPORT RHS
  supp_rhs3=unlist(lapply(lapply(strsplit(supp_rhs2, ","),as.numeric),max))
  supp_rhs3n=unlist(lapply(lapply(strsplit(supp_rhs2, ","),as.numeric),which.max))
  # ACCURACY RHS
  acc_rhs3=unlist(lapply(lapply(strsplit(as.character(acc_rhs2), ","),as.double),max))
  acc_rhs3n=unlist(lapply(lapply(strsplit(acc_rhs2, ","),as.numeric),which.max))
  
  if(is.null(acc_rhs3n)){
      stop("Rules produced only for one class. No right-hand values found.")
  }else{
  # COVERAGE RHS
  cov_rhs3=unlist(lapply(lapply(strsplit(as.character(cov_rhs2), ","),as.double),max))
  # COVERAGE LHS
  cov_lhs3=as.double(cov_lhs2)
  # STABL LHS
  stab_lhs3=unlist(lapply(lapply(strsplit(as.character(stab_lhs2), ","),as.double),max))
  # STAB RHS
  stab_rhs3=unlist(lapply(lapply(strsplit(as.character(stab_rhs2), ","),as.double),max))
  # RULES
  rules2=unlist(lapply(strsplit(as.character(rules), " =>", fixed=TRUE), `[`, 1))
  dec_class=strsplit(as.character(unlist(lapply(strsplit(as.character(rules), " => ", fixed=TRUE), `[`, 2))), " OR ", fixed=TRUE)
  
  # choosing element according to accuracy
   choose_nfl=rep(NA,length(acc_rhs3n))
   
  for(i in 1:max(as.numeric(acc_rhs3n))){
  choose_nfl[which(acc_rhs3n==i)]=unlist(lapply(dec_class, '[', i))[which(acc_rhs3n==i)]
  }
   
  rl2=strsplit(rules2," AND ")
  lst=lapply(lapply(rl2, function(x) strsplit(x, "\\(")), unlist)
  
  ### each element separately ###
  lst_feat=lapply(lapply(lst, function(x) x[seq(1,length(x),2)]), unlist)
  features2=unlist(lapply(lapply(lst_feat, function(x) paste(x, collapse = ",")), unlist))
  
  ### each element separately ###
  if(discrete==T){
  # for non discrete data  
  lst_cuts=lapply(lapply(lst, function(x) x[-seq(1,length(x),2)]), unlist)
  
  lstc=lapply(lst_cuts,function(x) gsub("[[() *]", "", x))
  lstc2=lapply(lstc,function(x) (gsub(',$','', x)))
  lstc3=lapply(lstc2,function(x) (gsub('^,','', x)))
  lstc4=lapply(lstc3, function(x) unlist(strsplit(x, ",")))
  
  ## catch only numeric values
  catchNumeric <- function(mylist){
  newlist <- suppressWarnings(as.numeric(mylist))
  mylist <- list(mylist)
  mylist[!is.na(newlist)] <- newlist[!is.na(newlist)]
  unlist(mylist)}
  
  lst_cuts2=lapply(lstc4, catchNumeric)
  #lst_cuts2=lapply(lapply(lst_cuts, function(x) as.numeric(unlist(regmatches(x,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",x))))), unlist)
  #lst_cuts2[which(lapply(lst_cuts2,length)==0)]<-lapply(lst_cuts[which(lapply(lst_cuts2,length)==0)], function(x) gsub(")", "",x,fixed = T))
  
  lst_cuts22=unlist(lapply(lapply(lst_cuts2, function(x) paste(x, collapse = ",")), unlist))
  
  # remove brackets and change them into conditions
  lst1=lapply(lst_cuts, function(x) gsub(".*\\*\\).*", "value>cut", x))
  lst2=lapply(lst1, function(x) gsub(".*\\[\\*.*", "value<cut", x))
  lst3=lapply(lst2, function(x) gsub(".*\\[.*\\).*", "cut<value<cut", x))
  lst4=lapply(lst3, function(x) gsub(".*).*","discrete",x))
  
  cuts2=unlist(lapply(lapply(lst4, function(x) paste(x, collapse = ",")), unlist))
  
  # calculate the sizes
  df222=data.frame(word = do.call(c, lst_cuts2),
                   group = rep(1:length(lst_cuts2), 
                   sapply(lst_cuts2, length)))
  
  # create constant size data frame for cuts
  lst_cuts3=lapply(lst_cuts2, 'length<-', max(table(df222$group)))
  df3=t(as.data.frame(lst_cuts3, stringsAsFactors=FALSE))
  
### retrieve discretization states ###
dataset_cuts=data.frame()
dataset_rules=data.frame()

st3=list()
#loop on undersampling files
for(l in 1:length(LFout)){

     if(.Platform$OS.type=="unix")
   {
  path_rules=paste0(tempDirNam,"/results/",LFout[l],"/outRosetta/rules")
  path_cuts=paste0(tempDirNam,"/results/",LFout[l],"/outRosetta/cuts")
  files_rules <- paste0(path_rules,"/",list.files(path=path_rules))
  files_cuts <- paste0(path_cuts,"/",list.files(path=path_cuts))}else{
  
   path_rules=paste0(tempDirNam,"\\results\\",LFout[l],"\\outRosetta\\rules")
  path_cuts=paste0(tempDirNam,"\\results\\",LFout[l],"\\outRosetta\\cuts")
  files_rules <- paste0(path_rules,"\\",list.files(path=path_rules))
  files_cuts <- paste0(path_cuts,"\\",list.files(path=path_cuts))
  }

  #loop on CV files
  for(k in 1:length(files_rules)){
    temp_dataset <-read.table(files_rules[k], header=FALSE, sep="\t")
    colnames(temp_dataset)<-"rules"
    dataset_rules<-temp_dataset
    rm(temp_dataset)
    
    temp_dataset <-read.table(files_cuts[k], header=FALSE, sep="\t")
    dataset_cuts<-temp_dataset
    rm(temp_dataset)
    
    key <- 0:(dim(dt)[2]-2)
    val <- colnames(dt)[-length(colnames(dt))]
    out<-lapply(1:(dim(dt)[2]-1),FUN = function(i){dataset_cuts[dataset_cuts == key[i],1] <<- val[i]})
    
    # filter out comments
    rl=dataset_rules
    rl2=as.matrix(rl[!grepl("%", rl, fixed = T)]) #deleting comments
    rl_r=which(grepl("=>", rl2, fixed = T)) #choosing rules
    rules=rl2[rl_r]
    
    rules2=unlist(lapply(strsplit(as.character(rules), " =>", fixed=TRUE), `[`, 1))
    rl2=strsplit(rules2," AND ")
    lst=lapply(lapply(rl2, function(x) strsplit(x, "\\(")), unlist)
    
    lst_cuts=lapply(lapply(lst, function(x) x[-seq(1,length(x),2)]), unlist)
    lstc=lapply(lst_cuts,function(x) gsub("[[() *]", "", x))
    lstc2=lapply(lstc,function(x) (gsub(',$','', x)))
    lstc3=lapply(lstc2,function(x) (gsub('^,','', x)))
    lstc4=lapply(lstc3, function(x) unlist(strsplit(x, ",")))
    
    ## catch only numeric values
    catchNumeric <- function(mylist){
      newlist <- suppressWarnings(as.numeric(mylist))
      mylist <- list(mylist)
      mylist[!is.na(newlist)] <- newlist[!is.na(newlist)]
      unlist(mylist)}
    lst_cuts2=lapply(lstc4, catchNumeric)
    
    ## list of the cuts - lstc3
    ## library - dataset_cuts
    ## list of the features
    lst_feat=lapply(lapply(lst, function(x) x[seq(1,length(x),2)]), unlist)
    ##features2=unlist(lapply(lapply(lst_feat, function(x) paste(x, collapse = ",")), unlist))
    
 #dataset_cuts[,2]<-dataset_cuts[,2]/1e+06
        
        round2 = function(x, n) {
          posneg = sign(x)
          z = abs(x)*10^n
          z = z + 0.5
          z = trunc(z)
          z = z/10^n
          z*posneg
        }
        
        epsil=0.0001
        ##create states
        st2=lapply(1:length(lst_feat),FUN = function(j){
          st=c()
          for(i in 1:length(lstc3[[j]]))
          {
            tempCuts<-dataset_cuts[which(dataset_cuts[,1] %in% lst_feat[[j]][i]),]
            rownames(tempCuts)<-NULL
            
            ##check if the feature is character
            if(dim(tempCuts)[1]==0){
              st[i]<-lstc3[[j]][i]
            }
            else{
            if(as.numeric(unlist(strsplit(lstc3[[j]][i], ",")))[1]%%1==0){ #integers
              if(grepl(",",lstc3[[j]][i])) ##for ranges -> middle classes
              {
                st[i]<-which(round2(tempCuts$V2,0)==min(as.numeric(unlist(strsplit(lstc3[[j]][i], ",")))))+1 
              }else{ ##for single -> extremes
                #left
                if(which(round2(tempCuts$V2,0)==as.numeric(unlist(lstc3[[j]][i])))==1){
                  st[i]<-1
                }else #right
                {
                  st[i]<-which(round2(tempCuts$V2,0)==as.numeric(unlist(lstc3[[j]][i])))+1
                }
              }
              
                
            }else{
              
            tempCuts$V2<-tempCuts$V2/1e+06
            
            if(grepl(",",lstc3[[j]][i])) ##for ranges -> middle classes
            {
              st[i]<-which(abs(tempCuts$V2-min(as.numeric(unlist(strsplit(lstc3[[j]][i], ","))))) < epsil & abs(tempCuts$V2-min(as.numeric(unlist(strsplit(lstc3[[j]][i], ","))))) >= 0)+1 
            }else{ ##for single -> extremes
              #left
              if(which(abs(tempCuts$V2-as.numeric(lstc3[[j]][i])) < epsil & abs(tempCuts$V2-as.numeric(lstc3[[j]][i])) >= 0)==1){
                st[i]<-1
              }else #right
              {
                st[i]<-which(abs(tempCuts$V2-as.numeric(lstc3[[j]][i])) < epsil & abs(tempCuts$V2-as.numeric(lstc3[[j]][i])) >= 0)+1
              }
                }
                }#else from integer
               }#else from character
             }#end for
          return(st)
          }) #end of function
    
    st3[[k]]=unlist(lapply(lapply(st2, function(x) paste(x, collapse = ",")), unlist))
  }
}
                        
  #######################################                           
                             
  decsFinal= unlist(lapply(as.character(choose_nfl), FUN=function(x) (regmatches(x, gregexpr("(?<=\\().*?(?=\\))", x, perl=T))[[1]])))
  df_out=data.frame(features2,decsFinal,unlist(st3), supp_lhs3, supp_rhs3, acc_rhs3, cov_lhs3, cov_rhs3, stab_lhs3, stab_rhs3, cuts2, df3)
  colnames(df_out)<-c("FEATURES","DECISION","DISC_CLASSES","SUPP_LHS","SUPP_RHS","ACC_RHS","COV_LHS","COV_RHS","STAB_LHS","STAB_RHS","CUTS_COND",paste0("CUT_",seq(1:max(table(df222$group)))))
  
  df_outU=unique(df_out[c("FEATURES", "DECISION", "CUTS_COND","DISC_CLASSES")])
  allMat=do.call(paste0, df_out[c("FEATURES", "DECISION", "CUTS_COND","DISC_CLASSES")])
  subMat=as.matrix(do.call(paste0, df_outU))
  
   aggregate2 <- function(x){ 
   df_out3=df_out[which(match(allMat, x) == 1),]
    
    meanOrCharacter <- function(x){
     if(class(x) == "factor"){
     # check if in factor columns are characters or the numbers
      if(is.na(as.numeric(as.character(unname(unique(x))), options(warn=-1)))[1]){
          return(as.character(x)[1]) 
      }else{
      return(round(mean(as.numeric(as.character(x))),  digits = 5))}
     }
      if(class(x) == "numeric"){
        out <- round(mean(as.numeric(x), na.rm = TRUE), digits = 5)
        return(out)}
    }
    
   indx <- sapply(df_out3, is.factor)
   df_out3[indx] <- lapply(df_out3[indx], function(x) as.character(x))
   df_out4=aggregate(.~FEATURES+DECISION+CUTS_COND+DISC_CLASSES, FUN=meanOrCharacter, data = df_out3, na.action = na.pass)
   return(df_out4)
   }
  
  df_out5=apply(subMat, 1, aggregate2)
  df_out2=do.call("rbind", df_out5)
  df_out2$SUPP_LHS<-round(df_out2$SUPP_LHS)
  df_out2$SUPP_RHS<-round(df_out2$SUPP_RHS)
  #df_out2=aggregate(.~FEATURES+DECISION+CUT_COND, mean, data = df_out, na.action = na.pass)
  }
  else{
  #for discrete data
  lst_cuts=lapply(lapply(lst, function(x) x[-seq(1,length(x),2)]), unlist)
  #lst_cuts2=lapply(lapply(lst_cuts, function(x) as.numeric(unlist(regmatches(x,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",x))))), unlist)
  lst_cuts2=lapply(lapply(lst_cuts, function(x) gsub(")","",x)), unlist)
  lst_cuts22=unlist(lapply(lapply(lst_cuts2, function(x) paste(x, collapse = ",")), unlist))
    
  df222=data.frame(word = do.call("c", lst_cuts2),
                   group = rep(1:length(lst_cuts2), 
                   sapply(lst_cuts2, length)))
    
  lst_cuts3=lapply(lst_cuts2, 'length<-', max(table(df222$group)))
  df3=t(as.data.frame(lst_cuts3))
  decsFinal= unlist(lapply(as.character(choose_nfl), FUN=function(x) (regmatches(x, gregexpr("(?<=\\().*?(?=\\))", x, perl=T))[[1]])))
  df_out=data.frame(features2,decsFinal, supp_lhs3, supp_rhs3, acc_rhs3, cov_lhs3, cov_rhs3, stab_lhs3, stab_rhs3, df3, lst_cuts22)
  colnames(df_out)<-c("FEATURES","DECISION","SUPP_LHS","SUPP_RHS","ACC_RHS","COV_LHS","COV_RHS","STAB_LHS","STAB_RHS",paste0("CUT_",seq(1:max(table(df222$group)))),"CUTS_COND")
  #df_out2=aggregate(.~FEATURES+DECISION+CUT_COND, mean, data = df_out, na.action = na.pass)
  df_outU=unique(df_out[c("FEATURES", "DECISION", "CUTS_COND")])
  allMat=do.call(paste0, df_out[c("FEATURES", "DECISION", "CUTS_COND")])
  subMat=as.matrix(do.call(paste0, df_outU))
  
   aggregate2 <- function(x){ 
   df_out3=df_out[which(match(allMat, x) == 1),]
   meanOrCharacter <- function(x){
     if(class(x) == "factor"){
       # check if in factor columns are characters or the numbers
       if(is.na(as.numeric(as.character(unname(unique(x))), options(warn=-1)))[1]){
         return(as.character(x)[1]) 
       }else{
         return(round(mean(as.numeric(as.character(x))),  digits = 4))}
     }
     if(class(x) == "numeric"){
       out <- round(mean(as.numeric(x), na.rm = TRUE), digits = 4)
       return(out)}
   }
      
  indx <- sapply(df_out3, is.factor)
  df_out3[indx] <- lapply(df_out3[indx], function(x) as.character(x))
  df_out4=aggregate(.~FEATURES+DECISION+CUTS_COND, FUN=meanOrCharacter, data = df_out3, na.action = na.pass)
  return(df_out4)
  }
    
  df_out5=apply(subMat, 1, aggregate2)
  df_out2=do.call("rbind", df_out5)
  df_out2$SUPP_LHS<-round(df_out2$SUPP_LHS)
  df_out2$SUPP_RHS<-round(df_out2$SUPP_RHS)
  }
  
  #df_out2[paste0("CUTS_",seq(1:max(table(df222$group))))]<-round(df_out2[paste0("CUTS_",seq(1:max(table(df222$group))))])
  #for(i in 1:length(levels(df_out$DECISION))){
  #  df_out2$DECISION[which(df_out2$DECISION==i)]<-levels(df_out$DECISION)[i]
  #}
  
  ## p-value for rules calculation ##
  PVAL <- c()
  RISK_PVAL <- c()
  CONF_INT <- c()
  REL_RISK <- c()
                   
   for(i in 1:length(df_out2$SUPP_RHS)){
   k <- round(df_out2$SUPP_RHS[i]*df_out2$ACC_RHS[i]) #total support adjusted by accuracy
   R1 <- unname(table(dt[,length(dt)])[names(table(dt[,length(dt)]))==as.character(df_out2$DECISION[i])])
   N <- dim(dt)[1] 
   R2 <- N-R1
   # the number of decisions/objects/patients
   C1 <- df_out2$SUPP_LHS[i]    # LHS Support
   #C2=N-C1                  # total drawn
   #R1=dim(dt)[2]            # total hits, number of features
   #R2=N-R1                  # number of features - number of decisions
   PVAL[i] <- phyper(k-1, m=R1, n=R2, C1, lower.tail = FALSE)  # calculate pvalue from phypergeometric
   
   # risk ratio
   invisible(capture.output(rr<-riskratio(k, C1, R1, N, conf.level=0.95)))
     
   CONF_INT[i] <- paste(as.character(round(rr$conf.int[1:2], digits=3)), collapse =":")
   RISK_PVAL[i] <- rr$p.value
   REL_RISK[i] <- rr$estimate
   }
  
   if(pAdjust){
   PVAL=p.adjust(PVAL, method=pAdjustMethod)}
  
  df_out3=data.frame(df_out2,PVAL, RISK_PVAL, REL_RISK, CONF_INT)
  df_out4=df_out3[order(df_out3$PVAL,decreasing = F),]
  rownames(df_out4) <- NULL
  # clear all the created files and set the first driectory
  unlink(tempDirNam, recursive = TRUE)
  
  #output 
  return(list(main=df_out4, quality=outRos, rules=rules2, usn=underSampleNum)) 
  }
  
} #last parenthesis of function
