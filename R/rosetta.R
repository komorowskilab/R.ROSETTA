#rROSETTA package
rosetta <- function(df,
                    classifier="StandardVoter",
                    cvNum=10,
                    discrete=TRUE,
                    discreteMethod="EqualFrequencyScaler",
                    discreteParam=3,
                    discreteMask=TRUE,
                    reducer="JohnsonReducer",
                    reducerDiscernibility="Object", #or Full
                    roc=TRUE,
                    clroc="autism",
                    fallBack=TRUE,
                    fallBackClass="autism",
                    maskFeatures=FALSE,
                    maskFeaturesNames=c(),
                    underSample=FALSE,
                    underSampleNum=0,
                    underSampleSize=0,
                    ruleFiltration=TRUE,
                    ruleFiltrSupport=c(1,3),
                    ruleFiltrAccuracy=c(0,0.5),
                    ruleFiltrCoverage=c(0,0),
                    ruleFiltrStability=c(0,0),
                    JohnsonParam=c(Modulo=TRUE,BRT=FALSE,BRTprec=0.9,Precompute=FALSE,Approximate=TRUE,Fraction=0.95),
                    GeneticParam=c(Modulo=TRUE,BRT=FALSE,BRTprec=0.9,Precompute=FALSE,Approximate=TRUE,Fraction=0.95,Algorithm="Simple"),
                    ManualNames=c(),
                    pAdjust=TRUE,
                    pAdjustMethod="BH"
                    )
  {
  # setting paths, creating temp directory where the analysis will go
  firstPath=tempdir()
  fname="data"
  #setwd(firstPath)

      if(.Platform$OS.type=="unix")
    {
    tempDirNam=paste(firstPath,paste0(format(Sys.time(), "%b_%d_%Y_%H%M%S"),"_RROS"),sep="/")
    }else
    {
    tempDirNam=paste(firstPath,paste0(format(Sys.time(), "%b_%d_%Y_%H%M%S"),"_RROS"),sep="\\")
    }

  dir.create(tempDirNam)
  #setwd(tempDirNam)
  dir.create(paste0(tempDirNam,"/data"))
  dir.create(paste0(tempDirNam,"/results"))
  #setwd("data")
  
  # training pipline length
  if(discrete==TRUE)
  {
    pipeLen=5
    
  }else
  {
     pipeLen=4
 }
  

  ##############################
  ######## undersampling #######
  
  if(underSample==TRUE){
    if(underSampleNum == 0)
    {
      tab=df
      n=min(unname(table(as.character(tab[,length(tab)])))) #min
      k=max(unname(table(as.character(tab[,length(tab)])))) #max
      rep=1000
      out=c()
      for(j in 1:rep){
        vec=rep(0,k)
        i=0
        while(length(which(vec==0))>0)
        {
          vec[sample(k,n)]<-1
          i=i+1
        }
        out[j]=i
      }
      underSampleNum=round(mean(unlist(out)))
    }
    # vector of the classes
    clvec=as.character(df[,length(df)])
    # number of the classes
    clnum=length(table(clvec))
    # names of the classes
    clnames=names(table(clvec))
    classL=list()
    
    # choose the number of objects in undersampled groups
    if(underSampleSize==0) 
    {
      minC=min(table(clvec))###if you want the minimum class
    }else
    {
      minC=underSampleSize ###if you want to choose the number
    }
    
    #choosing class
    for(i in 1:clnum)
    {
      classL[[i]]=which(clvec%in%clnames[i]) #
    }
    
    for(j in 1:underSampleNum){
      
      samp=list()
      for(i in 1:clnum)
      {
        samp[[i]]=sample(classL[[i]], minC)
      }
      
      df2=df[c(1,unlist(samp)),]
      dfToCsv(df2, paste0(fname,"_",j), tempDirNam, disc=discrete)
    }
    
  } else ##without undersampling
  {
    dfToCsv(df, fname, tempDirNam, disc=discrete)
  }
  ##############################
  
  csvFileName <- list.files(path=paste0(tempDirNam,"/data"), pattern = "\\.csv$")
  for(i in 1:length(csvFileName)) #loop by files
  {
    dir.create(paste0(tempDirNam,"/results/",csvFileName[i]))
    dir.create(paste0(tempDirNam,"/results/",csvFileName[i],"/outPrep"))
    file.copy(paste0(tempDirNam,"/data/",csvFileName[i]), paste0(tempDirNam,"/results/",csvFileName[i],"/outPrep"))
    dirList=paste0(tempDirNam,"/results/",csvFileName[i],"/outPrep")
    
    ###### convert CSV to ROS ######
    csvToRos(dirList)
    
    # checking filename of ROS file
    rosFileName <- list.files(path=dirList, pattern = "\\.ros$")
    
    if(.Platform$OS.type=="unix")
    {
    dirList2=paste0(tempDirNam,"/results/",csvFileName[i],"/outRosetta")
    pathExe <- paste(system.file(package="R.ROSETTA"), "exec/clrosetta.exe", sep="/")
    }else{
    dirList2=paste(tempDirNam,"results",csvFileName[i],"outRosetta", sep="\\")  
    pathExe <- paste(system.file(package="R.ROSETTA"), "exec","clrosetta.exe", sep="\\")
    }

    dir.create(dirList2)
    
    ## masking the attributes
    maskAttribute(maskFeaturesNames, dirList2)

    #copy file to execute it in folder

    #file.copy(pathExe, dirList2)
    file.copy(paste(dirList,"/",rosFileName,sep=""), dirList2)
    
    IDGfnam="maIDG.txt"
    FoldNam="objects"
    ## to mask the features set parameter IDGfn to TRUE
    ## generate command files to rosetta
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
                       #fallbackCertainty=
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
                       clroc=clroc,
    )
    
    seed=0
    # check the platform, for unix platform, you have to have wine Installed
    if(.Platform$OS.type=="unix")
    {
      comm=sprintf('wine %s CVSerialExecutor "INVERT = F; NUMBER = %i; SEED = %i; LENGTH = %i; FILENAME.COMMANDS = %s; FILENAME.LOG = %s" %s',
                   pathExe,
                   #paste0(dirList2,"/clrosetta.exe"),
                   cvNum,
                   seed,
                   pipeLen,
                   paste0(dirList2,"/","OUT_cmdCV.txt"),
                   paste0(dirList2,"/","logMain.txt"),
                   paste0(dirList2,"/",rosFileName)
      )
    }else{
      comm=sprintf('%s CVSerialExecutor "INVERT = F; NUMBER = %i; SEED = %i; LENGTH = %i; FILENAME.COMMANDS = %s; FILENAME.LOG = %s" %s',
                   pathExe,
                   #paste0(dirList2,"/clrosetta.exe"),
                   cvNum,
                   seed,
                   pipeLen,
                   paste(dirList2,"OUT_cmdCV.txt",sep="\\"),
                   paste(dirList2,"logMain.txt",sep="\\"),
                   paste(dirList2,rosFileName,sep="\\")
      )
    }
    try(system(command=comm, ignore.stdout = TRUE), silent=TRUE)
  }
  
  # prepare all results
  LFout=list.files(paste0(tempDirNam,"/results"))

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
  
  # statistic
  if(roc){
  for(i in 1:length(LFout)){
    path=paste0(tempDirNam,"/results","/",LFout[i],"/outRosetta")

      dfRes_rocAuc[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[1])))
      dfRes_rocAucSE[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[2])))
      ##ACC
      dfRes_accMean[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[3])))
      dfRes_accMedian[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[4])))
      dfRes_accStdDev[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[5])))
      dfRes_accMin[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[6])))
      dfRes_accMax[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[7])))
      ##ROC
      dfRes_rocMean[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[8])))
      dfRes_rocMedian[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[9])))
      dfRes_rocStdDev[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[10])))
      dfRes_rocMin[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[11])))
      dfRes_rocMax[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[12])))
      ##ROC SE
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
    
    }else{
    for(i in 1:length(LFout)){
    path=paste0(tempDirNam,"/results","/",LFout[i],"/outRosetta")
  
    dfRes_accMean[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[1])))
    dfRes_accMedian[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[2])))
    dfRes_accStdDev[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[3])))
    dfRes_accMin[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[4])))
    dfRes_accMax[i]=as.numeric(as.matrix(unname(rosResults(path, roc)$Value[5])))
    }
      
    outRos=data.frame(mean(dfRes_accMean),mean(dfRes_accMedian),mean(dfRes_accStdDev),mean(dfRes_accMin),mean(dfRes_accMax))
    colnames(outRos)<-c("Accuracy.Mean","Accuracy.Median","Accuracy.Std","Accuracy.Min","Accuracy.Max")
    rownames(outRos)<-""
    
    }

  
  #rownames(dfRes)<-as.character(unname(dfRes[,1]))
  #dfRes2=t(dfRes)
  #dfRes3 <-t(as.numeric(as.matrix(unname(dfRes2[2,]))))
  #colnames(dfRes3)<-colnames(dfRes2)
  
  # make mean accuracy for CV and also undersampling files
  dataset_merged=data.frame()
  for(i in 1:length(LFout)){
    path2=paste0(tempDirNam,"/results/",LFout[i],"/outRosetta/rules")
    file_list <- paste0(path2,"/",list.files(path=path2))
    
    for(file in file_list){
      temp_dataset <-read.table(file, header=TRUE, sep="\t")
      dataset_merged<-rbind(dataset_merged, temp_dataset)
      rm(temp_dataset)
    }
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
   #rules2=as.character(rl[,1])

  rl2=strsplit(rules2," AND ")
  lst=lapply(lapply(rl2, function(x) strsplit(x, "\\(")), unlist)
  
  ###each element separately###
  lst_feat=lapply(lapply(lst, function(x) x[seq(1,length(x),2)]), unlist)
  features2=unlist(lapply(lapply(lst_feat, function(x) paste(x, collapse = ",")), unlist))
  
  ###each element separately###
  if(discrete==T){
  lst_cuts=lapply(lapply(lst, function(x) x[-seq(1,length(x),2)]), unlist)
  
  lstc=lapply(lst_cuts,function(x) gsub("[[() *]", "", x))
  lstc2=lapply(lstc,function(x) (gsub(',$','', x)))
  lstc3=lapply(lstc2,function(x) (gsub('^,','', x)))
  lstc4=lapply(lstc3, function(x) unlist(strsplit(x, ",")))

  catchNumeric <- function(mylist) {
    newlist <- suppressWarnings(as.numeric(mylist))
    mylist <- list(mylist)
    mylist[!is.na(newlist)] <- newlist[!is.na(newlist)]
    unlist(mylist)
  }
  lst_cuts2=lapply(lstc4, catchNumeric)
  #lst_cuts2=lapply(lapply(lst_cuts, function(x) as.numeric(unlist(regmatches(x,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",x))))), unlist)
  #lst_cuts2[which(lapply(lst_cuts2,length)==0)]<-lapply(lst_cuts[which(lapply(lst_cuts2,length)==0)], function(x) gsub(")", "",x,fixed = T))
  
  lst_cuts22=unlist(lapply(lapply(lst_cuts2, function(x) paste(x, collapse = ",")), unlist))
  
  lst1=lapply(lst_cuts, function(x) gsub(".*\\*\\).*", "num>cut", x))
  lst2=lapply(lst1, function(x) gsub(".*\\[\\*.*", "num<cut", x))
  lst3=lapply(lst2, function(x) gsub(".*\\[.*\\).*", "cut1<num<cut2", x))
  lst4=lapply(lst3, function(x) gsub(".*).*","discrete",x))
  

  
  cuts2=unlist(lapply(lapply(lst4, function(x) paste(x, collapse = ",")), unlist))
  
  # calculate the sizes
  df222=data.frame(word = do.call(c, lst_cuts2),
                   group = rep(1:length(lst_cuts2), 
                               sapply(lst_cuts2, length)))
  
  # create constant size data frame
  lst_cuts3=lapply(lst_cuts2, 'length<-', max(table(df222$group)))
  df3=t(as.data.frame(lst_cuts3, stringsAsFactors=FALSE))
  
  decsFinal= unlist(lapply(as.character(choose_nfl), FUN=function(x) (regmatches(x, gregexpr("(?<=\\().*?(?=\\))", x, perl=T))[[1]])))
                          
  df_out=data.frame(features2,decsFinal, supp_lhs3, supp_rhs3, acc_rhs3, cov_lhs3, cov_rhs3, stab_lhs3, stab_rhs3, cuts2, df3)
  colnames(df_out)<-c("FEATURES","DECISION","SUPP_LHS","SUPP_RHS","ACC_RHS","COV_LHS","COV_RHS","STAB_LHS","STAB_RHS","CUT_COND",paste0("CUTS_",seq(1:max(table(df222$group)))))
  
  df_outU=unique(df_out[c("FEATURES", "DECISION", "CUT_COND")])
  allMat=do.call(paste0, df_out[c("FEATURES", "DECISION", "CUT_COND")])
  subMat=as.matrix(do.call(paste0, df_outU))
  #x=df_outU[1,]
  
  aggregate2 <- function(x){ 
    df_out3=df_out[which(match(allMat, x) == 1),]
    
    meanOrCharacter <- function(x){
      if(class(x) == "factor"){
        # check if in factor columns are characters or the numbers
        if(is.na(as.numeric(as.character(unname(unique(x))), options(warn=-1)))[1])
        {
          return(as.character(x)[1]) 
        }else{
          return(round(mean(as.numeric(as.character(x))),  digits = 4))
        }
      }
      if(class(x) == "numeric"){
        out <- round(mean(as.numeric(x), na.rm = TRUE), digits = 4)
        return(out)
      }
    }
    
    indx <- sapply(df_out3, is.factor)
    df_out3[indx] <- lapply(df_out3[indx], function(x) as.character(x))
    
    df_out4=aggregate(.~FEATURES+DECISION+CUT_COND, FUN=meanOrCharacter, data = df_out3, na.action = na.pass)
    return(df_out4)
  }
  
  df_out5=apply(subMat, 1, aggregate2)
  df_out2=do.call("rbind", df_out5)
  df_out2$SUPP_LHS<-round(df_out2$SUPP_LHS)
  df_out2$SUPP_RHS<-round(df_out2$SUPP_RHS)
  
  #df_out2=aggregate(.~FEATURES+DECISION+CUT_COND, mean, data = df_out, na.action = na.pass)
  
  }
  else 
  {
    #for already discretized data
    lst_cuts=lapply(lapply(lst, function(x) x[-seq(1,length(x),2)]), unlist)
    lst_cuts2=lapply(lapply(lst_cuts, function(x) as.numeric(unlist(regmatches(x,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",x))))), unlist)
    lst_cuts22=unlist(lapply(lapply(lst_cuts2, function(x) paste(x, collapse = ",")), unlist))
    
    df222=data.frame(word = do.call(c, lst_cuts2),
                     group = rep(1:length(lst_cuts2), 
                                 sapply(lst_cuts2, length)))
    
    lst_cuts3=lapply(lst_cuts2, 'length<-', max(table(df222$group)))
    df3=t(as.data.frame(lst_cuts3))
    
    decsFinal= unlist(lapply(as.character(choose_nfl), FUN=function(x) (regmatches(x, gregexpr("(?<=\\().*?(?=\\))", x, perl=T))[[1]])))
                                 
    df_out=data.frame(features2,decsFinal, supp_lhs3, supp_rhs3, acc_rhs3, cov_lhs3, cov_rhs3, stab_lhs3, stab_rhs3, df3, lst_cuts22)
    colnames(df_out)<-c("FEATURES","DECISION","SUPP_LHS","SUPP_RHS","ACC_RHS","COV_LHS","COV_RHS","STAB_LHS","STAB_RHS",paste0("CUTS_",seq(1:max(table(df222$group)))),"CUT_COND")
    df_out2=aggregate(.~FEATURES+DECISION+CUT_COND, mean, data = df_out, na.action = na.pass)
    
    }
  
  df_out2$SUPP_LHS<-round(df_out2$SUPP_LHS)
  df_out2$SUPP_RHS<-round(df_out2$SUPP_RHS)
  #df_out2[paste0("CUTS_",seq(1:max(table(df222$group))))]<-round(df_out2[paste0("CUTS_",seq(1:max(table(df222$group))))])
  
  #for(i in 1:length(levels(df_out$DECISION))){
  #  df_out2$DECISION[which(df_out2$DECISION==i)]<-levels(df_out$DECISION)[i]
  #}
  
  PVAL=c()
  for(i in 1:length(df_out2$SUPP_RHS)){
    k=round(df_out2$SUPP_LHS[i]*df_out2$ACC_RHS[i])
    
    R1=unname(table(df[,length(df)])[names(table(df[,length(df)]))== as.character(df_out2$DECISION[i])])
    N=dim(df)[1] 
    R2=N-R1
                # the number of decisions/objects/patients
    C1=df_out2$SUPP_LHS[i]   # LHS Support
    #C2=N-C1                  # total drawn
    #R1=dim(df)[2]            # total hits, number of features
    #R2=N-R1                  # number of features - number of decisions
    PVAL[i]=phyper(k-1, R1, R2, C1, lower.tail = FALSE)  # calculate pvalue from phypergeometric 
  }
  
  if(pAdjust){
  PVAL=p.adjust(PVAL, method=pAdjustMethod)
  }
  df_out3=data.frame(df_out2,PVAL)
  df_out4=df_out3[order(df_out3$PVAL,decreasing = F),]
  
  # clear all the created files and set the first driectory

  unlink(tempDirNam, recursive = TRUE)
  
  # create and return output
  # @statistic$...
  # @rules
  #setClass(Class="RosettaResults",representation(statistic="data.frame",rules="data.frame"))

  return(list(main=df_out4,quality=outRos,rules=rules2,usn=underSampleNum)) 

  }
  

 
}
