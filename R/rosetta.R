##### main function ##### 

rosetta <- function(dt,
                    classifier="StandardVoter",
                    cvNum=10,
                    discrete=FALSE,
                    discreteMethod="EqualFrequency",
                    discreteParam=3,
                    discreteMask=TRUE,
                    reducer="Johnson",
                    reducerDiscernibility="Object",
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
                    ruleFiltrSupport=c(1,5),
                    ruleFiltrAccuracy=c(0,0.6),
                    ruleFiltrCoverage=c(0,0),
                    ruleFiltrStability=c(0,0),
                    JohnsonParam=list(Modulo=TRUE, BRT=FALSE, BRTprec=0.9, Precompute=TRUE, Approximate=TRUE, Fraction=0.9),
                    GeneticParam=list(Modulo=TRUE, BRT=FALSE, BRTprec=0.9, Precompute=TRUE, Approximate=TRUE, Fraction=0.9, Algorithm="Simple"),
                    ManualNames=c(),
                    pAdjust=TRUE,
                    pAdjustMethod="bonferroni",
                    seed=1,
                    invert=FALSE,
                    fraction=0.5,
                    calibration=FALSE,
                    fillNA=FALSE,
                    fillNAmethod="meanMode",
                    remSpChars=FALSE)
{
# set seed
set.seed(seed)
  
if(dim(autcon)[2]-1 == 1){
  stop("Decision table is too small")
}else{
    
# change integers to numeric
indx <- sapply(dt, is.integer)
dt[indx] <- lapply(dt[indx], function(x) as.numeric(x))

# remove special characters from the feature names
if(remSpChars){
  colnames(dt) <- gsub("[[:punct:]]", "", colnames(dt))
}

# check if decision vector is factor and change if not
if(is.factor(dt[,length(dt)]) == F){
  dt[,length(dt)] <- as.factor(dt[,length(dt)])
}
    
##### additional functions #####

# check which element is numeric
catchNumeric <- function(mylist){
  newlist <- suppressWarnings(as.numeric(mylist))
  mylist <- list(mylist)
  mylist[!is.na(newlist)] <- newlist[!is.na(newlist)]
  unlist(mylist)
}

# clean the cuts from special characters    
cleanCuts <- function(mylist){
  lst_cuts <- lapply(lapply(lst, function(x) x[-seq(1,length(x),2)]), unlist)
  lstc <- lapply(lst_cuts,function(x) gsub("[[() *]", "", x))
  lstc2 <- lapply(lstc,function(x) (gsub(',$','', x)))
  lstc3 <- lapply(lstc2,function(x) (gsub('^,','', x)))
  lstc4 <- lapply(lstc3, function(x) unlist(strsplit(x, ",")))
  return(list(lstc3,lstc4))
}

# another version of rounding
round2 <- function(x, n){
  posneg <- sign(x)
  z <- trunc(abs(x)*10^n + 0.5)
  z <- z/10^n
  z*posneg
}

# check if in factor columns are characters or the numbers
meanOrCharacter <- function(x){
  if(class(x) == "factor"){
    if(is.na(as.numeric(as.character(unname(unique(x))), options(warn=-1)))[1]){
      return(as.character(x)[1]) 
    }else{
      return(round(mean(as.numeric(as.character(x))),  digits = 5))
    }
  }
  if(class(x) == "numeric"){
    out <- round(mean(as.numeric(x), na.rm = TRUE), digits = 5)
    return(out)
  }
}

# second version of aggregate 
aggregate2 <- function(x, y){ 
  df_out3 <- df_out[which(match(y, x) == 1),]
  indx <- sapply(df_out3, is.factor)
  df_out3[indx] <- lapply(df_out3[indx], function(x) as.character(x))
  df_out4 <- aggregate(.~features+levels+decision, FUN=meanOrCharacter, data = df_out3, na.action = na.pass)
  return(df_out4)
}

##### end of the functions #####
    
# set paths and create temporary directory
firstPath <- tempdir()
fname <- "data"
    
if(.Platform$OS.type == "unix")
{
  tempDirNam=paste(firstPath,paste0(format(Sys.time(), "%b_%d_%Y_%H%M%S"),"_RROS"),sep="/")
  dir.create(tempDirNam)
  dir.create(paste0(tempDirNam,"/data"))
  dir.create(paste0(tempDirNam,"/results"))
  }else{ # windows
  tempDirNam=paste(firstPath,paste0(format(Sys.time(), "%b_%d_%Y_%H%M%S"),"_RROS"),sep="\\")
  dir.create(tempDirNam)
  dir.create(paste0(tempDirNam,"\\data"))
  dir.create(paste0(tempDirNam,"\\results"))
  }
    
# training lines length
if(discrete == FALSE & fillNA == TRUE){
  pipeLen <- 6
}
if(discrete == FALSE & fillNA == FALSE){
  pipeLen <- 5
}
if(discrete == TRUE){
  pipeLen <- 4
}
    
##### undersampling #####
    
if(underSample == TRUE)
{
  if(underSampleNum == 0){
  # estimate the number of undersampling iterations
  n <- min(unname(table(as.character(dt[,length(dt)])))) #min
  k <- max(unname(table(as.character(dt[,length(dt)])))) #max
  rep <- 5000
  out <- integer(rep)
    for(j in 1:rep){
      vec <- rep(0,k)
      i <- 0
        while(length(which(vec==0))>0){
        vec[sample(k,n)]<-1
        i <- i+1
        }
          out[j] <- i
    }
        underSampleNum <- round(mean(unlist(out)))
  }
      
  # vector of the classes
  clvec <- as.character(dt[,length(dt)])
  # number of the classes
  clnum <- length(table(clvec))
  # names of the classes
  clnames <- names(table(clvec))
  classL <- list()
      
  # choose the number of objects in undersampled groups
  if(underSampleSize == 0){
  minC <- min(table(clvec)) # minimum class
    }else{
    minC <- underSampleSize # chosen number
    }
  # choose class
    for(i in 1:clnum){
    classL[[i]] <- which(clvec%in%clnames[i]) #
    }
  # create files with balanced classes
  for(j in 1:underSampleNum){
  samp <- list()
    for(i in 1:clnum){
    samp[[i]] <- sample(classL[[i]], minC)
    }
    df2 <- dt[unlist(samp),]
    dfToCsv(df2, paste0(fname,"_",j), tempDirNam, disc = discrete)
  }
# without undersampling
}else{
  dfToCsv(dt, fname, tempDirNam, disc = discrete)
}
    
##### end of undersampling part #####

ifelse(.Platform$OS.type == "unix", 
      csvFileName <- list.files(path=paste0(tempDirNam,"/data"), pattern = "\\.csv$"),
      csvFileName <- list.files(path=paste0(tempDirNam,"\\data"), pattern = "\\.csv$"))
# loop by files, 1 = no undersampling

for(i in 1:length(csvFileName)){
  if(.Platform$OS.type == "unix"){
  dir.create(paste0(tempDirNam,"/results/",csvFileName[i]))
  dir.create(paste0(tempDirNam,"/results/",csvFileName[i],"/outPrep"))
  file.copy(paste0(tempDirNam,"/data/",csvFileName[i]), paste0(tempDirNam,"/results/",csvFileName[i],"/outPrep"))
  dirList <- paste0(tempDirNam,"/results/",csvFileName[i],"/outPrep")
  }else{ #windows
  dir.create(paste0(tempDirNam,"\\results\\",csvFileName[i]))
  dir.create(paste0(tempDirNam,"\\results\\",csvFileName[i],"\\outPrep"))
  file.copy(paste0(tempDirNam,"\\data\\",csvFileName[i]), paste0(tempDirNam,"\\results\\",csvFileName[i],"\\outPrep"))
  dirList <- paste0(tempDirNam,"\\results\\",csvFileName[i],"\\outPrep")
  }
      
# convert CSV to ROS
csvToRos(dirList)
# find ROS files
rosFileName <- list.files(path = dirList, pattern = "\\.ros$")
# create directory for main results
dirList2 <- ifelse(.Platform$OS.type == "unix",
                   paste0(tempDirNam,"/results/",csvFileName[i],"/outRosetta"),
                   paste(tempDirNam,"results",csvFileName[i],"outRosetta", sep="\\"))
dir.create(dirList2)
# store pathway to ROSETTA exe
pathExe <- ifelse(.Platform$OS.type == "unix",
                  paste(system.file(package="R.ROSETTA"), "exec/clrosetta.exe", sep="/"),
                  paste(gsub("/","\\",system.file(package="R.ROSETTA"),fixed=T), "exec", "clrosetta.exe", sep="\\"))
# masking the attributes
IDGfnam <- maskAttribute(maskFeaturesNames, dirList2)
      
# copy .ros file to rosetta folder
cpyData <- ifelse(.Platform$OS.type=="unix",
                  file.copy(paste(dirList,"/",rosFileName,sep=""), dirList2),
                  file.copy(paste(dirList,"\\",rosFileName,sep=""), dirList2))
FoldNam <- "objects"

##### create commands ##### 
# to mask the features set parameter IDGfn to TRUE
# transmite parameters to generate command files to rosetta
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
  clroc=clroc,
  fraction=fraction,
  calibration=calibration,
  fillNA=fillNA,
  fillNAmethod=fillNAmethod)
      
# check the platform
if(.Platform$OS.type=="unix"){
comm <- sprintf('wine %s CVSerialExecutor "INVERT = %s; NUMBER = %i; SEED = %i; LENGTH = %i; FILENAME.COMMANDS = %s; FILENAME.LOG = %s" %s',
pathExe,
substr(as.character(invert),1,1),
cvNum,
seed,
pipeLen,
paste0(dirList2,"/","OUT_cmdCV.txt"),
paste0(dirList2,"/","logMain.txt"),
paste0(dirList2,"/",rosFileName))
# run ROSETTA exe
try(system(command=comm, ignore.stdout = TRUE), silent=TRUE) # suppress warnings and messages
  }else{
  comm <- paste0('cmd /K \"\"', 
  pathExe,
  '\" CVSerialExecutor \"INVERT = ',
  substr(as.character(invert),1,1),
  '; NUMBER = ',
  cvNum,
  '; SEED = ',
  seed,
  '; LENGTH = ',
  pipeLen,
  '; FILENAME.COMMANDS = ',
  paste0(dirList2,"\\","OUT_cmdCV.txt"),
  '; FILENAME.LOG =',
  paste0(dirList2,'\\','logMain.txt'),
  '\" ',
  paste0(dirList2,"\\",rosFileName),
  '\"')
  try(system(command = comm, ignore.stdout = TRUE), silent=TRUE) # supress warnings and comunicates
  }
}
    
# prepare all results
doNotDisplay <- ifelse(.Platform$OS.type == "unix",
                       LFout <- list.files(paste0(tempDirNam,"/results")),
                       LFout <- list.files(paste0(tempDirNam,"\\results")))
# make mean accuracy for CV and undersampled files
# statistic
if(roc){
  dfRes_rocAucSE<-dfRes_rocAuc<-dfRes_accMean<-dfRes_accMedian<-dfRes_accStdDev<-
  dfRes_accMin<-dfRes_accMax<-dfRes_rocMean<-dfRes_rocMedian<-dfRes_rocStdDev<-
  dfRes_rocMin<-dfRes_rocMax<-dfRes_rocseMean<-dfRes_rocseMedian<-dfRes_rocseStdDev<-
  dfRes_rocseMin<-dfRes_rocseMax<-c()
    for(i in 1:length(LFout)){
      if(.Platform$OS.type=="unix"){
      path <- paste0(tempDirNam,"/results","/",LFout[i],"/outRosetta")
      path_rocs <- paste0(tempDirNam,"/results","/",LFout[i],"/outRosetta/rocs")
      }else{
      path <- paste0(tempDirNam,"\\results","\\",LFout[i],"\\outRosetta")
      path_rocs <- paste0(tempDirNam,"\\results","\\",LFout[i],"\\outRosetta\\rocs")
      }
        
  rosres <- rosResults(path, roc)
        
  # ROC AUC
  dfRes_rocAuc[i] <- as.numeric(as.matrix(unname(rosres[which(rosres[,1]=="ROC.AUC"),2])))
  dfRes_rocAucSE[i] <- as.numeric(as.matrix(unname(rosres[which(rosres[,1]=="ROC.AUC.SE"),2])))
  # ACCURACY
  dfRes_accMean[i] <- as.numeric(as.matrix(unname(rosres[which(rosres[,1]=="Accuracy.Mean"),2])))
  dfRes_accMedian[i] <- as.numeric(as.matrix(unname(rosres[which(rosres[,1]=="Accuracy.Median"),2])))
  dfRes_accStdDev[i] <- as.numeric(as.matrix(unname(rosres[which(rosres[,1]=="Accuracy.StdDev"),2])))
  dfRes_accMin[i] <- as.numeric(as.matrix(unname(rosres[which(rosres[,1]=="Accuracy.Minimum"),2])))
  dfRes_accMax[i] <- as.numeric(as.matrix(unname(rosres[which(rosres[,1]=="Accuracy.Maximum"),2])))
  # ROC
  dfRes_rocMean[i] <- as.numeric(as.matrix(unname(rosres[which(rosres[,1]=="ROC.AUC.Mean"),2])))
  dfRes_rocMedian[i] <- as.numeric(as.matrix(unname(rosres[which(rosres[,1]=="ROC.AUC.Median"),2])))
  dfRes_rocStdDev[i] <- as.numeric(as.matrix(unname(rosres[which(rosres[,1]=="ROC.AUC.StdDev"),2])))
  dfRes_rocMin[i] <- as.numeric(as.matrix(unname(rosres[which(rosres[,1]=="ROC.AUC.Minimum"),2])))
  dfRes_rocMax[i] <- as.numeric(as.matrix(unname(rosres[which(rosres[,1]=="ROC.AUC.Maximum"),2])))
  # ROC SE
  dfRes_rocseMean[i] <- as.numeric(as.matrix(unname(rosres[which(rosres[,1]=="ROC.AUC.SE.Mean"),2])))
  dfRes_rocseMedian[i] <- as.numeric(as.matrix(unname(rosres[which(rosres[,1]=="ROC.AUC.SE.Median"),2])))
  dfRes_rocseStdDev[i] <- as.numeric(as.matrix(unname(rosres[which(rosres[,1]=="ROC.AUC.SE.StdDev"),2])))
  dfRes_rocseMin[i] <- as.numeric(as.matrix(unname(rosres[which(rosres[,1]=="ROC.AUC.SE.Minimum"),2])))
  dfRes_rocseMax[i] <- as.numeric(as.matrix(unname(rosres[which(rosres[,1]=="ROC.AUC.SE.Maximum"),2])))
        
  # create list of text files
  txt_files_ls <- list.files(path=path_rocs, pattern="*.txt", full.names = T) 
  # read txt files
  txt_files_df <- lapply(txt_files_ls, function(x) {read.table(file = x, fill=T)})
  combined_df <- data.frame()
    for(k in 1:length(txt_files_df)){
    combined_df <- rbind(combined_df,
                         data.frame(rep(k, dim(as.data.frame(txt_files_df[[k]])[,1:7])[1]),
                                    as.data.frame(txt_files_df[[k]])[,1:7]))
    }
  }
      
combined_df2 <- as.data.frame(apply(as.matrix(combined_df[-which(combined_df[,2]=="%"),]), 2, as.numeric))
colnames(combined_df2) <- c("CVNumber","OneMinusSpecificity","Sensitivity","Specificity","PPV","NPV","Accuracy","Threshold")

# create output for quality
outRos <- data.frame(mean(dfRes_accMean),mean(dfRes_accMedian),mean(dfRes_accStdDev),mean(dfRes_accMin),
  mean(dfRes_accMax),mean(dfRes_rocAuc),mean(dfRes_rocAucSE),mean(dfRes_rocMean), 
  mean(dfRes_rocMedian),mean(dfRes_rocStdDev), mean(dfRes_rocMin), mean(dfRes_rocMax),
  mean(dfRes_rocseMean), mean(dfRes_rocseMedian),mean(dfRes_rocseStdDev), mean(dfRes_rocseMin),
  mean(dfRes_rocseMax))
      
  colnames(outRos) <- c("accuracyMean","accuracyMedian","accuracyStd","accuracyMin","accuracyMax",
  "ROC.AUC","ROC.AUC.SE","ROC.AUC.MEAN","ROC.AUC.MEDIAN","ROC.AUC.STDEV","ROC.AUC.MIN","ROC.AUC.MAX",
  "ROC.AUC.SE.MEAN","ROC.AUC.SE.MEDIAN","ROC.AUC.SE.STDEV","ROC.AUC.SE.MIN","ROC.AUC.SE.MAX")
  rownames(outRos) <- ""
}else{ #ROC False
dfRes_accMean<-dfRes_accMedian<-dfRes_accStdDev<-dfRes_accMin<-dfRes_accMax<-c()
  for(i in 1:length(LFout)){
  path <- ifelse(.Platform$OS.type=="unix", paste0(tempDirNam,"/results","/",LFout[i],"/outRosetta"), paste0(tempDirNam,"\\results","\\",LFout[i],"\\outRosetta"))
  # ACCURACY
  dfRes_accMean[i] <- as.numeric(as.matrix(unname(rosResults(path, roc)$Value[1])))
  dfRes_accMedian[i] <- as.numeric(as.matrix(unname(rosResults(path, roc)$Value[2])))
  dfRes_accStdDev[i] <- as.numeric(as.matrix(unname(rosResults(path, roc)$Value[3])))
  dfRes_accMin[i] <- as.numeric(as.matrix(unname(rosResults(path, roc)$Value[4])))
  dfRes_accMax[i] <- as.numeric(as.matrix(unname(rosResults(path, roc)$Value[5])))
  }
      
outRos <- data.frame(mean(dfRes_accMean),mean(dfRes_accMedian),mean(dfRes_accStdDev),mean(dfRes_accMin),mean(dfRes_accMax))
colnames(outRos)<-c("accuracyMean","accuracyMedian","accuracyStd","accuracyMin","accuracyMax")
rownames(outRos)<-""
}
rules2 <- data.frame()
  for(i in 1:length(LFout)){
    if(.Platform$OS.type=="unix"){
    path2 <- paste0(tempDirNam,"/results/",LFout[i],"/outRosetta/rules")
    file_list <- paste0(path2,"/",list.files(path=path2))
    }else{ # windows
    path2 <- paste0(tempDirNam,"\\results\\",LFout[i],"\\outRosetta\\rules")
    file_list <- paste0(path2,"\\",list.files(path=path2))
    }
      
    for(file in file_list){
    temp_dataset <- read.table(file, header=TRUE, sep="\t")
    rules2 <- rbind(rules2, temp_dataset)
    rm(temp_dataset)
    }
  }
colnames(rules2) <- "rules"
    
# filtration
rules2 <- as.matrix(rules2)
rl2 <- as.matrix(rules2[!grepl("%", rules2, fixed = T)]) # delete comments
rl_r <- which(grepl("=>", rl2, fixed = T)) # select rules
rules <- rl2[rl_r]
    
# choose lines
supp_lhs <- rl2[which(grepl("Supp. (LHS) =", rl2, fixed = T))]
supp_rhs <- rl2[which(grepl("Supp. (RHS) =", rl2, fixed = T))]
acc_rhs <- rl2[which(grepl("Acc.  (RHS) =", rl2, fixed = T))]
cov_lhs <- rl2[which(grepl("Cov.  (LHS) =", rl2, fixed = T))]
cov_rhs <- rl2[which(grepl("Cov.  (RHS) =", rl2, fixed = T))]
stab_lhs <- rl2[which(grepl("Stab. (LHS) =", rl2, fixed = T))]
stab_rhs <- rl2[which(grepl("Stab. (RHS) =", rl2, fixed = T))]
# choose only values from lists
supp_lhs2 <- gsub(" object(s)","",unlist(regmatches(supp_lhs, gregexpr("\\[\\K[^\\]]+(?=\\])", supp_lhs, perl=TRUE))), fixed = T)
supp_rhs2 <- gsub(" object(s)","",unlist(regmatches(supp_rhs, gregexpr("\\[\\K[^\\]]+(?=\\])", supp_rhs, perl=TRUE))), fixed = T)
acc_rhs2 <- unlist(regmatches(acc_rhs, gregexpr("\\[\\K[^\\]]+(?=\\])", acc_rhs, perl=TRUE)))
cov_lhs2 <- unlist(regmatches(cov_lhs, gregexpr("\\[\\K[^\\]]+(?=\\])", cov_lhs, perl=TRUE)))
cov_rhs2 <- unlist(regmatches(cov_rhs, gregexpr("\\[\\K[^\\]]+(?=\\])", cov_rhs, perl=TRUE)))
stab_lhs2 <- unlist(regmatches(stab_lhs, gregexpr("\\[\\K[^\\]]+(?=\\])", stab_lhs, perl=TRUE)))
stab_rhs2 <- unlist(regmatches(stab_rhs, gregexpr("\\[\\K[^\\]]+(?=\\])", stab_rhs, perl=TRUE)))
    
# convert to numeric values 
# and choose max if there are two options
# SUPPORT LHS
supp_lhs3 <- unlist(lapply(lapply(strsplit(supp_lhs2, ","), as.numeric), max))
# SUPPORT RHS
supp_rhs3 <- unlist(lapply(lapply(strsplit(supp_rhs2, ","),as.numeric), max))
supp_rhs3n <- unlist(lapply(lapply(strsplit(supp_rhs2, ","),as.numeric), which.max))
# ACCURACY RHS
acc_rhs3 <- unlist(lapply(lapply(strsplit(as.character(acc_rhs2), ","), as.double), max))
acc_rhs3n <- unlist(lapply(lapply(strsplit(acc_rhs2, ","),as.numeric), which.max))
# COVERAGE RHS
cov_rhs3 <- unlist(lapply(lapply(strsplit(as.character(cov_rhs2), ","),as.double),max))
# COVERAGE LHS
cov_lhs3 <- unlist(lapply(lapply(strsplit(as.character(cov_lhs2), ","),as.double),max)) #as.double(cov_lhs2)
# STABIL LHS
stab_lhs3 <- unlist(lapply(lapply(strsplit(as.character(stab_lhs2), ","),as.double),max))
# STABIL RHS
stab_rhs3 <- unlist(lapply(lapply(strsplit(as.character(stab_rhs2), ","),as.double),max))
# RULES
rules2 <- unlist(lapply(strsplit(as.character(rules), " =>", fixed=TRUE), `[`, 1))
dec_class <- strsplit(as.character(unlist(lapply(strsplit(as.character(rules), " => ", fixed=TRUE), `[`, 2))), " OR ", fixed=TRUE)
    
# choosing element according to greater accuracy
choose_nfl0 <- character(length(acc_rhs3n))
    
choose_nfl <- sapply(1:max(as.numeric(acc_rhs3n)),
  FUN = function(i){
  choose_nfl0[which(acc_rhs3n == i)] <- unlist(lapply(dec_class, '[', i))[which(acc_rhs3n==i)]
  return(choose_nfl0)}
  )
    
choose_nfl <- apply(choose_nfl, 1, paste0, collapse="")
rl2 <- strsplit(as.character(rules2), " AND ")
lst <- lapply(lapply(rl2, function(x) strsplit(x, "\\(")), unlist)
lst_feat <- lapply(lapply(lst, function(x) x[seq(1,length(x),2)]), unlist)
features2 <- unlist(lapply(lapply(lst_feat, function(x) paste(x, collapse = ",")), unlist))
    
### continuous data ###
if(discrete == FALSE){
lst_cuts2 <- lapply(cleanCuts(lst)[[2]], catchNumeric)
lst_cuts22 <- unlist(lapply(lapply(lst_cuts2, function(x) paste(x, collapse = ",")), unlist))
# remove brackets and change them into conditions
lst_cuts <- lapply(lapply(lst, function(x) x[-seq(1,length(x),2)]), unlist)
lst1 <- lapply(lst_cuts, function(x) gsub(".*\\*\\).*", "value>cut", x))
lst2 <- lapply(lst1, function(x) gsub(".*\\[\\*.*", "value<cut", x))
lst3 <- lapply(lst2, function(x) gsub(".*\\[.*\\).*", "cut<value<cut", x))
lst4 <- lapply(lst3, function(x) gsub(".*).*","discrete",x))
cuts2 <- unlist(lapply(lapply(lst4, function(x) paste(x, collapse = ",")), unlist))
      
# calculate the sizes
df222 <- data.frame(word = do.call(c, lst_cuts2),
                    group = rep(1:length(lst_cuts2),
                                sapply(lst_cuts2, length)))
# create constant size data frame for cuts
lst_cuts3 <- lapply(lst_cuts2, 'length<-', max(table(df222$group)))
df3 <- t(as.data.frame(lst_cuts3, stringsAsFactors=FALSE))
# retrieve discretization states
dataset_cuts <- dataset_rules <- data.frame()
st3 <- cutsToStates<-list()
# loop on files
for(l in 1:length(LFout)){
if(.Platform$OS.type == "unix"){
  path_rules <- paste0(tempDirNam,"/results/",LFout[l],"/outRosetta/rules")
  path_cuts <- paste0(tempDirNam,"/results/",LFout[l],"/outRosetta/cuts")
  files_rules <- paste0(path_rules,"/",list.files(path=path_rules))
  files_cuts <- paste0(path_cuts,"/",list.files(path=path_cuts))
  }else{ # windows
  path_rules <- paste0(tempDirNam,"\\results\\",LFout[l],"\\outRosetta\\rules")
  path_cuts <- paste0(tempDirNam,"\\results\\",LFout[l],"\\outRosetta\\cuts")
  files_rules <- paste0(path_rules,"\\",list.files(path=path_rules))
  files_cuts <- paste0(path_cuts,"\\",list.files(path=path_cuts))
  }
        
# loop on CV files - retrieve discrete values from cuts
for(k in 1:length(files_rules)){
# read rule file
dataset_rules <- read.table(files_rules[k], header=FALSE, sep="\t")
colnames(dataset_rules) <- "rules"
# read cut file
dataset_cuts <- read.table(files_cuts[k], header=FALSE, sep="\t")
colnames(dataset_cuts) <- c("group","cuts")
# replace groups for feature names
key <- 0:(dim(dt)[2]-2)
val <- colnames(dt)[c(unname(which(unlist(lapply(dt, is.numeric)) | unlist(lapply(dt, is.integer)))),unname(which(!unlist(lapply(dt, is.numeric)) & !unlist(lapply(dt, is.integer)))))]
out <- lapply(1:(dim(dt)[2]-1),FUN = function(i){dataset_cuts[dataset_cuts$group == key[i],1] <<- val[i]})
          
# filter out comments
rl2 <- as.matrix(dataset_rules[!grepl("%", dataset_rules, fixed = T)]) #deleting comments
rl_r <- which(grepl("=>", rl2, fixed = T)) #choosing rules
rules2 <- unlist(lapply(strsplit(as.character(rl2[rl_r]), " =>", fixed=TRUE), `[`, 1))
lst <- lapply(lapply(strsplit(as.character(rules2)," AND "), function(x) strsplit(x, "\\(")), unlist)
lst_cuts2 <- lapply(cleanCuts(lst)[[2]], catchNumeric)
          
# in case of 2 levels, right or left
lstCuts2 <- lapply(lapply(lst, function(x) x[-seq(1,length(x),2)]), unlist)
lstCuts22 <- lapply(lstCuts2, function(x) gsub(".*\\*\\).*", 2, x))
lstCuts222 <- lapply(lstCuts22, function(x) gsub(".*\\[\\*.*", 1, x))
lstCuts3 <- cleanCuts(lst)[[1]]
# list of the cuts - lstc3
# library - dataset_cuts
# list of the features
lst_feat <- lapply(lapply(lst, function(x) x[seq(1,length(x),2)]), unlist)
## features2 <- unlist(lapply(lapply(lst_feat, function(x) paste(x, collapse = ",")), unlist))
## dataset_cuts[,2] <- dataset_cuts[,2]/1e+06
## epsil <- 0.0001
## create states
st2 <- list()
for(j in 1:length(lst_feat)){
st <- c()
  for(i in 1:length(lstCuts3[[j]]))
  {
  tempCuts <- dataset_cuts[which(dataset_cuts[,1] %in% lst_feat[[j]][i]),]
  rownames(tempCuts) <- NULL
  # check if the feature is character or not
    if(dim(tempCuts)[1]==0){
    st[i] <- lstCuts3[[j]][i]
    }else{
      if(as.numeric(unlist(strsplit(lstCuts3[[j]][i], ",")))[1]%%1==0 & !grepl("\\.",unlist(strsplit(lstCuts3[[j]][i], ","))[1])){ #integers
        if(grepl(",",lstCuts3[[j]][i])){ #for ranges -> middle classes
          st[i] <- which(round2(tempCuts$cuts,0)==min(as.numeric(unlist(strsplit(lstCuts3[[j]][i], ",")))))+1 
        }else{ # for single -> extremes #left
          if(which(round2(tempCuts$cuts,0)==as.numeric(unlist(lstCuts3[[j]][i])))==1){
          st[i] <- 1
        }else{ #right
        st[i] <- which(round2(tempCuts$cuts,0)==as.numeric(unlist(lstCuts3[[j]][i])))+1
        }
        }
      }else{
      tempCuts$cuts <- tempCuts$cuts/1e+04
        if(grepl(",",lstCuts3[[j]][i])){ ##for ranges -> middle classes
        st[i] <- which(abs(tempCuts$cuts-min(as.numeric(unlist(strsplit(lstCuts3[[j]][i], ",")))))==min(abs(tempCuts$cuts-min(as.numeric(unlist(strsplit(lstCuts3[[j]][i], ",")))))) & abs(tempCuts$cuts-min(as.numeric(unlist(strsplit(lstCuts3[[j]][i], ","))))) >= 0)+1 
        }else{ # for single -> extremes #left
          if(which(abs(tempCuts$cuts-as.numeric(lstCuts3[[j]][i]))==min(abs(tempCuts$cuts-as.numeric(lstCuts3[[j]][i]))) & abs(tempCuts$cuts-as.numeric(lstCuts3[[j]][i])) >= 0)==1){
            st[i] <- lstCuts222[[j]][i]
        }else{ #right
        st[i] <- which(abs(tempCuts$cuts-as.numeric(lstCuts3[[j]][i]))==min(abs(tempCuts$cuts-as.numeric(lstCuts3[[j]][i]))) & abs(tempCuts$cuts-as.numeric(lstCuts3[[j]][i])) >= 0)+1
        }
        }
       }
    }#else from integer
  }#else from character
  st2[[j]] <- st
}#end for
st3[[k]] <- unlist(lapply(lapply(st2, function(x) paste(x, collapse = ",")), unlist))
}
cutsToStates[[l]] <- unlist(st3)
}
      
##### final output #####                         
decsFinal <- unlist(lapply(as.character(choose_nfl), FUN=function(x) (regmatches(x, gregexpr("(?<=\\().*?(?=\\))", x, perl=T))[[1]])))
df_out <- data.frame(features2,unlist(cutsToStates),decsFinal, supp_lhs3, supp_rhs3, acc_rhs3, cov_lhs3, cov_rhs3, stab_lhs3, stab_rhs3, cuts2, df3)
colnames(df_out) <- c("features","levels","decision","supportLHS","supportRHS","accuracyRHS","coverageLHS","coverageRHS","stabilityLHS","stabilityRHS","cuts",paste0("cut",seq(1:max(table(df222$group)))))
df_outU <- unique(df_out[c("features","levels", "decision")])
allMat <- do.call(paste0, df_out[c("features","levels", "decision")])
subMat <- as.matrix(do.call(paste0, df_outU))
df_out5 <- apply(subMat, 1, aggregate2, y=allMat)
df_out2 <- do.call("rbind", df_out5)
df_out2$supportLHS <- round(df_out2$supportLHS)
df_out2$supportRHS <- round(df_out2$supportRHS)
  }else{ #for discrete data
  lst_cuts <- lapply(lapply(lst, function(x) x[-seq(1,length(x),2)]), unlist)
  lst_cuts2 <- lapply(lapply(lst_cuts, function(x) gsub(")","",x)), unlist)
  lst_cuts22 <- unlist(lapply(lapply(lst_cuts2, function(x) paste(x, collapse = ",")), unlist))
  df222 <- data.frame(word = do.call("c", lst_cuts2),
                        group = rep(1:length(lst_cuts2), 
                                    sapply(lst_cuts2, length)))
  lst_cuts3 <- lapply(lst_cuts2, 'length<-', max(table(df222$group)))
  df3 <- t(as.data.frame(lst_cuts3))
  decsFinal <- unlist(lapply(as.character(choose_nfl), FUN=function(x) (regmatches(x, gregexpr("(?<=\\().*?(?=\\))", x, perl=T))[[1]])))
  df_out <- data.frame(features2, lst_cuts22,decsFinal, supp_lhs3, supp_rhs3, acc_rhs3, cov_lhs3, cov_rhs3, stab_lhs3, stab_rhs3) #, df3
  colnames(df_out) <- c("features","levels","decision","supportLHS","supportRHS","accuracyRHS","coverageLHS","coverageRHS","stabilityLHS","stabilityRHS") #,paste0("Cut",seq(1:max(table(df222$group))))
  df_outU <- unique(df_out[c("features", "levels", "decision")])
  allMat <- do.call(paste0, df_out[c("features", "levels", "decision")])
  subMat <- as.matrix(do.call(paste0, df_outU))
      
#aggregate2 <- function(x, y){ 
#df_out3 <- df_out[which(match(y, x) == 1),]
#indx <- sapply(df_out3, is.factor)
#df_out3[indx] <- lapply(df_out3[indx], function(x) as.character(x))
#df_out4 <- aggregate(.~features+levels+decision, FUN=meanOrCharacter, data = df_out3, na.action = na.pass)
#return(df_out4)
#}
  df_out5 <- apply(subMat, 1, aggregate2, y=allMat)
  df_out2 <- do.call("rbind", df_out5)
  df_out2$supportLHS <- round(df_out2$supportLHS)
  df_out2$supportRHS <- round(df_out2$supportRHS)
  }
    
# rule statistics #
# p-value for rules
PVAL <- c()
RISK_PVAL <- c()
CONF_INT <- c()
REL_RISK <- c()
    
for(i in 1:length(df_out2$decision)){
k <- round(df_out2$supportRHS[i])-1 # P(X > k-1) <-> P(X >= k) ### 
  if(underSample == TRUE){
    if(underSampleSize == 0){
    R1 <- min(unname(table(as.character(dt[,length(dt)])))) #min
    }else{
    R1 <- underSampleSize}
    }else{
    R1 <- unname(table(dt[,length(dt)])[names(table(dt[,length(dt)]))==as.character(df_out2$decision[i])]) # num of samples for current decision - total white balls
  }
N <- dim(dt)[1] # total number
R2 <- N-R1 # num of samples for the rest samples - total black balls
# the number of decisions/objects/patients
C1 <- df_out2$supportLHS[i] #number of balls drawn 
PVAL[i] <- phyper(q=k, m=R1, n=R2, k=C1, lower.tail = FALSE)  # calculate pvalue from phypergeometric

# risk ratio
ge1 <- df_out2$supportRHS[i]
# The number of disease occurence among exposed cohort.
ge2 <- df_out2$supportLHS[i] - df_out2$supportRHS[i] ## LHS > RHS
# The number of disease occurence among non-exposed cohort.
gt1 <- R1
# The number of individuals in exposed cohort group.
gt2 <- R2
# The number of individuals in non-exposed cohort group.
invisible(capture.output(rr <- fmsb::riskratio(ge1, ge2, gt1, gt2)))
ints <- rr$conf.int[1:2]
ints[is.na(ints)] <- -Inf
CONF_INT[i] <- paste(as.character(round(ints, digits=3)), collapse =":") #rr 95% confidence intervals
RISK_PVAL[i] <- rr$p.value #rr p-value
REL_RISK[i] <- rr$estimate #risk ratio estimate
}
    
if(pAdjust){
PVAL <- p.adjust(PVAL, method=pAdjustMethod)
RISK_PVAL <- p.adjust(RISK_PVAL, method=pAdjustMethod)
}
numClass <- rep(0,length(df_out2$decision))
    
for(i in 1:length(table(dt[,length(dt)]))){
numClass[which(df_out2$decision == names(table(dt[,length(dt)]))[i])] <- unname(table(dt[,length(dt)]))[i]
}
percSupportLHS <- round(df_out2$supportLHS/numClass, digits=5)                         
percSupportRHS <- round(df_out2$supportRHS/numClass, digits=5)
df_out3 <- data.frame(df_out2, percSupportLHS, percSupportRHS, PVAL, REL_RISK, RISK_PVAL, CONF_INT)
colnames(df_out3) <- c(colnames(df_out2), "supportRatioLHS", "supportRatioRHS", "pValue", "riskRatio", "pValueRiskRatio", "confIntRiskRatio")
    
# sort rows by p-value
df_out4 <- df_out3[order(df_out3$pValue,decreasing = F),]
# reset row names
rownames(df_out4) <- NULL
# clear the files in temporary directory
unlink(tempDirNam, recursive = TRUE)
# output results
  if(underSample == TRUE){
    ifelse(roc, return(list(main=df_out4, quality=outRos, ROC.stats=combined_df2, usMeanAccs=dfRes_accMean, usn=underSampleNum)),
    return(list(main=df_out4, quality=outRos, usMeanAccs=dfRes_accMean, usn=underSampleNum)))
    }else{
    ifelse(roc, return(list(main=df_out4, quality=outRos, ROCstats=combined_df2)),
    return(list(main=df_out4, quality=outRos)))  
    }
}
  
} # close the function
