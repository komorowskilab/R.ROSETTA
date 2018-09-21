genCmdFilesRosetta <- function (dir_file3,
                                classifier,
                                discMethod, #discretization method
                                discParam, #discretization parameter
                                discMask, #discrtization mask
                                IDGlog, #Switch IDG, if F everything is switch off
                                IDGfn, #file IDG
                                maskFeaturesNames,
                                ruleMeth="JohnsonReducer", #reduce method
                                proNam=Sys.Date(), #project name
                                LogVerb, #log verbose
                                disc,
                                fallBack,
                                fallBackCert,
                                fallBackClass,
                                ruleFiltration,
                                ruleFiltrSupport,
                                ruleFiltrAccuracy,
                                ruleFiltrCoverage,
                                ruleFiltrStability,
                                reducerDiscernibility,
                                JohnsonParam,
                                GeneticParam,
                                ManualNames,
                                roc,
                                clroc
                                )
  
  {
  
  file_name=proNam
  fn=paste(file_name,"_cmdCV.txt", sep="")
  
  ##mainFolderName
  maFoNam=paste("OUT_",proNam,sep="")
  if (file.exists(fn)) unlink(file.path(dir_file3, fn), recursive=TRUE)

  ######-----DISCRETIZATION ON-----####################################
  ##########################################################################
  
  ##converting names of reducers
  if(ruleMeth=="Johnson")
  {
    ruleMeth="JohnsonReducer"
  }
  if(ruleMeth=="Genetic")
  {
    ruleMeth="SAVGeneticReducer"
  }
  if(ruleMeth=="Holte1R")
  {
    ruleMeth="Holte1RReducer"
  }
  if(ruleMeth=="Manual")
  {
    ruleMeth="ManualReducer"
  }
  
  if(.Platform$OS.type=="unix")
  {
  if(disc==FALSE){
    
    dir.create(paste0(dir_file3,"/rules"))
    dir.create(paste0(dir_file3,"/logs"))
    dir.create(paste0(dir_file3,"/cuts"))
    dir.create(paste0(dir_file3,"/rocs"))
    
    listOut=NULL
    
    #translating name of discretizations
    if(discMethod=="MDL")
    {
      discMethod="EntropyScaler"
    }
    
    if(discMethod=="Naive")
    {
      discMethod="NaiveScaler"
    }
    
    if(discMethod=="SemiNaive")
    {
      discMethod="SemiNaiveScaler"
    }
    
    if(discMethod=="EqualFrequency")
    {
      discMethod="EqualFrequencyScaler"
    }
    if(discMethod=="BROrthogonal")
    {
      discMethod="BROrthogonalScaler"
    }
    
    ##discretization methods and parameters
    listOut[1]=discMethod
    
    if(discMethod=="EntropyScaler" | discMethod=="NaiveScaler" | discMethod=="SemiNaiveScaler")
    {
    listOut[2]=paste0("{MODE=Save; MASK=",substr(as.character(discMask),1,1),"; FILENAME=",dir_file3,
                      "/cuts/cuts_",file_name,"_#ITERATION#.txt;}")
    }
    
    if(discMethod=="EqualFrequencyScaler")
    {
      listOut[2]=paste0("{MODE=Save; MASK=",substr(as.character(discMask),1,1),"; FILENAME=",dir_file3,
                        "/cuts/cuts_",file_name,"_#ITERATION#.txt; INTERVALS=",discParam,";}")
    }
    
    if(discMethod=="BROrthogonalScaler")
    {
      listOut[2]=paste0("{MODE=Save; MASK=",substr(as.character(discMask),1,1),"; FILENAME=",dir_file3,
                        "/cuts/cuts_",file_name,"_#ITERATION#.txt; APPROXIMATE=",substr(as.character(discParam[1]),1,1),"; FRACTION=",discParam[2],";}")
    }
    

    #ifs for reducers
    listOut[3]=ruleMeth
      if(ruleMeth=="JohnsonReducer"){
    listOut[4]=paste("{DISCERNIBILITY=",reducerDiscernibility,"; SELECTION=All; MODULO.DECISION=",substr(as.character(JohnsonParam[1]),1,1),"; BRT=",substr(as.character(JohnsonParam[2]),1,1),"; BRT.PRECISION=",JohnsonParam[3],"; IDG=",IDGlog,"; IDG.FILENAME=",
                     dir_file3,"/",IDGfn,"; PRECOMPUTE=",substr(as.character(JohnsonParam[4]),1,1),"; APPROXIMATE=",substr(as.character(JohnsonParam[5]),1,1),"; FRACTION=",JohnsonParam[6],"}",sep="")
      }
     if(ruleMeth=="SAVGeneticReducer"){
       listOut[4]=paste("{DISCERNIBILITY=",reducerDiscernibility,"; SELECTION=All; MODULO.DECISION=",substr(as.character(GeneticParam[1]),1,1),"; BRT=",substr(as.character(GeneticParam[2]),1,1),"; BRT.PRECISION=",GeneticParam[3],"; IDG=",IDGlog,"; IDG.FILENAME=",
                        dir_file3,"/",IDGfn,"; PRECOMPUTE=",substr(as.character(GeneticParam[4]),1,1),"; APPROXIMATE=",substr(as.character(GeneticParam[5]),1,1),"; FRACTION=",GeneticParam[6],"}",sep="")
     }
    if(ruleMeth=="Holte1RReducer"){
      listOut[4]=paste("{}",sep="")
    }
    if(ruleMeth=="ManualReducer"){
      listOut[4]=paste("{ATTRIBUTES=",paste(ManualNames,collapse=","),"}",sep="")
    }
    listOut[5]="RuleGenerator"
    listOut[6]="{}"
    
    if(ruleFiltration==T)
    {
    listOut[7]="MyRuleFilter"
    listOut[8]=paste0("{FILTERING = 1; SUPPORT.RHS.LOWER = ",ruleFiltrSupport[1],"; SUPPORT.RHS.UPPER = ",ruleFiltrSupport[2],
                      "; ACCURACY.RHS.LOWER = ",ruleFiltrAccuracy[1],"; ACCURACY.RHS.UPPER = ",ruleFiltrAccuracy[2],
                      "; COVERAGE.RHS.LOWER = ",ruleFiltrCoverage[1],"; COVERAGE.RHS.UPPER = ",ruleFiltrCoverage[2],
                      "; STABILITY.RHS.LOWER = ",ruleFiltrStability[1],"; STABILITY.RHS.UPPER = ",ruleFiltrStability[2],";}")
    }else
    {
      listOut[7]="MyRuleFilter"
      listOut[8]="{}"
    }

    
    
    listOut[9]="MyRuleExporter"
    listOut[10]=paste("{FILENAME=", dir_file3,"/rules/rules_", file_name,"_#ITERATION#.txt}", sep="")
    listOut[11]="OrthogonalFileScaler"
    listOut[12]=paste0("{MODE=Load; FILENAME =",paste0(dir_file3),"/cuts/cuts_",file_name,
                       "_#ITERATION#.txt}")
    listOut[13]="BatchClassifier"
    
    ### IF for IDG masking ###
    if(IDGlog)
    {
      IDGpath=paste0("; IDG.FILENAME=",dir_file3,"/",IDGfn)
    }else{
      IDGpath=""
    }
    
    #######ifs for classifiers######
    
    if(classifier=="ObjectTrackingVoter")
    {
    listOut[14]=paste("{CLASSIFIER=",classifier,"; FRACTION=0.0; IDG=",substr(as.character(IDGlog),1,1),IDGpath,";"
                      ," FALLBACK=",substr(as.character(fallBack),1,1),"; FALLBACK.CLASS=",fallBackClass,"; MULTIPLE=Best; LOG=T; LOG.FILENAME=",
                      dir_file3,"/logs/log_",file_name,"_#ITERATION#.txt; LOG.VERBOSE=",
                      substr(as.character(LogVerb),1,1),"; CALIBRATION=F; ROC=",substr(as.character(roc),1,1),"; ROC.CLASS=",clroc,"; ROC.FILENAME=",dir_file3,"/rocs/roc_",file_name,"_#ITERATION#.txt}", sep="")
    }
    if(classifier=="StandardVoter")
    {
      listOut[14]=paste("{CLASSIFIER=",classifier,"; FRACTION=0.0; IDG=",substr(as.character(IDGlog),1,1),IDGpath,";"
                        ," SPECIFIC=F; VOTING=Support; NORMALIZATION=Firing; FALLBACK=",substr(as.character(fallBack),1,1),"; FALLBACK.CLASS=",fallBackClass,"; MULTIPLE=Best; LOG=T; LOG.FILENAME=",
                        dir_file3,"/logs/log_",file_name,"_#ITERATION#.txt; LOG.VERBOSE=",
                        substr(as.character(LogVerb),1,1),"; CALIBRATION=F; ROC=",substr(as.character(roc),1,1),"; ROC.CLASS=",clroc,"; ROC.FILENAME=",dir_file3,"/rocs/roc_",file_name,"_#ITERATION#.txt}", sep="")
      
    }
    if(classifier=="NaiveBayesClassifier")
    {
      listOut[14]=paste("{CLASSIFIER=",classifier,";"
                        ," FALLBACK=",substr(as.character(fallBack),1,1),"; FALLBACK.CLASS=",fallBackClass,"; MULTIPLE=Best; LOG=T; LOG.FILENAME=",
                        dir_file3,"/logs/log_",file_name,"_#ITERATION#.txt; LOG.VERBOSE=",
                        substr(as.character(LogVerb),1,1),"; CALIBRATION=F; ROC=",substr(as.character(roc),1,1),"; ROC.CLASS=",clroc,"; ROC.FILENAME=",dir_file3,"/rocs/roc_",file_name,"_#ITERATION#.txt}", sep="")
      
    }
     write.table(listOut,file=paste(dir_file3,"/","OUT_cmdCV",".txt", sep=""),
              quote=F,col.names = F,row.names = F)
  }
  
  ######-----DISCRETIZATION OFF-----########################################
  ##########################################################################
  
    else{ 
    
    dir.create(paste0(dir_file3,"/rules"))
    dir.create(paste0(dir_file3,"/logs"))
    dir.create(paste0(dir_file3,"/rocs"))
    
    listOut=NULL
    
    ### IF for IDG masking ###
    if(IDGlog)
    {
      IDGpath=paste0("; IDG.FILENAME=",dir_file3,"/",IDGfn)
    }else{
      IDGpath=""
    }
     
     #if for reducers
     listOut[1]=ruleMeth
     
     if(ruleMeth=="JohnsonReducer"){
       listOut[2]=paste("{DISCERNIBILITY=",reducerDiscernibility,"; SELECTION=All; MODULO.DECISION=",substr(as.character(JohnsonParam[1]),1,1),"; BRT=",substr(as.character(JohnsonParam[2]),1,1),"; BRT.PRECISION=",JohnsonParam[3],"; IDG=",IDGlog,"; IDG.FILENAME=",
                        dir_file3,"/",IDGfn,"; PRECOMPUTE=",substr(as.character(JohnsonParam[4]),1,1),"; APPROXIMATE=",substr(as.character(JohnsonParam[5]),1,1),"; FRACTION=",JohnsonParam[6],"}",sep="")
     }
     if(ruleMeth=="SAVGeneticReducer"){
       listOut[2]=paste("{DISCERNIBILITY=",reducerDiscernibility,"; SELECTION=All; MODULO.DECISION=",substr(as.character(GeneticParam[1]),1,1),"; BRT=",substr(as.character(GeneticParam[2]),1,1),"; BRT.PRECISION=",GeneticParam[3],"; IDG=",IDGlog,"; IDG.FILENAME=",
                        dir_file3,"/",IDGfn,"; PRECOMPUTE=",substr(as.character(GeneticParam[4]),1,1),"; APPROXIMATE=",substr(as.character(GeneticParam[5]),1,1),"; FRACTION=",GeneticParam[6],"}",sep="")
     }
     if(ruleMeth=="Holte1RReducer"){
       listOut[2]=paste("{}",sep="")
     }
     if(ruleMeth=="ManualReducer"){
       listOut[2]=paste("{ATTRIBUTES=",paste(ManualNames,collapse=","),"}",sep="")
     }
     
     
          listOut[3]="RuleGenerator"
          listOut[4]="{}"
          
          if(ruleFiltration==T)
          {
            listOut[5]="MyRuleFilter"
            listOut[6]=paste0("{FILTERING = 1; SUPPORT.RHS.LOWER = ",ruleFiltrSupport[1],"; SUPPORT.RHS.UPPER = ",ruleFiltrSupport[2],
                              "; ACCURACY.RHS.LOWER = ",ruleFiltrAccuracy[1],"; ACCURACY.RHS.UPPER = ",ruleFiltrAccuracy[2],
                              "; COVERAGE.RHS.LOWER = ",ruleFiltrCoverage[1],"; COVERAGE.RHS.UPPER = ",ruleFiltrCoverage[2],
                              "; STABILITY.RHS.LOWER = ",ruleFiltrStability[1],"; STABILITY.RHS.UPPER = ",ruleFiltrStability[2],";}")
          }else
          {
            listOut[5]="MyRuleFilter"
            listOut[6]="{}"
          }      
          
          
          
          listOut[7]="MyRuleExporter"
          listOut[8]=paste("{FILENAME=", dir_file3,"/rules/rules_", file_name,"_#ITERATION#.txt}", sep="")
          listOut[9]="BatchClassifier"
          
          #######ifs for classifiers######
          
          if(classifier=="ObjectTrackingVoter")
          {
            listOut[10]=paste("{CLASSIFIER=",classifier,"; FRACTION=0.0; IDG=",substr(as.character(IDGlog),1,1),IDGpath,";"
                              ," FALLBACK=",substr(as.character(fallBack),1,1),"; FALLBACK.CLASS=",fallBackClass,"; MULTIPLE=Best; LOG=T; LOG.FILENAME=",
                              dir_file3,"/logs/log_",file_name,"_#ITERATION#.txt; LOG.VERBOSE=",
                              substr(as.character(LogVerb),1,1),"; CALIBRATION=F; ROC=",substr(as.character(roc),1,1),"; ROC.CLASS=",clroc,"; ROC.FILENAME=",dir_file3,"/rocs/roc_",file_name,"_#ITERATION#.txt}", sep="")
            
          }else
          {
          listOut[10]=paste("{CLASSIFIER=",classifier,"; FRACTION=0.0; IDG=",substr(as.character(IDGlog),1,1),IDGpath,";"
                            ," SPECIFIC=F; VOTING=Support; NORMALIZATION=Firing; FALLBACK=",substr(as.character(fallBack),1,1),"; FALLBACK.CLASS=",fallBackClass,
                            "; MULTIPLE=Best; LOG=T; LOG.FILENAME=",
                            dir_file3,"/logs/log_",file_name,"_#ITERATION#.txt; LOG.VERBOSE=",
                            substr(as.character(LogVerb),1,1),"; CALIBRATION=F; ROC=",substr(as.character(roc),1,1),"; ROC.CLASS=",clroc,"; ROC.FILENAME=",dir_file3,"/rocs/roc_",file_name,"_#ITERATION#.txt}", sep="")
          }
            write.table(listOut,file=paste(dir_file3,"/","OUT_cmdCV",".txt", sep=""),
                quote=F,col.names = F,row.names = F)

  }
  }else{ ##WINDOWS##
    
  if(disc==FALSE){
    
    dir.create(paste0(dir_file3,"\\rules"))
    dir.create(paste0(dir_file3,"\\logs"))
    dir.create(paste0(dir_file3,"\\cuts"))
    dir.create(paste0(dir_file3,"\\rocs"))
    
    listOut=NULL
    
    #translating name of discretizations
    if(discMethod=="MDL")
    {
      discMethod="EntropyScaler"
    }
    
    if(discMethod=="Naive")
    {
      discMethod="NaiveScaler"
    }
    
    if(discMethod=="SemiNaive")
    {
      discMethod="SemiNaiveScaler"
    }
    
    if(discMethod=="EqualFrequency")
    {
      discMethod="EqualFrequencyScaler"
    }
    if(discMethod=="BROrthogonal")
    {
      discMethod="BROrthogonalScaler"
    }
    
    ##discretization methods and parameters
    listOut[1]=discMethod
    
    if(discMethod=="EntropyScaler" | discMethod=="NaiveScaler" | discMethod=="SemiNaiveScaler")
    {
    listOut[2]=paste0("{MODE=Save; MASK=",substr(as.character(discMask),1,1),"; FILENAME=",dir_file3,
                      "\\cuts\\cuts_",file_name,"_#ITERATION#.txt;}")
    }
    
    if(discMethod=="EqualFrequencyScaler")
    {
      listOut[2]=paste0("{MODE=Save; MASK=",substr(as.character(discMask),1,1),"; FILENAME=",dir_file3,
                        "\\cuts\\cuts_",file_name,"_#ITERATION#.txt; INTERVALS=",discParam,";}")
    }
    
    if(discMethod=="BROrthogonalScaler")
    {
      listOut[2]=paste0("{MODE=Save; MASK=",substr(as.character(discMask),1,1),"; FILENAME=",dir_file3,
                        "\\cuts\\cuts_",file_name,"_#ITERATION#.txt; APPROXIMATE=",substr(as.character(discParam[1]),1,1),"; FRACTION=",discParam[2],";}")
    }
    

    #ifs for reducers
    listOut[3]=ruleMeth
      if(ruleMeth=="JohnsonReducer"){
    listOut[4]=paste("{DISCERNIBILITY=",reducerDiscernibility,"; SELECTION=All; MODULO.DECISION=",substr(as.character(JohnsonParam[1]),1,1),"; BRT=",substr(as.character(JohnsonParam[2]),1,1),"; BRT.PRECISION=",JohnsonParam[3],"; IDG=",IDGlog,"; IDG.FILENAME=",
                     dir_file3,"\\",IDGfn,"; PRECOMPUTE=",substr(as.character(JohnsonParam[4]),1,1),"; APPROXIMATE=",substr(as.character(JohnsonParam[5]),1,1),"; FRACTION=",JohnsonParam[6],"}",sep="")
      }
     if(ruleMeth=="SAVGeneticReducer"){
       listOut[4]=paste("{DISCERNIBILITY=",reducerDiscernibility,"; SELECTION=All; MODULO.DECISION=",substr(as.character(GeneticParam[1]),1,1),"; BRT=",substr(as.character(GeneticParam[2]),1,1),"; BRT.PRECISION=",GeneticParam[3],"; IDG=",IDGlog,"; IDG.FILENAME=",
                        dir_file3,"\\",IDGfn,"; PRECOMPUTE=",substr(as.character(GeneticParam[4]),1,1),"; APPROXIMATE=",substr(as.character(GeneticParam[5]),1,1),"; FRACTION=",GeneticParam[6],"}",sep="")
     }
    if(ruleMeth=="Holte1RReducer"){
      listOut[4]=paste("{}",sep="")
    }
    if(ruleMeth=="ManualReducer"){
      listOut[4]=paste("{ATTRIBUTES=",paste(ManualNames,collapse=","),"}",sep="")
    }
    listOut[5]="RuleGenerator"
    listOut[6]="{}"
    
    if(ruleFiltration==T)
    {
    listOut[7]="MyRuleFilter"
    listOut[8]=paste0("{FILTERING = 1; SUPPORT.RHS.LOWER = ",ruleFiltrSupport[1],"; SUPPORT.RHS.UPPER = ",ruleFiltrSupport[2],
                      "; ACCURACY.RHS.LOWER = ",ruleFiltrAccuracy[1],"; ACCURACY.RHS.UPPER = ",ruleFiltrAccuracy[2],
                      "; COVERAGE.RHS.LOWER = ",ruleFiltrCoverage[1],"; COVERAGE.RHS.UPPER = ",ruleFiltrCoverage[2],
                      "; STABILITY.RHS.LOWER = ",ruleFiltrStability[1],"; STABILITY.RHS.UPPER = ",ruleFiltrStability[2],";}")
    }else
    {
      listOut[7]="MyRuleFilter"
      listOut[8]="{}"
    }

    
    
    listOut[9]="MyRuleExporter"
    listOut[10]=paste("{FILENAME=", dir_file3,"\\rules\\rules_", file_name,"_#ITERATION#.txt}", sep="")
    listOut[11]="OrthogonalFileScaler"
    listOut[12]=paste0("{MODE=Load; FILENAME =",paste0(dir_file3),"\\cuts\\cuts_",file_name,
                       "_#ITERATION#.txt}")
    listOut[13]="BatchClassifier"
    
    ### IF for IDG masking ###
    if(IDGlog)
    {
      IDGpath=paste0("; IDG.FILENAME=",dir_file3,"\\",IDGfn)
    }else{
      IDGpath=""
    }
    
    #######ifs for classifiers######
    
    if(classifier=="ObjectTrackingVoter")
    {
    listOut[14]=paste("{CLASSIFIER=",classifier,"; FRACTION=0.0; IDG=",substr(as.character(IDGlog),1,1),IDGpath,";"
                      ," FALLBACK=",substr(as.character(fallBack),1,1),"; FALLBACK.CLASS=",fallBackClass,"; MULTIPLE=Best; LOG=T; LOG.FILENAME=",
                      dir_file3,"\\logs\\log_",file_name,"_#ITERATION#.txt; LOG.VERBOSE=",
                      substr(as.character(LogVerb),1,1),"; CALIBRATION=F; ROC=",substr(as.character(roc),1,1),"; ROC.CLASS=",clroc,"; ROC.FILENAME=",dir_file3,"\\rocs\\roc_",file_name,"_#ITERATION#.txt}", sep="")
    }
    if(classifier=="StandardVoter")
    {
      listOut[14]=paste("{CLASSIFIER=",classifier,"; FRACTION=0.0; IDG=",substr(as.character(IDGlog),1,1),IDGpath,";"
                        ," SPECIFIC=F; VOTING=Support; NORMALIZATION=Firing; FALLBACK=",substr(as.character(fallBack),1,1),"; FALLBACK.CLASS=",fallBackClass,"; MULTIPLE=Best; LOG=T; LOG.FILENAME=",
                        dir_file3,"\\logs\\log_",file_name,"_#ITERATION#.txt; LOG.VERBOSE=",
                        substr(as.character(LogVerb),1,1),"; CALIBRATION=F; ROC=",substr(as.character(roc),1,1),"; ROC.CLASS=",clroc,"; ROC.FILENAME=",dir_file3,"\\rocs\\roc_",file_name,"_#ITERATION#.txt}", sep="")
      
    }
    if(classifier=="NaiveBayesClassifier")
    {
      listOut[14]=paste("{CLASSIFIER=",classifier,";"
                        ," FALLBACK=",substr(as.character(fallBack),1,1),"; FALLBACK.CLASS=",fallBackClass,"; MULTIPLE=Best; LOG=T; LOG.FILENAME=",
                        dir_file3,"\\logs\\log_",file_name,"_#ITERATION#.txt; LOG.VERBOSE=",
                        substr(as.character(LogVerb),1,1),"; CALIBRATION=F; ROC=",substr(as.character(roc),1,1),"; ROC.CLASS=",clroc,"; ROC.FILENAME=",dir_file3,"\\rocs\\roc_",file_name,"_#ITERATION#.txt}", sep="")
      
    }
     write.table(listOut,file=paste(dir_file3,"\\","OUT_cmdCV",".txt", sep=""),
              quote=F,col.names = F,row.names = F)
  }
  
  ######-----DISCRETIZATION OFF-----########################################
  ##########################################################################
  
    else{ 
    
    dir.create(paste0(dir_file3,"\\rules"))
    dir.create(paste0(dir_file3,"\\logs"))
    dir.create(paste0(dir_file3,"\\rocs"))
    
    listOut=NULL
    
    ### IF for IDG masking ###
    if(IDGlog)
    {
      IDGpath=paste0("; IDG.FILENAME=",dir_file3,"\\",IDGfn)
    }else{
      IDGpath=""
    }
     
     #if for reducers
     listOut[1]=ruleMeth
     
     if(ruleMeth=="JohnsonReducer"){
       listOut[2]=paste("{DISCERNIBILITY=",reducerDiscernibility,"; SELECTION=All; MODULO.DECISION=",substr(as.character(JohnsonParam[1]),1,1),"; BRT=",substr(as.character(JohnsonParam[2]),1,1),"; BRT.PRECISION=",JohnsonParam[3],"; IDG=",IDGlog,"; IDG.FILENAME=",
                        dir_file3,"\\",IDGfn,"; PRECOMPUTE=",substr(as.character(JohnsonParam[4]),1,1),"; APPROXIMATE=",substr(as.character(JohnsonParam[5]),1,1),"; FRACTION=",JohnsonParam[6],"}",sep="")
     }
     if(ruleMeth=="SAVGeneticReducer"){
       listOut[2]=paste("{DISCERNIBILITY=",reducerDiscernibility,"; SELECTION=All; MODULO.DECISION=",substr(as.character(GeneticParam[1]),1,1),"; BRT=",substr(as.character(GeneticParam[2]),1,1),"; BRT.PRECISION=",GeneticParam[3],"; IDG=",IDGlog,"; IDG.FILENAME=",
                        dir_file3,"\\",IDGfn,"; PRECOMPUTE=",substr(as.character(GeneticParam[4]),1,1),"; APPROXIMATE=",substr(as.character(GeneticParam[5]),1,1),"; FRACTION=",GeneticParam[6],"}",sep="")
     }
     if(ruleMeth=="Holte1RReducer"){
       listOut[2]=paste("{}",sep="")
     }
     if(ruleMeth=="ManualReducer"){
       listOut[2]=paste("{ATTRIBUTES=",paste(ManualNames,collapse=","),"}",sep="")
     }
     
     
          listOut[3]="RuleGenerator"
          listOut[4]="{}"
          
          if(ruleFiltration==T)
          {
            listOut[5]="MyRuleFilter"
            listOut[6]=paste0("{FILTERING = 1; SUPPORT.RHS.LOWER = ",ruleFiltrSupport[1],"; SUPPORT.RHS.UPPER = ",ruleFiltrSupport[2],
                              "; ACCURACY.RHS.LOWER = ",ruleFiltrAccuracy[1],"; ACCURACY.RHS.UPPER = ",ruleFiltrAccuracy[2],
                              "; COVERAGE.RHS.LOWER = ",ruleFiltrCoverage[1],"; COVERAGE.RHS.UPPER = ",ruleFiltrCoverage[2],
                              "; STABILITY.RHS.LOWER = ",ruleFiltrStability[1],"; STABILITY.RHS.UPPER = ",ruleFiltrStability[2],";}")
          }else
          {
            listOut[5]="MyRuleFilter"
            listOut[6]="{}"
          }      
          
          
          
          listOut[7]="MyRuleExporter"
          listOut[8]=paste("{FILENAME=", dir_file3,"\\rules\\rules_", file_name,"_#ITERATION#.txt}", sep="")
          listOut[9]="BatchClassifier"
          
          #######ifs for classifiers######
          
          if(classifier=="ObjectTrackingVoter")
          {
            listOut[10]=paste("{CLASSIFIER=",classifier,"; FRACTION=0.0; IDG=",substr(as.character(IDGlog),1,1),IDGpath,";"
                              ," FALLBACK=",substr(as.character(fallBack),1,1),"; FALLBACK.CLASS=",fallBackClass,"; MULTIPLE=Best; LOG=T; LOG.FILENAME=",
                              dir_file3,"\\logs\\log_",file_name,"_#ITERATION#.txt; LOG.VERBOSE=",
                              substr(as.character(LogVerb),1,1),"; CALIBRATION=F; ROC=",substr(as.character(roc),1,1),"; ROC.CLASS=",clroc,"; ROC.FILENAME=",dir_file3,"\\rocs\\roc_",file_name,"_#ITERATION#.txt}", sep="")
            
          }else
          {
          listOut[10]=paste("{CLASSIFIER=",classifier,"; FRACTION=0.0; IDG=",substr(as.character(IDGlog),1,1),IDGpath,";"
                            ," SPECIFIC=F; VOTING=Support; NORMALIZATION=Firing; FALLBACK=",substr(as.character(fallBack),1,1),"; FALLBACK.CLASS=",fallBackClass,
                            "; MULTIPLE=Best; LOG=T; LOG.FILENAME=",
                            dir_file3,"\\logs\\log_",file_name,"_#ITERATION#.txt; LOG.VERBOSE=",
                            substr(as.character(LogVerb),1,1),"; CALIBRATION=F; ROC=",substr(as.character(roc),1,1),"; ROC.CLASS=",clroc,"; ROC.FILENAME=",dir_file3,"\\rocs\\roc_",file_name,"_#ITERATION#.txt}", sep="")
          }
            write.table(listOut,file=paste(dir_file3,"\\","OUT_cmdCV",".txt", sep=""),
                quote=F,col.names = F,row.names = F)

  }
  
  }
}
