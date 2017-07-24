####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(varhandle)
library(survey)

wd <- "F:/Documents/Data/P20_2013/meta"
setwd(wd)

povcalcuts <- read.csv("headcounts.csv",as.is=TRUE)
povcalcuts$filename[which(povcalcuts$filename=="bdhr70dt")] <- "bdhr61dt"
povcalcuts$filename[which(povcalcuts$filename=="bfhr70dt")] <- "bfhr62dt"
povcalcuts$filename[which(povcalcuts$filename=="idhr63dt")] <- "idhr51dt"
povcalcuts$filename[which(povcalcuts$filename=="kehr7hdt")] <- "kehr70dt"
povcalcuts$filename[which(povcalcuts$filename=="mdhr6hdt")] <- "mdhr51dt"
povcalcuts$filename[which(povcalcuts$filename=="mwhr71dt")] <- "mwhr7hdt"
povcalcuts$filename[which(povcalcuts$filename=="phhr61dt")] <- "phhr52dt"
# povcalcuts$filename[which(povcalcuts$filename=="tzhr6adt")] <- "tzhr63dt"
povcalcuts$filename[which(povcalcuts$filename=="ughr72dt")] <- "ughr60dt"

weighted.percentile <- function(x,w,prob,na.rm=TRUE){
  df <- data.frame(x,w)
  if(na.rm){
    df <- df[which(complete.cases(df)),]
  }
  #Sort
  df <- df[order(df$x),]
  sumw <- sum(df$w)
  df$cumsumw <- cumsum(df$w)
  #For each percentile
  cutList <- c()
  cutNames <-c()
  for(i in 1:length(prob)){
    p <- prob[i]
    pStr <- paste0(round(p*100,digits=2),"%")
    sumwp <- sumw*p
    df$above.prob <- df$cumsumw>=sumwp
    thisCut <- df$x[which(df$above.prob==TRUE)[1]]
    cutList <- c(cutList,thisCut)
    cutNames <- c(cutNames,pStr)
  }
  names(cutList) <- cutNames
  return(cutList)
}

####Run function####
# set our working directory, change this if using on another machine
wd <- "F:/Documents/Data/DHSauto/"
setwd(wd)

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

dataList <- list()
dataIndex <- 1

# Loop through every dir
for(i in 2:length(dirs)){
  dir <- dirs[i]
  # Pull some coded info out of the dir name
  country <- tolower(substr(basename(dir),1,2))
  recode <- tolower(substr(basename(dir),3,4))
  phase <- as.integer(substr(basename(dir),5,5))
  # For this analysis, we're only interested in individual member recodes, or "hr"
  if(basename(dir) %in% povcalcuts$filename){
    message(basename(dir))
    hrwd <- dir
    if(!file_test(op="-d", hrwd)){next;}
    
    hrBase <- basename(hrwd)
    iso2 <- toupper(substr(hrBase,1,2))
    phase <- substr(hrBase,5,6)
    
    prwd <- paste0("F:/Documents/Data/DHSauto/",tolower(iso2),"pr",phase,"dt/")
    if(!file_test(op="-d", prwd)){next;}
    
    pr <- read.dta(paste0(prwd,iso2,"PR",phase,"FL.DTA"),convert.factors = FALSE)
    data.labels=data.frame(names(pr),attributes(pr)[7])
    
    names(pr)[which(names(pr)=="hv271")] <- "wealth"
    pr$wealth <- pr$wealth/100000
    
    #Rename sample.weights var
    names(pr)[which(names(pr)=="hv005")] <- "sample.weights"
    pr$weights <- pr$sample.weights/1000000
    
    #Rename urban var
    names(pr)[which(names(pr)=="hv025")] <- "urban.rural"
    
    #Rename educ var
    names(pr)[which(names(pr)=="hv109")] <- "educ"
    recode.educ <- function(x){
      if(is.na(x)){return(NA)}
      else if(tolower(x)=="dk" | tolower(x)=="don't know" | tolower(x)=="missing" | x==8 | x==9){return(NA)}
      else if(x==0 | x==1 | tolower(x)=="no education, preschool" | tolower(x)=="no education" | tolower(x)=="incomplete primary"){return("No education, preschool")}
      else if(x==2 | x==3 | tolower(x)=="complete primary" | tolower(x)=="incomplete secondary"){return("Primary")}
      else if(x==4 | tolower(x)=="complete secondary"){return("Secondary")}
      else if(x==5 | tolower(x)=="higher"){return("Higher")}
      else{return(NA)}
    }
    pr$educ <- sapply(pr$educ,recode.educ)
    
    #Rename age var
    names(pr)[which(names(pr)=="hv105")] <- "age"
    
    #Rename sex var
    names(pr)[which(names(pr)=="hv104")] <- "sex"
    
    #Rename cluster/hh var
    names(pr)[which(names(pr)=="hv001")] <- "cluster"
    names(pr)[which(names(pr)=="hv002")] <- "household"
    names(pr)[which(names(pr)=="hv024")] <- "region"
    names(pr)[which(names(pr)=="hvidx")] <- "line"
    #names(pr)[which(names(pr)=="hv112")] <- "mother.line"
    pr$mother.line=pr$hv112
    pr$mother.line[which(pr$mother.line==99)] <- NA
    
    #Head vars
    names(pr)[which(names(pr)=="hv219")] <- "head.sex"
    names(pr)[which(names(pr)=="hv220")] <- "head.age"
    
    #reg?
    names(pr)[which(names(pr)=="hv140")] <- "birth.cert"
    
    #nutrition
    names(pr)[which(names(pr)=="ha40")] <- "woman.bmi"
    if(typeof(pr$woman.bmi)!="NULL"){
      pr$woman.bmi <- pr$woman.bmi/100 
    }else{
      pr$woman.bmi <- NA
    }
    names(pr)[which(names(pr)=="hb40")] <- "man.bmi"
    if(typeof(pr$man.bmi)!="NULL"){
      pr$man.bmi <- pr$man.bmi/100 
    }else{
      pr$man.bmi <- NA
    }
    names(pr)[which(names(pr)=="hc1")] <- "age.months"
    names(pr)[which(names(pr)=="hc2")] <- "weight.kg"
    names(pr)[which(names(pr)=="hc3")] <- "height.cm"
    names(pr)[which(names(pr)=="hc15")] <- "standing.lying"
    names(pr)[which(names(pr)=="hc5")] <- "child.height.age"
    if(typeof(pr$child.height.age)=="NULL"){
      pr$child.height.age <- NA
    }else{
      pr$child.height.age <- pr$child.height.age/100
    }
    pr$child.weights <- pr$weights
    
    povcalcut <- subset(povcalcuts,filename==hrBase)$hc
    extcut <- subset(povcalcuts,filename==hrBase)$extreme
    cuts <- c(povcalcut,extcut)
    povperc <- weighted.percentile(pr$wealth,pr$weights,prob=cuts)
    
    pr$p20 <- (pr$wealth < povperc[1])
    pr$ext <- (pr$wealth < povperc[2])
    
    mothers <- unique(pr[c("cluster","household","line","woman.bmi")])
    mothers <- mothers[complete.cases(mothers),]
    names(mothers) <- c("cluster","household","mother.line","mother.bmi")
    pr <- join(
      pr
      ,mothers
      ,by=c("cluster","household","mother.line")
    )
    ##Adding DHS code on orphanhood and living without parents http://userforum.dhsprogram.com/index.php?t=msg&goto=8904&&srch=orphan#msg_8904
    # * Construct the recodes used in "Children's living arrangements and orphanhood"
    # 
    # * orphanhood typology
    # tab hv111 hv113
    pr$hv111[which(pr$hv111>=8)]=NA
    pr$hv113[which(pr$hv113>=8)]=NA
    table(pr$hv111,pr$hv113)
    # gen orphan_type=.
    pr$orphantype=NA
    # replace orphan_type=1 if hv111==1 & hv113==1
    pr$orphantype[which(pr$hv111==1 & pr$hv113==1)] =1
    # replace orphan_type=2 if hv111==1 & hv113==0
    pr$orphantype[which(pr$hv111==1 & pr$hv113==0)] =2
    # replace orphan_type=3 if hv111==0 & hv113==1
    pr$orphantype[which(pr$hv111==0 & pr$hv113==1)] =3
    # replace orphan_type=4 if hv111==0 & hv113==0
    pr$orphantype[which(pr$hv111==0 & pr$hv113==0)] =4
    # replace orphan_type=5 if (hv111>1 & hv111<.) | (hv113>1 & hv113<.)
    pr$orphantype[which((pr$hv111>1 & !is.na(pr$hv111)) | (pr$hv113>1 & !is.na(pr$hv113)))] =5
    # 
    # * coresidence typology
    table(pr$hv112,pr$hv114)
    # tab1 hv112 hv114
    pr$cores_type=NA
    # gen cores_type=.
    # 
    # * hv112r and hv114r identify parents who are in the household AND are de jure residents 
    # 
    # gen hv112r=0
    pr$hv112r=0
    # replace hv112r=1 if hv112>0 & hv112<98
    pr$hv112r[which(pr$hv112>0 & pr$hv112<98)]=1
    # gen hv114r=0
    pr$hv114r=0
    # replace hv114r=1 if hv114>0 & hv114<98
    pr$hv114r[which(pr$hv114>0 & pr$hv114<98)]=1
    # 
    # replace cores_type=1 if hv112r==1 & hv114r==1
    pr$cores_type[which(pr$hv112r==1 & pr$hv114r==1)]=1
    # replace cores_type=2 if hv112r==1 & hv114r==0
    pr$cores_type[which(pr$hv112r==1 & pr$hv114r==0)]=2
    # replace cores_type=3 if hv112r==0 & hv114r==1
    pr$cores_type[which(pr$hv112r==0 & pr$hv114r==1)]=3
    # replace cores_type=4 if hv112r==0 & hv114r==0
    pr$cores_type[which(pr$hv112r==0 & pr$hv114r==0)]=4
    # 
    # * combined orphanhood and coresidence typology
    # tab orphan_type cores_type
    table(pr$orphantype,pr$cores_type)
    # gen orphan_cores_type=.
    pr$orphan_cores_type=NA
    # replace orphan_cores_type= 1 if orphan_type==1 & cores_type==1
   pr$orphan_cores_type[which(pr$orphantype==1 & pr$cores_type==1)]=1
    # replace orphan_cores_type= 2 if orphan_type==1 & cores_type==2
    pr$orphan_cores_type[which(pr$orphantype==1 & pr$cores_type==2)]=2
    # replace orphan_cores_type= 3 if orphan_type==2 & cores_type==2
    pr$orphan_cores_type[which(pr$orphantype==2 & pr$cores_type==2)]=3
    # replace orphan_cores_type= 4 if orphan_type==1 & cores_type==3
    pr$orphan_cores_type[which(pr$orphantype==1 & pr$cores_type==3)]=4
    # replace orphan_cores_type= 5 if orphan_type==3 & cores_type==3
    pr$orphan_cores_type[which(pr$orphantype==3 & pr$cores_type==3)]=5
    # replace orphan_cores_type= 6 if orphan_type==1 & cores_type==4
    pr$orphan_cores_type[which(pr$orphantype==1 & pr$cores_type==4)]=6
    # replace orphan_cores_type= 7 if orphan_type==3 & cores_type==4
    pr$orphan_cores_type[which(pr$orphantype==3 & pr$cores_type==4)]=7
    # replace orphan_cores_type= 8 if orphan_type==2 & cores_type==4
    pr$orphan_cores_type[which(pr$orphantype==2 & pr$cores_type==4)]=8
    # replace orphan_cores_type= 9 if orphan_type==4 & cores_type==4
    pr$orphan_cores_type[which(pr$orphantype==4 & pr$cores_type==4)]=9
    # replace orphan_cores_type=10 if orphan_type==5
    pr$orphan_cores_type[which(pr$orphantype==5)]=10
    # 
    # #delimit ;
    # label define orphan_type 1 "Both parents alive" 2 "Mother alive, father dead" 
    # 3 "Father alive, mother dead" 4 "Both parents dead" 5 "Info missing";
    # 
    # label define cores_type 1 "Living with both parents" 2 "With mother, not father" 
    # 3 "With father, not mother" 4 "Living with neither parent";
    # 
    # label define orphan_cores_type 1 "With both parents" 2 "With mother only, father alive" 
    # 3 "With mother only, father dead" 4 "With father only, mother alive" 
    # 5 "With father only, mother dead" 6 "With neither, both alive" 
    # 7 "With neither, only father alive" 8 "With neither, only mother alive" 
    # 9 "With neither, both dead" 10 "Survival info missing"; 
    # #delimit cr
    # 
    # label values orphan_type orphan_type
    # label values cores_type cores_type
    # label values orphan_cores_type orphan_cores_type
    # 
    # * not living with a biological parent
    # gen with_neither_parent=0
    pr$with_neither_parent=0
    # replace with_neither_parent=1 if orphan_cores_type>=6 & orphan_cores_type<=9
    pr$with_neither_parent[which(pr$orphan_cores_type>=6 & pr$orphan_cores_type<=9)]=1
    # 
    # * one or both parents dead
    # * note that the allocation of hv111 and hv113 codes other than 0 and 1 is
    # * slightly different in the table than in "Info missing" above
    # gen one_or_both_parents_dead=0
    pr$one_or_both_parents_dead=0
    # replace one_or_both_parents_dead=1 if hv111==0 | hv113==0
    pr$one_or_both_parents_dead[which(pr$hv111==0 | pr$hv113==0)]=1
    # 
    # * unweighted distributions
    # tab orphan_cores_type
    # table(pr$orphan_cores_type)
    # # tab with_neither_parent
    # table(pr$with_neither_parent)
    # # tab one_or_both_parents_dead
    # table(pr$one_or_both_parents_dead)
    # prdesign = svydesign(ids= pr$cluster, data= pr, weights=pr$weights )
    # ##Making Orphanages
    hrOrphanage = data.table(pr)[,.( 
      orphanage= (sum(with_neither_parent, na.rm=TRUE)>=10)
      ),by=.(cluster,household)]
    pr = merge(pr,hrOrphanage)
    # 
    # * weighted distributions
    # # tab orphan_cores_type [iweight=hv005/1000000]
    # prop.table(svytable(~orphan_cores_type,design=prdesign))
    #   # tab with_neither_parent [iweight=hv005/1000000]
    # prop.table(svytable(~with_neither_parent,design=prdesign))
    # # tab one_or_both_parents_dead [iweight=hv005/1000000]
    # prop.table(svytable(~one_or_both_parents_dead,design=prdesign))
    # 
    # chdesign = svydesign(ids= children$cluster, data= children, weights=children$weights )
    # # 
    # # * weighted distributions
    # # tab orphan_cores_type [iweight=hv005/1000000]
    # prop.table(svytable(~orphan_cores_type,design=chdesign))
    # # tab with_neither_parent [iweight=hv005/1000000]
    # prop.table(svytable(~with_neither_parent,design=chdesign))
    # # tab one_or_both_parents_dead [iweight=hv005/1000000]
    # prop.table(svytable(~one_or_both_parents_dead,design=chdesign))
    # 
    
    keep <- c("wealth","weights","urban.rural","region","educ","age","sex","cluster","household","head.sex","head.age","p20"
              ,"birth.cert","birth.reg","age.months","weight.kg","height.cm","standing.lying","child.height.age"
              ,"woman.bmi","man.bmi","child.weights","mother.bmi","ext","with_neither_parent", "one_or_both_parents_dead", "orphan_cores_type", "orphanage"
    )
    prNames <- names(pr)
    namesDiff <- setdiff(keep,prNames)
    if(length(namesDiff)>0){
      for(y in 1:length(namesDiff)){
        pr[namesDiff[y]] <- NA
        message(paste("Missing variable",namesDiff[y]))
      } 
    }
    data <- pr[keep]
    data$filename <- hrBase
    dataList[[dataIndex]] <- data
    dataIndex <- dataIndex + 1
  }
}
# 
# setwd("D:/Documents/Data/MICSmeta")
# varNames <- read.csv("mics_meta_vars_complete.csv",as.is=TRUE,na.strings="")
# classes <- read.csv("global_mics_classes.csv",as.is=TRUE,na.strings="NAN")
# 
# wd <- "D:/Documents/Data/MICSauto/"
# setwd(wd)
# 
# # List out all the directories in our wd, this is where our data is contained
# dirs <- list.dirs(wd,full.names=TRUE)
# 
# # dir <- "D:/Documents/Data/MICSauto/Somalia MICS 2006 SPSS Datasets"
# # dir <- "D:/Documents/Data/MICSauto/Algeria_MICS4_Datasets"
# # dir <- "D:/Documents/Data/MICSauto/Zimbabwe_MICS5_Datasets"
# 
# for(i in 2:length(dirs)){
#   dir <- dirs[i]
#   hrBase <- basename(dir)
#   if(hrBase %in% povcalcuts$filename){
#     
#     message(hrBase) 
#     if(exists("hh")){rm(hh)}
#     if(exists("hl")){rm(hl)}
#     if(exists("ch")){rm(ch)}
#     if(exists("wm")){rm(wm)}
#     load(paste0(dir,"/","hh.RData"))
#     load(paste0(dir,"/","hl.RData"))
#     load(paste0(dir,"/","ch.RData"))
#     load(paste0(dir,"/","wm.RData"))
#     hh <- data.frame(hh,as.is=TRUE,check.names=FALSE)
#     hl <- data.frame(hl,as.is=TRUE,check.names=FALSE)
#     ch <- data.frame(ch,as.is=TRUE,check.names=FALSE)
#     wm <- data.frame(wm,as.is=TRUE,check.names=FALSE)
#     names(hh) <- tolower(names(hh))
#     names(hl) <- tolower(names(hl))
#     names(ch) <- tolower(names(ch))
#     names(wm) <- tolower(names(wm))
#     
#     file.varName <- subset(varNames,filename==hrBase)
#     
#     attendedVar <- subset(file.varName,match=="attended")$varName
#     gradeVar <- subset(file.varName,match=="grade")$varName
#     schoolVar <- subset(file.varName,match=="school")$varName
#     
#     ynm.classes <- subset(classes,filename==hrBase & type=="ynm")
#     attended.classes <- subset(classes,filename==hrBase & type=="attended")
#     urban.rural.classes <- subset(classes,filename==hrBase & type=="urban.rural")
#     school.classes <- subset(classes,filename==hrBase & type=="school")
#     
#     missing.vals <- subset(ynm.classes,is.na(ynm))$value
#     no.vals <- subset(ynm.classes,ynm==0)$value
#     yes.vals <- subset(ynm.classes,ynm==1)$value
#     
#     missing.attended <- subset(attended.classes,is.na(attended))$value
#     no.attended <- subset(attended.classes,attended==0)$value
#     yes.attended <- subset(attended.classes,attended==1)$value
#     
#     missing.level <- subset(school.classes,is.na(level))$value
#     none.level <- subset(school.classes,level=="none")$value
#     preschool.level <- subset(school.classes,level=="preschool")$value
#     primary.level <- subset(school.classes,level=="primary")$value
#     secondary.level <- subset(school.classes,level=="secondary")$value
#     higher.level <- subset(school.classes,level=="higher")$value
#     
#     #Rename wealth var
#     if(typeof(hh$wlthscor)=="NULL" | typeof(hh$wlthscor)=="logical" | length(hh$wlthscor[which(!is.na(hh$wlthscor))])==0){
#       if(typeof(hh$wscore)=="NULL" | typeof(hh$wscore)=="logical" | length(hh$wscore[which(!is.na(hh$wscore))])==0){
#         message("Wealth missing!");return(NA)
#       }else{
#         names(hh)[which(names(hh)=="wscore")] <- "wealth"
#       }
#     }else{
#       names(hh)[which(names(hh)=="wlthscor")] <- "wealth"
#     }
#     
#     #Rename sample.weights var
#     names(hh)[which(names(hh)=="hhweight")] <- "weights"
#     
#     #Rename urban var
#     names(hh)[which(names(hh)=="hh6")] <- "urban.rural"
#     if(typeof(hh$urban.rural)=="NULL"){message("No urban.rural!");hh$urban.rural<-NA;urban.missing<-TRUE}else{urban.missing<-FALSE}
#     
#     #Rename educ var
#     names(hl)[which(names(hl)==attendedVar)] <- "attended"
#     names(hl)[which(names(hl)==schoolVar)] <- "school"
#     names(hl)[which(names(hl)==gradeVar)] <- "grade"
#     
#     #Rename age var
#     names(hl)[which(names(hl)=="hl6")] <- "age"
#     
#     #Rename sex var
#     names(hl)[which(names(hl)=="hl4")] <- "sex"
#     
#     #Rename head var
#     hl$head <- tolower(substr(hl$hl3,1,4)) %in% c("chef","head")
#     
#     #Rename child vars
#     names(ch)[which(names(ch)=="br1")] <- "birth.cert"
#     names(ch)[which(names(ch)=="br2")] <- "birth.reg"
#     names(ch)[which(names(ch)=="cage")] <- "age.months"
#     names(ch)[which(names(ch)=="chweight")] <- "child.weights"
#     names(ch)[which(names(ch)=="an3")] <- "weight.kg"
#     names(ch)[which(names(ch)=="an4a")] <- "standing.lying"
#     names(ch)[which(names(ch)=="haz2")] <- "child.height.age"
#     
#     #code female bmi
#     if(typeof(wm$anw4)!="NULL" & typeof(wm$anw5)!="NULL"){
#       wm$anw4[which(wm$anw4==99.9)] <- NA
#       wm$anw5[which(wm$anw5==999.9)] <- NA
#       wm$anw5 <- wm$anw5/100
#       wm$woman.bmi <- wm$anw4/(wm$anw5*wm$anw5) 
#     }
#     
#     #Rename cluster/hh var
#     names(hl)[which(names(hl)=="hh1")] <- "cluster"
#     names(hl)[which(names(hl)=="hh2")] <- "household"
#     names(hl)[which(names(hl)=="hl1")] <- "line"
#     names(hl)[which(names(hl)=="ln")] <- "line"
#     names(hh)[which(names(hh)=="hh1")] <- "cluster"
#     names(hh)[which(names(hh)=="hh2")] <- "household"
#     names(hh)[which(names(hh)=="hh7")] <- "region"
#     names(ch)[which(names(ch)=="hh1")] <- "cluster"
#     names(ch)[which(names(ch)=="hh2")] <- "household"
#     names(ch)[which(names(ch)=="ln")] <- "line"
#     names(ch)[which(names(ch)=="uf6")] <- "mother.line"
#     names(wm)[which(names(wm)=="hh1")] <- "cluster"
#     names(wm)[which(names(wm)=="hh2")] <- "household"
#     names(wm)[which(names(wm)=="ln")] <- "line"
#     
#     recode.educ <- function(attendedV,schoolV,gradeV){
#       educV <- c()
#       for(i in 1:length(attendedV)){
#         attended <- tolower(attendedV[i])
#         school <- tolower(schoolV[i])
#         if(length(school)<=0){
#           school <- NA
#         }
#         grade <- gradeV[i]
#         ###Ignore factor grades for now... We need to code these out in the metavars
#         if(is.factor(grade)){
#           grade <- NA
#         }
#         if(!is.na(grade)){
#           if(grade>90){grade<-NA}
#         }
#         if(attended %in% missing.attended){
#           if(school %in% missing.level){
#             if(is.na(grade)){
#               #missing all three
#               educ <- NA
#             }else{
#               #missing attended and level, but not grade
#               if(grade>=5 & grade<7){
#                 educ <- 1
#               }else if(grade>=7 & grade<9){
#                 educ <- 2
#               }else if(grade>9){
#                 educ <- 3
#               }else{
#                 educ <- 0
#               }
#             }
#           }else{
#             #missing attended, but not level
#             if(is.na(grade)){
#               #has level, but not grade
#               if(school %in% preschool.level | school %in% none.level){
#                 educ <- 0
#               }else if(school %in% primary.level){
#                 educ <- 1
#               }else if(school %in% secondary.level){
#                 educ <- 2
#               }else if(school %in% higher.level){
#                 educ <- 3
#               }else{
#                 educ <- NA
#               }
#             }else{
#               #missing attended and level, but not grade
#               if(grade>=5 & grade<7){
#                 educ <- 1
#               }else if(grade>=7 & grade<9){
#                 educ <- 2
#               }else if(grade>9){
#                 educ <- 3
#               }else{
#                 educ <- 0
#               }
#             }
#           }
#         }else if(attended %in% no.attended){
#           #No education
#           educ <- 0
#         }else{
#           if(school %in% missing.level){
#             if(is.na(grade)){
#               #has attended, but has no level or grade
#               educ <- NA
#             }else{
#               #has attended, missing level, but not missing grade
#               if(grade>=5 & grade<7){
#                 educ <- 1
#               }else if(grade>=7 & grade<9){
#                 educ <- 2
#               }else if(grade>9){
#                 educ <- 3
#               }else{
#                 educ <- 0
#               }
#             }
#           }else if(school %in% preschool.level | school %in% none.level){
#             if(is.na(grade)){
#               educ <- 0
#             }else if(grade>=5){
#               #Complete primary
#               educ <- 1
#             }else{
#               educ <- 0
#             }
#           } else if(school %in% primary.level){
#             if(is.na(grade)){
#               educ <- 0
#             }else if(grade<5){
#               #Incomplete primary
#               educ <- 0
#             }else if(grade>=5){
#               #Complete primary
#               educ <- 1
#             }else{
#               educ <- NA
#             }
#           } else if(school %in% secondary.level){
#             #(in)complete secondary
#             educ <- 2
#           } else if(school %in% higher.level){
#             #(in)complete higher
#             educ <- 3
#           }else if(grade>=5 & grade<7){
#             educ <- 1
#           }else if(grade>=7 & grade<9){
#             educ <- 2
#           }else if(grade>9){
#             educ <- 3
#           }else if(grade<5){
#             #not at least 5 years of some other schooling
#             educ <- 0
#           } else{
#             #missing grade with preschool, primary, or other
#             educ <- NA
#           }
#         }
#         educV <- c(educV,educ)
#       }
#       return(educV)
#     }
#     
#     hl$educ <- recode.educ(hl$attended,hl$school,hl$grade)
#     
#     head <- subset(hl,head==1)
#     names(head)[which(names(head)=="sex")] <- "head.sex"
#     names(head)[which(names(head)=="age")] <- "head.age"
#     keep <- c("cluster","household","head.sex","head.age")
#     head <- head[keep]
#     hh <- join(
#       hh
#       ,head
#       ,by=c("cluster","household")
#     )
#     
#     recode.urban.rural <- function(x){
#       item <- subset(urban.rural.classes,value==tolower(x))
#       if(nrow(item)==0){return(NA)}
#       else{item$urban[1]}
#     }
#     hh$urban.rural <- sapply(hh$urban.rural,recode.urban.rural)
#     
#     povcalcut <- subset(povcalcuts,filename==hrBase)$hc
#     np20cut <- 0.2
#     nplcut <- subset(povcalcuts,filename==hrBase)$pl.hc
#     extcut <- subset(povcalcuts,filename==hrBase)$extreme
#     cuts <- c(povcalcut,np20cut,nplcut,extcut)
#     povperc <- weighted.percentile(hh$wealth,hh$weights,prob=cuts)
#     
#     hh$p20 <- (hh$wealth < povperc[1])
#     hh$np20 <- (hh$wealth < povperc[2])
#     hh$npl <- (hh$wealth < povperc[3])
#     hh$ext <- (hh$wealth < povperc[4])
#     
#     wmkeep <- c("household","cluster","line","woman.bmi")
#     wmNames <- names(wm)
#     namesDiff <- setdiff(wmkeep,wmNames)
#     if(length(namesDiff)>0){
#       for(y in 1:length(namesDiff)){
#         wm[namesDiff[y]] <- NA
#         message(paste("Missing variable",namesDiff[y]))
#       } 
#     }
#     wm <- wm[wmkeep]
#     
#     hl <- join(
#       hl
#       ,wm
#       ,by=c("cluster","household","line")
#     )
#     
#     names(wm) <- c("household","cluster","mother.line","mother.bmi")
#     
#     ch <- join(
#       ch
#       ,wm
#       ,by=c("cluster","household","mother.line")
#     )
#     
#     chkeep <- c("household","cluster","line","birth.cert","birth.reg","age.months","child.weights","weight.kg","standing.lying"
#                 ,"child.height.age","mother.bmi")
#     chNames <- names(ch)
#     namesDiff <- setdiff(chkeep,chNames)
#     if(length(namesDiff)>0){
#       for(y in 1:length(namesDiff)){
#         ch[namesDiff[y]] <- NA
#         message(paste("Missing variable",namesDiff[y]))
#       } 
#     }
#     ch <- ch[chkeep]
#     
#     hl <- join(
#       hl
#       ,ch
#       ,by=c("cluster","household","line")
#     )
#     
#     
#     hhkeep <- c("wealth","weights","urban.rural","region","cluster","household","head.sex","head.age","p20","np20","npl","ext")
#     hhNames <- names(hh)
#     namesDiff <- setdiff(hhkeep,hhNames)
#     if(length(namesDiff)>0){
#       for(y in 1:length(namesDiff)){
#         hh[namesDiff[y]] <- NA
#         message(paste("Missing variable",namesDiff[y]))
#       } 
#     }
#     hh <- hh[hhkeep]
#     hl <- join(
#       hl
#       ,hh
#       ,by=c("cluster","household")
#     )
#     hl <- data.frame(hl,as.is=TRUE,check.names=FALSE)
#     keep <- c("wealth","weights","urban.rural","region","educ","age","sex","cluster","household","head.sex","head.age","p20"
#               ,"birth.cert","birth.reg","age.months","weight.kg","height.cm","standing.lying","child.height.age"
#               ,"woman.bmi","man.bmi","child.weights","mother.bmi","np20","npl","ext"
#     )
#     hlNames <- names(hl)
#     namesDiff <- setdiff(keep,hlNames)
#     if(length(namesDiff)>0){
#       for(y in 1:length(namesDiff)){
#         hl[namesDiff[y]] <- NA
#         message(paste("Missing variable",namesDiff[y]))
#       } 
#     }
#     hl <- hl[keep]
#     hl$filename <- hrBase
#     dataList[[dataIndex]] <- hl
#     dataIndex <- dataIndex + 1
#   }
# }

wd <- "F:/Documents/Data/MICSmeta"
setwd(wd)

data.total <- rbindlist(dataList)

recode.urban <- function(x){
  if(is.na(x)){return(NA)}
  else if(x==0 | tolower(x)=="rural"){return(0)}
  else if(x==1 | tolower(x)=="urban"){return(1)}
  else{return(NA)}
}
data.total$urban <- sapply(data.total$urban.rural,recode.urban)
# 
# recode.educ <- function(x){
#   if(is.na(x)){return(NA)}
#   else if(tolower(x)=="dk" | tolower(x)=="don't know"){return(NA)}
#   else if(x==0 | tolower(x)=="no education, preschool"){return("No education, preschool")}
#   else if(x==1 | tolower(x)=="primary"){return("Primary")}
#   else if(x==2 | tolower(x)=="secondary"){return("Secondary")}
#   else if(x==3 | tolower(x)=="higher"){return("Higher")}
#   else{return(NA)}
# }
# data.total$educ <- sapply(data.total$educ,recode.educ)
# data.total$educ <- factor(data.total$educ
#                           ,levels = c("No education, preschool","Primary","Secondary","Higher")
# )

# codeAgeCat <- function(x){
#   startAge <- 0
#   ageDiff <- 4
#   endAge <- 4
#   if(is.na(x)){
#     return("missing")
#   }
#   while(startAge<95){
#     endAge <- startAge+ageDiff
#     if(x>=startAge & x<=endAge){
#       return(
#         paste0(startAge,"-",endAge)  
#       )
#     }
#     startAge <- endAge + 1
#   }
#   if(x>=95){
#     return("95+")
#   }
#   return("missing")
# }

# data.total$ageCategory <- vapply(data.total$age,codeAgeCat,character(1))
# data.total$ageCategory <- factor(data.total$ageCategory,
#                                  levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
#                                             ,"35-39","40-44","45-49","50-54","55-59","60-64"
#                                             ,"65-69","70-74","75-79","80-84","85-89","90-94"
#                                             ,"95+","missing")                          
# )
# 
# data.total$head.ageCategory <- vapply(data.total$head.age,codeAgeCat,character(1))
# data.total$head.ageCategory <- factor(data.total$head.ageCategory,
#                                       levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
#                                                  ,"35-39","40-44","45-49","50-54","55-59","60-64"
#                                                  ,"65-69","70-74","75-79","80-84","85-89","90-94"
#                                                  ,"95+","missing")                          
# )

sex.missing = c(NA,"missing",9)
sex.male = c(1,"male","masculin","hombre")
sex.female = c(2, "female","feminin","mujer")
data.total$sex[which(tolower(data.total$sex) %in% sex.missing)] <- NA
data.total$sex[which(tolower(data.total$sex) %in% sex.male)] <- "Male"
data.total$sex[which(tolower(data.total$sex) %in% sex.female)] <- "Female"
data.total$head.sex[which(tolower(data.total$head.sex) %in% sex.missing)] <- NA
data.total$head.sex[which(tolower(data.total$head.sex) %in% sex.male)] <- "Male"
data.total$head.sex[which(tolower(data.total$head.sex) %in% sex.female)] <- "Female"

data.total$birth.reg.coded = NA
data.total$birth.reg.coded[which(data.total$birth.cert==0)]=0
data.total$birth.reg.coded[which(data.total$birth.cert==1 | data.total$birth.cert==2 )]=1
#0 - neither certificate or registered
#1 - has certificate
#2 - registered
#8 - dk
# birth.cert.missing <- c(NA,"dk","don't know",8,9,"missing","nsp","manquant","no sabe")
# birth.cert.no <- c("registered",0,2,"neither certificate or registered","no","non","has only hospital card")
# birth.cert.yes <- setdiff(unique(tolower(data.total$birth.cert)),c(birth.cert.no,birth.cert.missing))
# 
# birth.reg.missing <- c(NA,"dk","missing","nsp","manquant")
# birth.reg.no <- c("no","non")
# birth.reg.yes <- c("yes","oui")
# #count registrations if birth.cert var reveals it to be so
# birth.cert.registered <- c(2,"registered","has only hospital card",birth.cert.yes)
# birth.cert.not.registered <- c(0,"neither certificate or registered","no","non")
# data.total$birth.reg.coded <- unfactor(data.total$birth.reg)
# data.total$birth.reg.coded[which(is.na(data.total$birth.reg.coded) & tolower(data.total$birth.cert) %in% birth.cert.registered)] <- "Yes"
# data.total$birth.reg.coded[which(is.na(data.total$birth.reg.coded) & tolower(data.total$birth.cert) %in% birth.cert.not.registered)] <- "No"
# data.total$birth.reg.coded[which(is.na(data.total$birth.reg.coded) & grepl("visto",data.total$birth.cert))] <- "Yes"
# 
# data.total$birth.reg.coded[which(tolower(data.total$birth.reg.coded) %in% birth.reg.missing)] <- NA
# data.total$birth.reg.coded[which(tolower(data.total$birth.reg.coded) %in% birth.reg.no)] <- 0
# data.total$birth.reg.coded[which(tolower(data.total$birth.reg.coded) %in% birth.reg.yes)] <- 1
# data.total$birth.reg.coded[which(substr(data.total$birth.reg.coded,1,1)=="S")] <- 1
# 
# data.total$birth.reg <- data.total$birth.reg.coded
# 
# data.total$birth.cert <- unfactor(data.total$birth.cert)
# data.total$birth.cert[which(tolower(data.total$birth.cert) %in% birth.cert.missing)] <- NA
# data.total$birth.cert[which(tolower(data.total$birth.cert) %in% birth.cert.no)] <- 0
# data.total$birth.cert[which(tolower(data.total$birth.cert) %in% birth.cert.yes)] <- 1
# data.total$birth.cert[which(grepl("visto",data.total$birth.cert))] <- 1

data.total$woman.bmi[which(data.total$woman.bmi>80)] <- NA
data.total$woman.bmi.class <- NA
data.total$woman.bmi.class[which(data.total$woman.bmi<16)] <- "Severe thinness"
data.total$woman.bmi.class[which(data.total$woman.bmi>=16 & data.total$woman.bmi<17)] <- "Moderate thinness"
data.total$woman.bmi.class[which(data.total$woman.bmi>=17 & data.total$woman.bmi<18.5)] <- "Mild thinness"
data.total$woman.bmi.class[which(data.total$woman.bmi>=18.5 & data.total$woman.bmi<25)] <- "Normal range"
data.total$woman.bmi.class[which(data.total$woman.bmi>=25 & data.total$woman.bmi<30)] <- "Pre-obese"
data.total$woman.bmi.class[which(data.total$woman.bmi>=30 & data.total$woman.bmi<35)] <- "Obese class I"
data.total$woman.bmi.class[which(data.total$woman.bmi>=35 & data.total$woman.bmi<40)] <- "Obese class II"
data.total$woman.bmi.class[which(data.total$woman.bmi>=40)] <- "Obese class III"

data.total$woman.bmi.class <- factor(data.total$woman.bmi.class
                                     ,levels=c(
                                       "Severe thinness"
                                       ,"Moderate thinness"
                                       ,"Mild thinness"
                                       ,"Normal range"
                                       ,"Pre-obese"
                                       ,"Obese class I"
                                       ,"Obese class II"
                                       ,"Obese class III"
                                     ))

data.total$man.bmi[which(data.total$man.bmi>80)] <- NA
data.total$man.bmi.class <- NA
data.total$man.bmi.class[which(data.total$man.bmi<16)] <- "Severe thinness"
data.total$man.bmi.class[which(data.total$man.bmi>=16 & data.total$man.bmi<17)] <- "Moderate thinness"
data.total$man.bmi.class[which(data.total$man.bmi>=17 & data.total$man.bmi<18.5)] <- "Mild thinness"
data.total$man.bmi.class[which(data.total$man.bmi>=18.5 & data.total$man.bmi<25)] <- "Normal range"
data.total$man.bmi.class[which(data.total$man.bmi>=25 & data.total$man.bmi<30)] <- "Pre-obese"
data.total$man.bmi.class[which(data.total$man.bmi>=30 & data.total$man.bmi<35)] <- "Obese class I"
data.total$man.bmi.class[which(data.total$man.bmi>=35 & data.total$man.bmi<40)] <- "Obese class II"
data.total$man.bmi.class[which(data.total$man.bmi>=40)] <- "Obese class III"

data.total$man.bmi.class <- factor(data.total$man.bmi.class
                                   ,levels=c(
                                     "Severe thinness"
                                     ,"Moderate thinness"
                                     ,"Mild thinness"
                                     ,"Normal range"
                                     ,"Pre-obese"
                                     ,"Obese class I"
                                     ,"Obese class II"
                                     ,"Obese class III"
                                   ))

data.total$mother.bmi[which(data.total$mother.bmi>80)] <- NA
data.total$mother.bmi.class <- NA
data.total$mother.bmi.class[which(data.total$mother.bmi<16)] <- "Severe thinness"
data.total$mother.bmi.class[which(data.total$mother.bmi>=16 & data.total$mother.bmi<17)] <- "Moderate thinness"
data.total$mother.bmi.class[which(data.total$mother.bmi>=17 & data.total$mother.bmi<18.5)] <- "Mild thinness"
data.total$mother.bmi.class[which(data.total$mother.bmi>=18.5 & data.total$mother.bmi<25)] <- "Normal range"
data.total$mother.bmi.class[which(data.total$mother.bmi>=25 & data.total$mother.bmi<30)] <- "Pre-obese"
data.total$mother.bmi.class[which(data.total$mother.bmi>=30 & data.total$mother.bmi<35)] <- "Obese class I"
data.total$mother.bmi.class[which(data.total$mother.bmi>=35 & data.total$mother.bmi<40)] <- "Obese class II"
data.total$mother.bmi.class[which(data.total$mother.bmi>=40)] <- "Obese class III"

data.total$mother.bmi.class <- factor(data.total$mother.bmi.class
                                      ,levels=c(
                                        "Severe thinness"
                                        ,"Moderate thinness"
                                        ,"Mild thinness"
                                        ,"Normal range"
                                        ,"Pre-obese"
                                        ,"Obese class I"
                                        ,"Obese class II"
                                        ,"Obese class III"
                                      ))

data.total$child.height.age[which(data.total$child.height.age>80)] <- NA
data.total$stunting <- NA
# data.total$stunting[which(data.total$child.height.age<= (-6))] <- "Implausibly low"
data.total$stunting[which(data.total$child.height.age > (-6) & data.total$child.height.age<= (-3))] <- "Severely stunted"
data.total$stunting[which(data.total$child.height.age > (-3) & data.total$child.height.age<= (-2))] <- "Stunted, but not severely"
data.total$stunting[which(data.total$child.height.age > (-2) & data.total$child.height.age< (6))] <- "Not stunted"
# data.total$stunting[which(data.total$child.height.age>= (6))] <- "Implausibly high"

# data.total$stunting <- factor(data.total$stunting
#                               ,levels=c(
#                                 "Implausibly low"
#                                 ,"Severely stunted"
#                                 ,"Stunted, but not severely"
#                                 ,"Not stunted"
#                                 ,"Implausibly high"
#                               ))
data.total$stunted = NA
data.total$stunted[which(data.total$stunting=="Severely stunted" | data.total$stunting=="Stunted, but not severely")]=1
data.total$stunted[which(data.total$stunting=="Not stunted")]=0

wd <- "F:/Documents/Data/P20_2013/meta"
setwd(wd)

save(data.total,file="ChildrenOFC.RData")
load("ChildrenOFC.RData")
children=subset(data.total,age<=17)
# chtab=data.table(children)[,.(with_neither_parent=weighted.mean(with_neither_parent, weights,na.rm=TRUE),
#                               one_or_both_parents_dead=weighted.mean(one_or_both_parents_dead, weights,na.rm=TRUE),
#                               birth.reg.coded=weighted.mean(birth.reg.coded, weights, na.rm=TRUE),
#                               stunting=weighted.mean(stunting % in % c("Severely stunted","Stunted, but not severely"), weights, na.rm=TRUE)
# ), by=.(filename)]

chtab=data.table(children)[,.(with_neither_parent=weighted.mean(with_neither_parent, weights,na.rm=TRUE),
                              one_or_both_parents_dead=weighted.mean(one_or_both_parents_dead, weights,na.rm=TRUE),
                              birth.reg.coded=weighted.mean(birth.reg.coded, weights, na.rm=TRUE),
                              stunting=weighted.mean(stunted, weights, na.rm=TRUE),
                              with_neither_parentnonp20=weighted.mean(with_neither_parent & !p20, weights,na.rm=TRUE)/weighted.mean(!p20,weights,na.rm=TRUE),
                              one_or_both_parents_deadnonp20=weighted.mean(one_or_both_parents_dead & !p20, weights,na.rm=TRUE)/weighted.mean(!p20,weights,na.rm=TRUE), 
                              with_neither_parentP20=weighted.mean(with_neither_parent & p20, weights,na.rm=TRUE)/weighted.mean(p20, weights,na.rm=TRUE),
                              one_or_both_parents_deadP20=weighted.mean(one_or_both_parents_dead & p20, weights,na.rm=TRUE)/weighted.mean(p20, weights,na.rm=TRUE),
                              with_neither_parentnonp20totalpop=weighted.mean(with_neither_parent & !p20, weights,na.rm=TRUE),
                              one_or_both_parents_deadnonp20totalpop=weighted.mean(one_or_both_parents_dead & !p20, weights,na.rm=TRUE), 
                              with_neither_parentP20totalpop=weighted.mean(with_neither_parent & p20, weights,na.rm=TRUE),
                              one_or_both_parents_deadP20totalpop=weighted.mean(one_or_both_parents_dead & p20, weights,na.rm=TRUE),
                              with_neither_parent_unreg=weighted.mean(with_neither_parent & !birth.reg.coded, weights, na.rm=TRUE),
                              with_neither_parent_reg=weighted.mean(with_neither_parent & birth.reg.coded, weights, na.rm=TRUE),
                              one_or_both_parents_dead_unreg=weighted.mean(one_or_both_parents_dead & !birth.reg.coded, weights, na.rm=TRUE),
                              one_or_both_parents_dead_reg=weighted.mean(one_or_both_parents_dead & birth.reg.coded, weights, na.rm=TRUE),
                              with_neither_parent_stunted=weighted.mean(with_neither_parent & stunted, weights, na.rm=TRUE)/weighted.mean(with_neither_parent,weights,na.rm=TRUE),
                              with_neither_parent_unstunted=weighted.mean(with_neither_parent & !stunted, weights, na.rm=TRUE)/weighted.mean(with_neither_parent,weights,na.rm=TRUE),
                              one_or_both_parents_dead_stunted=weighted.mean(one_or_both_parents_dead & stunted, weights, na.rm=TRUE)/weighted.mean(one_or_both_parents_dead,weights,na.rm=TRUE),
                              one_or_both_parents_dead_unstunted=weighted.mean(one_or_both_parents_dead & !stunted, weights, na.rm=TRUE)/weighted.mean(one_or_both_parents_dead,weights,na.rm=TRUE),
                              orphanage=weighted.mean(orphanage, weights, na.rm=TRUE),
                              orphanageP20=weighted.mean(orphanage & p20, weights,na.rm=TRUE)/weighted.mean(p20, weights,na.rm=TRUE)
                              )
, by=.(filename)]
orphtab=data.table(children)[,.(.orphanage=weighted.mean(orphanage, weights, na.rm=TRUE),
                                orphanageP20=weighted.mean(orphanage & p20, weights,na.rm=TRUE)/weighted.mean(p20, weights,na.rm=TRUE),
                                orphanagenonP20=weighted.mean(orphanage & !p20, weights,na.rm=TRUE)/weighted.mean(!p20, weights,na.rm=TRUE))]
COFCtab=data.table(children)[,.(with_neither_parent=weighted.mean(with_neither_parent, weights,na.rm=TRUE),
                                  with_neither_parentnonp20=weighted.mean(with_neither_parent & !p20, weights,na.rm=TRUE)/weighted.mean(!p20,weights,na.rm=TRUE),
                                  with_neither_parentP20=weighted.mean(with_neither_parent & p20, weights,na.rm=TRUE)/weighted.mean(p20, weights,na.rm=TRUE))]

COFCttest = svydesign(id=~filename+cluster, data=children, weights=children$weights)
svyttest(with_neither_parent ~ p20, design=COFCttest)
summary(svyglm(with_neither_parent ~ p20, design=COFCttest, 
               family = quasibinomial()))
children$filefactor = factor(children$filename)
summary(glm(with_neither_parent ~ p20+filefactor, data=children,family = binomial()))
svyttest(with_neither_parent ~ stunted, design=COFCttest)
summary(glm(stunted ~ with_neither_parent, data=children,family = binomial()))
summary(svyglm(stunted ~ with_neither_parent, design=COFCttest, 
               family = quasibinomial()))
svyttest(one_or_both_parents_dead ~ stunted, design=COFCttest)
summary(glm(stunted ~ one_or_both_parents_dead, data=children,family = binomial()))
summary(svyglm(stunted ~ one_or_both_parents_dead, design=COFCttest, 
               family = quasibinomial()))

write.csv(chtab,"~/SOS Children's Villages/DHSchildstatus30June2017.csv", row.names=FALSE, na="")
