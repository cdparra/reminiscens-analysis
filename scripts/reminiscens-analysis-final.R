###################################################################################################
# Statistical analysis of Reminiscens Sessions and Participants                                   #
# Author: Cristhian Parra                                                                         #
###################################################################################################
# Auxiliary Packages to use
{
    library(ggplot2)
    library(manipulate)
    library(plyr)
    library(xtable)
    library(reporttools)
    library(reshape)
}

# Personal and External Function sources
{
    source("scripts/reminiscens.tests.utils.R")
    source("external/friedman.test.with.pohs.hoc.R") # Friedman test as implemented by [1] 
}         

# Some globals
{
    datadir <- "data/"
    sessfname <- "sessions_dataset_1.csv"
    partfname <- "participants_dataset_1.csv"
    sessfile <- paste(datadir,sessfname,sep="")
    partfile <- paste(datadir,partfname,sep="")
    figdir <- "plots/"
    
    qualscores <- c("p_score",                                          # preparation scores
                    "s_max_score", "se_score", "sg_score", "set_score",  # stimulation scores
                    "st_score",                                         # storytelling scores
                    "sr_score",                                         # self-reflection scores 
                    "pc_score", "pnc_score",                            # preservation scores
                    "cc_score", "cnc_score",                            # curation scores
                    "c_max_score"                                       # collaboration scores
    )
    
    preparation  <- "p_score"      # preservation scores
    stimulation  <- "s_max_score"  # stimulation scores
    guided_stim  <- "sg_score"
    extern_stim  <- "se_score"   
    tablet_stim  <- "set_score"
    storytelling <- "st_score"     # storytelling scores
    selfreflect  <- "sr_score"     # self-reflection scores 
    colpreserv   <- "pc_score"     # preservation scores
    nocolpreserv <- "pnc_score"
    colcuration  <- "cc_score"     # curation scores
    nocolcuration<- "cnc_score"  
    collaboration<- "c_max_score" # collaboration scores
    
    appvnames <- c("Personal","Questions", "Context", "All")
    
    studytracks <- c("Stable", "Firs week only", "Pilot")
    
    socialcontexts <- c("Family", "No Family")
}

# Load Reminiscens sessions dataset
{
    print("Reading and preparing the data...")
    sessfulltable <- read.csv(sessfile,header=T,sep=",")  
    partfulltable <- read.csv(partfile,header=T,sep=",")  
    attach(sessfulltable)
    attach(partfulltable)
}

# Filter out sessions
{
    sesspilot <- sessfulltable[done==1&group==0,]   # Pilot study sessions
    sess <- sessfulltable[done==1&group!=0,]    	# Only done sessions
    sessstabl <- sess[sess$group==1,]				# Stable group sessions
    sess1time <- sess[sess$group==2,]				# 1-Time group sessions
    sess1week <- sess[sess$week==1,]    			# First week sessions
    sessfamily <- sess[sess$family==1,]				# Sessions in Family
    sessnofamily <- sess[sess$family==0,]			# Sessions not in Family
    sessnofamilystabl <- 
        sessstabl[sessstabl$family==0,]    		    # Sessions not in Family
    
    # Filter out participants
    partpilot = 
        partfulltable[partfulltable$type==0,]		# Pilot study participants
    listpilot = 
        partpilot[partpilot$type==0
                  & partpilot$islistener==1,]      	# Pilot group listeners
    narrpilot = 
        partpilot[partpilot$type==0
                  & partpilot$islistener==0,]  		# Pilot group narrators
    
    part = partfulltable[partfulltable$type!=0,]	# All the participants
    partstabl = part[part$type==1,]				    # Stable group participants
    part1time = part[part$type==2,]					# 1-Time group participants
    list = part[part$islistener==1,]				# Listeners
    narr = part[part$islistener==0,]				# Narrators
    liststabl = list[list$type==1,]				    # Stable group listeners
    list1time = list[list$type==2,]					# 1-Time group listeners
    narrstabl = narr[narr$type==1,]				    # Stable group narrators
    narr1time = narr[narr$type==2,]					# 1-Time group narrators
    
    appvnames <- c("Personal","Questions", "Context", "All")
}


# SUMMARIES AND PIVOT TABLES #########################################################
{
    # Basic descriptive statistics of sessions    
    # App version per Track of the study
    ses_appv_group <- data.frame(sess$sid,sess$appversion,sess$group)
    attach(ses_appv_group)
    ses_apv_group <- xtabs(freq ~ sess.group+sess.appversion,
                           count(ses_appv_group,
                                 c("sess.group","sess.appversion")
                           )
    )	
    ses_apv_group
    ltx_ses_apv_group <- xtable(ses_apv_group)
    # tableNominal(vars = tli.table, 
    #              cap = "Sessions per Appverison", 
    #              vertical = FALSE, 
    #              lab = "tab:longstudy-sessions-appversion", 
    #              longtable = FALSE)
    ltx_ses_apv_group
    
    # App version against family/non family variable
    ses_appv_family = data.frame(sess$sid,sess$appversion,sess$family)
    attach(ses_appv_family)
    ses_appv_family <- xtabs(freq ~ sess.family+sess.appversion,
                             count(ses_appv_family,
                                   c("sess.family","sess.appversion")
                             )
    )  
    ses_appv_family
    ltx_ses_appv_family <- xtable(ses_appv_family)
    ltx_ses_appv_family
}

########################################################################################
# Auxiliary Data Preparations techniques, looking for unbalance in data
# Check data.preparations.R to see why we filter like this
{
    # FILTERED DATASETS
    # removed participant 77 because it did not tried app v1
    stable_1stweeks <- sess[sess$week<5 & sess$group==1 & sess$nid!=77,]        # first 4 weeks, all 4 versions
    # removed participant 75, 77, 79, 83, 98 because it did not tried all versions
    stable_lastweeks <- sess[sess$week>4 & sess$group==1 & sess$nid!=77     # last 4 weeks, all 4 versions
                             & sess$nid!=75 & sess$nid!=79 & sess$nid!=83 
                             & sess$nid!=98, ]
    
    # removed participant 77 because it did not tried app v1
    stable_1stweeks_v1v4 <- sess[sess$week<5 & sess$group==1 
                                 & sess$nid!=77 & sess$appversion!=2 
                                 & sess$appversion!=3,]        # first 4 weeks, all 4 versions
    
    # removed participant 79, 83, 98 because it did not tried v1 or v4
    stable_lastweeks_v1v4 <- sess[sess$week>4 & sess$group==1 & sess$nid!=79 
                                  & sess$nid!=83 & sess$nid!=98
                                  & sess$appversion!=2 
                                  & sess$appversion!=3, ]
    
    
    firstweekall <- sess[sess$week==1,]        # first week, all 4 versions
    
    
    # remove last 2 weeks
    stable_mood_week_all <- sess[sess$group==1,]
    stable_mood_week_6 <- sess[sess$group==1 & sess$week<7, ]
}



# ------------------------------------------------------------------------------------
# --> Friedman. => (SS) Created stories
{
    subset_columns <- c("week", "nid", "stat_stories_from_log", "appversion", "group")
    vcolumn <- "stat_stories_from_log"
    gcolumn <- "appversion"
    pcolumn <- "nid"
    test <- paste("SS-friedman",gcolumn,vcolumn,sep="")
    df <- prepare.subset.data(stable_1stweeks,vcolumn,gcolumn,pcolumn)
    file <-paste(figdir,test,"-boxplot-friedman.pdf",sep="")
    friedman.test.with.post.hoc(Value ~ Group | Participant, df, signif.P = .9
                                ,plot.filename=file)      
    
    # Repeat with second half of study
    df <- prepare.subset.data(stable_lastweeks,vcolumn,gcolumn,pcolumn)
    file <-paste(figdir,test,"-boxplot-lastweeks-friedman.pdf",sep="")
    friedman.test.with.post.hoc(Value ~ Group | Participant, df, signif.P = .9
                                , plot.filename=file)      
}

# ------------------------------------------------------------------------------------
# --> Friedman. => (PS) Created pictures
{
    subset_columns <- c("week", "nid", "stat_pictures", "appversion", "group")
    vcolumn <- "stat_pictures"
    gcolumn <- "appversion"
    pcolumn <- "nid"
    test <- paste("PS-friedman",gcolumn,vcolumn,sep="")
    df <- prepare.subset.data(stable_1stweeks,vcolumn,gcolumn,pcolumn)
    file <-paste(figdir,test,"-boxplot-friedman.pdf",sep="")
    friedman.test.with.post.hoc(Value ~ Group | Participant, df, signif.P = .9
                                ,plot.filename=file)      
    
    # Repeat with second half of study
    df <- prepare.subset.data(stable_lastweeks,vcolumn,gcolumn,pcolumn)
    file <-paste(figdir,test,"-boxplot-lastweeks-friedman.pdf",sep="")
    friedman.test.with.post.hoc(Value ~ Group | Participant, df, signif.P = .9
                                , plot.filename=file)      
}


# ------------------------------------------------------------------------------------
# --> Friedman. => (QS) Questions answered
{
    subset_columns <- c("week", "nid", "stat_questions_answered", "appversion", "group")
    vcolumn <- "stat_questions_answered"
    gcolumn <- "appversion"
    pcolumn <- "nid"
    test <- paste("QS-friedman",gcolumn,vcolumn,sep="")
    df <- prepare.subset.data(stable_1stweeks,vcolumn,gcolumn,pcolumn)
    file <-paste(figdir,test,"-boxplot-friedman.pdf",sep="")
    friedman.test.with.post.hoc(Value ~ Group | Participant, df, signif.P = .9
                                ,plot.filename=file)      
    
    # Repeat with second half of study
    df <- prepare.subset.data(stable_lastweeks,vcolumn,gcolumn,pcolumn)
    file <-paste(figdir,test,"-boxplot-lastweeks-friedman.pdf",sep="")
    friedman.test.with.post.hoc(Value ~ Group | Participant, df, signif.P = .9
                                , plot.filename=file)      
}


# ------------------------------------------------------------------------------------
# --> Friedman. => (MS) Public memento detail views
{
    subset_columns <- c("week", "nid", "stat_public_memento_detail_views", "appversion", "group")
    vcolumn <- "stat_public_memento_detail_views"
    gcolumn <- "appversion"
    pcolumn <- "nid"
    test <- paste("MS-friedman",gcolumn,vcolumn,sep="")
    df <- prepare.subset.data(stable_1stweeks,vcolumn,gcolumn,pcolumn)
    file <-paste(figdir,test,"-boxplot-friedman.pdf",sep="")
    friedman.test.with.post.hoc(Value ~ Group | Participant, df, signif.P = .9
                                ,plot.filename=file)      
    
    # Repeat with second half of study
    df <- prepare.subset.data(stable_lastweeks,vcolumn,gcolumn,pcolumn)
    file <-paste(figdir,test,"-boxplot-lastweeks-friedman.pdf",sep="")
    friedman.test.with.post.hoc(Value ~ Group | Participant, df, signif.P = .9
                                , plot.filename=file)      
}

# ------------------------------------------------------------------------------------
# --> Friedman. => (MSN) Mood narrator
{
    subset_columns <- c("week", "nid", "mood_after_narrator", "appversion", "group")
    vcolumn <- "mood_after_narrator"
    gcolumn <- "appversion"
    pcolumn <- "nid"
    test <- paste("MSN-friedman",gcolumn,vcolumn,sep="")
    df <- prepare.subset.data(stable_1stweeks,vcolumn,gcolumn,pcolumn)
    file <-paste(figdir,test,"-boxplot-friedman.pdf",sep="")
    friedman.test.with.post.hoc(Value ~ Group | Participant, df, signif.P = .9
                                ,plot.filename=file)      
    
    # Repeat with second half of study
    df <- prepare.subset.data(stable_lastweeks,vcolumn,gcolumn,pcolumn)
    file <-paste(figdir,test,"-boxplot-lastweeks-friedman.pdf",sep="")
    friedman.test.with.post.hoc(Value ~ Group | Participant, df, signif.P = .9
                                , plot.filename=file)      
}


# ------------------------------------------------------------------------------------
# --> Friedman. => (MSL) Mood narrator
{
    subset_columns <- c("week", "nid", "mood_after_listener", "appversion", "group")
    vcolumn <- "mood_after_listener"
    gcolumn <- "appversion"
    pcolumn <- "nid"
    test <- paste("MSL-friedman",gcolumn,vcolumn,sep="")
    df <- prepare.subset.data(stable_1stweeks,vcolumn,gcolumn,pcolumn)
    file <-paste(figdir,test,"-boxplot-friedman.pdf",sep="")
    friedman.test.with.post.hoc(Value ~ Group | Participant, df, signif.P = .9
                                ,plot.filename=file)      
    
    # Repeat with second half of study
    df <- prepare.subset.data(stable_lastweeks,vcolumn,gcolumn,pcolumn)
    file <-paste(figdir,test,"-boxplot-lastweeks-friedman.pdf",sep="")
    friedman.test.with.post.hoc(Value ~ Group | Participant, df, signif.P = .9
                                , plot.filename=file)      
}



# ------------------------------------------------------------------------------------
# --> Friedman. => (ST) Storytelling
{
    subset_columns <- c("week", "nid", "st_score", "appversion", "group")
    vcolumn <- "st_score"
    gcolumn <- "appversion"
    pcolumn <- "nid"
    test <- paste("ST-friedman",gcolumn,vcolumn,sep="")
    df <- prepare.subset.data(stable_1stweeks,vcolumn,gcolumn,pcolumn)
    file <-paste(figdir,test,"-boxplot-friedman.pdf",sep="")
    friedman.test.with.post.hoc(Value ~ Group | Participant, df, signif.P = 1
                                ,plot.filename=file)      
    
    # Repeat with second half of study
    df <- prepare.subset.data(stable_lastweeks,vcolumn,gcolumn,pcolumn)
    file <-paste(figdir,test,"-boxplot-lastweeks-friedman.pdf",sep="")
    friedman.test.with.post.hoc(Value ~ Group | Participant, df, signif.P = 1
                                , plot.filename=file)      
}








# ------------------------------------------------------------------------------------
# --> APPVERSION FOR  STORYTELLING, STIMULATION, COLLABORATION, in the first week
# ------------------------------------------------------------------------------------
# --> ANOVA of APPVERSION vs STORIES CREATED IN SESSION 1
# --> OBSERVATION: the design here is Between-Group
{
    subset_columns <- c("week", "nid",
                        "appversion",                                       
                        "group", 
                        "family",
                        qualscores)
    subset <- firstweekall[,subset_columns]
    values <- subset[,qualscores]
    summary(values)
    
    
    # 1. Storytelling
    vcolumn <- storytelling
    gcolumn <- "appversion"
    pcolumn <- "nid"
    test <- paste("aov-1time-",gcolumn,vcolumn,sep="")
    df <- prepare.subset.data(firstweekall,vcolumn,gcolumn,pcolumn)
    
    file <-paste(figdir,test,"-boxplot.pdf",sep="")
    anova.boxplot.posthoc(df,file,appvnames)
    
    # 1. Collaboration
    vcolumn <- collaboration 
    test <- paste("aov-1time-",gcolumn,vcolumn,sep="")
    df <- prepare.subset.data(firstweekall,vcolumn,gcolumn,pcolumn)
    file <-paste(figdir,test,"-boxplot.pdf",sep="")
    anova.boxplot.posthoc(df,file,appvnames)
    
    # 1. Stimulation
    vcolumn <- stimulation 
    test <- paste("aov-1time-",gcolumn,vcolumn,sep="")
    df <- prepare.subset.data(firstweekall,vcolumn,gcolumn,pcolumn)
    file <-paste(figdir,test,"-boxplot.pdf",sep="")
    anova.boxplot.posthoc(df,file,appvnames)
    
    
    # 1. Col Preservation
    vcolumn <- colpreserv 
    test <- paste("aov-1time-",gcolumn,vcolumn,sep="")
    df <- prepare.subset.data(firstweekall,vcolumn,gcolumn,pcolumn)
    file <-paste(figdir,test,"-boxplot.pdf",sep="")
    anova.boxplot.posthoc(df,file,appvnames)
    
}

# Variation of focus per weeks
{
    subset_columns <- c("week", "nid",
                        "appversion",                                       
                        "group", 
                        "family",
                        qualscores)
    
    #79 83 98
    
    dt <- sessstabl[sessstabl$nid<79,subset_columns]
    options(digits=2)
    
    cast(dt, week~nid, value=storytelling)
    qual <- ddply(dt,~week,summarise,
                  prep=mean(p_score,na.rm=T),
                  stim=mean((sg_score+se_score+set_score)/3),
                  story=mean(st_score),
                  self=mean(sr_score),
                  pres=mean((pc_score+pnc_score)/2),
                  cur=mean((cc_score+cnc_score)/2),
                  col=mean((pc_score+cc_score)/2),
                  gstim=mean(sg_score),
                  extestim=mean(se_score),
                  exttablet=mean(set_score),
                  colpres=mean(pc_score),
                  nocolpres=mean(pc_score),
                  colcur=mean(cc_score),
                  nocolcur=mean(cc_score))
    
    attach(qual)
    plot(qual$week,qual$prep,
         ylab="Average preparation score",
         xlab="Week")
    f=paste(figdir,"weeks-preparation.pdf",sep="")
    dev.copy2pdf(file=f)
    
    par
    
    plot(qual$week,qual$story,
         ylab="Average storytelling score",
         xlab="Week")
    f=paste(figdir,"weeks-storytelling.pdf",sep="")
    dev.copy2pdf(file=f)
    
    plot(qual$week,qual$self,
         ylab="Average self-reflection score",
         xlab="Week")
    f=paste(figdir,"weeks-selfreflection.pdf",sep="")
    dev.copy2pdf(file=f)
    
    plot(qual$week,qual$pres,
         ylab="Average preservation score",
         xlab="Week")
    f=paste(figdir,"weeks-preservation.pdf",sep="")
    dev.copy2pdf(file=f)
    
    plot(qual$week,qual$cur,
         ylab="Average curation score",
         xlab="Week")
    f=paste(figdir,"weeks-curation.pdf",sep="")
    dev.copy2pdf(file=f)
    
    plot(qual$week,qual$col,
         ylab="Average collaboration score",
         xlab="Week")
    f=paste(figdir,"weeks-collaboration.pdf",sep="")
    dev.copy2pdf(file=f)
    
    
    
}

########################################################################################
# TEST 4 ==> Cramer's V Correlation between family and SSQ
# TEST 5 ==> Spearman Correlation between intergneration_index/social_disengagemt and SSQ
# TEST 2 ==> Correlation between collaborative sessions and stories created

#79 83 98
{
    subset_columns <- c("week", 
                        "nid",
                        "appversion",                                       
                        "group", 
                        "family",
                        "cumulweek",
                        "intergenerational_index",
                        "mood_after_narrator",
                        "mood_after_narrator2",
                        "mood_after_listener",
                        "log_traffic",
                        "nsindex",
                        "n2sindex",
                        "lsindex",
                        "stat_questions_answered",
                        "stat_public_memento_views",
                        "stat_public_memento_detail_views",
                        "stat_stories_from_log",
                        "stat_editions_from_log",
                        "stat_pictures",
                        qualscores)
    dt <- sessstabl[,subset_columns]
    options(digits=2)
    cast(dt, week~nid, value="stat_pictures")
    
    CollabScores = (dt$pc_score+dt$cc_score)/2
    Stories = dt$stat_stories
    ExtStimulation = dt$se_score
    ExtTabletStimulation = dt$set_score
    GuidedStimulation = dt$sg_score
    Stimulation = (ExtStimulation + ExtTabletStimulation + GuidedStimulation)/3
    CollabPreservation = dt$pc_score
    NoCollabPreservation = dt$pnc_score
    Preservation = (CollabPreservation+NoCollabPreservation)/2
    Storytelling = dt$st_score
    Selfreflection = dt$sr_score
    CollabCuration = dt$cc_score
    NoCollabCuration = dt$cnc_score
    Curation = (CollabCuration+NoCollabCuration)/2
    
    
    MoodNarrator = dt$mood_after_narrator
    MoodListener = dt$mood_after_listener
    MoodNarrator2 = dt$mood_after_narator2
    Intergenerational = dt$intergenerational_index
    AppUsage = dt$log_traffic
    NarratorSocial = dt$nsindex
    Narrator2Social = dt$n2sindex
    ListenerSocial = dt$lsindex
    Family = dt$family
    
    
    cor.test(CollabScores,Stories,method="pearson")
    plot(Stories, CollabScores,
         ylab="Collaboration scores in sessions",
         xlab="Stories shared in sessions")
    f=paste(figdir,"relations-collaboration-stories.pdf",sep="")
    dev.copy2pdf(file=f)
    
    c<-cor.test(ExtStimulation,Stories,method="pearson")
    print(c)
    plot(Stories, ExtStimulation,
         ylab="Stimulation scores in sessions",
         xlab="Stories shared in sessions")
    f=paste(figdir,"relations-stimulation-stories.pdf",sep="")
    dev.copy2pdf(file=f)
    
    cor.test(MoodNarrator,ExtStimulation,method="pearson", )
    plot(MoodNarrator, ExtStimulation,
         xlab="Mood of narrator after sessions",
         ylab="Stimulation by listeners during sessions")
    f=paste(figdir,"relations-moodnarrator-extstimulation.pdf",sep="")
    dev.copy2pdf(file=f)
    
    cor.test(MoodNarrator,CollabScores,method="pearson")
    v1<-"moodnarrator"
    v2<-"collaboration"
    plot(MoodNarrator, CollabPreservation,
         xlab="Mood of narrator after sessions",
         ylab="Collaboration during sessions")
    f=paste(figdir,"relations-",v1,"-",v2,"",sep="")
    dev.copy2pdf(file=f)
    
    cor.test(Family,CollabScores,method="pearson")
    v1<-"family"
    v2<-"collaboration"
    plot(Family, CollabScores,
         xlab="Social Context (family/no family) during sessions",
         ylab="Collaboration during sessions")
    f=paste(figdir,"relations-",v1,"-",v2,"",sep="")
    dev.copy2pdf(file=f)
    
    
    cor.test(CollabScores,GuidedStimulation,method="pearson")
    v1<-"collaboration"
    v2<-"guidedstimulation"
    plot(CollabScores, GuidedStimulation,
         xlab="Collaboration during sessions",
         ylab="Stimulation by the application during sessions")
    f=paste(figdir,"relations-",v1,"-",v2,"",sep="")
    dev.copy2pdf(file=f)
    
    cor.test(CollabScores,Stimulation,method="pearson")
    v1<-"collaboration"
    v2<-"stimulation"
    plot(CollabScores, Stimulation,
         xlab="Collaboration during sessions",
         ylab="Stimulation during sessions")
    f=paste(figdir,"relations-",v1,"-",v2,"",sep="")
    dev.copy2pdf(file=f)
    
    cor.test(CollabScores,AppUsage,method="pearson")
    v1<-"collaboration"
    v2<-"appusage"
    plot(CollabScores, AppUsage,
         xlab="Collaboration during sessions",
         ylab="App usage during sessions")
    f=paste(figdir,"relations-",v1,"-",v2,"",sep="")
    dev.copy2pdf(file=f)
    
    cor.test(CollabScores,NarratorSocial,method="pearson")
    v1<-"collaboration"
    v2<-"sindex"
    plot(CollabScores, NarratorSocial,
         xlab="Collaboration during sessions",
         ylab="Social Disengagement of Narrator")
    f=paste(figdir,"relations-",v1,"-",v2,"",sep="")
    dev.copy2pdf(file=f)
    
    cor.test(Storytelling,NarratorSocial,method="pearson")
    v1<-"storytelling"
    v2<-"sindex"
    plot(Storytelling, NarratorSocial,
         xlab="Storytelling during sessions",
         ylab="Social Disengagement of Narrator")
    f=paste(figdir,"relations-",v1,"-",v2,"",sep="")
    dev.copy2pdf(file=f)
    
    cor.test(Storytelling,ListenerSocial,method="pearson")
    v1<-"storytelling"
    v2<-"sindex-listener"
    plot(Storytelling, ListenerSocial,
         xlab="Storytelling during sessions",
         ylab="Social Disengagement of Listener")
    f=paste(figdir,"relations-",v1,"-",v2,"",sep="")
    dev.copy2pdf(file=f)
    
    cor.test(Stimulation,ListenerSocial,method="pearson")
    v1<-"stimulation"
    v2<-"sindex-listener"
    plot(Stimulation, ListenerSocial,
         xlab="Stimulation during sessions",
         ylab="Social Disengagement of Listener")
    f=paste(figdir,"relations-",v1,"-",v2,"",sep="")
    dev.copy2pdf(file=f)
    
    
    cor.test(CollabScores,ListenerSocial,method="pearson") # 0.3422 ==> The highest so far
    v1<-"collaboration"
    v2<-"sindex-listener"
    plot(CollabScores, ListenerSocial,
         xlab="Collaboration during sessions",
         ylab="Social Disengagement of Listener")
    f=paste(figdir,"relations-",v1,"-",v2,"",sep="")
    dev.copy2pdf(file=f)
    
}


























