###################################################################################################
# Statistical analysis of Reminiscens Sessions and Participants                                   #
# Author: Cristhian Parra                                                                         #
###################################################################################################
# Auxiliary Packages to use
library(ggplot2)
library(manipulate)

# Some globals
datadir <- "data/"
sessfname <- "sessions_dataset_1.csv"
partfname <- "participants_dataset_1.csv"
sessfile <- paste(datadir,sessfname,sep="")
partfile <- paste(datadir,partfname,sep="")
figdir <- "plots/"

# Load Reminiscens sessions dataset
print("Reading and preparing the data...")
sessfulltable <- read.csv(sessfile,header=T,sep=";")  
partfulltable <- read.csv(partfile,header=T,sep=";")  
attach(sessfulltable)
attach(partfulltable)

# Filter out sessions
sesspilot <- sessfulltable[done==1&group==0,]   # Pilot study sessions
sess <- sessfulltable[done==1&group!=0,]		# Only done sessions
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
              &partpilot$islistener==1,]      	# Pilot group listeners
narrpilot = 
    partpilot[partpilot$type==0
              &partpilot$islistener==0,]  		# Pilot group narrators

part = partfulltable[partfulltable$type!=0,]	# All the participants
partstabl = part[part$type==1,]				    # Stable group participants
part1time = part[part$type==2,]					# 1-Time group participants
list = part[part$islistener==1,]				# Listeners
narr = part[part$islistener==0,]				# Narrators
liststabl = list[list$type==1,]				    # Stable group listeners
list1time = list[list$type==2,]					# 1-Time group listeners
narrstabl = narr[narr$type==1,]				    # Stable group narrators
narr1time = narr[narr$type==2,]					# 1-Time group narrators

# Basic descriptive statistics
# --> Listeners participants
print("Descriptive statistics")
print("--> Age summary of listeners")
summary(list$age)
print("--> Age summary of listeners in the pilot study")
summary(listpilot$age)
print("--> Age summary of listeners in the stable group")
summary(liststabl$age)
print("--> Age summary of listeners in the 1-Time group")
summary(list1time$age)

pdf(paste(figdir,"listeners_age_histogram.pdf",sep=""))
list_hist<-hist(list$age,main="Age distribution of listeners",xlab="Age")
dev.off()
hist(list$age,main="Age distribution of listeners",xlab="Age")

# --> Narrators participants
print("Descriptive statistics")
print("--> Age summary of narrators")
summary(narr$age)
print("--> Age summary of narrators in the pilot study")
summary(narrpilot$age)
print("--> Age summary of narrators in the stable group")
summary(narrstabl$age)
print("--> Age summary of narrators in the 1-Time group")
summary(narr1time$age)

pdf(paste(figdir,"narrators_age_histogram.pdf",sep=""))
narr_hist<-hist(narr$age,main="Age distribution of narrators",xlab="Age")
dev.off()
hist(narr$age,main="Age distribution of narrators",xlab="Age")

# SUMMARIES AND PIVOT TABLES #########################################################
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

########################################################################################
# TEST 1 ==> Repeated measures ANOVA Friedman's Test between appversion and SKM+SSQ
# We use Friedman's Test because the data is not normally distributed
source("/Users/cristhian/Dropbox/research/projects/reminiscens/analysis/statistics/friedman.test.with.pohs.hoc.R")

# 1st step -> Check Assumptions

# 2nd step -> Prepare data subset -> week, nid, stat_stories_from_log, appversion
df <- sess[,c("week", "nid", "stat_stories_from_log", "appversion", "group")]
str(df)
table(df$week)

# --> Take first only the first 4 weeks
df <- df[df$week<5 & df$group==1,]
str(df)

# --> Check balance in a pivot table of groups versus participants
cast(df,nid ~ appversion, value="stat_stories_from_log")
# --> Deal with unbalanced data
# --> * Taking out nid=77 becase it didn't try appversion 1 in first 4 week
cast(df,nid ~ appversion, value="stat_stories_from_log",subset=nid!=77)     # automatically, pivot table holds sums now

# --> Filtered data
df <- df[df$nid!=77,]
df <- data.frame(
    Value = df$stat_stories_from_log,
    Group = factor(df$appversion),
    Participant = factor(df$nid)
)

# 3rd step -> prepare values, groups and participants 
aov <- aov( Value ~ Group + Error(Participant/Group), df)
summary(aov)

with(df, boxplot(Value  ~ Group )) # boxploting 
# friedman.test.with.post.hoc(Value ~ Group | Participant, ReminiscensSession)  # the same with our function. With post hoc, and cool plots


# difference within subjects of the stories for each version, and then sum

# Check how to fix it
pairwise.t.test(x=df[df$nid!=77,]$stat_stories_from_log, g=df[df$nid!=77,]$appversion)
# Post-hoc analysis
# pairwise comparison 
# method "tukey"

TukeyHSD(aov)



with(ReminiscensSession, boxplot(Value  ~ Group )) # boxploting 
# friedman.test.with.post.hoc(Value ~ Group | Participant, ReminiscensSession)  # the same with our function. With post hoc, and cool plots

data = data.frame(Group,Value)
Group1 = data[data$Group==1,]$Value
Group2 = data[data$Group==2,]$Value
Group3 = data[data$Group==3,]$Value
Group4 = data[data$Group==4,]$Value

group_lists = list(Group1,Group2,Group3,Group4)
max_length = max(length(Group1),length(Group2),length(Group3),length(Group4))
data2 = sapply(group_lists,'[',1:max_length)
friedman.test(data2)

# post-hoc test
NewValue=c(data2[,1],data2[,2],data2[,3],data2[,4])
NewGroup=c(rep(1,23),rep(2,23),rep(3,23),rep(4,23))
pairwise.wilcox.test(NewValue, NewGroup, p.adj="bonferroni", exact=F, paired=T)

# --> 1.2 appversion vs stat_pictures (stories created in the sesion)

Value = sess$stat_pictures
with(ReminiscensSession, boxplot(Value  ~ Group )) # boxploting 
data = data.frame(Group,Value)
Group1 = data[data$Group==1,]$Value
Group2 = data[data$Group==2,]$Value
Group3 = data[data$Group==3,]$Value
Group4 = data[data$Group==4,]$Value
group_lists = list(Group1,Group2,Group3,Group4)
max_length = max(length(Group1),length(Group2),length(Group3),length(Group4))
data2 = sapply(group_lists,'[',1:max_length)
friedman.test(data2)

# post-hoc test
NewValue=c(data2[,1],data2[,2],data2[,3],data2[,4])
NewGroup=c(rep(1,23),rep(2,23),rep(3,23),rep(4,23))
pairwise.wilcox.test(NewValue, NewGroup, p.adj="bonferroni", exact=F, paired=T)

# --> 1.2 appversion vs sg_score (stories created in the sesion)

Value = sess$sg_score
with(ReminiscensSession, boxplot(Value  ~ Group )) # boxploting 
data = data.frame(Group,Value)
Group1 = data[data$Group==1,]$Value
Group2 = data[data$Group==2,]$Value
Group3 = data[data$Group==3,]$Value
Group4 = data[data$Group==4,]$Value
group_lists = list(Group1,Group2,Group3,Group4)
max_length = max(length(Group1),length(Group2),length(Group3),length(Group4))
data2 = sapply(group_lists,'[',1:max_length)
friedman.test(data2)
# post-hoc test
NewValue=c(data2[,1],data2[,2],data2[,3],data2[,4])
NewGroup=c(rep(1,23),rep(2,23),rep(3,23),rep(4,23))
pairwise.wilcox.test(NewValue, NewGroup, p.adj="bonferroni", exact=F, paired=T)



# TEST 2 ==> SplitPlot ANOVA between appversion/group and SKM+SSQ

# TEST 3 ==> SplitPlot ANOVA between appversion/family (group=1) and SKM+SSQ



########################################################################################
# TEST 4 ==> Cramer's V Correlation between family and SSQ
# TEST 5 ==> Spearman Correlation between intergneration_index/social_disengagemt and SSQ
# TEST 2 ==> Correlation between collaborative sessions and stories created

CollabScores = sess$c_max_score
Stories = sess$stat_stories
ExtStimulation = sess$se_score
MoodNarrator = sess$mood_after_narrator
GuidedStimulation = sess$sg_score
AppUsage = sess$log_traffic
NarratorSocial = sess$nsindex
Narrator2Social = sess$n2sindex
ListenerSocial = sess$lsindex

cor.test(CollabScores,Stories,method="pearson")
cor.test(ExtStimulation,Stories,method="pearson")
cor.test(MoodNarrator,ExtStimulation,method="pearson")
cor.test(MoodNarrator,CollabScores,method="pearson")
cor.test(CollabScores,GuidedStimulation,method="pearson")
cor.test(CollabScores,AppUsage,method="pearson")
cor.test(CollabScores,NarratorSocial,method="pearson")
cor.test(CollabScores,ListenerSocial,method="pearson") # 0.3422 ==> The highest so far
cor.test(CollabScores,ListenerSocial,method="pearson") # 0.3422 ==> The highest so far
















# Dataset description
# SAP = session and participants columns
# - sid 	= session id (ratio) 
# - nid 	= narrator id
# - nage 	= narrator age                             
# - ngender	= narrator gender
# - lid 	= listener id
# - lage	= listener age
# - lgender	= listener gender
# - n2id	= narrator 2 id
# - n2age 	= narrator 2 age  
# - n2gender= narrator 2 gender
#
# - nsindex	= narrator social disengagement
# - lsindex	= listener social disengagement
# - n2sindex= narrator 2 social disengagement
#         
# SC = session controlled conditions                
# - family
# - pair
# - group                            
# - week
# - finalweek                       
# -	cumulweek
# - intergenerational_index          
#
# SBS = Session basic stats
# - done                            
# - duration
# - wtest
#
# SC = session controlled conditions (2) 
# - appversion                      
# - appversion2 (separating versions of context introduced in half the study) 
#                
# SKM = sesion key metrics
# - stat_stories
# - stat_stories_from_log
# - stat_editions
# - stat_editions_from_log
# - stat_deletions                  
# - stat_pictures
# - stat_questions_views
# - stat_questions_answered
# - stat_public_memento_views
# - stat_public_memento_detail_views mood_week_narrator              
# - mood_before_narrator
# - mood_after_narrator
# - mood_week_listener              
# - mood_before_listener
# - mood_after_listener
# - mood_week_narrator2
# - mood_before_narrator2
# - mood_after_narrator2
# - log_traffic 
#
# SSQ = Session stages qualitative evaluation 
# - p_score_binary
# - p_score
# - s_max_score
# - sg_score
# - se_score
# - set_score                       
# - st_score
# - sr_score
# - pc_score
# - pnc_score
# - cc_score
# - cnc_score
# - c_max_score  
