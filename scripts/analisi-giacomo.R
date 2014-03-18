# Check Assumptions

# week, nid, stat_stories_from_log, appversion
df <- rses[,c("week", "nid", "stat_stories_from_log", "appversion", "group")]
str(df)

table(df$week)

df <- df[df$week<5 & df$group==1,]
str(df)
df

cast(df,nid ~ appversion, value="stat_stories_from_log", subset=nid!=77)

# To comment about balance of the dataset, taken out 77 becase didn't try appversion 1

aov <- aov(stat_stories_from_log ~ factor(appversion) + Error(factor(nid)/factor(appversion)), df[df$nid!=77,])
summary(aov)

# difference within subjects of the stories for each version, and then sum

# Check how to fix it
pairwise.t.test(x=df[df$nid!=77,]$stat_stories_from_log, g=df[df$nid!=77,]$appversion)
# Post-hoc analysis
# pairwise comparison 
# method "tukey"

TukeyHSD(aov)
