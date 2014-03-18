#######################################################################################
# Auxiliary functions to prepare data for reminiscens study analysis
# Author: Cristhian Parra

prepare.subset.data <- function (data, vcolumn, gcolumn, pcolumn, filter)
{
    
    # ToDo: find a way for also passing the filter for the data frame
    df <- data.frame(
        Value = data[,vcolumn],
        Group = factor(data[,gcolumn]),
        Participant = factor(data[,pcolumn])
    )
    return(df)
}

square.root.differences <- function(data, value="Value", group="Group", participant="Participant")
{
    # data is a long data.frame with three columns:    [[ Y (numeric), X (factor), block (factor)  ]]    
    # Loading needed packages
    if(!require(reshape))
    {
        print("You are missing the package 'reshape', we will now try to install it...")
        install.packages("reshape")    	
        library(reshape)
    }
    pdata <- cast(data,Participant ~ Group , value="Value")     # automatically, pivot table holds sums now
    conditions <- length(pdata[1,])
    sqroot_table <-matrix(nrow=conditions-1,ncol=conditions-1)
    
    for (i in 2:(conditions-1))
    {
        ini = i+1
        for (j in ini:conditions)
        {        
            # differences between conditions i and j
            diffs <- pdata[,i] - pdata[,j] 
            sqdif <- sum((diffs - mean(diffs))^2)/length(diff)
            sqroot_table[j-1,i-1] = sqdif
        }
    }
    return(sqroot_table)
}

anova.boxplot.posthoc <- function (data, bpfile = "plots/boxplot.pdf", condition_names)
{
    # 1. Anova for repeated measures
    aov <- aov( Value ~ Group + Error(Participant/Group), data)
    print(summary(aov))
    
    # 2. A boxplot of groups and frequencies
    pdf("plots/longstudy-appversion-sessions-boxplot.pdf")
    boxplot(data$Value  ~ data$Group,names = condition_names)
    dev.off()
    
    # 3. Posthoc analysis
    print("Square-root differences between conditions")
    print(square.root.differences(data))
    
    # Observation: posthoc is right now just a square root of differences
    # --> other methods for post-hoc that didn't work
    # pairwise.t.test(x=df$Value, g=df$Group,paired=T)
    # pairwise.wilcox.test(df$Value, df$Group, p.adj="bonferroni", exact=F, paired=T,  conf.level = 0.5)
    # TukeyHSD(aov) ---> this is not for repeated measures
}

