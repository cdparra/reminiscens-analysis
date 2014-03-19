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

prepare.subset.data.norepeated <- function (data, vcolumn, gcolumn, filter)
{
    
    # ToDo: find a way for also passing the filter for the data frame
    df <- data.frame(
        Value = data[,vcolumn],
        Group = factor(data[,gcolumn])
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

anova.boxplot.posthoc.repeated <- function (data, bpfile, condition_names)
{
    # 1. Anova for repeated measures
    aov <- aov( Value ~ Group + Error(Participant/Group), data)
    print(summary(aov))
    
    # 2. A boxplot of groups and frequencies
    boxplot(data$Value  ~ data$Group,names = condition_names)
    dev.copy2pdf(file=bpfile)
    
    # 3. Posthoc analysis
    print("Square-root differences between conditions")
    #print(square.root.differences(data))
    posthoc <- post.hoc.test.score.repeated(data)
    print(posthoc)
}

anova.boxplot.posthoc <- function (data, bpfile, condition_names)
{
    # 1. Anova for repeated measures
    aov <- aov( Value ~ Group, data)
    print(summary(aov))
    
    # 2. A boxplot of groups and frequencies
    boxplot(data$Value  ~ data$Group,names = condition_names)
    dev.copy2pdf(file=bpfile)
    
    # 3. Posthoc analysis
    print("Square-root differences between conditions")
    #print(square.root.differences(data))
    #print(post.hoc.test.score.repeated(data))
    # Observation: posthoc is right now just a square root of differences
    # --> other methods for post-hoc that didn't work
    # pairwise.t.test(x=df$Value, g=df$Group,paired=T)
    # pairwise.wilcox.test(df$Value, df$Group, p.adj="bonferroni", exact=F, paired=T,  conf.level = 0.5)
    TukeyHSD(aov) # ---> this is not for repeated measures
}


post.hoc.test.score.repeated <- function(data, value="Value", group="Group", participant="Participant", N=0, D=0)
{
    # data is a long data.frame with three columns:    [[ Y (numeric), X (factor), block (factor)  ]]    
    # By default: 
    # - N -> population is 0, meaning, is far larger than sample
    # - D -> hypothesized difference equal to 0, with null hypothesis being that the mean d is D
    # Loading needed packages
    if(!require(reshape))
    {
        print("You are missing the package 'reshape', we will now try to install it...")
        install.packages("reshape")        
        library(reshape)
    }
    
    pdata <- cast(data,Participant ~ Group , value="Value")     # automatically, pivot table holds sums now
    conditions <- length(pdata[1,])
    ttest_table <-matrix(nrow=conditions-1,ncol=conditions-1)
    
    for (i in 2:(conditions-1))
    {
        ini = i+1
        for (j in ini:conditions)
        {   
            # 1. Standard deviation. Compute the standard deviation (sd) of 
            #    the differences computed from n matched pairs of conditions 
            #    i and j
            
            condition_i <- pdata[,i]                        # right side value of matched pairs
            condition_j <- pdata[,j]                        # left side values of matched pairs
            x1 <- mean(condition_i)                         # mean of condition i 
            x2 <- mean(condition_j)                         # mean of condition j
            n <- length(condition_i)                        # number of matched pairs
            diffs <- condition_i - condition_j              # differences between pairs
            d <- mean(diffs)                                # mean of differences
            sd <- sqrt(sum((diffs-d)^2)/(n-1))              # sd of differences for n matched pairs
            
            # 2.  Standard error. Compute the standard error (SE) of the sampling 
            #     distribution of d.
             if(N>0)
             {
                 SE <- sd*sqrt((1/n)*(1-n/N)*(N/(N-1)))
             } 
             
             if(N==0)
             {
                # if N is unknown, we assume is much larger than n and hence approximate
                # SE with the following
                SE <- sd/sqrt(n)
            }
            
            # 4. Degrees of freedom. The degrees of freedom (DF) 
            DF <- n-1 
            
            # 5. Test statistic. The test statistic is a t-score (t) defined by the following equation.
            t <- ((x1-x2)-D)/SE # = (d - D) / SE
            
            # 6. Store result in a matrix 
            ttest_table[j-1,i-1] <- t
        }
    }
    return(ttest_table)
}

