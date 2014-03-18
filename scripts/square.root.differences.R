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
