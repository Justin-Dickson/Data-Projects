#############################################
#                                           #
# Author:     Justin Dickson                #
# Date:       11/20/2021                    #
# Class:      DSCI 618                      #
# Instructor: Dr. Xiaobo Li                 #
# File Name:  Dickson_Project4.R            #
#                                           # 
#############################################

library(dplyr)
library(ggplot2)

setwd("C:/Users/Justin/OneDrive/School/Data Science/Experimental Design/Week5")

# 1. Read the Dataset into R
library(readr)
Yield <- read_csv("Yield.csv")
Yield$Temperature <- as.factor(Yield$Temperature)
Yield$Pressure <- as.factor(Yield$Pressure)
Yield$Blocks <- as.factor(Yield$Blocks)
head(Yield, 6)


# 2. Examine data graphically
# I noticed what I think is a typo in the data that had 'Day3' instead of 'Day2'
# I went back and corrected it in the CSV, then re-imported the data.
stripchart(Yield ~ Blocks, Yield, vert=T, method="overplot", pch=1,
           main="Yield Example", xlab="Block", 
           ylab="Yield")

boxplot(Yield ~ Blocks, Yield,
        main="Yield Example", xlab="Block", 
        ylab="Yield")


# 3. Explain if blocking is necessary in this experiment
# Answer: Yes, it appears by the graphs as well as the later anova results that
#         blocking is necessary for this experiment.


# 4. Anova test on main effects. What conclusions are drawn from a=0.05?
# Answer: It appears that temperature as well as our block have a statistically
#         significant effect on the yield, while pressure does not.
Yield.aov1 <- aov(Yield~ Temperature + Pressure + Blocks, Yield)
summary(Yield.aov1)


# 5. Construct an interaction plot
Day1 <- Yield %>% filter(Blocks=="Day1")
Day2 <- Yield %>% filter(Blocks=="Day2")

with(Day1, interaction.plot(Pressure, Temperature, Yield,type="b",col=10:12,
                             pch=19, fixed=T,xlab="Pressure",ylab="Average Yield"))
title(main="Day1")

with(Day2, interaction.plot(Pressure, Temperature, Yield,type="b",col=10:12,
                            pch=19, fixed=T,xlab="Pressure",ylab="Average Yield"))
title(main="Day2")


# 6. Test interaction between factors. What conclusions are drawn from a=0.05?
# Answer: The interaction between temperature and pressure is not significant.
#         Further model comparison shows our simpler model is favored over
#         the more complex model (with interactions).
Yield.aov2 <- aov(Yield ~ Temperature * Pressure + Blocks, Yield)
summary(Yield.aov2)

anova(Yield.aov1,Yield.aov2)