#############################################
#                                           #
# Author:     Justin Dickson                #
# Date:       12/9/2021                     #
# Class:      DSCI 618                      #
# Instructor: Dr. Xiaobo Li                 #
# File Name:  Practice8.R                   #
#                                           # 
#############################################

library(dplyr)
library(ggplot2)

setwd("C:/Users/Justin/OneDrive/School/Data Science/Experimental Design/Week5")


library(WebPower)
library(ggplot2)
library(BDEsize)


wp.correlation(n=50, r=0.3, alpha=0.05, power=NULL)

wp.correlation(n=seq(50,100,5), r=0.3, alpha=0.05, power=NULL,
               alternative="two.sided")

wp.correlation(n=NULL, r=0.3, alpha=0.05, power=0.8)

# T-Tests

# To test the effectiveness of a training intervention, a researcher
# plans to recruit a group of students and test them before and after
# training. Suppose the expected effect size is 0.3. How many participants 
# are needed to maintain a 0.8 power?
# Paired two-sample t-test.
wp.t(n1=NULL, d=0.3, alpha=0.05, power=0.8, type='paired')


# For the above example, suppose the researcher would like to recruit
# two groups of participants, one group receiving training and the other
# not. What would be the required sample size based on a balanced design
# (two groups are of the same size)?
# Unpaired two-sample t-test.
wp.t(n1=NULL, d=0.3, alpha = 0.05, power=0.8, type='two.sample')


# For the above example, if one group has a size 100 and the other 250,
# what would be the power?
# Unpaired two-sample t-test with unbalanced design.
wp.t(n1=100, n2=250, d=0.3, alpha = 0.05, power=NULL, type='two.sample.2n')


# One-Way ANOVA

# A student hypothesizes that freshman, sophomore, junior and senior college
# students have different attitude towards obtaining arts degrees. 
# Based on his prior knowledge, he expects that the effect size is about 0.25.
# If he plans to interview 25 students on their attitude in each student group,
# what is the power for him to find the significant difference among the four
# groups?
# Power
wp.anova(f=0.25, k=4, n=100, alpha = 0.05, power = NULL)

# Minimum detectable effect (how to achieve a certain power)
wp.anova(f=NULL, k=4, n=100, alpha = 0.05, power=0.8)



# RCBD (Randomized Complete Block Design)
# Only main effects
A <- Size.Block(factor.lev=c(2,2), interaction = FALSE, delta_type=1, 
                delta=c(1,0,1), alpha=0.05, beta=0.2)
A



#including two-way interaction effects
B <- Size.Block(factor.lev=c(2,2), interaction = TRUE, delta_type=1, 
                delta=c(1,1,1), alpha=0.05, beta=0.2)
B



# Split Plot Design

#only main effects
A <- Size.Split(whole.factor.lev=c(2,2), split.factor.lev=c(2,2),
                interaction=FALSE, delta_type=1, delta=c(1,0,1,1), 
                alpha=0.05, beta=0.2)
A

#including two-way interaction effects
B <- Size.Split(whole.factor.lev=c(2,2), split.factor.lev=c(2,2),
                interaction=TRUE, delta_type=1, delta=c(1,1,1,1), 
                alpha=0.05, beta=0.2)
B