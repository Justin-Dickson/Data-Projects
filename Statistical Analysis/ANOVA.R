#############################################
#                                           #
# Author:     Justin Dickson                #
# Date:       10/23/2021                    #
# Class:      DSCI 618                      #
# Instructor: Dr. Xiaobo Li                 #
# File Name:  Dickson_Project2.R            #
#                                           # 
#############################################

setwd("C:/Users/Justin/OneDrive/School/Data Science/Experimental Design/Week2")


# 1. Read the Dataset into R
library(readr)
Portland_Cement <- read_csv("Portland_Cement_Data.csv")
head(Portland_Cement, 6)
Portland_Cement$Mixing <- as.factor(Portland_Cement$Mixing)


# 2. Examine the data graphically
stripchart(Strength ~ Mixing, Portland_Cement, vert=T, method="overplot", pch=1,
           main="Mixing Strength Example", xlab="Mixing Technique", 
           ylab="Observed Strength")

boxplot(Strength ~ Mixing, Portland_Cement,
        main="Mixing Strength Example", xlab="Mixing Technique", 
        ylab="Observed Strength")

# 3. Test the hypothesis that mixing techniques affect the strength with a=0.05
# Answer: The P-value of 0.000489 means we reject the null hypothesis and
#       conclude the mixing techniques do have an affect on the strength.

mix.str.aov <- aov(Strength ~ Mixing, Portland_Cement)
summary(mix.str.aov)


# 4. Construct a Normal Q-Q Plot. What conclusions are drawn about the validity
#     of the Normality assumption?
# Answer: The residuals are lined well on the dashed line, meaning the Normality
#       Assumption is valid.
plot(mix.str.aov, 2)


# 5. Plot the residuals versus the predicted tensile strength (Residuals vs Fitted)
# Answer: The line is nearly horizontal, meaning our data likely has a linear
#       relationship. Nor does there appear to be any large outliers.
plot(mix.str.aov, 1)


# 6. Scale-Location Plot. What conclusion is drawn about the Homoscedasticity
#     Assumption?
# Answer: Similar to the previous plot, the Scale-Location plot seems to have a 
#       near horizontal line. This shows our within-group variances are similar
#       and the Homoscedasticity assumption is valid.
plot(mix.str.aov, 3)


# 7. Comparing mean strengths of mixing techniques with a = 0.05
# Answers:
# 7a There is not a statistically significant difference between Techniques 1 and 3
# 7b There is a statistically significant difference between Techniques 2 and 4
# 7c There is not a statistically significant difference between the average of T1&T3 and T2.
c1 <- c(1, 0, -1, 0)
c2 <- c(0, 1, 0, -1)
c3 <- c(0.5, 0.5, -1, 0)

mat <- cbind (c1, c2, c3)
contrasts(Portland_Cement$Mixing) <- mat
anova_contrast <- aov(Strength ~ Mixing, Portland_Cement)
summary.aov(anova_contrast, split=list(Mixing=list("T1 vs T3"=1, "T2 vs T4"=2, "T1&T3 vs T2"=3)))


# 8. Calculate the Power of the analysis given means 2900,3100,2850,2680, error
#     variance = 230^2, a = 0.05 (I also picked groups=4 and n=5, similar to
#     the example)
# Answer: The power of the analysis given the above variables is 0.5656, or 56.6%
treatment.means <- c(2900, 3100, 2850, 2680)
power.anova.test(groups=4, n=5, between.var=var(treatment.means), within.var=230^2,
                 sig.level=0.05)