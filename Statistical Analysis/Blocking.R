#############################################
#                                           #
# Author:     Justin Dickson                #
# Date:       11/14/2021                    #
# Class:      DSCI 618                      #
# Instructor: Dr. Xiaobo Li                 #
# File Name:  Dickson_Project4.R            #
#                                           # 
#############################################

library(dplyr)
library(ggplot2)

setwd("C:/Users/Justin/OneDrive/School/Data Science/Experimental Design/Week4")

# 1. Read the Dataset into R
library(readr)
Batteries_Life <- read_csv("Batteries_Life.csv")
Batteries_Life$Brand <- as.factor(Batteries_Life$Brand)
Batteries_Life$Device <- as.factor(Batteries_Life$Device)
head(Batteries_Life, 6)

# 2. Analysis of variance and test hypotheses on main effects
#     What conclusions can be drawn using a=0.05?
# Answer: Both Brand and Device have a statistically significant affect on 
#         battery life, however Device has a much higher significance.  
Batteries.Life.aov1 <- aov(Life~Brand+Device, Batteries_Life)
summary(Batteries.Life.aov1)


# 3. Construct an interaction plot
Batteries_Life %>% ggplot(aes(x=Device,y=Life,color=Brand,group=Brand,label=Life))+
  geom_line()+
  geom_point()+
  geom_text(aes(label=Life))


# 4. Test on the interaction between factors
#     What conclusions can be drawn using a=0.05?
# Answer: Nothing changes too much between the previous ANOVA and this one. The
#         interaction between the main effects is not statistically significant,
#         but Brand and Device both continue to be.
Batteries.Life.aov2 <- aov(Life~Brand*Device, Batteries_Life)
summary(Batteries.Life.aov2)


#5. Prepare appropraite residual plots and comment on the model's adequacy
# Answer: The Normal Q-Q plot looks good and the residuals vs
#         fitted shows our data is linear, however the residuals are not
#         entirely spread according to the scale-location plot.
plot(Batteries.Life.aov2, 2)
plot(Batteries.Life.aov2, 3)
plot(Batteries.Life.aov2, 1)


# 6. Which brand of batteries would you recommend?
# Answer: My recommendation is brand B when it comes to battery life.