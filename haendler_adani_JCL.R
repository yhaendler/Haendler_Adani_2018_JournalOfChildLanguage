#----------------------------------------------------------------------------
# analysis script for the article:
# Haendler, Yair & Adani, Flavia (accepted for publication). Testing the effect of an arbitrary subject pronoun on relative clause comprehension: A study with Hebrew-speaking children. Journal of Child Language

rm(list=ls())

# loading the data
load("haendler_adani_JCL.RData")

### variables:
# 1) id -- subject id variable (60 levels; 30 adults [A_] and 30 children [C_])
# 2) age -- in months
# 3) gender -- Female / Male
# 4) list -- 1 / 2 (the two lists contain the same items but in a different order)
# 5) Stimulus -- item id variable (29 levels)
# 6) condition -- 4 levels: SR+2DP   -- subject relative with two lexical noun phrases
#                           OR+2DP   -- object relative with two lexical noun phrases
#                           OR+3pro  -- object relative with an embedded referential third-person pronoun (hem)
#                           OR+pro   -- object relative with an embedded non-referential arbitrary subject pronoun
# 7) condition_new -- 5 levels: all conditions are the same as in the "condition" variable except for OR+2DP, which here is split between:
#                               OR+2DP:nores -- object relative with two lexical noun phrases and without a resumptive pronoun (otam) at the end of the sentence
#                               OR+2DP:res   -- object relative with two lexical noun phrases and with a resumptive pronoun
# 8) fds_bds -- participants did two memory tests: a forward (fds) and a backward (bds) repetition of digits of varying length. The fds_bds variable is the average score on the two tests
# 9) accuracy -- dependent variable: 1 is correct answer; 0 is incorrect answer
# 10) error_type -- 0 is no error (equivalent to 1 in the "accuracy" variable)
#                   1 is distractor error (the participant named the color of the pair of animals that are like the target ones, but on the wrong side of the visual scene)
#                   2 is middle error (the participant named the color of the middle pair of animals) 

# creating variable group
data$group <- factor(ifelse(grepl("A_",data$id),"adults","children"))

#----------------------------------------------------------------------------

# checking performance on subject relatives
library(plyr)
sr <- ddply(subset(data,condition=="SR+2DP"), .(group), summarize,
            accuracy = mean(accuracy))

print(levels(sr$group)); print(sr$accuracy)

#----------------------------------------------------------------------------

# Table 1: Adults' and children's proportion of correct responses on object relatives, divided by the type of embedded subject constituent, with 95% confidence intervals
t1 <- droplevels( ddply(subset(data, condition!="SR+2DP"), .(id,group,condition), summarize,
                  accuracy = mean(accuracy)) )

GM <- mean(t1$accuracy) # grand mean
t1 <- ddply(t1, .(id), transform, accuracy.w = accuracy - mean(accuracy) + GM)
nl <- nlevels(t1$condition) # product of levels of within-subject factors
mf <- sqrt( nl/(nl-1) )  # morey factor

t1 <- ddply(t1, .(group,condition), summarize, N = length(accuracy),
            # mean accuracy
            M = mean(accuracy),
            # 95% confidence intervals with Morey's (2008) correction
            CI = sd(accuracy.w)/sqrt(length(accuracy.w))*mf*qt(.975, length(accuracy.w)-1) )

#----------------------------------------------------------------------------

# checking the effect of resumptive pronoun on object relatives with two lexical noun phrases

# excluding subject relatives
no_sr <- droplevels(subset(data,condition!="SR+2DP"))

# aggregating the data - once for adults and once for children
res_prn <- ddply(no_sr[no_sr$group=="children" & grepl("2DP",no_sr$condition_new),],
                 .(id, condition_new), summarize,
                 accuracy=mean(accuracy))

res <- subset(res_prn, condition_new=="OR+2DP:res")
nores <- subset(res_prn, condition_new=="OR+2DP:nores")

t.test(res$accuracy, nores$accuracy)

#----------------------------------------------------------------------------

# comparing adults' performance on object relatives with referential (hem) and non-referential (ASP) pronoun

# only adults
a <- droplevels(subset(data, group=="adults"))

# aggregating
a <- ddply(a[a$condition=="OR+3pro" | a$condition=="OR+pro",],
           .(id, condition), summarize,
           accuracy=mean(accuracy))

hem <- subset(a, condition=="OR+3pro")
asp <- subset(a, condition=="OR+pro")

t.test(hem$accuracy, asp$accuracy, paired=T)

#----------------------------------------------------------------------------

# Figure 2: Adults' proportion of correct responses on the three types of object relatives, divided according to their response on object relatives with 'hem' pronoun

# grouping adults based on their performance on object relatives with 'hem' pronoun
data$a_grouping <- factor( ifelse(data$id=="A_12" |
                                  data$id=="A_13" |
                                  data$id=="A_14" |
                                  data$id=="A_18" | 
                                  data$id=="A_20" | 
                                  data$id=="A_21" | 
                                  data$id=="A_23" |
                                  data$id=="A_24" | 
                                  data$id=="A_26" | 
                                  data$id=="A_28" | 
                                  data$id=="A_30" | 
                                  data$id=="A_5"  |
                                  data$id=="A_6"  |
                                  data$id=="A_7", "correct", # 6 or 7 correct responses on object relatives with 'hem'
                                       
                           ifelse(data$id=="A_11" | 
                                  data$id=="A_16" | 
                                  data$id=="A_27" | 
                                  data$id=="A_29" | 
                                  data$id=="A_8", "middle",
                                  
                                  "incorrect")) ) # 1 or 0 correct responses on object relatives with 'hem'

# aggregating
f2 <- droplevels( ddply(subset(data,condition!="SR+2DP" & group=="adults"),
                          .(id,condition,a_grouping),summarize,
                          accuracy=mean(accuracy)) )

GM <- mean(f2$accuracy) # grand mean
f2 <- ddply(f2, .(id), transform, accuracy.w = accuracy - mean(accuracy) + GM)  
nl <- nlevels(f2$condition) # product of levels of within-subject factors
mf <- sqrt( nl/(nl-1) )  # morey factor

f2 <- ddply(f2, .(condition,a_grouping), summarize, 
            # number of subjects in each group
            N = length(accuracy),
            # mean accuracy
            M = mean(accuracy),
            # 95% confidence intervals
            CI_M = sd(accuracy.w)/sqrt(length(accuracy.w))*mf*qt(.975, length(accuracy.w)-1) )

# changing names of factor levels
levels(f2$condition) <- c("lexical NP", "ASP", "hem")
levels(f2$a_grouping) <- c("consistently correct (N=14)",
                           "consistently incorrect (N=11)",
                           "half-correct/half-incorrect (N=5)")

# plotting
library(ggplot2)

ggplot(data=f2, aes(x=a_grouping, y=M, group=condition,
                   color=condition,fill=condition)) + 
  
  geom_bar(stat="identity", position=position_dodge(), width=.5) +
  
  geom_errorbar(aes(max=M+CI_M, min=M-CI_M,width=.2), size=.1, 
                colour="black",position=position_dodge(width=.5),linetype="solid") + 
  
  geom_point(size=4, position=position_dodge(width=.5)) + theme_bw() +
  
  scale_color_manual(values=c("#000000","#696969","#B8B8B8"), name="embedded subject\nconstituent") +
  scale_fill_manual(values=c("#000000","#696969","#B8B8B8"), name="embedded subject\nconstituent") + 
  
  xlab(expression(paste("grouping by response on object relatives with ", italic("hem"), "'they'"))) + 
  ylab("proportion of correct responses") + 
  
  theme(
    axis.title.y=element_text(size=22, angle=90),
    axis.title.x=element_text(size=22, angle=0),
    axis.text.x=element_text(size=22, color="black"),
    axis.text.y=element_text(size=22, color="black"),
    legend.title=element_text(size=18),
    legend.text=element_text(size=18),
    legend.justification=c(1,1), legend.position=c(1,1),
    legend.background=element_rect(fill="transparent"),
    panel.grid.major=element_blank(),
    panel.background=element_blank())

#----------------------------------------------------------------------------

# children's data analysis

# no adults, no subject relatives
c <- droplevels(subset(data, group=="children" & condition!="SR+2DP"))

# centering the age covariate
c$age_cent <- scale(c$age, scale=T,center=T)

# contrast coding for "condition" - sliding differences
library(MASS)
contrasts(c$condition) <- contr.sdif(3) # 2-1 = OR+pro - OR+2DP
                                        # 3-2 = OR+3pro - OR+pro

# generalized linear mixed model with logit link function
library(lme4)
m <- glmer(accuracy ~ age_cent*condition + 
             
          (1 |id) + (1 |Stimulus),
          
          data=c, family=binomial,
          
          control=glmerControl(optimizer="bobyqa"))

# printing model results (Table 2 shows the fixed effects part)
print(summary(m), corr=F)

#----------------------------------------------------------------------------

# Table 3: The number of children who answered correctly on trials of object relatives with a non-referential arbitrary subject pronoun and with the referential pronoun hem 'they'

# only children, only relevant conditions (object relatives with the two pronouns)
c <- subset(data, group=="children")
c <- droplevels( subset(c, condition=="OR+pro" | condition=="OR+3pro") )

# object relatives with referential pronoun
t3_hem <- ddply(subset(c,condition=="OR+3pro"),
                .(id,condition), summarize,
                N=sum(accuracy))

# how many subjects perform with 0-1 correct answers
nrow(subset(t3_hem, N==0 | N==1))
# how many subjects perform with 2-3 correct answers
nrow(subset(t3_hem, N==2 | N==3))
# how many subjects perform with 4-5 correct answers
nrow(subset(t3_hem, N==4 | N==5))
# how many subjects perform with 6-7 correct answers
nrow(subset(t3_hem, N==6 | N==7))

# object relatives with non-referential pronoun
t3_pro <- ddply(subset(c,condition=="OR+pro"),
                .(id,condition), summarize,
                N=sum(accuracy))

# how many subjects perform with 0-1 correct answers
nrow(subset(t3_pro, N==0 | N==1))
# how many subjects perform with 2-3 correct answers
nrow(subset(t3_pro, N==2 | N==3))
# how many subjects perform with 4-5 correct answers
nrow(subset(t3_pro, N==4 | N==5))
# how many subjects perform with 6-7 correct answers
nrow(subset(t3_pro, N==6 | N==7))

#----------------------------------------------------------------------------

# Figure 3: Children's proportion of correct responses on the three types of object relatives, divided according to their response on object relatives with a non-referential arbitrary subject pronoun

# grouping children based on their performance on object relatives with ASP
data$c_grouping <-   factor( ifelse(data$id=="C_1" |
                                    data$id=="C_14" |
                                    data$id=="C_16" | 
                                    data$id=="C_21" | 
                                    data$id=="C_27" | 
                                    data$id=="C_33" | 
                                    data$id=="C_35", "correct", # 6 or 7 correct responses on object relatives with ASP
                                    
                                    "incorrect")) # 1, 2 or 3 correct responses on object relatives with ASP

# aggregating
f3 <- droplevels( ddply(subset(data,condition!="SR+2DP" & group=="children"),
                        .(id,condition,c_grouping),summarize,
                        accuracy=mean(accuracy)) )

GM <- mean(f3$accuracy) # grand mean
f3 <- ddply(f3, .(id), transform, accuracy.w = accuracy - mean(accuracy) + GM)  
nl <- nlevels(f3$condition) # product of levels of within-subject factors
mf <- sqrt( nl/(nl-1) )  # morey factor

f3 <- ddply(f3, .(condition,c_grouping), summarize, 
            # number of subjects in each group
            N = length(accuracy),
            # mean accuracy
            M = mean(accuracy),
            # 95% confidence intervals
            CI_M = sd(accuracy.w)/sqrt(length(accuracy.w))*mf*qt(.975, length(accuracy.w)-1) )

# changing names of factor levels
levels(f3$condition) <- c("lexical NP", "ASP", "hem")
levels(f3$c_grouping) <- c("high-ASP (N=7)",
                           "low-ASP (N=23)")

# plotting
library(ggplot2)

ggplot(data=f3, aes(x=c_grouping, y=M, group=condition,
                    color=condition,fill=condition)) + 
  
  geom_bar(stat="identity", position=position_dodge(), width=.5) +
  
  geom_errorbar(aes(max=M+CI_M, min=M-CI_M,width=.2), size=.1, 
                colour="black",position=position_dodge(width=.5),linetype="solid") + 
  
  geom_point(size=4, position=position_dodge(width=.5)) + theme_bw() +
  
  scale_color_manual(values=c("#000000","#696969","#B8B8B8"), name="embedded subject\nconstituent") +
  scale_fill_manual(values=c("#000000","#696969","#B8B8B8"), name="embedded subject\nconstituent") + 
  
  xlab("grouping by response on object relatives with ASP") + 
  ylab("proportion of correct responses") + 
  
  theme(
    axis.title.y=element_text(size=22, angle=90),
    axis.title.x=element_text(size=22, angle=0),
    axis.text.x=element_text(size=22, color="black"),
    axis.text.y=element_text(size=22, color="black"),
    legend.title=element_text(size=18),
    legend.text=element_text(size=18),
    legend.justification=c(1,1), legend.position=c(1,1),
    legend.background=element_rect(fill="transparent"),
    panel.grid.major=element_blank(),
    panel.background=element_blank())

#----------------------------------------------------------------------------

# Table 4: children's response types

c <- droplevels(subset(data, condition!="SR+2DP" & group=="children"))

xtabs(~ error_type + condition, c)

# error type 0 = no error
# error type 1 = distractor response
# error type 2 = middle response

#----------------------------------------------------------------------------