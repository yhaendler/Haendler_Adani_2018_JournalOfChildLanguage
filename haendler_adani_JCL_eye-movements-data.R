#----------------------------------------------------------------------------
# supplementary script for the article:
# Haendler, Yair & Adani, Flavia (accepted for publication). Testing the effect of an arbitrary subject pronoun on relative clause comprehension: A study with Hebrew-speaking children. Journal of Child Language

## eye-movement data (not presented in the article) ##

## Information about the data:
## eye-tracker used: SMI RED-m, 60Hz
## We excluded from the data set: 
# 1) camera samples with a hit outside any of the three areas of intereest
# 2) trials in which more than 50% of the data consisted of track-loss
## Information about Time -- 2962ms is the onset of the relative pronoun she- "that"; from this point on the conditions start to diverge
#                         -- 4100ms is the average offset of the sentence
#                         -- 7000ms is the average end of the trial (end of the silence that followed the sentence)

rm(list=ls())

# loading the data
load("haendler_adani_JCL_eye-movements-data.RData")

### variables:
# 1) Stimulus -- item id variable (37 levels, including fillers and 4 experimental conditions)
# 2) id -- subject id variable (60 levels; 30 adults [A_] and 30 children [C_])
# 3) group -- adults / children
# 4) x -- location of tracked event on the x-axis
# 5) y -- location of tracked event on the y-axis
# 6) ms -- time in milliseconds
# 7) condition -- 4 levels: SR+2DP   -- subject relative with two lexical noun phrases
#                           OR+2DP   -- object relative with two lexical noun phrases
#                           OR+3pro  -- object relative with an embedded referential third-person pronoun (hem)
#                           OR+pro   -- object relative with an embedded non-referential arbitrary subject pronoun
# 8) direction -- direction of the visual scene -- from left to right (l-r) or from right to left (r-l)
# 9) target_figure -- type of animal that appeared (in a pair) in the target and distractor positions
# 10) target_color -- color of the pair of animals in the target position
# 11) distractor_color -- color of the pair of animals in the distractor position
# 12) middle_figure -- type of animal that appeared (in a pair) in the middle position
# 13) middle_color -- color of the pair of animals in the middle position
# 14) verb -- verb used in a given trial (catch / tickle / wash)
# 15) target.pos -- position of the target pair of animals (left = l / right = r)
# 16) aoi.le -- hit on the left area of interest (aoi)
# 17) aoi.mi -- hit on the middle aoi
# 18) aoi.ri -- hit on the right aoi
# 19) aoi.target -- hit on the target aoi
# 20) aoi.distractor -- hit on the distractor aoi
# 21) aoi.middle -- hit on the middle aoi
# 22) age -- age of a given participant, in months
# 23) gender -- female / male
# 24) list -- 1 / 2 (the two lists contain the same items but in a different order)
# 25) accuracy -- offline dependent variable: 1 is correct answer; 0 is incorrect answer
# 26) error_type -- 0 is no error (equivalent to 1 in the "accuracy" variable)
#                   1 is distractor error (the participant named the color of the pair of animals that are like the target ones, but on the wrong side of the visual scene)
#                   2 is middle error (the participant named the color of the middle pair of animals) 
# 27) fds_bds -- participants did two memory tests: a forward (fds) and a backward (bds) repetition of digits of varying length. The fds_bds variable is the average score on the two tests
# 28) condition_new -- 5 levels: all conditions are the same as in the "condition" variable except for OR+2DP, which here is split between:
#                                OR+2DP:nores -- object relative with two lexical noun phrases and without a resumptive pronoun (otam) at the end of the sentence
#                                OR+2DP:res   -- object relative with two lexical noun phrases and with a resumptive pronoun

#----------------------------------------------------------------------------

# excluding data points outside the average trial time
data <- subset(data, ms<7000)

# aggregating time into 50ms long bins
binsize <- 50
data$bin <- floor(data$ms/binsize)*binsize

#----------------------------------------------------------------------------

## aggregating and plotting ##
library(plyr)

agg1 <- ddply(data, .(id,bin,condition,group),summarize,
              dv = mean(aoi.target) / ( mean(aoi.target) + mean(aoi.middle) + mean(aoi.distractor) ) )

GM <- mean(agg1$dv) # grand mean
agg1 <- ddply(agg1, .(id), transform, dv.w = dv - mean(dv) + GM)  
nl <- nlevels(agg1$condition) * nlevels(agg1$bin) # product of levels of within-subject factors
mf <- sqrt( nl/(nl-1) )  # morey factor

agg2 <- ddply(agg1, .(bin,condition,group), summarize, 
            # number of subjects in each group
            N = length(dv),
            # mean accuracy
            M = mean(dv),
            # 95% confidence intervals
            CI_M = sd(dv.w)/sqrt(length(dv.w))*mf*qt(.975, length(dv.w)-1) )

# chancing the order of levels of "condition"
agg2$condition <- factor(agg2$condition, levels=c("SR+2DP","OR+2DP","OR+pro","OR+3pro"))

# plotting
library(ggplot2)

ggplot(data=agg2, aes(x=bin, y=M, color=condition, group=condition)) +
  
  geom_line(size=2.5) + 
  
  ylab("Proportion of target looks") + 
  xlab("Time (ms)") + theme_bw() + facet_grid(.~group) +
  
  # a vertical line marking the end of the sentence
  geom_vline(aes(xintercept=4100), size=1.5, linetype=2, col=gray(0.2)) + 
  
  theme(axis.title.x=element_text(size=24, angle=0),
        axis.title.y=element_text(size=24, angle=90),
        axis.text.x=element_text(size=22, color="black"),
        axis.text.y=element_text(size=22, color="black"),
        legend.title=element_text(size=26),
        legend.text=element_text(size=26),
        strip.text.x = element_text(size=30, angle=0))
