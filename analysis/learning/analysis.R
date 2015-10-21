## load packages
library(plyr)
library(reshape)
library(ggplot2)

theme_set(theme_bw())

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## functions
zscore <- function(x){
  mean.x <- mean(x)
  sd.x <- sd(x)
  
  out <- (x - mean.x) / sd.x
  
  return(out)
}

## load data
original.data <- read.csv("~/experiments/ProjectionExperiments/data/likert/likert.filtered")
learning.data <- read.csv("~/experiments/HumanSimulation/data/learning/results.filtered")

## means
original.data <- ddply(original.data, .(subj), transform, z=zscore(response))
learning.data <- ddply(learning.data, .(subj), transform, z=zscore(response))

original.data.means <- ddply(original.data, .(verb0, verb1), summarise, meanz.original=mean(z))
learning.data.means <- ddply(learning.data, .(verb0, verb1), summarise, meanz.original=mean(z))

learning.data.means <- subset(learning.data.means, verb0 != 'amaze' & verb1 != 'amaze')

cor(t(as.matrix(cast(learning.data.means, comparison + verb ~ verb1))))


## 

data.real <- merge(original.data.means, subset(learning.data, comparison=='truesim')[c('subj', 'verb', 'verb0', 'verb1', 'lexical', 'comparison', 'z')])
data.nonce <- merge(original.data.means, subset(learning.data, comparison=='noncesim')[c('subj', 'verb', 'verb1', 'lexical', 'comparison', 'z')], by.x=c('verb0', 'verb1'), by.y=c('verb', 'verb1'))

data.nonce$verb <- data.nonce$verb0

data <- rbind(data.real, data.nonce)
data$comparison <- relevel(data$comparison, 'truesim')
data$verb <- relevel(data$verb, 'think')

summary(m <- lmer(z ~ verb*meanz.original*comparison*lexical+(1|subj), data=data))
anova(m)

ggplot(data, aes(x=meanz.original, y=z, color=comparison)) + geom_hline(yintercept=0) + geom_vline(xintercept=0) + geom_point() + facet_grid(verb~lexical) + theme_bw() + scale_colour_manual(values=cbPalette)
ggplot(data, aes(x=meanz.original, y=z, color=comparison)) + geom_hline(yintercept=0) + geom_vline(xintercept=0) + geom_smooth(method="lm", fullrange=T) + facet_grid(verb~lexical) + theme_bw() + scale_colour_manual(values=cbPalette)
ggplot(subset(data, comparison=='noncesim'), aes(x=meanz.original, y=z)) + geom_hline(yintercept=0) + geom_vline(xintercept=0) + geom_smooth(aes(fill=subj), method="lm", fullrange=T, color="#E69F00", se=F) + geom_smooth(data=subset(data, comparison=='truesim'), aes(x=meanz.original, y=z), method="lm", fullrange=T, size=2, color="#999999") + facet_grid(verb~lexical) + theme_bw() + theme(legend.position="none")

## correlations

correlations <- ddply(data, .(verb, lexical, comparison, subj), summarise, correlation=cor(meanz.original, z))
correlations.cast <- cast(correlations, verb + lexical + subj ~ comparison)

correlations.cast$cordiff <- correlations.cast$noncesim - correlations.cast$truesim

ggplot(correlations.cast, aes(x=verb, y=cordiff, fill=lexical)) + geom_boxplot() + scale_fill_manual(values=cbPalette[c(2,1)])