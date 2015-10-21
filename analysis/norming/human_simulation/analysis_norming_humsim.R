###################
## load packages ##
###################

library(dplyr)
library(reshape2)
library(ggplot2)
library(lme4)
library(randomForest)
library(tikzDevice)
library(xtable)
library(stargazer)

####################
## configure session
####################

root.dir <- '~/experiments/HumanSimulation'
analysis.dir <- file.path(root.dir, 'analysis/norming/human_simulation')
plots.dir <- file.path(root.dir, 'analysis/norming/plots')

setwd(analysis.dir)

set.seed(465)

###################
## run filtering ##
###################

source('filter_norming_humsim.R')

##
##
##

## accuracy analysis
mean.accuracy <- data.filtered.humsim %>%
  group_by(lexical, context, verb, item) %>%
  summarise (meanacc=mean(responseaccuracy))

mean.mean.accuracy <- summarise(group_by(mean.accuracy, verb), meanmeanacc=mean(meanacc))
mean.accuracy$verb.ordered <- ordered(mean.accuracy$verb, levels=mean.mean.accuracy[order(-mean.mean.accuracy$meanmeanacc),]$verb)

p.accuracy <- ggplot(mean.accuracy, aes(x=verb.ordered, y=meanacc, fill=lexical)) + geom_boxplot() + scale_fill_grey(name='Lexical context', start = .3, end=.7) + facet_grid(context~.) + scale_x_discrete(name='') + scale_y_continuous(name='Accuracy') + theme(legend.justification=c(1,0), legend.position=c(1,.75), legend.background=element_rect(color="black"))

tikz(paste(plots.dir, 'humsimnormingaccuracy.tikz', sep=''), width=6, height=4)
p.accuracy
dev.off()

data.filtered.humsim$lexicalcontext <- paste(data.filtered.humsim$lexical, data.filtered.humsim$context, sep='')

## same as nested (1|verb/item) = (1|verb) + (1|item:verb)
## But this equals (1|item) + (1|verb) since items are already unique to verbs
m.accuracy <- glmer(responseaccuracy ~ lexical*context + (1|id) + (1|verb) + (1|item), family="binomial", data=data.filtered.humsim)
m.accuracy.nointer <- glmer(responseaccuracy ~ lexical+context + (1|id) + (1|verb) + (1|item), family="binomial", data=data.filtered.humsim)
m.accuracy.lexicalonly <- glmer(responseaccuracy ~ lexical + (1|id) + (1|verb) + (1|item), family="binomial", data=data.filtered.humsim)
m.accuracy.contextonly <- glmer(responseaccuracy ~ context + (1|id) + (1|verb) + (1|item), family="binomial", data=data.filtered.humsim)

anova(m.accuracy, m.accuracy.nointer, m.accuracy.lexicalonly)
anova(m.accuracy, m.accuracy.nointer, m.accuracy.contextonly)

stargazer(m.accuracy.lexicalonly)

data.filtered.humsim$truewordfreqnorm <- data.filtered.humsim$truewordfreq-min(data.filtered.humsim$truewordfreq)

m.accuracy.lexicalonly.truefreq <- glmer(responseaccuracy ~ lexical+truewordfreqnorm + (1|id) + (1|verb) + (1|item), family="binomial", data=data.filtered.humsim)
m.accuracy.lexicalonly.truefreqinter <- glmer(responseaccuracy ~ lexical*truewordfreqnorm + (1|id) + (1|verb) + (1|item), family="binomial", data=data.filtered.humsim)

anova(m.accuracy.lexicalonly, m.accuracy.lexicalonly.truefreq, m.accuracy.lexicalonly.truefreqinter)

stargazer(m.accuracy.lexicalonly.truefreq)

items.coded <- read.csv("~/experiments/HumanSimulation/materials/learning/items_coded.csv")

data.filtered.humsim <- merge(data.filtered.humsim, items.coded)

m.accuracy.lexicalonly.comp <- glmer(responseaccuracy ~ lexical + complementizer + (1|id) + (1|verb) + (1|item), family="binomial", data=data.filtered.humsim)
m.accuracy.lexicalonly.comp <- glmer(responseaccuracy ~ lexical + main.subject + (1|id) + (1|verb) + (1|item), family="binomial", data=data.filtered.humsim)
m.accuracy.lexicalonly.comp <- glmer(responseaccuracy ~ lexical + embedded.subject + (1|id) + (1|verb) + (1|item), family="binomial", data=data.filtered.humsim)
m.accuracy.lexicalonly.comp <- glmer(responseaccuracy ~ lexical + embedded.tense + (1|id) + (1|verb) + (1|item), family="binomial", data=data.filtered.humsim)
m.accuracy.lexicalonly.comp <- glmer(responseaccuracy ~ lexical + main.object1 + (1|id) + (1|verb) + (1|item), family="binomial", data=data.filtered.humsim)
m.accuracy.lexicalonly.comp <- glmer(responseaccuracy ~ lexical + main.object2 + (1|id) + (1|verb) + (1|item), family="binomial", data=data.filtered.humsim)


## get unique pairs of true word and response
true.response.counts <- data.filtered.humsim %>%
  group_by(lexical, verb, responselemma, item) %>%
  summarise (n = n()) %>%
  group_by(lexical, verb, item) %>%
  mutate(freq = n/sum(n)) %>%
  group_by(lexical, verb, responselemma) %>%
  summarise(freqmean = mean(c(freq, rep(0, 40-n()))), 
            freqlow = quantile(c(freq, rep(0, 40-n())),  0.0),
            freqhigh = quantile(c(freq, rep(0, 40-n())),  1.0)) %>%
  arrange(lexical, verb, -freqmean) %>%
  group_by(lexical, verb) %>%
  mutate(rank=row_number())

# write.csv2(unique(true.response.counts[c('responselemma')]),
#            file='/home/aaronsteven/experiments/HumanSimulation/analysis/norming/human_simulation/unique_responselemmas',
#            row.names=FALSE,
#            quote=FALSE)

p.responses <- ggplot(true.response.counts, aes(x=-rank, y=freqmean, ymax=freqhigh, ymin=freqlow, label=responselemma, fill=lexical)) + geom_bar(stat="identity", position="dodge", color="black") + geom_errorbar(position=position_dodge(.9), width=.1) + geom_text(hjust=-.25,position=position_dodge(.9), aes(y=freqhigh), size=2) + scale_x_continuous(name='Rank of by-item median proportion', breaks=-10:-1, limits=c(-10.5,-.5), labels=10:1) + scale_y_continuous(name='Relative frequency', breaks=seq(0,1,.1), limits=c(0,1.15)) + scale_fill_grey(name='Lexical context') + coord_flip() + theme_bw() + geom_hline(yintercept=0) + theme(legend.justification=c(1,0), legend.position=c(1,0), legend.background=element_rect(color="black")) + facet_wrap(~verb, nrow=5)

# tikz(file.path(plots.dir, 'humsimnormingresponses.tikz'), width=7, height=10)
# p.responses
# dev.off()

plot.verb.responses <- function(v) {
  verb.responses <- filter(true.response.counts, verb %in% v)
  p <- ggplot(true.response.counts, aes(x=-rank, y=freqmean, ymax=freqhigh, ymin=freqlow, label=responselemma, fill=lexical)) + geom_bar(stat="identity", position="dodge", color="black") + geom_errorbar(position=position_dodge(.9), width=.1) + geom_text(hjust=-.25,position=position_dodge(.9), aes(y=freqhigh)) + scale_x_continuous(name='Rank of by-item mean accuracy', breaks=-10:-1, limits=c(-10.5,-.5), labels=10:1) + scale_y_continuous(name='Relative frequency', breaks=seq(0,1,.1), limits=c(0,1.15)) + scale_fill_grey(name='Lexical context') + coord_flip() + theme_bw() + geom_hline(yintercept=0) + theme(legend.justification=c(1,0), legend.position=c(1,0), legend.background=element_rect(color="black"))

  if(length(v) > 1){
    p <- p + facet_wrap(~verb)
  }
  
  return(p)
}

##

unique.responselemmas.filtered <- read.table("~/experiments/HumanSimulation/analysis/norming/human_simulation/unique_responselemmas", header=TRUE, quote="\"")
verb.responses <- droplevels(unique.responselemmas.filtered[grep('[*]', unique.responselemmas.filtered$responselemma, invert=TRUE),])

true.response.counts <- filter(true.response.counts, responselemma %in% verb.responses)

# write.csv2(true.response.counts[c('verb', 'responselemma')],
#            file='/home/aaronsteven/experiments/HumanSimulation/analysis/norming/human_simulation/unique_truelemma_responselemma_pairs',
#            row.names=FALSE,
#            quote=FALSE)

# write.csv2(data.filtered.humsim[c('id', 'item', 'verb', 'context', 'lexical', 'verbinflected', 'response', 'responselemma', 'responseaccuracy')],
#            file='/home/aaronsteven/experiments/HumanSimulation/data/norming/human_simulation/results.filtered',
#            row.names=FALSE,
#            quote=FALSE)

mean.error <- data.filtered.humsim %>%
  group_by(lexical, context, verb, item) %>%
  summarise(meanerr=mean(!(responselemma %in% verb.responses)))

mean.error$verb.ordered <- ordered(mean.error$verb, levels=mean.mean.accuracy[order(-mean.mean.accuracy$meanmeanacc),]$verb)

p.error <- ggplot(mean.error, aes(x=verb.ordered, y=meanerr, fill=lexical)) + geom_boxplot() + scale_fill_grey(name='Lexical context', start = .3, end=.7) + facet_grid(context~.) + scale_x_discrete(name='') + scale_y_continuous(name='Proportion nonverb responses') + theme(legend.justification=c(1,0), legend.position=c(1,0.25), legend.background=element_rect(color="black"))

# tikz(file.path(plots.dir, 'humsimnormingnonverb.tikz'), width=6, height=4)
# p.error
# dev.off()

mean.acc.err <- merge(mean.accuracy, mean.error)
cor(mean.acc.err$meanacc, mean.acc.err$meanerr, method="spearman")

data.filtered.humsim$nonverb <- !(data.filtered.humsim$responselemma %in% verb.responses)

m.syntax <- randomForest(as.factor(responseaccuracy) ~ verb + lexical + context + complementizer + main.subject + embedded.subject + main.object1 + main.object2 + embedded.tense + truewordfreq + main.prep, data=filter(data.filtered.humsim, !nonverb))
m.syntax <- ctree(as.factor(responseaccuracy) ~ verb + lexical + context + complementizer + main.subject + embedded.subject + main.object1 + main.object2 + embedded.tense + main.prep, data=data.filtered.humsim)

m.error <- glmer(nonverb ~ lexical*context + (1|id) + (1|verb) + (1|item), family="binomial", data=data.filtered.humsim)
m.error.nointer <- glmer(nonverb ~ lexical+context + (1|id) + (1|verb) + (1|item), family="binomial", data=data.filtered.humsim)
m.error.nocontext <- glmer(nonverb ~ lexical + (1|id) + (1|verb) + (1|item), family="binomial", data=data.filtered.humsim)
m.error.nolexical <- glmer(nonverb ~ context + (1|id) + (1|verb) + (1|item), family="binomial", data=data.filtered.humsim)

anova(m.error, m.error.nointer, m.error.nocontext)
anova(m.error, m.error.nointer, m.error.nolexical)

sentences.ordered <- read.table("~/experiments/HumanSimulation/materials/learning/sentences_ordered", quote="\"", sep='|')
sentences.ordered$nword <- sapply(gregexpr("[[:alpha:]]+", sentences.ordered$V6), length)

data.filtered.humsim <- merge(data.filtered.humsim, sentences.ordered[c('V1', 'nword')], by.x='item', by.y='V1')

m.error.nointer.nword <- glmer(nonverb ~ lexical+context+nword + (1|id) + (1|verb) + (1|item), family="binomial", data=data.filtered.humsim)

anova(m.error.nointer, m.error.nointer.nword)

# trueinflected.response.counts <- data.filtered.humsim %>%
#   group_by(lexical, verb, verbinflected, response) %>%
#   summarise (n = n()) %>%
#   group_by(lexical, verb, verbinflected) %>%
#   mutate(freq = n / sum(n)) %>% 
#   arrange(lexical, verb, verbinflected, -n) %>%
#   group_by(lexical, verb, verbinflected) %>%
#   mutate(rank=row_number())