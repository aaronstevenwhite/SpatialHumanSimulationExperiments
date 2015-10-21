###################
## load packages ##
###################

library(dplyr)
library(reshape2)
library(ggplot2)
library(lme4)
library(tikzDevice)
library(xtable)

#######################
## configure session ##
#######################

root.dir <- '~/experiments/HumanSimulation'
analysis.dir <- file.path(root.dir, 'analysis/norming')
plots.dir <- file.path(analysis.dir, 'plots/')

setwd(analysis.dir)

set.seed(465)

###################
## run filtering ##
###################

source(file.path(root.dir, 'analysis/normalizing_functions.R'))

source(file.path(analysis.dir, 'human_simulation/filter_norming_humsim.R'))
source(file.path(analysis.dir, 'similarity/filter_norming_similarity.R'))

theme_set(theme_bw())


cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

data.filtered.simil <- mutate(group_by(data.filtered.simil, id), ridit=ridit(factor(rating)), zscore=zscore(rating))
simil <- summarise(group_by(data.filtered.simil, verb0, verb1), similarity.ridit=mean(ridit), similarity.zscore=mean(zscore))

data <- merge(data.filtered.humsim, simil, all.x=T, by.x=c('verb', 'responselemma'), by.y=c('verb0', 'verb1'))
data <- merge(data, items.coded)
data[as.character(data$verb) == as.character(data$responselemma),]$similarity.ridit <- 1
data[is.na(data$similarity.ridit),]$similarity.ridit <- 0

m.similarity <- lmer(similarity.zscore ~ lexical*context + (1|id) + (1|verb) + (1|item), data=data)
m.similarity.nointer <- lmer(similarity.zscore ~ lexical+context + (1|id) + (1|verb) + (1|item), data=filter(data, !is.na(similarity.zscore)))
m.similarity.lexicalonly <- lmer(similarity.zscore ~ lexical + (1|id) + (1|verb) + (1|item), data=filter(data, !is.na(similarity.zscore)))
m.similarity.contextonly <- lmer(similarity.zscore ~ context + (1|id) + (1|verb) + (1|item), data=filter(data, !is.na(similarity.zscore)))

anova(m.similarity, m.similarity.nointer, m.similarity.lexicalonly)
anova(m.similarity, m.similarity.nointer, m.similarity.contextonly)

similarity.all <- data %>%
                  group_by(verb, context, lexical, item) %>%
                  summarise(simmean=mean(similarity.ridit))


similarity.all.ordered <- similarity.all[rev(order(similarity.all$verb, similarity.all$context, similarity.all$lexical, similarity.all$simmean)),]

#write.csv(similarity.all.ordered, '/home/aaronsteven/experiments/HumanSimulation/analysis/norming/similarity_order', row.names=F, quote=F)

similarity.all$simtype <- 'All responses'

similarity.only <- filter(data, !(similarity.ridit %in% c(0,1))) %>%
  group_by(verb, context, lexical, item) %>%
  summarise(simmean=mean(similarity.ridit))

similarity.only$simtype <- 'Inaccurate verb responses'

similarity <- rbind(similarity.all, similarity.only)

similarity$verb.ordered <- ordered(similarity$verb, levels=mean.mean.accuracy[order(-mean.mean.accuracy$meanmeanacc),]$verb)

p.similarity.all <- ggplot(filter(similarity, simtype=='All responses'), aes(x=verb.ordered, y=simmean, fill=lexical)) + geom_boxplot() + scale_fill_grey(name='Lexical context', start = .3, end=.7) + facet_grid(context~.) + scale_x_discrete(name='') + scale_y_continuous(name='Mean similarity') + theme(legend.justification=c(1,0), legend.position=c(1,0), legend.background=element_rect(color="black"))
p.similarity.only <- ggplot(filter(similarity, simtype=='Inaccurate verb responses'), aes(x=verb.ordered, y=simmean, fill=lexical)) + geom_boxplot() + scale_fill_grey(name='Lexical context', start = .3, end=.7) + facet_grid(context~.) + scale_x_discrete(name='') + scale_y_continuous(name='Mean similarity') + theme(legend.justification=c(1,0), legend.position=c(1,0), legend.background=element_rect(color="black"))

tikz(file.path(plots.dir, 'humsimnormingsimilarityall.tikz'), width=6, height=4)
p.similarity.all
dev.off()

tikz(file.path(plots.dir, 'humsimnormingsimilarityonly.tikz'), width=6, height=4)
p.similarity.only
dev.off()

data.simil.orig.raw <- read.csv("~/experiments/ProjectionExperiments/data/likert/likert.filtered")
data.simil.orig <- data.simil.orig.raw %>% 
                   group_by(subj) %>%
                   mutate(similarity=ridit(factor(response))) %>%
                   group_by(verb0, verb1) %>%
                   summarise(similarity=mean(similarity))

data.attitudes.ordered <- data.simil.orig[order(-data.simil.orig$similarity),]

data$similarity.zscore <- ifelse(data$responseaccuracy==1, max(data$similarity.zscore), data$similarity.zscore)

m.similarity.syntax <- randomForest(similarity.zscore ~ verb+lexical+context+truewordfreq+complementizer+main.subject+main.object1+main.object2+main.prep+embedded.tense+embedded.subject, data=filter(data, !is.na(similarity.zscore)))

m.similarity.nointer.syntax <- lmer(similarity.zscore ~ lexical+context+complementizer+main.subject+main.object1+main.object2+main.prep+embedded.tense + (1|id) + (1|verb) + (1|item), data=filter(data, !is.na(similarity.zscore)), REML=F)
m.similarity.nointer.syntax.mainsubj <- lmer(similarity.zscore ~ lexical+context+main.subject + (1|id) + (1|verb) + (1|item), data=filter(data, !is.na(similarity.zscore)), REML=F)
m.similarity.nointer.syntax.mainobj1 <- lmer(similarity.zscore ~ lexical+context+main.object1 + (1|id) + (1|verb) + (1|item), data=filter(data, !is.na(similarity.zscore)), REML=F)
m.similarity.nointer.syntax.mainobj2 <- lmer(similarity.zscore ~ lexical+context+main.object2 + (1|id) + (1|verb) + (1|item), data=filter(data, !is.na(similarity.zscore)), REML=F)
m.similarity.nointer.syntax.mainprep <- lmer(similarity.zscore ~ lexical+context+main.prep + (1|id) + (1|verb) + (1|item), data=filter(data, !is.na(similarity.zscore)), REML=F)
m.similarity.nointer.syntax.embtense <- lmer(similarity.zscore ~ lexical+context+embedded.tense + (1|id) + (1|verb) + (1|item), data=filter(data, !is.na(similarity.zscore)), REML=F)

anova(m.similarity.nointer, m.similarity.nointer.syntax.comp)
anova(m.similarity.nointer, m.similarity.nointer.syntax.mainprep)
anova(m.similarity.nointer, m.similarity.nointer.syntax.mainobj2)
anova(m.similarity.nointer, m.similarity.nointer.syntax.mainobj1)
anova(m.similarity.nointer, m.similarity.nointer.syntax.embsubj)
anova(m.similarity.nointer, m.similarity.nointer.syntax.mainsubj)

# write.table(data.attitudes.ordered[seq(1,930,30),c('verb0', 'verb1')],
#           '/home/aaronsteven/experiments/HumanSimulation/analysis/norming/real_verb_pairs',
#           row.names=F, quote=F, col.names=FALSE, sep=',')

# i <- seq(20, 39)
# 
# for (j in seq(0, 600, 40)){
#   results[results$item %in% c(i+j),]$item <- results[results$item %in% c(i+j),]$item - 20
# }
# 
# accmeans <- ddply(results, .(verb, context, lexical, item, truewordfreq), summarise, accmean=mean(responseaccuracy))
# accmeans.ordered <- accmeans[rev(order(accmeans$verb, accmeans$context, accmeans$lexical, accmeans$accmean)),]
# 
# accmeans <- ddply(results, .(verb, lexical, item, truewordfreq), summarise, accmean=mean(responseaccuracy))
# accmeans.cast <- cast(accmeans, verb + item ~ lexical)
# 
# p.accmeans <- ggplot(accmeans, aes(x=verb, y=accmean, fill=lexical)) + geom_boxplot() + scale_fill_manual(values=cbPalette[2:8]) + ggtitle('Distribution of average accuracy')
# p.accmeans.diff <- ggplot(accmeans.cast, aes(x=verb, y=real-nonce))  + geom_hline(yintercept=0) + geom_boxplot(fill=cbPalette[1]) + ggtitle('Difference in average accuracy')
# 
# simmeans <- ddply(results, .(verb, lexical, item, truewordfreq), summarise, simmean=mean(responsesimilarity, na.rm=T))
# simmeans.cast <- cast(simmeans, verb + item ~ lexical)
# 
# p.simmeans <- ggplot(simmeans, aes(x=verb, y=simmean, fill=lexical)) + geom_boxplot() + scale_fill_manual(values=cbPalette[2:8]) + scale_y_continuous(limits=c(0,1)) + ggtitle('Distribution of average similarity (all responses)')
# p.simmeans.diff <- ggplot(simmeans.cast, aes(x=verb, y=real-nonce))  + geom_hline(yintercept=0) + geom_boxplot(fill=cbPalette[1]) + ggtitle('Difference in average similarity (all responses)')
# 
# simmeans.nocorrect <- ddply(subset(results, responseaccuracy != 1), .(verb, lexical, item, truewordfreq), summarise, simmean=mean(responsesimilarity, na.rm=T))
# simmeans.nocorrect <- subset(simmeans.nocorrect, simmean != 'NaN')
# simmeans.nocorrect.cast <- cast(simmeans.nocorrect, verb + item ~ lexical)
# 
# p.simmeans.nocorrect <- ggplot(simmeans.nocorrect, aes(x=verb, y=simmean, fill=lexical)) + geom_boxplot() + scale_fill_manual(values=cbPalette[2:8]) + scale_y_continuous(limits=c(0,1)) + ggtitle('Distribution of average similarity (inaccurate responses only)')
# p.simmeans.nocorrect.diff <- ggplot(simmeans.nocorrect.cast, aes(x=verb, y=real-nonce))  + geom_hline(yintercept=0) + geom_boxplot(fill=cbPalette[1]) + ggtitle('Difference in average similarity (inaccurate responses only)')
# p.simmeans.nocorrect.freq <- ggplot(simmeans.nocorrect, aes(x=truewordfreq, y=simmean, color=lexical)) + geom_point() +geom_smooth() + scale_color_manual(values=cbPalette[2:8]) + scale_y_continuous(limits=c(0,1)) + ggtitle('Distribution of average similarity (inaccurate responses only)')
# 
# p.simmeans.nocorrect.truefreq <- ggplot(simmeans.nocorrect, aes(x=truewordfreq, y=simmean, color=lexical)) + geom_point(position=position_jitter(c(.25,.05))) + scale_color_manual(values=cbPalette[2:8]) + scale_y_continuous(limits=c(0,1)) + ggtitle('Distribution of average similarity (inaccurate responses only)') + geom_smooth(method="lm")
# 
# p.simmeans.nocorrect.truefreq <- ggplot(subset(results, responseaccuracy!=1), aes(x=truewordfreq, y=responsesimilarity, color=lexical)) + geom_point(position=position_jitter(c(.25,.1))) + scale_color_manual(values=cbPalette[2:8]) + scale_y_continuous(limits=c(0,1)) + ggtitle('Similarity by true word frequency (inaccurate responses only)') + geom_smooth(method="lm")
# p.simmeans.nocorrect.responsefreq <- ggplot(subset(results, responseaccuracy!=1), aes(x=responsewordfreq, y=responsesimilarity, color=lexical)) + geom_point() + scale_color_manual(values=cbPalette[2:8]) + scale_y_continuous(limits=c(0,1)) + ggtitle('Similarity by response word frequency (inaccurate responses only)') + geom_smooth(method="lm")
# 
# p.simmeans.accmeans <- ggplot(merge(simmeans.nocorrect, accmeans), aes(x=accmean, y=simmean, color=lexical)) + geom_point() + geom_smooth(method="lm") + scale_color_manual(values=cbPalette[2:8]) + ggtitle('Similarity by accuracy')
# p.simmeans.accmeans.verb <- ggplot(merge(simmeans.nocorrect, accmeans), aes(x=accmean, y=simmean, color=lexical)) + geom_point() + geom_smooth(method="lm") + scale_color_manual(values=cbPalette[2:8]) + ggtitle('Similarity by accuracy (by verb)') + facet_wrap(~verb)
# 
# ## entropy
# 
# probs <- ddply(count(results, .(verb, context, lexical, item, response)), .(verb, context, lexical, item), transform, prob=freq/sum(freq))
# entropy <- ddply(probs, .(verb, context, lexical, item), summarise, entropy=-sum(prob * log2(prob)))
# 
# acc.sim.entropy <- merge(merge(entropy, accmeans), simmeans)
# 
# p.entropy <- ggplot(entropy, aes(x=verb, y=entropy, fill=lexical)) + geom_boxplot()
# p.entropy <- ggplot(entropy, aes(x=context, y=entropy, fill=lexical)) + geom_boxplot()
# 
# m.entropy <- lmer(entropy ~ context+lexical+(1|item)+(1|verb), data=entropy)
