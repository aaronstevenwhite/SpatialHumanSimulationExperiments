###################
## load packages ##
###################

library(dplyr)
library(reshape2)
library(ggplot2)
library(lme4)
library(tikzDevice)
library(xtable)
library(stargazer)

#######################
## configure session ##
#######################

root.dir <- '~/experiments/HumanSimulation'
analysis.dir <- file.path(root.dir, 'analysis/learning')
plots.dir <- file.path(analysis.dir, 'plots/')

setwd(analysis.dir)

set.seed(465)

source('filter_learning.R')
source(file.path(root.dir, 'analysis/normalizing_functions.R'))

data.base <- read.csv("~/experiments/ProjectionExperiments/data/likert/likert.filtered")
data.base.stats <- data.base %>%
                   group_by(subj) %>%
                   mutate(zscore=zscore(response)) %>%
                   group_by(verb0, verb1) %>%
                   summarise(similarity.base=mean(zscore))

data.learning.filtered <- data.learning.filtered %>%
                          group_by(id) %>%
                          mutate(zscore=zscore(rating))
data.learning.filtered.nonce <- filter(data.learning.filtered, comptype=='nonce')

data.nonce <- merge(data.learning.filtered.nonce, 
                    data.base.stats, 
                    by.x=c('verb', 'verb1'), 
                    by.y=c('verb0', 'verb1'))

data.nonce.stats <- data.nonce %>%
                    group_by(id, verb, context, lexical, informativity, trainingsize, rating) %>%
                    mutate(n=n()) %>%
                    group_by(id, verb, context, lexical, informativity, trainingsize) %>%
                    summarise(maxn=max(n), correlation=cor(zscore, similarity.base))

data.nonce.stats$lexical <- relevel(data.nonce.stats$lexical, 'nonce')
data.nonce.stats$informativity <- relevel(data.nonce.stats$informativity, 'low')
data.nonce.stats$trainingsize <- relevel(data.nonce.stats$trainingsize, 'small')

m.full <- lmer(correlation ~ context*lexical*informativity*trainingsize+(1|id)+(1|verb), data=filter(data.nonce.stats, !is.na(correlation)))
m.threeway <- lmer(correlation ~ (context+lexical+informativity+trainingsize)^2+(1|id)+(1|verb), data=filter(data.nonce.stats, !is.na(correlation)))
m.twoway.nolexsize <- lmer(correlation ~ lexical*informativity+informativity*trainingsize+(1|id)+(1|verb), data=filter(data.nonce.stats, !is.na(correlation)))
m.twoway.noinformsize <- lmer(correlation ~ lexical*informativity+lexical*trainingsize+(1|id)+(1|verb), data=filter(data.nonce.stats, !is.na(correlation)))
m.twoway.nolexinform <- lmer(correlation ~ informativity*trainingsize+lexical*trainingsize+(1|id)+(1|verb), data=filter(data.nonce.stats, !is.na(correlation)))

anova(m.full, m.twoway, m.twoway.nolexsize)
anova(m.full, m.twoway, m.twoway.noinformsize)
anova(m.full, m.twoway, m.twoway.nolexinform)

m.twoway.nolexinform.nolexsize <- lmer(correlation ~ informativity*trainingsize+lexical+(1|id)+(1|verb), data=filter(data.nonce.stats, !is.na(correlation)))

anova(m.twoway.nolexsize, m.twoway.nolexinform.nolexsize)
anova(m.twoway.nolexinform, m.twoway.nolexinform.nolexsize)

m.nointer <- lmer(correlation ~ informativity+trainingsize+lexical+(1|id)+(1|verb), data=filter(data.nonce.stats, !is.na(correlation)))

anova(m.twoway.nolexinform.nolexsize, m.nointer)

summary(m.twoway.nolexinform.nolexsize)

data.nonce$informativity <- relevel(data.nonce$informativity, 'low')
data.nonce$trainingsize <- relevel(data.nonce$trainingsize, 'small')

prior <- list(R = list(V = 1, nu = 0.002), G = list(G1 = list(V = diag(2), nu=2), G2 = list(V = diag(16), nu=16)))

m.full <- MCMCglmm(zscore~1+similarity.base*lexical*informativity*trainingsize,
                  random=~idh(1+similarity.base):id+idh(1+similarity.base*lexical*informativity*trainingsize):verb,
                  prior=prior,family="gaussian",
                  data=data.nonce,nitt=25000,burnin=10000,thin=10)

data.nonce$informativity <- relevel(data.nonce$informativity, 'low') 
data.nonce$trainingsize <- relevel(data.nonce$trainingsize, 'small') 

m.full <- lmer(zscore ~ (similarity.base+lexical+informativity+trainingsize)^4 + (1+similarity.base|idunique) + (1+similarity.base|verb), data=data.nonce, REML=F)
m.three <- lmer(zscore ~ (similarity.base+lexical+informativity+trainingsize)^3 + (1+similarity.base|idunique) + (1+similarity.base|verb), data=data.nonce, REML=F)

anova(m.full, m.three)

m.three.notrain <- lmer(zscore ~ (similarity.base+lexical+informativity+trainingsize)^2 + (similarity.base+lexical+informativity)^3 + (1+similarity.base|idunique) + (1+similarity.base|verb), data=data.nonce, REML=F)
m.three.noinform <- lmer(zscore ~ (similarity.base+lexical+informativity+trainingsize)^2 + (similarity.base+lexical+trainingsize)^3 + (1+similarity.base|idunique) + (1+similarity.base|verb), data=data.nonce, REML=F)
m.three.nolexical <- lmer(zscore ~ (similarity.base+lexical+informativity+trainingsize)^2 + (similarity.base+informativity+trainingsize)^3 + (1+similarity.base|idunique) + (1+similarity.base|verb), data=data.nonce, REML=F)

anova(m.three, m.three.notrain)
anova(m.three, m.three.noinform)
anova(m.three, m.three.nolexical)

m.two <- lmer(zscore ~ (similarity.base+lexical+informativity+trainingsize)^2 + (1+similarity.base|idunique) + (1+similarity.base|verb), data=data.nonce, REML=F)

anova(m.two, m.three.nolexical)

data.nonce$predicted <- predict(m.three.nolexical)

p.humsimsimverb <- ggplot(data.nonce, aes(x=similarity.base, y=predicted, linetype=lexical, fill=verb)) + stat_smooth(method="lm", se=F, color="grey", fullrange=T) + facet_grid(trainingsize~informativity) + geom_smooth(aes(x=similarity.base, y=predicted, linetype=lexical, fill=lexical), method="lm", se=F, color="black", size=1.5) + scale_fill_discrete(guide="none") + scale_x_continuous(name='True word similarity (z-score)') + scale_y_continuous(name='Learned word similarity (z-score)')
p.humsimsimsubj <- ggplot(data.nonce, aes(x=similarity.base, y=predicted, linetype=lexical, fill=id)) + stat_smooth(method="lm", se=F, color="grey", fullrange=T) + facet_grid(trainingsize~informativity) + geom_smooth(aes(x=similarity.base, y=predicted, linetype=lexical, fill=lexical), method="lm", se=F, color="black", size=1.5) + scale_fill_discrete(guide="none") + scale_x_continuous(name='True word similarity (z-score)') + scale_y_continuous(name='Learned word similarity (z-score)')

tikz(file.path(plots.dir, 'humsimsimverb.tikz'), width=6, height=4)
p.humsimsimverb
dev.off()

tikz(file.path(plots.dir, 'humsimsimsubj.tikz'), width=6, height=4)
p.humsimsimsubj
dev.off()

random.eff.subj <- as.data.frame(ranef(m.three.nolexical)$idunique)
random.eff.subj$idunique <- rownames(random.eff.subj)

random.eff.subj <- merge(count(data.nonce, idunique, verb, context, lexical, informativity, trainingsize), random.eff.subj)

plot.sims <- function(tsize, inform){
  ggplot(data.nonce) + geom_smooth(aes(x=zscore, y=zscore), color="grey", fullrange=T) + geom_smooth(data=filter(data.nonce, trainingsize==tsize, informativity==inform), method="lm", color="black", fullrange=T, aes(x=zscore, y=similarity.base, linetype=lexical)) + facet_wrap(~verb, ncol=5)   
}
