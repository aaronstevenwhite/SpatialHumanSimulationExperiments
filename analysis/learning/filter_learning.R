library(dplyr)
library(lme4)

## load data
memory.learning <- read.csv("~/experiments/HumanSimulation/data/learning/memory.preprocessed")
data.learning <- read.csv("~/experiments/HumanSimulation/data/learning/results.preprocessed")

## filter by memory task
memory.learning$informativity <- relevel(memory.learning$informativity, 'low')
memory.learning$trainingsize <- relevel(memory.learning$trainingsize, 'small')

m.memory <- glmer(accuracy ~ lexical*informativity*trainingsize + (1|id) + (1|verb), family="binomial", data=memory.learning)
m.memory.twoway <- glmer(accuracy ~ (lexical+informativity+trainingsize)^2 + (1|id) + (1|verb), family="binomial", data=memory.learning)

anova(m.memory, m.memory.twoway)

blups <- ranef(m.memory)$id
names(blups) <- 'intercept'
blups$id <- rownames(blups)

good.memory <- filter(blups, intercept > mean(blups$intercept)-2*sqrt(var(blups$intercept)))
good.memory.ids <- unique(good.memory$id)

data.learning.filtered <- filter(data.learning, id %in% good.memory.ids)

## filter by RTs
data.learning.filtered <- filter(data.learning.filtered, rt > 0)

## remove fast responders
data.learning.filtered <- mutate(group_by(data.learning.filtered, id), 
                        medlogrt=median(log(rt)),
                        iqrlogrt=IQR(log(rt)),
                        logrt25=quantile(log(rt), .25), 
                        logrt75=quantile(log(rt), .75), 
                        lowlogrt=logrt25-1.5*iqrlogrt)

bad.responders.med <- filter(data.learning.filtered, medlogrt < quantile(data.learning.filtered$medlogrt, .25) - 1.5*IQR(data.learning.filtered$medlogrt))
bad.responders.iqr <- filter(data.learning.filtered, iqrlogrt < quantile(data.learning.filtered$iqrlogrt, .25) - 1.5*IQR(data.learning.filtered$iqrlogrt) | iqrlogrt > quantile(data.learning.filtered$iqrlogrt, .75) + 1.5*IQR(data.learning.filtered$iqrlogrt))
bad.responses <- filter(data.learning.filtered, log(rt) < lowlogrt)

data.learning.filtered <- filter(data.learning.filtered, medlogrt > quantile(data.learning.filtered$medlogrt, .25) - 1.5*IQR(data.learning.filtered$medlogrt))
data.learning.filtered <- filter(data.learning.filtered, iqrlogrt > quantile(data.learning.filtered$iqrlogrt, .25) - 1.5*IQR(data.learning.filtered$iqrlogrt), iqrlogrt < quantile(data.learning.filtered$iqrlogrt, .75) + 1.5*IQR(data.learning.filtered$iqrlogrt))
data.learning.filtered <- filter(data.learning.filtered, log(rt) > lowlogrt)

data.learning.filtered <- droplevels(data.learning.filtered[names(data.learning.filtered)[1:11]])

data.learning.filtered$idunique <- paste(data.learning.filtered$id, data.learning.filtered$verb, data.learning.filtered$context, data.learning.filtered$lexical, data.learning.filtered$informativity, data.learning.filtered$trainingsize, sep='')

write.csv2(data.learning.filtered[c(12,2:11)],
           file='~/experiments/HumanSimulation/data/learning/results.filtered',
           row.names=FALSE,
           quote=FALSE)