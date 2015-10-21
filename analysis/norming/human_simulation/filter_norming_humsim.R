library(dplyr)
library(reshape2)
library(ggplot2)

theme_set(theme_bw())

data.humsim <- read.delim("~/experiments/HumanSimulation/data/norming/human_simulation/results.preprocessed")

## filter by question accuracy
question.accuracy <- summarise(group_by(data.humsim, id), 
                               qsum=sum(questionaccuracy, na.rm=TRUE)/(n()/40)) # 
question.accuracy.filtered <- filter(question.accuracy, qsum >= 8)
data.filtered.humsim <- filter(data.humsim, id %in% question.accuracy.filtered$id)

## remove fast responders
data.humsim.stats <- summarise(group_by(data.filtered.humsim, id), 
                               medlogrt=median(log(responseRT)),
                               iqrlogrt=IQR(log(responseRT)),
                               logrt25=quantile(log(responseRT), .25), 
                               logrt75=quantile(log(responseRT), .75), 
                               lowlogrt=logrt25-1.5*IQR(log(responseRT)))

data.filtered.humsim <- mutate(group_by(data.filtered.humsim, id), 
                           medlogrt=median(log(responseRT)),
                           iqrlogrt=IQR(log(responseRT)),
                           logrt25=quantile(log(responseRT), .25), 
                           logrt75=quantile(log(responseRT), .75), 
                           lowlogrt=logrt25-1.5*IQR(log(responseRT)))

data.filtered.humsim <- droplevels(filter(data.filtered.humsim, medlogrt > quantile(data.humsim.stats$medlogrt, .25) - 1.5*IQR(data.humsim.stats$medlogrt)))
data.filtered.humsim <- droplevels(filter(data.filtered.humsim, iqrlogrt > quantile(data.humsim.stats$iqrlogrt, .25) - 1.5*IQR(data.humsim.stats$iqrlogrt)))
data.filtered.humsim <- filter(data.filtered.humsim, log(responseRT) > lowlogrt)

## find responses that are not attested words
## (nonattested words will have an NA for responsewordfreq)
data.nonwords <- filter(data.filtered.humsim, is.na(responsewordfreq))

correct.spelling <- function(data.filtered.humsim, incorrect, correct, replaceresponsewordfreq=NA){
  if(is.na(replaceresponsewordfreq)){
    data.filtered.humsim[data.filtered.humsim$response==incorrect,]$responsewordfreq <- as.numeric(filter(data.filtered.humsim, response==correct)[1,'responsewordfreq'])
  } else {
    data.filtered.humsim[data.filtered.humsim$response==incorrect,]$responsewordfreq <- as.numeric(replaceresponsewordfreq)
  }
  
  data.filtered.humsim[data.filtered.humsim$response==incorrect,]$response <- correct
  
  return(data.filtered.humsim)
}

data.filtered.humsim <- correct.spelling(data.filtered.humsim, "think'", "think")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "thwink", "think")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "knkow", "know")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "knokw", "know")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "knlw", "know")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "want?", "want")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "wisj", "wish")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "needf", "needs")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "saww", "saw")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "boughy", "bought")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "gavd", "gave")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "carae", "care")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "exxpect", "expect")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "love's", "loves")

data.nonwords.postcorrection <- filter(data.filtered.humsim, is.na(responsewordfreq))

data.filtered.humsim <- droplevels(filter(data.filtered.humsim, !is.na(responsewordfreq)))

## exclude hand-coded nonwords
nonwords <- c("bac", "boun", "blorp", "bomp", "cant", "conjurs", "couldn't", 
              "dees", "didn", "dg", "dp", "dont", "don't", "dreed", "effin", 
              "ellaborate", "excell", "gange", "gare", "gee", "geeze", "gimme", "gope", "gosh",
              "haunch", "ho", "isn't", "it's", "ken", "magu", "morf", "orked", "plumm", "practic", "scunt", "smag", 
              "sara", "scunt", "slove", "smag", "smark", "spang", "spaz", "steve", "sue", "spiff", "swoom", "tammy", "thats", "theth", 
              "thow", "thwap", "tole", "wat", "wazoo", "weet", "wh", "wont", "wouldn't", "fleep", "flomp", "spered")
data.filtered.humsim <- droplevels(filter(data.filtered.humsim, !(response %in% nonwords)))

data.filtered.humsim <- correct.spelling(data.filtered.humsim, "anymroe", "anymore")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "becuase", "because")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "bellieve", "believe")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "did.", "did")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "doiong", "doing")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "droing", "doing")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "fil", "fill")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "forgt", "forget")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "giv", "give")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "gte", "get")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "haave", "have")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "heared", "heard")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "kno", "know")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "kniw", "know")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "knwo", "know")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "kow", "know")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "konw", "know")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "likr", "like")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "lke", "like")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "lnow", "know")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "mena", "mean")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "sa", "saw")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "saie", "said")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "smike", "smile")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "sleep.", "sleep")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "sned", "send")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "takl", "talk")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "tallk", "talk")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "tel", "tell")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "tel.", "tell")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "tels", "tells")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "thikn", "think")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "thinki", "think")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "thinkk", "think")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "thibk", "think")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "thionk", "think")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "thihnk", "think")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "thinl", "think")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "thinik", "think")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "thought4", "thought")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "thouht", "thought")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "wanr", "want")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "wnat", "want")
data.filtered.humsim <- correct.spelling(data.filtered.humsim, "won.", "won")

data.filtered.humsim <- droplevels(data.filtered.humsim)

responselemmas <- read.csv("~/experiments/HumanSimulation/analysis/norming/human_simulation/responselemmas", sep=";")

data.filtered.humsim <- merge(data.filtered.humsim, responselemmas)

data.filtered.humsim <- data.filtered.humsim[c('id', 'verb', 'context', 'lexical', 'item', 'responselemma', 'responseaccuracy', 'truewordfreq', 'responsewordfreq', 'responseRT')]

#data.filtered.humsim <- filter(data.filtered.humsim, !(responselemma%in%c('fuck', 'shit', 'screw', 'fart', 'poop', 'fornicate', 'masturbate', 'shag', 'burp')))

# write.csv2(data.frame(response=unique(filter(data.filtered.humsim, is.na(responselemma))$response), responselemma=unique(filter(data.filtered.humsim, is.na(responselemma))$response)),
#            file='/home/aaronsteven/experiments/HumanSimulation/analysis/norming/responselemmas',
#            row.names=FALSE,
#            quote=FALSE)