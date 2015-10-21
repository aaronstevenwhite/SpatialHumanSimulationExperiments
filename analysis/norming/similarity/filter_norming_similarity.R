data.simil <- read.csv("~/experiments/HumanSimulation/data/norming/similarity/results.preprocessed")

## remove fast responders
data.stats.simil <- summarise(group_by(data.simil, id), 
                        medlogrt=median(log(rt)),
                        iqrlogrt=IQR(log(rt)),
                        logrt25=quantile(log(rt), .25), 
                        logrt75=quantile(log(rt), .75))

data.simil <- mutate(group_by(data.simil, id), 
                              medlogrt=median(log(rt)),
                              iqrlogrt=IQR(log(rt)),
                              logrt25=quantile(log(rt), .25), 
                              logrt75=quantile(log(rt), .75), 
                              lowlogrt=logrt25-1.5*iqrlogrt)

data.filtered.simil <- filter(data.simil, medlogrt > quantile(data.stats.simil$medlogrt, .25) - 1.5*IQR(data.stats.simil$medlogrt))
data.filtered.simil <- filter(data.filtered.simil, iqrlogrt > quantile(data.stats.simil$iqrlogrt, .25) - 1.5*IQR(data.stats.simil$iqrlogrt))
data.filtered.simil <- filter(data.filtered.simil, log(rt) > lowlogrt)

data.filtered.simil <- data.filtered.simil[c('id','verb0','verb1','rating','rt')]

write.csv2(data.filtered.simil,
           file='~/experiments/HumanSimulation/data/norming/similarity/results.filtered',
           row.names=FALSE,
           quote=FALSE)