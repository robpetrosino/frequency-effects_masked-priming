library(tidyverse)
library(rstatix)

### databases

nonwords <- read.csv("frequency-effects_nonword-database.csv")
words <- read.csv("frequency-effects_word-database-mono-subtxt.csv")

replaceCommas<-function(x){
  x<-as.numeric(gsub("\\,", "", x))
}

numeric.columns.words <- c(2:44, 52:61)

words <- words %>%
  mutate_at(colnames(words)[numeric.columns.words], ~replaceCommas(.))

nonwords <- nonwords %>%
  mutate_at(colnames(nonwords)[4:11], ~replaceCommas(.))

nonwords <- anti_join(nonwords, words, by='Word') # just making sure that all non-words are *actual* non-words (i.e., without frequencies)

databases <- bind_rows(words, nonwords)

### Experiment 2 suffered a number of problems regarding the word and non-word error rate. Let's find out what the predicted accuracy of these words were, according to the mean accuracy reported in the ELP

stims.exp2 <- read.csv("frequency-effects_experiment2_stimuli.csv") %>% mutate(exp=2) %>%
  left_join(., databases, by='Word')

stims.exp2 %>% 
  filter(type=='word') %>%
  group_by(freq.bin) %>%
  summarise(minLgSUBTF = min(LgSUBTLWF), maxLgSUBTF = max(LgSUBTLWF),
            meanLgSUBTF = mean(LgSUBTLWF), sdLgSUBTF = sd(LgSUBTLWF),
            minFreq= min(Log_Freq_HAL, na.rm = T), maxFreq=max(Log_Freq_HAL, na.rm = T),
            meanFreq = mean(Log_Freq_HAL, na.rm = T), sdFreq = sd(Log_Freq_HAL, na.rm = T),
            meanRT=mean(as.numeric(I_Mean_RT), na.rm = T), sdRT=sd(as.numeric(I_Mean_RT), na.rm = T),
            meanLength = mean(Length, na.rm = T), sdLength=sd(Length, na.rm = T), 
            meanN = mean(Ortho_N, na.rm=T), sdN = sd(Ortho_N, na.rm=T),
            meanAccuracy = mean(I_Mean_Accuracy, na.rm=T), sd=sd(I_Mean_Accuracy, na.rm=T))

stims.exp2 %>% 
  filter(type=='word') %>%
  ggplot(aes(x=I_Mean_Accuracy)) + facet_wrap(~freq.bin) + geom_histogram()

stims.exp2 %>% 
  filter(type=='non-word') %>%
  summarise(meanRT=mean(as.numeric(NWI_Mean_RT), na.rm = T), sdRT=sd(as.numeric(NWI_Mean_RT), na.rm = T),
            meanLength = mean(Length, na.rm = T), sdLength=sd(Length, na.rm = T), 
            meanN = mean(Ortho_N, na.rm=T), sdN = sd(Ortho_N, na.rm=T),
            meanAccuracy = mean(NWI_Mean_Accuracy, na.rm=T), sd=sd(NWI_Mean_Accuracy, na.rm=T))

stims.exp2 %>% 
  filter(type=='non-word') %>%
  ggplot(aes(x=NWI_Mean_Accuracy)) + geom_histogram()

#### Experiment 3 will take into account the mean accuracy as reported in the ELP as an additional filtering condition for the item sampling.

### EXPERIMENT 3 - FIRST OPTION #####


bad.words <- c("broke", "drunk", "found", "heard", "maybe", "third", "twice", "worse", "bitch", "idiot", "least", "might", "still", 
               "stunk", "stung", "spunk", "speed", "naked", "spank") 

low <- words %>% drop_na(SUBTLWF) %>% 
  filter(!Word %in% bad.words,
         between(LgSUBTLWF, min(LgSUBTLWF), 20), 
         between(I_Mean_Accuracy, 0.9, 1),
         Length==5,
         !grepl("[er|y|or|ic|ism|ist|en|al]$", Word),
         grepl("NN||JJ||VB", POS),
         !grepl("minor", POS),
         str_detect(Word, "^[^[:upper:]]+$"), 
         I_Mean_RT!='#')

high <- words %>% drop_na(SUBTLWF) %>% 
  filter(!Word %in% bad.words,
         between(SUBTLWF, 50, max(SUBTLWF)), 
         between(I_Mean_Accuracy, 0.9, 1),
         Length==5,
         grepl("NN|JJ|VB)", POS),
         !grepl("[er|y|or|ic|ism|ist|en]$", Word),
         !grepl("minor", POS),
         str_detect(Word, "^[^[:upper:]]+$"), 
         I_Mean_RT!='#')

lowSample <- low %>% sample_n(100) %>% mutate(freq.bin="low")
highSample <- high %>% sample_n(100) %>% mutate(freq.bin="high")

mergeSample <- bind_rows(lowSample, highSample) %>% mutate(type='word') %>%
  select(Word, type, I_Mean_RT, I_Mean_Accuracy, freq.bin, LgSUBTLWF, SUBTLWF, Log_Freq_HAL, Freq_HAL, Length, Ortho_N)

mergeSample %>% group_by(freq.bin) %>%
  summarise(#meanSUBTF = mean(SUBTLWF), sdSUBTF = sd(SUBTLWF),
            meanLgSUBTF = mean(LgSUBTLWF), sdLgSUBTF = sd(LgSUBTLWF),
            minLgSUBTF = min(LgSUBTLWF), maxLgSUBTF = max(LgSUBTLWF),
            meanSUBTF = mean(SUBTLWF), sdSUBTF = sd(SUBTLWF),
            minSUBTF = min(SUBTLWF), maxSUBTF = max(SUBTLWF),
            #meanHAL = mean(Log_Freq_HAL), sdHAL = sd(Freq_HAL),
            meanLgHAL = mean(Log_Freq_HAL), sdLgHAL = sd(Log_Freq_HAL),
            minLgHAL = min(Log_Freq_HAL), maxLgHAL = max(Log_Freq_HAL),
            meanRT=mean(as.numeric(I_Mean_RT)), sdRT=sd(as.numeric(I_Mean_RT)),
            meanAccuracy=mean(as.numeric(I_Mean_Accuracy), na.rm = T), sdAccuracy=sd(as.numeric(I_Mean_Accuracy), na.rm = T),
            meanLength = mean(Length), sdLength=sd(Length))

anova(lm(Log_Freq_HAL~freq.bin, data=mergeSample))

## non-words

bad.non.words <- c('sceep', 'affox', 'adbum', 'scock', 'ittro', 'ablow', 'louth', 'shawt', 'usurz', 'uszip', 'fuack',
                   'usurz', 'twank', 'tixth', 'huick', 'nylaw', 'chilo', 'eggem', 'flyem', 'oubdo', 'rusto',
                   'outbo', 'ownem', 'pipem', 'brism', 'umarm', 'axiab', 'bakem', 'feeth', 'clith', 'aloem', 'thogh')

good.nonwords <- nonwords %>% filter(Length==5,
                                 between(NWI_Mean_Accuracy, 0.9, 1),
                                 str_detect(Word, "^[^[:upper:]|^']+$"), 
                                   !Word %in% bad.non.words,
                                 grepl("[^ed|er|y|al|ing|est|ic|np|sex|u]$", Word))

sample_nw <- good.nonwords %>% sample_n(200) %>% mutate(type = 'non-word')

## merge all together
dataset <- bind_rows(mergeSample, sample_nw) %>%
  mutate(type = factor(type)) %>%
  unite("Mean_Accuracy", I_Mean_Accuracy, NWI_Mean_Accuracy, na.rm=T) %>%
  unite("Mean_RT", I_Mean_RT, NWI_Mean_RT, na.rm=T)

# and check for length across type (word vs non-word)
dataset %>% group_by(type) %>%
  summarise(meanLength = mean(Length), sdLength=sd(Length))

summary(lm(Mean_RT ~ type, data=dataset))
summary(lm(Mean_Accuracy ~ type, data=dataset))

summary(lm(Mean_RT ~ freq.bin, data=subset(dataset, type=='word')))
summary(lm(Mean_Accuracy ~ freq.bin, data=subset(dataset, type=='word')))
summary(lm(Ortho_N ~ freq.bin, data=dataset))

### double check word properties
dataset %>% group_by(freq.bin, type) %>%
  summarise(#minLgSUBTF = min(LgSUBTLWF), maxLgSUBTF = max(LgSUBTLWF),
            #meanLgSUBTF = mean(LgSUBTLWF), sdLgSUBTF = sd(LgSUBTLWF),
            minSUBTF = min(SUBTLWF), maxSUBTF = max(SUBTLWF),
            meanSUBTF = mean(SUBTLWF), sdSUBTF = sd(SUBTLWF),
            minFreq= min(Freq_HAL, na.rm = T), maxFreq=max(Freq_HAL, na.rm = T),
            meanFreq = mean(Freq_HAL, na.rm = T), sdFreq = sd(Freq_HAL, na.rm = T),
            meanRT=mean(as.numeric(Mean_RT), na.rm = T), sdRT=sd(as.numeric(Mean_RT), na.rm = T),
            meanAccuracy=mean(as.numeric(Mean_Accuracy), na.rm = T), sdAccuracy=sd(as.numeric(Mean_Accuracy), na.rm = T),
            meanN = mean(Ortho_N, na.rm=T), sdN = sd(Ortho_N, na.rm=T))

write.csv(dataset, 'frequency-effects_experiment3_stimuli.csv', row.names=F)

dataset %>% 
  ggplot(aes(x=as.numeric(Mean_Accuracy))) + facet_wrap(type~freq.bin) + geom_histogram(bins=4) +
  ggtitle("First option -- 13-point difference")


### EXPERIMENT 3 - SECOND OPTION #####

bad.words <- c("broke", "drunk", "found", "heard", "maybe", "third", "twice", "worse", "bitch", "idiot", "least", "might", "still", 
               "stunk", "stung", "spunk", "speed", "naked", "spank") 

low_2 <- words %>% drop_na(SUBTLWF) %>% 
  filter(!Word %in% bad.words,
         between(SUBTLWF, min(SUBTLWF), 15), 
         between(I_Mean_Accuracy, 0.95, 1),
         Length==5,
         !grepl("[er|y|or|ic|ism|ist|en|al]$", Word),
         grepl("NN||JJ||VB", POS),
         !grepl("minor", POS),
         str_detect(Word, "^[^[:upper:]]+$"), 
         I_Mean_RT!='#')

high_2 <- words %>% drop_na(SUBTLWF) %>% 
  filter(!Word %in% bad.words,
         between(SUBTLWF, 20, max(SUBTLWF)), 
         between(I_Mean_Accuracy, 0.9, 1),
         Length==5,
         grepl("NN|JJ|VB)", POS),
         !grepl("[er|y|or|ic|ism|ist|en]$", Word),
         !grepl("minor", POS),
         str_detect(Word, "^[^[:upper:]]+$"), 
         I_Mean_RT!='#')

lowSample_2 <- low_2 %>% sample_n(100) %>% mutate(freq.bin="low")
highSample_2 <- high_2 %>% sample_n(100) %>% mutate(freq.bin="high")

mergeSample_2 <- bind_rows(lowSample_2, highSample_2) %>% mutate(type='word') %>%
  select(Word, type, I_Mean_RT, I_Mean_Accuracy, freq.bin, SUBTLWF, Freq_HAL, Length, Ortho_N)

mergeSample_2 %>% group_by(freq.bin) %>%
  summarise(#meanSUBTF = mean(SUBTLWF), sdSUBTF = sd(SUBTLWF),
    meanSUBTF = mean(SUBTLWF), sdSUBTF = sd(SUBTLWF),
    minSUBTF = min(SUBTLWF), maxSUBTF = max(SUBTLWF),
    #meanHAL = mean(Log_Freq_HAL), sdHAL = sd(Freq_HAL),
    meanHAL = mean(Freq_HAL), sdLgHAL = sd(Freq_HAL),
    minHAL = min(Freq_HAL), maxLgHAL = max(Freq_HAL),
    meanRT=mean(as.numeric(I_Mean_RT)), sdRT=sd(as.numeric(I_Mean_RT)),
    meanAccuracy=mean(as.numeric(I_Mean_Accuracy), na.rm = T), sdAccuracy=sd(as.numeric(I_Mean_Accuracy), na.rm = T),
    meanLength = mean(Length), sdLength=sd(Length))

anova(lm(SUBTLWF~freq.bin, data=mergeSample_2))
anova(lm(I_Mean_Accuracy~freq.bin, data=mergeSample_2))


good.nonwords_2 <- nonwords %>% filter(Length==5,
                                     between(NWI_Mean_Accuracy, 0.93, 1),
                                     str_detect(Word, "^[^[:upper:]|^']+$"), 
                                     !Word %in% bad.non.words,
                                     grepl("[^ed|er|y|al|ing|est|ic|np|sex|u]$", Word))

sample_nw_2 <- good.nonwords_2 %>% sample_n(200) %>% mutate(type = 'non-word')

## merge all together
dataset_2 <- bind_rows(mergeSample_2, sample_nw_2) %>%
  mutate(type = factor(type)) %>%
  unite("Mean_Accuracy", I_Mean_Accuracy, NWI_Mean_Accuracy, na.rm=T) %>%
  unite("Mean_RT", I_Mean_RT, NWI_Mean_RT, na.rm=T)

# and check for length across type (word vs non-word)
dataset_2 %>% group_by(type) %>%
  summarise(meanLength = mean(Length), sdLength=sd(Length))

summary(lm(Mean_RT ~ type, data=dataset_2))
summary(lm(Mean_Accuracy ~ type, data=dataset_2))

summary(lm(Mean_RT ~ freq.bin, data=subset(dataset_2, type=='word')))
summary(lm(Mean_Accuracy ~ freq.bin, data=subset(dataset_2, type=='word')))
summary(lm(Ortho_N ~ freq.bin, data=dataset_2))

### double check word properties
dataset_2 %>% group_by(freq.bin, type) %>%
  summarise(minSUBTF = min(SUBTLWF), maxSUBTF = max(SUBTLWF),
            meanSUBTF = mean(SUBTLWF), sdSUBTF = sd(SUBTLWF),
            minFreq= min(Freq_HAL, na.rm = T), maxFreq=max(Freq_HAL, na.rm = T),
            meanFreq = mean(Freq_HAL, na.rm = T), sdFreq = sd(Freq_HAL, na.rm = T),
            meanRT=mean(as.numeric(Mean_RT), na.rm = T), sdRT=sd(as.numeric(Mean_RT), na.rm = T),
            meanAccuracy=mean(as.numeric(Mean_Accuracy), na.rm = T), sdAccuracy=sd(as.numeric(Mean_Accuracy), na.rm = T),
            meanN = mean(Ortho_N, na.rm=T), sdN = sd(Ortho_N, na.rm=T))

write.csv(dataset, 'frequency-effects_experiment3_second_stimuli.csv', row.names=F)

dataset_2 %>% 
  ggplot(aes(x=as.numeric(Mean_Accuracy))) + facet_wrap(type~freq.bin) + geom_histogram(bins=4) +
  ggtitle("Second option -- 5-point difference")

### EXPERIMENT 3 - THIRD OPTION #####

bad.words <- c("broke", "drunk", "found", "heard", "maybe", "third", "twice", "worse", "bitch", "idiot", "least", "might", "still", "forth",
               "stunk", "stung", "spunk", "speed", "naked", "spank", "loath", "tramp", "swank", "frank", "champ", "prick", "drank", "bound") 

low_3 <- words %>% drop_na(SUBTLWF) %>% 
  filter(!Word %in% bad.words,
         between(SUBTLWF, 7, 25), 
         between(I_Mean_Accuracy, 0.97, 1),
         Length==5,
         !grepl("[er|y|or|ic|ism|ist|en|al]$", Word),
         grepl("NN||JJ||VB", POS),
         !grepl("minor", POS),
         str_detect(Word, "^[^[:upper:]]+$"), 
         I_Mean_RT!='#')

high_3 <- words %>% drop_na(SUBTLWF) %>% 
  filter(!Word %in% bad.words,
         between(SUBTLWF, 56, max(SUBTLWF)), 
         between(I_Mean_Accuracy, 0.9, 1),
         Length==5,
         grepl("NN|JJ|VB)", POS),
         !grepl("[er|y|or|ic|ism|ist|en]$", Word),
         !grepl("minor", POS),
         str_detect(Word, "^[^[:upper:]]+$"), 
         I_Mean_RT!='#')

lowSample_3 <- low_3 %>% sample_n(52) %>% mutate(freq.bin="low")
highSample_3 <- high_3 %>% sample_n(52) %>% mutate(freq.bin="high")

mergeSample_3 <- bind_rows(lowSample_3, highSample_3) %>% mutate(type='word') %>%
  select(Word, type, I_Mean_RT, I_Mean_Accuracy, freq.bin, SUBTLWF, Freq_HAL, Length, Ortho_N)

mergeSample_3 %>% group_by(freq.bin) %>%
  summarise(#meanSUBTF = mean(SUBTLWF), sdSUBTF = sd(SUBTLWF),
    meanSUBTF = mean(SUBTLWF), sdSUBTF = sd(SUBTLWF),
    minSUBTF = min(SUBTLWF), maxSUBTF = max(SUBTLWF),
    #meanHAL = mean(Log_Freq_HAL), sdHAL = sd(Freq_HAL),
    meanHAL = mean(Freq_HAL), sdLgHAL = sd(Freq_HAL),
    minHAL = min(Freq_HAL), maxLgHAL = max(Freq_HAL),
    meanRT=mean(as.numeric(I_Mean_RT)), sdRT=sd(as.numeric(I_Mean_RT)),
    meanAccuracy=mean(as.numeric(I_Mean_Accuracy), na.rm = T), sdAccuracy=sd(as.numeric(I_Mean_Accuracy), na.rm = T),
    meanLength = mean(Length), sdLength=sd(Length))

anova(lm(SUBTLWF~freq.bin, data=mergeSample_3))
anova(lm(I_Mean_Accuracy~freq.bin, data=mergeSample_3))


good.nonwords_3 <- nonwords %>% filter(Length==5,
                                       between(NWI_Mean_Accuracy, 0.95, 1),
                                       str_detect(Word, "^[^[:upper:]|^']+$"), 
                                       !Word %in% bad.non.words,
                                       grepl("[^ed|er|y|al|ing|est|ic|np|sex|u]$", Word))

sample_nw_3 <- good.nonwords_3 %>% sample_n(104) %>% mutate(type = 'non-word')

anti_nonwords <- anti_join(good.nonwords_3, sample_nw_3, by='Word') %>% slice_sample(n=10)

## merge all together
dataset_3 <- bind_rows(mergeSample_3, sample_nw_3) %>%
  mutate(type = factor(type)) %>%
  unite("Mean_Accuracy", I_Mean_Accuracy, NWI_Mean_Accuracy, na.rm=T) %>%
  unite("Mean_RT", I_Mean_RT, NWI_Mean_RT, na.rm=T)

# and check for length across type (word vs non-word)
dataset_3 %>% group_by(type) %>%
  summarise(meanLength = mean(Length), sdLength=sd(Length))

summary(lm(Mean_RT ~ type, data=dataset_3))
summary(lm(Mean_Accuracy ~ type, data=dataset_3))

summary(lm(Mean_RT ~ freq.bin, data=subset(dataset_3, type=='word')))
summary(lm(Mean_Accuracy ~ freq.bin, data=subset(dataset_3, type=='word')))
summary(lm(Ortho_N ~ freq.bin, data=dataset_3))

### double check word properties
dataset_3 %>% group_by(freq.bin, type) %>%
  summarise(n = n(), minSUBTF = min(SUBTLWF), maxSUBTF = max(SUBTLWF),
            meanSUBTF = mean(SUBTLWF), sdSUBTF = sd(SUBTLWF),
            minFreq= min(Freq_HAL, na.rm = T), maxFreq=max(Freq_HAL, na.rm = T),
            meanFreq = mean(Freq_HAL, na.rm = T), sdFreq = sd(Freq_HAL, na.rm = T),
            meanRT=mean(as.numeric(Mean_RT), na.rm = T), sdRT=sd(as.numeric(Mean_RT), na.rm = T),
            meanAccuracy=mean(as.numeric(Mean_Accuracy), na.rm = T), sdAccuracy=sd(as.numeric(Mean_Accuracy), na.rm = T),
            meanN = mean(Ortho_N, na.rm=T), sdN = sd(Ortho_N, na.rm=T))

write.csv(dataset_3, 'frequency-effects_experiment3_third_stimuli.csv', row.names=F)

dataset_3 %>% 
  ggplot(aes(x=as.numeric(Mean_Accuracy))) + facet_wrap(type~freq.bin) + geom_histogram(bins=4)

