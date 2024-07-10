######################### FREQUENCY ATTENUATION EFFECT IN MASKED PRIMING ######################
##################################### MATERIAL PREP ###########################################
## author: Roberto Petrosino
## affiliation: New York University Abu Dhabi
## contact: rp3650@nyu.edu
## last mod: Mar 15, 2024

# loading packages
library(tidyverse)

### databases

nonwords <- read.csv("experiment-1_nonword-database.csv")
words <- read.csv("experiment-1_word-database-mono.csv")

## words

low <- words %>% filter(between(Log_Freq_HAL, 3, 5), Length==5,
                        str_detect(Word, "^[^[:upper:]]+$"), 
                        I_Mean_RT!='#')
mid <- words %>% filter(between(Log_Freq_HAL, 6, 8), Length==5,
                        str_detect(Word, "^[^[:upper:]]+$"), 
                        I_Mean_RT!='#', !is.na(I_Mean_RT))
high <- words %>% filter(between(Log_Freq_HAL, 10, 12), Length==5,
                         str_detect(Word, "^[^[:upper:]]+$"), 
                         I_Mean_RT!='#', !is.na(I_Mean_RT))

lowSample <- low %>% na.exclude %>% sample_n(100) %>% mutate(freq.bin="low")
midSample <- mid %>% na.exclude %>% sample_n(100) %>% mutate(freq.bin="mid")
highSample <- high %>% na.exclude %>% sample_n(100) %>% mutate(freq.bin="high")

mergeSample <- bind_rows(lowSample, midSample, highSample) %>% mutate(type='word')
mergeSample$I_Mean_RT <- eeptools::decomma(mergeSample$I_Mean_RT)
mergeSample$I_Mean_RT <- as.numeric(mergeSample$I_Mean_RT)

mergeSample %>% group_by(freq.bin) %>%
  summarise(meanRT=mean(as.numeric(I_Mean_RT)), sdRT=sd(as.numeric(I_Mean_RT)),
            meanLength = mean(Length), sdLength=sd(Length))

anova(lm(Length~freq.bin, data=mergeSample))
anova(lm(Log_Freq_HAL~freq.bin, data=mergeSample))

## non-words

sample_nw <- nonwords %>% filter(Length==5 & 
                                   str_detect(Word, "^[^[:upper:]|^']+$") &
                                   grepl("[^s|^ed|^er|^y|^al|^ing]$", Word)) %>%
  sample_n(300, weight=Length) %>% mutate(type = 'non-word')

## find unrelated primes for non-words

extra.words <- anti_join(words, mergeSample, by="Word") %>%
  filter(between(Log_Freq_HAL, 3, 12), Length==5,
         str_detect(Word, "^[^[:upper:]]+$"), 
         I_Mean_RT!='#', !is.na(I_Mean_RT)) %>% sample_n(150) %>%
  mutate(I_Mean_RT = as.numeric(I_Mean_RT), type='extra.words')

## merge all together
dataset <- bind_rows(mergeSample, sample_nw, extra.words)

# and check for length across type (word vs non-word)
dataset %>% group_by(type) %>%
  summarise(meanLength = mean(Length), sdLength=sd(Length))

anova(lm(Length~type, data=dataset))
 