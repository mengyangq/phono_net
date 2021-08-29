View(items)
library(dplyr)

words <- filter(items,IsWord=="TRUE")

words <- words %>% mutate(idx = row_number())
words <- words %>% select(idx, everything())
words <- subset(words,select=-c(WAV,IsWord,StressPattern,POS,StressCat,AllPOS))