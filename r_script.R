setwd("~/Documents/github/phono_net")
rt <- read.csv("~/Documents/github/phono_net/mald_acc_rt_9075.csv")

baseline <- lmer(log(RT) ~ scale(PhonUP)+scale(Trial)+scale(Duration)+scale(WordRunLength)+log(FreqSUBTLEX+1)+(1|Subject)+(1|Item),data=rt)

delins <- lmer(log(RT) ~ scale(PhonUP) + scale(Trial) + scale(Duration) + scale(WordRunLength) + log(FreqSUBTLEX + 1) + log(delete_insert + 1) + (1|Subject) + (1|Item),data=rt)

replace <- lmer(log(RT) ~ scale(PhonUP) + scale(Trial) + scale(Duration) + scale(WordRunLength) + log(FreqSUBTLEX + 1) + log(replace + 1) + (1|Subject) + (1|Item),data=rt)

original <- lmer(log(RT) ~ scale(PhonUP) + scale(Trial) + scale(Duration) + scale(WordRunLength) + log(FreqSUBTLEX + 1) + log(original + 1) + (1|Subject) + (1|Item),data=rt)

phonnd <- lmer(log(RT) ~ scale(PhonUP) + scale(Trial) + scale(Duration) + scale(WordRunLength) + log(FreqSUBTLEX + 1) + log(PhonND + 1) + (1|Subject) + (1|Item),data=rt)

l50 <- lmer(log(RT) ~ scale(PhonUP) + scale(Trial) + scale(Duration) + scale(WordRunLength) + log(FreqSUBTLEX + 1) + log(pos_l50 + 1) + (1|Subject) + (1|Item),data=rt)

m50 <- lmer(log(RT) ~ scale(PhonUP) + scale(Trial) + scale(Duration) + scale(WordRunLength) + log(FreqSUBTLEX + 1) + log(pos_m50 + 1) + (1|Subject) + (1|Item),data=rt)


original_cc <- lmer(log(RT) ~ scale(PhonUP) + scale(Trial) + scale(Duration) + scale(WordRunLength) + log(FreqSUBTLEX + 1) + scale(all_cc) + (1|Subject) + (1|Item),data=rt)

replace_cc <- lmer(log(RT) ~ scale(PhonUP) + scale(Trial) + scale(Duration) + scale(WordRunLength) + log(FreqSUBTLEX + 1) + scale(replace_cc) + (1|Subject) + (1|Item),data=rt)

l50_cc <- lmer(log(RT) ~ scale(PhonUP) + scale(Trial) + scale(Duration) + scale(WordRunLength) + log(FreqSUBTLEX + 1) + scale(pos_l50_cc) + (1|Subject) + (1|Item),data=rt)

m50_cc <- lmer(log(RT) ~ scale(PhonUP) + scale(Trial) + scale(Duration) + scale(WordRunLength) + log(FreqSUBTLEX + 1) + scale(pos_m50_cc) + (1|Subject) + (1|Item),data=rt)

delins_and_original_cc <- lmer(log(RT) ~ scale(PhonUP) + scale(Trial) + scale(Duration) + scale(WordRunLength) + log(FreqSUBTLEX + 1) + log(delete_insert + 1) + scale(all_cc)+(1|Subject) + (1|Item),data=rt)

delins_and_replace_cc <- lmer(log(RT) ~ scale(PhonUP) + scale(Trial) + scale(Duration) + scale(WordRunLength) + log(FreqSUBTLEX + 1) + log(delete_insert + 1) + scale(replace_cc)+(1|Subject) + (1|Item),data=rt)


original_degree_and_cc <- lmer(log(RT) ~ scale(PhonUP) + scale(Trial) + scale(Duration) + scale(WordRunLength) + log(FreqSUBTLEX + 1) + log(original + 1)+scale(all_cc) + (1|Subject) + (1|Item),data=rt)

