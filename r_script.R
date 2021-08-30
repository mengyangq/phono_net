setwd("~/Documents/github/phono_net")
library(lme4)

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


mald_baseline <- lmer(log(RT) ~ scale(PhonUP)+scale(Trial)+scale(Duration)+scale(WordRunLength)+log(FreqSUBTLEX+1)+(1|Subject)+(1|Item),data=mald)
mald_all <- lmer(log(RT) ~ scale(PhonUP)+scale(Trial)+scale(Duration)+scale(WordRunLength)+log(FreqSUBTLEX+1)+log(AllD+1)+(1|Subject)+(1|Item),data=mald)
mald_replace <- lmer(log(RT) ~ scale(PhonUP)+scale(Trial)+scale(Duration)+scale(WordRunLength)+log(FreqSUBTLEX+1)+log(ReplaceD+1)+(1|Subject)+(1|Item),data=mald)
mald_delins <- lmer(log(RT) ~ scale(PhonUP)+scale(Trial)+scale(Duration)+scale(WordRunLength)+log(FreqSUBTLEX+1)+log(DelinsD+1)+(1|Subject)+(1|Item),data=mald)
#mald_phonnd <- lmer(log(RT) ~ scale(PhonUP)+scale(Trial)+scale(Duration)+scale(WordRunLength)+log(FreqSUBTLEX+1)+log(PhonND+1)+(1|Subject)+(1|Item),data=mald)


af_baseline <- lmer(log(RT) ~ scale(PhonUP)+scale(TrialNo)+scale(Dur)+log(FreqSUBTLEX+1)+(1|SubjectID)+(1|Item),data=af)
#af_baseline2 <- lmer(log(RT) ~ scale(PhonUP)+scale(TrialNo)+scale(Dur)+log(FreqSUBTLEX+1)+(1|SubjectID)+(1|Item),data=af)
#af_baseline3 <- lmer(log(RT) ~ scale(PhonUP)+scale(Dur)+log(FreqSUBTLEX+1)+(1|SubjectID)+(1|Item),data=af)

af_all <- lmer(log(RT) ~ scale(PhonUP)+scale(TrialNo)+scale(Dur)+log(FreqSUBTLEX+1)+log(AllD+1)+(1|SubjectID)+(1|Item),data=af)
af_replace <- lmer(log(RT) ~ scale(PhonUP)+scale(TrialNo)+scale(Dur)+log(FreqSUBTLEX+1)+log(ReplaceD+1)+(1|SubjectID)+(1|Item),data=af)
af_delins <- lmer(log(RT) ~ scale(PhonUP)+scale(TrialNo)+scale(Dur)+log(FreqSUBTLEX+1)+log(DelinsD+1)+(1|SubjectID)+(1|Item),data=af)
#af_phonnd <- lmer(log(RT) ~ scale(PhonUP)+scale(TrialNo)+scale(Dur)+log(FreqSUBTLEX+1)+log(PhonND+1)+(1|SubjectID)+(1|Item),data=af)



am_baseline <- lmer(log(RT) ~ scale(PhonUP)+scale(TrialNo)+scale(Dur)+log(FreqSUBTLEX+1)+(1|SubjectID)+(1|Item),data=am)

am_all <- lmer(log(RT) ~ scale(PhonUP)+scale(TrialNo)+scale(Dur)+log(FreqSUBTLEX+1)+log(original+1)+(1|SubjectID)+(1|Item),data=am)
am_replace <- lmer(log(RT) ~ scale(PhonUP)+scale(TrialNo)+scale(Dur)+log(FreqSUBTLEX+1)+log(ReplaceD+1)+(1|SubjectID)+(1|Item),data=am)
am_delins <- lmer(log(RT) ~ scale(PhonUP)+scale(TrialNo)+scale(Dur)+log(FreqSUBTLEX+1)+log(DelinsD+1)+(1|SubjectID)+(1|Item),data=am)
#am_phonnd <- lmer(log(RT) ~ scale(PhonUP)+scale(TrialNo)+scale(Dur)+log(FreqSUBTLEX+1)+log(PhonND+1)+(1|SubjectID)+(1|Item),data=am)




mald_baseline <- lmer(log(RT) ~ scale(PhonUP)+scale(Trial)+scale(Duration)+scale(WordRunLength)+log(FreqSUBTLEX+1)+(1|Subject)+(1|Item),data=mald)
mald_all <- lmer(log(RT) ~ scale(PhonUP)+scale(Trial)+scale(Duration)+scale(WordRunLength)+log(FreqSUBTLEX+1)+log(original+1)+(1|Subject)+(1|Item),data=mald)
mald_replace <- lmer(log(RT) ~ scale(PhonUP)+scale(Trial)+scale(Duration)+scale(WordRunLength)+log(FreqSUBTLEX+1)+log(replace+1)+(1|Subject)+(1|Item),data=mald)
mald_delins <- lmer(log(RT) ~ scale(PhonUP)+scale(Trial)+scale(Duration)+scale(WordRunLength)+log(FreqSUBTLEX+1)+log(delete_insert+1)+(1|Subject)+(1|Item),data=mald)
mald_phonnd <- lmer(log(RT) ~ scale(PhonUP)+scale(Trial)+scale(Duration)+scale(WordRunLength)+log(FreqSUBTLEX+1)+log(PhonND+1)+(1|Subject)+(1|Item),data=mald)


af_baseline <- lmer(log(RT) ~ scale(PhonUP)+scale(TrialNo)+scale(Dur)+log(FreqSUBTLEX+1)+(1|SubjectID)+(1|Item),data=af)
af_all <- lmer(log(RT) ~ scale(PhonUP)+scale(TrialNo)+scale(Dur)+log(FreqSUBTLEX+1)+log(original+1)+(1|SubjectID)+(1|Item),data=af)
af_replace <- lmer(log(RT) ~ scale(PhonUP)+scale(TrialNo)+scale(Dur)+log(FreqSUBTLEX+1)+log(replace+1)+(1|SubjectID)+(1|Item),data=af)
af_delins <- lmer(log(RT) ~ scale(PhonUP)+scale(TrialNo)+scale(Dur)+log(FreqSUBTLEX+1)+log(delete_insert+1)+(1|SubjectID)+(1|Item),data=af)
af_phonnd <- lmer(log(RT) ~ scale(PhonUP)+scale(TrialNo)+scale(Dur)+log(FreqSUBTLEX+1)+log(PhonND+1)+(1|SubjectID)+(1|Item),data=af)



am_baseline <- lmer(log(RT) ~ scale(PhonUP)+scale(TrialNo)+scale(Dur)+log(FreqSUBTLEX+1)+(1|SubjectID)+(1|Item),data=am)
am_all <- lmer(log(RT) ~ scale(PhonUP)+scale(TrialNo)+scale(Dur)+log(FreqSUBTLEX+1)+log(original+1)+(1|SubjectID)+(1|Item),data=am)
am_replace <- lmer(log(RT) ~ scale(PhonUP)+scale(TrialNo)+scale(Dur)+log(FreqSUBTLEX+1)+log(replace+1)+(1|SubjectID)+(1|Item),data=am)
am_delins <- lmer(log(RT) ~ scale(PhonUP)+scale(TrialNo)+scale(Dur)+log(FreqSUBTLEX+1)+log(delete_insert+1)+(1|SubjectID)+(1|Item),data=am)
am_phonnd <- lmer(log(RT) ~ scale(PhonUP)+scale(TrialNo)+scale(Dur)+log(FreqSUBTLEX+1)+log(PhonND+1)+(1|SubjectID)+(1|Item),data=am)



AIC(mald_baseline)
AIC(mald_all)
AIC(mald_replace)
AIC(mald_delins)
AIC(mald_phonnd)

AIC(af_baseline)
AIC(af_all)
AIC(af_replace)
AIC(af_delins)
AIC(af_phonnd)

AIC(am_baseline)
AIC(am_all)
AIC(am_replace)
AIC(am_delins)
AIC(am_phonnd)




summary(mald_baseline)
summary(mald_all)
summary(mald_replace)
summary(mald_delins)
summary(mald_phonnd)

summary(af_baseline)
summary(af_all)
summary(af_replace)
summary(af_delins)
summary(af_phonnd)

summary(am_baseline)
summary(am_all)
summary(am_replace)
summary(am_delins)
summary(am_phonnd)











french_all <- lm(log(Megalex.auditory.rt.m)~log(freqfilms2+1)+log(all+1),data=french)
french_replace <- lm(log(Megalex.auditory.rt.m)~log(freqfilms2+1)+log(replace+1),data=french)
french_delins <- lm(log(Megalex.auditory.rt.m)~log(freqfilms2+1)+log(delins+1),data=french)


french_all2 <- lm(Megalex.auditory.zrt.m~log(freqfilms2+1)+log(all+1),data=french)
french_replace2 <- lm(Megalex.auditory.zrt.m~log(freqfilms2+1)+log(replace+1),data=french)
french_delins2 <- lm(Megalex.auditory.zrt.m~log(freqfilms2+1)+log(delins+1),data=french)

summary(french_all)
summary(french_replace)
summary(french_delins)


summary(french_all2)
summary(french_replace2)
summary(french_delins2)
