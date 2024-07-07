# Coordination in adolescents and young adults. 
# Serena Stagnitto & Gabriele Chierchia

packages=c("ggplot2","readxl", "dplyr", "tidyr")
lapply(packages, require, character.only = TRUE)

# Objectives: 
# 0.5) Capire e riprodurre adolescent preprocessing
# 1) Extend preprocessing to adult data
# 2) Incorporate adolescents and adult data with additional variable indicating
# age (continuous and categorical)
# 3) Extend graphical and statistical analysis to DVs of interest (Choices,
# entropy, payoffs, RTs) to age.
# 4) Explore correlates of interest (mentalising, non-verbal reasoning,
# sociometrics)

# Preprocessing ----
# path.out = "/Users/serenamariastagnitto/Dropbox/Serene&Gabriele/sPT in children_adolescents/DATA/T1_preprocessed"

# coord <- read.csv("/Users/serenamariastagnitto/Dropbox/Serene&Gabriele/sPT in children_adolescents/DATA/T1 - DATA ottobre:novembre22/data_exp_103231-v1/COORDINATION.task/data_exp_103231-v1_task-dm9k.csv",
#                       header=TRUE, sep=",", dec=",")

# GC
coord <- read.csv("/Users/gabrielechierchia/Dropbox/Serene&Gabriele/Coordination/Data/coordination/adolescenti/data_exp_103231-v1_task-dm9k.csv",
                      header=TRUE, sep=",", dec=",")
names(coord)[56] <- "trial.number"
names(coord)[57] <- "left.column"
names(coord)[58] <- "right.column"

coord = coord%>% dplyr::mutate_if(is.character, as.factor)

# Choose cols of interest
coord.sel = coord[,c('Participant.Private.ID', 'Display', 'Screen','Reaction.Time', 
                     'Trial.Number', 'Object.Name', 'trial.number', 'left.column','right.column')]
colnames(coord.sel)[1]='sub' 

# Removing lines that do NOT have Screen=="trial"
coord.sel = droplevels(coord.sel[coord.sel$Screen=="trial",])
coord.sel=droplevels(coord.sel[coord.sel$sub!="7310608",])

# Convert RTs from factor to numeric: 
coord.sel$Reaction.Time=as.numeric(as.character(coord.sel$Reaction.Time))
coord.sel$rt.out=0
coord.sel$rt.out[coord.sel$Reaction.Time>(mean(coord.sel$Reaction.Time)+2.5*sd(coord.sel$Reaction.Time)) |
                   coord.sel$Reaction.Time<(mean(coord.sel$Reaction.Time)-2.5*sd(coord.sel$Reaction.Time))]=1


#adding column answer, indicating the type of answer participants choose: SP (sure pay) or UP (unsure pay)
coord.sel$left.response="SP"
coord.sel$left.response[coord.sel$left.column == '15soloseacaso.png'] <- "UP"
coord.sel$left.response[coord.sel$left.column == '15soloseinsieme.png'] <- "UP"
coord.sel$left.response[coord.sel$left.column == '15solosesoli.png'] <- "UP"


coord.sel$right.response="SP"
coord.sel$right.response[coord.sel$right.column == '15soloseacaso.png'] <- "UP"
coord.sel$right.response[coord.sel$right.column == '15soloseinsieme.png'] <- "UP"
coord.sel$right.response[coord.sel$right.column == '15solosesoli.png'] <- "UP"

coord.sel$answer=coord.sel$left.response
coord.sel$answer[coord.sel$Object.Name == 'right'] <- coord.sel$right.response[coord.sel$Object.Name == 'right']

summary(coord.sel)

#adding column "response" indicating 0 = SP(sure pay); 1= UP (unsure pay)
coord.sel$response=0
coord.sel$response[coord.sel$answer=="UP"]=1
summary(coord.sel)
str(coord.sel)

write.csv(coord.sel, file.path(path.out,"COORDINATION.game_preprocessed.csv"))
#trial.number indicates the SP value for each trial 

coord.cond = coord.sel %>%
  group_by(sub, Display) %>%
  summarise(sum = sum(response),
            mean = mean(response),
            mean.rt = mean(Reaction.Time), 
            prop = sum(response)/15,
            median.rt = median(Reaction.Time), 
            count.trial = length(response),
            flag.trial = if(length(response)==15){0}else{1},
            flag.variance = if(var(response)==0){1}else{0}) 

summary(coord.cond)
str(coord.cond)
table(coord.cond$flag.variance) #27 Display-cases (out of 567) in which the variance is equal to 0 (i.e., 567 / 3 conditions = 189 participants)

write.csv(coord.cond, file.path(path.out,"coord.conditions.csv"))

coord.sel$Display = factor(coord.sel$Display, levels = c("coordination", "lottery", "competition"))
summary(coord.sel)

# RESPONSE CHOICE ----
## grafico ----
ggplot(coord.sel, aes(trial.number, response, col = Display, fill =Display, group=Display))+
  geom_smooth()+stat_summary(fun.data = "mean_cl_boot")


ggplot(coord.sel, aes(trial.number, response,
                      col = factor(Display, c("coordination", "lottery", "competition")),
                      fill =factor(Display, c("coordination", "lottery", "competition")),
                      group=factor(Display, c("coordination", "lottery", "competition"))))+
  geom_smooth()+stat_summary(fun.y = mean, alpha=0.5)+
  theme_minimal()+
  labs(x="Sure payoff",y = "Prop. of UP choice")+
  theme(legend.title= element_blank())
  #quello aggiudicato!


ggplot(coord.sel, aes(trial.number, response, col = Display, fill =Display))+
  geom_smooth()+facet_wrap(~Display)+stat_summary()

geom_jitter(width=0, height=0.3, alpha=0.2)


## grafico risposte medie per condizione ----

ggplot(coord.cond, 
       aes(x=factor(Display, level= c('coordination','lottery', 'competition')), y=mean, fill=Display)) +
  #scale_fill_brewer(palette="Blues") +
  theme_classic() +
  stat_boxplot()+
  stat_summary(fun.data=mean_se,geom='errorbar', width=.3)
#+geom_point(position=pj, alpha=0.5, size=0.5)
#+geom_line(aes(group=sub), alpha=0.2)
#+labs(x="conditions", y="UP") + theme(legend.position = "right")



##repeated measure anova ----

coord.cond <- read.csv("/Users/serenamariastagnitto/Dropbox/Serene&Gabriele/sPT in children_adolescents/DATA/T1_preprocessed/coord.conditions.csv",
                       header=TRUE, sep=",", dec=",")
coord.cond = coord.cond%>% dplyr::mutate_if(is.character, as.factor)
str(coord.cond)
coord.cond$mean <- as.numeric(coord.cond$mean)
library(rstatix)
coord.cond$sub <- as.factor(coord.cond$sub)

anova <- aov(mean ~ Display + Error(sub/Display), data=coord.cond)
summary(anova)
pairwise.t.test(coord.cond$mean, coord.cond$Display, p.adjust.method = "bonferroni")

wilcox.test(mean ~ Display, paired=T, coord.cond[coord.cond$Display!= "coordination",])
wilcox.test(mean ~ Display, paired=T, coord.cond[coord.cond$Display!= "lottery",])
wilcox.test(mean ~ Display, paired=T, coord.cond[coord.cond$Display!= "competition",])


## glmer responses ----
library(lme4)
library(lmerTest)
library(emmeans)
coord.sel$tn.s = scale(coord.sel$trial.number, scale=F)
coord.sel$sub = as.factor(coord.sel$sub)
glmer <- glmer(response ~ tn.s * Display + (tn.s+Display | sub), family="binomial", 
               control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)),
               data = coord.sel)
summary(glmer)
emmeans(glmer, pairwise~Display, adjust="bonferroni")

glmer.choice <- glmer(response ~ trial.number * Display + (trial.number+Display | sub), family="binomial", 
                      control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)),
                      data = coord.sel)
summary(glmer.choice)
Anova(glmer.choice, type=3)
emmeans(glmer.choice, pairwise~Display, adjust="bonferroni")

library(lme4)
tidy(glmer.choice,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
cc <- confint(glmer.choice,parm="beta_") 


summary(coord.sel)

modplot=glmer.choice
m.newdat <- expand.grid(
  trial.number=unique(coord.sel$trial.number),
  Display=c("coordination","lottery", "competition"),
  response = 0
)
m.mm <- model.matrix(terms(modplot),m.newdat)
m.newdat$response <- m.mm %*% fixef(modplot)
m.pvar1 <- diag(m.mm %*% tcrossprod(vcov(modplot),m.mm))
# so this is fucked up. I'm trying to understand what it is exactly.
# Plotting model (doesn't look too good, confidence intervals are really big)
m.newdat <- data.frame(
  m.newdat
  , m.plo = binomial()$linkinv(m.newdat$response-1.96*sqrt(m.pvar1))
  , m.phi = binomial()$linkinv(m.newdat$response+1.96*sqrt(m.pvar1))
)
m.newdat$response = plogis(m.newdat$response)
View(m.newdat)
summary(m.newdat)
ggplot(m.newdat, aes(trial.number, response, group = Display))+geom_line(aes(col = Display), size=1)+
  geom_ribbon(aes(ymin=m.plo, ymax=m.phi, fill = Display), alpha=0.2)+
  geom_point(aes(x=trial.number, y=response), size=2, alpha=0.4, pch=21, colour="black")+
  theme(legend.justification=c(1,1), legend.position=c(1,1))+xlab("Sure payoff")+
  ylab("Probability of UP choice")+theme_bw()+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=16))+
  scale_x_continuous(breaks=seq(0, 15, 2.5))+facet_wrap(~Display)



## Effect size ----
library(rstatix)
coord.sel$Display <- factor(coord.sel$Display,levels = c('coordination','lottery', 'competition'))

#coordination - competition
coord.comp <- droplevels(coord.sel[coord.sel$Display!="lottery", ])
coord.comp %>% 
  cohens_d(response ~ Display, paired = TRUE, ci=T)

t.test(coord.comp$response~ coord.comp$Display, paired=T)

library(psych)

#coordination - lottery
coord.lott <- droplevels(coord.sel[coord.sel$Display!="competition",])
summary(coord.lott)
coord.lott %>% 
  cohens_d(response ~ Display, paired = TRUE, ci=T)
t.test(coord.lott$response~ coord.lott$Display, paired=T)




#dani ----
install.packages('compute.es')
require(plyr)
require(compute.es)

cord_agg <- ddply(coord.new,
                  c('Display'),
                  summarize,
                  m_resp = mean(response),
                  sd_resp = sd(response))

cord_agg

mes(.41,.49,.49,.50,189,189)

# RTs ----
View(coord.cond)

ggplot(droplevels(coord.sel[coord.sel$rt.out==0,]), aes(trial.number, log(Reaction.Time)))+
  geom_boxplot(outlier.shape=2)+
  geom_jitter(alpha=0.02, width=0.1,height=0)+
  theme_minimal()+facet_wrap(~Display)

coord.sel$rt.log = log(coord.sel$Reaction.Time)
qqnorm(log(coord.sel$Reaction.Time[coord.sel$rt.out==0]), )


ggplot(coord.sel[coord.sel$rt.out==0,], aes(log(Reaction.Time)))+
  geom_histogram()+
  facet_wrap(~Display)


rt.sub.cond = coord.sel[coord.sel$rt.out==0,] %>%
  group_by()

coord.cond.rt = coord.sel[coord.sel$rt.out==0 & coord.sel$response==1, ] %>%
  group_by(sub, Display) %>%
  summarise(mean.rt = mean(Reaction.Time), 
            median.rt = median(Reaction.Time)) 

coord.rt.wide <- na.omit(coord.cond.rt %>% pivot_wider(id_cols="sub",names_from="Display",values_from=c("median.rt")))

wilcox.test(coord.rt.wide$coordination, coord.rt.wide$competition, paired=T)
wilcox.test(coord.rt.wide$coordination, coord.rt.wide$lottery, paired=T)
wilcox.test(coord.rt.wide$competition, coord.rt.wide$lottery, paired=T)

library(car)
glmer.rt <- glmer(Reaction.Time ~ Display + (Display| sub), family=Gamma(link="log"),
                             contrasts=list(Display="contr.sum"), 
                  data=coord.sel[coord.sel$response==1,])
Anova(glmer.rt, type=3)
emmeans(glmer.rt, pairwise~Display, adjust="none")
#no differences in RTs in risky choices comparing conditions

glmer.rt.choice <- glmer(Reaction.Time ~ Display*response + (1| sub), family=Gamma(link="log"),
                  contrasts=list(Display="contr.sum"), 
                  data=coord.sel[coord.sel$rt.out==0,])
Anova(glmer.rt.choice, type=3)
emmeans(glmer.rt.choice, pairwise~response, adjust="Bonferroni") #higher RTs in competition compared to lottery and coordination
emmeans(glmer.rt.choice, pairwise~Display, adjust="Bonferroni")
emmeans(glmer.rt.choice, pairwise~response | Display, adjust="Bonferroni")
#differences in RTs in lottery only (in competition marginally significant .07): higher RTs for risky vs. unrisky choice


#grafici
coord.sel$Display <- factor(coord.sel$Display,levels = c('coordination','lottery', 'competition'))

#with resp choices
ggplot(coord.sel[coord.sel$rt.out==0,], aes(as.factor(response), Reaction.Time))+
  stat_summary(fun.data = "mean_cl_boot")+
  facet_wrap(~Display)
ggplot(coord.sel[coord.sel$rt.out==0,], aes(as.factor(response), Reaction.Time))+
  stat_summary(fun.data = "mean_cl_boot")+
  facet_wrap(~Display)+
  theme_classic()+
  geom_bar(stat="identity")+
  labs(x="Response choice" ,y = "Decision times")

ggplot(coord.sel[coord.sel$rt.out==0,], aes(Display, Reaction.Time))+
  #stat_summary(fun.y=mean, geom = "bar")+
  stat_summary(fun.data="mean_cl_boot",geom='errorbar', width=0.1)+
  #facet_wrap(~Display)+
  theme_classic()+
  labs(x="scenario" ,y = "Decision Time")

#with UP resp only
ggplot(coord.sel[coord.sel$rt.out==0 & coord.sel$response==1,], aes(Display, Reaction.Time))+
  stat_summary(fun.data = "mean_cl_boot")
ggplot(coord.sel[coord.sel$rt.out==0 & coord.sel$response==1,], aes(Display, Reaction.Time))+
  stat_summary(fun.data = "mean_cl_boot")+
  theme_classic()+
  geom_bar(stat="identity")+
  labs(x="scenario" ,y = "Decision times")

##Effect size ----
summary(coord.sel)
coord.sel$Display <- factor(coord.sel$Display,levels = c('coordination','lottery', 'competition'))

#competition - coordination
comp.coord <- droplevels(coord.sel[coord.sel$Display!="lottery", ])
summary(comp.coord)
comp.coord$Display <- factor(comp.coord$Display,levels = c('competition', 'coordination'))
comp.coord %>% 
  cohens_d(Reaction.Time ~ Display, paired = TRUE, ci = T)
t.test(coord.comp$Reaction.Time ~ coord.comp$Display, paired=T)

#competition - lottery
comp.lott <- droplevels(coord.sel[coord.sel$Display!="coordination", ])
comp.lott$Display <- factor(comp.lott$Display,levels = c('competition', 'lottery'))
summary(comp.lott)
comp.lott %>% 
  cohens_d(Reaction.Time ~ Display, paired = TRUE, ci = T)
t.test(comp.lott$Reaction.Time~ coord.lott$Display, paired=T)




# switch ----
180/20
length(sub)
for (i in c(1:9)) {
  
}
ggplot(droplevels(coord.sel[coord.sel$sub %in% sub[c(1:20)],]), 
       aes(trial.number, response, group = Display, col =Display))+
  geom_jitter(height=0.1, width =0)+
  geom_smooth(method = glm, method.args = list(family ="binomial"), se =F, alpha =0.5)+
  facet_wrap(~sub)


sort(unique(coord.sel$trial.number))
for (i in c(1:nrow(coord.sel))){
  if(coord.sel$trial.number[i] !=1){
    # ifelse(coord.sel$response[i] == coord.sel$response[coord.sel$sub==coord.sel$sub[i] & coord.sel$Display==coord.sel$Display[i] & coord.sel$trial.number == (coord.sel$trial.number[i]-1)],
    #        coord.sel$switch[i] = 0,coord.sel$switch[i] = 1),
    coord.sel$switch[i] = ifelse(coord.sel$response[i] == coord.sel$response[coord.sel$sub==coord.sel$sub[i] & coord.sel$Display==coord.sel$Display[i] & coord.sel$trial.number == (coord.sel$trial.number[i]-1)],0,1)
  }
}
coord.sel$switch[coord.sel$trial.number==1]=NA
summary(coord.sel)

coord.sel.ord = coord.sel[order(coord.sel$sub,coord.sel$Display, coord.sel$trial.number),]
View(coord.sel.ord)



coord.cond = coord.sel %>%
  group_by(sub, Display) %>%
  summarise(sum = sum(response),
            mean = mean(response),
            mean.rt = mean(Reaction.Time), 
            prop = sum(response)/15,
            median.rt = median(Reaction.Time), 
            count.trial = length(response),
            flag.trial = if(length(response)==15){0}else{1},
            switch = mean(switch[trial.number !=1])) 


ggplot(coord.cond, 
       aes(x=factor(Display, level= c('coordination','lottery', 'competition')), y=switch, fill=Display)) +
  #scale_fill_brewer(palette="Blues") +
  theme_classic() +
  stat_boxplot()+
  stat_summary(fun.data=mean_se,geom='errorbar', width=.3)
#+geom_point(position=pj, alpha=0.5, size=0.5)
  #geom_line(aes(group=sub), alpha=0.2)


ggplot(coord.sel, aes(trial.number, switch, col = Display, fill =Display, group=Display))+
  geom_smooth()+stat_summary()



##differenza tra le condizioni nello switch ----
summary(coord.cond)
wilcox.test(switch ~ Display, paired=T, coord.cond[coord.cond$Display!= "coordination",])
wilcox.test(switch ~ Display, paired=T, coord.cond[coord.cond$Display!= "lottery",])
wilcox.test(switch ~ Display, paired=T, coord.cond[coord.cond$Display!= "competition",])


anova.switch <- aov(switch ~ Display + Error(sub/Display), data=coord.cond)
summary(anova.switch)
pairwise.t.test(coord.cond$switch, coord.cond$Display, p.adjust.method = "bonferroni")
#higher switch in competition 

##Effect size ----

#competition - coordination
comp.coord <- droplevels(coord.sel[coord.sel$Display!="lottery", ])
summary(comp.coord)
comp.coord$Display <- factor(comp.coord$Display,levels = c('competition', 'coordination'))
comp.coord %>% 
  cohens_d(switch ~ Display, paired = TRUE, ci = T)
t.test(coord.comp$switch ~ coord.comp$Display, paired=T)

#competition - lottery
comp.lott <- droplevels(coord.sel[coord.sel$Display!="coordination", ])
comp.lott$Display <- factor(comp.lott$Display,levels = c('competition', 'lottery'))
summary(comp.lott)
comp.lott %>% 
  cohens_d(switch ~ Display, paired = TRUE, ci=T)

t.test(comp.lott$switch~ coord.lott$Display, paired=T) #qua non significativo

comp.lott.cond <- droplevels(coord.cond[coord.cond$Display!="coordination", ])
comp.lott.cond$Display <- factor(comp.lott.cond$Display,levels = c('competition', 'lottery'))
t.test(comp.lott.cond$switch~ comp.lott.cond$Display, paired=T) #qua sì

comp.lott.cond %>% 
  cohens_d(switch ~ Display, paired = TRUE)








# pay-off ----
library(readr)
coord.sel = read.csv("/Users/serenamariastagnitto/Dropbox/Serene&Gabriele/sPT in children_adolescents/DATA/T1_preprocessed/COORDINATION.game_preprocessed.csv", stringsAsFactors = T)[,-1]
View(coord.sel)
summary(coord.sel)

for (i in c(1:nrow(coord.sel))){
  meanUp.coord.others = mean(coord.sel$response[coord.sel$Display=="coordination" & coord.sel$trial.number == coord.sel$trial.number[i] & coord.sel$sub != coord.sel$sub[i]])
  meanUp.comp.others = mean(coord.sel$response[coord.sel$Display=="competition" & coord.sel$trial.number == coord.sel$trial.number[i] & coord.sel$sub != coord.sel$sub[i]])
  if (coord.sel$response[i] == 0){coord.sel$payoff[i]=coord.sel$trial.number[i]} else {
    if (coord.sel$Display[i]=="lottery"){coord.sel$payoff[i]=7.5}
    if (coord.sel$Display[i]=="coordination"){coord.sel$payoff[i]=15*meanUp.coord.others}
    if (coord.sel$Display[i]=="competition"){coord.sel$payoff[i]=15* (1-meanUp.comp.others)}
  }
}

agg = coord.sel %>% 
  group_by(trial.number, Display) %>%
  summarise(mean.up = mean(response))
summary(agg)
summary(agg[agg$Display=="coordination",])

coord.sel[coord.sel$Display=="coordination" & coord.sel$trial.number==1 & coord.sel$response==1,]

agg = coord.sel %>% 
  group_by(sub, Display) %>%
  summarise(mean.pay = mean(payoff))
View(agg)
ggplot(agg, aes(x=factor(Display, c("coordination", "lottery", "competition")), mean.pay))+geom_boxplot()+
  theme_minimal()

wilcox.test(mean.pay~Display, paired =T, agg[agg$Display!="lottery",])
wilcox.test(mean.pay~Display, paired =T, agg[agg$Display!="coordination",])
wilcox.test(mean.pay~Display, paired =T, agg[agg$Display!="competition",])
#higher payoff in coordination 

##Effect size ----
#coordination - competition
coord.comp <- droplevels(coord.sel[coord.sel$Display!="lottery", ])
coord.comp %>% 
  cohens_d(payoff ~ Display, paired = TRUE, ci=T)
t.test(coord.comp$response~ coord.comp$Display, paired=T)

library(psych)

#coordination - lottery
coord.lott <- droplevels(coord.sel[coord.sel$Display!="competition",])
summary(coord.lott)
coord.lott %>% 
  cohens_d(payoff ~ Display, paired = TRUE, ci=T)
t.test(coord.lott$response~ coord.lott$Display, paired=T)








#grafici responses and switch barplot ----
summary(coord.cond)
resp <- ggplot(coord.cond, 
       aes(x=factor(Display, level= c('coordination','lottery', 'competition')), y=mean, fill=Display)) +
  theme_classic() +
  stat_summary(fun.y=mean, geom = "bar")+
  stat_summary(fun.data="mean_cl_boot",geom='errorbar', width=0.1)+
  labs(x="scenario" ,y = "Prop. of UP choice")

switch <-ggplot(coord.cond, 
       aes(x=factor(Display, level= c('coordination','lottery', 'competition')), y=switch, fill=Display)) +
  #scale_fill_brewer(palette="Blues") +
  theme_classic() +
  stat_summary(fun.y=mean, geom = "bar")+
  stat_summary(fun.data="mean_cl_boot",geom='errorbar', width=0.1)+
  labs(x="scenario" ,y = "switch")

library(ggpubr)
ggarrange(resp, switch, ncol=2, nrow=1)


# delta scores ----
summary(coord.sel)
coord.delta = coord.sel  %>%
  group_by(sub) %>%
  summarise(resp.coord = mean(response[Display=="coordination"]),
            resp.comp = mean(response[Display=="competition"]),
            switch.coord = mean(switch[trial.number !=1 & Display=="coordination" ]),
            switch.comp = mean(switch[trial.number !=1 & Display=="competition"]),
            delta.resp.coord = mean(response[Display=="coordination"])-mean(response[Display=="lottery"]),
            delta.resp.comp = mean(response[Display=="competition"])-mean(response[Display=="lottery"]),
            delta.switch.coord = mean(switch[trial.number !=1 & Display=="coordination"])-mean(switch[trial.number !=1 & Display=="lottery"]),
            delta.switch.comp = mean(switch[trial.number !=1 & Display=="competition"])-mean(switch[trial.number !=1 & Display=="lottery"])
            )
View(coord.delta)
summary(coord.delta)

write.csv(coord.delta, file.path(path.out,"coord.mean.scores.wide.csv"))

#EFFECT SIZES -----
##responses 
coord.sel$tn.s = scale(coord.sel$trial.number, scale=F)
coord.sel$sub = as.factor(coord.sel$sub)
summary(coord.sel)
glmer.resp <- glmer(response ~ tn.s * Display + (tn.s+Display | sub), family="binomial", 
                    control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)),
                    data = coord.sel)
summary(glmer.resp)
Anova(glmer.resp, type = 'III')
emm = emmeans(glmer.resp, pairwise~Display, adjust="Bonf")
summary(emm, type = 'response')
confint(emm, level=0.95) # Confidence intervals of those differences
emmeans::eff_size(emm, sigma = sigma(glmer.resp), edf = df.residual(glmer.resp)) # Effect sizes of those differences.

test(emtrends(glmer.resp, ~ tn.s | Display, var = 'tn.s'))



glmer.resp.i <- glmer(response ~ Display / tn.s + (tn.s+Display | sub), family="binomial", 
                    control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)),
                    data = coord.sel)
summary(glmer.resp.i)



#plot model (daniele)
install.packages('effects')
require(effects)

plot(allEffects(glmer.resp))
plot(predictorEffects(glmer.resp,~ tn.s, residuals=F),
     main = "",axes = list(grid = T,x = list(rotate = 0,rug = F, tn.s = list(lab = 'sure payoff')),
                           y = list(rotate = 0,rug = F,type = "response", lim = c(0,1), lab = 'proportion old resp')),
     partial.residuals = list(smooth = F,col = "black",cex = .3,pch = 16),
     lines=list(multiline=T,col=c('royalblue','red','green')), confint=list(style='auto'),
     lattice=list(layout=c(1, 1)))


##payoff
summary(coord.sel)
coord.sel$tn.s = scale(coord.sel$trial.number, scale=F)
coord.sel$payoff = scale(coord.sel$payoff, scale=F)
coord.sel$sub = as.factor(coord.sel$sub)
summary(coord.sel)
lmer.payoff <- lmer(payoff ~ Display + (1 | sub),
                    contrasts=list(Display="contr.sum"),
                    data = coord.sel)
Anova(lmer.payoff, type=3)
summary(lmer.payoff)
emm.p = emmeans(lmer.payoff, pairwise~Display, adjust="Bonf") 
summary(emm.p, type = 'response')

confint(emm.p, level=0.95) # Confidence intervals of those differences
emmeans::eff_size(emm.p, sigma = sigma(lmer.payoff), edf = df.residual(lmer.payoff)) # Effect sizes of those differences.



##RTs
coord.sel$Display <- factor(coord.sel$Display,levels = c('competition', 'coordination', 'lottery'))
str(coord.sel)
glmer.rt.choice <- glmer(Reaction.Time ~ Display*response + (1| sub), family=Gamma(link="log"),
                         contrasts=list(Display="contr.sum"), 
                         data=coord.sel[coord.sel$rt.out==0,])
Anova(glmer.rt.choice, type=3)
emmeans(glmer.rt.choice, pairwise~Display, adjust="Bonferroni") #higher RTs in competition compared to lottery and coordination
emm.rts = emmeans(glmer.rt.choice, pairwise~Display, adjust="Bonf") 
confint(emm.rts, level=0.95) # Confidence intervals of those differences
emmeans::eff_size(emm.rts, sigma = sigma(glmer.rt.choice), edf = df.residual(glmer.rt.choice)) # Effect sizes of those differences.



##switch
summary(coord.sel)
table(coord.sel$switch)
glmer.switch <- glmer(switch ~  Display + (Display | sub), family="binomial", 
                    control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)),
                    data = coord.sel)

Anova(glmer.switch, type=3)
emm.switch = emmeans(glmer.switch, pairwise~Display, adjust="Bonferroni")
confint(emm.switch, level=0.95) # Confidence intervals of those differences
emmeans::eff_size(emm.switch, sigma = sigma(glmer.switch), edf = df.residual(glmer.switch)) # Effect sizes of those differences.















# AGE groups ----
dem <- read.csv("/Users/serenamariastagnitto/Dropbox/Serene&Gabriele/sPT in children_adolescents/DATA/T1_preprocessed/DEM_wide_adjusted.csv",
                  header=TRUE, sep=",", dec=",")
View(dem)
dem <- dem[,-1]

coord.sel.age <- merge(coord.sel, dem, by = "sub", all= T)
summary(coord.sel.age)



## response - age ----
coord.sel.age$tn.s = scale(coord.sel.age$trial.number, scale=F)
coord.sel.age$sub = as.factor(coord.sel.age$sub)
coord.sel.age$class.age = as.factor(coord.sel.age$class.age)
coord.sel.age <- na.omit(coord.sel.age)



glmer.age <- glmer(response ~ tn.s * Display * class.age + (tn.s*Display| sub), family="binomial", 
               control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)),
               data = coord.sel.age)

Anova(glmer.age, type=3)
emmeans(glmer.age, pairwise~class.age, adjust="bonferroni") #NO main effect of age.class on response
emmeans(glmer.age, pairwise~Display | class.age | tn.s, adjust="bonferroni")

test(emtrends(glmer.age, pairwise~class.age | Display, var = "tn.s")) #interaction effect tn.s*Display*class.age
#comparing classes:
#in coordination e lottery: 1 differ from 2=3
#in competition: no differences

ggplot(coord.sel.age, aes(tn.s, response, col = class.age, fill = Display, group=class.age))+
  geom_smooth(se=F, na.rm=T)+
  #stat_summary(fun.y = mean, alpha=0.5)+
  theme_minimal()+
  facet_wrap(~ Display)




lm.age <- lmer(response ~ Display * class.age + (1| sub),
               contrasts=list(Display="contr.sum"),data = coord.sel.age)
Anova(lm.age, type=3)
#no main effect of class age (nor interaction *Display) for response 

ggplot(coord.sel.age, aes(x=factor(Display, level= c('coordination','lottery', 'competition')), y=response, fill=Display)) +
  #scale_fill_brewer(palette="Blues") +
  theme_classic() +
  stat_summary(fun.y=mean, geom = "bar")+
  stat_summary(fun.data="mean_cl_boot",geom='errorbar', width=0.1)+
  labs(x="scenario" ,y = "probability of UP choice")+
  facet_wrap(~class.age)




## switch - age ----
glmer.switch.age <- glmer(switch ~ Display * as.factor(class.age) + (1| sub), family="binomial", 
                   #control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)),
                   contrasts=list("class.age"=contr.sum),
                   data = coord.sel.age)
Anova(glmer.switch.age, type=3)
emmeans(glmer.switch.age, pairwise~class.age, adjust="bonferroni") #NO main effect of age.class on switch
emmeans(glmer.switch.age, pairwise~class.age | Display, adjust="BH") #higher switch in group 1 in lottery only


ggplot(coord.sel.age, aes(x=factor(Display, level= c('coordination','lottery', 'competition')), y=switch, fill=Display)) +
  #scale_fill_brewer(palette="Blues") +
  theme_classic() +
  stat_summary(fun.y=mean, geom = "bar")+
  stat_summary(fun.data="mean_cl_boot",geom='errorbar', width=0.1)+
  labs(x="scenario" ,y = "switch")+
  facet_wrap(~class.age)

#NON-PARAMETRIC ANALYSES: Kruskal-Wallis Test
library(rstatix)
res.kruskal <- coord.sel.age %>% kruskal_test(switch ~ class.age)
res.kruskal

# Pairwise comparisons
res.kruskal.pwc <- coord.sel.age %>% 
  dunn_test(switch ~ class.age, p.adjust.method = "bonferroni") 
res.kruskal.pwc
# switch in group 1 differ from group 2 and 3


## delta - age ----
coord.age <- merge(coord.delta, dem, by = "sub", all= T)
summary(coord.age)
View(coord.age)
coord.age <- na.omit(coord.age)

# response - choice graphs
resp.coord <- ggplot(coord.age, aes(x=factor(class.age, level= c('1','2', '3')), y=resp.coord)) +
                       theme_minimal() +
  stat_summary(fun.y=mean, geom = "bar")+
  stat_summary(fun.data="mean_cl_boot",geom='errorbar', width=0.1)+
                       labs(x="Class-age",y = "Prop. of UP choice", title = "Coordination")

resp.comp <- ggplot(coord.age, aes(x=factor(class.age, level= c('1','2', '3')), y=resp.comp)) +
  theme_minimal() +
  stat_summary(fun.y=mean, geom = "bar")+
  stat_summary(fun.data="mean_cl_boot",geom='errorbar', width=0.1)+
  labs(x="Class-age",y = "Prop. of UP choice", title = "Competition")

delta.resp.coord <- ggplot(coord.age, aes(x=factor(class.age, level= c('1','2', '3')), y=delta.resp.coord)) +
  theme_minimal() +
  stat_summary(fun.y=mean, geom = "bar")+
  stat_summary(fun.data="mean_cl_boot",geom='errorbar', width=0.1)+
  labs(x="Class-age",y = "Delta prop. of UP choice", title = "Coordination - Lottery")

delta.resp.comp <- ggplot(coord.age, aes(x=factor(class.age, level= c('1','2', '3')), y=delta.resp.comp)) +
  theme_minimal() +
  stat_summary(fun.y=mean, geom = "bar")+
  stat_summary(fun.data="mean_cl_boot",geom='errorbar', width=0.1)+
  labs(x="Class-age",y = "Delta prop. of UP choice", title = "Competition - Lottery")

ggarrange(resp.coord, resp.comp, delta.resp.coord, delta.resp.comp,
          ncol=2, nrow=2)



# switch graphs
switch.coord <- ggplot(coord.age, aes(x=factor(class.age, level= c('1','2', '3')), y=switch.coord)) +
  theme_minimal() +
  stat_summary(fun.y=mean, geom = "bar")+
  stat_summary(fun.data="mean_cl_boot",geom='errorbar', width=0.1)+
  labs(x="Class-age",y = "Switch", title = "Coordination")
switch.comp <- ggplot(coord.age, aes(x=factor(class.age, level= c('1','2', '3')), y=switch.comp)) +
  theme_minimal() +
  stat_summary(fun.y=mean, geom = "bar")+
  stat_summary(fun.data="mean_cl_boot",geom='errorbar', width=0.1)+
  labs(x="Class-age",y = "Switch", title = "Competition")
delta.switch.coord <- ggplot(coord.age, aes(x=factor(class.age, level= c('1','2', '3')), y=delta.switch.coord)) +
  theme_minimal() +
  stat_summary(fun.y=mean, geom = "bar")+
  stat_summary(fun.data="mean_cl_boot",geom='errorbar', width=0.1)+
  labs(x="Class-age",y = "Delta switch", title = "Coordination - Lottery")
delta.switch.comp <- ggplot(coord.age, aes(x=factor(class.age, level= c('1','2', '3')), y=delta.switch.comp)) +
  theme_minimal() +
  stat_summary(fun.y=mean, geom = "bar")+
  stat_summary(fun.data="mean_cl_boot",geom='errorbar', width=0.1)+
  labs(x="Class-age",y = "Delta switch", title = "Competition - Lottery")

ggarrange(switch.coord, switch.comp, delta.switch.coord, delta.switch.comp,
          ncol=2, nrow=2)

## preliminary analyses - age differences? ----
summary(coord.age)


anova.resp.coord <- aov(resp.coord ~ class.age, data=coord.age)
summary(anova.resp.coord)
pairwise.t.test(coord.age$resp.coord, coord.age$class.age, p.adjust.method = "none")

anova.resp.comp <- aov(resp.comp ~ class.age, data=coord.age)
summary(anova.resp.comp)
pairwise.t.test(coord.age$resp.comp, coord.age$class.age, p.adjust.method = "none")

anova.delta.resp.coord <- aov(delta.resp.coord ~ class.age, data=coord.age)
summary(anova.delta.resp.coord)
pairwise.t.test(coord.age$delta.resp.coord, coord.age$class.age, p.adjust.method = "none")

anova.delta.resp.comp <- aov(delta.resp.comp ~ class.age, data=coord.age)
summary(anova.delta.resp.comp)
pairwise.t.test(coord.age$delta.resp.comp, coord.age$class.age, p.adjust.method = "none")

#switch
anova.switch.coord <- aov(switch.coord ~ class.age, data=coord.age)
summary(anova.switch.coord)
pairwise.t.test(coord.age$switch.coord, coord.age$class.age, p.adjust.method = "none")

anova.switch.comp <- aov(switch.comp ~ class.age, data=coord.age)
summary(anova.switch.comp)
pairwise.t.test(coord.age$switch.comp, coord.age$class.age, p.adjust.method = "none")

anova.delta.switch.coord <- aov(delta.switch.coord ~ class.age, data=coord.age)
summary(anova.delta.switch.coord)
pairwise.t.test(coord.age$delta.switch.coord, coord.age$class.age, p.adjust.method = "none")

anova.delta.switch.comp <- aov(delta.switch.comp ~ class.age, data=coord.age)
summary(anova.delta.switch.comp)
pairwise.t.test(coord.age$delta.switch.comp, coord.age$class.age, p.adjust.method = "none")


## payoff - age -----

ggplot(coord.sel.age, aes(x=factor(Display, level= c('coordination','lottery', 'competition')), y=payoff, fill=Display)) +
  #scale_fill_brewer(palette="Blues") +
  theme_classic() +
  stat_summary(fun.y=mean, geom = "bar")+
  stat_summary(fun.data="mean_cl_boot",geom='errorbar', width=0.1)+
  labs(x="scenario" ,y = "payoff")+
  facet_wrap(~class.age)

ggplot(coord.sel.age, aes(Display, payoff))+geom_boxplot()+facet_wrap(~class.age)

View(coord.sel.age)

prova <- lmer(payoff ~ Display * class.age + (1| sub),data = coord.sel.age)
Anova(prova, type=3)
summary(prova)
emmeans(prova, pairwise~class.age, adjust="bonferroni") #lower payoff in 1 compared to 2 and 3 
emmeans(prova, pairwise ~class.age | Display, adjust="bonferroni") #interaction: no age differences in competition, lower payoff in 1 compared to 2 and 3 in coordination, lower payoff in 1 compared to 3 in lottery


#NON-PARAMETRIC ANALYSES: Kruskal-Wallis Test
library(rstatix)
kruskal <- coord.sel.age %>% kruskal_test(payoff ~ class.age)
kruskal

# Pairwise comparisons
kruskal.pwc <- coord.sel.age %>% 
  dunn_test(payoff ~ class.age, p.adjust.method = "bonferroni") 
kruskal.pwc
# payoff in group 1 differ from group 2 and 3


## RTs - age ----

library(car)
summary(coord.sel.age)
lmer.rt.age <- lmer(Reaction.Time ~ Display*class.age + (1| sub),
                    data=coord.sel.age[coord.sel.age$response==1,])
Anova(lmer.rt.age)
emmeans(lmer.rt.age, pairwise~ class.age, adjust="none")
emmeans(lmer.rt.age, pairwise~Display | class.age, adjust="bonferroni")



glmer.rt.age <- glmer(log(Reaction.Time) ~ Display*class.age + (1| sub), family=Gamma(link="log"),
                  control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)),
                  #contrasts=list(Display="contr.sum"), 
                  data=coord.sel.age[coord.sel.age$response==1,])
Anova(glmer.rt.age, type=3)
emmeans(glmer.rt.age, pairwise~Display | class.age, adjust="none")
emmeans(glmer.rt.age, pairwise~class.age | Display, adjust="none")
emmeans(glmer.rt.age, pairwise~class.age, adjust="none")
emmeans(glmer.rt.age, pairwise~Display, adjust="none")


#no differences in RTs in risky choices comparing conditions

ggplot(coord.sel.age[coord.sel.age$rt.out==0,], aes(as.factor(response), Reaction.Time, fill=Display, col=Display))+
  stat_summary(fun.data = "mean_cl_boot")+
  facet_wrap(~class.age)

ggplot(coord.sel.age[coord.sel.age$rt.out==0 & coord.sel.age$response==1,], aes(Display, Reaction.Time, fill=Display, col=Display))+
  stat_summary(fun.data = "mean_cl_boot")+
  facet_wrap(~class.age) #only with riscky choice 

ggplot(coord.sel.age[coord.sel.age$rt.out==0,], aes(as.factor(response), Reaction.Time, fill=Display, col=Display))+
  stat_summary(fun.data = "mean_cl_boot")+
  facet_wrap(~class.age)

ggplot(coord.sel.age[coord.sel.age$rt.out==0,], aes(as.factor(response), Reaction.Time, fill=Display, col=Display))+
  stat_summary(fun.data = "mean_cl_boot")+
  facet_wrap(~class.age*Display)




# NOMINE ----
library(readxl)
nomine <- read_excel("Dropbox/Serene&Gabriele/sPT in children_adolescents/DATA/LIBRETTI - DATA/T1_nomine.xlsx")

summary(nomine)

#distributions
ggplot(nomine, aes(ML_mean)) + geom_histogram(colour="black", fill="white")
ggplot(nomine, aes(LL_smean)) + geom_histogram(colour="black", fill="white")

ggplot(nomine, aes(prosociality_mean)) + geom_histogram(colour="black", fill="white")
ggplot(nomine, aes(popularity_mean)) + geom_histogram(colour="black", fill="white")
ggplot(nomine, aes(exclusion_mean)) + geom_histogram(colour="black", fill="white")



coord.simple = coord.sel[coord.sel$Display != "lottery" & coord.sel$rt.out==0,] %>%
  group_by(sub, Display) %>%
  summarise(prop.response = sum(response)/15,
            prop.switch = mean(switch[trial.number !=1])) 

coord.coordination = coord.sel[coord.sel$Display == "cooordination" & coord.sel$rt.out==0,] %>%
  group_by(sub) %>%
  summarise(prop.response = sum(response)/15,
            prop.switch = mean(switch[trial.number !=1])) 
View(coord.competition)
coord.competition$Display <- "coordination"

coord.competition = coord.sel[coord.sel$Display == "competition" & coord.sel$rt.out==0,] %>%
  group_by(sub) %>%
  summarise(prop.response = sum(response)/15,
            prop.switch = mean(switch[trial.number !=1])) 
View(coord.competition)
coord.competition$Display <- "competition"

coord.long = rbind(coord.coordination, coord.competition)
View(coord.long)
coord.long2<- coord.long %>% pivot_longer(!c(sub, Display), names_to= "prop", values_to= "value")
View(coord.long2)

coord.long.age <- merge(coord.long2, dem, by = "sub", all= T)
coord.nomine <- merge(coord.long.age, nomine, by = "sub.name", all= T)
#deleting rown with NAs
coord.nomine <- na.omit(coord.nomine)
View(coord.nomine)
write.csv(coord.nomine, file.path(path.out,"T1_coord.nomine.csv"))



#dataset wide 
coord.cond.wide <- 
  pivot_wider(coord.simple, id_cols = "sub", names_from = "Display", values_from = c("prop.response", "prop.switch"))
coord.cond.wide.age <- merge(coord.cond.wide, dem, by = "sub")
coord.nomine.wide <- merge(coord.cond.wide.age, nomine, by = "sub.name", all= T)
coord.nomine.wide <- na.omit(coord.nomine.wide)#deleting rown with NAs 
#175 ppts
write.csv(coord.nomine.wide, file.path(path.out,"T1_coord.nomine.wide.csv"))

summary(nomine)



# ML nomine = quanto agli altri compagni piace stare con te 
ggplot(coord.nomine, aes(ML_stand, value, group= Display, col = Display, fill =Display))+
  geom_smooth(method = "lm")+
  geom_smooth(method = "loess", se=F)+
  gepm_point(alpha=0.5)
  theme_minimal()+
  stat_cor(method = "spearman")+
  facet_wrap(~prop, scales="free_y")


# LL nomine = quanto agli altri compagni non piace stare con te 
ggplot(coord.nomine, aes(LL_stand, value, group= Display, col = Display, fill =Display))+
  geom_smooth(method = "lm")+
  #stat_summary(fun.y = mean, alpha=0.5)+
  theme_minimal()+
  stat_cor(method = "spearman")+
  facet_wrap(~prop)

# prosociality = quanto secondo gli altri compagni tu sei prosociale 
ggplot(coord.nomine, aes(prosociality_stand, value, group= Display, col = Display, fill =Display))+
  geom_smooth(method = "lm")+
  #stat_summary(fun.y = mean, alpha=0.5)+
  theme_minimal()+
  stat_cor(method = "spearman")+
  facet_wrap(~prop)

# popularity = quanto secondo gli altri compagni tu sei popolare 
ggplot(coord.nomine, aes(popularity_stand, value, group= Display, col = Display, fill =Display))+
  geom_smooth(method = "lm")+
  #stat_summary(fun.y = mean, alpha=0.5)+
  theme_minimal()+
  stat_cor(method = "spearman")+
  facet_wrap(~prop)

# exclusion = quanto secondo gli altri compagni tu sei escluso 
ggplot(coord.nomine, aes(exclusion_stand, value, group= Display, col = Display, fill =Display))+
  geom_smooth(method = "lm")+
  #stat_summary(fun.y = mean, alpha=0.5)+
  theme_minimal()+
  stat_cor(method = "spearman")+
  facet_wrap(~prop)


## delta dataset - nomine ----
View(coord.age)
coord.delta = coord.sel  %>%
  group_by(sub) %>%
  summarise(resp.coord = mean(response[Display=="coordination"]),
            resp.comp = mean(response[Display=="competition"]),
            switch.coord = mean(switch[trial.number !=1 & Display=="coordination" ]),
            switch.comp = mean(switch[trial.number !=1 & Display=="competition"]),
            delta.resp.coord = mean(response[Display=="coordination"])-mean(response[Display=="lottery"]),
            delta.resp.comp = mean(response[Display=="competition"])-mean(response[Display=="lottery"]),
            delta.switch.coord = mean(switch[trial.number !=1 & Display=="coordination"])-mean(switch[trial.number !=1 & Display=="lottery"]),
            delta.switch.comp = mean(switch[trial.number !=1 & Display=="competition"])-mean(switch[trial.number !=1 & Display=="lottery"])
  )
View(coord.delta)
summary(coord.delta)



coord.simple = coord.sel[coord.sel$Display != "lottery" & coord.sel$rt.out==0,] %>%
  group_by(sub, Display) %>%
  summarise(prop.response = sum(response)/15,
            prop.switch = mean(switch[trial.number !=1])) 

coord.coordination = coord.sel[coord.sel$rt.out==0,] %>%
  group_by(sub) %>%
  summarise(prop.response = mean(response[Display=="coordination"]),
            prop.switch = mean(switch[trial.number !=1 & Display=="coordination" ]),
            delta.resp = mean(response[Display=="coordination"])-mean(response[Display=="lottery"]),
            delta.switch = mean(switch[trial.number !=1 & Display=="coordination"])-mean(switch[trial.number !=1 & Display=="lottery"])) 
coord.coordination$Display <- "coordination"

coord.competition = coord.sel[coord.sel$rt.out==0,] %>%
  group_by(sub) %>%
  summarise(prop.response = mean(response[Display=="competition"]),
            prop.switch = mean(switch[trial.number !=1 & Display=="competition" ]),
            delta.resp = mean(response[Display=="competition"])-mean(response[Display=="lottery"]),
            delta.switch = mean(switch[trial.number !=1 & Display=="competition"])-mean(switch[trial.number !=1 & Display=="lottery"])) 
coord.competition$Display <- "competition"

coord.long = rbind(coord.coordination, coord.competition)
View(coord.long)
coord.long2<- coord.long %>% pivot_longer(!c(sub, Display), names_to= "prop", values_to= "value")
View(coord.long2)

coord.long.age <- merge(coord.long2, dem, by = "sub", all= T)
coord.nomine <- merge(coord.long.age, nomine, by = "sub.name", all= T)
#deleting rown with NAs
coord.nomine <- na.omit(coord.nomine)
View(coord.nomine)
write.csv(coord.nomine, file.path(path.out,"T1_coord.nomine.csv"))


##grafici ----






# ML nomine = quanto agli altri compagni piace stare con te 
ML <- ggplot(coord.nomine, aes(ML_mean, value, group= Display, col = Display, fill =Display))+
  geom_smooth(method = "lm")+
  geom_smooth(method = "loess", se=F)+
  geom_point(alpha=0.5)+
  theme_minimal()+
  stat_cor(label.y.npc="top", label.x.npc = "left", method = "spearman")+
  stat_cor(label.y.npc="top", label.x.npc = "center", method = "pearson")+
  facet_wrap(~prop, scales="free_y")

# LL nomine = quanto agli altri compagni non piace stare con te 
LL <- ggplot(coord.nomine, aes(LL_smean, value, group= Display, col = Display, fill =Display))+
  geom_smooth(method = "lm")+
  geom_smooth(method = "loess", se=F)+
  geom_point(alpha=0.5)+
  theme_minimal()+
  stat_cor(label.y.npc="top", label.x.npc = "left", method = "spearman")+
  stat_cor(label.y.npc="top", label.x.npc = "center", method = "pearson")+
  facet_wrap(~prop, scales="free_y")


# prosociality = quanto secondo gli altri compagni tu sei prosociale 
pros <- ggplot(coord.nomine, aes(prosociality_mean, value, group= Display, col = Display, fill =Display))+
  geom_smooth(method = "lm")+
  geom_smooth(method = "loess", se=F)+
  geom_point(alpha=0.5)+
  theme_minimal()+
  stat_cor(label.y.npc="top", label.x.npc = "left", method = "spearman")+
  stat_cor(label.y.npc="top", label.x.npc = "center", method = "pearson")+
  facet_wrap(~prop, scales="free_y")

# popularity = quanto secondo gli altri compagni tu sei popolare 
popul <- ggplot(coord.nomine, aes(popularity_mean, value, group= Display, col = Display, fill =Display))+
  geom_smooth(method = "lm")+
  geom_smooth(method = "loess", se=F)+
  geom_point(alpha=0.5)+
  theme_minimal()+
  stat_cor(label.y.npc="top", label.x.npc = "left", method = "spearman")+
  stat_cor(label.y.npc="top", label.x.npc = "center", method = "pearson")+
  facet_wrap(~prop, scales="free_y")

# exclusion = quanto secondo gli altri compagni tu sei escluso 
exclus <- ggplot(coord.nomine, aes(exclusion_mean, value, group= Display, col = Display, fill =Display))+
  geom_smooth(method = "lm")+
  #stat_summary(fun.y = mean, alpha=0.5)+
  theme_minimal()+
  stat_cor(method = "spearman")+
  facet_wrap(~prop)

ggarrange(ML, LL, nrow=1)
ggarrange(pros, popul, exclus, nrow=1)


# DYSWIS ----

coord.long.age <- merge(coord.long2, dem, by = "sub", all= T)
spt <- read_csv("Dropbox/Serene&Gabriele/sPT in children_adolescents/DATA/T1_preprocessed/DYSWIS.aggregates.wide.csv", 
                                   col_types = cols(...1 = col_skip(), mean.static = col_number(), 
                                                    mean.movement = col_number(), mean.rt.static = col_number(), 
                                                    mean.rt.movement = col_number(), 
                                                    median.rt.static = col_number(), 
                                                    median.rt.movement = col_number()))

spt <- na.omit(spt)#deleting rown with NAs 
spt.sel <- spt[,1:3]
spt.long<- spt.sel %>% pivot_longer(!c(sub), names_to= "spt.component", values_to= "spt.mean")


coord.spt <- merge(coord.long.age, spt.long, by = "sub", all= T)
#deleting rown with NAs
coord.spt <- na.omit(coord.spt)
View(coord.spt)
summary(coord.spt)
View(coord.spt)
write.csv(coord.spt, file.path(path.out,"T1_coord.spt.aggregates.csv"))

#grafico
ggplot(coord.spt, aes(spt.mean, value, group= Display, col = Display, fill =Display))+
  geom_smooth(method = "lm")+
  #stat_summary(fun.y = mean, alpha=0.5)+
  theme_minimal()+
  stat_cor(method = "spearman")+
  facet_wrap(~prop*spt.component, nrow=2)



# DIRECTOR ----
director <- read_excel("Dropbox/Serene&Gabriele/sPT in children_adolescents/DATA/T1_preprocessed/director.conditions.xlsx", 
                                  col_types = c("skip", "numeric", "text", 
                                                "numeric", "numeric", "numeric", 
                                                "numeric"))
#importare dataset giusto!
director <- read_csv("Dropbox/Serene&Gabriele/sPT in children_adolescents/DATA/T1_preprocessed/director.conditions.csv", 
                                col_types = cols(...1 = col_skip(), sum = col_number(), 
                                                 mean = col_number(), mean.rt = col_number(), 
                                                 median.rt = col_number()))
View(director)
summary(director.exp)
View(director)

director.exp <- director[director$condition=="Experimental",]
director.exp <- director.exp[,c("sub", "mean")]
colnames(director.exp)[2]='director.mean'
View(director.exp)

coord.director <- merge(coord.long.age, director.exp, by = "sub", all= T)
#deleting rown with NAs
coord.director <- na.omit(coord.director)
View(coord.director)
write.csv(coord.director, file.path(path.out,"T1_coord.director.exp.csv"))



#merging dem2 (session III in which there is the director task) and coordination 
dem2 <- read_csv("Dropbox/Serene&Gabriele/sPT in children_adolescents/DATA/T1_preprocessed/DEM2_wide_adjusted.csv", 
                               col_types = cols(...1 = col_skip()))
summary(dem2)

View(dem2)

director.dem <- merge(director.exp, dem2, by = "sub", all= T)
director.dem$sub = as.factor(director.dem$sub)
director.dem$class.age = as.factor(director.dem$class.age)

coord.director <- merge(coord.long.age, director.dem, by = "sub.name", all= T)
coord.director <- na.omit(coord.director)
summary(coord.director)
View(coord.director)
write.csv(coord.director, file.path(path.out,"T1_coord.director.csv"))



#grafico
ggplot(coord.director, aes(director.mean, value, group= Display, col = Display, fill =Display))+
  geom_smooth(method = "lm")+
  geom_smooth(method = "loess", se=F)+
  #stat_summary(fun.y = mean, alpha=0.5)+
  geom_point(alpha=0.5)+
  theme_minimal()+
  stat_cor(method = "spearman")+
  facet_wrap(~prop, scales="free_y")






