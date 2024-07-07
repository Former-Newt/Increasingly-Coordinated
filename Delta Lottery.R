# Delta Analyses ----

load_pkg <- rlang::quos(here,
                        dplyr,
                        tidyr,
                        tidyverse,
                        ggplot2,
                        ggthemes,
                        wesanderson,
                        lme4,
                        janitor,
                        ggpubr,
                        emmeans,
                        ggrain,
                        MetBrewer,
                        car,
                        ggpattern,
                        modelsummary)

invisible(lapply(lapply(load_pkg, rlang::quo_name),
                 library,
                 character.only = TRUE
))




# Delta Coord-Lott

delta.coop_lott = coord.aggr %>% select(-flag.trial,-flag.variance,-prop, -sum, -count.trial)%>%
  pivot_wider(names_from=game, values_from = c(mean.up, mean.rt, median.rt, mean.switch, mean.payoff)) %>%
  group_by(sub,age.cat,version) %>%
  summarise(Risk = mean.up_coordination - mean.up_lottery,
            RT = mean.rt_coordination - mean.rt_lottery,
            RT2 = median.rt_coordination - median.rt_lottery,
            Switch = mean.switch_coordination - mean.switch_lottery,
            Payoff = mean.payoff_coordination - mean.payoff_lottery) %>%
  pivot_longer(!c(sub, age.cat,version)) %>%
  mutate_at(c("sub","age.cat","version","name"),factor)

delta.coop_lott$name <- fct_relevel(delta.coop_lott$name, "Risk", "Payoff", "Switch","RT","RT2")

delta.coop_lott$age.cat <- fct_relevel(delta.coop_lott$age.cat, "Children", "Adolescents", "Adults")
levels(delta.coop_lott$name)

delta.coop_lott_rt = coord.aggr_rt %>% select(-flag.trial,-flag.variance,-prop, -sum, -count.trial)%>%
  pivot_wider(names_from=game, values_from = c(mean.up, mean.rt, median.rt, mean.switch, mean.payoff)) %>%
  group_by(sub,age.cat,version)%>%
  summarise(Risk = mean.up_coordination - mean.up_lottery,
            RT = mean.rt_coordination - mean.rt_lottery,
            RT2 = median.rt_coordination - median.rt_lottery,
            Switch = mean.switch_coordination - mean.switch_lottery,
            Payoff = mean.payoff_coordination - mean.payoff_lottery) %>%
  pivot_longer(!c(sub, age.cat,version)) %>%
  mutate_at(c("sub","age.cat","version","name"),factor)

delta.coop_lott_rt <- left_join(delta.coop_lott_rt,flags)

delta.coop_lott_rt$name <- fct_relevel(delta.coop_lott_rt$name, "Risk", "Payoff", "Switch","RT","RT2")
delta.coop_lott_rt$age.cat <- fct_relevel(delta.coop_lott_rt$age.cat, "Children", "Adolescents", "Adults")

levels(delta.coop_lott_rt$name)

# Delta Comp-Lott

delta.comp_lott = coord.aggr %>% select(-flag.trial,-flag.variance,-prop, -sum, -count.trial)%>%
  pivot_wider(names_from=game, values_from = c(mean.up, mean.rt, median.rt, mean.switch, mean.payoff)) %>%
  group_by(sub,age.cat,version) %>%
  summarise(Risk = mean.up_competition - mean.up_lottery,
            RT = mean.rt_competition - mean.rt_lottery,
            RT2 = median.rt_competition - median.rt_lottery,
            Switch = mean.switch_competition - mean.switch_lottery,
            Payoff = mean.payoff_competition - mean.payoff_lottery) %>%
  pivot_longer(!c(sub, age.cat,version)) %>%
  mutate_at(c("sub","age.cat","version","name"),factor)

delta.comp_lott$name <- fct_relevel(delta.comp_lott$name, "Risk", "Payoff", "Switch","RT","RT2")

delta.comp_lott$age.cat <- fct_relevel(delta.comp_lott$age.cat, "Children", "Adolescents", "Adults")
levels(delta.comp_lott$name)

delta.comp_lott_rt = coord.aggr_rt %>% select(-flag.trial,-flag.variance,-prop, -sum, -count.trial)%>%
  pivot_wider(names_from=game, values_from = c(mean.up, mean.rt, median.rt, mean.switch, mean.payoff)) %>%
  group_by(sub,age.cat,version)%>%
  summarise(Risk = mean.up_competition - mean.up_lottery,
            RT = mean.rt_competition - mean.rt_lottery,
            RT2 = median.rt_competition - median.rt_lottery,
            Switch = mean.switch_competition - mean.switch_lottery,
            Payoff = mean.payoff_competition - mean.payoff_lottery) %>%
  pivot_longer(!c(sub, age.cat,version)) %>%
  mutate_at(c("sub","age.cat","version","name"),factor)

delta.comp_lott_rt <- left_join(delta.comp_lott_rt,flags)

delta.comp_lott_rt$name <- fct_relevel(delta.comp_lott_rt$name, "Risk", "Payoff", "Switch","RT","RT2")
delta.comp_lott_rt$age.cat <- fct_relevel(delta.comp_lott_rt$age.cat, "Children", "Adolescents", "Adults")

levels(delta.comp_lott_rt$name)
# Merging with control variables ----

delta.coop_lott <- left_join(delta.coop_lott,controls)

delta.coop_lott_rt <- left_join(delta.coop_lott_rt,controls)

delta.comp_lott <- left_join(delta.comp_lott,controls)

delta.comp_lott_rt <- left_join(delta.comp_lott_rt,controls)

# write.csv(delta.social, file.path(here("Processed data/coordination/delta.social.csv")))
# 
# write.csv(delta.social_rt, file.path(here("Processed data/coordination/delta.social_rt.csv")))

# Coord-Lott Plot

ggplot(delta.coop_lott_rt %>% filter(name!="RT2" &
                                    name!="Payoff",
                                  flag.lottery==0) %>%
         droplevels(), aes(x = "", y = value, fill = age.cat, color=age.cat,
                           shape=age.cat)) +
  geom_rain(rain.side = "l",
            violin.args = list(alpha=0.4,linewidth=1),
            boxplot.args =list(alpha= 0.5, colour="black", outlier.shape = NA),
            boxplot.args.pos = list(
              position = ggpp::position_dodgenudge(width = 0.2), width = 0.16),
            point.args.pos= list(position = position_jitter(width = 0.08)),
            point.args = list(alpha=0,aes(group=age.cat)))+
  geom_point(alpha=0.15,position = position_jitterdodge(jitter.width = 0.1,dodge.width = 0.2))+
  # adjust layout 
  xlab("")+
  ylab("Cooperation - Lottery")+
  facet_wrap(~name, scale = 'free', nrow = 2, axes = "all_y") +
  # custom colours and theme+
  theme_minimal()+
  theme(axis.text=element_text(size=18,),
        axis.title=element_text(size=22,face="bold"), plot.title = element_text(size = 18, face = "bold"),
        legend.text=element_text(size=18),
        legend.title=element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        strip.text.x = element_text(size = 20, face = "bold",
                                    margin = margin(t = 0, r = 0, b = 40, l = 40)),
        axis.title.y = element_text(margin = margin(t = 0, r = 40, b = 0, l = 0)))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")

# Comp-Lott Plot

ggplot(delta.comp_lott_rt %>% filter(name!="RT2" &
                                        name!="Payoff",
                                      flag.lottery==0) %>%
         droplevels(), aes(x = "", y = value, fill = age.cat, color=age.cat,
                           shape=age.cat)) +
  geom_rain(rain.side = "l",
            violin.args = list(alpha=0.4,linewidth=1),
            boxplot.args =list(alpha= 0.5, colour="black", outlier.shape = NA),
            boxplot.args.pos = list(
              position = ggpp::position_dodgenudge(width = 0.2), width = 0.16),
            point.args.pos= list(position = position_jitter(width = 0.08)),
            point.args = list(alpha=0,aes(group=age.cat)))+
  geom_point(alpha=0.15,position = position_jitterdodge(jitter.width = 0.1,dodge.width = 0.2))+
  # adjust layout 
  xlab("")+
  ylab("Competition - Lottery")+
  facet_wrap(~name, scale = 'free', nrow = 2, axes = "all_y") +
  # custom colours and theme+
  theme_minimal()+
  theme(axis.text=element_text(size=18,),
        axis.title=element_text(size=22,face="bold"), plot.title = element_text(size = 18, face = "bold"),
        legend.text=element_text(size=18),
        legend.title=element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        strip.text.x = element_text(size = 20, face = "bold",
                                    margin = margin(t = 0, r = 0, b = 40, l = 40)),
        axis.title.y = element_text(margin = margin(t = 0, r = 40, b = 0, l = 0)))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")

# Delta Analyses ----

## Coop-Lott----

### Delta Risk ----

lm.up2 = lm(value ~ age.cat,
           droplevels(delta.coop_lott[delta.coop_lott$name=="Risk",]))

summary(lm.up2)
Anova(lm.up2, type=3)

emm.up2 = emmeans(lm.up2, pairwise~age.cat, adjust="Bonf")
summary(emm.up2, infer = c(T,T))

emmeans::eff_size(emm.up2, sigma = sigma(lm.up2), edf = df.residual(lm.up2)) # Effect sizes of those differences.

etaSquared(lm.up2)

### Delta Switch ----

lm.switch2 = lm(value ~ age.cat,
               droplevels(delta.coop_lott[delta.coop_lott$name=="Switch",]))

summary(lm.switch2)
Anova(lm.switch2, type=3)

emm.switch2 = emmeans(lm.switch2, pairwise~age.cat, adjust="Bonf")
summary(emm.switch2, infer = c(T,T))

emmeans::eff_size(emm.switch2, sigma = sigma(lm.switch2), edf = df.residual(lm.switch2)) # Effect sizes of those differences.

etaSquared(lm.switch2)

### Delta RT ----

lm.rt2 = lm(value ~ age.cat,
           droplevels(delta.coop_lott[delta.coop_lott$name=="RT",]))

summary(lm.rt2)
Anova(lm.rt2, type=3)

emm.rt2 = emmeans(lm.rt2, pairwise~age.cat, adjust="Bonf")
summary(emm.rt2, infer = c(T,T))

emmeans::eff_size(emm.rt2, sigma = sigma(lm.rt2), edf = df.residual(lm.rt2)) # Effect sizes of those differences.

etaSquared(lm.rt2)


## Comp-Lott----

### Delta Risk ----

lm.up3 = lm(value ~ age.cat,
            droplevels(delta.comp_lott[delta.comp_lott$name=="Risk",]))

summary(lm.up3)
Anova(lm.up3, type=3)

emm.up3 = emmeans(lm.up3, pairwise~age.cat, adjust="Bonf")
summary(emm.up3, infer = c(T,T))

emmeans::eff_size(emm.up3, sigma = sigma(lm.up3), edf = df.residual(lm.up3)) # Effect sizes of those differences.

etaSquared(lm.up3)

### Delta Switch ----

lm.switch3 = lm(value ~ age.cat,
                droplevels(delta.comp_lott[delta.comp_lott$name=="Switch",]))

summary(lm.switch3)
Anova(lm.switch3, type=3)

emm.switch3 = emmeans(lm.switch3, pairwise~age.cat, adjust="Bonf")
summary(emm.switch3, infer = c(T,T))

emmeans::eff_size(emm.switch3, sigma = sigma(lm.switch3), edf = df.residual(lm.switch3)) # Effect sizes of those differences.

etaSquared(lm.switch3)

### Delta RT ----

lm.rt3 = lm(value ~ age.cat,
            droplevels(delta.comp_lott[delta.comp_lott$name=="RT",]))

summary(lm.rt3)
Anova(lm.rt3, type=3)

emm.rt3 = emmeans(lm.rt3, pairwise~age.cat, adjust="Bonf")
summary(emm.rt3, infer = c(T,T))

emmeans::eff_size(emm.rt3, sigma = sigma(lm.rt3), edf = df.residual(lm.rt3)) # Effect sizes of those differences.

etaSquared(lm.rt3)
