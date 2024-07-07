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

# Split Violin function ----

GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

# Custom Emmeans function for modelsummary and modelplot----

tidy.emm_list <- function(x, ...) {
  s <- summary(x,...)
  ret <- data.frame(
    term      = s$emmean[,1],
    estimate  = s$emmean[,2],
    std.error= s$emmean[,3],
    conf.low  = s$emmean[,5],
    conf.high = s$emmean[,6])
  ret
}

glance.emm_list<- function(x, ...) {
  s <- summary(x, ...)
  ret <- data.frame(
    contrasts=s$contrast[,2])
  ret
}

summary(lm.up)

# Deltas ----

delta.social = coord.aggr %>% select(-flag.trial,-flag.variance,-prop, -sum, -count.trial) %>% filter(game!="lottery")%>%
  pivot_wider(names_from=game, values_from = c(mean.up, mean.rt, median.rt, mean.switch, mean.payoff)) %>%
  group_by(sub,age.cat,version)%>%
  summarise(Risk = mean.up_coordination - mean.up_competition,
            RT = mean.rt_coordination - mean.rt_competition,
            RT2 = median.rt_coordination - median.rt_competition,
            Switch = mean.switch_coordination - mean.switch_competition,
            Payoff = mean.payoff_coordination - mean.payoff_competition) %>%
  pivot_longer(!c(sub, age.cat,version)) %>%
  mutate_at(c("sub","age.cat","version","name"),factor)

delta.social$name <- fct_relevel(delta.social$name, "Risk", "Payoff", "Switch","RT","RT2")

delta.social$age.cat <- fct_relevel(delta.social$age.cat, "Children", "Adolescents", "Adults")
levels(delta.social$name)

delta.social_rt = coord.aggr_rt %>% select(-flag.trial,-flag.variance,-prop, -sum, -count.trial) %>% filter(game!="lottery")%>%
  pivot_wider(names_from=game, values_from = c(mean.up, mean.rt, median.rt, mean.switch, mean.payoff)) %>%
  group_by(sub,age.cat,version)%>%
  summarise(Risk = mean.up_coordination - mean.up_competition,
            RT = mean.rt_coordination - mean.rt_competition,
            RT2 = median.rt_coordination - median.rt_competition,
            Switch = mean.switch_coordination - mean.switch_competition,
            Payoff = mean.payoff_coordination - mean.payoff_competition) %>%
  pivot_longer(!c(sub, age.cat,version)) %>%
  mutate_at(c("sub","age.cat","version","name"),factor)

delta.social_rt <- left_join(delta.social_rt,flags)

delta.social_rt$name <- fct_relevel(delta.social_rt$name, "Risk", "Payoff", "Switch","RT","RT2")
delta.social_rt$age.cat <- fct_relevel(delta.social_rt$age.cat, "Children", "Adolescents", "Adults")

levels(delta.social_rt$name)


# Merging with control variables ----

delta.social <- left_join(delta.social,controls)

delta.social_rt <- left_join(delta.social_rt,controls)

write.csv(delta.social, file.path(here("Processed data/coordination/delta.social.csv")))

write.csv(delta.social_rt, file.path(here("Processed data/coordination/delta.social_rt.csv")))

# Plotting Deltas ----

ggplot(delta.social_rt[delta.social_rt$name !='RT2',] %>% 
         dplyr::filter(age.cat!="Adults") %>%
         droplevels(),aes("",value,
                                                    fill=age.cat,
                                                    colour = age.cat,
                                                    shape=age.cat)) +
  geom_split_violin(alpha=0.5,linewidth=0.8, color="black")+
  geom_point(position = position_jitterdodge(dodge.width = 0.6, jitter.width=0.4), alpha=0.3)+
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", position=position_dodge(.5), width=0.3,size=1, color="black")+
  ylab("Cooperation - Competition")+
  theme_minimal()+
  scale_fill_manual(values=wes_palette("Darjeeling1",2, type="discrete"))+
  scale_color_manual(values=wes_palette("Darjeeling1",2, type="discrete"))+
  xlab("")+
  facet_wrap(~name, scales ='free_y') +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=22,face="bold"), plot.title = element_text(size = 18, face = "bold"),
        legend.text=element_text(size=18),
        legend.title=element_blank(),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        strip.text.x = element_text(size = 20, face = "bold",
                                    margin = margin(t = 0, r = 0, b = 40, l = 0)))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")

# Raincloud plot ----

ggplot(delta.social_rt %>% filter(name!="RT2" &
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
  ylab("Cooperation - Competition")+
  facet_wrap(~name, scale = 'free', nrow = 2, axes = "all_y") +
  # custom colours and theme+
  theme_minimal()+
  theme(axis.text=element_text(size=18,),
        axis.title=element_text(size=22,face="bold"), plot.title = element_text(size = 18, face = "bold"),
        legend.text=element_text(size=18),
        legend.title=element_blank(),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        strip.text.x = element_text(size = 20, face = "bold",
                                    margin = margin(t = 0, r = 0, b = 40, l = 40)),
        axis.title.y = element_text(margin = margin(t = 0, r = 40, b = 0, l = 0)))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")

# Delta Social (Cooperation-Competition) Analyses ----

## Delta Risk ----

lm.up = lm(value ~ age.cat,
           droplevels(delta.social[delta.social$name=="Risk",]))

summary(lm.up)
Anova(lm.up, type=3)

emm.up = emmeans(lm.up, pairwise~age.cat, adjust="Bonf")
summary(emm.up, infer = c(T,T))

emmeans::eff_size(emm.up, sigma = sigma(lm.up), edf = df.residual(lm.up)) # Effect sizes of those differences.

etaSquared(lm.up)

### Controlling for gender----

lm.up.g = lm(value ~ age.cat + Gender, contrasts = list(Gender = "contr.sum"),
             droplevels(delta.social[delta.social$name=="Risk",]))

summary(lm.up.g)
Anova(lm.up.g, type=3)

emm.up.g = emmeans(lm.up.g, pairwise~age.cat, adjust="Bonf")
summary(emm.up.g, infer = c(T,T))

emmeans::eff_size(emm.up.g, sigma = sigma(lm.up.g), edf = df.residual(lm.up.g)) # Effect sizes of those differences.

etaSquared(lm.up.g)


### Controlling for MARS ----

lm.up.m = lm(value ~ age.cat + mars.mc,
             droplevels(delta.social[delta.social$name=="Risk",]))

summary(lm.up.m)
Anova(lm.up.m, type = 3)

emm.up.m = emmeans(lm.up.m, pairwise~age.cat, adjust="Bonf")
summary(emm.up.m, infer = c(T,T))

confint(emm.up.m, level=0.95)
emmeans::eff_size(emm.up.m, sigma = sigma(lm.up.m), edf = df.residual(lm.up.m)) # Effect sizes of those differences.

etaSquared(lm.up.m)

### Controlling for Pre-coord incorrect responses----

lm.up.pre = lm(value ~ age.cat + pre_err.mc,
               droplevels(delta.social[delta.social$name=="Risk",]))

summary(lm.up.pre)
Anova(lm.up.pre, type=3)

emm.up.pre = emmeans(lm.up.pre, pairwise~age.cat, adjust="Bonf")
summary(emm.up.pre, infer = c(T,T))

emmeans::eff_size(emm.up.pre, sigma = sigma(lm.up.pre), edf = df.residual(lm.up.pre)) # Effect sizes of those differences.

etaSquared(lm.up.pre)

### Excluding flag.RT ----

lm.up.frt = lm(value ~ age.cat,
               droplevels(delta.social_rt[delta.social_rt$name=="Risk",]))

summary(lm.up.frt)
Anova(lm.up.frt, type=3)

emm.up.frt = emmeans(lm.up.frt, pairwise~age.cat, adjust="Bonf")
summary(emm.up.frt, infer = c(T,T))

confint(emm.up.frt, level=0.95)
emmeans::eff_size(emm.up.frt, sigma = sigma(lm.up.frt), edf = df.residual(lm.up.frt)) # Effect sizes of those differences.

etaSquared(lm.up.frt)

### Excluding flag.lottery slope+ns ----

lm.up.fslope = lm(value ~ age.cat,
                  droplevels(delta.social[delta.social$name=="Risk" & delta.social$flag.lottery==0,]))

summary(lm.up.fslope)
Anova(lm.up.fslope, type=3)

emm.up.fslope = emmeans(lm.up.fslope, pairwise~age.cat, adjust="Bonf")
summary(emm.up.fslope, infer = c(T,T))

emmeans::eff_size(emm.up.fslope, sigma = sigma(lm.up.fslope), edf = df.residual(lm.up.fslope)) # Effect sizes of those differences.

etaSquared(lm.up.fslope)

### Controlling for post coord ----

lm.up.post = lm(value ~ age.cat + post_delta_ref.mc,
               droplevels(delta.social[delta.social$name=="Risk",]))

summary(lm.up.post)
Anova(lm.up.post, type=3)

emm.up.post = emmeans(lm.up.post, pairwise~age.cat, adjust="Bonf")
summary(emm.up.post, infer = c(T,T))

emmeans::eff_size(emm.up.post, sigma = sigma(lm.up.post), edf = df.residual(lm.up.post)) # Effect sizes of those differences.

etaSquared(lm.up.post)

### Controlling for risk in lottery ----

lm.up.lott = lm(value ~ age.cat + risk.lott.mc,
                droplevels(delta.social[delta.social$name=="Risk",]))

summary(lm.up.lott)
Anova(lm.up.lott, type=3)

emm.up.lott = emmeans(lm.up.lott, pairwise~age.cat, adjust="Bonf")
summary(emm.up.lott, infer = c(T,T))

emmeans::eff_size(emm.up.lott, sigma = sigma(lm.up.lott), edf = df.residual(lm.up.lott)) # Effect sizes of those differences.

etaSquared(lm.up.lott)

### All variables ----

lm.up.all = lm(value ~ age.cat + Gender + 
                 mars.mc + 
                 pre_err.mc +
                 post_delta_ref.mc +
                 risk.lott.mc,
               contrasts = list(Gender = "contr.sum"),
                droplevels(delta.social_rt[delta.social_rt$name=="Risk" & 
                                             delta.social$flag.lottery==0,]))

summary(lm.up.all)
Anova(lm.up.all, type=3)

emm.up.all = emmeans(lm.up.all, pairwise~age.cat, adjust="Bonf")
summary(emm.up.all, infer = c(T,T))

emmeans::eff_size(emm.up.all, sigma = sigma(lm.up.all), edf = df.residual(lm.up.all)) # Effect sizes of those differences.

etaSquared(lm.up.all)

### Delta UP models comparisons ----

Up.list.emm <- list("Default Model"= emm.up,
                    "Controlling for Gender"= emm.up.g,
                    "Controlling for MARS"= emm.up.m,
                    "Controlling for Pre-coord"= emm.up.pre,
                    "Controlling for Post-coord"= emm.up.post,
                    "Controlling for Lottery Risk"= emm.up.lott,
                     "Without Slope & NS flags Lottery"= emm.up.fslope,
                    "Without RT Flags"= emm.up.frt,
                    "All Predictors"= emm.up.all)


### Emmeans plot of Risk models ----

modelplot(Up.list.emm, size=.8)+
  labs(title = NULL,
       x=NULL)+
  theme(axis.text = element_text(size=16),
        plot.title = element_text(size=25, face = "bold", color = "#4cae43"),
        axis.title.x.bottom = element_text(size=14, hjust=0.6),
        legend.position = "none")+
  geom_vline(xintercept = 0, color = 'red', linetype=2)+
  aes(shape = ifelse(p.value < 0.05, "p < 0.05", "p > 0.05"))

## Delta Payoff ----

lm.payoff = lm(value ~ age.cat,
           droplevels(delta.social[delta.social$name=="Payoff",]))

summary(lm.payoff)
Anova(lm.payoff, type=3)

emm.payoff = emmeans(lm.payoff, pairwise~age.cat, adjust="Bonf")
summary(emm.payoff, infer = c(T,T))

emmeans::eff_size(emm.payoff, sigma = sigma(lm.payoff), edf = df.residual(lm.payoff)) # Effect sizes of those differences.

etaSquared(lm.payoff)

### Controlling for gender----

lm.payoff.g = lm(value ~ age.cat + Gender, contrasts = list(Gender = "contr.sum"),
             droplevels(delta.social[delta.social$name=="Payoff",]))

summary(lm.payoff.g)
Anova(lm.payoff.g, type=3)

emm.payoff.g = emmeans(lm.payoff.g, pairwise~age.cat, adjust="Bonf")
summary(emm.payoff.g, infer = c(T,T))

emmeans::eff_size(emm.payoff.g, sigma = sigma(lm.payoff.g), edf = df.residual(lm.payoff.g)) # Effect sizes of those differences.

etaSquared(lm.payoff.g)


### Controlling for MARS ----

lm.payoff.m = lm(value ~ age.cat + mars.mc, 
             droplevels(delta.social[delta.social$name=="Payoff",]))

summary(lm.payoff.m)
Anova(lm.payoff.m, type = 3)

emm.payoff.m = emmeans(lm.payoff.m, pairwise~age.cat, adjust="Bonf")
summary(emm.payoff.m, infer = c(T,T))

confint(emm.payoff.m, level=0.95)
emmeans::eff_size(emm.payoff.m, sigma = sigma(lm.payoff.m), edf = df.residual(lm.payoff.m)) # Effect sizes of those differences.

etaSquared(lm.payoff.m)

### Controlling for Pre-coord incorrect responses----

lm.payoff.pre = lm(value ~ age.cat + pre_err.mc,
               droplevels(delta.social[delta.social$name=="Payoff",]))

summary(lm.payoff.pre)
Anova(lm.payoff.pre, type=3)

emm.payoff.pre = emmeans(lm.payoff.pre, pairwise~age.cat, adjust="Bonf")
summary(emm.payoff.pre, infer = c(T,T))

emmeans::eff_size(emm.payoff.pre, sigma = sigma(lm.payoff.pre), edf = df.residual(lm.payoff.pre)) # Effect sizes of those differences.

etaSquared(lm.payoff.pre)

### Excluding flag.RT ----

lm.payoff.frt = lm(value ~ age.cat,
               droplevels(delta.social_rt[delta.social_rt$name=="Payoff",]))

summary(lm.payoff.frt)
Anova(lm.payoff.frt, type=3)

emm.payoff.frt = emmeans(lm.payoff.frt, pairwise~age.cat, adjust="Bonf")
summary(emm.payoff.frt, infer = c(T,T))

confint(emm.payoff.frt, level=0.95)
emmeans::eff_size(emm.payoff.frt, sigma = sigma(lm.payoff.frt), edf = df.residual(lm.payoff.frt)) # Effect sizes of those differences.

etaSquared(lm.payoff.frt)

### Excluding flag.lottery slope+ns ----

lm.payoff.fslope = lm(value ~ age.cat,
                  droplevels(delta.social[delta.social$name=="Payoff" & delta.social$flag.lottery==0,]))

summary(lm.payoff.fslope)
Anova(lm.payoff.fslope, type=3)

emm.payoff.fslope = emmeans(lm.payoff.fslope, pairwise~age.cat, adjust="Bonf")
summary(emm.payoff.fslope, infer = c(T,T))

emmeans::eff_size(emm.payoff.fslope, sigma = sigma(lm.payoff.fslope), edf = df.residual(lm.payoff.fslope)) # Effect sizes of those differences.

etaSquared(lm.payoff.fslope)

### Controlling for post coord ----

lm.payoff.post = lm(value ~ age.cat + post_delta_ref.mc,
                droplevels(delta.social[delta.social$name=="Payoff",]))

summary(lm.payoff.post)
Anova(lm.payoff.post, type=3)

emm.payoff.post = emmeans(lm.payoff.post, pairwise~age.cat, adjust="Bonf")
summary(emm.payoff.post, infer = c(T,T))

emmeans::eff_size(emm.payoff.post, sigma = sigma(lm.payoff.post), edf = df.residual(lm.payoff.post)) # Effect sizes of those differences.

etaSquared(lm.payoff.post)

### Controlling for Risk in lottery ----

lm.payoff.lott = lm(value ~ age.cat + risk.lott.mc,
                droplevels(delta.social[delta.social$name=="Payoff",]))

summary(lm.payoff.lott)
Anova(lm.payoff.lott, type=3)

emm.payoff.lott = emmeans(lm.payoff.lott, pairwise~age.cat, adjust="Bonf")
summary(emm.payoff.lott, infer = c(T,T))

emmeans::eff_size(emm.payoff.lott, sigma = sigma(lm.payoff.lott), edf = df.residual(lm.payoff.lott)) # Effect sizes of those differences.

etaSquared(lm.payoff.lott)

### All variables ----

lm.payoff.all = lm(value ~ age.cat + Gender + 
                 mars.mc + 
                 pre_err.mc +
                 post_delta_ref.mc +
                 risk.lott.mc,
                 contrasts = list(Gender = "contr.sum"),
               droplevels(delta.social_rt[delta.social_rt$name=="Payoff" & 
                                            delta.social$flag.lottery==0,]))

summary(lm.payoff.all)
Anova(lm.payoff.all, type=3)

emm.payoff.all = emmeans(lm.payoff.all, pairwise~age.cat, adjust="Bonf")
summary(emm.payoff.all, infer = c(T,T))

emmeans::eff_size(emm.payoff.all, sigma = sigma(lm.payoff.all), edf = df.residual(lm.payoff.all)) # Effect sizes of those differences.

etaSquared(lm.payoff.all)

### Delta payoff models comparisons ----

payoff.list.emm <- list("Default Model"= emm.payoff,
                    "Controlling for Gender"= emm.payoff.g,
                    "Controlling for MARS"= emm.payoff.m,
                    "Controlling for Pre-coord"= emm.payoff.pre,
                    "Controlling for Post-coord"= emm.payoff.post,
                    "Controlling for Lottery Risk"= emm.payoff.lott,
                    "Without Slope & NS flags Lottery"= emm.payoff.fslope,
                    "Without RT Flags"= emm.payoff.frt,
                    "All Predictors" = emm.payoff.all)


### Emmeans plot of Payoff models ----

modelplot(payoff.list.emm, size=.8)+
  labs(title = NULL,
       x=NULL)+
  theme(axis.text = element_text(size=16),
        plot.title = element_text(size=25, face = "bold"),
        axis.title.x.bottom = element_text(size=14, hjust=0.6),
        legend.position = "none")+
  geom_vline(xintercept = 0, color = 'red', linetype=2)+
  aes(shape = ifelse(p.value < 0.05, "p < 0.05", "p > 0.05"))

## Delta Switch ----

lm.switch = lm(value ~ age.cat,
               droplevels(delta.social[delta.social$name=="Switch",]))

summary(lm.switch)
Anova(lm.switch, type=3)

emm.switch = emmeans(lm.switch, pairwise~age.cat, adjust="Bonf")
summary(emm.switch, infer = c(T,T))

emmeans::eff_size(emm.switch, sigma = sigma(lm.switch), edf = df.residual(lm.switch)) # Effect sizes of those differences.

etaSquared(lm.switch)

### Controlling for gender----

lm.switch.g = lm(value ~ age.cat + Gender, contrasts = list(Gender = "contr.sum"),
                 droplevels(delta.social[delta.social$name=="Switch",]))

summary(lm.switch.g)
Anova(lm.switch.g, type=3)

emm.switch.g = emmeans(lm.switch.g, pairwise~age.cat, adjust="Bonf")
summary(emm.switch.g, infer = c(T,T))

emmeans::eff_size(emm.switch.g, sigma = sigma(lm.switch.g), edf = df.residual(lm.switch.g)) # Effect sizes of those differences.

etaSquared(lm.switch.g)


### Controlling for MARS ----

lm.switch.m = lm(value ~ age.cat + mars.mc, 
                 droplevels(delta.social[delta.social$name=="Switch",]))

summary(lm.switch.m)
Anova(lm.switch.m, type = 3)

emm.switch.m = emmeans(lm.switch.m, pairwise~age.cat, adjust="Bonf")
summary(emm.switch.m, infer = c(T,T))

confint(emm.switch.m, level=0.95)
emmeans::eff_size(emm.switch.m, sigma = sigma(lm.switch.m), edf = df.residual(lm.switch.m)) # Effect sizes of those differences.

etaSquared(lm.switch.m)

### Controlling for Pre-coord incorrect responses----

lm.switch.pre = lm(value ~ age.cat + pre_err.mc,
                   droplevels(delta.social[delta.social$name=="Switch",]))

summary(lm.switch.pre)
Anova(lm.switch.pre, type=3)

emm.switch.pre = emmeans(lm.switch.pre, pairwise~age.cat, adjust="Bonf")
summary(emm.switch.pre, infer = c(T,T))

emmeans::eff_size(emm.switch.pre, sigma = sigma(lm.switch.pre), edf = df.residual(lm.switch.pre)) # Effect sizes of those differences.

etaSquared(lm.switch.pre)

### Excluding flag.RT ----

lm.switch.frt = lm(value ~ age.cat,
                   droplevels(delta.social_rt[delta.social_rt$name=="Switch",]))

summary(lm.switch.frt)
Anova(lm.switch.frt, type=3)

emm.switch.frt = emmeans(lm.switch.frt, pairwise~age.cat, adjust="Bonf")
summary(emm.switch.frt, infer = c(T,T))

confint(emm.switch.frt, level=0.95)
emmeans::eff_size(emm.switch.frt, sigma = sigma(lm.switch.frt), edf = df.residual(lm.switch.frt)) # Effect sizes of those differences.

etaSquared(lm.switch.frt)

### Excluding flag.lottery slope+ns ----

lm.switch.fslope = lm(value ~ age.cat,
                      droplevels(delta.social[delta.social$name=="Switch" & delta.social$flag.lottery==0,]))

summary(lm.switch.fslope)
Anova(lm.switch.fslope, type=3)

emm.switch.fslope = emmeans(lm.switch.fslope, pairwise~age.cat, adjust="Bonf")
summary(emm.switch.fslope, infer = c(T,T))

emmeans::eff_size(emm.switch.fslope, sigma = sigma(lm.switch.fslope), edf = df.residual(lm.switch.fslope)) # Effect sizes of those differences.

etaSquared(lm.switch.fslope)

### Controlling for post coord ----

lm.switch.post = lm(value ~ age.cat + post_delta_ref.mc,
                    droplevels(delta.social[delta.social$name=="Switch",]))

summary(lm.switch.post)
Anova(lm.switch.post, type=3)

emm.switch.post = emmeans(lm.switch.post, pairwise~age.cat, adjust="Bonf")
summary(emm.switch.post, infer = c(T,T))

emmeans::eff_size(emm.switch.post, sigma = sigma(lm.switch.post), edf = df.residual(lm.switch.post)) # Effect sizes of those differences.

etaSquared(lm.switch.post)

### Controlling for risk in lottery ----

lm.switch.lott = lm(value ~ age.cat + risk.lott.mc,
                    droplevels(delta.social[delta.social$name=="Switch",]))

summary(lm.switch.lott)
Anova(lm.switch.lott, type=3)

emm.switch.lott = emmeans(lm.switch.lott, pairwise~age.cat, adjust="Bonf")
summary(emm.switch.lott, infer = c(T,T))

emmeans::eff_size(emm.switch.lott, sigma = sigma(lm.switch.lott), edf = df.residual(lm.switch.lott)) # Effect sizes of those differences.

etaSquared(lm.switch.lott)

### All variables ----

lm.switch.all = lm(value ~ age.cat + Gender + 
                     mars.mc + 
                     pre_err.mc +
                     post_delta_ref.mc +
                     risk.lott.mc,
                   contrasts = list(Gender = "contr.sum"),
                   droplevels(delta.social_rt[delta.social_rt$name=="Switch" & 
                                                delta.social$flag.lottery==0,]))

summary(lm.switch.all)
Anova(lm.switch.all, type=3)

emm.switch.all = emmeans(lm.switch.all, pairwise~age.cat, adjust="Bonf")
summary(emm.switch.all, infer = c(T,T))

emmeans::eff_size(emm.switch.all, sigma = sigma(lm.switch.all), edf = df.residual(lm.switch.all)) # Effect sizes of those differences.

etaSquared(lm.switch.all)


### Delta switch models comparisons ----

switch.list.emm <- list("Default Model"= emm.switch,
                        "Controlling for Gender"= emm.switch.g,
                        "Controlling for MARS"= emm.switch.m,
                        "Controlling for Pre-coord"= emm.switch.pre,
                        "Controlling for Post-coord"= emm.switch.post,
                        "Controlling for Lottery Risk"= emm.switch.lott,
                        "Without Slope & NS flags Lottery"= emm.switch.fslope,
                        "Without RT Flags"= emm.switch.frt,
                        "All Predictors" = emm.switch.all)


### Emmeans plot of Switch models ----

modelplot(switch.list.emm , size=.8)+
  labs(title = NULL,
       x=NULL)+
  theme(axis.text = element_text(size=16),
        plot.title = element_text(size=25, face = "bold"),
        axis.title.x.bottom = element_text(size=14, hjust=0.6),
        legend.position = "none")+
  geom_vline(xintercept = 0, color = 'red', linetype=2)+
  aes(shape = ifelse(p.value < 0.05, "p < 0.05", "p > 0.05"))

## Delta RT ----

lm.rt = lm(value ~ age.cat,
               droplevels(delta.social[delta.social$name=="RT",]))

summary(lm.rt)
Anova(lm.rt, type=3)

emm.rt = emmeans(lm.rt, pairwise~age.cat, adjust="Bonf")
summary(emm.rt, infer = c(T,T))

emmeans::eff_size(emm.rt, sigma = sigma(lm.rt), edf = df.residual(lm.rt)) # Effect sizes of those differences.

etaSquared(lm.rt)

### Controlling for gender----

lm.rt.g = lm(value ~ age.cat + Gender, contrasts = list(Gender = "contr.sum"),
                 droplevels(delta.social[delta.social$name=="RT",]))

summary(lm.rt.g)
Anova(lm.rt.g, type=3)

emm.rt.g = emmeans(lm.rt.g, pairwise~age.cat, adjust="Bonf")
summary(emm.rt.g, infer = c(T,T))

emmeans::eff_size(emm.rt.g, sigma = sigma(lm.rt.g), edf = df.residual(lm.rt.g)) # Effect sizes of those differences.

etaSquared(lm.rt.g)


### Controlling for MARS ----

lm.rt.m = lm(value ~ age.cat + mars.mc, 
                 droplevels(delta.social[delta.social$name=="RT",]))

summary(lm.rt.m)
Anova(lm.rt.m, type = 3)

emm.rt.m = emmeans(lm.rt.m, pairwise~age.cat, adjust="Bonf")
summary(emm.rt.m, infer = c(T,T))

confint(emm.rt.m, level=0.95)
emmeans::eff_size(emm.rt.m, sigma = sigma(lm.rt.m), edf = df.residual(lm.rt.m)) # Effect sizes of those differences.

etaSquared(lm.rt.m)

### Controlling for Pre-coord incorrect responses----

lm.rt.pre = lm(value ~ age.cat + pre_err.mc,
                   droplevels(delta.social[delta.social$name=="RT",]))

summary(lm.rt.pre)
Anova(lm.rt.pre, type=3)

emm.rt.pre = emmeans(lm.rt.pre, pairwise~age.cat, adjust="Bonf")
summary(emm.rt.pre, infer = c(T,T))

emmeans::eff_size(emm.rt.pre, sigma = sigma(lm.rt.pre), edf = df.residual(lm.rt.pre)) # Effect sizes of those differences.

etaSquared(lm.rt.pre)

### Excluding flag.RT ----

lm.rt.frt = lm(value ~ age.cat,
                   droplevels(delta.social_rt[delta.social_rt$name=="RT",]))

summary(lm.rt.frt)
Anova(lm.rt.frt, type=3)

emm.rt.frt = emmeans(lm.rt.frt, pairwise~age.cat, adjust="Bonf")
summary(emm.rt.frt, infer = c(T,T))

confint(emm.rt.frt, level=0.95)
emmeans::eff_size(emm.rt.frt, sigma = sigma(lm.rt.frt), edf = df.residual(lm.rt.frt)) # Effect sizes of those differences.

etaSquared(lm.rt.frt)

### Excluding flag.lottery slope+ns ----

lm.rt.fslope = lm(value ~ age.cat,
                      droplevels(delta.social[delta.social$name=="RT" & delta.social$flag.lottery==0,]))

summary(lm.rt.fslope)
Anova(lm.rt.fslope, type=3)

emm.rt.fslope = emmeans(lm.rt.fslope, pairwise~age.cat, adjust="Bonf")
summary(emm.rt.fslope, infer = c(T,T))

emmeans::eff_size(emm.rt.fslope, sigma = sigma(lm.rt.fslope), edf = df.residual(lm.rt.fslope)) # Effect sizes of those differences.

etaSquared(lm.rt.fslope)

### Controlling for post coord ----

lm.rt.post = lm(value ~ age.cat + post_delta_ref.mc,
                    droplevels(delta.social[delta.social$name=="RT",]))

summary(lm.rt.post)
Anova(lm.rt.post, type=3)

emm.rt.post = emmeans(lm.rt.post, pairwise~age.cat, adjust="Bonf")
summary(emm.rt.post, infer = c(T,T))

emmeans::eff_size(emm.rt.post, sigma = sigma(lm.rt.post), edf = df.residual(lm.rt.post)) # Effect sizes of those differences.

etaSquared(lm.rt.post)

### Controlling for risk in lottery ----

lm.rt.lott = lm(value ~ age.cat + risk.lott.mc,
                    droplevels(delta.social[delta.social$name=="RT",]))

summary(lm.rt.lott)
Anova(lm.rt.lott, type=3)

emm.rt.lott = emmeans(lm.rt.lott, pairwise~age.cat, adjust="Bonf")
summary(emm.rt.lott, infer = c(T,T))

emmeans::eff_size(emm.rt.lott, sigma = sigma(lm.rt.lott), edf = df.residual(lm.rt.lott)) # Effect sizes of those differences.

etaSquared(lm.rt.lott)

### All variables ----

lm.rt.all = lm(value ~ age.cat + Gender + 
                     mars.mc + 
                     pre_err.mc +
                     post_delta_ref.mc +
                     risk.lott.mc,
               contrasts = list(Gender = "contr.sum"),
                   droplevels(delta.social_rt[delta.social_rt$name=="RT" & 
                                                delta.social$flag.lottery==0,]))

summary(lm.rt.all)
Anova(lm.rt.all, type=3)

emm.rt.all = emmeans(lm.rt.all, pairwise~age.cat, adjust="Bonf")
summary(emm.rt.all, infer = c(T,T))

emmeans::eff_size(emm.rt.all, sigma = sigma(lm.rt.all), edf = df.residual(lm.rt.all)) # Effect sizes of those differences.

etaSquared(lm.rt.all)
### Delta rt models comparisons ----

rt.list.emm <- list("Default Model"= emm.rt,
                        "Controlling for Gender"= emm.rt.g,
                        "Controlling for MARS"= emm.rt.m,
                        "Controlling for Pre-coord"= emm.rt.pre,
                        "Controlling for Post-coord"= emm.rt.post,
                        "Controlling for Lottery Risk"= emm.rt.lott,
                        "Without Slope & NS flags Lottery"= emm.rt.fslope,
                        "Without RT Flags"= emm.rt.frt,
                    "All Predictors"= emm.rt.all)


### Emmeans plot of RT models ----

modelplot(rt.list.emm  , size=.8)+
  labs(title = NULL,
       x=NULL)+
  theme(axis.text = element_text(size=16),
        plot.title = element_text(size=25, face = "bold"),
        axis.title.x.bottom = element_text(size=14, hjust=0.6),
        legend.position = "none")+
  geom_vline(xintercept = 0, color = 'red', linetype=2)+
  aes(shape = ifelse(p.value < 0.05, "p < 0.05", "p > 0.05"))


# P value ANOVA Plots of Predictors ----

## Risk ----

anova.up.m <- Anova(lm.up.m, type=3)
anova.up.pre <-  Anova(lm.up.pre, type=3)
anova.up.post <- Anova(lm.up.post, type=3)

pvals.up <- c(anova.up.m$"Pr(>F)"[3],
              anova.up.pre$"Pr(>F)"[3],
              anova.up.post$"Pr(>F)"[3])
# Determine shape based on significance level
shapes <- ifelse(pvals.up < 0.05, "p < 0.05", "p > 0.05")

# Extract coefficients and standard errors from linear models
coefs.up <- c(coef(lm.up.m)["mars.mc"], 
                  coef(lm.up.pre)["pre_err"],
                  coef(lm.up.post)["post_delta_ref"])

se.up <- c(sqrt(vcov(lm.up.m)[4,4]), 
        sqrt(vcov(lm.up.pre)[4,4]),
        sqrt(vcov(lm.up.post)[4,4])) # Standard errors for each coefficient

# Calculate upper and lower bounds of confidence intervals
ci_lower.up <- mars.mc=confint(emm.up.m)$emmeans[1,5]  # 95% confidence interval
ci_upper.up <- coefs.up  + 1.96 * se.up  # 95% confidence interval

# Create a data frame for plotting
preds.up <- data.frame(Predictor = c("Mars", "Pre-Cooord", "Post-Coord"),
                       DV="Risk",
                 p_value = pvals.up,
                 Coefficient = coefs.up ,
                 CI_lower = ci_lower.up,
                 CI_upper = ci_upper.up,
                 Shape = shapes)

# Plot using ggplot2
ggplot(preds.up, aes(x = Predictor, y = p_value, color=Predictor, shape = Shape)) +
  geom_point(size = 5) +
  # geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.3) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red4") +
  annotate("text",x=2, y=0.055, label="Alpha Level", size=4, color="red4")+
  labs(x = "ANOVA Models", y = "P Values", title = "P Values of Risk with 95% CI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15))

## RT ----

anova.rt.m <- Anova(lm.rt.m, type=3)
anova.rt.pre <-  Anova(lm.rt.pre, type=3)
anova.rt.post <- Anova(lm.rt.post, type=3)

pvals.rt <- c(anova.rt.m$"Pr(>F)"[3],
              anova.rt.pre$"Pr(>F)"[3],
              anova.rt.post$"Pr(>F)"[3])
# Determine shape based on significance level
shapes <- ifelse(pvals.rt < 0.05, "p < 0.05", "p > 0.05")

# Extract coefficients and standard errors from linear models
coefs.rt <- c(coef(lm.rt.m)["mars.mc"], 
              coef(lm.rt.pre)["pre_err"],
              coef(lm.rt.post)["post_delta_ref"])

se.rt <- c(sqrt(vcov(lm.rt.m)[4,4]), 
           sqrt(vcov(lm.rt.pre)[4,4]),
           sqrt(vcov(lm.rt.post)[4,4])) # Standard errors for each coefficient

# Calculate rtper and lower bounds of confidence intervals
ci_lower.rt <- coefs.rt  - 1.96 * se.rt  # 95% confidence interval
ci_upper.rt <- coefs.rt  + 1.96 * se.rt  # 95% confidence interval

# Create a data frame for plotting
preds.rt <- data.frame(Predictor = c("Mars", "Pre-Cooord", "Post-Coord"),
                       DV="RT",
                       p_value = pvals.rt,
                       Coefficient = coefs.rt ,
                       CI_lower = ci_lower.rt,
                       CI_upper = ci_rtper.rt,
                       Shape = shapes)

# Plot using ggplot2
ggplot(preds.rt, aes(x = Predictor, y = p_value, color=Predictor, shape = Shape)) +
  geom_point(size = 5) +
  # geom_errorbar(aes(ymin = CI_lower, ymax = CI_rtper), width = 0.3) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red4") +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red4") +
  annotate("text",x=2, y=0.055, label="Alpha Level 0.05", size=4, color="red4")+
  labs(x = "ANOVA Models", y = "P Values", title = "P Values of Risk with 95% CI") +
  theme_few() +
  scale_y_log10(breaks = c(round(preds.rt$p_value, 2), 0.05))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15))


bla <- rbind(preds.up,preds.rt)

ggplot(bla, aes(x = Predictor, y = p_value, color=Predictor, shape = Shape, fill=DV)) +
  geom_point(size = 5) +
  # geom_errorbar(aes(ymin = CI_lower, ymax = CI_rtper), width = 0.3) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red4") +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red4") +
  annotate("text",x=2, y=0.055, label="Alpha Level 0.05", size=4, color="red4")+
  labs(x = "ANOVA Models", y = "P Values", title = "P Values of Risk with 95% CI") +
  theme_minimal() +
  scale_y_log10()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15))+
  facet_wrap(~DV)
