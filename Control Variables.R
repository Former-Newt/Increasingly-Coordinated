# Demographics ----
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
                        modelsummary,
                        GGally,
                        sjPlot)

invisible(lapply(lapply(load_pkg, rlang::quo_name),
                 library,
                 character.only = TRUE
))

my_fn <- function(data, mapping, method="loess", ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point(colour="#1cade4", alpha=0.2) + 
    geom_smooth(method=method,colour="#1cade4",fill="#1cade4",...)
  p
}



## Adolescents ----

ado.dem <- read.csv(file.path(here("Unprocessed data/demographics/adolescenti/DEM_wide_adjusted2.csv")),
                    header=TRUE, sep=",") %>% 
  select(-X) %>% mutate_at(c("sub","Gender","class.age"), factor) %>% 
  mutate(Age = age.months / 12,age.months=NULL) %>% mutate_if(is.numeric, round, 0)

summary(ado.dem)
## Adults ----

adu1.dem <- read.csv(file.path(here("Unprocessed data/demographics/adulti/dem.wide.sel2.csv")),
         header=TRUE, sep=",", dec=",") %>% 
  select(sub,Gender,Age) %>% mutate_at(c("sub","Gender"), factor)

adu2.dem <- read.csv(file.path(here("Unprocessed data/demographics/adulti/dem.nov.csv")),
                     header=TRUE, sep=",", dec=",") %>% 
  select(sub,Gender,Age) %>% mutate_at(c("sub","Gender"), factor)

adu.dem <- rbind(adu1.dem,adu2.dem) %>% filter(!is.na(Age)) %>%
  mutate(Age = as.numeric(gsub("\\D", "", Age)))

summary(adu.dem)

adu.dem <- adu.dem %>% mutate(Gender = case_when(Gender == "Femminile" ~ "F",
                                   Gender == "Maschile" ~ "M",
                                   Gender == "Altro / Non indica" ~ NA)) %>%
  mutate_if(is.character,as.factor)

## Kids ----

kids.dem <- read.csv(file.path(here("Unprocessed data/demographics/bambini/dem_bambini.csv")),
                    header=TRUE, sep=",") %>% 
  select(-X) %>% mutate_at(c("sub","Gender","class.age"), factor) %>% 
  mutate(Age = age.months / 12,age.months=NULL) %>% mutate_if(is.numeric, round, 0)


dems <- rbind(ado.dem %>% select(sub,Gender,Age),adu.dem,kids.dem %>% select(sub,Gender,Age))

write.csv(dems, file.path(here("Processed data/Control Variables/dems.csv")))

# MaRs ----

## Adolescents ----

ado2.dem <- read.csv(file.path(here("Unprocessed data/demographics/adolescenti/DEM2_wide_adjusted.csv")),
                    header=TRUE, sep=";") %>% 
  select(-X) %>% mutate_at(c("sub","Gender","class.age"), factor) %>% 
  mutate(Age = age.months / 12,age.months=NULL) %>% mutate_if(is.numeric, round, 0)

# ado2 is the demographic dataset containing the sub numbers matching MARS, but not coord,
# therefore it is needed to get to the common sub number by merging based on the participants' names

ado2.mars <- read.csv(file.path(here("Unprocessed data/mars/adolescenti/MARS.ALL.wide.csv")),
                     header=TRUE, sep=",") %>%
  select(-c(X,version)) %>% mutate_at(c("sub"), factor) %>% na.omit

ado2.mars <- left_join(ado2.dem, ado2.mars)

ado2.mars %>% 
  group_by(sub.name,class,Gender) %>% count() %>% #some duplicates, but those duplicates have NA on mars
  filter(n()>1)

ado.mars<- full_join(ado2.mars, ado.dem, by="sub.name")


ado.mars <- ado.mars %>% select(sub.y,mean,sub.name) %>% rename(sub=sub.y) %>% na.omit()


ado.mars %>% group_by(sub,sub.name) %>% filter(n()>1)

# There are Duplicates of 3 participants since 2 different subs in ado2.mars
# have the same sub # in ado2.dem


#Removing manually Duplicates of 3 participants doing it automatically leaves the 
#same participant twice with a different sub number

ado.mars <- ado.mars[!(row.names(ado.mars) %in% c("9","178","55","228","24","232")),]

#Removed: 7321766  with mean 0.833
# 7322040  with mean 0.312
# 7312957 and 7321891  with mean 0.8125 (two people with same mean mars and repeated sub #)
# 7321883  with mean 0.733 and 7322023 with mean 0.867

## Adults ----

adu1.mars <- read.csv(file.path(here("Unprocessed data/mars/adulti/MARS.all.wide.csv")),
                                      header=TRUE, sep=",") %>%
  select(sub,mean,-c(X,version)) %>% mutate_at(c("sub"), factor) %>% na.omit

adu2.mars <- read.csv(file.path(here("Unprocessed data/mars/adulti/MARS.november.csv")),
                      header=TRUE, sep=",") %>%
  select(sub,mean,-version) %>% mutate_at(c("sub"), factor) %>% na.omit


adu.mars <- rbind(adu1.mars,adu2.mars)

## Children ----

kids.mars1 <- read.csv(file.path(here("Unprocessed data/mars/bambini/mars_bambini.csv")),
                     header=TRUE, sep=",") %>%
  select(sub,mean,-c(X,version)) %>% mutate_at(c("sub"), factor)

kids.mars <- left_join(kids.dem,kids.mars1)

kids.mars <- kids.mars %>%
  group_by(sub.name) %>%
  mutate(mean = ifelse(is.na(mean), mean(mean, na.rm = TRUE), mean),
         Age = ifelse(is.na(Age), mean(Age, na.rm = TRUE), Age)) %>%
  filter(!grepl("^1097", as.factor(sub))) %>% droplevels() %>%  # Filter subs starting with "1097" incopatible with other measures such as coord
  filter(!is.na(mean)) %>% ungroup()

kids.mars %>% group_by(sub.name) %>% count() %>% filter(n>2)

## Merging ----

mars <- rbind(ado.mars %>% select(sub,mean),adu.mars, kids.mars %>% select(sub,mean)) %>% rename(mean_mars=mean)

# mars$age.cat <- as.factor(sapply(mars$sub, get_category))

mars$version <- as.factor(sapply(mars$sub, get_version))

# mars <- mars %>%
#   mutate(mars_age.cat = case_when(
#     substr(sub, 1, 1) == "7" ~ "Adolescents",
#     substr(sub, 1, 1) == "8" ~ "Adults",
#     substr(sub, 1, 1) == "9" ~ "Adults2",
#     substr(sub, 1, 1) == "1" ~ "Children",
#   ))


write.csv(mars, file.path(here("Processed data/Control Variables/mars.csv")))


#Pre-coord----

ado.pre <- read.csv(file.path(here("Unprocessed data/coordination/pre-coordination-questionnaire/adolescenti/PRE-coord_preprocessed.csv"))) %>%
  select(-X)

adu1.pre <- read.csv(file.path(here("Unprocessed data/coordination/pre-coordination-questionnaire/adulti/PRE-coord_preprocessed_ADU.csv"))) %>%
  select(-X)

adu2.pre <- read.csv(file.path(here("Unprocessed data/pre-coord_nov2023/PRE-coord_preprocessed_ADU2.csv"))) %>%
  select(-X)

kids.pre <- read.csv(file.path(here("Unprocessed data/coordination/pre-coordination-questionnaire/bambini/PRE-coord_preprocessed_BAMB.csv")))%>%
  select(-X)


pre.coord <- rbind(ado.pre,adu1.pre,adu2.pre,kids.pre)

length(unique(pre.coord$sub))

# setdiff(pre.coord$sub,coord.trials$sub) # Subs that exist in PRE but not in coord, to be removed
# 
# pre.coord <- pre.coord[(pre.coord$sub %in% coord.trials$sub),] # Keeping people that only exist in coord as well

pre.coord <- pre.coord %>% mutate_if(is.character, as.factor) %>%
  mutate(
    Display = recode_factor(
      Display,
      soloseinsieme = "coordination",
      solosesoli = "competition",
      soloseacaso = "lottery"),
    q_nr = ifelse(Display !="general", Screen.Counter-1, Screen.Counter)) %>% 
  select(-c(Screen.Counter,Response.Type)) %>%
  filter(q_nr>0) %>%
  rename("game"= "Display") %>% mutate_at('sub',factor)

pre.coord <- right_join(pre.coord, coord.aggr %>% select(sub,age.cat) %>% unique()) %>% drop_na()

pre.aggr <- pre.coord %>% group_by(game, q_nr, age.cat) %>%
  summarise(total_count = n(),
            .groups = 'drop') %>%
  mutate(total_incorrect = case_when(age.cat== "Adolescents" ~ total_count - length(unique(pre.coord$sub[pre.coord$age.cat== "Adolescents"])),
                                     age.cat== "Adults" ~ total_count - length(unique(pre.coord$sub[pre.coord$age.cat== "Adults"])),
                                     age.cat== "Children" ~ total_count - length(unique(pre.coord$sub[pre.coord$age.cat== "Children"]))))

pre.coord.sub <- pre.coord %>% group_by(sub, game, q_nr, age.cat) %>%
  summarise(total_count = n(),
            .groups = 'drop',
            total_incorrect= total_count-1) %>%
  mutate(error_flag = ifelse(total_incorrect>0,1,0))

pre.err.sub <- pre.coord.sub %>% select(sub,age.cat, total_incorrect) %>% group_by(sub,age.cat)%>% 
  summarise(pre_err = sum(total_incorrect), .groups = 'drop')


pre.err.one <- pre.coord.sub %>% group_by(game, q_nr, age.cat) %>% 
  summarise(least_one_tot= sum(error_flag)) %>%
  mutate(prc_error = case_when(age.cat== "Adolescents" ~ least_one_tot/length(unique(pre.coord.sub$sub[pre.coord.sub$age.cat== "Adolescents"]))*100,
                               age.cat== "Adults" ~ least_one_tot/length(unique(pre.coord.sub$sub[pre.coord.sub$age.cat== "Adults"]))*100,
                               age.cat== "Children" ~ least_one_tot/length(unique(pre.coord.sub$sub[pre.coord.sub$age.cat== "Children"]))*100))


write.csv(pre.coord, file.path(here("Processed data/Control Variables/pre.coord.csv")))
write.csv(pre.err.sub, file.path(here("Processed data/Control Variables/pre.err.sub.csv")))
write.csv(pre.err.one, file.path(here("Processed data/Control Variables/pre.err.one.csv")))

## Plots ----
ggplot(pre.aggr, aes(q_nr, total_incorrect,
                     fill = game,
                     col= game))+ # Raw number of attemps graph  
  geom_col(position = 'dodge', color="black")+
  facet_wrap(~age.cat)+
  theme_minimal()

ggplot(pre.err.one, # % of participants with at least one mistake
       aes(q_nr, prc_error,
           fill = game,
           col= game)) +     
  geom_col(position = 'dodge', color="black")+
  labs(x= "Question Number", y ="% of at Least One Mistake", fill="Condition")+
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0,100))+
  facet_wrap(~age.cat)+
  theme_minimal()

ggplot(pre.err.sub, # % of participants with at least one mistake
       aes(x=age.cat,y=pre_err,
           fill = age.cat,
           col= age.cat)) +     
  geom_bar(position='dodge', stat='summary', fun='mean')+
  theme_minimal()

ggplot(pre.err.sub, aes(age.cat, pre_err)) +
  geom_bar_pattern(
    aes(
      pattern_filename = I(here("cross.png"))
    ), 
    pattern          = 'image', 
    pattern_type     = 'tile',
    pattern_scale = 0.3,
    colour           = 'black',
    fill='red4',
    linewidth =  1.5,
    stat='summary', fun='mean'
    
  ) +
  theme_pubclean()+
  theme(
    axis.text = element_text(size = 20)
  )

### Analyses ----

lm.pre <- lm(pre_err ~ age.cat, 
                        contrast= list(age.cat='contr.sum'),
               pre.err.sub)

summary(lm.pre)

Anova(lm.pre, type=3)

emm.lm.pre = emmeans(lm.pre, pairwise~age.cat, adjust="Bonf", infer=c(T,T))

summary(emm.lm.pre)


#Post-coord ----

ado.post <- read.csv(file.path(here("Unprocessed data/coordination/post-coordination.questionnaire/adolescenti/POST-coord_preprocessed.csv"))) %>%
  select(-X) %>% rename(Question=Question.Key)


adu1.post <- read.csv(file.path(here("Unprocessed data/coordination/post-coordination.questionnaire/adulti/POST-coord_preprocessedADU.csv"))) %>%
  select(-X)

adu2.post <- read.csv(file.path(here("Unprocessed data/post-coord_nov2023/POST-coord_preprocessedADU2.csv"))) %>%
  select(-X)

kids.post <- read.csv(file.path(here("Unprocessed data/coordination/post-coordination.questionnaire/bambini/POST-coord_preprocessedBAMB.csv")))%>%
                     select(-X)

post.coord <- rbind(ado.post,adu1.post,adu2.post,kids.post) %>% mutate_all(~ifelse(. == "", NA, .)) %>% drop_na()

post.coord <- post.coord %>%
  mutate(Question= case_when(
    grepl("^response-1", Question) ~ "Question_1",
    grepl("^response-2", Question) ~ "Question_2",
    grepl("^response-3", Question) ~ "Question_3",
    grepl("^response-4", Question) ~ "Question_4",
    grepl("^response-5", Question) ~ "Question_5",
    grepl("^response-6", Question) ~ "Question_6",
    grepl("^response-7", Question) ~ "Question_7",
    grepl("^response-8", Question) ~ "Question_8")) %>% 
  mutate_at(c("sub","Question"), factor)

post.coord<- left_join(post.coord, coord.aggr %>% select(sub,age.cat) %>% unique()) %>% drop_na()

post.coord %>% select(sub,age.cat) %>% group_by(age.cat) %>% count()

## Sliders----

sliders <- c('Question_5', 'Question_6', 'Question_7', 'Question_8')

post.sliders <- post.coord %>% filter(Question %in% sliders) %>%
  droplevels()

post.sliders <- post.sliders %>% mutate(game= ifelse(post.sliders$Question %in% c("Question_5", "Question_6"), "Coordination", "Competition"),
                        ref= ifelse(post.sliders$Question %in% c("Question_5", "Question_7"), "Self", "Other"),
                        Response = as.numeric(Response))


post.sliders.sub <- post.sliders %>% select(-c(Question,game,age.cat)) %>% group_by(sub,ref) %>% summarise(Response=sum(Response)) %>% 
  pivot_wider(names_from=ref, values_from = Response) %>%
  summarise(post_delta_ref = Other - Self)

post.sliders.other <- post.sliders %>% filter(ref=="Other") %>% droplevels()%>% group_by(age.cat,game,ref) %>%
  mutate_at('Response', as.numeric) %>%
  summarise(means= mean(Response),
            stdev= sd(Response))

write.csv(post.coord , file.path(here("Processed data/Control Variables/post.coord.csv")))
write.csv(post.sliders.sub , file.path(here("Processed data/Control Variables/post.sliders.sub.csv")))

### Plot----


ggplot(post.sliders %>% filter(ref=="Other") %>% droplevels(),
       aes(age.cat, Response,
           fill = age.cat,
           col= age.cat)) +     
  geom_bar(position = 'dodge', color="black", stat='summary', fun="mean",size=0.8,show.legend = FALSE)+
  scale_y_continuous(name= NULL)+
  scale_x_discrete(name= NULL)+
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",color="black", position=position_dodge(.5), width=0.3,size=0.8)+
  theme_minimal()+
  theme(axis.text = element_text(size = 20, color = "black"))

ggbetweenstats(
  data = post.sliders %>% filter(ref=="Other") %>% droplevels(),
  x = age.cat,
  y = Response,
  fill=age.cat,
  p.adjust.method = "bonferroni",
  boxplot.args = list(width = 0.3, alpha = 0.2),
  violin.args = list(width = 0.5, alpha = 0.2),
  ggplot.component = 
)

### Analyses ----

lm.post.sliders <- lmer(Response ~ age.cat*game*ref +(1|sub), 
                        contrast= list(age.cat='contr.sum',
                                       game='contr.sum',
                                       ref='contr.sum'),
                        post.sliders)

summary(lm.post.sliders)

Anova(lm.post.sliders, type=3)

emm.post.sliders = emmeans(lm.post.sliders, pairwise~age.cat|ref, adjust="Bonf", infer=c(T,T))

summary(emm.post.sliders)


## Multiple Choice ----

choice <- c('Question_1', 'Question_3')


post.choice  <- post.coord %>% filter(Question %in% choice) %>% mutate(game= ifelse(Question %in% "Question_1", "Lott-Coop", "Lott-Comp")) %>%
mutate_if(is.character,as.factor) %>%
  droplevels


post.choice.grouped <- post.choice %>%
  group_by(age.cat, game) %>% 
  summarise(Sì= sum(Response=="Sì"),
            No= sum(Response=="No"),
            'Non so'= sum(Response=="Non so")) %>% 
  pivot_longer(3:5,
               names_to = "Response",
               values_to="Count") %>% mutate_at("Response",factor)

write.csv(post.choice , file.path(here("Processed data/Control Variables/post.choice.csv")))

### Plot----

ggplot(post.choice.grouped, # Raw number of attemps graph
       aes(game,Count,
           fill = Response,
           col= Response)) +     
  geom_col(position = 'stack',color="black")+
  labs(x= "Question Number", y ="Response Count", fill="Response")+
  facet_wrap(~age.cat)+
  theme_pubr()


### Analyses ----

post.choice$Response <- relevel(post.choice$Response, ref = "Non so")
post.choice$game

mult.post.choice <- multinom(formula = Response ~ age.cat*game, data = post.choice)


summary(mult.post.choice)


z_stats <- summary(mult.post.choice)$coefficients/
  summary(mult.post.choice)$standard.errors

p_values <- (1 - pnorm(abs(z_stats)))*2

data.frame(t(p_values))

coef_summary <- summary(mult.post.choice)$coefficients


# Risk Lottery ----

risk.lottery <- coord.aggr %>% filter(game=="lottery")%>% select(sub,sum,age.cat) %>% rename('risk.lott'=sum)


## Plot----

ggplot(risk.lottery, 
       aes(age.cat, risk.lott,
           fill = age.cat,
           col= age.cat)) +     
  geom_bar(position = 'dodge', color="black", stat='summary', fun="mean")+
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", position=position_dodge(.5), width=0.3,size=1,color="black")+
  theme_minimal()

ggbetweenstats(
  data = post.sliders %>% filter(ref=="Other") %>% droplevels(),
  x = age.cat,
  y = Response,
  fill=age.cat,
  p.adjust.method = "bonferroni",
  boxplot.args = list(width = 0.3, alpha = 0.2),
  violin.args = list(width = 0.5, alpha = 0.2),
  ggplot.component = 
)

## Analyses ----

lm.risk.lott <- lm(risk.lott ~ age.cat, 
                        contrast= list(age.cat='contr.sum'),
                     risk.lottery)

summary(lm.risk.lott)

Anova(lm.risk.lott, type=3)

emm.risk.lott = emmeans(lm.risk.lott, pairwise~age.cat, adjust="Bonf", infer=c(T,T))

summary(emm.risk.lott)


# Creating Control Variables Dataset ----

controls <- list(flags %>% select(sub,flag.lottery),
                 risk.lottery %>% select(-age.cat),
                 dems,
                 mars,
                 pre.err.sub %>% select(-age.cat) %>%
                   mutate(pre_err=-1*pre_err) %>% mutate_at("sub",factor),
                 post.sliders.sub %>% mutate_at("sub",factor))

controls <- controls %>% reduce(full_join, by='sub')

controls <- inner_join(coord.aggr %>% select(sub,age.cat) %>% unique(), controls)

controls$version <- as.factor(sapply(controls$sub, get_version))

controls<- controls %>%
  mutate(mars_age.cat = case_when(
    age.cat=="Adolescents" ~ "Adolescents",
    substr(sub, 1, 1)  == "8" ~ "Adults",
    substr(sub, 1, 1) == "9" ~ "Adults2",
    age.cat=="Children" ~ "Children"
  )) %>% mutate_at("mars_age.cat",factor)

controls$age.cat<- fct_relevel(controls$age.cat, "Children", "Adolescents", "Adults")

controls$mars_age.cat<- fct_relevel(controls$mars_age.cat, "Children", "Adolescents", "Adults", "Adults2")
levels(controls$age.cat)

controls <- controls %>% ungroup() %>%
  mutate(mars.mc= as.numeric(ifelse(version == "ver1", scale(mean_mars, scale=F), NA)),
         risk.lott.mc = as.numeric(scale(risk.lott,scale=F)),
         pre_err.mc= as.numeric(scale(pre_err,scale=F)),
         post_delta_ref.mc= as.numeric(scale(post_delta_ref,scale=F))) 



controls <- controls %>% group_by(mars_age.cat) %>% mutate(mars.gmc =as.numeric(scale(mean_mars, scale=F)))


write.csv(controls, file.path(here("Processed data/Control Variables/controls.csv")))

# Controls ----

controls.l <- controls %>% mutate_at("flag.lottery", as.character) %>% 
  mutate_at("flag.lottery",as.numeric) %>% 
  pivot_longer(cols = c("flag.lottery","risk.lott","mean_mars","mars.mc","pre_err","post_delta_ref"),
                           names_to = "ctrl_name",
                           values_to = "ctrl_value")

ggplot(controls.l, aes(x = "", y = value, fill = age.cat, color=age.cat,
                                                       shape=age.cat)) +
  geom_rain(rain.side = "l",
            violin.args = list(alpha=0.7,linewidth=1),
            boxplot.args =list(alpha= 0.7, colour="black", outlier.shape = NA),
            boxplot.args.pos = list(
              position = ggpp::position_dodgenudge(width = 0.2), width = 0.16),
            point.args.pos= list(position = position_jitter(width = 0.08)),
            point.args = list(alpha=0,aes(group=age.cat)))+
  geom_point(alpha=0.15,position = position_jitterdodge(jitter.width = 0.1,dodge.width = 0.2))+
  # adjust layout 
  xlab("")+
  ylab("Cooperation - Competition")+
  facet_wrap(~ctrl_name, scale = 'free', nrow = 1, axes = "all_y") +
  # custom colours and theme+
  theme_minimal()+
  theme(axis.text=element_text(size=18,),
        axis.title=element_text(size=22,face="bold"), plot.title = element_text(size = 18, face = "bold"),
        legend.text=element_text(size=18),
        legend.title=element_blank(),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        strip.text.x = element_text(size = 20, face = "bold",
                                    margin = margin(t = 0, r = 0, b = 40, l = 40)),
        axis.title.y = element_text(margin = margin(t = 0, r = 40, b = 0, l = 0)))



# Plots and Analyses of Predictor and Control variables ----

# Overall Correlation ----

ggpairs(controls %>% filter(flag.lottery==0) %>% droplevels(),
        columns= c("mars.mc","pre_err.mc","post_delta_ref.mc"),
        columnLabels = c("MaRs", "Comprehension", "Strategies"),
        lower = list(continuous = my_fn),
        diag = list(continuous =  wrap("densityDiag",colour="#1cade4",fill="#1cade4",alpha=0.2)),
        upper = list(continuous = wrap("cor", method = "spearman",colour="#1cade4",size = 6)))+
  theme_tufte(base_family = "sans",base_size=22)


## MaRs ----

### Plot

ggplot(controls, aes(x = age.cat, y = mean_mars, fill = age.cat, color=age.cat,
                           shape=age.cat)) +
  geom_rain(rain.side = "l",
            violin.args = list(alpha=0.4,linewidth=1,show.legend = FALSE),
            boxplot.args =list(alpha= 0.5, colour="black", outlier.shape = NA, show.legend = FALSE),
            boxplot.args.pos = list(
              position = ggpp::position_dodgenudge(width = 0.2), width = 0.16),
            point.args.pos= list(position = position_jitter(width = 0.08),show.legend = FALSE),
            point.args = list(alpha=0,aes(group=age.cat)), show.legend = FALSE)+
  scale_y_continuous(name= NULL)+
  scale_x_discrete(name= NULL)+
  geom_point(alpha=0.15,position = position_jitterdodge(jitter.width = 0.1,dodge.width = 0.2), show.legend = FALSE)+
  theme_tufte(base_family = "sans",base_size=22)+
  theme(axis.text = element_text(size = 20, color = "black"))

# ggplot(controls %>% filter(flag.lottery==0),
#        aes(age.cat, mars.mc,
#            fill = age.cat,
#            col= age.cat)) +     
#   geom_bar(position = 'dodge', color="black", stat='summary', fun="mean",size=0.8,show.legend = FALSE)+
#   scale_y_continuous(name= NULL)+
#   scale_x_discrete(name= NULL)+
#   stat_summary(fun.data = mean_cl_boot, geom = "errorbar",color="black", position=position_dodge(.5), width=0.3,size=0.8)+
#   theme_tufte(base_family = "sans",base_size=22, ticks = TRUE)+
#   theme(axis.text = element_text(size = 20, color = "black"))

### Analyses 

lm.mars <- lm(mean_mars ~ age.cat, 
              contrast= list(age.cat='contr.sum'),
              controls)

summary(lm.mars)

Anova(lm.mars, type=3)

emm.mars = emmeans(lm.mars, pairwise~age.cat, adjust="Bonf", infer=c(T,T))

summary(emm.mars)



## Flagged Lottery Differences ----

### Plot

flags.test <- controls %>% ungroup() %>% select(sub,age.cat,flag.lottery) %>% 
  mutate(flag.lottery = as.numeric(as.character(flag.lottery)))

ggplot(flags.test,
       aes(age.cat, flag.lottery,
           fill = age.cat,
           col= age.cat)) +     
  geom_bar(position = 'dodge', color="black", stat='summary', fun="mean",size=0.8,show.legend = FALSE)+
  scale_y_continuous(name= NULL)+
  scale_x_discrete(name= NULL)+
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",color="black", position=position_dodge(.5), width=0.3,size=0.8)+
  theme_tufte(base_family = "sans",base_size=22)+
  theme(axis.text = element_text(size = 20, color = "black"))


### Analysis

kruskal.test(flag.lottery ~ age.cat, data = flags.test)

pairwise.wilcox.test(flags.test$flag.lottery, flags.test$age.cat,
                     p.adjust.method = "bonferroni")

## Post Coord Questionnaire ----


### Plot


ggplot(controls,
       aes(age.cat, post_delta_ref,
           fill = age.cat,
           col= age.cat)) +     
  geom_bar(position = 'dodge', color="black", stat='summary', fun="mean",size=0.8,show.legend = FALSE)+
  scale_y_continuous(name= NULL)+
  scale_x_discrete(name= NULL)+
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",color="black", position=position_dodge(.5), width=0.3,size=0.8)+
  theme_tufte(base_family = "sans",base_size=22, ticks = TRUE)+
  geom_hline(yintercept = 0, linetype="dashed")+
  theme(axis.text = element_text(size = 20, color = "black"))

# ggplot(post.sliders %>% filter(ref=="Other") %>% droplevels(), aes(x = age.cat, y = Response, fill = age.cat, color=age.cat,
#                      shape=age.cat)) +
#   geom_rain(rain.side = "l",
#             violin.args = list(alpha=0.4,linewidth=1,show.legend = FALSE),
#             boxplot.args =list(alpha= 0.5, colour="black", outlier.shape = NA, show.legend = FALSE),
#             boxplot.args.pos = list(
#               position = ggpp::position_dodgenudge(width = 0.2), width = 0.16),
#             point.args.pos= list(position = position_jitter(width = 0.08),show.legend = FALSE),
#             point.args = list(alpha=0,aes(group=age.cat)), show.legend = FALSE)+
#   scale_y_continuous(name= NULL)+
#   scale_x_discrete(name= NULL)+
#   geom_point(alpha=0.15,position = position_jitterdodge(jitter.width = 0.1,dodge.width = 0.2), show.legend = FALSE)+
#   theme_tufte(base_family = "sans",base_size=22)+
#   theme(axis.text = element_text(size = 20, color = "black"))

### Analysis

lm.post.sliders <- lm(post_delta_ref ~ age.cat, 
                        contrast= list(age.cat='contr.sum'),
                        controls)

summary(lm.post.sliders)

Anova(lm.post.sliders, type=3)

emm.post.sliders = emmeans(lm.post.sliders, pairwise~age.cat, adjust="Bonf", infer=c(T,T))

summary(emm.post.sliders)

## Pre Coordination Questionnaire ----

ggplot(controls,
       aes(age.cat, pre_err,
           fill = age.cat,
           col= age.cat)) +     
  geom_bar(position = 'dodge', color="black", stat='summary', fun="mean",size=0.8,show.legend = FALSE)+
  scale_y_continuous(name= NULL)+
  scale_x_discrete(name= NULL)+
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",color="black", position=position_dodge(.5), width=0.3,size=0.8)+
  theme_tufte(base_family = "sans",base_size=22, ticks = TRUE)+
  theme(axis.text = element_text(size = 20, color = "black"))

### Analysis 

lm.pre <- lm(pre_err.mc ~ age.cat, 
             contrast= list(age.cat='contr.sum'),
             controls)

summary(lm.pre)

Anova(lm.pre, type=3)

emm.lm.pre = emmeans(lm.pre, pairwise~age.cat, adjust="Bonf", infer=c(T,T))

summary(emm.lm.pre)


# Predictor-Game plots and analyses for the 4 DVs ----

coord.aggr_rt <- left_join(coord.aggr_rt,controls)

coord.aggr_rt.l <- coord.aggr_rt %>% select(-median.rt) %>% rename(Risk=mean.up,
                                            RT=mean.rt,
                                            Switch=mean.switch,
                                            Payoff=mean.payoff)

coord.aggr_rt.l <- pivot_longer(data = coord.aggr_rt.l, Risk:Payoff,
                              names_to = "DV",
                              values_to="value")

coord.aggr_rt.l$DV <- fct_relevel(coord.aggr_rt.l$DV, "Risk", "Payoff", "Switch","RT")

## Post Coord Questionnaire ----

ggplot(coord.aggr_rt.l %>% 
         filter(flag.lottery==0,
                DV!="Payoff") %>%
         droplevels(), aes(x=post_delta_ref.mc, 
                           y=value,
                           linetype=game,
                           shape=game,
                           colour= game,
                           fill=game))+
  stat_summary(fun.y = "mean", geom="point", size=2.5, alpha=.5,show.legend = FALSE)+
  theme_tufte(base_family = "sans",base_size=22, ticks = FALSE)+
  stat_smooth(method = "lm", alpha=0.2, se=TRUE, linewidth=1.2, show.legend = F, linetype="solid")+
  geom_smooth(show.legend = F,linewidth=0.8, se = F,linetype="dashed")+
  #scale_colour_brewer(palette="dark")+
  scale_y_continuous(name= NULL)+
  scale_x_continuous(name= NULL)+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"))+
  # scale_linetype_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_shape_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_fill_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_colour_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  geom_vline(xintercept = 0,linetype="dashed")+
  facet_wrap(~DV,nrow = 2, scales = "free")

### Risk Analysis ----

lmer.up.post <- lmer(value ~ age.cat*game*post_delta_ref.mc+(1|sub),
     coord.aggr_rt.l %>% 
       filter(flag.lottery==0,
              DV=="Risk") %>% droplevels())

Anova(lmer.up.post, type=3)

emtr.up.post= emtrends(lmer.up.post, pairwise~game, var="post_delta_ref.mc")

summary(emtr.up.post,infer=c(T,T))

### Payoff Analysis ----

lmer.payoff.post <- lmer(value ~ age.cat*game*post_delta_ref.mc+(1|sub), 
                  contrast= list(game='contr.sum'),
                  coord.aggr_rt.l %>% 
                    filter(flag.lottery==0,
                           DV=="Payoff"))

Anova(lmer.payoff.post, type=3)

emtr.payoff.post= emtrends(lmer.payoff.post, pairwise~ game, var = "post_delta_ref.mc")

summary(emtr.payoff.post,infer=c(T,T))

### Switch Analysis ----

lmer.switch.post <- lmer(value ~ age.cat*game*post_delta_ref.mc+(1|sub), 
                         contrast= list(game='contr.sum'),
                         coord.aggr_rt.l %>% 
                           filter(flag.lottery==0,
                                  DV=="Switch"))

Anova(lmer.switch.post, type=3)

emtr.switch.post= emtrends(lmer.switch.post, pairwise~game,var = 'post_delta_ref.mc')

summary(emtr.switch.post,infer=c(T,T))

### RT Analysis ----

lmer.rt.post <- lmer(value ~ age.cat*game*post_delta_ref.mc+(1|sub), 
                         contrast= list(game='contr.sum'),
                         coord.aggr_rt.l %>% 
                           filter(flag.lottery==0,
                                  DV=="RT"))

Anova(lmer.rt.post, type=3)

emtr.rt.post= emtrends(lmer.rt.post, pairwise~game,var = 'post_delta_ref.mc')

summary(emtr.rt.post,infer=c(T,T))

## Pre Coord Questionnaire ----

ggplot(coord.aggr_rt.l %>% 
         filter(flag.lottery==0 &
                  DV=="Risk") %>%
         droplevels(), aes(x=pre_err.mc, 
                           y=value,
                           linetype=game,
                           shape=game,
                           colour= game,
                           fill=game))+
  stat_summary(fun.y = "mean", geom="point", size=2.5, alpha=.5,show.legend = FALSE)+
  theme_tufte(base_family = "sans",base_size=22, ticks = FALSE)+
  stat_smooth(method = "lm", alpha=0.2, se=TRUE, linewidth=1.2, show.legend = F, linetype="solid")+
  geom_smooth(show.legend = F,linewidth=0.8, se = F,linetype="dashed")+
  #scale_colour_brewer(palette="dark")+
  scale_y_continuous(name= NULL)+
  scale_x_continuous(name= NULL)+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"))+
  # scale_linetype_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_shape_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_fill_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_colour_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  facet_wrap(~age.cat, scales = "free")


### Risk Analysis ----

lmer.up.pre <- lmer(value ~ age.cat*game+game*(pre_err.mc+post_delta_ref+mars.mc)+(1|sub), 
                     contrast= list(game='contr.sum'),
                     coord.aggr_rt.l %>% 
                       filter(flag.lottery==0,
                              DV=="Risk"))

Anova(lmer.up.pre, type=3)

emtr.up.pre= emtrends(lmer.up.pre, pairwise~game, var = c("pre_err"))

summary(emtr.up.pre, infer=c(T,T))

### Payoff Analysis ----

lmer.payoff.pre <- lmer(value ~ game*age.cat*pre_err.mc+(1|sub), 
                         contrast= list(game='contr.sum'),
                         coord.aggr_rt.l %>% 
                           filter(flag.lottery==0,
                                  DV=="Payoff"))

Anova(lmer.payoff.pre, type=3)

emtr.payoff.pre= emtrends(lmer.payoff.pre, pairwise~ game, var = "pre_err")

summary(emtr.payoff.pre,infer=c(T,T))

### switch Analysis ----

lmer.switch.pre <- lmer(value ~ game*age.cat*pre_err.mc+(1|sub), 
                         contrast= list(game='contr.sum'),
                         coord.aggr_rt.l %>% 
                           filter(flag.lottery==0,
                                  DV=="Switch"))

Anova(lmer.switch.pre, type=3)

emtr.switch.pre= emtrends(lmer.switch.pre, pairwise~game,var = 'pre_err')

summary(emtr.switch.pre,infer=c(T,T))

### RT Analysis ----

lmer.rt.pre <- lmer(value ~ game*age.cat*pre_err.mc+(1|sub), 
                     contrast= list(game='contr.sum'),
                     coord.aggr_rt.l %>% 
                       filter(flag.lottery==0,
                              DV=="RT"))

Anova(lmer.rt.pre, type=3)

emtr.rt.pre= emtrends(lmer.rt.pre, pairwise~game,var = 'pre_err')

summary(emtr.rt.pre,infer=c(T,T))

## Mars ----

ggplot(coord.aggr_rt.l %>% 
         filter(flag.lottery==0) %>%
         droplevels(), aes(x=mars.mc, 
                           y=value,
                           linetype=game,
                           shape=game,
                           colour= game,
                           fill=game))+
  stat_summary(fun.y = "mean", geom="point", size=2.5, alpha=.5,show.legend = FALSE)+
  theme_tufte(base_family = "sans",base_size=22, ticks = FALSE)+
  geom_smooth(show.legend = FALSE)+
  #scale_colour_brewer(palette="dark")+
  scale_y_continuous(name= NULL)+
  scale_x_continuous(name= NULL)+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"))+
  # scale_linetype_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_shape_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_fill_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_colour_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  facet_wrap(~DV, scales = "free")


### Risk Analysis ----

lmer.up.mars <- lmer(value ~ game*age.cat*mars.mc+(1|sub), 
                    contrast= list(game='contr.sum'),
                    coord.aggr_rt.l %>% 
                      filter(flag.lottery==0,
                             DV=="Risk"))

Anova(lmer.up.mars, type=3)

emtr.up.mars= emtrends(lmer.up.mars,  pairwise~game, var="mars.mc")

summary(emtr.up.mars,infer=c(T,T))


### Payoff Analysis ----

lmer.payoff.mars <- lmer(value ~ game*age.cat*mars.mc+(1|sub), 
                        contrast= list(game='contr.sum'),
                        coord.aggr_rt.l %>% 
                          filter(flag.lottery==0,
                                 DV=="Payoff"))

Anova(lmer.payoff.mars, type=3)

emtr.payoff.mars= emtrends(lmer.payoff.mars, pairwise~ game, var = "mars.mc")

summary(emtr.payoff.mars,infer=c(T,T))

### Switch Analysis ----

lmer.switch.mars <- lmer(value ~ game*age.cat*mars.mc+(1|sub), 
                        contrast= list(game='contr.sum'),
                        coord.aggr_rt.l %>% 
                          filter(flag.lottery==0,
                                 DV=="Switch"))

Anova(lmer.switch.mars, type=3)

emtr.switch.mars= emtrends(lmer.switch.mars, pairwise~ game, var = "mars.mc")

summary(emtr.switch.mars,infer=c(T,T))

### RT Analysis ----

lmer.rt.mars <- lmer(value ~ age.cat*game*mars.mc+(1|sub), 
                    contrast= list(game='contr.sum'),
                    coord.aggr_rt.l %>% 
                      filter(flag.lottery==0,
                             DV=="RT"))

Anova(lmer.rt.mars, type=3)

emtr.rt.mars= emtrends(lmer.rt.mars, pairwise~game,var = 'mars.mc')

summary(emtr.rt.mars,infer=c(T,T))

# Predictor-Delta plots and analyses for the 4 DVs ----

## Post Coord Questionnaire ----

# delta.social_rt.l <- left_join(delta.social_rt %>% select(sub,age.cat,name,value),controls.l %>% ungroup() %>% select(sub,ctrl_name, ctrl_value), by="sub")

ggplot(delta.social_rt %>% 
         filter(flag.lottery==0,
                name!="RT2" &
                  name!="Payoff") %>%
         droplevels(), aes(x=post_delta_ref.mc, 
                           y=value,
                           fill=name,
                           color=name))+
  stat_summary(fun.y = "mean", geom="point", size=2.5, alpha=.5,show.legend = FALSE)+
  theme_tufte(base_family = "sans",base_size=22, ticks = FALSE)+
  geom_smooth(show.legend = F,linewidth=0.8, se = F,linetype="dashed")+
  stat_smooth(method = "lm", alpha=0.2, se=TRUE, linewidth=1.2, show.legend = F)+
  scale_fill_manual(values = c("Risk" = "#4cae43", "Payoff" = "#f0a22e","Switch" = "#e32d91","RT" = "#3494ba"))+
  scale_color_manual(values = c("Risk" = "#4cae43", "Payoff" = "#f0a22e","Switch" = "#e32d91","RT" = "#3494ba"))+
  #scale_colour_brewer(palette="dark")+
  scale_y_continuous(name= NULL)+
  scale_x_continuous(name= NULL)+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"))+
  # scale_linetype_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_shape_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_fill_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_colour_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  geom_vline(xintercept = 0,linetype="dashed")+
  geom_hline(yintercept = 0,linetype="dashed")+
  facet_wrap(~name, nrow=2,scales = "free")

### Risk----

lm.delta.post.up = lm(value ~ post_delta_ref.mc,
                     delta.social_rt %>% 
                       filter(flag.lottery==0,
                              name=="Risk") %>%
                       droplevels())

summary(lm.delta.post.up)
Anova(lm.delta.post.up, type = 3)

### Payoff----

lm.delta.post.pay = lm(value ~post_delta_ref.mc,
                      delta.social_rt %>% 
                        filter(flag.lottery==0,
                               name=="Payoff") %>%
                        droplevels())

summary(lm.delta.post.pay)
Anova(lm.delta.post.pay, type = 3)

### Switch----

lm.delta.post.switch = lm(value ~post_delta_ref.mc,
                         delta.social_rt %>% 
                           filter(flag.lottery==0,
                                  name=="Switch") %>%
                           droplevels())

summary(lm.delta.post.switch)
Anova(lm.delta.post.switch, type = 3)

### RT----

lm.delta.post.rt= lm(value ~post_delta_ref.mc,
                    delta.social_rt %>% 
                      filter(flag.lottery==0,
                             name=="RT") %>%
                      droplevels())

summary(lm.delta.post.rt)
Anova(lm.delta.post.rt, type = 3)

## Pre-Coord Questionnaire ----


ggplot(delta.social_rt %>% 
         filter(flag.lottery==0,
                name!="RT2" &
                  name!="Payoff",
                pre_err.mc>-44) %>%
         droplevels(), aes(x=pre_err.mc, 
                           y=value,
                           fill=name,
                           color=name))+
  stat_summary(fun.y = "mean", geom="point", size=2.5, alpha=.5,show.legend = FALSE)+
  theme_tufte(base_family = "sans",base_size=22, ticks = FALSE)+
  geom_smooth(show.legend = F,linewidth=0.8, se = F,linetype="dashed")+
  stat_smooth(method = "lm", alpha=0.2, se=TRUE, linewidth=1.2, show.legend = F)+
  scale_fill_manual(values = c("Risk" = "#4cae43", "Payoff" = "#f0a22e","Switch" = "#e32d91","RT" = "#3494ba"))+
  scale_color_manual(values = c("Risk" = "#4cae43", "Payoff" = "#f0a22e","Switch" = "#e32d91","RT" = "#3494ba"))+
  #scale_colour_brewer(palette="dark")+
  scale_y_continuous(name= NULL)+
  scale_x_continuous(name= NULL)+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"))+
  geom_hline(yintercept = 0,linetype="dashed")+
  # scale_linetype_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_shape_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_fill_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_colour_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  facet_wrap(~name,nrow = 2, scales = "free")

### Risk----

lm.delta.pre.up = lm(value ~ pre_err.mc,
                delta.social_rt %>% 
                  filter(flag.lottery==0,
                         name=="Risk",
                         pre_err.mc>-44) %>%
                  droplevels())

summary(lm.delta.pre.up)
Anova(lm.delta.pre.up, type = 3)

### Payoff----

lm.delta.pre.pay = lm(value ~pre_err.mc,
             delta.social_rt %>% 
               filter(flag.lottery==0,
                      name=="Payoff",
                      pre_err.mc>-44) %>%
               droplevels())

summary(lm.delta.pre.pay)
Anova(lm.delta.pre.pay, type = 3)

### Switch----

lm.delta.pre.switch = lm(value ~pre_err.mc,
                     delta.social_rt %>% 
                       filter(flag.lottery==0,
                              name=="Switch",
                              pre_err.mc>-44) %>%
                       droplevels())

summary(lm.delta.pre.switch)
Anova(lm.delta.pre.switch, type = 3)

### RT----

lm.delta.pre.rt= lm(value ~pre_err.mc,
                 delta.social_rt %>% 
                   filter(flag.lottery==0,
                          name=="RT",
                          pre_err.mc>-44) %>%
                   droplevels())

summary(lm.delta.pre.rt)
Anova(lm.delta.pre.rt, type = 3)

## MaRs ----


ggplot(delta.social_rt %>% 
         filter(flag.lottery==0,
                name!="RT2" &
                  name!="Payoff") %>%
         droplevels(), aes(x=mars.mc, 
                           y=value,
                           fill=name,
                           color=name))+
  stat_summary(fun.y = "mean", geom="point", size=2.5, alpha=.5,show.legend = FALSE)+
  theme_tufte(base_family = "sans",base_size=22, ticks = FALSE)+
  geom_smooth(show.legend = F,linewidth=0.8, se = F,linetype="dashed")+
  stat_smooth(method = "lm", alpha=0.2, se=TRUE, linewidth=1.2, show.legend = F)+
  scale_fill_manual(values = c("Risk" = "#4cae43", "Payoff" = "#f0a22e","Switch" = "#e32d91","RT" = "#3494ba"))+
  scale_color_manual(values = c("Risk" = "#4cae43", "Payoff" = "#f0a22e","Switch" = "#e32d91","RT" = "#3494ba"))+
  #scale_colour_brewer(palette="dark")+
  scale_y_continuous(name= NULL)+
  scale_x_continuous(name= NULL)+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"))+
  geom_hline(yintercept = 0,linetype="dashed")+
  # scale_linetype_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_shape_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_fill_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_colour_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  facet_wrap(~name,nrow=2, scales = "free")

### Risk----

lm.delta.m.up = lm(value ~ mars.mc,
                delta.social_rt %>% 
                  filter(flag.lottery==0,
                         name=="Risk") %>%
                  droplevels())

summary(lm.delta.m.up)
Anova(lm.delta.m.up, type = 3)

### Payoff----

lm.delta.m.pay = lm(value ~mars.mc,
             delta.social_rt %>% 
               filter(flag.lottery==0,
                      name=="Payoff") %>%
               droplevels())

summary(lm.delta.m.pay)
Anova(lm.delta.m.pay, type = 3)

### Switch----

lm.delta.m.switch = lm(value ~mars.mc,
                     delta.social_rt %>% 
                       filter(flag.lottery==0,
                              name=="Switch") %>%
                       droplevels())

summary(lm.delta.m.switch)
Anova(lm.delta.m.switch, type = 3)

### RT----

lm.delta.m.rt= lm(value ~mars.mc,
                 delta.social_rt %>% 
                   filter(flag.lottery==0,
                          name=="RT") %>%
                   droplevels())

summary(lm.delta.m.rt)
Anova(lm.delta.m.rt, type = 3)



# MaRs Analysis game*age.cat ----

## Switch no adults2 ----

lmer.mars.switch <- lmer(value ~ game*age.cat*mars.mc+(1|sub), 
                       contrast= list(game='contr.sum', age.cat='contr.sum'),
                       coord.aggr_rt.l %>% filter(DV=="Switch",
                                                  flag.lottery==0) %>% droplevels())

Anova(lmer.mars.switch, type=3)


emtr.mars.switch= emtrends(lmer.mars.switch, ~game*age.cat,var = 'mars.mc')

summary(emtr.mars.switch,infer=c(T,T))

plot_model(lmer.mars.switch, type='eff',colors=c("red", "limegreen", "dodgerblue4","orange"), terms=c("mars.mc","game","age.cat"))+
  theme_pubclean()+
  theme(axis.text.x = element_text(face="bold", 
                                   size=18),
        axis.text.y = element_text(face="bold", 
                                   size=18))

## Switch with adults2 ----

lmer.mars.switch2 <- lmer(value ~ game*mars_age.cat*mars.gmc+(1|sub), 
                         contrast= list(game='contr.sum', mars_age.cat='contr.sum'),
                         coord.aggr_rt.l %>% filter(DV=="Switch",
                                                    flag.lottery==0) %>% droplevels())

Anova(lmer.mars.switch2, type=3)


emtr.mars.switch2= emtrends(lmer.mars.switch2, ~game*mars_age.cat,var = 'mars.gmc')

summary(emtr.mars.switch2,infer=c(T,T))

plot_model(lmer.mars.switch2, type='eff',colors=c("red", "limegreen", "dodgerblue4","orange"), terms=c("mars.gmc","game","mars_age.cat"))+
  theme_pubclean()+
  theme(axis.text.x = element_text(face="bold", 
                                   size=18),
        axis.text.y = element_text(face="bold", 
                                   size=18))

## RT ----


lmer.mars.rt<- lmer(value ~ game*age.cat*mars.mc+(1|sub), 
                         contrast= list(game='contr.sum', mars_age.cat='contr.sum'),
                         coord.aggr_rt.l %>% filter(DV=="RT",
                                                    flag.lottery==0) %>% droplevels())

Anova(lmer.mars.rt, type=3)


emtr.mars.rt= emtrends(lmer.mars.rt, ~game, var = 'mars.mc')

summary(emtr.mars.rt,infer=c(T,T))
levels(coord.aggr_rt.l$game)

plot_model(lmer.mars.rt, type='eff',colors=c("red", "limegreen", "dodgerblue4"), terms=c("mars.gmc","game"))+
  theme_pubclean()+
  theme(axis.text.x = element_text(face="bold", 
                                   size=18),
        axis.text.y = element_text(face="bold", 
                                   size=18))
## Payoff ----


lmer.mars.payoff <- lmer(value ~ game*mars_age.cat*mars.gmc+(1|sub), 
                    contrast= list(game='contr.sum', mars_age.cat='contr.sum'),
                    coord.aggr_rt.l %>% filter(DV=="Payoff",
                                               flag.lottery==0) %>% droplevels())

Anova(lmer.mars.payoff, type=3)


emtr.mars.payoff= emtrends(lmer.mars.payoff, ~mars_age.cat,var = 'mars.gmc')

summary(emtr.mars.payoff,infer=c(T,T))

plot_model(lmer.mars.payoff, type='eff',colors=c("red", "limegreen", "dodgerblue4","orange"), terms=c("mars.gmc","mars_age.cat"))+
  theme_pubclean()+
  theme(axis.text.x = element_text(face="bold", 
                                   size=18),
        axis.text.y = element_text(face="bold", 
                                   size=18))
# Demographics ----
controls %>%
  group_by(age.cat,Gender) %>%
  summarise(
    Count = n(),
    Mean_Age = mean(Age, na.rm = TRUE),
    SD_Age = sd(Age, na.rm = TRUE),
    min_age= min(Age,na.rm = TRUE),
    max_age= max(Age,na.rm = TRUE)
  )

sjt.xtab(controls$age.cat, controls$Age)
