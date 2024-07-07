#Preprocessing Script

load_pkg <- rlang::quos(here,
                        dplyr,
                        tidyr,
                        tidyverse,
                        ggplot2,
                        ggthemes,
                        wesanderson,
                        lme4,
                        janitor,
                        statxp,
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

#Adolescents ----

ado <- read.csv(file.path(here("Unprocessed data/coordination/adolescenti/data_exp_103231-v1_task-dm9k.csv")),
                  header=TRUE, sep=",", dec=",")
names(ado)[56] <- "trial.number"
names(ado)[57] <- "left.column"
names(ado)[58] <- "right.column"

ado = ado%>% dplyr::mutate_if(is.character, as.factor)



# Choose cols of interest
ado.trials = ado[,c('Participant.Private.ID', 'Display', 'Screen','Reaction.Time', 
                     'Trial.Number', 'Object.Name', 'trial.number', 'left.column','right.column')]
ado.trials <- ado.trials %>% rename(sub=Participant.Private.ID,
                   game= Display,
                   rt= Reaction.Time,
                   sp=trial.number,
                   trial=Trial.Number)

ado.trials$trial= ado.trials$trial- 1

# Removing lines that do NOT have Screen=="trial"
ado.trials = droplevels(ado.trials[ado.trials$Screen=="trial",])
ado.trials=droplevels(ado.trials[ado.trials$sub!="7310608",])

# Convert RTs from factor to numeric: 
ado.trials$rt=as.numeric(as.character(ado.trials$rt))
ado.trials$rt.out=0
ado.trials$rt.out[ado.trials$rt>(mean(ado.trials$rt)+2.5*sd(ado.trials$rt)) |
                   ado.trials$rt<(mean(ado.trials$rt)-2.5*sd(ado.trials$rt))]=1


#adding column answer, indicating the type of answer participants choose: SP (sure pay) or UP (unsure pay)
ado.trials$left.response="SP"
ado.trials$left.response[ado.trials$left.column == '15soloseacaso.png'] <- "UP"
ado.trials$left.response[ado.trials$left.column == '15soloseinsieme.png'] <- "UP"
ado.trials$left.response[ado.trials$left.column == '15solosesoli.png'] <- "UP"


ado.trials$right.response="SP"
ado.trials$right.response[ado.trials$right.column == '15soloseacaso.png'] <- "UP"
ado.trials$right.response[ado.trials$right.column == '15soloseinsieme.png'] <- "UP"
ado.trials$right.response[ado.trials$right.column == '15solosesoli.png'] <- "UP"

ado.trials$answer=ado.trials$left.response
ado.trials$answer[ado.trials$Object.Name == 'right'] <- ado.trials$right.response[ado.trials$Object.Name == 'right']

summary(ado.trials)

#adding column "response" indicating 0 = SP(sure pay); 1= UP (unsure pay)
ado.trials$response=0
ado.trials$response[ado.trials$answer=="UP"]=1
summary(ado.trials)
str(ado.trials)

## Adding Switching ----

for (i in c(1:nrow(ado.trials))) {
  if (ado.trials$sp[i] != 1) {
    # ifelse(coord.trials$response[i] == coord.trials$response[coord.trials$sub==coord.trials$sub[i] & coord.trials$Display==coord.trials$Display[i] & coord.trials$trial.number == (coord.trials$trial.number[i]-1)],
    #        coord.trials$switch[i] = 0,coord.trials$switch[i] = 1),
    ado.trials$switch[i] = ifelse(ado.trials$response[i] == ado.trials$response[ado.trials$sub ==
                                                                            ado.trials$sub[i] &
                                                                            ado.trials$game == ado.trials$game[i] &
                                                                            ado.trials$sp == (ado.trials$sp[i] - 1)], 0, 1)
  }
}

ado.trials$switch[ado.trials$sp==1]=NA

## Adding Payoff ----

for (i in c(1:nrow(ado.trials))){
  meanUp.coord.others0 = mean(ado.trials$response[ado.trials$game=="coordination" & ado.trials$sp == ado.trials$sp[i] & ado.trials$sub != ado.trials$sub[i]])
  meanUp.comp.others0 = mean(ado.trials$response[ado.trials$game=="competition" & ado.trials$sp == ado.trials$sp[i] & ado.trials$sub != ado.trials$sub[i]])
  if (ado.trials$response[i] == 0){ado.trials$payoff[i]=ado.trials$sp[i]} else {
    if (ado.trials$game[i]=="lottery"){ado.trials$payoff[i]=7.5}
    if (ado.trials$game[i]=="coordination"){ado.trials$payoff[i]=15*meanUp.coord.others0}
    if (ado.trials$game[i]=="competition"){ado.trials$payoff[i]=15* (1-meanUp.comp.others0)}
  }
}


write.csv(ado.trials, file.path(here("Processed data/coordination/ado.trials.csv")))

#trial.number indicates the SP value for each trial 

ado.aggr = ado.trials %>%
  group_by(sub, game) %>%
  summarise(sum = sum(response),
            mean.up = mean(response),
            mean.rt = mean(rt),
            median.rt = median(rt), 
            mean.switch= mean(switch[sp!=1]),
            mean.payoff= mean(payoff),
            prop = sum(response)/15,
            count.trial = length(response),
            flag.trial = if(length(response)==15){0}else{1},
            flag.variance = if(var(response)==0){1}else{0}) 

summary(ado.aggr)
str(ado.aggr)
table(ado.aggr$flag.variance) #27 Display-cases (out of 567) in which the variance is equal to 0 (i.e., 567 / 3 conditions = 189 participants)

write.csv(ado.aggr, file.path(here("Processed data/coordination/ado.aggr.csv")))

#Adults 1 ----

adu1 <- read.csv(file.path(here("Unprocessed data/coordination/adulti/COORD-data_exp_130562-v2_task-dbrt_ADULTS.csv")),
                header=TRUE, sep=",", dec=",")

names(adu1)[55] <- "trial.number"
names(adu1)[56] <- "left.column"
names(adu1)[57] <- "right.column"


# Choose cols of interest
adu1.trials = adu1[,c('Participant.Private.ID', 'Display', 'Screen','Reaction.Time', 
                 'Trial.Number', 'Object.Name', 'trial.number', 'left.column','right.column')]

adu1.trials = adu1.trials%>% dplyr::mutate_if(is.character, as.factor)

adu1.trials <- adu1.trials %>% rename(sub=Participant.Private.ID,
                              game= Display,
                              rt= Reaction.Time,
                              sp=trial.number,
                              trial=Trial.Number)

# Removing lines that do NOT have Screen=="trial"
adu1.trials = droplevels(adu1.trials[adu1.trials$Screen=="trial",])

# Convert RTs from factor to numeric: 
adu1.trials$rt=as.numeric(as.character(adu1.trials$rt))
adu1.trials$rt.out=0
adu1.trials$rt.out[adu1.trials$rt>(mean(adu1.trials$rt)+2.5*sd(adu1.trials$rt)) |
                 adu1.trials$rt<(mean(adu1.trials$rt)-2.5*sd(adu1.trials$rt))]=1


#adding column answer, indicating the type of answer participants choose: SP (sure pay) or UP (unsure pay)
adu1.trials$left.response="SP"
adu1.trials$left.response[adu1.trials$left.column == '15soloseacaso.png'] <- "UP"
adu1.trials$left.response[adu1.trials$left.column == '15soloseinsieme.png'] <- "UP"
adu1.trials$left.response[adu1.trials$left.column == '15solosesoli.png'] <- "UP"


adu1.trials$right.response="SP"
adu1.trials$right.response[adu1.trials$right.column == '15soloseacaso.png'] <- "UP"
adu1.trials$right.response[adu1.trials$right.column == '15soloseinsieme.png'] <- "UP"
adu1.trials$right.response[adu1.trials$right.column == '15solosesoli.png'] <- "UP"

adu1.trials$answer=adu1.trials$left.response
adu1.trials$answer[adu1.trials$Object.Name == 'right'] <- adu1.trials$right.response[adu1.trials$Object.Name == 'right']

summary(adu1.trials)

#adding column "response" indicating 0 = SP(sure pay); 1= UP (unsure pay)
adu1.trials$response=0
adu1.trials$response[adu1.trials$answer=="UP"]=1
summary(adu1.trials)
str(adu1.trials)

##Dupes Removal ----

#The two flagged subjects

adu1.trials %>% group_by(sub,game,sp) %>% count() %>% filter(n>1) #Two participants with more than one sp for each game
xtabs(~sp+game, adu1.trials[adu1.trials$sub==8624043,]) #Same choice repeated in coord 6 times
xtabs(~sp+game, adu1.trials[adu1.trials$sub==8624066,]) #Same choice repeated in comp 10 times

adu1.trials <- adu1.trials %>% distinct(sp, game, sub, .keep_all = TRUE)

#16 rows were removed (32 duplicates/2), as a consequence means have slightly changed for adults but not for adolescents (as intended)
#Sub length is correct.
summary(adu1.trials)

## Adding Switching ----

for (i in c(1:nrow(adu1.trials))) {
  if (adu1.trials$sp[i] != 1) {
    # ifelse(coord.trials$response[i] == coord.trials$response[coord.trials$sub==coord.trials$sub[i] & coord.trials$Display==coord.trials$Display[i] & coord.trials$trial.number == (coord.trials$trial.number[i]-1)],
    #        coord.trials$switch[i] = 0,coord.trials$switch[i] = 1),
    adu1.trials$switch[i] = ifelse(adu1.trials$response[i] == adu1.trials$response[adu1.trials$sub ==
                                                                            adu1.trials$sub[i] &
                                                                            adu1.trials$game == adu1.trials$game[i] &
                                                                            adu1.trials$sp == (adu1.trials$sp[i] - 1)], 0, 1)
  }
}

adu1.trials$switch[adu1.trials$sp==1]=NA

## Adding Payoff ----

for (i in c(1:nrow(adu1.trials))){
  meanUp.coord.others1 = mean(adu1.trials$response[adu1.trials$game=="coordination" & adu1.trials$sp == adu1.trials$sp[i] & adu1.trials$sub != adu1.trials$sub[i]])
  meanUp.comp.others1 = mean(adu1.trials$response[adu1.trials$game=="competition" & adu1.trials$sp == adu1.trials$sp[i] & adu1.trials$sub != adu1.trials$sub[i]])
  if (adu1.trials$response[i] == 0){adu1.trials$payoff[i]=adu1.trials$sp[i]} else {
    if (adu1.trials$game[i]=="lottery"){adu1.trials$payoff[i]=7.5}
    if (adu1.trials$game[i]=="coordination"){adu1.trials$payoff[i]=15*meanUp.coord.others1}
    if (adu1.trials$game[i]=="competition"){adu1.trials$payoff[i]=15* (1-meanUp.comp.others1)}
  }
}

write.csv(adu1.trials, file.path(here("Processed data/coordination/adu1.trials.csv")))
#trial.number indicates the SP value for each trial 

adu1.aggr = adu1.trials %>%
  group_by(sub, game) %>%
  summarise(sum = sum(response),
            mean.up = mean(response),
            mean.rt = mean(rt),
            median.rt = median(rt), 
            mean.switch= mean(switch[sp!=1]),
            mean.payoff= mean(payoff),
            prop = sum(response)/15,
            count.trial = length(response),
            flag.trial = if(length(response)==15){0}else{1},
            flag.variance = if(var(response)==0){1}else{0}) 

summary(adu1.aggr)
str(adu1.aggr)
table(adu1.aggr$flag.variance)


write.csv(adu1.aggr, file.path(here("Processed data/coordination/adu1.aggr.csv")))

#Adults 2 ----

adu2.1 <- read.csv(file.path(here("Unprocessed data/coordination/adulti/COORD_data_exp_152679-v3_task-7zql.csv")),
                   header=TRUE, sep=",", dec=",")
adu2.2 <- read.csv(file.path(here("Unprocessed data/coordination/adulti/COORD_data_exp_152679-v5_task-7zql.csv")),
                   header=TRUE, sep=",", dec=",")

adu2<- rbind(adu2.1,adu2.2)

names(adu2)[56] <- "trial.number"
names(adu2)[57] <- "left.column"
names(adu2)[58] <- "right.column"

# Choose cols of interest

adu2.trials= adu2[,c('Participant.Private.ID', 'Display', 'Screen','Reaction.Time', 
              'Trial.Number', 'Object.Name', 'trial.number', 'left.column','right.column')]

adu2.trials = adu2.trials%>% dplyr::mutate_if(is.character, as.factor)

adu2.trials <- adu2.trials %>% rename(sub=Participant.Private.ID,
                              game= Display,
                              rt= Reaction.Time,
                              sp=trial.number,
                              trial=Trial.Number)

# Removing lines that do NOT have Screen=="trial"
adu2.trials = droplevels(adu2.trials[adu2.trials$Screen=="trial",])

# Convert RTs from factor to numeric: 
adu2.trials$rt=as.numeric(as.character(adu2.trials$rt))
adu2.trials$rt.out=0
adu2.trials$rt.out[adu2.trials$rt>(mean(adu2.trials$rt)+2.5*sd(adu2.trials$rt)) |
                 adu2.trials$rt<(mean(adu2.trials$rt)-2.5*sd(adu2.trials$rt))]=1


#adding column answer, indicating the type of answer participants choose: SP (sure pay) or UP (unsure pay)
adu2.trials$left.response="SP"
adu2.trials$left.response[adu2.trials$left.column == '15soloseacaso.png'] <- "UP"
adu2.trials$left.response[adu2.trials$left.column == '15soloseinsieme.png'] <- "UP"
adu2.trials$left.response[adu2.trials$left.column == '15solosesoli.png'] <- "UP"


adu2.trials$right.response="SP"
adu2.trials$right.response[adu2.trials$right.column == '15soloseacaso.png'] <- "UP"
adu2.trials$right.response[adu2.trials$right.column == '15soloseinsieme.png'] <- "UP"
adu2.trials$right.response[adu2.trials$right.column == '15solosesoli.png'] <- "UP"

adu2.trials$answer=adu2.trials$left.response
adu2.trials$answer[adu2.trials$Object.Name == 'right'] <- adu2.trials$right.response[adu2.trials$Object.Name == 'right']

summary(adu2.trials)

#adding column "response" indicating 0 = SP(sure pay); 1= UP (unsure pay)
adu2.trials$response=0
adu2.trials$response[adu2.trials$answer=="UP"]=1
summary(adu2.trials)
str(adu2.trials)

##Dupes Removal ----

#The two flagged subjects

adu2.trials %>% group_by(sub,game,sp) %>% count() %>% filter(n>1) #One participant with more than one sp for each game
xtabs(~sp+game, adu2.trials[adu2.trials$sub==9818161,]) #Same choice repeated in coord 6 times

adu2.trials <- adu2.trials %>% distinct(sp, game, sub, .keep_all = TRUE)

#16 rows were removed (32 duplicates/2), as a consequence means have slightly changed for adults but not for adolescents (as intended)
#Sub length is correct.
summary(adu1.trials)

## Adding Switching ----

for (i in c(1:nrow(adu2.trials))) {
  if (adu2.trials$sp[i] != 1) {
    # ifelse(coord.trials$response[i] == coord.trials$response[coord.trials$sub==coord.trials$sub[i] & coord.trials$Display==coord.trials$Display[i] & coord.trials$trial.number == (coord.trials$trial.number[i]-1)],
    #        coord.trials$switch[i] = 0,coord.trials$switch[i] = 1),
    adu2.trials$switch[i] = ifelse(adu2.trials$response[i] == adu2.trials$response[adu2.trials$sub ==
                                                                            adu2.trials$sub[i] &
                                                                            adu2.trials$game == adu2.trials$game[i] &
                                                                            adu2.trials$sp == (adu2.trials$sp[i] - 1)], 0, 1)
  }
}

adu2.trials$switch[adu2.trials$sp==1]=NA

## Adding Payoff ----

for (i in c(1:nrow(adu2.trials))){
  meanUp.coord.others2 = mean(adu2.trials$response[adu2.trials$game=="coordination" & adu2.trials$sp == adu2.trials$sp[i] & adu2.trials$sub != adu2.trials$sub[i]])
  meanUp.comp.others2 = mean(adu2.trials$response[adu2.trials$game=="competition" & adu2.trials$sp == adu2.trials$sp[i] & adu2.trials$sub != adu2.trials$sub[i]])
  if (adu2.trials$response[i] == 0){adu2.trials$payoff[i]=adu2.trials$sp[i]} else {
    if (adu2.trials$game[i]=="lottery"){adu2.trials$payoff[i]=7.5}
    if (adu2.trials$game[i]=="coordination"){adu2.trials$payoff[i]=15*meanUp.coord.others2}
    if (adu2.trials$game[i]=="competition"){adu2.trials$payoff[i]=15* (1-meanUp.comp.others2)}
  }
}

write.csv(adu2.trials, file.path(here("Processed data/coordination/adu2.trials.csv")))
#trial.number indicates the SP value for each trial 

adu2.aggr = adu2.trials %>%
  group_by(sub, game) %>%
  summarise(sum = sum(response),
            mean.up = mean(response),
            mean.rt = mean(rt),
            median.rt = median(rt), 
            mean.switch= mean(switch[sp!=1]),
            mean.payoff= mean(payoff),
            prop = sum(response)/15,
            count.trial = length(response),
            flag.trial = if(length(response)==15){0}else{1},
            flag.variance = if(var(response)==0){1}else{0}) 

summary(adu2.aggr)
str(adu2.aggr)
table(adu2.aggr$flag.variance)


write.csv(adu2.aggr, file.path(here("Processed data/coordination/adu2.aggr.csv")))

# Children----

kids <- read.csv(file.path(here("Unprocessed data/coordination/bambini/COORD_data_exp_175940-v1_task-dm9k.csv")),
                 header=TRUE, sep=",", dec=",")

names(kids)[56] <- "trial.number"
names(kids)[57] <- "left.column"
names(kids)[58] <- "right.column"

# Choose cols of interest

kids.trials= kids[,c('Participant.Private.ID', 'Display', 'Screen','Reaction.Time', 
                     'Trial.Number', 'Object.Name', 'trial.number', 'left.column','right.column')]

kids.trials = kids.trials%>% dplyr::mutate_if(is.character, as.factor)

kids.trials <- kids.trials %>% rename(sub=Participant.Private.ID,
                                      game= Display,
                                      rt= Reaction.Time,
                                      sp=trial.number,
                                      trial=Trial.Number)

# Removing lines that do NOT have Screen=="trial"
kids.trials = droplevels(kids.trials[kids.trials$Screen=="trial",])

# Convert RTs from factor to numeric: 
kids.trials$rt=as.numeric(as.character(kids.trials$rt))
kids.trials$rt.out=0
kids.trials$rt.out[kids.trials$rt>(mean(kids.trials$rt)+2.5*sd(kids.trials$rt)) |
                     kids.trials$rt<(mean(kids.trials$rt)-2.5*sd(kids.trials$rt))]=1


#adding column answer, indicating the type of answer participants choose: SP (sure pay) or UP (unsure pay)
kids.trials$left.response="SP"
kids.trials$left.response[kids.trials$left.column == '15soloseacaso.png'] <- "UP"
kids.trials$left.response[kids.trials$left.column == '15soloseinsieme.png'] <- "UP"
kids.trials$left.response[kids.trials$left.column == '15solosesoli.png'] <- "UP"


kids.trials$right.response="SP"
kids.trials$right.response[kids.trials$right.column == '15soloseacaso.png'] <- "UP"
kids.trials$right.response[kids.trials$right.column == '15soloseinsieme.png'] <- "UP"
kids.trials$right.response[kids.trials$right.column == '15solosesoli.png'] <- "UP"

kids.trials$answer=kids.trials$left.response
kids.trials$answer[kids.trials$Object.Name == 'right'] <- kids.trials$right.response[kids.trials$Object.Name == 'right']

summary(kids.trials)

#adding column "response" indicating 0 = SP(sure pay); 1= UP (unsure pay)
kids.trials$response=0
kids.trials$response[kids.trials$answer=="UP"]=1
summary(kids.trials)
str(kids.trials)

## Adding Switching ----

for (i in c(1:nrow(kids.trials))) {
  if (kids.trials$sp[i] != 1) {
    # ifelse(coord.trials$response[i] == coord.trials$response[coord.trials$sub==coord.trials$sub[i] & coord.trials$Display==coord.trials$Display[i] & coord.trials$trial.number == (coord.trials$trial.number[i]-1)],
    #        coord.trials$switch[i] = 0,coord.trials$switch[i] = 1),
    kids.trials$switch[i] = ifelse(kids.trials$response[i] == kids.trials$response[kids.trials$sub ==
                                                                                     kids.trials$sub[i] &
                                                                                     kids.trials$game == kids.trials$game[i] &
                                                                                     kids.trials$sp == (kids.trials$sp[i] - 1)], 0, 1)
  }
}

kids.trials$switch[kids.trials$sp==1]=NA

## Adding Payoff ----

for (i in c(1:nrow(kids.trials))){
  meanUp.coord.others3 = mean(kids.trials$response[kids.trials$game=="coordination" & kids.trials$sp == kids.trials$sp[i] & kids.trials$sub != kids.trials$sub[i]])
  meanUp.comp.others3 = mean(kids.trials$response[kids.trials$game=="competition" & kids.trials$sp == kids.trials$sp[i] & kids.trials$sub != kids.trials$sub[i]])
  if (kids.trials$response[i] == 0){kids.trials$payoff[i]=kids.trials$sp[i]} else {
    if (kids.trials$game[i]=="lottery"){kids.trials$payoff[i]=7.5}
    if (kids.trials$game[i]=="coordination"){kids.trials$payoff[i]=15*meanUp.coord.others3}
    if (kids.trials$game[i]=="competition"){kids.trials$payoff[i]=15* (1-meanUp.coord.others3)}
  }
}

write.csv(kids.trials, file.path(here("Processed data/coordination/kids.trials.csv")))
#trial.number indicates the SP value for each trial 

kids.aggr = kids.trials %>%
  group_by(sub, game) %>%
  summarise(sum = sum(response),
            mean.up = mean(response),
            mean.rt = mean(rt),
            median.rt = median(rt), 
            mean.switch= mean(switch[sp!=1]),
            mean.payoff= mean(payoff),
            prop = sum(response)/15,
            count.trial = length(response),
            flag.trial = if(length(response)==15){0}else{1},
            flag.variance = if(var(response)==0){1}else{0}) 

summary(kids.aggr)
str(kids.aggr)
table(kids.aggr$flag.variance)


write.csv(kids.aggr, file.path(here("Processed data/coordination/kids.aggr.csv")))

# Merging ----

coord.trials <- bind_rows(ado.trials, adu1.trials, adu2.trials, kids.trials)

# get_category <- function(sub) {
#   if (substr(sub, 1, 1) %in% c("7")) {
#     return("Adolescents")
#   } else if (substr(sub, 1, 1) %in% c("8", "9")) {
#     return("Adults")
#   } else if (substr(sub, 1, 2) == "10") {
#     return("Children")
#   }
# }

# # Function for getting category based on the initial sub number, 7 for adolescents, the rest for adults
# 
# coord.trials$age.cat <- sapply(coord.trials$sub, get_category) # applying the function for sel and then cond datasets
# 
 coord.aggr<- bind_rows(ado.aggr, adu1.aggr, adu2.aggr,kids.aggr)
# 
# coord.aggr$age.cat <- sapply(coord.aggr$sub, get_category)
# 
# length(unique(coord.trials$sub[coord.trials$age.cat=="Children"]))
# length(unique(adu1.trials$sub)) + length(unique(ado.trials$sub))

get_version <- function(sub) {
  if (substr(sub, 1, 1) %in% c("9")) {
    return("ver2")
  } else {
    return("ver1")
  }
}# For later purposes of MARS analysis, i distinguish between ver1 and ver2 of mars, with adults beginning with 9 as 2nd ver

coord.aggr$version <- sapply(coord.aggr$sub, get_version)
coord.trials$version <- sapply(coord.trials$sub, get_version)

coord.trials <- coord.trials %>% mutate_at('sub',factor)
coord.aggr <- coord.aggr %>% mutate_at('sub',factor)

# Data Cleaning ----

coord.trials_rt = coord.trials %>%
  filter(!(rt<250 |
             (rt>(mean(coord.trials$rt)+3*sd(coord.trials$rt)))))

coord.aggr_rt = coord.trials_rt %>%
  group_by(sub, game, version) %>%
  summarise(sum = sum(response),
            mean.up = mean(response),
            mean.rt = mean(rt),
            median.rt = median(rt), 
            mean.switch= mean(switch[sp!=1]),
            mean.payoff= mean(payoff),
            prop = sum(response)/15,
            count.trial = length(response),
            flag.trial = if(length(response)==15){0}else{1},
            flag.variance = if(var(response)==0){1}else{0}) 

#No switch flags, all flags concern lottery only

flags = coord.aggr %>% filter(game=="lottery") %>%
  mutate(flag.switch.0 = ifelse((mean.switch==0 & 
                                   mean.up==0 & 
                                   game=="lottery"),1,0),
         flag.switch.1 = ifelse((mean.switch==0 & 
                                   mean.up==1 &
                                   game=="lottery"),1,0),
         flag.ns= ifelse(mean.switch==0 &
                           game=="lottery",1,0)) %>%
  select(sub, flag.switch.0,flag.switch.1,flag.ns) %>%
  mutate_at(c('sub','flag.switch.0','flag.switch.1','flag.ns'),factor)

coord.trials <- left_join(coord.trials,flags)

# Lottery slopes

slopes.lott <- data.frame(slopes.lott = ranef(glmer(
  response ~ 1 + (scale(sp) | sub), family = 'binomial',
  coord.trials[coord.trials$game =="lottery" & coord.trials$flag.ns==0, ]))$sub)

slopes.lott <- slopes.lott %>% mutate(sub= as.factor(row.names(slopes.lott)),
                                        slope.lottery= slopes.lott.scale.sp.) %>% select(-c(1,2)) %>%
  mutate_if(is.character,as.factor)

rownames(slopes.lott) <- 1:nrow(slopes.lott)

flags <- left_join(flags,slopes.lott)

quantiles.slopes <- quantile(flags$slope.lottery, probs = seq(0.1, 1, by = .1), na.rm = T)

quantiles.slopes[[9]]

# -0.09804546 is the 9th quantile of slopes lott when excluding no switches (596 participants)

flags <- flags %>% 
  mutate(flag.slopes = ifelse(slope.lottery>quantiles.slopes[[9]],1,0)) %>%
  mutate(flag.lottery = ifelse(flag.slopes==1 | flag.ns==1,1,0)) %>%
  mutate (flag.slopes = ifelse(is.na(flag.slopes), 0, flag.slopes),
          flag.lottery = ifelse(is.na(flag.lottery), 0, flag.lottery)) %>%
  mutate(across(c(where(is.numeric), -slope.lottery), as.factor))

coord.trials <- left_join(coord.trials ,flags %>% select (sub,slope.lottery,flag.slopes,flag.lottery))

coord.trials_rt <- left_join(coord.trials_rt,flags %>% select (sub,slope.lottery,flag.slopes,flag.lottery))

coord.trials %>%
  filter(flag.ns==1) %>%
  summarise(count = n_distinct(sub)) #25 flagged participants for no switching

coord.trials %>%
  filter(flag.slopes==1) %>%
  summarise(count = n_distinct(sub)) #57 flagged participants in lottery slopes

coord.trials %>%
  filter(flag.lottery==1) %>%
  summarise(count = n_distinct(sub)) #82 flagged participants in lottery slopes + ns

#Checking the look of slopes 

ggplot(coord.trials %>% filter(flag.slopes==1,
                               game=="lottery") %>% droplevels(), aes(x = sp))+
  # geom_line(aes(y=response))+
  geom_smooth(aes(y=response), method='glm', method.args = list(family="binomial"), se =F)+
  geom_point(aes(y=response), alpha=0.5)+
  geom_line(aes(y=switch), lty=2)+
  facet_wrap(~sub)+theme_minimal()

coord.aggr <- left_join(coord.aggr,flags)
coord.aggr_rt <- left_join(coord.aggr_rt,flags)
setdiff(colnames(coord.aggr),colnames(coord.aggr_rt))


# Adding Demographics, to set the age groups ----


coord.trials <- left_join(coord.trials,dems)
coord.trials_rt <- left_join(coord.trials_rt,dems)
coord.aggr <- left_join(coord.aggr,dems)
coord.aggr_rt <- left_join(coord.aggr_rt,dems)


coord_list <- list(coord.trials= coord.trials , 
                   coord.trials_rt = coord.trials_rt, 
                   coord.aggr= coord.aggr,
                   coord.aggr_rt= coord.aggr_rt)

coord_names <- names(coord_list)


for (i in seq_along(coord_list)) {
  coord_list[[i]] <- coord_list[[i]] %>% 
    mutate(age.cat = case_when(
      (is.na(Age) & (substr(sub, 1, 1) == "1"))  | (Age >= 9 & Age <= 11) ~ "Children",
      Age >= 12 & Age <= 15 ~ "Adolescents",
      (is.na(Age) & (substr(sub, 1, 1) %in% c("8", "9"))) | Age>=18   ~ "Adults"
    ))
  
  # Assign each modified dataset to a separate object
  assign(coord_names[i], coord_list[[i]])
}


# Re-leveling everything ----

coord.trials$game<- fct_relevel(coord.trials$game, "coordination", "competition", "lottery")

coord.trials$age.cat<- fct_relevel(coord.trials$age.cat, "Children", "Adolescents", "Adults")

coord.trials_rt$game<- fct_relevel(coord.trials_rt$game, "coordination", "competition", "lottery")

coord.trials_rt$age.cat<- fct_relevel(coord.trials_rt$age.cat, "Children", "Adolescents", "Adults")

coord.aggr$game<- fct_relevel(coord.aggr$game, "coordination", "competition", "lottery")

coord.aggr$age.cat<- fct_relevel(coord.aggr$age.cat, "Children", "Adolescents", "Adults")

coord.aggr_rt$game<- fct_relevel(coord.aggr_rt$game, "coordination", "competition", "lottery")

coord.aggr_rt$age.cat<- fct_relevel(coord.aggr_rt$age.cat, "Children", "Adolescents", "Adults")


levels(coord.trials$age.cat)
levels(coord.trials_rt$age.cat)
levels(coord.aggr$age.cat)
levels(coord.aggr_rt$age.cat)
levels(coord.trials$game)
levels(coord.trials_rt$game)
levels(coord.aggr$game)
levels(coord.aggr_rt$game)

# Saving Relevant datasets ----

# convert_to_factors <- function(df, columns) {
#   for (col in columns) {
#     df[[col]] <- as.factor(df[[col]])
#   }
#   return(df)
# }

# List of dataframes



#columns_to_convert <- c("sub", "age.cat", "version","flag.ns","flag.slopes")

# Apply the function to each dataframe in the list

#coord_list <- lapply(coord_list, function(df) convert_to_factors(df, columns_to_convert))



# coord.trials <- coord_list[[1]]
# coord.trials_rt <- coord_list[[2]]
# coord.aggr <- coord_list[[3]]
# coord.aggr_rt <- coord_list[[4]]


for (df_name in names(coord_list)) {
  filename <- paste0(df_name, ".csv")
  filepath <- file.path(file.path(here("Processed data/coordination/")), filename)
  write.csv(coord_list[[df_name]], file = filepath, row.names = FALSE)
}

# Trial-level plots ----

## Risk ----

### By Age category ----
ggplot(coord.trials_rt %>% 
         filter(flag.lottery==0) %>%
         droplevels(), aes(x=sp, 
                                                          y=response,
                                                          linetype =game,
                                                          shape=game,
                                                          colour= game,
                                                          fill=game))+
  stat_summary(fun.y = "mean", geom="point", size=2.5, alpha=.5,show.legend = FALSE)+
  theme_tufte(base_family = "sans",base_size=22, ticks = FALSE)+
  geom_smooth(show.legend = FALSE)+
  #scale_colour_brewer(palette="dark")+
  scale_x_continuous(name= NULL, breaks=seq(1,15,2))+
   scale_y_continuous(name= NULL)+
    theme(axis.text.x = element_text(face="bold"),
          axis.text.y = element_text(face="bold"),
          strip.text.x = element_blank())+
  # scale_linetype_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_shape_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_fill_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_colour_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  geom_hline(yintercept=0.5, linetype="dashed", color = "black")+
  facet_wrap(~age.cat)

### By Game  ----
ggplot(coord.trials_rt %>% 
         filter(flag.lottery==0) %>%
         droplevels(), aes(x=sp, 
                         y=response,
                         linetype =age.cat,
                         shape=age.cat,
                         colour= age.cat,
                         fill=age.cat))+
  stat_summary(fun.y = "mean", geom="point", size=2.5, alpha=.5,show.legend = FALSE)+
  theme_tufte(base_family = "sans",base_size=22, ticks = FALSE)+
  geom_smooth(show.legend = FALSE)+
  #scale_colour_brewer(palette="dark")+
  scale_x_continuous(name= NULL, breaks=seq(1,15,2))+
  scale_y_continuous(name= NULL)+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"),
        strip.text.x = element_blank())+
  # scale_linetype_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_shape_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_fill_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_colour_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  geom_hline(yintercept=0.5, linetype="dashed", color = "black")+
  facet_wrap(~game)

## Payoff ----

### By Age category ----
ggplot(coord.trials_rt %>%
         filter(flag.lottery==0) %>%
         droplevels(), aes(x=sp, 
                         y=payoff,
                         linetype =game,
                         shape=game,
                         colour= game,
                         fill=game))+
  stat_summary(fun.y = "mean", geom="point", size=2.5, alpha=.5,show.legend = FALSE)+
  theme_tufte(base_family = "sans",base_size=22, ticks = FALSE)+
  geom_smooth(show.legend = FALSE)+
  #scale_colour_brewer(palette="dark")+
  scale_x_continuous(name= NULL, breaks=seq(1,15,2))+
  scale_y_continuous(name= NULL, breaks=seq(0,15,5))+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"),
        strip.text.x = element_blank())+
  # scale_linetype_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_shape_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_fill_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_colour_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  facet_wrap(~age.cat)

### By Game  ----
ggplot(coord.trials_rt %>%
         filter(flag.lottery==0) %>%
         droplevels(), aes(x=sp, 
                            y=payoff,
                            linetype =age.cat,
                            shape=age.cat,
                            colour= age.cat,
                            fill=age.cat))+
  stat_summary(fun.y = "mean", geom="point", size=2.5, alpha=.5,show.legend = FALSE)+
  theme_tufte(base_family = "sans",base_size=22, ticks = FALSE)+
  geom_smooth(show.legend = FALSE)+
  #scale_colour_brewer(palette="dark")+
  scale_x_continuous(name= NULL, breaks=seq(1,15,2))+
  scale_y_continuous(name= NULL, breaks=seq(0,15,5))+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"),
        strip.text.x = element_blank())+
  # scale_linetype_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_shape_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_fill_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_colour_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  facet_wrap(~game)

## Switch ----

### By Age category ----
ggplot(coord.trials_rt %>%
         filter(flag.lottery==0) %>%
         droplevels(), aes(x=sp, 
                         y=switch,
                         linetype =game,
                         shape=game,
                         colour= game,
                         fill=game))+
  stat_summary(fun.y = "mean", geom="point", size=2.5, alpha=.5,show.legend = FALSE)+
  theme_tufte(base_family = "sans",base_size=22, ticks = FALSE)+
  geom_smooth(show.legend = FALSE)+
  #scale_colour_brewer(palette="dark")+
  scale_x_continuous(name= NULL, breaks=seq(1,15,2))+
  scale_y_continuous(name= NULL)+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"),
        strip.text.x = element_blank())+
  # scale_linetype_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_shape_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_fill_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_colour_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  facet_wrap(~age.cat)

### By Game  ----
ggplot(coord.trials_rt %>%
         filter(flag.lottery==0) %>%
         droplevels(), aes(x=sp, 
                            y=switch,
                            linetype =age.cat,
                            shape=age.cat,
                            colour= age.cat,
                            fill=age.cat))+
  stat_summary(fun.y = "mean", geom="point", size=2.5, alpha=.5,show.legend = FALSE)+
  theme_tufte(base_family = "sans",base_size=22, ticks = FALSE)+
  geom_smooth(show.legend = FALSE)+
  #scale_colour_brewer(palette="dark")+
  scale_x_continuous(name= NULL, breaks=seq(1,15,2))+
  scale_y_continuous(name= NULL)+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"),
        strip.text.x = element_blank())+
  # scale_linetype_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_shape_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_fill_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_colour_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  facet_wrap(~game)

## RT ----

### By Age category ----
ggplot(coord.trials_rt %>%
         filter(flag.lottery==0) %>%
         droplevels(), aes(x=sp, 
                         y=rt,
                         linetype =game,
                         shape=game,
                         colour= game,
                         fill=game))+
  stat_summary(fun.y = "mean", geom="point", size=2.5, alpha=.5,show.legend = FALSE)+
  theme_tufte(base_family = "sans",base_size=22, ticks = FALSE)+
  geom_smooth(show.legend = FALSE)+
  #scale_colour_brewer(palette="dark")+
  scale_x_continuous(name= NULL, breaks=seq(1,15,2))+
  scale_y_continuous(name= NULL)+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"),
        strip.text.x = element_blank())+
  # scale_linetype_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_shape_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_fill_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_colour_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  facet_wrap(~age.cat)

### By Game  ----
ggplot(coord.trials_rt %>%
         filter(flag.lottery==0) %>%
         droplevels(), aes(x=sp, 
                         y=rt,
                         linetype =age.cat,
                         shape=age.cat,
                         colour= age.cat,
                         fill=age.cat))+
  stat_summary(fun.y = "mean", geom="point", size=2.5, alpha=.5,show.legend = FALSE)+
  theme_tufte(base_family = "sans",base_size=22, ticks = FALSE)+
  geom_smooth(show.legend = FALSE)+
  #scale_colour_brewer(palette="dark")+
  scale_x_continuous(name= NULL, breaks=seq(1,15,2))+
  scale_y_continuous(name= NULL)+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"),
        strip.text.x = element_blank())+
  # scale_linetype_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_shape_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_fill_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  # scale_colour_discrete(labels=c("Cooperation", "Competition", "Lottery"))+
  facet_wrap(~game)

# Trial-Level Analyses ----

## Adolescents ----

glmer.ado <- glmer(response ~ game+(game|sub), family="binomial",
                   # control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)),
                   data= coord.trials_rt %>% 
                     filter(flag.lottery==0,
                            age.cat="Adolescents") %>%
                     droplevels())

Anova(glmer.ado, type=3)

emm.ado = emmeans(glmer.ado, pairwise~game, adjust="Bonf")

summary(emm.ado, infer = c(T,T), type="response")
confint(emm.ado, level=0.95)

emmeans::eff_size(emm.ado, type= "Cohen-d", sigma = sigma(glmer.ado), edf = df.residual(glmer.ado)) # Effect sizes of those differences.

emm.ado.int= emmeans(glmer.ado, pairwise~game|sp, adjust="Bonf")

summary(emm.ado.int, infer = c(T,T))
plot(emm.ado)
summary()

## Adults ----

glmer.adu <- glmer(response ~ game+(game|sub), family="binomial",
                   # control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)),
                   data= coord.trials_rt %>% 
                     filter(flag.lottery==0,
                            age.cat="Adults") %>%
                     droplevels())

Anova(glmer.adu, type=3)

emm.adu = emmeans(glmer.adu, pairwise~game, adjust="Bonf")

summary(emm.adu, infer = c(T,T), type="response")
confint(emm.adu, level=0.95)

emmeans::eff_size(emm.adu, type= "Cohen-d", sigma = sigma(glmer.adu), edf = df.residual(glmer.adu)) # Effect sizes of those differences.

emm.adu.int= emmeans(glmer.adu, pairwise~game|sp, adjust="Bonf")

summary(emm.adu.int, infer = c(T,T))
plot(emm.adu)
summary()

## Children ----

glmer.kids <- glmer(response ~ game+(game|sub), family="binomial",
                   # control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)),
                   data= coord.trials_rt %>% 
                     filter(flag.lottery==0,
                            age.cat="Children") %>%
                     droplevels())

Anova(glmer.kids, type=3)

emm.kids = emmeans(glmer.kids, pairwise~game, adjust="Bonf")

summary(emm.kids, infer = c(T,T), type="response")
confint(emm.kids, level=0.95)

emmeans::eff_size(emm.kids, type= "Cohen-d", sigma = sigma(glmer.kids), edf = df.residual(glmer.kids)) # Effect sizes of those differences.

emm.kids.int= emmeans(glmer.kids, pairwise~game|sp, adjust="Bonf")

summary(emm.kids.int, infer = c(T,T))
plot(emm.kids)
summary()