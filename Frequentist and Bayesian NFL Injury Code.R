setwd("D:/Injury analysis/Player Injuries/")

require(tidyverse)
require(corrplot)
require(MuMIn)
require(R2jags)
require(mcmcplots)
require(lme4)
require(knitr)

# read in our dataset, we are going to work primarily with customized PFF IDs, so we can remve a bunch of unecessary columns

df <- read.csv("big.final.csv") %>% dplyr::select(-X, -name_abbr, - team_abbr, - first_name, -last_name, -ID) %>%
                                    dplyr::rename(ID = player.ID) %>% dplyr::select(ID, everything(.))

head(df)

# Build folling data set 
# make df with percent snaps by year, plus number of weeks on injured reserve in previous year, and number of weeks on injured reserve
# for current year. Include all other info, position, age, etc. 

# Build sub data sets to merge at the end for analysis 

# Yearly total snaps by player, foundation of dataset. First though, change all instances of redskins to WFT, because that creates 
# duplicate records for players on that team when the name changed. ALSO, Los angeles rams and St louis rams are both included in the OG data
# change all rams records to LA rams. Same thing with chargers 

df.1 <- df %>% dplyr::mutate(Team = ifelse(Team == "St. Louis Rams", "Los Angeles Rams", Team), 
                             Team = ifelse(Team == "Washington Redskins", "Washington Football Team", Team), 
                             Team = ifelse(Team == "San Diego Chargers", "Los Angeles Chargers", Team)) %>% 
                distinct(ID, Season, Week, .keep_all = TRUE) %>% dplyr::filter(ID != "a5358") # this player has inaccurate data

snaps <- df.1 %>% dplyr::filter(OVD != "ST") %>% dplyr::group_by(ID, Season) %>% dplyr::summarise(Season.snaps = sum(Total.Snaps))

# Add covariates 

# Sum number of games on IR for each player

IR <- df.1 %>% dplyr::filter(Game_Designation == "Injured Reserve") %>% dplyr::group_by(ID, Season) %>% 
                          dplyr::summarise(IR.games = n()) %>% dplyr::mutate(IR.factor = 1) 


# Okay, now we need to deal with the injury_type data, which are a total mess. For our purposes, we are going to make new columns 
# for injury types of interest to our analysis. AKA, knees, ankles, Achilles, hamstring, back, neck, groin, 

IR.Inj <- df.1 %>% dplyr::filter(Game_Designation == "Injured Reserve") %>% dplyr::mutate(Injury.cat = NA)

# This is going to be ugly, but these data need to be cleaned in some way to be useful for an analysis 

for (i in 1:length(IR.Inj$Injury_Type)) { 
  
  if(grepl("Foot", IR.Inj$Injury_Type[i]))
  IR.Inj$Injury_Type[i] <- as.character("Foot") 
  
  if(grepl("Toe", IR.Inj$Injury_Type[i]))
    IR.Inj$Injury_Type[i] <- as.character("Foot") 
  
  else if(grepl("Knee", IR.Inj$Injury_Type[i]))
    IR.Inj$Injury_Type[i] <- as.character("Knee")
  
  else if(grepl("Shoulder", IR.Inj$Injury_Type[i]))
    IR.Inj$Injury_Type[i] <- as.character("Shoulder")
  
  else if(grepl("Clavicle", IR.Inj$Injury_Type[i]))
    IR.Inj$Injury_Type[i] <- as.character("Shoulder")
  
  else if(grepl("Collarbone", IR.Inj$Injury_Type[i]))
    IR.Inj$Injury_Type[i] <- as.character("Shoulder")
  
  else if(grepl("Ankle", IR.Inj$Injury_Type[i]))
    IR.Inj$Injury_Type[i] <- as.character("Ankle")

  else if(grepl("Achill", IR.Inj$Injury_Type[i]))
    IR.Inj$Injury_Type[i] <- as.character("Achilles")
  
  else if(grepl("Abdom", IR.Inj$Injury_Type[i]))
    IR.Inj$Injury_Type[i] <- as.character("Abdominal")
  
  else if(grepl("Groin", IR.Inj$Injury_Type[i]))
    IR.Inj$Injury_Type[i] <- as.character("Groin")
  
  else if(grepl("ACL", IR.Inj$Injury_Type[i]))
    IR.Inj$Injury_Type[i] <- as.character("Knee")
  
  else if(grepl("AcL", IR.Inj$Injury_Type[i]))
    IR.Inj$Injury_Type[i] <- as.character("Knee")
  
  else if(grepl("Acl", IR.Inj$Injury_Type[i]))
    IR.Inj$Injury_Type[i] <- as.character("Knee")
  
  else if(grepl("MCL", IR.Inj$Injury_Type[i]))
    IR.Inj$Injury_Type[i] <- as.character("Knee")
  
  else if(grepl("Tricep", IR.Inj$Injury_Type[i]))
    IR.Inj$Injury_Type[i] <- as.character("Arm")
  
  else if(grepl("Forearm", IR.Inj$Injury_Type[i]))
    IR.Inj$Injury_Type[i] <- as.character("Arm")
  
  else if(grepl("Elbow", IR.Inj$Injury_Type[i]))
    IR.Inj$Injury_Type[i] <- as.character("Arm")
  
  else if(grepl("Wrist", IR.Inj$Injury_Type[i]))
    IR.Inj$Injury_Type[i] <- as.character("Arm")
  
  else if(grepl("Bicep", IR.Inj$Injury_Type[i]))
    IR.Inj$Injury_Type[i] <- as.character("Arm")
  
  else if(grepl("Neck", IR.Inj$Injury_Type[i]))
    IR.Inj$Injury_Type[i] <- as.character("Spine")
  
  else if(grepl("Back", IR.Inj$Injury_Type[i]))
    IR.Inj$Injury_Type[i] <- as.character("Spine")
  
  else if(grepl("Shin", IR.Inj$Injury_Type[i]))
    IR.Inj$Injury_Type[i] <- as.character("Lower.Leg")
  
  else if(grepl("Tibia", IR.Inj$Injury_Type[i]))
    IR.Inj$Injury_Type[i] <- as.character("Lower.Leg")
  
  else if(grepl("Fibula", IR.Inj$Injury_Type[i]))
    IR.Inj$Injury_Type[i] <- as.character("Lower.Leg")
  
  else if(grepl("Calf", IR.Inj$Injury_Type[i]))
    IR.Inj$Injury_Type[i] <- as.character("Lower.Leg")
  
}

levels(as.factor(IR.Inj$Injury_Type))

# Select last IR injury des for each player for each season 

last.IR <- IR.Inj %>% dplyr::group_by(ID, Season) %>% dplyr::arrange(ID, Season, Week) %>% slice_tail()

head(last.IR)

help(lme4)

# Make factor covs for Achilles, Ankle, Shoulder, Foot, Spine, Hamstring, and Knee injuries based on the last IR-landing injury for each player 
# in each season. Retain the week column for each "final IR" stint by player/season, which can be used as a covariate showing how late into 
# the year each player was on IR in the previous season. # Create "other column" for injuries that aren't included in our factors. Just good 
# housekeeping. There are way too many categories to include in the analysis, but at least we can incorporate the "other" column t partially 
# incorporate those data. 

last.IR.1 <- last.IR %>% dplyr::mutate(Achilles = ifelse(grepl(Injury_Type, "Achilles") , 1, 0), 
                                       Knee =  ifelse(grepl(Injury_Type, "Knee") , 1, 0), 
                                        Foot =  ifelse(grepl(Injury_Type, "Foot") , 1, 0), 
                                      Spine = ifelse(grepl(Injury_Type, "Spine") , 1, 0), 
                                      Hamstring = ifelse(grepl(Injury_Type, "Hamstring") , 1, 0), 
                                      Groin = ifelse(grepl(Injury_Type, "Groin") , 1, 0), 
                                      Shoulder = ifelse(grepl(Injury_Type, "Shoulder") , 1, 0)) %>% 
                          dplyr::select(ID, Season, Week, Achilles, Knee, Foot, Spine, Hamstring, Groin, Shoulder) %>% 
                          dplyr::mutate(Other = ifelse(Achilles == 0 & Knee == 0 & Foot == 0 & Spine == 0 & Hamstring == 0 & 
                                                         Groin == 0 & Shoulder == 0,1, 0)) %>% dplyr::mutate(Season = Season + 1)
                                                                                    # Here we are modifying the season column so that 
                                                                    # IR covariates are matched with the NEXT years snap counts for our analysis.
                                                                    # IE IR stats from 2014, are paired covs with 2015 snaps 

head(last.IR.1)

# Nweeks on IR, >= 5 weeks on IR, >= 10 weeks on IR, etc.

# Get rid of holdout/suspension IR weeks, and then sum number of weeks on IR per season for each player, create factor variables for 
# >= 1, 5 and 10 weeks on IR for each season. Leave season as is, because these can  be used both as covariates or potential response variables. 

IR.inj.only <- IR.Inj %>% dplyr::filter(!grepl("Suspension|Holdout", Injury_Type)) %>% 
                            dplyr::group_by(ID, Season) %>% dplyr::summarise(N.IR.Wks = n()) %>% 
                                        dplyr::mutate(IR.1 = ifelse(N.IR.Wks > 0, 1, 0),
                                                      IR.5 = ifelse(N.IR.Wks > 4, 1, 0), 
                                                      IR.10 = ifelse(N.IR.Wks > 9, 1, 0)) %>%
                                              dplyr::mutate(Season = Season + 1)

head(IR.inj.only)
table(IR.inj.only$IR.10)
nrow(IR.inj.only)

# Now make a df of player-level covariates, age to start season, position, offense/defense, weight, 

player.covs <- df.1 %>% dplyr::group_by(ID, Season) %>% dplyr::select(ID, Season, Age_Start_Season, Position, OVD, weight) %>% 
                                                                      distinct()

head(player.covs)

# Now combine snaps df with all of the covariate DFS by Name, Season, and Team (cannot just use season and name because there are players
# with the same name in the data set). DFs IR.inj.only, Last.IR.1, player.covs WITH snaps 

head(snaps)

head(IR.inj.only)

head(last.IR.1)

all <- left_join(snaps, player.covs, by = c("ID", "Season")) %>% 
            left_join(., IR.inj.only, by = c("ID", "Season")) %>% 
                left_join(., last.IR.1, by = c("ID", "Season")) %>% 
                      dplyr::mutate(N.IR.Wks = ifelse(is.na(N.IR.Wks), 0, N.IR.Wks), 
                                    IR.1 = ifelse(is.na(IR.1), 0, IR.1), 
                                    IR.5 = ifelse(is.na(IR.5), 0, IR.5), 
                                    IR.10 = ifelse(is.na(IR.10), 0, IR.10), 
                                   Achilles = ifelse(is.na(Achilles), 0, Achilles), 
                                   Knee = ifelse(is.na(Knee), 0, Knee), 
                                   Foot = ifelse(is.na(Foot), 0, Foot), 
                                   Spine = ifelse(is.na(Spine), 0, Spine), 
                                   Hamstring = ifelse(is.na(Hamstring), 0, Hamstring), 
                                   Groin = ifelse(is.na(Groin), 0, Groin), 
                                   Shoulder = ifelse(is.na(Shoulder), 0, Shoulder), 
                                   Other = ifelse(is.na(Other), 0, Other)) %>% 
                        dplyr::rename(Prev.IR.wks = N.IR.Wks, 
                                      Prev.IR.1 = IR.1, 
                                      Prev.IR.5 = IR.5, 
                                      Prev.IR.10 = IR.10) %>% 
                              dplyr::mutate(Prev.IR.fact = ifelse(Prev.IR.wks > 0, 1, 0))


names(all)

head(all)


# Okay so now we have all of our IR covariates lined up with snap counts from the subsequent season. BUT we still want to pair IR each seasons IR 
# data with current season snap counts to use as potential covariates (IE how do last years IR data influence current years IR status for 
# each player). We will do this with the IR week data, but the injury type data. 

# Take original IR dataframe and modify back to original season (added 1 year to each season previously, now subtract one year)

IR.inj.only.2 <- IR.inj.only %>% dplyr::mutate(Season = Season -1, 
                                               Curr.IR.fact = 1) %>%
                                dplyr::rename(Curr.IR.wks = N.IR.Wks, 
                                              Curr.IR.1 = IR.1, 
                                              Curr.IR.5 = IR.5, 
                                              Curr.IR.10 = IR.10)

head(IR.inj.only.2)

# Join all and update IR df. Lastly, create cumulative IR covariates, which show the cumulative sum of weeks each player has spent on their
# career (does not reset each season) plus the cumulative number of consecutive seasons that each player has spent at least one week on IR. 

all.1 <- left_join(all, IR.inj.only.2, by = c("ID", "Season")) %>% 
                                                      dplyr::mutate(Curr.IR.wks = ifelse(is.na(Curr.IR.wks), 0, Curr.IR.wks), 
                                                                    Curr.IR.1 = ifelse(is.na(Curr.IR.1), 0, Curr.IR.1), 
                                                                    Curr.IR.5 = ifelse(is.na(Curr.IR.5), 0, Curr.IR.5), 
                                                                    Curr.IR.10 = ifelse(is.na(Curr.IR.10), 0, Curr.IR.10), 
                                                                    Curr.IR.fact = ifelse(is.na(Curr.IR.fact), 0, Curr.IR.fact)) %>% 
                        dplyr::group_by(ID) %>% dplyr::arrange(ID, Season) %>% 
                                                                dplyr::mutate(Cum.IR.wks = cumsum(Curr.IR.wks), 
                                                                      Cum.IR.fact = cumsum(Curr.IR.fact)) %>% 
                                                        dplyr::mutate(Week = ifelse(is.na(Week), 0, Week))

head(all.1)

# Now we have our final data set. We will have to tweak our data here and there based on which analysis we are doing, as if cannot analyze 
# data using previous season IR covariates 

# Correlation and colinearity 

cora <- with(na.omit(all.1), cor(data.frame(Season, Season.snaps, Age_Start_Season, weight, Prev.IR.wks, Prev.IR.1, Prev.IR.5, Prev.IR.10, 
                                            Week, Curr.IR.wks, Curr.IR.1, Curr.IR.5, Curr.IR.10, Cum.IR.wks)))

cora

# Some interesting correlation, namely the correlation between spring NDVI values and carnivore harvest. Also of note, the lack of correlation between
# chest girth and birth mass, Lastly, obvious correlation between total female lion harvest and other lion harvest metrics. 

cor.plot <- corrplot::corrplot(cora,"shade")

cor.plot

# Frequentist Modeling - Create 4 different model suites, where we test all possible combinations of covariates to explain variation in 
# Snaps, current IR weeks (linear all data, what is related to the N weeks on IR), Current IR factor (if on IR in previous
# season, then what factors contribute to prob of going back on IR ), and Current IR 5 for players with previous IR 5 year.

# Snaps - frequentist model comparison of linear regression models
# Question - How are previous years IR stats and other covariates related to the number of player snaps in the cocurrent season
# First, we are going to test the influence of previous seasons IR covariates on N snaps per each player/season. Tested IR covariates include 
# Prev.IR.wks, Prev.IR.5, Prev.IR.10, the week the player last went on IR in the previous season, various factors to describe injuries to specific
# body parts in the previous season, and cumulative IR weeks a player has spent on IR across seasons

# As mentioned above, we are going to have to tweakk our dataframe for each specific suite of models. In the case of our snaps models, 
# we need to remove the first record for each player (IE the earliest season that appears in our dataframe), as those data are not associated
# with any "previous seasons IR data" to use as covariates (that is just the earliest season we have data for). 

# Also, players that never see the field, regardless of their injury/IR status, may confound our ability to analyze trends in our data. Therefore, 
# we will remove players from our data who have never played more than 200 snaps in a season (within the temporal scope of our data). 

career.stats <- all.1 %>% dplyr::group_by(ID) %>% dplyr::summarise(Sum.snaps = max(Season.snaps)) %>% 
                                                      dplyr::filter(Sum.snaps > 200)


snaps.df <- all.1 %>% dplyr::arrange(ID, Season) %>% dplyr::group_by(ID) %>%
                                                                          slice(2:n()) %>% dplyr::filter(ID %in% career.stats$ID, 
                                                                                                         Position != "ST") 



Curr.IR.df <- all.1 %>% dplyr::arrange(ID, Season) %>% dplyr::group_by(ID) %>%
  slice(2:n())

Curr.IR.df.1 <- Curr.IR.df %>% dplyr::filter(Position != "ST")

########### MODELING MODELING MODELING MODELING MODELING MODELING MODELING MODELING ##################

# Still leaves us with plenty of data 

nrow(snaps.df)

head(snaps.df)

# build global model, using gaussian distribution (assumes normally distributed variables) for now. 

global.snaps <- glm(Season.snaps~Age_Start_Season + Position + OVD + weight + Prev.IR.wks + as.factor(Prev.IR.5) + 
                      as.factor(Prev.IR.10) + Week, data = snaps.df, family = gaussian)

options(na.action = "na.pass") 

summary(global.snaps)

# set global options to NA pass because we already remove NAs from our snaps data and 
# we cannot dredge models that use different data. 

snap.dredge <- dredge(global.snaps)
  
snap.dredge

top.snaps <- glm(Season.snaps~Age_Start_Season + Position +  weight + Prev.IR.wks + Prev.IR.10, data = snaps.df, family = gaussian)

summary(top.snaps)

nrow(snap.dredge)

# N Weeks on IR IN current season based on covariates ############### Linear regression 
################# N WEEKS ON IR IN CURRENT SEASON - LINEAR REGRESSION #################

# Here we will again remve the first record for each player as we did with the "snaps" dataframe, because we are interested in how the previous years IR covariates
# and others influence the number of weeks spend on IR in the current season. 

# still leaves us with plenty of data 

nrow(Curr.IR.df)

# Set up global model where response var is the number of weeks spent on IR in the current season, based on 
# age, position, offense vs. defense, number of weeks on IR In previous season, weight, factors for 5 or 10 weeks on 
# IR in the previous season, as well as the last week spent on IR in the previous season. 

global.curr.IR.wks <- glm(Curr.IR.wks~Age_Start_Season + Position + OVD + weight + 
                      Prev.IR.wks + as.factor(Prev.IR.5) + as.factor(Prev.IR.10) + Week, data = Curr.IR.df, family = gaussian)

Curr.IR.dredge <- dredge(global.curr.IR.wks)

head(Curr.IR.dredge)

nrow(Curr.IR.dredge)

################### CURRENT IR FACTOR BASED ON PREVIOUS IR STATS AND OTHER COVARIATES - LOGISTIC REGRESSION #########################
###############################################################################################################

# USe the curr.IR.df again 


global.curr.IR.binom <- glm(as.factor(Curr.IR.fact) ~ Age_Start_Season + Position + OVD + weight + 
                              Prev.IR.wks + as.factor(Prev.IR.5) + as.factor(Prev.IR.10) + Week, data = Curr.IR.df, family = "binomial")


curr.IR.binom.dredge <- dredge(global.curr.IR.binom)

head(curr.IR.binom.dredge)

################## Current IR five FACTOR - LOgistic regression ##################################################

global.curr.IR.5.binom <- glm(as.factor(Curr.IR.5) ~ Age_Start_Season + Position + OVD + weight + 
                              Prev.IR.wks + as.factor(Prev.IR.5) + as.factor(Prev.IR.10) + Week,
                              data = Curr.IR.df, family = "binomial")



curr.IR.5.binom.dredge <- dredge(global.curr.IR.5.binom)

head(curr.IR.5.binom.dredge)

################## CUrrent IR TEN FACTOR - Logistic Regression ################################################

global.curr.IR.10.binom <- glm(as.factor(Curr.IR.10) ~ Age_Start_Season + Position + OVD + weight + 
                                Prev.IR.wks + as.factor(Prev.IR.5) + as.factor(Prev.IR.10) + Week, data = Curr.IR.df, family = "binomial")

curr.IR.10.binom.dredge <- dredge(global.curr.IR.10.binom)

head(curr.IR.10.binom.dredge) ### This one is pretty weak. 

################### Now run a few injury specific models ##################################################################

##################### Injury specific IR Weeks analysis 

names(Curr.IR.df)

global.curr.IR.wks.inj <- glm(Curr.IR.wks~as.factor(Achilles) + as.factor(Knee) + as.factor(Foot) + as.factor(Spine)
                              + as.factor(Hamstring) + as.factor(Groin) + as.factor(Shoulder) + Week, data = Curr.IR.df, family = gaussian)

Curr.IR.inj.dredge <- dredge(global.curr.IR.wks.inj)

head(Curr.IR.inj.dredge)

################### Injury specific CURRENT IR FACTOR BASED ON PREVIOUS IR STATS AND OTHER COVARIATES - LOGISTIC REGRESSION #########################
###############################################################################################################

# USe the curr.IR.df again 


global.curr.IR.inj.binom <- glm(Curr.IR.fact ~ as.factor(Achilles) + as.factor(Knee) + as.factor(Foot) + as.factor(Spine)
                                + as.factor(Hamstring) + as.factor(Groin) + as.factor(Shoulder) + Week,
                            data = Curr.IR.df, family = "binomial")


curr.IR.inj.binom.dredge <- dredge(global.curr.IR.inj.binom)

head(curr.IR.inj.binom.dredge)

################## Injury specific Current IR five FACTOR - LOgistic regression ##################################################

global.curr.IR.inj.5.binom <- glm(Curr.IR.5 ~ as.factor(Achilles) + as.factor(Knee) + as.factor(Foot) + as.factor(Spine)
                                  + as.factor(Hamstring) + as.factor(Groin) + as.factor(Shoulder) + Week,
                              data = Curr.IR.df, family = "binomial")

curr.IR.inj.5.binom.dredge <- dredge(global.curr.IR.inj.5.binom)

head(curr.IR.inj.5.binom.dredge)

######################### Injury Specific Current IR ten Factor - Logistic Regression ###############################################


global.curr.IR.inj.10.binom <- glm(Curr.IR.10 ~ as.factor(Achilles) + as.factor(Knee) + as.factor(Foot) + as.factor(Spine)
                                  + as.factor(Hamstring) + as.factor(Groin) + as.factor(Shoulder) + Week,
                                  data = Curr.IR.df, family = "binomial")

curr.IR.inj.10.binom.dredge <- dredge(global.curr.IR.inj.10.binom)

nrow(curr.IR.inj.10.binom.dredge)


##############################################################################################

######################## BAYSEIAN MODELING AND PREDICTION ###################################

# Now we will take all covariates retained from our comparisons of all possible models, and refit the models to JAGS (Bayes) to 
# enable easy prediction and also to add a random effect of player 

################ LINEAR Regression Bayseian models (3 total)

# Current Snaps~Age + Position + Prev.IR.Weeks + weight - Linear 

# Curr.IR.wks~Age + Position + Prev.IR.5 + Week + weight - Linear

# Current IR Weeks~Hamstring + Knee + Spine + Week - Linear 

head(Curr.IR.inj.dredge)

############ Current Snaps by Position + Age + Prev.IR.Weeks + weight in Bayes - Add random effect of ID (oh im excited for this)

snap.dredge.1 <- as.data.frame(snap.dredge) %>% dplyr::filter(delta <4) %>% dplyr::select(-"(Intercept)", -df, -logLik, -AICc, -delta) %>% 
                      summarise_all(funs(1 - sum(is.na(.))/nrow(dplyr::filter(snap.dredge, delta < 4)))) %>% tidyr::gather() %>%
  dplyr::rename(Covariate = key, Prop.Mod = value) 


snap.dredge.1

NID <- length(levels(as.factor(snaps.df$ID)))

snapsdata_s3 <- with(snaps.df, list(Prev.IR.wks = Prev.IR.wks, ID = as.factor(ID),
                                    Position = as.factor(Position), Age_Start_Season = Age_Start_Season, weight = weight,
                                  N = length(snaps.df$Season.snaps), NID = NID, Season_Snaps = Season.snaps, 
                                  age.predict = seq(min(snaps.df$Age_Start_Season), max(snaps.df$Age_Start_Season), 1), # These .predict columns
                                  prev.ir.predict = seq(min(snaps.df$Prev.IR.wks), max(snaps.df$Prev.IR.wks), 1), # will come in handy later
                                  weight.predict = seq(min(snaps.df$weight), max(snaps.df$weight), 1)
                                  )
                     )

snaps_jags <- function(){
  # Likelihood:
  for (i in 1:N){
    Season_Snaps[i] ~ dnorm(mu[i], tau) # tau is precision (1 / variance)
    mu[i] <- alpha + a[ID[i]] + beta[Position[i]] + # Set up random effect of player and fixed effects coefs for each position 
                                     beta1 * Age_Start_Season[i] + beta2 * Prev.IR.wks[i] + beta3 * weight[i] # Add betas for age, previrweeks, and weight
  }
  # Priors:
  alpha ~ dnorm(0, 0.01) # intercept
  sigma_a ~ dunif(0, 100) # standard deviation of random effect (variance between sites)
  beta1 ~ dnorm(0, 0.01) # Covariate priors 
  beta2 ~ dnorm(0, 0.01)
  beta3 ~ dnorm(0, 0.01)
  tau_a <- 1 / (sigma_a * sigma_a) # convert to precision
  for (j in 1:NID){
    a[j] ~ dnorm(0, tau_a) # random intercept for each player
  }
  beta[1] <- 0 # Set up reference category
  for (i in 2:8){
  beta[i] ~ dnorm(0, 0.01) # Set up coefficients for each subsequent position category 
  }
  sigma ~ dunif(0, 100) # standard deviation of fixed effect (variance within players)
  tau <- 1 / (sigma * sigma) # convert to precision
  
  # NOw a bunch of derived parameters. Basically, we will use the intercept and betas to predict snaps across values of 
  # covariates using the pre-fab prediction data I put in the jags snap data
  
  for (i in 1:length(age.predict)){ # Age prediction across range of ages in our data 
    
    predict.age[i] <- alpha + beta1 * age.predict[i] + beta2 * 1.1 + beta3 * 240
  }
  
  for (i in 1:length(weight.predict)){
    predict.weight[i] <- alpha + beta3 * weight.predict[i] + beta2 * 1.1 + beta1 * 26
  }
  
  for (i  in 1:length(prev.ir.predict)){
    predict.ir[i] <- alpha + beta2 * prev.ir.predict[i] + beta3 * 240 + beta1 * 26
  }
  
  for (i in 2:8){ # Prediction for each position 
    pos.1[i] <- alpha + beta[i] * 1 + beta2 * 1.1 + beta1 * 26 + beta3 * 240
  }
  
  pos.0 <- alpha + beta2 * 1.1 + beta1 * 26 + beta3 * 240                                    # Prediction for reference position 
  
}


snaps.init <- function(){
  list(alpha = rnorm(1), sigma_a = runif(1), sigma = runif(1))
}

snaps.params <- c("alpha", "beta","beta1", "beta2", "beta3", "sigma", "sigma_a", 
            "predict.age", "predict.ir","predict.weight", "pos.1", "pos.0")

fit_snaps <- jags(data = snapsdata_s3, inits = snaps.init, parameters.to.save = snaps.params, model.file = snaps_jags,
                n.chains = 3, n.iter = 50, n.burnin = 10,  DIC = F)

fit_snaps

###################################################################################################################################
######################### Curr.IR.wks~Age + Position +  Week + weight - Linear #####################
###################################################################################################################################

head(Curr.IR.dredge)

curr.ir.dredge.1 <- as.data.frame(Curr.IR.dredge) %>% dplyr::filter(delta <4) %>% dplyr::select(-"(Intercept)", -df, -logLik, -AICc, -delta) %>% 
  summarise_all(funs(1 - sum(is.na(.))/nrow(dplyr::filter(Curr.IR.dredge, delta < 4)))) %>% tidyr::gather() %>%
  dplyr::rename(Covariate = key, Prop.Mod = value)

curr.ir.dredge.1

NID <- length(levels(as.factor(Curr.IR.df.1$ID)))


IRweeksdata_s3 <- with(Curr.IR.df.1, list(ID = as.factor(ID), Curr.IR.wks = Curr.IR.wks, Week = Week, 
                                    Position = as.factor(Position), Age_Start_Season = Age_Start_Season, 
                                    weight = weight,
                                    age.predict = seq(min(Curr.IR.df$Age_Start_Season), max(Curr.IR.df$Age_Start_Season), 1), # These .predict columns
                                    weight.predict = seq(min(Curr.IR.df$weight), max(Curr.IR.df$weight), 1),
                                    week.predict = seq(min(Curr.IR.df$Week), max(Curr.IR.df$Week), 1),
                                    N = length(Curr.IR.df.1$Curr.IR.wks), NID = NID))


curr.ir.jags <- function(){
  # Likelihood:
  for (i in 1:N){
    Curr.IR.wks[i] ~ dnorm(mu[i], tau) # tau is precision (1 / variance)
    mu[i] <- alpha + a[ID[i]] + beta[Position[i]] + 
      beta1 * Age_Start_Season[i] + beta2 * Week[i] + beta3 * weight[i] # Add betas for age, week, and weight
  }
  # Priors:
  alpha ~ dnorm(0, 0.01) # intercept
  sigma_a ~ dunif(0, 100) # standard deviation of random effect (variance between sites)
  beta1 ~ dnorm(0, 0.01) # Covariate priors 
  beta2 ~ dnorm(0, 0.01)
  beta3 ~ dnorm(0, 0.01)
  
  tau_a <- 1 / (sigma_a * sigma_a) # convert to precision
  for (j in 1:NID){
    a[j] ~ dnorm(0, tau_a) # random intercept for each player
  }
  beta[1] <- 0 # Set up reference category
  for (i in 2:8){
    beta[i] ~ dnorm(0, 0.01) # Set up coefficients for each subsequent position category 
  }
  sigma ~ dunif(0, 100) # standard deviation of fixed effect (variance within players)
  tau <- 1 / (sigma * sigma) # convert to precision
  
  # NOw a bunch of derived parameters. 
  
  for (i in 1:length(age.predict)){ # Age prediction across range of ages in our data 
    
    predict.age[i] <- alpha + beta1 * age.predict[i] + beta2 * 8 + beta3 * 240
  }
  
  for (i in 1:length(weight.predict)){ # Weight prediction 
    
    predict.weight[i] <- alpha + beta3 * weight.predict[i] + beta1 * 26 + beta2 * 8
  }
  
  for (i in 1:length(week.predict)){ # Week prediction 
    
    predict.week[i] <- alpha + beta2 * week.predict[i] + beta1 * 26 + beta3 * 240
  }
  
  for (i in 2:8){ # Prediction for each position 
    pos.1[i] <- alpha + beta[i] * 1 + beta1 * 26 + beta2 * 8 + beta3 * 240
  }
  
  pos.0 <- alpha + beta1 * 26 + beta2 * 8 + beta3 * 240 # Prediction for reference position 
  
}


curr.iR.init <- function(){
  list(alpha = rnorm(1), sigma_a = runif(1), sigma = runif(1))
}

curr.ir.params <- c("alpha", "beta", "beta1", "beta2", "beta3", "sigma", "sigma_a",
             "pos.1", "pos.0", "predict.age", "predict.weight", "predict.week")

fit_curr.ir.wks <- jags(data = IRweeksdata_s3, inits = curr.iR.init, parameters.to.save = curr.ir.params, model.file = curr.ir.jags,
                n.chains = 3, n.iter = 500, n.burnin = 10,  DIC = F)

fit_curr.ir.wks

########################################################################################################################################
############################################# CUrrent IR wks ~ Knee + Spine + Hamstring + Week + weight ################################
########################################################################################################################################

#global.curr.IR.wks.inj <- glm(Curr.IR.wks~as.factor(Achilles) + as.factor(Knee) + as.factor(Foot) + as.factor(Spine)
                            #  + as.factor(Hamstring) + as.factor(Groin) + as.factor(Shoulder) + Week, data = Curr.IR.df, family = gaussian)

curr.ir.inj.dredge.1 <- as.data.frame(Curr.IR.inj.dredge) %>% dplyr::filter(delta <4) %>% dplyr::select(-"(Intercept)", -df, -logLik, -AICc, -delta) %>% 
  summarise_all(funs(1 - sum(is.na(.))/nrow(dplyr::filter(Curr.IR.inj.dredge, delta < 4)))) %>% tidyr::gather() %>%
  dplyr::rename(Covariate = key, Prop.Mod = value) 


curr.ir.inj.dredge.1

NID <- length(levels(as.factor(Curr.IR.df.1$ID)))

weeks.inj.data <- with(Curr.IR.df.1, list(ID = as.factor(ID), Curr.IR.wks = Curr.IR.wks, Week = Week, 
                                          weight = weight, Knee = as.factor(Knee), Spine = as.factor(Spine), Hamstring = as.factor(Hamstring),
                                          predict.data = c(1,0),
                                          N = length(Curr.IR.df.1$Curr.IR.wks), NID = NID))

weeks.inj.data

weeks.inj.jags <- function(){
  # Likelihood:
  for (i in 1:N){
    Curr.IR.wks[i] ~ dnorm(mu[i], tau) # tau is precision (1 / variance)
    mu[i] <- alpha + a[ID[i]] +
                beta1 * Week[i] + beta2 * weight[i] + 
      beta3 * Knee[i] + beta4 * Spine[i] + beta5 * Hamstring[i]# Add betas knee, spine, and hammy
  }
  # Priors:
  alpha ~ dnorm(0, 0.01) # intercept
  sigma_a ~ dunif(0, 100) # standard deviation of random effect (variance between sites)
  beta1 ~ dnorm(0, 0.01) # Covariate priors 
  beta2 ~ dnorm(0, 0.01)
  beta3 ~ dnorm(0, 0.01)
  beta4 ~ dnorm(0, 0.01)
  beta5 ~ dnorm(0, 0.01)
  
  tau_a <- 1 / (sigma_a * sigma_a) # convert to precision
  for (j in 1:NID){
    a[j] ~ dnorm(0, tau_a) # random intercept for each player
  }
  
  sigma ~ dunif(0, 100) # standard deviation of fixed effect (variance within players)
  tau <- 1 / (sigma * sigma) # convert to precision
  
  predict.knee[1] <- alpha + beta1 * 8 + beta2 * 240 # Very simple derived parameters for this one, but still worth within jags because it gives us 
  predict.knee[2] <- alpha + beta3 * 1 + beta1 * 8 + beta2 + 240 # free uncertainty without having to use delta method or bootstrap
  
  predict.spine[1] <- alpha + beta1 * 8 + beta2 * 240
  predict.spine[2] <- alpha + beta4 * 1 + beta1 * 8 + beta2 + 240 #
  
  predict.hamstring[1] <- alpha + beta1 * 8 + beta2 * 240
  predict.hamstring[2] <- alpha + beta5 * 1 + beta1 * 8 + beta2 * 240#

  
  
}

weeks.inj.init <- function(){
  list(alpha = rnorm(1), sigma_a = runif(1), sigma = runif(1))
}

weeks.inj.params <- c("alpha", "beta1", "beta2", "beta3","beta4", "beta5", "sigma", "sigma_a", 
                      "predict.knee", "predict.spine", "predict.hamstring")

fit_weeks.inj <- jags(data = weeks.inj.data, inits = weeks.inj.init, parameters.to.save = weeks.inj.params, model.file = weeks.inj.jags,
                        n.chains = 3, n.iter = 50, n.burnin = 10,  DIC = F)

fit_weeks.inj

######### Other ideas ####################################################################

{
  
start_time <- Sys.time()
  
fit_curr.ir.wks <- jags(data = IRweeksdata_s3, inits = curr.iR.init, parameters.to.save = curr.ir.params, model.file = curr.ir.jags,
                        n.chains = 3, n.iter = 50000, n.burnin = 2000,  DIC = F)

fit_curr.ir.wks

fit_snaps <- jags(data = snapsdata_s3, inits = snaps.init, parameters.to.save = snaps.params, model.file = snaps_jags,
                  n.chains = 3, n.iter = 50000, n.burnin = 2000,  DIC = F)

fit_snaps

fit_weeks.inj <- jags(data = weeks.inj.data, inits = weeks.inj.init, parameters.to.save = weeks.inj.params, model.file = weeks.inj.jags,
                      n.chains = 3, n.iter = 50000, n.burnin = 2000,  DIC = F)

fit_weeks.inj

end_time <- Sys.time()

total.time <- end_time - start_time

total.time

}

#save(fit_curr.ir.wks, file = "Fit.Curr.IR.Wks.RData")

#save(fit_snaps, file = "Fit.Snaps.RData")

#save(fit_weeks.inj, file = "Fit.Wks.Inj.RData")

###################################################################################################
#################################   LOGISTIC REGRESSION JAGS MODELS ################################
###################################################################################################
###################################################################################################

###################################################################################################
# IR FACTOR MAIN ############## IR.BINOM ~ AGE + POSITION + WEEK + WEIGHT 

global.curr.IR.binom <- glm(as.factor(Curr.IR.fact) ~ Age_Start_Season + Position + OVD + weight + 
                              Prev.IR.wks + as.factor(Prev.IR.5) + as.factor(Prev.IR.10) + Week, data = Curr.IR.df, family = "binomial")

curr.IR.binom.dredge.1 <- as.data.frame(curr.IR.binom.dredge) %>% dplyr::filter(delta <4) %>% dplyr::select(-"(Intercept)", -df, -logLik, -AICc, -delta) %>% 
  summarise_all(funs(1 - sum(is.na(.))/nrow(dplyr::filter(curr.IR.binom.dredge, delta < 4)))) %>% tidyr::gather() %>%
  dplyr::rename(Covariate = key, Prop.Mod = value) 

curr.IR.binom.dredge.1

# Setup JAGS model 

NID <- length(levels(as.factor(Curr.IR.df.1$ID)))

IR.binom.data <- with(Curr.IR.df.1, list(ID = as.factor(ID), Curr.IR.fact = as.integer(Curr.IR.fact), 
                                         Week = Week, Position = as.factor(Position), 
                                         Age_Start_Season = Age_Start_Season, weight = weight,
                                          age.predict = seq(min(Curr.IR.df$Age_Start_Season), max(Curr.IR.df$Age_Start_Season), 1), # These .predict columns
                                          weight.predict = seq(min(Curr.IR.df$weight), max(Curr.IR.df$weight), 1),
                                          week.predict = seq(min(Curr.IR.df$Week), max(Curr.IR.df$Week), 1),
                                          N = length(Curr.IR.df.1$Curr.IR.fact), NID = NID))

IR.binom.jags <- function() {
  for( i in 1 : N ) {
    Curr.IR.fact[i] ~ dbern(p[i])
    logit(p[i]) <- b0 + a[ID[i]] + beta1*Age_Start_Season[i] + 
                                   beta2*Week[i] + beta3*weight[i] + beta[Position[i]]
  }     
  
  b0 ~ dnorm( 0 , 1.0E-12 )
  beta1 ~ dnorm( 0 , 1.0E-12 )
  beta2 ~ dnorm( 0 , 1.0E-12 )
  beta3 ~ dnorm( 0 , 1.0E-12 )
  
  beta[1] <- 0 # Set up reference category
  for (i in 2:8){
    beta[i] ~ dnorm(0, 0.01) # Set up coefficients for each subsequent position category 
  }

  sigma_a ~ dunif(0, 100) # standard deviation of random effect (variance between sites)
  tau_a <- 1 / (sigma_a * sigma_a) # convert to precision
  for (j in 1:NID){
    a[j] ~ dnorm(0, tau_a) # random intercept for each site
  }
  sigma ~ dunif(0, 100) # standard deviation of fixed effect (variance within sites)
  tau <- 1 / (sigma * sigma) # convert to precision
  
  for (i in 1:length(age.predict)){ # Age prediction across range of ages in our data 
    
    logit(predict.age[i]) <- b0 + beta1 * age.predict[i] + beta2 * 8 + beta3 * 240
  }
  
  for (i in 1:length(weight.predict)){ # Weight prediction 
    
    logit(predict.weight[i]) <- b0 + beta3 * weight.predict[i] + beta1 * 26 + beta2 * 8
  }
  
  for (i in 1:length(week.predict)){ # Week prediction 
    
    logit(predict.week[i]) <- b0 + beta2 * week.predict[i] + beta1 * 26 + beta3 * 240
  }
  
  for (i in 2:8){ # Prediction for each position 
    logit(pos.1[i]) <- b0 + beta[i] * 1 + beta1 * 26 + beta2 * 8 + beta3 * 240
  }
  
  logit(pos.0) <- b0 + beta1 * 26 + beta2 * 8 + beta3 * 240 # Prediction for reference position 
  
  
}

estIR.binom <- glm(IR.binom.data$Curr.IR.fact~IR.binom.data$Age_Start_Season + IR.binom.data$Week + 
                       IR.binom.data$weight, family=binomial(logit))


#Ir.binom.inits <-list(
 # list(b0=estIR.binom$coef[1]+.5, beta1=estIR.binom$coef[2]+.1, 
  #     beta2=estIR.binom$coef[3]+.1, beta3=estIR.binom$coef[4]+.1,
   #    sigma_a = runif(1), sigma = runif(1)),
  
  
#  list(b0=estIR.binom$coef[1]-.5, beta1=estIR.binom$coef[2]-.1, 
 #      beta2=estIR.binom$coef[3]-.1, beta3=estIR.binom$coef[4]-.1, 
  #     sigma_a = runif(1), sigma = runif(1))        
#)

IR.binom.params <- c("b0", "beta1","beta2","beta", "beta3", "sigma", "sigma_a", 
                     "predict.age", "predict.weight", "predict.week", "pos.0", "pos.1")

#fit_IR.binom <- jags(data = IR.binom.data , inits = NULL, 
                 #      parameters.to.save = IR.binom.params, model.file = IR.binom.jags,
                   #      n.chains = 2, n.iter = 100, n.burnin = 50, n.thin = 10, DIC = F)

################################################### IR.5 MAIN ##############################################
####################################################################################################################################

global.curr.IR.5.binom <- glm(as.factor(Curr.IR.5) ~ Age_Start_Season + Position + OVD + weight + 
                                Prev.IR.wks + as.factor(Prev.IR.5) + as.factor(Prev.IR.10) + Week, data = Curr.IR.df, family = "binomial")

head(curr.IR.5.binom.dredge)

curr.IR.5.binom.dredge.1 <- as.data.frame(curr.IR.5.binom.dredge) %>% dplyr::filter(delta <4) %>% dplyr::select(-"(Intercept)", -df, -logLik, -AICc, -delta) %>% 
  summarise_all(funs(1 - sum(is.na(.))/nrow(dplyr::filter(curr.IR.5.binom.dredge, delta < 4)))) %>% tidyr::gather() %>%
  dplyr::rename(Covariate = key, Prop.Mod = value) 


curr.IR.5.binom.dredge.1

NID <- length(levels(as.factor(Curr.IR.df.1$ID)))

IR.5.binom.data <- with(Curr.IR.df.1, list(ID = as.factor(ID), Curr.IR.5 = as.integer(Curr.IR.5), 
                                         Week = Week, Position = as.factor(Position), Prev.IR.5 = as.factor(Prev.IR.5),
                                         Age_Start_Season = Age_Start_Season, weight = weight,
                                         age.predict = seq(min(Curr.IR.df$Age_Start_Season), max(Curr.IR.df$Age_Start_Season), 1), # These .predict columns
                                         weight.predict = seq(min(Curr.IR.df$weight), max(Curr.IR.df$weight), 1),
                                         week.predict = seq(min(Curr.IR.df$Week), max(Curr.IR.df$Week), 1),
                                         N = length(Curr.IR.df.1$Curr.IR.fact), NID = NID))


IR.5.binom.jags <- function() {
  for( i in 1 : N ) {
    Curr.IR.5[i] ~ dbern(p[i])
    logit(p[i]) <- b0 + a[ID[i]] + beta1*Age_Start_Season[i] + 
      beta2*Week[i] + beta3*weight[i] + beta[Position[i]] + beta4*Prev.IR.5[i]
  }     
  
  b0 ~ dnorm( 0 , 1.0E-12 )
  beta1 ~ dnorm( 0 , 1.0E-12 )
  beta2 ~ dnorm( 0 , 1.0E-12 )
  beta3 ~ dnorm( 0 , 1.0E-12 )
  beta4 ~ dnorm( 0 , 1.0E-12 )
  
  beta[1] <- 0 # Set up reference category
  for (i in 2:8){
    beta[i] ~ dnorm(0, 0.01) # Set up coefficients for each subsequent position category 
  }
  
  sigma_a ~ dunif(0, 100) # standard deviation of random effect (variance between sites)
  tau_a <- 1 / (sigma_a * sigma_a) # convert to precision
  for (j in 1:NID){
    a[j] ~ dnorm(0, tau_a) # random intercept for each site
  }
  sigma ~ dunif(0, 100) # standard deviation of fixed effect (variance within sites)
  tau <- 1 / (sigma * sigma) # convert to precision
  
  for (i in 1:length(age.predict)){ # Age prediction across range of ages in our data 
    
    logit(predict.age[i]) <- b0 + beta1 * age.predict[i] + beta2 * 8 + beta3 * 240 + beta4 * 0.5
  }
  
  for (i in 1:length(weight.predict)){ # Weight prediction 
    
    logit(predict.weight[i]) <- b0 + beta3 * weight.predict[i] + beta1 * 26 + beta2 * 8 + beta4 * 0.5
  }
  
  for (i in 1:length(week.predict)){ # Week prediction 
    
    logit(predict.week[i]) <- b0 + beta2 * week.predict[i] + beta1 * 26 + beta3 * 240 + beta4 * 0.5
  }
  
  for (i in 2:8){ # Prediction for each position 
    logit(pos.1[i]) <- b0 + beta[i] * 1 + beta1 * 26 + beta2 * 8 + beta3 * 240 + beta4 * 0.5
  }
  
  logit(pos.0) <- b0 + beta1 * 26 + beta2 * 8 + beta3 * 240 + beta4 * 0.5 # Prediction for reference position 
  
  logit(prev.IR.5.predict1) <- b0 + beta4  * 1 + beta1 * 26 + beta2 * 8 + beta3 * 240
  
  logit(prev.IR.5.predict0) <- b0 + beta4  * 0 + beta1 * 26 + beta2 * 8 + beta3 * 240
  
  
}

IR.5.binom.params <- c("b0", "beta1","beta2","beta", "beta3","beta4", "sigma", "sigma_a", 
                     "predict.age", "predict.weight", "predict.week", "pos.0", "pos.1", 
                     "prev.IR.5.predict1", "prev.IR.5.predict0")

#fit_IR.5.binom <- jags(data = IR.5.binom.data , inits = NULL, 
                #        parameters.to.save = IR.5.binom.params, model.file = IR.5.binom.jags,
                   #    n.chains = 1, n.iter = 100, n.burnin = 50, n.thin = 10, DIC = F)

##################### IR 10 MAIN ############################################

global.curr.IR.10.binom <- glm(as.factor(Curr.IR.10) ~ Age_Start_Season + Position + OVD + weight + 
                                 Prev.IR.wks + as.factor(Prev.IR.5) + as.factor(Prev.IR.10) + Week, data = Curr.IR.df, family = "binomial")

head(curr.IR.10.binom.dredge)


curr.IR.10.binom.dredge.1 <- as.data.frame(curr.IR.10.binom.dredge) %>% dplyr::filter(delta <4) %>% dplyr::select(-"(Intercept)", -df, -logLik, -AICc, -delta) %>% 
  summarise_all(funs(1 - sum(is.na(.))/nrow(dplyr::filter(curr.IR.10.binom.dredge, delta < 4)))) %>% tidyr::gather() %>%
  dplyr::rename(Covariate = key, Prop.Mod = value) 


curr.IR.10.binom.dredge.1



IR.10.binom.data <- with(Curr.IR.df.1, list(ID = as.factor(ID), Curr.IR.10 = as.integer(Curr.IR.10), 
                                           Week = Week, Position = as.factor(Position), 
                                           Age_Start_Season = Age_Start_Season, weight = weight,
                                           age.predict = seq(min(Curr.IR.df$Age_Start_Season), max(Curr.IR.df$Age_Start_Season), 1), # These .predict columns
                                           weight.predict = seq(min(Curr.IR.df$weight), max(Curr.IR.df$weight), 1),
                                           week.predict = seq(min(Curr.IR.df$Week), max(Curr.IR.df$Week), 1),
                                           N = length(Curr.IR.df.1$Curr.IR.fact), NID = NID))


IR.10.binom.jags <- function() {
  for( i in 1 : N ) {
    Curr.IR.10[i] ~ dbern(p[i])
    logit(p[i]) <- b0 + a[ID[i]]  + 
      beta1*Week[i] + beta2*weight[i] + beta[Position[i]] 
  }     
  
  b0 ~ dnorm( 0 , 1.0E-12 )
  beta1 ~ dnorm( 0 , 1.0E-12 )
  beta2 ~ dnorm( 0 , 1.0E-12 )
  
  beta[1] <- 0 # Set up reference category
  for (i in 2:8){
    beta[i] ~ dnorm(0, 0.01) # Set up coefficients for each subsequent position category 
  }
  
  sigma_a ~ dunif(0, 100) # standard deviation of random effect (variance between sites)
  tau_a <- 1 / (sigma_a * sigma_a) # convert to precision
  for (j in 1:NID){
    a[j] ~ dnorm(0, tau_a) # random intercept for each site
  }
  sigma ~ dunif(0, 100) # standard deviation of fixed effect (variance within sites)
  tau <- 1 / (sigma * sigma) # convert to precision
  
  
  for (i in 1:length(weight.predict)){ # Weight prediction 
    
    logit(predict.weight[i]) <- b0 + beta2 * weight.predict[i] + beta1 * 8
  }
  
  for (i in 1:length(week.predict)){ # Week prediction 
    
    logit(predict.week[i]) <- b0 + beta1 * week.predict[i] + beta2 * 240
  }
  
  for (i in 2:8){ # Prediction for each position 
    logit(pos.1[i]) <- b0 + beta[i] * 1 + beta1 * 8 + beta2 * 240
  }
  
  logit(pos.0) <- b0 + beta1 * 8 + beta2 * 240 # Prediction for reference position 
  
}

IR.10.binom.params <- c("b0", "beta1","beta2","beta", "sigma", "sigma_a", 
                        "predict.weight", "predict.week", "pos.0", "pos.1")

#fit_IR.10.binom <- jags(data = IR.10.binom.data , inits = NULL, 
        #                  parameters.to.save = IR.10.binom.params, model.file = IR.10.binom.jags,
                #          n.chains = 1, n.iter = 500, n.burnin = 100, n.thin = 10, DIC = F)

#################################################### IR INJURY FACTOR  ##########################################################

global.curr.IR.inj.binom <- glm(Curr.IR.fact ~ as.factor(Achilles) + as.factor(Knee) + as.factor(Foot) + as.factor(Spine)
                                + as.factor(Hamstring) + as.factor(Groin) + as.factor(Shoulder) + Week,
                                data = Curr.IR.df, family = "binomial")

head(curr.IR.inj.binom.dredge)

curr.IR.inj.binom.dredge.1 <- as.data.frame(curr.IR.inj.binom.dredge) %>% dplyr::filter(delta <4) %>% dplyr::select(-"(Intercept)", -df, -logLik, -AICc, -delta) %>% 
  summarise_all(funs(1 - sum(is.na(.))/nrow(dplyr::filter(curr.IR.inj.binom.dredge, delta < 4)))) %>% tidyr::gather() %>%
  dplyr::rename(Covariate = key, Prop.Mod = value) 


curr.IR.inj.binom.dredge.1

IR.inj.binom.data <- with(Curr.IR.df.1, list(ID = as.factor(ID), Curr.IR.fact = Curr.IR.fact, Week = Week, 
                                          weight = weight, Knee = as.factor(Knee), Spine = as.factor(Spine), 
                                          Hamstring = as.factor(Hamstring),
                                          predict.data = c(1,0),
                                          N = length(Curr.IR.df.1$Curr.IR.wks), NID = NID))

IR.inj.binom.jags <- function() {
  for( i in 1 : N ) {
    Curr.IR.fact[i] ~ dbern(p[i])
    logit(p[i]) <- b0 + a[ID[i]] +
      beta1 * Week[i] + beta2 * weight[i] + 
      beta3 * Knee[i] + beta4 * Spine[i] + beta5 * Hamstring[i]
  }     
  
  b0 ~ dnorm( 0 , 1.0E-12 )
  beta1 ~ dnorm( 0 , 1.0E-12 )
  beta2 ~ dnorm( 0 , 1.0E-12 )
  beta3 ~ dnorm( 0 , 1.0E-12 )
  beta4 ~ dnorm( 0 , 1.0E-12 )
  beta5 ~ dnorm( 0 , 1.0E-12 )
  
  sigma_a ~ dunif(0, 100) # standard deviation of random effect (variance between sites)
  tau_a <- 1 / (sigma_a * sigma_a) # convert to precision
  for (j in 1:NID){
    a[j] ~ dnorm(0, tau_a) # random intercept for each site
  }
  sigma ~ dunif(0, 100) # standard deviation of fixed effect (variance within sites)
  tau <- 1 / (sigma * sigma) # convert to precision
  
  logit(predict.knee[1]) <- b0 + beta1 * 8 + beta2 * 240 # Very simple derived parameters for this one, but still worth within jags because it gives us 
   logit(predict.knee[2]) <- b0 + beta3 * 1 + beta1 * 8 + beta2 * 240 # free uncertainty without having to use delta method or bootstrap
  
   logit(predict.spine[1]) <- b0 + beta1 * 8 + beta2 * 240
   logit(predict.spine[2]) <- b0 + beta4 * 1 + beta1 * 8 + beta2 * 240 #
  
   logit(predict.hamstring[1]) <- b0  + beta1 * 8 + beta2 * 240
   logit(predict.hamstring[2]) <- b0 + beta5 * 1 + beta1 * 8 + beta2 * 240 #
  
}

IR.inj.binom.params <- c("b0", "beta1","beta2", "beta3", "beta4", "beta5",
                         "predict.knee", "predict.spine", "predict.hamstring",
                         "sigma", "sigma_a")

#fit_IR.inj.binom <- jags(data = IR.inj.binom.data , inits = NULL, 
 #                         parameters.to.save = IR.inj.binom.params, model.file = IR.inj.binom.jags,
  #                    n.chains = 2, n.iter = 50, n.burnin = 10, n.thin = 10, DIC = F)


#################################################### IR.5 INJURY FACTOR ##############################################################################################

global.curr.IR.inj.5.binom <- glm(Curr.IR.5 ~ as.factor(Achilles) + as.factor(Knee) + as.factor(Foot) + as.factor(Spine)
                                  + as.factor(Hamstring) + as.factor(Groin) + as.factor(Shoulder) + Week,
                                  data = Curr.IR.df, family = "binomial")

head(curr.IR.inj.5.binom.dredge)

curr.IR.inj.5.binom.dredge.1 <- as.data.frame(curr.IR.inj.5.binom.dredge) %>% dplyr::filter(delta <4) %>% dplyr::select(-"(Intercept)", -df, -logLik, -AICc, -delta) %>% 
  summarise_all(funs(1 - sum(is.na(.))/nrow(dplyr::filter(curr.IR.inj.5.binom.dredge, delta < 4)))) %>% tidyr::gather() %>%
  dplyr::rename(Covariate = key, Prop.Mod = value)


curr.IR.inj.5.binom.dredge.1

names(Curr.IR.df.1)

IR.5.inj.binom.data <- with(Curr.IR.df.1, list(ID = as.factor(ID), Curr.IR.5 = Curr.IR.5, Week = Week, 
                                             weight = weight, Knee = as.factor(Knee), Spine = as.factor(Spine), 
                                             Hamstring = as.factor(Hamstring),
                                             N = length(Curr.IR.df.1$Curr.IR.wks), NID = NID))

IR.5.inj.binom.jags <- function() {
  for( i in 1 : N ) {
    Curr.IR.5[i] ~ dbern(p[i])
    logit(p[i]) <- b0 + a[ID[i]] +
      beta1 * Week[i] + beta2 * weight[i] + 
      beta3 * Knee[i] + beta4 * Spine[i] + beta5 * Hamstring[i]
  }     
  
  b0 ~ dnorm( 0 , 1.0E-12 )
  beta1 ~ dnorm( 0 , 1.0E-12 )
  beta2 ~ dnorm( 0 , 1.0E-12 )
  beta3 ~ dnorm( 0 , 1.0E-12 )
  beta4 ~ dnorm( 0 , 1.0E-12 )
  beta5 ~ dnorm( 0 , 1.0E-12 )
  
  sigma_a ~ dunif(0, 100) # standard deviation of random effect (variance between sites)
  tau_a <- 1 / (sigma_a * sigma_a) # convert to precision
  for (j in 1:NID){
    a[j] ~ dnorm(0, tau_a) # random intercept for each site
  }
  sigma ~ dunif(0, 100) # standard deviation of fixed effect (variance within sites)
  tau <- 1 / (sigma * sigma) # convert to precision
  
  logit(predict.knee[1]) <- b0 + beta1 * 8 + beta2 * 240 # Very simple derived parameters for this one, but still worth within jags because it gives us 
  logit(predict.knee[2]) <- b0 + beta3 * 1 + beta1 * 8 + beta2 * 240 # free uncertainty without having to use delta method or bootstrap
  
  logit(predict.spine[1]) <- b0 + beta1 * 8 + beta2 * 240
  logit(predict.spine[2]) <- b0 + beta4 * 1 + beta1 * 8 + beta2 * 240 #
  
  logit(predict.hamstring[1]) <- b0  + beta1 * 8 + beta2 * 240
  logit(predict.hamstring[2]) <- b0 + beta5 * 1 + beta1 * 8 + beta2 * 240 #
  
}

IR.5.inj.binom.params <- c("b0", "beta1","beta2", "beta3", "beta4", "beta5",
                         "predict.knee", "predict.spine", "predict.hamstring",
                         "sigma", "sigma_a")

#fit_IR.5.inj.binom <- jags(data = IR.5.inj.binom.data , inits = NULL, 
               #            parameters.to.save = IR.5.inj.binom.params, model.file = IR.5.inj.binom.jags,
                    #      n.chains = 2, n.iter = 50, n.burnin = 10, n.thin = 10, DIC = F)


############################################## IR.10 INJURY FACTOR #######################################

global.curr.IR.inj.10.binom <- glm(Curr.IR.10 ~ as.factor(Achilles) + as.factor(Knee) + as.factor(Foot) + as.factor(Spine)
                                   + as.factor(Hamstring) + as.factor(Groin) + as.factor(Shoulder) + Week,
                                   data = Curr.IR.df, family = "binomial")

head(curr.IR.inj.10.binom.dredge)

curr.IR.inj.10.binom.dredge.1 <- as.data.frame(curr.IR.inj.10.binom.dredge) %>% dplyr::filter(delta <4) %>% dplyr::select(-"(Intercept)", -df, -logLik, -AICc, -delta) %>% 
  summarise_all(funs(1 - sum(is.na(.))/nrow(dplyr::filter(curr.IR.inj.10.binom.dredge, delta < 4)))) %>% tidyr::gather() %>%
  dplyr::rename(Covariate = key, Prop.Mod = value) 


curr.IR.inj.10.binom.dredge.1

IR.10.inj.binom.data <- with(Curr.IR.df.1, list(ID = as.factor(ID), Curr.IR.10 = Curr.IR.10, Week = Week, 
                                               weight = weight, Knee = as.factor(Knee), Spine = as.factor(Spine), 
                                               Hamstring = as.factor(Hamstring), Achilles = as.factor(Achilles),
                                               N = length(Curr.IR.df.1$Curr.IR.wks), NID = NID))

IR.10.inj.binom.jags <- function() {
  for( i in 1 : N ) {
    Curr.IR.10[i] ~ dbern(p[i])
    logit(p[i]) <- b0 + a[ID[i]] +
      beta1 * Week[i] + beta2 * weight[i] + 
      beta3 * Knee[i] + beta4 * Spine[i] + beta5 * Hamstring[i] + beta6 * Achilles[i]
  }     
  
  b0 ~ dnorm( 0 , 1.0E-12 )
  beta1 ~ dnorm( 0 , 1.0E-12 )
  beta2 ~ dnorm( 0 , 1.0E-12 )
  beta3 ~ dnorm( 0 , 1.0E-12 )
  beta4 ~ dnorm( 0 , 1.0E-12 )
  beta5 ~ dnorm( 0 , 1.0E-12 )
  beta6 ~ dnorm( 0 , 1.0E-12 )
  
  sigma_a ~ dunif(0, 100) # standard deviation of random effect (variance between sites)
  tau_a <- 1 / (sigma_a * sigma_a) # convert to precision
  for (j in 1:NID){
    a[j] ~ dnorm(0, tau_a) # random intercept for each site
  }
  sigma ~ dunif(0, 100) # standard deviation of fixed effect (variance within sites)
  tau <- 1 / (sigma * sigma) # convert to precision
  
  logit(predict.knee[1]) <- b0 + beta1 * 8 + beta2 * 240 # Very simple derived parameters for this one, but still worth within jags because it gives us 
  logit(predict.knee[2]) <- b0 + beta3 * 1 + beta1 * 8 + beta2 * 240 # free uncertainty without having to use delta method or bootstrap
  
  logit(predict.spine[1]) <- b0 + beta1 * 8 + beta2 * 240
  logit(predict.spine[2]) <- b0 + beta4 * 1 + beta1 * 8 + beta2 * 240 #
  
  logit(predict.hamstring[1]) <- b0 + beta1 * 8 + beta2 * 240
  logit(predict.hamstring[2]) <- b0 + beta5 * 1 + beta1 * 8 + beta2 * 240 #
  
  logit(predict.achilles[1]) <- b0+ beta1 * 8 + beta2 * 240
  logit(predict.achilles[2]) <- b0 + beta6 * 1 + beta1 * 8 + beta2 * 240
  
}

IR.10.inj.binom.params <- c("b0", "beta1","beta2", "beta3", "beta4", "beta5", "beta6",
                           "predict.knee", "predict.spine", "predict.hamstring",
                           "sigma", "sigma_a")

#fit_IR.10.inj.binom <- jags(data = IR.10.inj.binom.data , inits = NULL, 
         #                 parameters.to.save = IR.10.inj.binom.params, model.file = IR.10.inj.binom.jags,
                         #  n.chains = 2, n.iter = 50, n.burnin = 10, n.thin = 10, DIC = F)

############ Run all logistic models at once ################################ 

{

start_time <- Sys.time()

##

fit_IR.binom <- jags(data = IR.binom.data , inits = NULL, 
                     parameters.to.save = IR.binom.params, model.file = IR.binom.jags,
                     n.chains = 2, n.iter = 50000, n.burnin = 2000, n.thin = 10, DIC = F)

##

fit_IR.5.binom <- jags(data = IR.5.binom.data , inits = NULL, 
                       parameters.to.save = IR.5.binom.params, model.file = IR.5.binom.jags,
                       n.chains = 2, n.iter = 50000, n.burnin = 2000, n.thin = 10, DIC = F)
##

fit_IR.10.binom <- jags(data = IR.10.binom.data , inits = NULL, 
                        parameters.to.save = IR.10.binom.params, model.file = IR.10.binom.jags,
                        n.chains = 2, n.iter = 50000, n.burnin = 2000, n.thin = 10, DIC = F)


fit_IR.inj.binom <- jags(data = IR.inj.binom.data , inits = NULL, 
                         parameters.to.save = IR.inj.binom.params, model.file = IR.inj.binom.jags,
                         n.chains = 2, n.iter = 50000, n.burnin = 2000, n.thin = 10, DIC = F)
##

fit_IR.5.inj.binom <- jags(data = IR.5.inj.binom.data , inits = NULL, 
                           parameters.to.save = IR.5.inj.binom.params, model.file = IR.5.inj.binom.jags,
                           n.chains = 2, n.iter = 50000, n.burnin = 2000, n.thin = 10, DIC = F)

##

fit_IR.10.inj.binom <- jags(data = IR.10.inj.binom.data , inits = NULL, 
                            parameters.to.save = IR.10.inj.binom.params, model.file = IR.10.inj.binom.jags,
                            n.chains = 2, n.iter = 50000, n.burnin = 2000, n.thin = 10, DIC = F)

### Linear Regression Models

fit_curr.ir.wks <- jags(data = IRweeksdata_s3, inits = curr.iR.init, parameters.to.save = curr.ir.params, model.file = curr.ir.jags,
                        n.chains = 3, n.iter = 50000, n.burnin = 2000,  DIC = F)


fit_snaps <- jags(data = snapsdata_s3, inits = snaps.init, parameters.to.save = snaps.params, model.file = snaps_jags,
                  n.chains = 3, n.iter = 50000, n.burnin = 2000,  DIC = F)

fit_weeks.inj <- jags(data = weeks.inj.data, inits = weeks.inj.init, parameters.to.save = weeks.inj.params, model.file = weeks.inj.jags,
                      n.chains = 3, n.iter = 50000, n.burnin = 2000,  DIC = F)

# Save Logistic Regression outputs 

save(fit_IR.binom, file = "D:/Injury analysis/Player Injuries/Fit.models/fit_IR.binom.RData")

save(fit_IR.5.binom, file = "D:/Injury analysis/Player Injuries/Fit.models/fit_IR.5.binom.RData")

save(fit_IR.10.binom, file = "D:/Injury analysis/Player Injuries/Fit.models/fit_IR.10.binom.RData")

save(fit_IR.inj.binom, file = "D:/Injury analysis/Player Injuries/Fit.models/fit_IR.inj.binom.RData")

save(fit_IR.5.inj.binom, file = "D:/Injury analysis/Player Injuries/Fit.models/fit_IR.5.inj.binom.RData")

save(fit_IR.10.inj.binom, file = "D:/Injury analysis/Player Injuries/Fit.models/fit_IR.10.inj.binom.RData")

# Save linear regression outputs 

save(fit_snaps, file = "D:/Injury analysis/Player Injuries/Fit.models/fit_snaps.RData")

save(fit_curr.ir.wks, file = "D:/Injury analysis/Player Injuries/Fit.models/fit_IR.wks.RData")

save(fit_weeks.inj, file = "D:/Injury analysis/Player Injuries/Fit.models/fit_IR.wks.inj.RData")

end_time <- Sys.time()

total.time <- end_time - start_time

total.time

}

# Cumulative IR through career as covariates 

# Weekly survival IR analysis???
# Multistate model - if was on IR, prob of IR for 5 weeks, ir for 10 weeks, not on IR again

# Big all possible combinations set of models in frequentist, and then run/compare the top couple of models in JAGS