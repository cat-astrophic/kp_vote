# This script runs the data prep and econometric models for a project linking H.Res.211 (105) to voting outcomes in 1998

# Loading libraries

library(sf)
library(AER)
library(dplyr)
library(lmtest)
library(ggplot2)
library(margins)
library(sandwich)
library(stargazer)

# Project directory

direc <- 'D:/kp_vote/'

# Reading in the data sets

df <- read.csv(paste0(direc, 'data/data.csv'))
vote <- read.csv(paste0(direc, 'data/1976-2022-house.csv'))

# Subsetting vote for only general elections

vote <- vote %>% filter(stage == 'GEN')

# Fixing an issue in the voting data

for (i in 1:dim(vote)[1]) {

  if (vote$candidate[i] == 'MARTOM OLAV SABO') {

    vote$candidate[i] <- 'MARTIN OLAV SABO'

  }

}

# Add a column to vote to allow for easy matching of observations

party <- c()

for (i in 1:dim(vote)[1]) {

  if (vote$party[i] == 'REPUBLICAN') {

    party <- c(party, 'R')

  } else if (vote$party[i] == 'DEMOCRAT') {

    party <- c(party, 'D')

  } else if (vote$party[i] == 'DEMOCRATIC-FARMER-LABOR') {

    party <- c(party, 'D')

  } else {

    party <- c(party, 'X')

  }

}

vote$P <- party

# Create a uniqueID column in each data.frame to identify districts

df$ID <- paste0(df$State, df$District)
vote$ID <- paste0(vote$state_po, vote$district)

# Create year-specific data.frames

vote4 <- vote %>% filter(year == 1994)
vote6 <- vote %>% filter(year == 1996)
vote8 <- vote %>% filter(year == 1998)

# Extract winners of elections in 1996 so that the data set is complete (initial pull consisted only of sponsors/co-sponsors of H.Res.211)

winners <- c()
parties <- c()
states <- c()
districts <- c()
votes6 <- c()
vote_share6 <- c()
comp6 <- c()

for (id in unique(vote6$ID)) {

  x <- vote6 %>% filter(ID == id)
  y <- group_by(x, candidate) %>% summarize(votes = sum(candidatevotes))
  z <- y %>% filter(votes == max(y$votes))

  votes6 <- c(votes6, z$votes)
  vote_share6 <- c(vote_share6, z$votes / sum(y$votes))
  comp6 <- c(comp6, dim(y)[1]-1)

  x <- x %>% filter(candidate == z$candidate)

  winners <- c(winners, x$candidate[1])
  states <- c(states, x$state_po[1])
  districts <- c(districts, x$district[1])

  if ('JO ANN EMERSON' %in% y$candidate) {

    parties <- c(parties, 'R')

  } else if ('REPUBLICAN' %in% x$party) {

    parties <- c(parties, 'R')

  } else if ('DEMOCRAT' %in% x$party) {

    parties <- c(parties, 'D')

  } else {

    parties <- c(parties, 'X')

  }

}

data <- as.data.frame(cbind(winners, states, districts, parties, votes6, vote_share6, comp6))
colnames(data) <- c('Name', 'State', 'District', 'Party', 'Votes_96', 'Share_96', 'Competitors_96')
data$ID <- paste0(data$State, data$District)

# Several Texas election were missing this year and only this year (bc ofc), so they are added manually here :: source == https://en.wikipedia.org/wiki/1996_United_States_House_of_Representatives_elections_in_Texas

TX3 <- c('SAM JOHNSON', 'TX', 3, 'R', 102908, 0.5224525, 2, 'TX3')
TX5 <- c('PETE SESSIONS', 'TX', 5, 'R', 80196, 0.5306811, 1, 'TX5')
TX6 <- c('JOE BARTON', 'TX', 6, 'R', 160800, 0.7711638, 3, 'TX6')
TX7 <- c('BILL ARCHER', 'TX', 7, 'R', 152024, 0.8136979, 3, 'TX7')
TX18 <- c('SHEILA JACKSON LEE', 'TX', 18, 'D', 106111, 0.7706627, 4, 'TX18')
TX22 <- c('TOM DELAY', 'TX', 22, 'R', 126056, 0.6810672, 1, 'TX22')
TX24 <- c('MARTIN FROST', 'TX', 24, 'D', 77855, 0.5575528, 3, 'TX24')
TX26 <- c('DICK ARMEY', 'TX', 26, 'R', 163708, 0.7362891, 1, 'TX26')
TX29 <- c('GENE GREEN', 'TX', 29, 'D', 61751, 0.6750809, 2, 'TX29')
TX30 <- c('EDDIE BERNICE JOHNSON', 'TX', 30, 'D', 61725, 0.5458911, 7, 'TX30')

data <- rbind(data, TX3, TX5, TX6, TX7, TX18, TX22, TX24, TX26, TX29, TX30)

# Merge data and df

status <- c()
delay <- c()

for (i in 1:dim(data)[1]) {

  x <- df %>% filter(ID == data$ID[i])

  if (dim(x)[1] > 0) {

    status <- c(status, x$Status)
    delay <- c(delay, x$Delay)

  } else {

    status <- c(status, 'Unaffiliated')
    delay <- c(delay, 0)

  }

}

data$Status <- status
data$Delay <- delay
data$Delayed_CS <- as.integer(data$Delay > 0)

# Check for incumbent status in 1996

flag6 <- c()

for (i in 1:(dim(data)[1]-10)) {

  x <- vote4 %>% filter(ID == data$ID[i])
  y <- group_by(x, candidate) %>% summarize(votes = sum(candidatevotes))
  z <- y %>% filter(votes == max(y$votes))

  if (z$candidate == data$Name[i]) {

    flag6 <- c(flag6, 1)

  } else {

    flag6 <- c(flag6, 0)

  }

}

flag6 <- c(flag6, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1) # Accounting for TX districts
data$Incumbent_96 <- flag6

# See who ran again in 1998 :: source == https://en.wikipedia.org/wiki/1998_United_States_House_of_Representatives_elections#Incumbent_retirements

flag8 <- c('CA3', 'CA34', 'CA36', 'CO2', 'CT1', 'IL9', 'IL19', 'IN9', 'KY6', 'MA8', 'NY7',
           'NY9', 'NC8', 'OH11', 'OR1', 'PA15', 'TX20', 'CA1', 'CO6', 'ID2', 'IL13', 'KY4',
           'MS4', 'NE2', 'NV1', 'NY22', 'NY27', 'OR2', 'PA10', 'SC4', 'WA3', 'WI1', 'WI2', 'CA41')

data$Ran_98 <- 1 - as.integer(data$ID %in% flag8)

# Election data for 1998

votes8 <- c()
vote_share8 <- c()
comp8 <- c()
won8 <- c()

tx_checks <- c('TX3', 'TX5', 'TX6', 'TX7', 'TX18', 'TX22', 'TX24', 'TX26', 'TX29', 'TX30') # All TX people from 96 won reelection in 98

for (id in unique(vote8$ID)) {

  if (id %in% tx_checks) {

    x <- vote8 %>% filter(ID == id)
    y <- group_by(x, candidate) %>% summarize(votes = sum(candidatevotes))
    z <- y %>% filter(votes == max(y$votes))

    votes8 <- c(votes8, z$votes)
    vote_share8 <- c(vote_share8, z$votes / sum(y$votes))
    comp8 <- c(comp8, dim(y)[1]-1)
    won8 <- c(won8, 1)

  } else {

    x <- vote8 %>% filter(ID == id)
    y <- group_by(x, candidate) %>% summarize(votes = sum(candidatevotes))
    z <- y %>% filter(votes == max(y$votes))

    comp8 <- c(comp8, dim(y)[1] - 1)

    name <- data[which(data$ID == id),]$Name

    if (z$candidate == name) {

      won8 <- c(won8, 1)
      votes8 <- c(votes8, z$votes)
      vote_share8 <- c(vote_share8, z$votes / sum(y$votes))

    } else if (name %in% y$candidate) {

      won8 <- c(won8, 0)

      floof <- y %>% filter(candidate == name)

      votes8 <- c(votes8, floof$votes)
      vote_share8 <- c(vote_share8, floof$votes / sum(y$votes))

    } else {

      won8 <- c(won8, 0)
      votes8 <- c(votes8, 0)
      vote_share8 <- c(vote_share8, 0)

    }

  }

}

data$Votes_98 <- votes8
data$Share_98 <- vote_share8
data$Competitors_98 <- comp8
data$Won_98 <- won8

# Final data prep

data$Votes_96 <- as.numeric(data$Votes_96)
data$Share_96 <- as.numeric(data$Share_96)
data$Competitors_96 <- as.numeric(data$Competitors_96)

data$CVS <- data$Share_98 - data$Share_96
data$CC <- data$Competitors_98 - as.integer(data$Competitors_96)
data$KP <- as.integer(data$Status != 'Unaffiliated')

# A first difference model for vote share

cvs1 <- lm(CVS ~ KP, data = data[which(data$Ran_98 == 1),])
cvs2 <- lm(CVS ~ KP + CC, data = data[which(data$Ran_98 == 1),])
cvs3 <- lm(CVS ~ KP + CC + Share_96, data = data[which(data$Ran_98 == 1),])
cvs4 <- lm(CVS ~ KP + CC + Share_96 + factor(Party), data = data[which(data$Ran_98 == 1),])
cvs5 <- lm(CVS ~ KP + CC + Share_96 + factor(Party) + factor(State), data = data[which(data$Ran_98 == 1),])
cvs6 <- lm(CVS ~ KP*factor(Party) + CC + Share_96 + factor(State), data = data[which(data$Ran_98 == 1),])

stargazer(cvs1, cvs2, cvs3, cvs4, cvs5, cvs6, type = 'text', omit = c('State'), omit.stat = c('f', 'ser'))

xcvs1 <- coeftest(cvs1, vcov. = vcovCL(cvs1, type = 'HC0'))
xcvs2 <- coeftest(cvs2, vcov. = vcovCL(cvs2, type = 'HC0'))
xcvs3 <- coeftest(cvs3, vcov. = vcovCL(cvs3, type = 'HC0'))
xcvs4 <- coeftest(cvs4, vcov. = vcovCL(cvs4, type = 'HC0'))
xcvs5 <- coeftest(cvs5, vcov. = vcovCL(cvs5, type = 'HC0'))
xcvs6 <- coeftest(cvs6, vcov. = vcovCL(cvs6, type = 'HC0'))

stargazer(xcvs1, xcvs2, xcvs3, xcvs4, xcvs5, xcvs6, type = 'text', omit = c('State'), omit.stat = c('f', 'ser'))

# Run another set of models predicting who ran in 1998

ran1 <- lm(Ran_98 ~ KP, data = data)
ran2 <- lm(Ran_98 ~ KP + CC, data = data)
ran3 <- lm(Ran_98 ~ KP + CC + Share_96, data = data)
ran4 <- lm(Ran_98 ~ KP + CC + Share_96 + factor(Party), data = data)
ran5 <- lm(Ran_98 ~ KP + CC + Share_96 + factor(Party) + factor(State), data = data)
ran6 <- lm(Ran_98 ~ KP*factor(Party) + CC + Share_96 + factor(State), data = data)

stargazer(ran1, ran2, ran3, ran4, ran5, ran6, type = 'text', omit = c('State'), omit.stat = c('f', 'ser'))

xran1 <- coeftest(ran1, vcov. = vcovCL(ran1, type = 'HC0'))
xran2 <- coeftest(ran2, vcov. = vcovCL(ran2, type = 'HC0'))
xran3 <- coeftest(ran3, vcov. = vcovCL(ran3, type = 'HC0'))
xran4 <- coeftest(ran4, vcov. = vcovCL(ran4, type = 'HC0'))
xran5 <- coeftest(ran5, vcov. = vcovCL(ran5, type = 'HC0'))
xran6 <- coeftest(ran6, vcov. = vcovCL(ran6, type = 'HC0'))

stargazer(xran1, xran2, xran3, xran4, xran5, xran6, type = 'text', omit = c('State'), omit.stat = c('f', 'ser'))

gran1 <- glm(Ran_98 ~ KP, family = binomial(link = logit), data = data)
gran2 <- glm(Ran_98 ~ KP + CC, family = binomial(link = logit), data = data)
gran3 <- glm(Ran_98 ~ KP + CC + Share_96, family = binomial(link = logit), data = data)
gran4 <- glm(Ran_98 ~ KP + CC + Share_96 + factor(Party), family = binomial(link = logit), data = data)
gran5 <- glm(Ran_98 ~ KP + CC + Share_96 + factor(Party) + factor(State), family = binomial(link = logit), data = data)
gran6 <- glm(Ran_98 ~ KP*factor(Party) + CC + Share_96 + factor(State), family = binomial(link = logit), data = data)

stargazer(gran1, gran2, gran3, gran4, gran5, gran6, type = 'text', omit = c('State'), omit.stat = c('f', 'ser'))

xgran1 <- coeftest(gran1, vcov. = vcovCL(ran1, type = 'HC0'))
xgran2 <- coeftest(gran2, vcov. = vcovCL(ran2, type = 'HC0'))
xgran3 <- coeftest(gran3, vcov. = vcovCL(ran3, type = 'HC0'))
xgran4 <- coeftest(gran4, vcov. = vcovCL(ran4, type = 'HC0'))
xgran5 <- coeftest(gran5, vcov. = vcovCL(ran5, type = 'HC0'))
xgran6 <- coeftest(gran6, vcov. = vcovCL(ran6, type = 'HC0'))

stargazer(xgran1, xgran2, xgran3, xgran4, xgran5, xgran6, type = 'text', omit = c('State'), omit.stat = c('f', 'ser'))

# Run another set of models predicting who won conditional on running in 1998

won1 <- lm(Won_98 ~ KP, data = data[which(data$Ran_98 == 1),])
won2 <- lm(Won_98 ~ KP + CC, data = data[which(data$Ran_98 == 1),])
won3 <- lm(Won_98 ~ KP + CC + Share_96, data = data[which(data$Ran_98 == 1),])
won4 <- lm(Won_98 ~ KP + CC + Share_96 + factor(Party), data = data[which(data$Ran_98 == 1),])
won5 <- lm(Won_98 ~ KP + CC + Share_96 + factor(Party) + factor(State), data = data[which(data$Ran_98 == 1),])
won6 <- lm(Won_98 ~ KP*factor(Party) + CC + Share_96 + factor(State), data = data[which(data$Ran_98 == 1),])

stargazer(won1, won2, won3, won4, won5, won6, type = 'text', omit = c('State'), omit.stat = c('f', 'ser'))

xwon1 <- coeftest(won1, vcov. = vcovCL(won1, type = 'HC0'))
xwon2 <- coeftest(won2, vcov. = vcovCL(won2, type = 'HC0'))
xwon3 <- coeftest(won3, vcov. = vcovCL(won3, type = 'HC0'))
xwon4 <- coeftest(won4, vcov. = vcovCL(won4, type = 'HC0'))
xwon5 <- coeftest(won5, vcov. = vcovCL(won5, type = 'HC0'))
xwon6 <- coeftest(won6, vcov. = vcovCL(won6, type = 'HC0'))

stargazer(xwon1, xwon2, xwon3, xwon4, xwon5, xwon6, type = 'text', omit = c('State'), omit.stat = c('f', 'ser'))

gwon1 <- glm(Won_98 ~ KP, family = binomial(link = logit), data = data[which(data$Ran_98 == 1),])
gwon2 <- glm(Won_98 ~ KP + CC, family = binomial(link = logit), data = data[which(data$Ran_98 == 1),])
gwon3 <- glm(Won_98 ~ KP + CC + Share_96, family = binomial(link = logit), data = data[which(data$Ran_98 == 1),])
gwon4 <- glm(Won_98 ~ KP + CC + Share_96 + factor(Party), family = binomial(link = logit), data = data[which(data$Ran_98 == 1),])
gwon5 <- glm(Won_98 ~ KP + CC + Share_96 + factor(Party) + factor(State), family = binomial(link = logit), data = data[which(data$Ran_98 == 1),])
gwon6 <- glm(Won_98 ~ KP*factor(Party) + CC + Share_96 + factor(State), family = binomial(link = logit), data = data[which(data$Ran_98 == 1),])

stargazer(gwon1, gwon2, gwon3, gwon4, gwon5, gwon6, type = 'text', omit = c('State'), omit.stat = c('f', 'ser'))

xgwon1 <- coeftest(gwon1, vcov. = vcovCL(gwon1, type = 'HC0'))
xgwon2 <- coeftest(gwon2, vcov. = vcovCL(gwon2, type = 'HC0'))
xgwon3 <- coeftest(gwon3, vcov. = vcovCL(gwon3, type = 'HC0'))
xgwon4 <- coeftest(gwon4, vcov. = vcovCL(gwon4, type = 'HC0'))
xgwon5 <- coeftest(gwon5, vcov. = vcovCL(gwon5, type = 'HC0'))
xgwon6 <- coeftest(gwon6, vcov. = vcovCL(gwon6, type = 'HC0'))

stargazer(xgwon1, xgwon2, xgwon3, xgwon4, xgwon5, xgwon6, type = 'text', omit = c('State'), omit.stat = c('f', 'ser'))

# Flip this and predict who KP'd

kip1 <- lm(KP ~ Share_96, data = data)
kip2 <- lm(KP ~ Share_96 + CC, data = data)
kip3 <- lm(KP ~ Share_96 + CC + factor(Party), data = data)
kip4 <- lm(KP ~ Share_96 + CC + factor(Party) + factor(State), data = data)

stargazer(kip1, kip2, kip3, kip4, type = 'text', omit = c('State'), omit.stat = c('f', 'ser'))

xkip1 <- coeftest(kip1, vcov. = vcovCL(kip1, type = 'HC0'))
xkip2 <- coeftest(kip2, vcov. = vcovCL(kip2, type = 'HC0'))
xkip3 <- coeftest(kip3, vcov. = vcovCL(kip3, type = 'HC0'))
xkip4 <- coeftest(kip4, vcov. = vcovCL(kip4, type = 'HC0'))

stargazer(xkip1, xkip2, xkip3, xkip4, type = 'text', omit = c('State'), omit.stat = c('f', 'ser'))

gkip1 <- glm(KP ~ Share_96, family = binomial(link = logit), data = data)
gkip2 <- glm(KP ~ Share_96 + CC, family = binomial(link = logit), data = data)
gkip3 <- glm(KP ~ Share_96 + CC + factor(Party), family = binomial(link = logit), data = data)
gkip4 <- glm(KP ~ Share_96 + CC + factor(Party) + factor(State), family = binomial(link = logit), data = data)

stargazer(gkip1, gkip2, gkip3, gkip4, type = 'text', omit = c('State'), omit.stat = c('f', 'ser'))

xgkip1 <- coeftest(gkip1, vcov. = vcovCL(gkip1, type = 'HC0'))
xgkip2 <- coeftest(gkip2, vcov. = vcovCL(gkip2, type = 'HC0'))
xgkip3 <- coeftest(gkip3, vcov. = vcovCL(gkip3, type = 'HC0'))
xgkip4 <- coeftest(gkip4, vcov. = vcovCL(gkip4, type = 'HC0'))

stargazer(xgkip1, xgkip2, xgkip3, xgkip4, type = 'text', omit = c('State'), omit.stat = c('f', 'ser'))

# Creating DID setup

vs <- c(data$Share_96, data$Share_98)
kp <- c(data$KP, data$KP)
post <- c(rep(0,dim(data)[1]), rep(1,dim(data)[1]))
dcs <- c(data$Delayed_CS, data$Delayed_CS)
party <- c(data$Party, data$Party)
state <- c(data$State, data$State)
district <- c(data$District, data$District)
inc <- c(data$Incumbent_96, data$Ran_98)
comps <- c(data$Competitors_96, data$Competitors_98)
drop <- c(data$Ran_98, data$Ran_98)

didata <- as.data.frame(cbind(vs, kp, post, dcs, party, state, district, inc, comps, drop))
colnames(didata) <- c('Share', 'KP', 'Post', 'DCS', 'Party', 'State', 'District', 'Incumbent', 'Competitors', 'Drop')
didata <- didata %>% filter(drop == 1)
didata <- didata[,1:dim(didata)[2]-1] # Good thing this isn't recursive
didata$Unopposed <- as.integer(didata$Competitors == 0)
didata$Competitors <- as.integer(didata$Competitors)
didata$ID <- paste0(didata$State, didata$District)

# Running DID models

did1 <- lm(Share ~ KP*Post, data = didata)
did2 <- lm(Share ~ KP*Post + Incumbent, data = didata)
did3 <- lm(Share ~ KP*Post + Incumbent + Unopposed, data = didata)
did4 <- lm(Share ~ KP*Post + Incumbent + Unopposed + Competitors, data = didata)
did5 <- lm(Share ~ KP*Post + Incumbent + Unopposed + Competitors + factor(Party), data = didata)
did6 <- lm(Share ~ KP*Post + Incumbent + Unopposed + Competitors + factor(Party) + factor(State), data = didata)
did7 <- lm(Share ~ KP*Post + Incumbent + Unopposed + Competitors + factor(Party) + factor(State) + factor(ID), data = didata)

stargazer(did1, did2, did3, did4, did5, did6, did7, type = 'text', omit = c('State', 'ID'), omit.stat = c('f', 'ser'))

xdid1 <- coeftest(did1, vcov. = vcovCL(did1, type = 'HC0'))
xdid2 <- coeftest(did2, vcov. = vcovCL(did2, type = 'HC0'))
xdid3 <- coeftest(did3, vcov. = vcovCL(did3, type = 'HC0'))
xdid4 <- coeftest(did4, vcov. = vcovCL(did4, type = 'HC0'))
xdid5 <- coeftest(did5, vcov. = vcovCL(did5, type = 'HC0'))
xdid6 <- coeftest(did6, vcov. = vcovCL(did6, type = 'HC0'))
xdid7 <- coeftest(did7, vcov. = vcovCL(did7, type = 'HC0'))

stargazer(xdid1, xdid2, xdid3, xdid4, xdid5, xdid6, xdid7, type = 'text', omit = c('State', 'ID'), omit.stat = c('f', 'ser'))

# New data

data55 <- data[which(data$Share_96 < .55),]

didata55 <- didata
didata55$poop <- c(didata55$Share[1:(dim(didata55)[1]/2)], didata55$Share[1:(dim(didata55)[1]/2)])
didata55 <- didata55[which(didata55$poop < .55),]
didata55 <- didata55[,1:dim(didata55)[2]-1]

# Run another set of first difference model for vote share restricted to 55% vs in 1996

ccvs1 <- lm(CVS ~ KP, data = data55[which(data55$Ran_98 == 1),])
ccvs2 <- lm(CVS ~ KP + CC, data = data55[which(data55$Ran_98 == 1),])
ccvs3 <- lm(CVS ~ KP + CC + Share_96, data = data55[which(data55$Ran_98 == 1),])
ccvs4 <- lm(CVS ~ KP + CC + Share_96 + factor(Party), data = data55[which(data55$Ran_98 == 1),])
ccvs5 <- lm(CVS ~ KP + CC + Share_96 + factor(Party) + factor(State), data = data55[which(data55$Ran_98 == 1),])
ccvs6 <- lm(CVS ~ KP*factor(Party) + CC + Share_96 + factor(State), data = data55[which(data55$Ran_98 == 1),])

stargazer(ccvs1, ccvs2, ccvs3, ccvs4, ccvs5, ccvs6, type = 'text', omit = c('State'), omit.stat = c('f', 'ser'))

xccvs1 <- coeftest(ccvs1, vcov. = vcovCL(ccvs1, type = 'HC0'))
xccvs2 <- coeftest(ccvs2, vcov. = vcovCL(ccvs2, type = 'HC0'))
xccvs3 <- coeftest(ccvs3, vcov. = vcovCL(ccvs3, type = 'HC0'))
xccvs4 <- coeftest(ccvs4, vcov. = vcovCL(ccvs4, type = 'HC0'))
xccvs5 <- coeftest(ccvs5, vcov. = vcovCL(ccvs5, type = 'HC0'))
xccvs6 <- coeftest(ccvs6, vcov. = vcovCL(ccvs6, type = 'HC0'))

stargazer(xccvs1, xccvs2, xccvs3, xccvs4, xccvs5, xccvs6, type = 'text', omit = c('State'), omit.stat = c('f', 'ser'))

# Run another set of models predicting who ran in 1998 restricted to 55% vs in 1996

rran1 <- lm(Ran_98 ~ KP, data = data55)
rran2 <- lm(Ran_98 ~ KP + CC, data = data55)
rran3 <- lm(Ran_98 ~ KP + CC + Share_96, data = data55)
rran4 <- lm(Ran_98 ~ KP + CC + Share_96 + factor(Party), data = data55)
rran5 <- lm(Ran_98 ~ KP + CC + Share_96 + factor(Party) + factor(State), data = data55)
rran6 <- lm(Ran_98 ~ KP*factor(Party) + CC + Share_96 + factor(State), data = data55)

stargazer(rran1, rran2, rran3, rran4, rran5, rran6, type = 'text', omit = c('State'), omit.stat = c('f', 'ser'))

xrran1 <- coeftest(rran1, vcov. = vcovCL(rran1, type = 'HC0'))
xrran2 <- coeftest(rran2, vcov. = vcovCL(rran2, type = 'HC0'))
xrran3 <- coeftest(rran3, vcov. = vcovCL(rran3, type = 'HC0'))
xrran4 <- coeftest(rran4, vcov. = vcovCL(rran4, type = 'HC0'))
xrran5 <- coeftest(rran5, vcov. = vcovCL(rran5, type = 'HC0'))
xrran6 <- coeftest(rran6, vcov. = vcovCL(rran6, type = 'HC0'))

stargazer(xrran1, xrran2, xrran3, xrran4, xrran5, xrran6, type = 'text', omit = c('State'), omit.stat = c('f', 'ser'))

grran1 <- glm(Ran_98 ~ KP, family = binomial(link = logit), data = data55)
grran2 <- glm(Ran_98 ~ KP + CC, family = binomial(link = logit), data = data55)
grran3 <- glm(Ran_98 ~ KP + CC + Share_96, family = binomial(link = logit), data = data55)
grran4 <- glm(Ran_98 ~ KP + CC + Share_96 + factor(Party), family = binomial(link = logit), data = data55)
grran5 <- glm(Ran_98 ~ KP + CC + Share_96 + factor(Party) + factor(State), family = binomial(link = logit), data = data55)
grran6 <- glm(Ran_98 ~ KP*factor(Party) + CC + Share_96 + factor(State), family = binomial(link = logit), data = data55)

stargazer(grran1, grran2, grran3, grran4, grran5, grran6, type = 'text', omit = c('State'), omit.stat = c('f', 'ser'))

xgrran1 <- coeftest(grran1, vcov. = vcovCL(rran1, type = 'HC0'))
xgrran2 <- coeftest(grran2, vcov. = vcovCL(rran2, type = 'HC0'))
xgrran3 <- coeftest(grran3, vcov. = vcovCL(rran3, type = 'HC0'))
xgrran4 <- coeftest(grran4, vcov. = vcovCL(rran4, type = 'HC0'))
xgrran5 <- coeftest(grran5, vcov. = vcovCL(rran5, type = 'HC0'))
xgrran6 <- coeftest(grran6, vcov. = vcovCL(rran6, type = 'HC0'))

stargazer(xgrran1, xgrran2, xgrran3, xgrran4, xgrran5, xgrran6, type = 'text', omit = c('State'), omit.stat = c('f', 'ser'))

# Run another set of models predicting who won conditional on running in 1998 restricted to 55% vs in 1996

wwon1 <- lm(Won_98 ~ KP, data = data55[which(data55$Ran_98 == 1),])
wwon2 <- lm(Won_98 ~ KP + CC, data = data55[which(data55$Ran_98 == 1),])
wwon3 <- lm(Won_98 ~ KP + CC + Share_96, data = data55[which(data55$Ran_98 == 1),])
wwon4 <- lm(Won_98 ~ KP + CC + Share_96 + factor(Party), data = data55[which(data55$Ran_98 == 1),])
wwon5 <- lm(Won_98 ~ KP + CC + Share_96 + factor(Party) + factor(State), data = data55[which(data55$Ran_98 == 1),])
wwon6 <- lm(Won_98 ~ KP*factor(Party) + CC + Share_96 + factor(State), data = data55[which(data55$Ran_98 == 1),])

stargazer(wwon1, wwon2, wwon3, wwon4, wwon5, wwon6, type = 'text', omit = c('State'), omit.stat = c('f', 'ser'))

xwwon1 <- coeftest(wwon1, vcov. = vcovCL(wwon1, type = 'HC0'))
xwwon2 <- coeftest(wwon2, vcov. = vcovCL(wwon2, type = 'HC0'))
xwwon3 <- coeftest(wwon3, vcov. = vcovCL(wwon3, type = 'HC0'))
xwwon4 <- coeftest(wwon4, vcov. = vcovCL(wwon4, type = 'HC0'))
xwwon5 <- coeftest(wwon5, vcov. = vcovCL(wwon5, type = 'HC0'))
xwwon6 <- coeftest(wwon6, vcov. = vcovCL(wwon6, type = 'HC0'))

stargazer(xwwon1, xwwon2, xwwon3, xwwon4, xwwon5, xwwon6, type = 'text', omit = c('State'), omit.stat = c('f', 'ser'))

gwwon1 <- glm(Won_98 ~ KP, family = binomial(link = logit), data = data55[which(data55$Ran_98 == 1),])
gwwon2 <- glm(Won_98 ~ KP + CC, family = binomial(link = logit), data = data55[which(data55$Ran_98 == 1),])
gwwon3 <- glm(Won_98 ~ KP + CC + Share_96, family = binomial(link = logit), data = data55[which(data55$Ran_98 == 1),])
gwwon4 <- glm(Won_98 ~ KP + CC + Share_96 + factor(Party), family = binomial(link = logit), data = data55[which(data55$Ran_98 == 1),])
gwwon5 <- glm(Won_98 ~ KP + CC + Share_96 + factor(Party) + factor(State), family = binomial(link = logit), data = data55[which(data55$Ran_98 == 1),])
gwwon6 <- glm(Won_98 ~ KP*factor(Party) + CC + Share_96 + factor(State), family = binomial(link = logit), data = data55[which(data55$Ran_98 == 1),])

stargazer(gwwon1, gwwon2, gwwon3, gwwon4, gwwon5, gwwon6, type = 'text', omit = c('State'), omit.stat = c('f', 'ser'))

xgwwon1 <- coeftest(gwwon1, vcov. = vcovCL(gwwon1, type = 'HC0'))
xgwwon2 <- coeftest(gwwon2, vcov. = vcovCL(gwwon2, type = 'HC0'))
xgwwon3 <- coeftest(gwwon3, vcov. = vcovCL(gwwon3, type = 'HC0'))
xgwwon4 <- coeftest(gwwon4, vcov. = vcovCL(gwwon4, type = 'HC0'))
xgwwon5 <- coeftest(gwwon5, vcov. = vcovCL(gwwon5, type = 'HC0'))
xgwwon6 <- coeftest(gwwon6, vcov. = vcovCL(gwwon6, type = 'HC0'))

stargazer(xgwwon1, xgwwon2, xgwwon3, xgwwon4, xgwwon5, xgwwon6, type = 'text', omit = c('State'), omit.stat = c('f', 'ser'))

# Flip this and predict who KP'd restricted to 55% in 1996

kkip1 <- lm(KP ~ Share_96, data = data55)
kkip2 <- lm(KP ~ Share_96 + CC, data = data55)
kkip3 <- lm(KP ~ Share_96 + CC + factor(Party), data = data55)
kkip4 <- lm(KP ~ Share_96 + CC + factor(Party) + factor(State), data = data55)

stargazer(kkip1, kkip2, kkip3, kkip4, type = 'text', omit = c('State'), omit.stat = c('f', 'ser'))

xkkip1 <- coeftest(kkip1, vcov. = vcovCL(kkip1, type = 'HC0'))
xkkip2 <- coeftest(kkip2, vcov. = vcovCL(kkip2, type = 'HC0'))
xkkip3 <- coeftest(kkip3, vcov. = vcovCL(kkip3, type = 'HC0'))
xkkip4 <- coeftest(kkip4, vcov. = vcovCL(kkip4, type = 'HC0'))

stargazer(xkkip1, xkkip2, xkkip3, xkkip4, type = 'text', omit = c('State'), omit.stat = c('f', 'ser'))

gkkip1 <- glm(KP ~ Share_96, family = binomial(link = logit), data = data55)
gkkip2 <- glm(KP ~ Share_96 + CC, family = binomial(link = logit), data = data55)
gkkip3 <- glm(KP ~ Share_96 + CC + factor(Party), family = binomial(link = logit), data = data55)
gkkip4 <- glm(KP ~ Share_96 + CC + factor(Party) + factor(State), family = binomial(link = logit), data = data55)

stargazer(gkkip1, gkkip2, gkkip3, gkkip4, type = 'text', omit = c('State'), omit.stat = c('f', 'ser'))

xgkkip1 <- coeftest(gkkip1, vcov. = vcovCL(gkkip1, type = 'HC0'))
xgkkip2 <- coeftest(gkkip2, vcov. = vcovCL(gkkip2, type = 'HC0'))
xgkkip3 <- coeftest(gkkip3, vcov. = vcovCL(gkkip3, type = 'HC0'))
xgkkip4 <- coeftest(gkkip4, vcov. = vcovCL(gkkip4, type = 'HC0'))

stargazer(xgkkip1, xgkkip2, xgkkip3, xgkkip4, type = 'text', omit = c('State'), omit.stat = c('f', 'ser'))

# Running DID models restricted to 55% vs in 1996

ddid1 <- lm(Share ~ KP*Post, data = didata55)
ddid2 <- lm(Share ~ KP*Post + Incumbent, data = didata55)
ddid3 <- lm(Share ~ KP*Post + Incumbent + Unopposed, data = didata55)
ddid4 <- lm(Share ~ KP*Post + Incumbent + Unopposed + Competitors, data = didata55)
ddid5 <- lm(Share ~ KP*Post + Incumbent + Unopposed + Competitors + factor(Party), data = didata55)
ddid6 <- lm(Share ~ KP*Post + Incumbent + Unopposed + Competitors + factor(Party) + factor(State), data = didata55)
ddid7 <- lm(Share ~ KP*Post + Incumbent + Unopposed + Competitors + factor(Party) + factor(State) + factor(ID), data = didata55)

stargazer(ddid1, ddid2, ddid3, ddid4, ddid5, ddid6, ddid7, type = 'text', omit = c('State', 'ID'), omit.stat = c('f', 'ser'))

xddid1 <- coeftest(ddid1, vcov. = vcovCL(ddid1, type = 'HC0'))
xddid2 <- coeftest(ddid2, vcov. = vcovCL(ddid2, type = 'HC0'))
xddid3 <- coeftest(ddid3, vcov. = vcovCL(ddid3, type = 'HC0'))
xddid4 <- coeftest(ddid4, vcov. = vcovCL(ddid4, type = 'HC0'))
xddid5 <- coeftest(ddid5, vcov. = vcovCL(ddid5, type = 'HC0'))
xddid6 <- coeftest(ddid6, vcov. = vcovCL(ddid6, type = 'HC0'))
xddid7 <- coeftest(ddid7, vcov. = vcovCL(ddid7, type = 'HC0'))

stargazer(xddid1, xddid2, xddid3, xddid4, xddid5, xddid6, xddid7, type = 'text', omit = c('State', 'ID'), omit.stat = c('f', 'ser'))

# Reading in the district geographies

districts_105 <- read_sf(paste0(direc, 'data/districts105/districtShapes/districts105.shp'))
districts_106 <- read_sf(paste0(direc, 'data/districts106/districtShapes/districts106.shp'))

# Data prep for lotting the districts

tall <- c('Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut',
          'Delaware', 'Florida', 'Georgia', 'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa',
          'Kansas', 'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts', 'Michigan',
          'Minnesota', 'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New Hampshire',
          'New Jersey', 'New Mexico', 'New York', 'North Carolina', 'North Dakota', 'Ohio',
          'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island', 'South Carolina', 'South Dakota',
          'Tennessee', 'Texas', 'Utah', 'Vermont', 'Virginia', 'Washington', 'West Virginia',
          'Wisconsin', 'Wyoming', 'District Of Columbia')

short <- c('AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'FL', 'GA', 'HI', 'ID', 'IL',
           'IN', 'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA', 'MI', 'MN', 'MS', 'MO', 'MT',
           'NE', 'NV', 'NH', 'NJ', 'NM', 'NY', 'NC', 'ND', 'OH', 'OK', 'OR', 'PA', 'RI',
           'SC', 'SD', 'TN', 'TX', 'UT', 'VT', 'VA', 'WA', 'WV', 'WI', 'WY', 'DC')

idcol5 <- c()
idcol6 <- c()

for (i in 1:dim(districts_105)[1]) {

  idcol5 <- c(idcol5, paste0(short[which(tall == districts_105$STATENAME[i])], districts_105$DISTRICT[i]))
  idcol6 <- c(idcol6, paste0(short[which(tall == districts_106$STATENAME[i])], districts_106$DISTRICT[i]))

}

districts_105$IDX <- idcol5
districts_106$IDX <- idcol6

vals5 <- c()
vals6 <- c()

for (i in 1:dim(districts_105)[1]) {

  if (districts_105$STATENAME[i] == 'District Of Columbia') {

    vals5 <- c(vals5, 0)

  } else {

    floof <- data %>% filter(ID == districts_105$IDX[i])
    vals5 <- c(vals5, floof$KP)

  }

}

for (i in 1:dim(districts_106)[1]) {

  if (districts_106$STATENAME[i] == 'District Of Columbia') {

    vals6 <- c(vals6, 0)

  } else {

    floof <- data %>% filter(ID == districts_106$IDX[i])
    vals6 <- c(vals6, floof$KP)

  }

}

districts_105$KP <- vals5
districts_106$KP <- vals6

p5 <- c()
p6 <- c()

for (i in 1:dim(districts_105)[1]) {

  if (districts_105$IDX[i] %in% data$ID) {

    floof <- data %>% filter(ID == districts_105$IDX[i])
    p5 <- c(p5, floof$Party)

  } else {

    p5 <- c(p5, 'D')

  }

}

for (i in 1:dim(districts_106)[1]) {

  if (districts_106$IDX[i] %in% data$ID) {

    floof <- data %>% filter(ID == districts_106$IDX[i])
    p6 <- c(p6, floof$Party)

  } else {

    p6 <- c(p6, 'D')

  }

}

districts_105$Party <- p5
districts_106$Party <- p6

txt5 <- paste0(districts_105$Party, districts_105$KP)
txt6 <- paste0(districts_106$Party, districts_106$KP)

noname5 <- c()
noname6 <- c()

for (i in 1:length(txt5)) {

  if (txt5[i] == 'R1') {

    noname5 <- c(noname5, 'Red')

  } else if (txt5[i] == 'D1') {

    noname5 <- c(noname5, 'Blue')

  } else {

    noname5 <- c(noname5, 'White')

  }

  if (txt6[i] == 'R1') {

    noname6 <- c(noname6, 'Red')

  } else if (txt6[i] == 'D1') {

    noname6 <- c(noname6, 'Blue')

  } else {

    noname6 <- c(noname6, 'White')

  }

}

districts_105$KP2 <- noname5
districts_106$KP2 <- noname6

# Plotting who (co-)sponsored H.Res.211 by district

plot.df <- districts_105 %>% filter(STATENAME != 'Alaska') %>% filter(STATENAME != 'Hawaii')

plot(plot.df$geometry, col = plot.df$KP2)

# Saving all regression results

write.csv(stargazer(cvs1, cvs2, cvs3, cvs4, cvs5, cvs6, type = 'text', omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/cvs_text.txt'), row.names = FALSE)
write.csv(stargazer(xcvs1, xcvs2, xcvs3, xcvs4, xcvs5, xcvs6, type = 'text', omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/cvs_r_text.txt'), row.names = FALSE)
write.csv(stargazer(ccvs1, ccvs2, ccvs3, ccvs4, ccvs5, ccvs6, type = 'text', omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/cvs55_text.txt'), row.names = FALSE)
write.csv(stargazer(xccvs1, xccvs2, xccvs3, xccvs4, xccvs5, xccvs6, type = 'text', omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/cvs55_r_text.txt'), row.names = FALSE)

write.csv(stargazer(cvs1, cvs2, cvs3, cvs4, cvs5, cvs6, omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/cvs.txt'), row.names = FALSE)
write.csv(stargazer(xcvs1, xcvs2, xcvs3, xcvs4, xcvs5, xcvs6, omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/cvs_r.txt'), row.names = FALSE)
write.csv(stargazer(ccvs1, ccvs2, ccvs3, ccvs4, ccvs5, ccvs6, omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/cvs55.txt'), row.names = FALSE)
write.csv(stargazer(xccvs1, xccvs2, xccvs3, xccvs4, xccvs5, xccvs6, omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/cvs55_r.txt'), row.names = FALSE)

write.csv(stargazer(ran1, ran2, ran3, ran4, ran5, ran6, type = 'text', omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/ran_text.txt'), row.names = FALSE)
write.csv(stargazer(xran1, xran2, xran3, xran4, xran5, xran6, type = 'text', omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/ran_r_text.txt'), row.names = FALSE)
write.csv(stargazer(rran1, rran2, rran3, rran4, rran5, rran6, type = 'text', omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/ran55_text.txt'), row.names = FALSE)
write.csv(stargazer(xrran1, xrran2, xrran3, xrran4, xrran5, xrran6, type = 'text', omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/ran55_r_text.txt'), row.names = FALSE)

write.csv(stargazer(gran1, gran2, gran3, gran4, gran5, gran6, type = 'text', omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/ran_glm_text.txt'), row.names = FALSE)
write.csv(stargazer(xgran1, xgran2, xgran3, xgran4, xgran5, xgran6, type = 'text', omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/ran_r_glm_text.txt'), row.names = FALSE)
write.csv(stargazer(grran1, grran2, grran3, grran4, grran5, grran6, type = 'text', omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/ran55_glm_text.txt'), row.names = FALSE)
write.csv(stargazer(xgrran1, xgrran2, xgrran3, xgrran4, xgrran5, xgrran6, type = 'text', omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/ran55_r_glm_text.txt'), row.names = FALSE)

write.csv(stargazer(ran1, ran2, ran3, ran4, ran5, ran6, omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/ran.txt'), row.names = FALSE)
write.csv(stargazer(xran1, xran2, xran3, xran4, xran5, xran6, omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/ran_r.txt'), row.names = FALSE)
write.csv(stargazer(rran1, rran2, rran3, rran4, rran5, rran6, omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/ran55.txt'), row.names = FALSE)
write.csv(stargazer(xrran1, xrran2, xrran3, xrran4, xrran5, xrran6, omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/ran55_r.txt'), row.names = FALSE)

write.csv(stargazer(gran1, gran2, gran3, gran4, gran5, gran6, omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/ran_glm.txt'), row.names = FALSE)
write.csv(stargazer(xgran1, xgran2, xgran3, xgran4, xgran5, xgran6, omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/ran_r_glm.txt'), row.names = FALSE)
write.csv(stargazer(grran1, grran2, grran3, grran4, grran5, grran6, omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/ran55_glm.txt'), row.names = FALSE)
write.csv(stargazer(xgrran1, xgrran2, xgrran3, xgrran4, xgrran5, xgrran6, omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/ran55_r_glm.txt'), row.names = FALSE)

write.csv(stargazer(won1, won2, won3, won4, won5, won6, type = 'text', omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/won_text.txt'), row.names = FALSE)
write.csv(stargazer(xwon1, xwon2, xwon3, xwon4, xwon5, xwon6, type = 'text', omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/won_r_text.txt'), row.names = FALSE)
write.csv(stargazer(wwon1, wwon2, wwon3, wwon4, wwon5, wwon6, type = 'text', omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/won55_text.txt'), row.names = FALSE)
write.csv(stargazer(xwwon1, xwwon2, xwwon3, xwwon4, xwwon5, xwwon6, type = 'text', omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/won55_r_text.txt'), row.names = FALSE)

write.csv(stargazer(gwon1, gwon2, gwon3, gwon4, gwon5, gwon6, type = 'text', omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/won_glm_text.txt'), row.names = FALSE)
write.csv(stargazer(xgwon1, xgwon2, xgwon3, xgwon4, xgwon5, xgwon6, type = 'text', omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/won_r_glm_text.txt'), row.names = FALSE)
write.csv(stargazer(gwwon1, gwwon2, gwwon3, gwwon4, gwwon5, gwwon6, type = 'text', omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/won55_glm_text.txt'), row.names = FALSE)
write.csv(stargazer(xgwwon1, xgwwon2, xgwwon3, xgwwon4, xgwwon5, xgwwon6, type = 'text', omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/won55_r_glm_text.txt'), row.names = FALSE)

write.csv(stargazer(won1, won2, won3, won4, won5, won6, omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/won.txt'), row.names = FALSE)
write.csv(stargazer(xwon1, xwon2, xwon3, xwon4, xwon5, xwon6, omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/won_r.txt'), row.names = FALSE)
write.csv(stargazer(wwon1, wwon2, wwon3, wwon4, wwon5, wwon6, omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/won55.txt'), row.names = FALSE)
write.csv(stargazer(xwwon1, xwwon2, xwwon3, xwwon4, xwwon5, xwwon6, omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/won55_r.txt'), row.names = FALSE)

write.csv(stargazer(gwon1, gwon2, gwon3, gwon4, gwon5, gwon6, omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/won_glm.txt'), row.names = FALSE)
write.csv(stargazer(xgwon1, xgwon2, xgwon3, xgwon4, xgwon5, xgwon6, omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/won_r_glm.txt'), row.names = FALSE)
write.csv(stargazer(gwwon1, gwwon2, gwwon3, gwwon4, gwwon5, gwwon6, omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/won55_glm.txt'), row.names = FALSE)
write.csv(stargazer(xgwwon1, xgwwon2, xgwwon3, xgwwon4, xgwwon5, xgwwon6, omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/won55_r_glm.txt'), row.names = FALSE)

write.csv(stargazer(kip1, kip2, kip3, kip4, type = 'text', omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/kip_text.txt'), row.names = FALSE)
write.csv(stargazer(xkip1, xkip2, xkip3, xkip4, type = 'text', omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/kip_r_text.txt'), row.names = FALSE)
write.csv(stargazer(kkip1, kkip2, kkip3, kkip4, type = 'text', omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/kip55_text.txt'), row.names = FALSE)
write.csv(stargazer(xkkip1, xkkip2, xkkip3, xkkip4, type = 'text', omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/kip55_r_text.txt'), row.names = FALSE)

write.csv(stargazer(gkip1, gkip2, gkip3, gkip4, type = 'text', omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/kip_glm_text.txt'), row.names = FALSE)
write.csv(stargazer(xgkip1, xgkip2, xgkip3, xgkip4, type = 'text', omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/kip_r_glm_text.txt'), row.names = FALSE)
write.csv(stargazer(gkkip1, gkkip2, gkkip3, gkkip4, type = 'text', omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/kip55_glm_text.txt'), row.names = FALSE)
write.csv(stargazer(xgkkip1, xgkkip2, xgkkip3, xgkkip4, type = 'text', omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/kip55_r_glm_text.txt'), row.names = FALSE)

write.csv(stargazer(kip1, kip2, kip3, kip4, omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/kip.txt'), row.names = FALSE)
write.csv(stargazer(xkip1, xkip2, xkip3, xkip4, omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/kip_r.txt'), row.names = FALSE)
write.csv(stargazer(kkip1, kkip2, kkip3, kkip4, omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/kip55.txt'), row.names = FALSE)
write.csv(stargazer(xkkip1, xkkip2, xkkip3, xkkip4, omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/kip55_r.txt'), row.names = FALSE)

write.csv(stargazer(gkip1, gkip2, gkip3, gkip4, omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/kip_glm.txt'), row.names = FALSE)
write.csv(stargazer(xgkip1, xgkip2, xgkip3, xgkip4, omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/kip_r_glm.txt'), row.names = FALSE)
write.csv(stargazer(gkkip1, gkkip2, gkkip3, gkkip4, omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/kip55_glm.txt'), row.names = FALSE)
write.csv(stargazer(xgkkip1, xgkkip2, xgkkip3, xgkkip4, omit = c('State'), omit.stat = c('ser')), paste0(direc, 'results/kip55_r_glm.txt'), row.names = FALSE)

write.csv(stargazer(did1, did2, did3, did4, did5, did6, did7, type = 'text', omit = c('State', 'ID'), omit.stat = c('ser')), paste0(direc, 'results/did_text.txt'), row.names = FALSE)
write.csv(stargazer(xdid1, xdid2, xdid3, xdid4, xdid5, xdid6, xdid7, type = 'text', omit = c('State', 'ID'), omit.stat = c('ser')), paste0(direc, 'results/did_r_text.txt'), row.names = FALSE)
write.csv(stargazer(ddid1, ddid2, ddid3, ddid4, ddid5, ddid6, ddid7, type = 'text', omit = c('State', 'ID'), omit.stat = c('ser')), paste0(direc, 'results/did55_text.txt'), row.names = FALSE)
write.csv(stargazer(xddid1, xddid2, xddid3, xddid4, xddid5, xddid6, xddid7, type = 'text', omit = c('State', 'ID'), omit.stat = c('ser')), paste0(direc, 'results/did55_r_text.txt'), row.names = FALSE)

write.csv(stargazer(did1, did2, did3, did4, did5, did6, did7, omit = c('State', 'ID'), omit.stat = c('ser')), paste0(direc, 'results/did.txt'), row.names = FALSE)
write.csv(stargazer(xdid1, xdid2, xdid3, xdid4, xdid5, xdid6, xdid7, omit = c('State', 'ID'), omit.stat = c('ser')), paste0(direc, 'results/did_r.txt'), row.names = FALSE)
write.csv(stargazer(ddid1, ddid2, ddid3, ddid4, ddid5, ddid6, ddid7, omit = c('State', 'ID'), omit.stat = c('ser')), paste0(direc, 'results/did55.txt'), row.names = FALSE)
write.csv(stargazer(xddid1, xddid2, xddid3, xddid4, xddid5, xddid6, xddid7, omit = c('State', 'ID'), omit.stat = c('ser')), paste0(direc, 'results/did55_r.txt'), row.names = FALSE)



# Reading in weather data as a potential instrument

noaa <- read.csv('D:/NOAA/us_data/NOAA_1997.csv')

# Updating noaa

noaa <- noaa %>% filter(is.na(LATITUDE) == FALSE) %>% filter(is.na(LONGITUDE) == FALSE)
noaa <- st_as_sf(noaa, coords = c('LONGITUDE', 'LATITUDE'), crs = st_crs(districts_105))

farms <- st_intersects(noaa, districts_105)

bloop <- c()

for (i in 1:dim(noaa)[1]) {

  bloop <- c(bloop, max(-1,farms[i][[1]]))

}

snarf <- c()

for (i in 1:length(bloop)) {

  if (bloop[i] != -1) {

    snarf <- c(snarf, data$ID[bloop[i]])

  } else {

    snarf <- c(snarf, 'SHITFUCK')

  }

}

noaa$DISSY <- snarf

# Creating IV data

noaa_moaa <- group_by(noaa, DISSY) %>% summarize(max = mean(MAX), rain = mean(PRCP))

maxx <- c()
prcpx <- c()

for (i in 1:dim(data)[1]) {

  floof <- noaa_moaa %>% filter(DISSY == data$ID[i])

  if (dim(floof)[1] > 0) {

    maxx <- c(maxx, floof$max)
    prcpx <- c(prcpx, floof$rain)

  } else {

    maxx <- c(maxx, NA)
    prcpx <- c(prcpx, NA)

  }

}

maxdid <- c()
prcpdid <- c()

for (i in 1:dim(didata)[1]) {

  floof <- noaa_moaa %>% filter(DISSY == didata$ID[i])

  if (dim(floof)[1] > 0) {

    maxdid <- c(maxdid, floof$max)
    prcpdid <- c(prcpdid, floof$rain)

  } else {

    maxdid <- c(maxdid, NA)
    prcpdid <- c(prcpdid, NA)

  }

}

data$IV1 <- maxx
data$IV2 <- prcpx

didata$IV1 <- maxdid
didata$IV2 <- prcpdid

data$IV3 <- as.integer(data$Share_96 < .55)

# Running IV regressions

# A first difference model for vote share

icvs1 <- ivreg(CVS ~ KP | . - KP + IV3, data = data[which(data$Ran_98 == 1),])
icvs2 <- ivreg(CVS ~ KP + CC | . - KP + IV3, data = data[which(data$Ran_98 == 1),])
icvs3 <- ivreg(CVS ~ KP + CC + Share_96 | . - KP + IV3, data = data[which(data$Ran_98 == 1),])
icvs4 <- ivreg(CVS ~ KP + CC + Share_96 + factor(Party) | . - KP + IV3, data = data[which(data$Ran_98 == 1),])
icvs5 <- ivreg(CVS ~ KP + CC + Share_96 + factor(Party) + factor(State) | . - KP + IV3, data = data[which(data$Ran_98 == 1),])
icvs6 <- ivreg(CVS ~ KP*factor(Party) + CC + Share_96 + factor(State) | . - KP + IV3, data = data[which(data$Ran_98 == 1),])

stargazer(icvs1, icvs2, icvs3, icvs4, icvs5, icvs6, type = 'text', omit = c('State'), omit.stat = c('f', 'ser'))

ixcvs1 <- coeftest(icvs1, vcov. = vcovCL(icvs1, type = 'HC0'))
ixcvs2 <- coeftest(icvs2, vcov. = vcovCL(icvs2, type = 'HC0'))
ixcvs3 <- coeftest(icvs3, vcov. = vcovCL(icvs3, type = 'HC0'))
ixcvs4 <- coeftest(icvs4, vcov. = vcovCL(icvs4, type = 'HC0'))
ixcvs5 <- coeftest(icvs5, vcov. = vcovCL(icvs5, type = 'HC0'))
ixcvs6 <- coeftest(icvs6, vcov. = vcovCL(icvs6, type = 'HC0'))

stargazer(ixcvs1, ixcvs2, ixcvs3, ixcvs4, ixcvs5, ixcvs6, type = 'text', omit = c('State'), omit.stat = c('f', 'ser'))









