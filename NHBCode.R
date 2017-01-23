
## Code to accompany ``Social Support Networks and Religiosity in Rural South India'' ##
## Includes code for generating the social support networks, running the ERGMs, and visualizing the results
## The required files that accompany the code are not included here. Those interested in accessing the required files should contact epower@santafe.edu


require(igraph)

## Read in the files that include details of each individual, including age, gender, caste & religion, years of education, household wealth, religious participation, etc.
Ten <- read.csv("TenMetadata.csv",header=TRUE)
Ala <- read.csv("AlaMetadata.csv",header=TRUE)


##############
## Tenpatti ##
##############

## Read in the edge list, which includes columns for Ego, Alter, and then each of the 12 support types
elTen <- read.csv("TenSocialEdgeList.csv")

elTen$Vouch <- elTen$Work + elTen$Defend + elTen$Position
elTen$Emo <- elTen$Talk + elTen$Close
elTen$Behav <- elTen$Errand + elTen$Borrow + elTen$Babysit
elTen$Finan <- elTen$Cash + elTen$Loan
elTen$Adv <- elTen$ImpIss + elTen$Advice

## generate a network from the edgelist, but this lacks the associated metadata
snaTen <- graph.data.frame(elTen)
V(snaTen)$label <- V(snaTen)$name
IndivIDTen <- V(snaTen)$label
IndivIDTen <- data.frame(IndivIDTen)

## append the individual metadata to each individual included in the network
attTen <- merge(IndivIDTen,Ten,by.x="IndivIDTen",by.y="IndivID",sort=FALSE,all.x=TRUE)
colnames(attTen)[1] <- "IndivID"

## generate the full network
snaTenSupFull <- graph.data.frame(d = elTen, vertices = attTen, directed=TRUE)

## reduce the network down to include only those who completed the survey in the village (many people outside the village are named, but the analyses here require that each individual can have both incoming and outgoing ties)
snaTenSup <- delete.vertices(snaTenSupFull,V(snaTenSupFull)[degree(snaTenSupFull,mode="out")==0])

## networks of each support type
snaTenVouch <- delete.edges(snaTenSup, E(snaTenSup)[get.edge.attribute(snaTenSup, name = "Vouch") == 0])
snaTenEmo <- delete.edges(snaTenSup, E(snaTenSup)[get.edge.attribute(snaTenSup, name = "Emo") == 0])
snaTenBehav <- delete.edges(snaTenSup, E(snaTenSup)[get.edge.attribute(snaTenSup, name = "Behav") == 0])
snaTenFinan <- delete.edges(snaTenSup, E(snaTenSup)[get.edge.attribute(snaTenSup, name = "Finan") == 0])
snaTenAdv <- delete.edges(snaTenSup, E(snaTenSup)[get.edge.attribute(snaTenSup, name = "Adv") == 0])

## reciprocal network
E(snaTenSup)$recip <- is.mutual(snaTenSup)
snaTenRecip <- delete.edges(snaTenSup, E(snaTenSup)[get.edge.attribute(snaTenSup, name = "recip") == "FALSE"])
snaTenRecip <- as.undirected(snaTenRecip)


#################
## Azhagapuram ##
#################

elAla <- read.csv("AlaSocialEdgeList.csv")

elAla$Vouch <- elAla$Work + elAla$Defend + elAla$Position
elAla$Emo <- elAla$Talk + elAla$Close
elAla$Behav <- elAla$Errand + elAla$Borrow + elAla$Babysit
elAla$Finan <- elAla$Cash + elAla$Loan
elAla$Adv <- elAla$ImpIss + elAla$Advice

snaAla <- graph.data.frame(elAla)
V(snaAla)$label <- V(snaAla)$name
IndivIDAla <- V(snaAla)$label
IndivIDAla <- data.frame(IndivIDAla)

attAla <- merge(IndivIDAla,Ala,by.x="IndivIDAla",by.y="IndivID",sort=FALSE,all.x=TRUE)
colnames(attAla)[1] <- "IndivID"

snaAlaSupFull <- graph.data.frame(d = elAla, vertices = attAla, directed=TRUE)

## reduce the network down to include only those who completed the survey in the village
snaAlaSup <- delete.vertices(snaAlaSupFull,V(snaAlaSupFull)[degree(snaAlaSupFull,mode="out")==0])

## networks of each support type
snaAlaVouch <- delete.edges(snaAlaSup, E(snaAlaSup)[get.edge.attribute(snaAlaSup, name = "Vouch") == 0])
snaAlaEmo <- delete.edges(snaAlaSup, E(snaAlaSup)[get.edge.attribute(snaAlaSup, name = "Emo") == 0])
snaAlaBehav <- delete.edges(snaAlaSup, E(snaAlaSup)[get.edge.attribute(snaAlaSup, name = "Behav") == 0])
snaAlaFinan <- delete.edges(snaAlaSup, E(snaAlaSup)[get.edge.attribute(snaAlaSup, name = "Finan") == 0])
snaAlaAdv <- delete.edges(snaAlaSup, E(snaAlaSup)[get.edge.attribute(snaAlaSup, name = "Adv") == 0])

## reciprocal networks
E(snaAlaSup)$recip <- is.mutual(snaAlaSup)
snaAlaRecip <- delete.edges(snaAlaSup, E(snaAlaSup)[get.edge.attribute(snaAlaSup, name = "recip") == "FALSE"])
snaAlaRecip <- as.undirected(snaAlaRecip)


######################
## Kinship Networks ##
######################

## This file is an edgelist, including a line for each pair of individuals who are related to one another through a close kinship tie, here including: spousal, parent/child, sibling
closekinEL <- read.csv("CloseKinEdgeList.csv",header=TRUE)

require(car)

## This code generates a full list of all of the dyads in the support network, and then populates it with the data from the kinship edgelist, recording whether each dyad is related or not.
kinship<-function(g){
  kin <- V(g)$name
  kinel <- expand.grid(kin,kin)
  colnames(kinel) <- c("Ego","Alter")
  kinel$Edge <- paste(kinel[,1],kinel[,2],sep="_")
  kin1 <- merge(kinel,closekinEL[,c(3,4)],by="Edge",all.x=TRUE)
  kin1$Kin <- recode(kin1$Kin,"NA=0")
  kin1$Edge<- NULL
  knet <- graph.data.frame(kin1)
  return(knet)
}

knetTen<-kinship(snaTenSup)
knetAla<-kinship(snaAlaSup)


########################
## Household Distance ##
########################

## These two files record the distance (as the bird flies) between every pair of households in the two villages
TenHouseholdDist<-read.csv("TenHouseholdDist.csv",as.is=TRUE)
AlaHouseholdDist<-read.csv("AlaHouseholdDist.csv",as.is=TRUE)

## Extract the household ID for each individual
Teni<-V(snaTenSup)$name
Tenh<-V(snaTenSup)$HouseID
Tenv<-cbind(Teni,Tenh)
Tenv<-as.data.frame(Tenv)
colnames(Tenv)<-c("IndivID","HouseID")
Tenv<-Tenv[with(Tenv, order(IndivID)), ]

## Make an empty matrix with names as the House ID
Tendistancemat <-matrix(nrow=length(Tenv$IndivID),ncol=length(Tenv$IndivID),dimnames=list(as.vector(Tenv$HouseID),as.vector(Tenv$HouseID)))

## Populate the matrix with the values from the Distance file. This will take a moment.
for(i in 1:nrow(TenHouseholdDist)){Tendistancemat[rownames(Tendistancemat)==TenHouseholdDist[i,1],colnames(Tendistancemat)==TenHouseholdDist[i,2]] <- as.numeric(TenHouseholdDist[i,3])}

## Rename points so that they now correspond properly to IndivID not House ID
dimnames(Tendistancemat)<-list(as.vector(Tenv$IndivID),as.vector(Tenv$IndivID))

## Replace NAs, meaning individuals who are in the same house, with 0s
Tendistancemat[is.na(Tendistancemat)]<-0


## Same for Alakapuram
Alai<-V(snaAlaSup)$name
Alah<-V(snaAlaSup)$HouseID
Alav<-cbind(Alai,Alah)
Alav<-as.data.frame(Alav)
colnames(Alav)<-c("IndivID","HouseID")
Alav<-Alav[with(Alav, order(IndivID)), ]

Aladistancemat <-matrix(nrow=length(Alav$IndivID),ncol=length(Alav$IndivID),dimnames=list(as.vector(Alav$HouseID),as.vector(Alav$HouseID)))
for(i in 1:nrow(AlaHouseholdDist)){Aladistancemat[rownames(Aladistancemat)==AlaHouseholdDist[i,1],colnames(Aladistancemat)==AlaHouseholdDist[i,2]] <- as.numeric(AlaHouseholdDist[i,3])}
dimnames(Aladistancemat)<-list(as.vector(Alav$IndivID),as.vector(Alav$IndivID))
Aladistancemat[is.na(Aladistancemat)]<-0


########################
## PORTING TO STATNET ##
########################

detach(package:igraph)
require(intergraph)
require(network)
require(ergm)

Net_snaTenSup <- asNetwork(snaTenSup)
Net_kinTen <- asNetwork(knetTen)

Net_snaTenRecip <- asNetwork(snaTenRecip)
Net_snaTenBehav <- asNetwork(snaTenBehav)
Net_snaTenFinan <- asNetwork(snaTenFinan)
Net_snaTenVouch <- asNetwork(snaTenVouch)
Net_snaTenEmo <- asNetwork(snaTenEmo)
Net_snaTenAdv <- asNetwork(snaTenAdv)

Net_snaAlaSup <- asNetwork(snaAlaSup)
Net_kinAla <- asNetwork(knetAla)

Net_snaAlaRecip <- asNetwork(snaAlaRecip)
Net_snaAlaBehav <- asNetwork(snaAlaBehav)
Net_snaAlaFinan <- asNetwork(snaAlaFinan)
Net_snaAlaVouch <- asNetwork(snaAlaVouch)
Net_snaAlaEmo <- asNetwork(snaAlaEmo)
Net_snaAlaAdv <- asNetwork(snaAlaAdv)

#########################################################################
#########################################################################
########## FROM HERE RUNNING FULL MODELS THAT TAKE A LONG TIME ##########
#########################################################################
#########################################################################


## OVERALL SUPPORT ERGMs
## Note that the GWESP alpha values used here are the ones that result in the lowest AIC & BIC values. To establish these values, I ran models starting with GWESP alpha = 0.1 ((gwesp(alpha=0.1,T)) and increased by steps of 0.1 until AIC and BIC values reached their lowest point.
## Models were also evaluated by looking at the MCMC output (mcmc.diagnostics(modelname)) and goodness of fit (gof(modelname ~idegree + odegree + esp + distance)) to ensure that the generated networks are reasonable approximations of the observed network

modelTenSup <- ergm(Net_snaTenSup ~ edges + nodecov("Age") + nodematch("Gender") + nodeifactor("Gender") + edgecov(Net_kinTen,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodeicov("HouseholdWealthCalc") + absdiff("EduYears") + nodeifactor("EverCommMember") + edgecov(Tendistancemat/10) + nodeifactor("WeeklyPlusWorship") + nodeifactor("Possession") + nodeicov("WeightedReligPart") + mutual + gwesp(0.5,T), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
summary(modelTenSup)

modelTenSup.WithGenScaled <- ergm(Net_snaTenSup ~ edges + nodecov("Age") + nodematch("Gender") + nodeifactor("Gender") + edgecov(Net_kinTen,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodeicov("HouseholdWealthCalc") + absdiff("EduYears") + nodeifactor("EverCommMember") + edgecov(Tendistancemat/10) + nodeifactor("WeeklyPlusWorship") + nodeifactor("Possession") + nodeicov("WeightedReligPart") + nodeicov("GenerousScaled") + mutual + gwesp(0.5,T), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
summary(modelTenSup.WithGenScaled)

modelAlaSup <- ergm(Net_snaAlaSup ~ edges + nodecov("Age") + nodematch("Gender") + nodeifactor("Gender") + edgecov(Net_kinAla,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodeicov("HouseholdWealthCalc") + absdiff("EduYears") + nodeifactor("EverCommMember") + edgecov(Aladistancemat/10) + nodeifactor("WeeklyPlusWorship") + nodeifactor("Possession") + nodeicov("WeightedReligPart") + mutual + gwesp(0.6,T), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
summary(modelAlaSup)

modelAlaSup.WithGenScaled <- ergm(Net_snaAlaSup ~ edges + nodecov("Age") + nodematch("Gender") + nodeifactor("Gender") + edgecov(Net_kinAla,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodeicov("HouseholdWealthCalc") + absdiff("EduYears") + nodeifactor("EverCommMember") + edgecov(Aladistancemat/10) + nodeifactor("WeeklyPlusWorship") + nodeifactor("Possession") + nodeicov("WeightedReligPart") + nodeicov("GenerousScaled") + mutual + gwesp(0.6,T), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
summary(modelAlaSup.WithGenScaled)

## EACH SUPPORT TYPE ERGMs

# Tenpatti

modelTenEmo <- ergm(Net_snaTenEmo ~ edges + nodecov("Age") + nodematch("Gender") + nodeifactor("Gender") + edgecov(Net_kinTen,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodeicov("HouseholdWealthCalc") + absdiff("EduYears") + nodeifactor("EverCommMember") + edgecov(Tendistancemat/10) + nodeifactor("WeeklyPlusWorship") + nodeifactor("Possession") + nodeicov("WeightedReligPart") + mutual + gwesp(0.2,T) + gwdsp(0.2,T) + idegree(0) + odegree(0), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
summary(modelTenEmo)

modelTenEmo.WithGenScaled <- ergm(Net_snaTenEmo ~ edges + nodecov("Age") + nodematch("Gender") + nodeifactor("Gender") + edgecov(Net_kinTen,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodeicov("HouseholdWealthCalc") + absdiff("EduYears") + nodeifactor("EverCommMember") + edgecov(Tendistancemat/10) + nodeifactor("WeeklyPlusWorship") + nodeifactor("Possession") + nodeicov("WeightedReligPart") + nodeicov("GenerousScaled") + mutual + gwesp(0.2,T) + gwdsp(0.2,T) + idegree(0) + odegree(0), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
summary(modelTenEmoWithGenScaled)

modelTenBehav <- ergm(Net_snaTenBehav ~ edges + nodecov("Age") + nodematch("Gender") + nodeifactor("Gender") + edgecov(Net_kinTen,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodeicov("HouseholdWealthCalc") + absdiff("EduYears") + nodeifactor("EverCommMember") + edgecov(Tendistancemat/10) + nodeifactor("WeeklyPlusWorship") + nodeifactor("Possession") + nodeicov("WeightedReligPart") + mutual + gwesp(0.1,T) + gwdsp(0.1,T) + idegree(0) + odegree(0), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
summary(modelTenBehav)

modelTenBehav.WithGenScaled <- ergm(Net_snaTenBehav ~ edges + nodecov("Age") + nodematch("Gender") + nodeifactor("Gender") + edgecov(Net_kinTen,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodeicov("HouseholdWealthCalc") + absdiff("EduYears") + nodeifactor("EverCommMember") + edgecov(Tendistancemat/10) + nodeifactor("WeeklyPlusWorship") + nodeifactor("Possession") + nodeicov("WeightedReligPart") + nodeicov("GenerousScaled") + mutual + gwesp(0.1,T) + gwdsp(0.1,T) + idegree(0) + odegree(0), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
summary(modelTenBehav.WithGenScaled)

modelTenFinan <- ergm(Net_snaTenFinan ~ edges + nodecov("Age") + nodematch("Gender") + nodeifactor("Gender") + edgecov(Net_kinTen,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodeicov("HouseholdWealthCalc") + absdiff("EduYears") + nodeifactor("EverCommMember") + edgecov(Tendistancemat/10) + nodeifactor("WeeklyPlusWorship") + nodeifactor("Possession") + nodeicov("WeightedReligPart") + mutual + gwesp(0.1,T) + gwdsp(0.1,T) + idegree(0) + odegree(0), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
summary(modelTenFinan)

modelTenFinan.WithGenScaled <- ergm(Net_snaTenFinan ~ edges + nodecov("Age") + nodematch("Gender") + nodeifactor("Gender") + edgecov(Net_kinTen,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodeicov("HouseholdWealthCalc") + absdiff("EduYears") + nodeifactor("EverCommMember") + edgecov(Tendistancemat/10) + nodeifactor("WeeklyPlusWorship") + nodeifactor("Possession") + nodeicov("WeightedReligPart") + nodeicov("GenerousScaled") + mutual + gwesp(0.1,T) + gwdsp(0.1,T) + idegree(0) + odegree(0), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
summary(modelTenFinan.WithGenScaled)

modelTenVouch <- ergm(Net_snaTenVouch ~ edges + nodecov("Age") + nodematch("Gender") + nodeifactor("Gender") + edgecov(Net_kinTen,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodeicov("HouseholdWealthCalc") + absdiff("EduYears") + nodeifactor("EverCommMember") + edgecov(Tendistancemat/10) + nodeifactor("WeeklyPlusWorship") + nodeifactor("Possession") + nodeicov("WeightedReligPart") + mutual + gwesp(0.4,T) + gwdsp(0.4,T) + idegree(0) + odegree(0), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
summary(modelTenVouch)

modelTenVouch.WithGenScaled <- ergm(Net_snaTenVouch ~ edges + nodecov("Age") + nodematch("Gender") + nodeifactor("Gender") + edgecov(Net_kinTen,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodeicov("HouseholdWealthCalc") + absdiff("EduYears") + nodeifactor("EverCommMember") + edgecov(Tendistancemat/10) + nodeifactor("WeeklyPlusWorship") + nodeifactor("Possession") + nodeicov("WeightedReligPart") + nodeicov("GenerousScaled") + mutual + gwesp(0.4,T) + gwdsp(0.4,T) + idegree(0) + odegree(0), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
summary(modelTenVouch.WithGenScaled)

modelTenAdv <- ergm(Net_snaTenAdv ~ edges + nodecov("Age") + nodematch("Gender") + nodeifactor("Gender") + edgecov(Net_kinTen,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodeicov("HouseholdWealthCalc") + absdiff("EduYears") + nodeifactor("EverCommMember") + edgecov(Tendistancemat/10) + nodeifactor("WeeklyPlusWorship") + nodeifactor("Possession") + nodeicov("WeightedReligPart") + mutual + gwesp(0.1,T) + gwdsp(0.1,T) + idegree(0) + odegree(0), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
summary(modelTenAdv)

modelTenAdv.WithGenScaled <- ergm(Net_snaTenAdv ~ edges + nodecov("Age") + nodematch("Gender") + nodeifactor("Gender") + edgecov(Net_kinTen,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodeicov("HouseholdWealthCalc") + absdiff("EduYears") + nodeifactor("EverCommMember") + edgecov(Tendistancemat/10) + nodeifactor("WeeklyPlusWorship") + nodeifactor("Possession") + nodeicov("WeightedReligPart") + nodeicov("GenerousScaled") + mutual + gwesp(0.1,T) + gwdsp(0.1,T) + idegree(0) + odegree(0), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
summary(modelTenAdv.WithGenScaled)

# Alakapuram

modelAlaEmo <- ergm(Net_snaAlaEmo ~ edges + nodecov("Age") + nodematch("Gender") + nodeifactor("Gender") + edgecov(Net_kinAla,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodeicov("HouseholdWealthCalc") + absdiff("EduYears") + nodeifactor("EverCommMember") + edgecov(Aladistancemat/10) + nodeifactor("WeeklyPlusWorship") + nodeifactor("Possession") + nodeicov("WeightedReligPart") + mutual + gwesp(0.1,T) + gwdsp(0.1,T) + idegree(0) + odegree(0), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
summary(modelAlaEmo)

modelAlaEmo.WithGenScaled <- ergm(Net_snaAlaEmo ~ edges + nodecov("Age") + nodematch("Gender") + nodeifactor("Gender") + edgecov(Net_kinAla,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodeicov("HouseholdWealthCalc") + absdiff("EduYears") + nodeifactor("EverCommMember") + edgecov(Aladistancemat/10) + nodeifactor("WeeklyPlusWorship") + nodeifactor("Possession") + nodeicov("WeightedReligPart") + nodeicov("GenerousScaled") + mutual + gwesp(0.1,T) + gwdsp(0.1,T) + idegree(0) + odegree(0), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
summary(modelAlaEmo.WithGenScaled)

modelAlaBehav <- ergm(Net_snaAlaBehav ~ edges + nodecov("Age") + nodematch("Gender") + nodeifactor("Gender") + edgecov(Net_kinAla,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodeicov("HouseholdWealthCalc") + absdiff("EduYears") + nodeifactor("EverCommMember") + edgecov(Aladistancemat/10) + nodeifactor("WeeklyPlusWorship") + nodeifactor("Possession") + nodeicov("WeightedReligPart") + mutual + gwesp(0.3,T) + gwdsp(0.3,T) + idegree(0) + odegree(0), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
summary(modelAlaBehav)

modelAlaBehav.WithGenScaled <- ergm(Net_snaAlaBehav ~ edges + nodecov("Age") + nodematch("Gender") + nodeifactor("Gender") + edgecov(Net_kinAla,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodeicov("HouseholdWealthCalc") + absdiff("EduYears") + nodeifactor("EverCommMember") + edgecov(Aladistancemat/10) + nodeifactor("WeeklyPlusWorship") + nodeifactor("Possession") + nodeicov("WeightedReligPart") + nodeicov("GenerousScaled") + mutual + gwesp(0.3,T) + gwdsp(0.3,T) + idegree(0) + odegree(0), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
summary(modelAlaBehav.WithGenScaled)

modelAlaFinan <- ergm(Net_snaAlaFinan ~ edges + nodecov("Age") + nodematch("Gender") + nodeifactor("Gender") + edgecov(Net_kinAla,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodeicov("HouseholdWealthCalc") + absdiff("EduYears") + nodeifactor("EverCommMember") + edgecov(Aladistancemat/10) + nodeifactor("WeeklyPlusWorship") + nodeifactor("Possession") + nodeicov("WeightedReligPart") + mutual + gwesp(0.1,T) + gwdsp(0.1,T) + idegree(0) + odegree(0), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
summary(modelAlaFinan)

modelAlaFinan.WithGenScaled <- ergm(Net_snaAlaFinan ~ edges + nodecov("Age") + nodematch("Gender") + nodeifactor("Gender") + edgecov(Net_kinAla,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodeicov("HouseholdWealthCalc") + absdiff("EduYears") + nodeifactor("EverCommMember") + edgecov(Aladistancemat/10) + nodeifactor("WeeklyPlusWorship") + nodeifactor("Possession") + nodeicov("WeightedReligPart") + nodeicov("GenerousScaled") + mutual + gwesp(0.1,T) + gwdsp(0.1,T) + idegree(0) + odegree(0), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
summary(modelAlaFinan.WithGenScaled)

modelAlaVouch <- ergm(Net_snaAlaVouch ~ edges + nodecov("Age") + nodematch("Gender") + nodeifactor("Gender") + edgecov(Net_kinAla,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodeicov("HouseholdWealthCalc") + absdiff("EduYears") + nodeifactor("EverCommMember") + edgecov(Aladistancemat/10) + nodeifactor("WeeklyPlusWorship") + nodeifactor("Possession") + nodeicov("WeightedReligPart") + mutual + gwesp(0.2,T) + gwdsp(0.2,T) + idegree(0) + odegree(0), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
summary(modelAlaVouch)

modelAlaVouch.WithGenScaled <- ergm(Net_snaAlaVouch ~ edges + nodecov("Age") + nodematch("Gender") + nodeifactor("Gender") + edgecov(Net_kinAla,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodeicov("HouseholdWealthCalc") + absdiff("EduYears") + nodeifactor("EverCommMember") + edgecov(Aladistancemat/10) + nodeifactor("WeeklyPlusWorship") + nodeifactor("Possession") + nodeicov("WeightedReligPart") + nodeicov("GenerousScaled") + mutual + gwesp(0.2,T) + gwdsp(0.2,T) + idegree(0) + odegree(0), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
summary(modelAlaVouch.WithGenScaled)

modelAlaAdv <- ergm(Net_snaAlaAdv ~ edges + nodecov("Age") + nodematch("Gender") + nodeifactor("Gender") + edgecov(Net_kinAla,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodeicov("HouseholdWealthCalc") + absdiff("EduYears") + nodeifactor("EverCommMember") + edgecov(Aladistancemat/10) + nodeifactor("WeeklyPlusWorship") + nodeifactor("Possession") + nodeicov("WeightedReligPart") + mutual + gwesp(0.1,T) + gwdsp(0.1,T) + idegree(0) + odegree(0), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
summary(modelAlaAdv)

modelAlaAdv.WithGenScaled <- ergm(Net_snaAlaAdv ~ edges + nodecov("Age") + nodematch("Gender") + nodeifactor("Gender") + edgecov(Net_kinAla,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodeicov("HouseholdWealthCalc") + absdiff("EduYears") + nodeifactor("EverCommMember") + edgecov(Aladistancemat/10) + nodeifactor("WeeklyPlusWorship") + nodeifactor("Possession") + nodeicov("WeightedReligPart") + nodeicov("GenerousScaled") + mutual + gwesp(0.1,T) + gwdsp(0.1,T) + idegree(0) + odegree(0), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
summary(modelAlaAdv.WithGenScaled)


## RECIPROCAL SUPPORT ERGMs
## The reciprocal networks are undirected, so the terms change slightly from those above, and the `mutual' term (meaning reciprocity) cannot be included. The GWDSP (geometrically weighted dyadwise shared partner) term is added as an additional control variable, as is degree(0), which accounts for the individuals with no reciprocal relationships

modelTenSup.Recip <- ergm(Net_snaTenRecip ~ edges + nodecov("Age") + nodematch("Gender") + nodefactor("Gender") + edgecov(Net_kinTen,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodecov("HouseholdWealthCalc") + absdiff("EduYears") + nodefactor("EverCommMember") + edgecov(Tendistancemat/10) + nodefactor("WeeklyPlusWorship") + nodefactor("Possession") + nodecov("WeightedReligPart") + gwesp(0.3,T) + gwdsp(0.3,T) + degree(0), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
summary(modelTenSup.Recip)

modelTenSup.RecipWithGenScaled <- ergm(Net_snaTenRecip ~ edges + nodecov("Age") + nodematch("Gender") + nodefactor("Gender") + edgecov(Net_kinTen,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodecov("HouseholdWealthCalc") + absdiff("EduYears") + nodefactor("EverCommMember") + edgecov(Tendistancemat/10) + nodefactor("WeeklyPlusWorship") + nodefactor("Possession") + nodecov("WeightedReligPart") + nodecov("GenerousScaled") + gwesp(0.3,T) + gwdsp(0.3,T) + degree(0), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
summary(modelTenSup.RecipWithGenScaled)

modelAlaSup.Recip <- ergm(Net_snaAlaRecip ~ edges + nodecov("Age") + nodematch("Gender") + nodefactor("Gender") + edgecov(Net_kinAla,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodecov("HouseholdWealthCalc") + absdiff("EduYears") + nodefactor("EverCommMember") + edgecov(Aladistancemat/10) + nodefactor("WeeklyPlusWorship") + nodefactor("Possession") + nodecov("WeightedReligPart") + gwesp(0.5,T) + gwdsp(0.5,T) + degree(0), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
summary(modelAlaSup.Recip)

modelAlaSup.RecipWithGenScaled <- ergm(Net_snaAlaRecip ~ edges + nodecov("Age") + nodematch("Gender") + nodefactor("Gender") + edgecov(Net_kinAla,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodecov("HouseholdWealthCalc") + absdiff("EduYears") + nodefactor("EverCommMember") + edgecov(Aladistancemat/10) + nodefactor("WeeklyPlusWorship") + nodefactor("Possession") + nodecov("WeightedReligPart") + nodecov("GenerousScaled") + gwesp(0.5,T) + gwdsp(0.5,T) + degree(0), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
summary(modelAlaSup.RecipWithGenScaled)



#######################
### REPORTING ERGMs ###
#######################

require(xtable)
require(texreg)

## This code results in the LaTex code for Supplementary Table 8.
or <- exp( modelTenSup$coef ) 
ste <- sqrt( diag( modelTenSup$covar ) ) 
lci <- exp( modelTenSup$coef-1.96*ste ) 
uci <- exp( modelTenSup$coef+1.96*ste ) 
oddsratios <- rbind( round( lci,digits = 4 ),round( or,digits = 4 ),round( uci,digits = 4 ) ) 
oddsratios <- t( oddsratios ) 
colnames( oddsratios ) <- c( "Lower","OR","Upper" )
teststat <- modelTenSup$coef/ste 
teststats <- rbind( round( teststat,digits = 4 )) 
teststats <- t( teststats ) 
colnames( teststats ) <- c("Wald")

Tencoefs <- modelTenSup$coef

Tenresults<-cbind(summary(modelTenSup)$coefs[1],summary(modelTenSup)$coefs[2],or,summary(modelTenSup)$coefs[4])
xtable(Tenresults,digits=c(3,3,3,3,4))

## This code results in the LaTex code for Supplementary Table 9.
modelTenSup.OnlyRelig <- ergm(Net_snaTenSup ~ edges + nodeifactor("WeeklyPlusWorship") + nodeifactor("Possession") + nodeicov("WeightedReligPart"), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
modelTenSup.ReligandCov <- ergm(Net_snaTenSup ~ edges + nodecov("Age") + nodematch("Gender") + nodeifactor("Gender") + edgecov(Net_kinTen,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodeicov("HouseholdWealthCalc") + absdiff("EduYears") + nodeifactor("EverCommMember") + edgecov(Tendistancemat/10) + nodeifactor("WeeklyPlusWorship") + nodeifactor("Possession") + nodeicov("WeightedReligPart"), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
texreg(list(modelTenSup.OnlyRelig,modelTenSup.ReligandCov,modelTenSup,modelTenSup.WithGenScaled),digits=3)


## This code results in the LaTex code for Supplementary Table 10.
or <- exp( modelAlaSup$coef ) 
ste <- sqrt( diag( modelAlaSup$covar ) ) 
lci <- exp( modelAlaSup$coef-1.96*ste ) 
uci <- exp( modelAlaSup$coef+1.96*ste ) 
oddsratios <- rbind( round( lci,digits = 4 ),round( or,digits = 4 ),round( uci,digits = 4 ) ) 
oddsratios <- t( oddsratios ) 
colnames( oddsratios ) <- c( "Lower","OR","Upper" )
teststat <- modelAlaSup$coef/ste 
teststats <- rbind( round( teststat,digits = 4 )) 
teststats <- t( teststats ) 
colnames( teststats ) <- c("Wald")

Alacoefs <- modelAlaSup$coef

Alaresults<-cbind(summary(modelAlaSup)$coefs[1],summary(modelAlaSup)$coefs[2],or,summary(modelAlaSup)$coefs[4])
xtable(Alaresults,digits=c(3,3,3,3,4))

## This code results in the LaTex code for Supplementary Table 11.
modelAlaSup.OnlyRelig <- ergm(Net_snaAlaSup ~ edges + nodeifactor("WeeklyPlusWorship") + nodeifactor("Possession") + nodeicov("WeightedReligPart"), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
modelAlaSup.ReligandCov <- ergm(Net_snaAlaSup ~ edges + nodecov("Age") + nodematch("Gender") + nodeifactor("Gender") + edgecov(Net_kinAla,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodeicov("HouseholdWealthCalc") + absdiff("EduYears") + nodeifactor("EverCommMember") + edgecov(Aladistancemat/10) + nodeifactor("WeeklyPlusWorship") + nodeifactor("Possession") + nodeicov("WeightedReligPart"), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
texreg(list(modelAlaSup.OnlyRelig,modelAlaSup.ReligandCov,modelAlaSup,modelAlaSup.WithGenScaled),digits=3)

## This code results in the LaTex code for Supplementary Table 12.
texreg(list(modelTenBehav,modelTenEmo,modelTenFinan,modelTenVouch,modelTenAdv),digits=3)

## This code results in the LaTex code for Supplementary Table 13.
texreg(list(modelTenBehav.WithGenScaled,modelTenEmo.WithGenScaled,modelTenFinan.WithGenScaled,modelTenVouch.WithGenScaled,modelTenAdv.WithGenScaled),digits=3)

## This code results in the LaTex code for Supplementary Table 14.
texreg(list(modelAlaBehav,modelAlaEmo,modelAlaFinan,modelAlaVouch,modelAlaAdv),digits=3)

## This code results in the LaTex code for Supplementary Table 15.
texreg(list(modelAlaBehav.WithGenScaled,modelAlaEmo.WithGenScaled,modelAlaFinan.WithGenScaled,modelAlaVouch.WithGenScaled,modelAlaAdv.WithGenScaled),digits=3)

## This code results in the LaTex code for Supplementary Table 16.
or <- exp( modelTenSup.Recip$coef ) 
ste <- sqrt( diag( modelTenSup.Recip$covar ) ) 
lci <- exp( modelTenSup.Recip$coef-1.96*ste ) 
uci <- exp( modelTenSup.Recip$coef+1.96*ste ) 
oddsratios <- rbind( round( lci,digits = 4 ),round( or,digits = 4 ),round( uci,digits = 4 ) ) 
oddsratios <- t( oddsratios ) 
colnames( oddsratios ) <- c( "Lower","OR","Upper" )
teststat <- modelTenSup.Recip$coef/ste 
teststats <- rbind( round( teststat,digits = 4 )) 
teststats <- t( teststats ) 
colnames( teststats ) <- c("Wald")

TenRecipcoefs <- modelTenSup.Recip$coef

TenRecipresults<-cbind(summary(modelTenSup.Recip)$coefs[1],summary(modelTenSup.Recip)$coefs[2],or,summary(modelTenSup.Recip)$coefs[4])
xtable(TenRecipresults,digits=c(3,3,3,3,4))

## This code results in the LaTex code for Supplementary Table 17.
modelTenSup.Recip.OnlyRelig <- ergm(Net_snaTenRecip ~ edges + nodefactor("WeeklyPlusWorship") + nodefactor("Possession") + nodecov("WeightedReligPart"), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
modelTenSup.Recip.ReligandCov <- ergm(Net_snaTenRecip ~ edges + nodecov("Age") + nodematch("Gender") + nodefactor("Gender") + edgecov(Net_kinTen,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodecov("HouseholdWealthCalc") + absdiff("EduYears") + nodefactor("EverCommMember") + edgecov(Tendistancemat/10) + nodefactor("WeeklyPlusWorship") + nodefactor("Possession") + nodecov("WeightedReligPart"), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
texreg(list(modelTenSup.Recip.OnlyRelig,modelTenSup.Recip.ReligandCov,modelTenSup.Recip,modelTenSup.RecipWithGenScaled),digits=3)

## This code results in the LaTex code for Supplementary Table 18.
or <- exp( modelAlaSup.Recip$coef ) 
ste <- sqrt( diag( modelAlaSup.Recip$covar ) ) 
lci <- exp( modelAlaSup.Recip$coef-1.96*ste ) 
uci <- exp( modelAlaSup.Recip$coef+1.96*ste ) 
oddsratios <- rbind( round( lci,digits = 4 ),round( or,digits = 4 ),round( uci,digits = 4 ) ) 
oddsratios <- t( oddsratios ) 
colnames( oddsratios ) <- c( "Lower","OR","Upper" )
teststat <- modelAlaSup.Recip$coef/ste 
teststats <- rbind( round( teststat,digits = 4 )) 
teststats <- t( teststats ) 
colnames( teststats ) <- c("Wald")

AlaRecipcoefs <- modelAlaSup.Recip$coef

AlaRecipresults<-cbind(summary(modelAlaSup.Recip)$coefs[1],summary(modelAlaSup.Recip)$coefs[2],or,summary(modelAlaSup.Recip)$coefs[4])
xtable(AlaRecipresults,digits=c(3,3,3,3,4))

## This code results in the LaTex code for Supplementary Table 19.
modelAlaSup.Recip.OnlyRelig <- ergm(Net_snaAlaRecip ~ edges + nodefactor("WeeklyPlusWorship") + nodefactor("Possession") + nodecov("WeightedReligPart"), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
modelAlaSup.Recip.ReligandCov <- ergm(Net_snaAlaRecip ~ edges + nodecov("Age") + nodematch("Gender") + nodefactor("Gender") + edgecov(Net_kinAla,attrname="Kin") + nodematch("Caste") + nodefactor("Caste") + nodecov("HouseholdWealthCalc") + absdiff("EduYears") + nodefactor("EverCommMember") + edgecov(Aladistancemat/10) + nodefactor("WeeklyPlusWorship") + nodefactor("Possession") + nodecov("WeightedReligPart"), control = control.ergm(MCMC.burnin=15000,MCMC.samplesize=50000,MCMC.interval=1000),verbose=FALSE)
texreg(list(modelAlaSup.Recip.OnlyRelig,modelAlaSup.Recip.ReligandCov,modelAlaSup.Recip,modelAlaSup.RecipWithGenScaled),digits=3)


###############################
## Visualizing Model Results ##
###############################

## To determine the predicted tie likelihood, we take entries for each model covariate, multiply them by the model estimates, sum them, and 
## e.g., the entries corresponding to the first example in the text would be: c(1, 60, 1, 0, 0, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 200, 0, 0, 10, 0, 0, 0, 0, 1)
## This corresponds to: Edges = 1 (required), Age = the sum of the two hypothetical individual's ages, so two 30 year olds = 60, Same Gender = 1, Gender = 0 (two females, if two males = 2), Kin = 0, Match Caste = 1, then entries for Agamudaiyaar = 2, Household Wealth = 200 (a tie directed to a person with the average household wealth of ~200,000 Rs), Difference in years of education = 0, Committee Membership = 0 (neither having held some political position), Distance = 10 (the average distance of 100m between households), Regular Worship = 0 (the woman to whom the tie is directed does not worships regularly), Possession = 0 (the woman to whom the tie is directed does not get possessed), Public Religious Acts = 0 (the woman to whom the tie is directed does not perform any public ritual acts), Mutual = 0 (neither has nominated the other as someone who provides them with support), GWESP = 1 (they have one partner in common)
## The predicted tie likelihood for this hypothetical dyad = estoprob(sum( c(1, 60, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 200, 0, 0, 10, 1, 0, 6, 0, 0)*Tencoefs)) = 4.1%
## This is equivalent to 1/(1+exp(-sum( c(1, 60, 1, 0, 0, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 200, 0, 0, 10, 0, 0, 0, 0, 1)*Tencoefs))), the logistic function
estoprob <- function(b) {
  exp(b)/(1+exp(b))
}

## To look at how shifting combinations of religious action alter the likelihood of a tie, I created csv files with various hypothetical dyads. 
## Each column has an entry for each term in the model (as above). Generally, the entries look at increasing religious participation, for different combinations of affiliation. These files are used to create Supplementary Figures 5 - 8.
## PredictERGMTenPallF has a Pallar woman as its base, ...RCF has a RC Yaathavar woman.
Tenpredergm<-read.csv("PredictERGMTenPallF.csv",header=TRUE,as.is=TRUE)
#Tenpredergm<-read.csv("PredictERGMTenRCF.csv",header=TRUE,as.is=TRUE)


pred.vect = rep(0,924)
for (i in 2:925) {
  pred.vect[i-1]=estoprob(sum(as.numeric(Tenpredergm[,i])*Tencoefs))
}

## This code generates Supplementary Figure 5 (top with PredictERGMTenPallF, bottom with PredictERGMTenRCF).
require(RColorBrewer)
colors <- brewer.pal(8,"Dark2")

plot(pred.vect[c(2:21,rep(NA,64),86:105,rep(NA,64),170:189,rep(NA,64),254:273,rep(NA,64),338:357,rep(NA,64),422:441,rep(NA,64),506:525,rep(NA,64),590:609,rep(NA,64),674:693,rep(NA,64),758:777,rep(NA,64),842:861,rep(NA,64))],col=colors[1],type="l",lwd=3,ylab="Likelihood of Tie",xlab="Model Specifications",main="Predicted Likelihood of a Support Tie for an Older Pallar Woman in Tenpatti",xaxt="n",ylim=c(0,1))
points(pred.vect[c(1,rep(NA,83),85,rep(NA,83),169,rep(NA,83),253,rep(NA,83),337,rep(NA,83),421,rep(NA,83),505,rep(NA,83),589,rep(NA,83),673,rep(NA,83),757,rep(NA,83),841)],ylim=c(0,1),col=colors[1],lwd=2,pch=20)

lines(pred.vect[c(rep(NA,21),23:42,rep(NA,64),107:126,rep(NA,64),191:210,rep(NA,64),275:294,rep(NA,64),359:378,rep(NA,64),443:462,rep(NA,64),527:546,rep(NA,64),611:630,rep(NA,64),695:714,rep(NA,64),779:798,rep(NA,64),863:882,rep(NA,64))],col=colors[2],lwd=3,ylim=c(0,1))
points(pred.vect[c(rep(NA,21),1,rep(NA,83),85,rep(NA,83),169,rep(NA,83),253,rep(NA,83),337,rep(NA,83),421,rep(NA,83),505,rep(NA,83),589,rep(NA,83),673,rep(NA,83),757,rep(NA,83),841)+21],ylim=c(0,1),col=colors[2],lwd=2,pch=3)

lines(pred.vect[c(rep(NA,42),23:42,rep(NA,64),107:126,rep(NA,64),191:210,rep(NA,64),275:294,rep(NA,64),359:378,rep(NA,64),443:462,rep(NA,64),527:546,rep(NA,64),611:630,rep(NA,64),695:714,rep(NA,64),779:798,rep(NA,64),863:882,rep(NA,64))+21],col=colors[6],lwd=3,ylim=c(0,1))
points(pred.vect[c(rep(NA,42),1,rep(NA,83),85,rep(NA,83),169,rep(NA,83),253,rep(NA,83),337,rep(NA,83),421,rep(NA,83),505,rep(NA,83),589,rep(NA,83),673,rep(NA,83),757,rep(NA,83),841)+42],ylim=c(0,1),col=colors[6],lwd=2,pch=4)

lines(pred.vect[c(rep(NA,64),23:42,rep(NA,64),107:126,rep(NA,64),191:210,rep(NA,64),275:294,rep(NA,64),359:378,rep(NA,64),443:462,rep(NA,64),527:546,rep(NA,64),611:630,rep(NA,64),695:714,rep(NA,64),779:798,rep(NA,64),863:882,rep(NA,64))+42],col=colors[5],lwd=3,ylim=c(0,1))
points(pred.vect[c(rep(NA,63),1,rep(NA,83),85,rep(NA,83),169,rep(NA,83),253,rep(NA,83),337,rep(NA,83),421,rep(NA,83),505,rep(NA,83),589,rep(NA,83),673,rep(NA,83),757,rep(NA,83),841)+63],ylim=c(0,1),col=colors[5],lwd=2,pch=8)
abline(0, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.1, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.2, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.3, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.4, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.5, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.6, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.7, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.8, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.9, 0, lwd=1,col="lightgrey",lty="dotted")
abline(1, 0, lwd=1,col="lightgrey",lty="dotted")

labloc=c(1,85,169,253,337,421,505,589,673,757,841)+42
lab=c("Diff caste","Same caste","+Mutual","+GWESP=1","GWESP=2","GWESP=3","Close kin","+Mutual","+GWESP=1","GWESP=2","GWESP=3")
text(x=labloc,y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[4]/3),labels=lab,cex=0.6,pos=3,adj=2,xpd=TRUE)
legend("topleft",c("No Religious Action","Worship","Possession","Worship + Possession","Public Ritual"),cex=0.6,col=c(colors[c(1,2,6,5)],"grey"),lty=c(NA,NA,NA,NA,1),lwd=2,pch=c(20,3,4,8,NA))


# PredictERGMTenPallF has a Pallar woman, ...RCF has a RC Yaathavar woman
Alapredergm<-read.csv("PredictERGMAlaPallF.csv",header=TRUE,as.is=TRUE)
#Alapredergm<-read.csv("PredictERGMAlaCSIF.csv",header=TRUE,as.is=TRUE)

pred.vect = rep(0,924)
for (i in 2:925) {
  pred.vect[i-1]=estoprob(sum(as.numeric(Alapredergm[,i])*Alacoefs))
}

## This code generates Supplementary Figure 6 (top with PredictERGMAlaPallF, bottom with PredictERGMAlaCSIF).
plot(pred.vect[c(2:21,rep(NA,64),86:105,rep(NA,64),170:189,rep(NA,64),254:273,rep(NA,64),338:357,rep(NA,64),422:441,rep(NA,64),506:525,rep(NA,64),590:609,rep(NA,64),674:693,rep(NA,64),758:777,rep(NA,64),842:861,rep(NA,64))],col=colors[1],type="l",lwd=3,ylab="Likelihood of Tie",xlab="Model Specifications",main="Predicted Likelihood of a Support Tie for an Older Protestant Pariayar Woman in Alakapuram",xaxt="n",ylim=c(0,1))
points(pred.vect[c(1,rep(NA,83),85,rep(NA,83),169,rep(NA,83),253,rep(NA,83),337,rep(NA,83),421,rep(NA,83),505,rep(NA,83),589,rep(NA,83),673,rep(NA,83),757,rep(NA,83),841)],ylim=c(0,1),col=colors[1],lwd=2,pch=20)

lines(pred.vect[c(rep(NA,21),23:42,rep(NA,64),107:126,rep(NA,64),191:210,rep(NA,64),275:294,rep(NA,64),359:378,rep(NA,64),443:462,rep(NA,64),527:546,rep(NA,64),611:630,rep(NA,64),695:714,rep(NA,64),779:798,rep(NA,64),863:882,rep(NA,64))],col=colors[2],lwd=3,ylim=c(0,1))
points(pred.vect[c(rep(NA,21),1,rep(NA,83),85,rep(NA,83),169,rep(NA,83),253,rep(NA,83),337,rep(NA,83),421,rep(NA,83),505,rep(NA,83),589,rep(NA,83),673,rep(NA,83),757,rep(NA,83),841)+21],ylim=c(0,1),col=colors[2],lwd=2,pch=3)

lines(pred.vect[c(rep(NA,42),23:42,rep(NA,64),107:126,rep(NA,64),191:210,rep(NA,64),275:294,rep(NA,64),359:378,rep(NA,64),443:462,rep(NA,64),527:546,rep(NA,64),611:630,rep(NA,64),695:714,rep(NA,64),779:798,rep(NA,64),863:882,rep(NA,64))+21],col=colors[6],lwd=3,ylim=c(0,1))
points(pred.vect[c(rep(NA,42),1,rep(NA,83),85,rep(NA,83),169,rep(NA,83),253,rep(NA,83),337,rep(NA,83),421,rep(NA,83),505,rep(NA,83),589,rep(NA,83),673,rep(NA,83),757,rep(NA,83),841)+42],ylim=c(0,1),col=colors[6],lwd=2,pch=4)

lines(pred.vect[c(rep(NA,64),23:42,rep(NA,64),107:126,rep(NA,64),191:210,rep(NA,64),275:294,rep(NA,64),359:378,rep(NA,64),443:462,rep(NA,64),527:546,rep(NA,64),611:630,rep(NA,64),695:714,rep(NA,64),779:798,rep(NA,64),863:882,rep(NA,64))+42],col=colors[5],lwd=3,ylim=c(0,1))
points(pred.vect[c(rep(NA,64),1,rep(NA,83),85,rep(NA,83),169,rep(NA,83),253,rep(NA,83),337,rep(NA,83),421,rep(NA,83),505,rep(NA,83),589,rep(NA,83),673,rep(NA,83),757,rep(NA,83),841)+63],ylim=c(0,1),col=colors[5],lwd=2,pch=8)

abline(0, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.1, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.2, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.3, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.4, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.5, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.6, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.7, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.8, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.9, 0, lwd=1,col="lightgrey",lty="dotted")
abline(1, 0, lwd=1,col="lightgrey",lty="dotted")


labloc=c(1,85,169,253,337,421,505,589,673,757,841)+42
lab=c("Diff caste","Same caste","+Mutual","+GWESP=1","GWESP=2","GWESP=3","Close kin","+Mutual","+GWESP=1","GWESP=2","GWESP=3")
text(x=labloc,y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[4]/3),labels=lab,cex=0.6,pos=3,adj=2,xpd=TRUE)
legend("topleft",c("No Religious Action","Worship","Possession","Worship + Possession","Public Ritual"),cex=0.6,col=c(colors[c(1,2,6,5)],"grey"),lty=c(NA,NA,NA,NA,1),lwd=2,pch=c(20,3,4,8,NA))



## For Each Support Type

Tenpredergm<-read.csv("PredictERGMTenPallF.csv",header=TRUE,as.is=TRUE)

TencoefsBehav <- modelTenBehav$coef
TencoefsEmo <- modelTenEmo$coef
TencoefsFinan <- modelTenFinan$coef
TencoefsVouch <- modelTenVouch$coef
TencoefsAdv <- modelTenAdv$coef

Tenpredergm<- rbind(Tenpredergm,c("GWDSP",rep(0,924)),c("idegree",rep(0,924)),c("odegree",rep(0,924)))

pred.vectBehav = rep(0,924)
for (i in 2:925) {
  pred.vectBehav[i-1]=estoprob(sum(as.numeric(Tenpredergm[,i])*TencoefsBehav))
}

pred.vectEmo = rep(0,924)
for (i in 2:925) {
  pred.vectEmo[i-1]=estoprob(sum(as.numeric(Tenpredergm[,i])*TencoefsEmo))
}

pred.vectFinan = rep(0,924)
for (i in 2:925) {
  pred.vectFinan[i-1]=estoprob(sum(as.numeric(Tenpredergm[,i])*TencoefsFinan))
}

pred.vectVouch = rep(0,924)
for (i in 2:925) {
  pred.vectVouch[i-1]=estoprob(sum(as.numeric(Tenpredergm[,i])*TencoefsVouch))
}

pred.vectAdv = rep(0,924)
for (i in 2:925) {
  pred.vectAdv[i-1]=estoprob(sum(as.numeric(Tenpredergm[,i])*TencoefsAdv))
}

## This code generates Supplementary Figure 7 top (with PredictERGMTenPallF).
plot(pred.vectBehav[c(NA,170:189,rep(NA,63))],col=colors[1],type="l",lwd=3,ylab="Likelihood of Tie",xlab="Model Specifications",main="Predicted Likelihood of Each Type of Support Tie for an Older Pallar Woman in Tenpatti",xaxt="n",ylim=c(0,.30))
points(pred.vectBehav[c(169)],ylim=c(0,.15),col=colors[1],lwd=2,pch=20)

lines(pred.vectBehav[c(rep(NA,22),191:210)],col=colors[2],lwd=3,ylim=c(0,.15))
points(pred.vectBehav[c(rep(NA,21),190)],ylim=c(0,.15),col=colors[2],lwd=2,pch=3)

lines(pred.vectBehav[c(rep(NA,43),212:231)],col=colors[6],lwd=3,ylim=c(0,.15))
points(pred.vectBehav[c(rep(NA,42),211)],ylim=c(0,.15),col=colors[6],lwd=2,pch=4)

lines(pred.vectBehav[c(rep(NA,64),233:252)],col=colors[5],lwd=3,ylim=c(0,.15))
points(pred.vectBehav[c(rep(NA,63),232)],ylim=c(0,.15),col=colors[5],lwd=2,pch=8)

lines(pred.vectEmo[c(NA,170:189,rep(NA,63))],col=colors[1],type="l",lwd=3,lty=2,ylim=c(0,.15))
points(pred.vectEmo[c(169)],ylim=c(0,.15),col=colors[1],lwd=2,pch=20)

lines(pred.vectEmo[c(rep(NA,22),191:210)],col=colors[2],lwd=3,ylim=c(0,.15),lty=2)
points(pred.vectEmo[c(rep(NA,21),190)],ylim=c(0,.15),col=colors[2],lwd=2,pch=3)

lines(pred.vectEmo[c(rep(NA,43),212:231)],col=colors[6],lwd=3,ylim=c(0,.15),lty=2)
points(pred.vectEmo[c(rep(NA,42),211)],ylim=c(0,.15),col=colors[6],lwd=2,pch=4)

lines(pred.vectEmo[c(rep(NA,64),233:252)],col=colors[5],lwd=3,ylim=c(0,.15),lty=2)
points(pred.vectEmo[c(rep(NA,63),232)],ylim=c(0,.15),col=colors[5],lwd=2,pch=8)

lines(pred.vectFinan[c(NA,170:189,rep(NA,63))],col=colors[1],type="l",lwd=3,lty=3,ylim=c(0,.15))
points(pred.vectFinan[c(169)],ylim=c(0,.15),col=colors[1],lwd=2,pch=20)

lines(pred.vectFinan[c(rep(NA,22),191:210)],col=colors[2],lwd=3,ylim=c(0,.15),lty=3)
points(pred.vectFinan[c(rep(NA,21),190)],ylim=c(0,.15),col=colors[2],lwd=2,pch=3)

lines(pred.vectFinan[c(rep(NA,43),212:231)],col=colors[6],lwd=3,ylim=c(0,.15),lty=3)
points(pred.vectFinan[c(rep(NA,42),211)],ylim=c(0,.15),col=colors[6],lwd=2,pch=4)

lines(pred.vectFinan[c(rep(NA,64),233:252)],col=colors[5],lwd=3,ylim=c(0,.15),lty=3)
points(pred.vectFinan[c(rep(NA,63),232)],ylim=c(0,.15),col=colors[5],lwd=2,pch=8)

lines(pred.vectAdv[c(NA,170:189,rep(NA,63))],col=colors[1],type="l",lwd=3,lty=4,ylim=c(0,.15))
points(pred.vectAdv[c(169)],ylim=c(0,.15),col=colors[1],lwd=2,pch=20)

lines(pred.vectAdv[c(rep(NA,22),191:210)],col=colors[2],lwd=3,ylim=c(0,.15),lty=4)
points(pred.vectAdv[c(rep(NA,21),190)],ylim=c(0,.15),col=colors[2],lwd=2,pch=3)

lines(pred.vectAdv[c(rep(NA,43),212:231)],col=colors[6],lwd=3,ylim=c(0,.15),lty=4)
points(pred.vectAdv[c(rep(NA,42),211)],ylim=c(0,.15),col=colors[6],lwd=2,pch=4)

lines(pred.vectAdv[c(rep(NA,64),233:252)],col=colors[5],lwd=3,ylim=c(0,.15),lty=4)
points(pred.vectAdv[c(rep(NA,63),232)],ylim=c(0,.15),col=colors[5],lwd=2,pch=8)

lines(pred.vectVouch[c(NA,170:189,rep(NA,63))],col=colors[1],type="l",lwd=3,lty=5,ylim=c(0,.15))
points(pred.vectVouch[c(169)],ylim=c(0,.15),col=colors[1],lwd=2,pch=20)

lines(pred.vectVouch[c(rep(NA,22),191:210)],col=colors[2],lwd=3,ylim=c(0,.15),lty=5)
points(pred.vectVouch[c(rep(NA,21),190)],ylim=c(0,.15),col=colors[2],lwd=2,pch=3)

lines(pred.vectVouch[c(rep(NA,43),212:231)],col=colors[6],lwd=3,ylim=c(0,.15),lty=5)
points(pred.vectVouch[c(rep(NA,42),211)],ylim=c(0,.15),col=colors[6],lwd=2,pch=4)

lines(pred.vectVouch[c(rep(NA,64),233:252)],col=colors[5],lwd=3,ylim=c(0,.15),lty=5)
points(pred.vectVouch[c(rep(NA,63),232)],ylim=c(0,.15),col=colors[5],lwd=2,pch=8)

abline(0, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.05, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.10,0, lwd=1,col="lightgrey",lty="dotted")
abline(0.15,0, lwd=1,col="lightgrey",lty="dotted")
abline(0.20, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.25,0, lwd=1,col="lightgrey",lty="dotted")
abline(0.30,0, lwd=1,col="lightgrey",lty="dotted")

legend("topleft",c("No Religious Action","Worship","Possession","Worship + Possession","Public Ritual","Behavioral","Emotional","Financial","Vouched","Guidance"),cex=0.6,col=c(colors[c(1,2,6,5)],rep("grey",6)),lty=c(NA,NA,NA,NA,1,1,2,3,4,5),lwd=2,pch=c(20,3,4,8,NA,NA,NA,NA,NA,NA),ncol=2)

labloc=c(1,2,6,12,18,22,23,27,33,39,43,44,48,54,60,64,65,69,75,81)
lab=c("none",2,6,12,18,"reg. worship",2,6,12,18,"possession",2,6,12,18,"reg. wor. & poss.",2,6,12,18)
axis(1,at=labloc,labels=FALSE)
text(x=labloc,y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[4]/1.4),labels=lab,srt=45,adj=1,xpd=TRUE,cex=0.6)




Alapredergm<-read.csv("PredictERGMAlaPallF.csv",header=TRUE,as.is=TRUE)

AlacoefsBehav <- modelAlaBehav$coef
AlacoefsEmo <- modelAlaEmo$coef
AlacoefsFinan <- modelAlaFinan$coef
AlacoefsVouch <- modelAlaVouch$coef
AlacoefsAdv <- modelAlaAdv$coef

Alapredergm<- rbind(Alapredergm,c("GWDSP",rep(0,924)),c("idegree",rep(0,924)),c("odegree",rep(0,924)))

pred.vectBehav = rep(0,924)
for (i in 2:925) {
  pred.vectBehav[i-1]=estoprob(sum(as.numeric(Alapredergm[,i])*AlacoefsBehav))
}

pred.vectEmo = rep(0,924)
for (i in 2:925) {
  pred.vectEmo[i-1]=estoprob(sum(as.numeric(Alapredergm[,i])*AlacoefsEmo))
}

pred.vectFinan = rep(0,924)
for (i in 2:925) {
  pred.vectFinan[i-1]=estoprob(sum(as.numeric(Alapredergm[,i])*AlacoefsFinan))
}

pred.vectVouch = rep(0,924)
for (i in 2:925) {
  pred.vectVouch[i-1]=estoprob(sum(as.numeric(Alapredergm[,i])*AlacoefsVouch))
}

pred.vectAdv = rep(0,924)
for (i in 2:925) {
  pred.vectAdv[i-1]=estoprob(sum(as.numeric(Alapredergm[,i])*AlacoefsAdv))
}

## This code generates Supplementary Figure 7 top (with PredictERGMAlaPallF).
plot(pred.vectBehav[c(NA,170:189,rep(NA,63))],col=colors[1],type="l",lwd=3,ylab="Likelihood of Tie",xlab="Model Specifications",main="Predicted Likelihood of Each Type of Support Tie for an Older Pallar Woman in Alakapuram",xaxt="n",ylim=c(0,.30))
points(pred.vectBehav[c(169)],ylim=c(0,.15),col=colors[1],lwd=2,pch=20)

lines(pred.vectBehav[c(rep(NA,22),191:210)],col=colors[2],lwd=3,ylim=c(0,.15))
points(pred.vectBehav[c(rep(NA,21),190)],ylim=c(0,.15),col=colors[2],lwd=2,pch=3)

lines(pred.vectBehav[c(rep(NA,43),212:231)],col=colors[6],lwd=3,ylim=c(0,.15))
points(pred.vectBehav[c(rep(NA,42),211)],ylim=c(0,.15),col=colors[6],lwd=2,pch=4)

lines(pred.vectBehav[c(rep(NA,64),233:252)],col=colors[5],lwd=3,ylim=c(0,.15))
points(pred.vectBehav[c(rep(NA,63),232)],ylim=c(0,.15),col=colors[5],lwd=2,pch=8)

lines(pred.vectEmo[c(NA,170:189,rep(NA,63))],col=colors[1],type="l",lwd=3,lty=2,ylim=c(0,.15))
points(pred.vectEmo[c(169)],ylim=c(0,.15),col=colors[1],lwd=2,pch=20)

lines(pred.vectEmo[c(rep(NA,22),191:210)],col=colors[2],lwd=3,ylim=c(0,.15),lty=2)
points(pred.vectEmo[c(rep(NA,21),190)],ylim=c(0,.15),col=colors[2],lwd=2,pch=3)

lines(pred.vectEmo[c(rep(NA,43),212:231)],col=colors[6],lwd=3,ylim=c(0,.15),lty=2)
points(pred.vectEmo[c(rep(NA,42),211)],ylim=c(0,.15),col=colors[6],lwd=2,pch=4)

lines(pred.vectEmo[c(rep(NA,64),233:252)],col=colors[5],lwd=3,ylim=c(0,.15),lty=2)
points(pred.vectEmo[c(rep(NA,63),232)],ylim=c(0,.15),col=colors[5],lwd=2,pch=8)

lines(pred.vectFinan[c(NA,170:189,rep(NA,63))],col=colors[1],type="l",lwd=3,lty=3,ylim=c(0,.15))
points(pred.vectFinan[c(169)],ylim=c(0,.15),col=colors[1],lwd=2,pch=20)

lines(pred.vectFinan[c(rep(NA,22),191:210)],col=colors[2],lwd=3,ylim=c(0,.15),lty=3)
points(pred.vectFinan[c(rep(NA,21),190)],ylim=c(0,.15),col=colors[2],lwd=2,pch=3)

lines(pred.vectFinan[c(rep(NA,43),212:231)],col=colors[6],lwd=3,ylim=c(0,.15),lty=3)
points(pred.vectFinan[c(rep(NA,42),211)],ylim=c(0,.15),col=colors[6],lwd=2,pch=4)

lines(pred.vectFinan[c(rep(NA,64),233:252)],col=colors[5],lwd=3,ylim=c(0,.15),lty=3)
points(pred.vectFinan[c(rep(NA,63),232)],ylim=c(0,.15),col=colors[5],lwd=2,pch=8)

lines(pred.vectAdv[c(NA,170:189,rep(NA,63))],col=colors[1],type="l",lwd=3,lty=4,ylim=c(0,.15))
points(pred.vectAdv[c(169)],ylim=c(0,.15),col=colors[1],lwd=2,pch=20)

lines(pred.vectAdv[c(rep(NA,22),191:210)],col=colors[2],lwd=3,ylim=c(0,.15),lty=4)
points(pred.vectAdv[c(rep(NA,21),190)],ylim=c(0,.15),col=colors[2],lwd=2,pch=3)

lines(pred.vectAdv[c(rep(NA,43),212:231)],col=colors[6],lwd=3,ylim=c(0,.15),lty=4)
points(pred.vectAdv[c(rep(NA,42),211)],ylim=c(0,.15),col=colors[6],lwd=2,pch=4)

lines(pred.vectAdv[c(rep(NA,64),233:252)],col=colors[5],lwd=3,ylim=c(0,.15),lty=4)
points(pred.vectAdv[c(rep(NA,63),232)],ylim=c(0,.15),col=colors[5],lwd=2,pch=8)

lines(pred.vectVouch[c(NA,170:189,rep(NA,63))],col=colors[1],type="l",lwd=3,lty=5,ylim=c(0,.15))
points(pred.vectVouch[c(169)],ylim=c(0,.15),col=colors[1],lwd=2,pch=20)

lines(pred.vectVouch[c(rep(NA,22),191:210)],col=colors[2],lwd=3,ylim=c(0,.15),lty=5)
points(pred.vectVouch[c(rep(NA,21),190)],ylim=c(0,.15),col=colors[2],lwd=2,pch=3)

lines(pred.vectVouch[c(rep(NA,43),212:231)],col=colors[6],lwd=3,ylim=c(0,.15),lty=5)
points(pred.vectVouch[c(rep(NA,42),211)],ylim=c(0,.15),col=colors[6],lwd=2,pch=4)

lines(pred.vectVouch[c(rep(NA,64),233:252)],col=colors[5],lwd=3,ylim=c(0,.15),lty=5)
points(pred.vectVouch[c(rep(NA,63),232)],ylim=c(0,.15),col=colors[5],lwd=2,pch=8)

abline(0, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.05, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.10,0, lwd=1,col="lightgrey",lty="dotted")
abline(0.15,0, lwd=1,col="lightgrey",lty="dotted")
abline(0.20, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.25,0, lwd=1,col="lightgrey",lty="dotted")
abline(0.30,0, lwd=1,col="lightgrey",lty="dotted")

legend("topleft",c("No Religious Action","Worship","Possession","Worship + Possession","Public Ritual","Behavioral","Emotional","Financial","Vouched","Guidance"),cex=0.6,col=c(colors[c(1,2,6,5)],rep("grey",6)),lty=c(NA,NA,NA,NA,1,1,2,3,4,5),lwd=2,pch=c(20,3,4,8,NA,NA,NA,NA,NA,NA),ncol=2)

labloc=c(1,2,6,12,18,22,23,27,33,39,43,44,48,54,60,64,65,69,75,81)
lab=c("none",2,6,12,18,"reg. worship",2,6,12,18,"possession",2,6,12,18,"reg. wor. & poss.",2,6,12,18)
axis(1,at=labloc,labels=FALSE)
text(x=labloc,y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[4]/1.4),labels=lab,srt=45,adj=1,xpd=TRUE,cex=0.6)



## Reciprocal 

TenRecippredergm<-read.csv("PredictERGMRecipTenPallF.csv",header=TRUE,as.is=TRUE)

pred.vect = rep(0,757)
for (i in 2:757) {
  pred.vect[i-1]=estoprob(sum(as.numeric(TenRecippredergm[,i])*TenRecipcoefs))
}

## This code generates Supplementary Figure 8 top (with PredictERGMRecipTenPallF).
plot(pred.vect[c(2:21,rep(NA,64),86:105,rep(NA,64),170:189,rep(NA,64),254:273,rep(NA,64),338:357,rep(NA,64),422:441,rep(NA,64),506:525,rep(NA,64),590:609,rep(NA,64),674:693,rep(NA,64))],col=colors[1],type="l",lwd=3,ylab="Likelihood of Tie",xlab="Model Specifications",main="Predicted Likelihood of a Reciprocal Support Tie for an Older Pallar Woman in Tenpatti",xaxt="n",ylim=c(0,1))
points(pred.vect[c(1,rep(NA,83),85,rep(NA,83),169,rep(NA,83),253,rep(NA,83),337,rep(NA,83),421,rep(NA,83),505,rep(NA,83),589,rep(NA,83),673,rep(NA,83))],ylim=c(0,1),col=colors[1],lwd=2,pch=20)

lines(pred.vect[c(rep(NA,21),23:42,rep(NA,64),107:126,rep(NA,64),191:210,rep(NA,64),275:294,rep(NA,64),359:378,rep(NA,64),443:462,rep(NA,64),527:546,rep(NA,64),611:630,rep(NA,64),695:714,rep(NA,64))],col=colors[2],lwd=3,ylim=c(0,1))
points(pred.vect[c(rep(NA,21),1,rep(NA,83),85,rep(NA,83),169,rep(NA,83),253,rep(NA,83),337,rep(NA,83),421,rep(NA,83),505,rep(NA,83),589,rep(NA,83),673,rep(NA,83))+21],ylim=c(0,1),col=colors[2],lwd=2,pch=3)

lines(pred.vect[c(rep(NA,42),23:42,rep(NA,64),107:126,rep(NA,64),191:210,rep(NA,64),275:294,rep(NA,64),359:378,rep(NA,64),443:462,rep(NA,64),527:546,rep(NA,64),611:630,rep(NA,64),695:714,rep(NA,64))+21],col=colors[6],lwd=3,ylim=c(0,1))
points(pred.vect[c(rep(NA,42),1,rep(NA,83),85,rep(NA,83),169,rep(NA,83),253,rep(NA,83),337,rep(NA,83),421,rep(NA,83),505,rep(NA,83),589,rep(NA,83),673,rep(NA,83))+42],ylim=c(0,1),col=colors[6],lwd=2,pch=4)

lines(pred.vect[c(rep(NA,64),23:42,rep(NA,64),107:126,rep(NA,64),191:210,rep(NA,64),275:294,rep(NA,64),359:378,rep(NA,64),443:462,rep(NA,64),527:546,rep(NA,64),611:630,rep(NA,64),695:714,rep(NA,64))+42],col=colors[5],lwd=3,ylim=c(0,1))
points(pred.vect[c(rep(NA,63),1,rep(NA,83),85,rep(NA,83),169,rep(NA,83),253,rep(NA,83),337,rep(NA,83),421,rep(NA,83),505,rep(NA,83),589,rep(NA,83),673,rep(NA,83))+63],ylim=c(0,1),col=colors[5],lwd=2,pch=8)
abline(0, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.1, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.2, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.3, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.4, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.5, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.6, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.7, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.8, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.9, 0, lwd=1,col="lightgrey",lty="dotted")
abline(1, 0, lwd=1,col="lightgrey",lty="dotted")

labloc=c(1,85,169,253,337,421,505,589,673)+42
lab=c("Diff caste","Same caste","+GWESP=1","GWESP=2","GWESP=3","Close kin","+GWESP=1","GWESP=2","GWESP=3")
text(x=labloc,y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[4]/3),labels=lab,cex=0.6,pos=3,adj=2,xpd=TRUE)
legend("topleft",c("No Religious Action","Worship","Possession","Worship + Possession","Public Ritual"),cex=0.6,col=c(colors[c(1,2,6,5)],"grey"),lty=c(NA,NA,NA,NA,1),lwd=2,pch=c(20,3,4,8,NA))



AlaRecippredergm<-read.csv("PredictERGMRecipAlaPallF.csv",header=TRUE,as.is=TRUE)

pred.vect = rep(0,757)
for (i in 2:757) {
  pred.vect[i-1]=estoprob(sum(as.numeric(AlaRecippredergm[,i])*AlaRecipcoefs))
}

## This code generates Supplementary Figure 8 bottom (with PredictERGMRecipAlaPallF).
plot(pred.vect[c(2:21,rep(NA,64),86:105,rep(NA,64),170:189,rep(NA,64),254:273,rep(NA,64),338:357,rep(NA,64),422:441,rep(NA,64),506:525,rep(NA,64),590:609,rep(NA,64),674:693,rep(NA,64))],col=colors[1],type="l",lwd=3,ylab="Likelihood of Tie",xlab="Model Specifications",main="Predicted Likelihood of a Reciprocal Support Tie for an Older Pallar Woman in Alakapuram",xaxt="n",ylim=c(0,1))
points(pred.vect[c(1,rep(NA,83),85,rep(NA,83),169,rep(NA,83),253,rep(NA,83),337,rep(NA,83),421,rep(NA,83),505,rep(NA,83),589,rep(NA,83),673,rep(NA,83))],ylim=c(0,1),col=colors[1],lwd=2,pch=20)

lines(pred.vect[c(rep(NA,21),23:42,rep(NA,64),107:126,rep(NA,64),191:210,rep(NA,64),275:294,rep(NA,64),359:378,rep(NA,64),443:462,rep(NA,64),527:546,rep(NA,64),611:630,rep(NA,64),695:714,rep(NA,64))],col=colors[2],lwd=3,ylim=c(0,1))
points(pred.vect[c(rep(NA,21),1,rep(NA,83),85,rep(NA,83),169,rep(NA,83),253,rep(NA,83),337,rep(NA,83),421,rep(NA,83),505,rep(NA,83),589,rep(NA,83),673,rep(NA,83))+21],ylim=c(0,1),col=colors[2],lwd=2,pch=3)

lines(pred.vect[c(rep(NA,42),23:42,rep(NA,64),107:126,rep(NA,64),191:210,rep(NA,64),275:294,rep(NA,64),359:378,rep(NA,64),443:462,rep(NA,64),527:546,rep(NA,64),611:630,rep(NA,64),695:714,rep(NA,64))+21],col=colors[6],lwd=3,ylim=c(0,1))
points(pred.vect[c(rep(NA,42),1,rep(NA,83),85,rep(NA,83),169,rep(NA,83),253,rep(NA,83),337,rep(NA,83),421,rep(NA,83),505,rep(NA,83),589,rep(NA,83),673,rep(NA,83))+42],ylim=c(0,1),col=colors[6],lwd=2,pch=4)

lines(pred.vect[c(rep(NA,64),23:42,rep(NA,64),107:126,rep(NA,64),191:210,rep(NA,64),275:294,rep(NA,64),359:378,rep(NA,64),443:462,rep(NA,64),527:546,rep(NA,64),611:630,rep(NA,64),695:714,rep(NA,64))+42],col=colors[5],lwd=3,ylim=c(0,1))
points(pred.vect[c(rep(NA,63),1,rep(NA,83),85,rep(NA,83),169,rep(NA,83),253,rep(NA,83),337,rep(NA,83),421,rep(NA,83),505,rep(NA,83),589,rep(NA,83),673,rep(NA,83))+63],ylim=c(0,1),col=colors[5],lwd=2,pch=8)
abline(0, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.1, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.2, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.3, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.4, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.5, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.6, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.7, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.8, 0, lwd=1,col="lightgrey",lty="dotted")
abline(0.9, 0, lwd=1,col="lightgrey",lty="dotted")
abline(1, 0, lwd=1,col="lightgrey",lty="dotted")

labloc=c(1,85,169,253,337,421,505,589,673)+42
lab=c("Diff caste","Same caste","+GWESP=1","GWESP=2","GWESP=3","Close kin","+GWESP=1","GWESP=2","GWESP=3")
text(x=labloc,y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[4]/3),labels=lab,cex=0.6,pos=3,adj=2,xpd=TRUE)
legend("topleft",c("No Religious Action","Worship","Possession","Worship + Possession","Public Ritual"),cex=0.6,col=c(colors[c(1,2,6,5)],"grey"),lty=c(NA,NA,NA,NA,1),lwd=2,pch=c(20,3,4,8,NA))


