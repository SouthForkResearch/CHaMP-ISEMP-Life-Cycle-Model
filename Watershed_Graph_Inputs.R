
# Graph Inputs
# This code is separate but complimentary to the ISEMP Life Cycle
# Model Code "Watershed.R".
# It's intent is simply to graph key inputs specified by the set of input
# files used by the life cycle model, such that the user can check that
# the actual inputs match the intended inputs as specified by the input
# files.  In that sense this is a quality assurance program for the LCM.

# Matt Nahorniak
# Updated December 2016
############################################################################

windows(record=T)
# Load required packages
library(MCMCpack, quietly=T) # for Dirichlet (multivariable beta) distributions
library(VGAM, quietly=T) # Library with posnorm functions (positiive only normal)

# Load required functions written for the LCM
source("Watershed_ReadData.r")
source("Watershed_MonteCarlo.r")

# Read header file
header<- Read.Header("Watershed_Header_File.csv") 

# Read Inputs files
Inputs<-Read.Input.File(header)


r=1
#Mirror the inital steps of the LCM
variables=Initialize.Variables(header)
parameters=MonteCarlo(Inputs, variables, header)

# Store parameters - initialize
parametersNR = matrix(rep(parameters, header$R), c(header$R, length(parameters)))

parametersNR[1,]=parameters
colnames(parametersNR) = names(parameters)
#names(parametersNR[1,])

# Cycle through number of MC interations to build parametersNR
r=1
for (r in 2:header$R) {
print(paste("MC Iteration",r,"of",header$R))
variables=Initialize.Variables(header)
parameters=MonteCarlo(Inputs, variables, header)
parametersNR[r,]=parameters
}


# Start building the plots

attach(header)
######################################
#######################################
# Ak_x_Lqk

for (k in 1:K) {

ul = rep(0,R)
for (r in 1:R) {ul[r] = max(parametersNR[r,]$Ak_x_Lqk[k,,])}
maxAk_x_Lqk = max(ul)

plot(c(1:Tr), parametersNR[1,]$Ak_x_Lqk[k,1,], 
main=paste("Site",k,"Ak_x_Lqk"),
ylim=c(0, 1.4*maxAk_x_Lqk), type="l",
xlab="time", ylab="Ak_x_Lqk")

for (r in 1:R) {
 for (q in 1:Q) {
     lines(c(1:Tr), parametersNR[r,]$Ak_x_Lqk[k,q,],
        col=1, lt=q)
}}
legend("topright",paste(rep("Land Use Classification", Q) ,c(1:Q)), lt=c(1:Q))
} # end of sites
##########################################




######################################
#######################################
# M
for (k in 1:K) {

ul = rep(0,R)
for (r in 1:R) {ul[r] = max(parametersNR[r,]$M[k,,,])}
maxM = max(ul)

plot(c(1:Tr), parametersNR[1,]$M[k,1,1,], 
main=paste("Site",k,"M"),
ylim=c(0, 1.4*maxM), type="l",
xlab="time", ylab="M")


for (r in 1:R) {
 for (q in 1:Q) {
  for (j in 1:J) {
     lines(c(1:Tr), parametersNR[r,]$M[k,q,j,],
        col=j, lt=q)
}}
}

legend("topright",paste(rep("Habitat Type", J) ,c(1:J)), lt=1,col=c(1:J))
legend("topleft",paste(rep("Land Use Classification",Q) ,c(1:Q)), lt=c(1:Q))

} # end of sites





### Sr ###########
### Take out i=5 (OnePlus handled separately) and i=1 (NA)
k=1
for (k in 1:K) {

maxSr=1
plot(c(1:Tr),parametersNR[1,]$Sr[k,2,], 
main=paste("Site",k,"Sr: Survival Probability by 
Life Stage (OnePlus to Smolt Not Included)"),
ylim=c(0, 1.4*maxSr), type="l",
xlab="time", ylab="Sr")

i=2
c(2:4, 6:17)
for (r in 1:R) {
 for (i in c(2:4, 6:17)) {
     lines(c(1:Tr), 
parametersNR[r,]$Sr[k,i,]
,        col=i, lt=c(rep(1,8), rep(2,7)))
}
}
as.character(0:9)

legend("topright",
c("Spawner-Egg","Egg-Fry","Fry-Parr","Parr-OnePlus",
"Smolt-Adult", paste(rep("Ocean Age ",10),0:9,"-",seq(1:10)),sep=""),
col= c(2:4,6:17), lt=c(rep(1,8), rep(2,7)),ncol=3, cex=.8)

} # end of sites





###  SR5 ###########
k=1
for (k in 1:K) {

maxSR5=1
plot(c(1:Tr),parametersNR[1,]$SR5[k,1,], 
main=paste("Site",k,"SR5: OnePlus Survival Probability by 
Years as OnePlus"),
ylim=c(0, 1.4*maxSR5), type="l",
xlab="time", ylab="SR5")


for (r in 1:R) {
 for (i in c(1:5)) {
     lines(c(1:Tr), 
parametersNR[r,]$SR5[k,i,]
,        col=i, lt=c(rep(1,5), rep(2,5)))
}
}


legend("topright",
paste(rep("OnePlus YR ",10),c(0:9),"-",c(1:10), sep=""),
,        col=1:10, lt=c(rep(1,5), rep(2,5)),
ncol=3, cex=.8)


} # end of sites

###########
#######################################
# D
for (k in 1:K) {

ul = rep(0,R)
for (r in 1:R) {ul[r] = max(parametersNR[r,]$D[k,,,])}
maxD = max(ul)
maxD
plot(c(1:Tr), parametersNR[1,]$D[k,1,1,], 
main=paste("Site",k,"D: Maximum Density (fish/sq meter)"),
ylim=c(.1, 8*maxD), type="l",
xlab="time", ylab="D", log="y")


for (r in 1:R) {
 for (j in 1:J) {
  for (d in 1:5) {
     lines(c(1:Tr), parametersNR[r,]$D[k,j,d,],
        col=d, lt=j)
}}
}

parametersNR[r,]$D[k,2,d,]

legend("topright",paste(rep("Habitat Type", J) ,c(1:J)),lt=c(1:J),cex=.8)
legend("topleft",c("Egg","Fry","Parr","OnePlus-Y0","Smolt"),lt=1, col=c(1:5),cex=.8)

} # end of sites


# Prod Scalar
#######################################
for (k in 1:K) {

ul = rep(0,R)
ll = rep(0,R)
for (r in 1:R) {
ul[r] = max(parametersNR[r,]$Prod_Scalar[k,,1:5,])
ll[r] = min(parametersNR[r,]$Prod_Scalar[k,,1:5,])
}

maxD = max(ul)
minD = min(ll)
minD

plot(c(1:Tr), parametersNR[1,]$Prod_Scalar[k,1,1,], 
main=paste("Site",k,"Productivity Scalar by Land Use Classification"),
ylim=c(
round(.95*minD,digits=2)
, maxD+(0.4*(maxD-minD))), type="l",
xlab="time", ylab="D")


for (r in 1:R) {
 for (q in 1:Q) {
  for (d in 1:5) {
     lines(c(1:Tr), parametersNR[r,]$Prod_Scalar[k,q,d,],
        col=d, lt=q)
}}}

legend("topright",paste(rep("Land Use Class", Q) ,c(1:Q)),lt=c(1:Q),cex=.8)
legend("topleft",c("Egg","Fry","Parr","OnePlus-Y0","Smolt"),lt=1, col=c(1:5),cex=.8)

} # end of sites

############################################
############################################
#N5.Psmolt_F
for (k in 1:K) {

ul = rep(0,R)
ll = rep(0,R)
for (r in 1:R) {

      names(parametersNR[r,])
ul[r] = max(parametersNR[r,]$N5.Psmolt_F[k,,])
ll[r] = min(parametersNR[r,]$N5.Psmolt_F[k,,])
}

maxD = 1
minD = min(ll)
minD
lt = c(1,1,1,1,1,2,2,2,2,2)
color = c(1,2,3,4,5,1,2,3,4,5)

plot(c(1:Tr), parametersNR[1,]$N5.Psmolt_F[k,1,], 
main=paste("Site",k,"Probability of Females Smolting 
by Years as OnePlus"),
ylim=c(round(.95*minD,digits=2), 1.4)
, type="l",
xlab="time", ylab="D")


for (r in 1:R) {
  for (i5 in 1:5) {
     lines(c(1:Tr), parametersNR[r,]$N5.Psmolt_F[k,i5,],
        col=color[i5], lt=lt[i5])}}

legend("topright",paste(rep("Years=", ) ,1:I5),
lt=lt, col=color,cex=.8, ncol=3)
} # end of sites

#########
############################################
#N5.Psmolt_M
for (k in 1:K) {

ul = rep(0,R)
ll = rep(0,R)
for (r in 1:R) {

      names(parametersNR[r,])
ul[r] = max(parametersNR[r,]$N5.Psmolt_M[k,,])
ll[r] = min(parametersNR[r,]$N5.Psmolt_M[k,,])
}

maxD = 1
minD = min(ll)
minD
lt = c(1,1,1,1,1,2,2,2,2,2)
color = c(1,2,3,4,5,1,2,3,4,5)

plot(c(1:Tr), parametersNR[1,]$N5.Psmolt_M[k,1,], 
main=paste("Site",k,"Probability of Males Smolting 
by Years as OnePlus"),
ylim=c(round(.95*minD,digits=2), 1.4)
, type="l",
xlab="time", ylab="D")


for (r in 1:R) {
  for (i5 in 1:5) {
     lines(c(1:Tr), parametersNR[r,]$N5.Psmolt_M[k,i5,],
        col=color[i5], lt=lt[i5])}}

legend("topright",paste(rep("Years=", ) ,1:I5),
lt=lt, col=color,cex=.8, ncol=3)
} # end of sites

#########
#########


############################################
#N5.Pstay_F
for (k in 1:K) {

ul = rep(0,R)
ll = rep(0,R)
for (r in 1:R) {
ul[r] = max(parametersNR[r,]$N5.Pstay_F[k,,])
ll[r] = min(parametersNR[r,]$N5.Pstay_F[k,,])}

maxD = 1
minD = min(ll)
minD
lt = c(1,1,1,1,1,2,2,2,2,2)
color = c(1,2,3,4,5,1,2,3,4,5)

plot(c(1:Tr), parametersNR[1,]$N5.Pstay_F[k,1,], 
main=paste("Site",k,"Probability of a Female
 Staying a OnePlus, by Years as OnePlus"),
ylim=c(round(.95*minD,digits=2), 1.4)
, type="l",
xlab="time", ylab="D")


for (r in 1:R) {
  for (i5 in 1:5) {
     lines(c(1:Tr), parametersNR[r,]$N5.Psmolt_F[k,i5,],
        col=color[i5], lt=lt[i5])}}

legend("topright",paste(rep("Years=", ) ,1:I5),
lt=lt, col=color,cex=.8, ncol=3)
} # end of sites


##############
############################################
#N5.Pstay_M
for (k in 1:K) {

ul = rep(0,R)
ll = rep(0,R)
for (r in 1:R) {
ul[r] = max(parametersNR[r,]$N5.Pstay_M[k,,])
ll[r] = min(parametersNR[r,]$N5.Pstay_M[k,,])}

maxD = 1
minD = min(ll)
minD
lt = c(1,1,1,1,1,2,2,2,2,2)
color = c(1,2,3,4,5,1,2,3,4,5)

plot(c(1:Tr), parametersNR[1,]$N5.Pstay_M[k,1,], 
main=paste("Site",k,"Probability of a Male
 Staying a OnePlus, by Years as OnePlus"),
ylim=c(round(.95*minD,digits=2), 1.4)
, type="l",
xlab="time", ylab="D")


for (r in 1:R) {
  for (i5 in 1:5) {
     lines(c(1:Tr), parametersNR[r,]$N5.Psmolt_M[k,i5,],
        col=color[i5], lt=lt[i5])}}

legend("topright",paste(rep("Years=", ) ,1:I5),
lt=lt, col=color,cex=.8, ncol=3)
} # end of sites


##############
############################################
############################################
#N5.Pspawn_F
for (k in 1:K) {

ul = rep(0,R)
ll = rep(0,R)
for (r in 1:R) {
ul[r] = max(parametersNR[r,]$N5.Pspawn_F[k,,])
ll[r] = min(parametersNR[r,]$N5.Pspawn_F[k,,])}

maxD = 1
minD = min(ll)
minD
lt = c(1,1,1,1,1,2,2,2,2,2)
color = c(1,2,3,4,5,1,2,3,4,5)

plot(c(1:Tr), parametersNR[1,]$N5.Pspawn_F[k,1,], 
main=paste("Site",k,"Probability of Female Spawning 
(as resident rainbow) by Years as OnePlus"),
ylim=c(round(.95*minD,digits=2), 1.4)
, type="l",
xlab="time", ylab="D")


for (r in 1:R) {
  for (i5 in 1:5) {
     lines(c(1:Tr), parametersNR[r,]$N5.Pspawn_F[k,i5,],
        col=color[i5], lt=lt[i5])}}

legend("topright",paste(rep("Years=", ) ,1:I5),
lt=lt, col=color,cex=.8, ncol=3)
} # end of sites

############################################
#N5.Pspawn_M
for (k in 1:K) {

ul = rep(0,R)
ll = rep(0,R)
for (r in 1:R) {
ul[r] = max(parametersNR[r,]$N5.Pspawn_M[k,,])
ll[r] = min(parametersNR[r,]$N5.Pspawn_M[k,,])}

maxD = 1
minD = min(ll)
minD
lt = c(1,1,1,1,1,2,2,2,2,2)
color = c(1,2,3,4,5,1,2,3,4,5)

plot(c(1:Tr), parametersNR[1,]$N5.Pspawn_M[k,1,], 
main=paste("Site",k,"Probability of Male Spawning 
(as resident rainbow) by Years as OnePlus"),
ylim=c(round(.95*minD,digits=2), 1.4)
, type="l",
xlab="time", ylab="D")


for (r in 1:R) {
  for (i5 in 1:5) {
     lines(c(1:Tr), parametersNR[r,]$N5.Pspawn_M[k,i5,],
        col=color[i5], lt=lt[i5])}}

legend("topright",paste(rep("Years=", ) ,1:I5),
lt=lt, col=color,cex=.8, ncol=3)
} # end of sites

############################################
#N5.cap
for (k in 1:K) {

ul = rep(0,R)
ll = rep(0,R)
for (r in 1:R) {
ul[r] = max(parametersNR[r,]$N5.cap[k,,])
ll[r] = min(parametersNR[r,]$N5.cap[k,,])}


maxD= max(ul)
minD = min(ll)
minD
lt = c(1,1,1,1,1,2,2,2,2,2)
color = c(1,2,3,4,5,1,2,3,4,5)

plot(c(1:Tr), parametersNR[1,]$N5.cap[k,1,], 
main=paste("Site",k,"OnePlus Capcity Scalar by Number of Years as OnePlus"),
ylim=c(round(.95*minD,digits=2), 1.4)
, type="l",
xlab="time", ylab="D")


for (r in 1:R) {
  for (i5 in 1:I5) {
     lines(c(1:Tr), parametersNR[r,]$N5.cap[k,i5,],
        col=color[i5], lt=lt[i5])}}

legend("topright",paste(rep("Years=", ) ,1:I5),
lt=lt, col=color,cex=.8, ncol=3)
} # end of sites

############################################

############################################
############################################
#Mat8Plus_F
for (k in 1:K) {

ul = rep(0,R)
ll = rep(0,R)
for (r in 1:R) {
ul[r] = max(parametersNR[r,]$Mat8Plus_F[k,,])
ll[r] = min(parametersNR[r,]$Mat8Plus_F[k,,])}


minD = 0
maxD = 1
minD
maxD
lt = c(1,1,1,1,1,2,2,2,2,2)
color = c(1,2,3,4,5,1,2,3,4,5)

plot(c(1:Tr), parametersNR[1,]$Mat8Plus_F[k,1,], 
main=paste("Site",k,"Probablity of Females Attempting 
to Spawn by Years as Adult"),
ylim=c(0,  1.4*maxD), type="l",xlab="time", ylab="D")


for (r in 1:R) {
  for (i in 1:6) {
     lines(c(1:Tr), parametersNR[r,]$Mat8Plus_F[k,i,],
        col=color[i], lt=lt[i])}}


legend("topright",paste(rep("Years=", ) ,1:I5),
lt=lt, col=color,cex=.8, ncol=3)
} # end of sites

############################################
#Mat8Plus_M
for (k in 1:K) {

ul = rep(0,R)
ll = rep(0,R)
for (r in 1:R) {
ul[r] = max(parametersNR[r,]$Mat8Plus_M[k,,])
ll[r] = min(parametersNR[r,]$Mat8Plus_M[k,,])}


minD = 0
maxD = 1
minD
maxD
lt = c(1,1,1,1,1,2,2,2,2,2)
color = c(1,2,3,4,5,1,2,3,4,5)

plot(c(1:Tr), parametersNR[1,]$Mat8Plus_M[k,1,], 
main=paste("Site",k,"Probablity of Males Attempting 
to Spawn by Years as Adult"),
ylim=c(0,  1.4*maxD), type="l",xlab="time", ylab="D")


for (r in 1:R) {
  for (i in 1:6) {
     lines(c(1:Tr), parametersNR[r,]$Mat8Plus_M[k,i,],
        col=color[i], lt=lt[i])}}


legend("topright",paste(rep("Years=", ) ,1:I5),
lt=lt, col=color,cex=.8, ncol=3)
} # end of sites


##################################################
# frac
for (k in 1:K) {

ul = rep(0,R)
for (r in 1:R) {ul[r] = max(parametersNR[r,]$frac[k,,,])}
maxD = max(ul)
maxD
plot(c(1:Tr), parametersNR[1,]$frac[k,1,1,], 
main=paste("Site",k,"fractional area by life-stage / Habitat Type"),
ylim=c(.1, 1.4*maxD), type="l",
xlab="time", ylab="D")


for (r in 1:R) {
 for (j in 1:J) {
  for (d in 1:5) {
     lines(c(1:Tr), parametersNR[r,]$frac[k,d,j,],
        col=d, lt=j)
}}
}


legend("topright",paste(rep("Habitat Type", J) ,c(1:J)),lt=c(1:J),cex=.8)
legend("topleft",c("Spawner","Egg","Fry","Parr","OnePlus-Y0"),lt=1, col=c(1:5),cex=.8)

} # end of sites

##################################################
# harvest.wild and harvest.hatc
for (k in 1:K) {

maxD = 1
minD = 0

plot(c(1:Tr), parametersNR[1,]$harvest.wild[k,], 
main=paste("Site",k,"Hatchery and Wild Fish Harvest Rates"),
ylim=c(minD, 1.4*maxD), type="l",
xlab="time", ylab="Harvest Rate")


for (r in 1:R) {
     lines(c(1:Tr), parametersNR[r,]$harvest.wild[k,], col="blue")
     lines(c(1:Tr), parametersNR[r,]$harvest.hatch[k,], col="red")
}

legend("topright",c("wild", "hatchery"),col=c("blue","red"),lt=1,cex=.8)
} # end of sites


#######################################
# Rel_Surv
lifestage = c("Spawner","Egg","Fry","OnePlus","Smolt","Adult (all ages)")

for (i in 1:6) {
for (k in 1:K) {
ul = rep(0,R)
for (r in 1:R) {ul[r] = max(parametersNR[r,]$Rel_Surv[k,,,])}
maxM = max(ul)

plot(c(1:Tr), 
parametersNR[1,]$Rel_Surv[k,1,,1],
 main=paste("Site",k,lifestage[i],"Rel Survival
Probability Across Genetetic Subtypes"),
ylim=c(0, 1.4*maxM), type="l",
xlab="time", ylab="Relative Survival PRob")

color=c(1,2,3,4,5,6,1,2,2,3,4)
lt= c(1,1,1,1,1,1,2,2,2,2,2)


for (r in 1:R) {
  for (g in 1:G) {
     lines(c(1:Tr), parametersNR[r,]$Rel_Surv[k,i,,g],
        col=color[g], lt=lt[g])
}}


legend("topright",c("N","H1","N.H2","H2","N-N.H3","N-H3","N.H3-H3","N.H3-H2","H3-H2",
"H3","N.H3-N.H3"), lt=lt,col=color, ncol=3, cex=.8)

} # end of sites
} # end of life stages

#######################################


#######################################
# Female_Fecundity
for (i in 1:6) {
for (k in 1:K) {

ul = rep(0,R)
for (r in 1:R) {ul[r] = max(parametersNR[r,]$Female_Fecundity[k,,,])}
maxM = max(ul)
maxM

plot(c(1:Tr), 
parametersNR[1,]$Female_Fecundity[k,1,,1],
 main=paste("Site",k, ", Ocean Age= ",i,": Fecundity of Female 
Spawners, by Genetic Sub-Type",sep=""),
ylim=c(0, 1.4*maxM), type="l",
xlab="time", ylab="Fraction Females")

color=c(1,2,3,4,5,6,1,2,2,3,4)
lt= c(1,1,1,1,1,1,2,2,2,2,2)


for (r in 1:R) {
  for (g in 1:G) {
     lines(c(1:Tr), parametersNR[r,]$Female_Fecundity[k,i,,g],
        col=color[g], lt=lt[g])
}}


legend("topright",c("N","H1","N.H2","H2","N-N.H3","N-H3","N.H3-H3","N.H3-H2","H3-H2",
"H3","N.H3-N.H3"), lt=lt,col=color, ncol=3, cex=.8)

} # end of sites
} # end of life stages

#######################################











##################################################
# Fry.x.siteMigration
colors=c(1,2,3,4,5,1,2,3,4,5)
ls = c(1,1,1,1,1,2,2,2,2,2)
for (k1 in 1:K) {
k2=1
plot(c(1:Tr), parametersNR[1,]$Fry.x.siteMigration[k1,k2,], 
main= paste("Fry: Cross Site Migration from Site",k1),
ylim=c(0, 1.4), type="l",
xlab="time", ylab="Cross Site Migration Fraction")


for (r in 1:R) {
 for (k2 in 1:K) {
     lines(c(1:Tr), parametersNR[r,]$Fry.x.siteMigration[k1,k2,],
        col=colors[k2], lt=ls[k2])
}}

legend("topright",paste(rep("To Site", K), c(1:K)) , col=colors[1:K],lt=lt[1:K],cex=.8,
ncol=2)

} # end of sites

##################################################
##################################################
# Par.x.siteMigration
colors=c(1,2,3,4,5,1,2,3,4,5)
ls = c(1,1,1,1,1,2,2,2,2,2)
for (k1 in 1:K) {
k2=1
plot(c(1:Tr), parametersNR[1,]$Par.x.siteMigration[k1,k2,], 
main= paste("Parr: Cross Site Migration from Site",k1),
ylim=c(0, 1.4), type="l",
xlab="time", ylab="Cross Site Migration Fraction")


for (r in 1:R) {
 for (k2 in 1:K) {
     lines(c(1:Tr), parametersNR[r,]$Par.x.siteMigration[k1,k2,],
        col=colors[k2], lt=ls[k2])
}}

legend("topright",paste(rep("To Site", K), c(1:K)) , col=colors[1:K],lt=lt[1:K],cex=.8,
ncol=2)

} # end of sites

##################################################
##################################################
# OnePlus.x.siteMigration
colors=c(1,2,3,4,5,1,2,3,4,5)
ls = c(1,1,1,1,1,2,2,2,2,2)
for (k1 in 1:K) {
k2=1
plot(c(1:Tr), parametersNR[1,]$OnePlus.x.siteMigration[k1,k2,], 
main= paste("OnePlus: Cross Site Migration from Site",k1),
ylim=c(0, 1.4), type="l",
xlab="time", ylab="Cross Site Migration Fraction")


for (r in 1:R) {
 for (k2 in 1:K) {
     lines(c(1:Tr), parametersNR[r,]$OnePlus.x.siteMigration[k1,k2,],
        col=colors[k2], lt=ls[k2])
}}

legend("topright",paste(rep("To Site", K), c(1:K)) , col=colors[1:K],lt=lt[1:K],cex=.8,
ncol=2)

} # end of sites

##################################################
##################################################
# Spanwer.x.siteMigration
colors=c(1,2,3,4,5,1,2,3,4,5)
ls = c(1,1,1,1,1,2,2,2,2,2)
k2=1

for (k1 in 1:K) {
plot(c(1:Tr), parametersNR[1,]$Spawner.x.siteMigration[k1,k2,], 
main= paste("Spawner: Cross Site Migration from Site",k1),
ylim=c(0, 1.4), type="l",
xlab="time", ylab="Cross Site Migration Fraction")


for (r in 1:R) {
 for (k2 in 1:K) {
     lines(c(1:Tr), parametersNR[r,]$Spawner.x.siteMigration[k1,k2,],
        col=colors[k2], lt=ls[k2])
}}

legend("topright",paste(rep("To Site", K), c(1:K)) , col=colors[1:K],lt=lt[1:K],cex=.8,
ncol=2)

} # end of sites

##################################################











