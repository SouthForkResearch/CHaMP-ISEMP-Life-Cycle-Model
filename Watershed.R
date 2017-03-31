#
####################################################
# ISEMP Watershed Production Model
# R-Code Written by Matt Nahorniak, South-Fork Research
# matt@southforkresearch.org
# (541) 740-5487
# Pete McHugh's John Day 2015-2016 tweaks are included
# peter.a.mchugh@gmail.com 
#####################################################
rm(list = ls(all=TRUE)) #Clean up shop before beginning

	library(MCMCpack, quietly=T) # for Dirichlet (multivariable beta) distributions
	library(VGAM, quietly=T) # Library with posnorm functions (positiive only normal)

# Load R-Scripts in Different Files
	source("Watershed_ReadData.r")
	source("Watershed_MonteCarlo.r")
	source("Watershed_BevHolt.r")
	source("Watershed_Post_Processing.r")
#  source("Watershed_JohnDayOutputs.r") #Only use this if you're interested; it's only been vetted well for single pop/G-type situations

	# Reconstruction
	source("Risk.Plots.r")

# Read Header and Input Files
	print("Reading Header Files")
	header<- Read.Header("Watershed_Header_File.csv") 
	Inputs<-Read.Input.File(header)

# Initialze some Global Variables
# NR is global variable containing all counts N in run R
#NR = Abundance for site k, stage i, time t, life stage g, run R
#NR_F = fraction of NR that are females
#N5R = NR except just for OnePlus
#N5R_F = fraction of N5R that are females (a fraction)
#SmoltsR = OnePlus attempting to smolt (# pre Bev-Holt survival)
	NR=with(header, {array(rep(0,K*I*Tr*G*R),c(K,I,Tr,G,R))})
      NR_F=with(header, {array(rep(0,K*I*Tr*G*R),c(K,I,Tr,G,R))}) 
      N5R=with(header, {array(rep(0,K*I5*Tr*G*R), c(K,I5,Tr,G,R))})
      N5R_F=with(header, {array(rep(0,K*I5*Tr*G*R), c(K,I5,Tr,G,R))})
      Candidate_Smolt_NR = with(header, {array(rep(0, K*Tr*G*R), c(K,Tr,G,R))})      
      Female_Spawners_NR = with(header, {array(rep(0, K*Tr*G*R), c(K,Tr,G,R))})

	Male_Spawners_NR = with(header, {array(rep(0, K*Tr*G*R), c(K,Tr,G,R))})
	Bonneville_NT = with(header, {array(rep(0, K*10*Tr*G*R), c(K,10,Tr,G,R))})
	Candidate_Smolt_ByAge = with(header, {array(rep(0,K*I5*Tr*G*R), c(K,I5,Tr,G,R))})
	Resident_Spawners_NR=with(header, {array(rep(0, K*Tr*G*R), c(K,Tr,G,R))})
	ResidentN=with(header, {array(rep(0, K*Tr*G*R), c(K,Tr,G,R))})
	ResidentSexRatio=with(header, {array(rep(0, K*Tr*G*R), c(K,Tr,G,R))})
	Prod=with(header, {array(rep(0,K*I*Tr*R),c(K,I,Tr,R))})
	Cap=with(header, {array(rep(0,K*I*Tr*R),c(K,I,Tr,R))})

	Spawners_NR = with(header, {array(rep(0, K*Tr*G*R), c(K,Tr,G,R))})
	Escapement_NR = with(header, {array(rep(0, K*Tr*G*R), c(K,Tr,G,R))})
	Female_Escapement_NR = with(header, {array(rep(0, K*Tr*G*R), c(K,Tr,G,R))})
	N_RECRUITS_NR = with(header, {array(rep(0, Tr*11*R), c(Tr,11,R)) })
	N_SPAWNER_RECRUIT_NR = with(header, {array(rep(0, Tr*2*R), c(Tr,2,R)) })

####################################################################
# Here we loop through the code one for each monte-carlo simulation
# First we set the variables, then we add stochasticity via a monte-
# carlo code, then run the Beverton-Holt algorithm
r=1
for (r in 1:header$R) {
	cat(paste('Iteration #',r), "\n")
		variables=Initialize.Variables(header)
		parameters=MonteCarlo(Inputs, variables, header)
		results= BevHolt(parameters, variables, header) 

  	# Store results for Each Iteration
	NR[,,,,r]=results$N
	N5R[,,,,r]=results$N5
	Female_Spawners_NR[,,,r] = results$Female_Spawners
	Spawners_NR[,,,r] = results$Spawners
	Escapement_NR[,,,r] = results$Escapement
	Female_Escapement_NR[,,,r] = results$Female_Escapement

	N_RECRUITS_NR[,,r] = results$N_RECRUITS
	Candidate_Smolt_NR[,,,r] = results$Candidate_Smolt

  } # End of Loop through MC Iteration
################################################################

PostProcessing(header, NR, N5R, Female_Spawners_NR) 

for (r in 1:header$R) {
	# in second dimension: spawner=1, recruit=2 
	for (t in 1:header$Tr) {
		N_SPAWNER_RECRUIT_NR[t,1,r] = sum(Female_Spawners_NR[,t,,r])

		for (age in 1:11) {
			# this test ensures that the adult's brood year are not prior to the start of the simulation.
			if (age < t) {
				N_SPAWNER_RECRUIT_NR[t-age,2,r] = N_SPAWNER_RECRUIT_NR[t-age,2,r] + N_RECRUITS_NR[t,age,r]
			}
		}
	}	
}


#print (N_SPAWNER_RECRUIT_NR)

Reconstruction(header)

####################################################################
# Generate plot for coefficient of variation vs number of MC iterations

if (header$R > 1) {
  if (header$K==1) {mar = 2} else {mar = 3}
  r.range=2:200
  escapement_Mean = mean(apply(Escapement_NR[,header$Tr-1,,],mar,sum)) #to correct error, Pete changed apply argument 2 to 2 (from 3)
  escapement_SD = sd(apply(Escapement_NR[,header$Tr-1,,],mar,sum))
  escapement_se_by_r = escapement_SD/sqrt(r.range)
  escapement_COV_by_r = escapement_se_by_r/ escapement_Mean
  OnePlus_Cand_mean = mean(apply(Candidate_Smolt_NR[,header$Tr-1,,],mar,sum))
  OnePlus_Cand_SD = sd(apply(Candidate_Smolt_NR[,header$Tr-1,,],mar,sum))
  OnePlus_Cand_se_by_r= OnePlus_Cand_SD/sqrt(r.range)
  OnePlus_Cand_COV_by_r= OnePlus_Cand_se_by_r /  OnePlus_Cand_mean
  
  plot(r.range, escapement_COV_by_r, type="l", 
  main="Coefficient of Variation for Total Escapement and
  Total OnePlus, by Number of MC Iterations" ,
  xlab= "Number of MC Iterations",
  ylab="Coefficient of Variation",
  ylim=c(0, 1))
  lines(r.range, OnePlus_Cand_COV_by_r, lt=2)
  legend("topright", c("Escapement", "Smolts (Pre-Dam Passage)"), lt=c(1,2))
}

# results$Rainbow_N[1,,1:25,1]
# results$Rainbow_N_F[1,,1:25,1]









