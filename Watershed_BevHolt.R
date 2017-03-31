BevHolt <- function(Parameter.data, Var.data, header.data) {

# for debugging uncomment out thees two)
#attach(variables) 
#attach(parameters)
#attach(header)

#detach(variables)
#detach(parameters)
#detach(header)

attach(Var.data)
attach(Parameter.data)
attach(header.data)

# Move to initialization section
Fry_Migrate.T=rep(0,K)
Par_Migrate.T=rep(0,K)
OnePlus_Migrate.T=rep(0,K)
OnePlusN5_Migrate.T = array(rep(0, K*I5), c(K, I5))
Candidate_Smolt=array(rep(0,K*Tr*G), c(K,Tr,G))

Rainbow_Spawners = array(rep(0, K*Tr*G), c(K,Tr,G))
Rainbow_Female_Spawners  = array(rep(0, K*Tr*G),c(K,Tr,G))

# Maximum number of ocean years (0-5, totalling six years)
OCEAN_AGES = 6
# Maximum number of fresh water years (0-5, totalling six years)
FRESH_AGES = 6
AGE = FRESH_AGES+OCEAN_AGES-1

N_FISH = array(0, c(K,I,Tr,G,K,2)) 
N_RAINBOW = array(0, c(K,I,Tr,G,K,2))
N_SPAWNERS = array(0, c(K,Tr,G,2))
N_RAINBOW_SPAWNERS = array(0, c(K,I5,Tr,G,2))
N_ESCAPEMENT = array(0, c(K,Tr,G,2))

# ** AGE RECONSTRUCTION.  Smolts array is only needed to construct Adults array.
# For all adults, store distribution of fresh water age and true age
N_ADULTS = array(0, c(K,FRESH_AGES,Tr,G,2,OCEAN_AGES)) 
# For smolts, keep track of years in fresh water (1 for Chinook, 1-6 for Rainbow)
N_SMOLTS = array(0, c(K,FRESH_AGES,Tr,G,2))
# For adults that make it back to spawn, keep track of them all by true
N_RECRUITS = array(0, c(Tr,AGE))

N5_FISH = array(0, c(K,I5,Tr,G,K,2)) 
NT_FISH = array(0, c(K,OCEAN_AGES,Tr,G,2))
 
#Initialize new array
for (k in 1:K) {
      N5_FISH[k,,,,k,1]=N5[k,,,] *.5
      N5_FISH[k,,,,k,2]=N5[k,,,] *.5
}

#Initialize these
Post_Spawn_Returns_Anadromous = array(0, c(K,OCEAN_AGES,G,2))
Post_Spawn_Returns_Rainbow = array(0, c(K,I5,G,2))

#######################################################
#######################################################
###################################################
# Start Time Loops (t=1 to Tr) for each MC iteration
####################################################

for (k in 1:K) {
      N_FISH[k,,,,k,1]=N[k,,,] *.5
      N_FISH[k,,,,k,2]=N[k,,,] *.5
}

# For year one, ocean age 0-5, smolt-year 1 <- fish at stage 7-12
for (t in 1:Tr) {
for (k in 1:K) {
	for (g in 1:G) {
		for (y in 1:OCEAN_AGES ) {  # need to handle Year 0 returnees
			# ** AGE RECONSTRUCTION. We will guess that all ocean fish spent
			#    one year in fresh water (fresh age=2)and their ocean age is 1-6 (depending on ocean stage)
			N_ADULTS[k,2,t,g,1,y] = sum(N_FISH[k,y+6,t,g,,1])
			N_ADULTS[k,2,t,g,2,y] = sum(N_FISH[k,y+6,t,g,,2])
		}
	}
}
}

t=2
#for (t in 2:5) {
for (t in 2:(Tr-1)){
  
      print(paste("t=",t))
      
      # calculate A: Total Area by watershed K, for time t
      # Lots of stuff done within as this may become time dependent at some point,
      # but I don't want to carry everything in memory for a time periods.
      
      # Area for each site.
      A = rep(0, K)
      
      for (k in 1:K) { 
            A[k] = sum(Ak_x_Lqk[k,1:Q,t])
      }
      
    	# Figure out % in each land use area
    	for (k in 1:K) {
      	for (q in 1:Q) {
			L[k,q,t] =  Ak_x_Lqk[k,q,t]/max(A[k],.0000000000001)
		}
      }
      
      # H[k,j,t] (Equation 13)
      for (k in 1:K) {
            for (j in 1:J) {
      		sum=0
                  for (q in 1:Q) {
                        sum=sum+(M[k,q,j,t]*L[k,q,t])
                  }
                  H[k,j,t]= A[k]*sum
            }
      
      }
      
      # D is capacity per sq. meter
    	# Note: per document description of table 2-4, assume nearly "infinite"
    	# density and resulting capacity for fry-to-par, and thus only productivity
    	# parameter will affect this life stage cycle.  But this way, the hooks are
    	# in to add this density when appropriate
      
    	# Capacity by life-stage i, in watershed k, at time t (Equation 14)
      
      
      for (k in 1:K) {
            for (i in 1:6){
			c[k,i,t]=
				(sum(H[k,,t]*D[k,,i,t]*frac[k,i,,t])+.0000000000000001)
            }
      }
      
      
      # OK, we have carrying capacity for first four stages:
      # spawning, egg, fry, OnePlus     
            
      #calculate productivity for time t, site k
      for (k in 1:K) {
            for (i in 1:6) {
            # Note the (i+1) in the Sr below.  For outdated reasons, the first entry
            # in that array is a zero, so the i+1 is to fix the indexing error.  Sloppy.
            # will multiply be Rel_Fecund at egg stage, but still need a productivity
                p[k,i,t] = Sr[k,(i+1),t]*(sum(Prod_Scalar[k,,i,t]*L[k,,t])/(sum(L[k,,t])+.0000000000001))
            }	
      }
      
      ##############################################################
      # Cycle through sites
      ##############################################################
   
      k=1
      for (k in 1:K) { 

            # starting at time step 2.... so I'm going to start at adult, mature fish
            # from time step 1 to calculate number of spawners and adjust adult
            # fish counts from back in step 1.  Gotta start somewhere...


ADULTS1 = array(0, 6)
ADULTS2 = array(0, 6)
if (0) {
print("START")
for (jj in 1:5) {
	ADULTS1[jj] = sum(N_FISH[,jj+7,t-1,,,])
	ADULTS2[jj] = sum(N_ADULTS[,,t-1,,,jj+1])
}
print(ADULTS1)
print(ADULTS2)
}           
            ###############
            # N8 through N12: ocean ages 1 ->5 (taking into account loss due to maturation rates of prior years)

            for (i in 7:11) {
                  CandN_M = apply(N_FISH[k,i,t-1,,,1], 1, sum) * (1-Mat8Plus_M[k,i-6,t-1])
			CandN_F = apply(N_FISH[k,i,t-1,,,2], 1, sum) * (1-Mat8Plus_F[k,i-6,t-1])

			# Mat8Plus_M/F -- user specified input for the fraction of M or F fish that mature 
			#   (i.e. are going to try to spawn)at each adult age.  The adult age is i-7, but the
			#   array needs to handle values 0-5, indexes 1-6.
			# CandN_M/F -- The number of fish trying to remain as adults for another year
			#   the number of adult fish of each age times (1-%fish that are leaving to spawn)

                  # Here's where I should be adding the surviving spawners???  Otherwise,
                  # 1-Mat8Plus gets rid of them all.....  initialize to zero than carry from
                  # previous time loop and add here.  Do similar for resident rainbow.
                  
                  CandN = CandN_M+ CandN_F
                  for (g in 1:G) {
				MALE_PERCENT = CandN_M[g]/CandN[g] 
				MALE_PERCENT[is.na(MALE_PERCENT)] = 0

				# Ocean capacity is "infinite"
				# Sr -- density independent survival. 
				Candidates = (CandN[g]*Rel_Surv[k,i,t-1,g]) * Sr[k,i+1,t-1]
				Candidates [is.na(Candidates )] = 0

				# Calculate how many male and female fish stick around for another
				# 	year as adult and age one year.

				N_FISH[k,i+1,t,g,k,1]= MALE_PERCENT * Candidates 
				N_FISH[k,i+1,t,g,k,2]= (1-MALE_PERCENT) * Candidates 

				# For reconstruction, maintain "fresh water age" distribution
				for (jj in 1:FRESH_AGES) {
					FWA_DISTRIBUTION = sum(N_ADULTS[k,jj,t-1,g,,])/sum(N_ADULTS[k,,t-1,g,,])
					FWA_DISTRIBUTION[is.na(FWA_DISTRIBUTION)] = 0
					N_ADULTS[k,jj,t,g,1,i-5] = MALE_PERCENT * Candidates * FWA_DISTRIBUTION
					N_ADULTS[k,jj,t,g,2,i-5] = (1-MALE_PERCENT) * Candidates * FWA_DISTRIBUTION					
				}
                  } 
            } # end of i in (7:11)

if (0) {
print("Stayers")
for (jj in 1:5) {
	ADULTS1[jj] = sum(N_FISH[,jj+7,t,,,])
	ADULTS2[jj] = sum(N_ADULTS[,,t,,,jj+1])
}
print(ADULTS1)
print(ADULTS2)
}
            ## Add back surviving spawners from before...      
            for (i in 1:OCEAN_AGES){

			# Steelhead spawners can return to become adults and then spawn again.
			# ** AGE RECONSTRUCTION. Ignore 

			N_FISH[k,i+6,t,,k,] = N_FISH[k,i+6,t,,k,] + Post_Spawn_Returns_Anadromous[k,i,,]
			for (jj in 1:FRESH_AGES) {
				FWA_DISTRIBUTION = sum(N_ADULTS[k,jj,t-1,g,,])/sum(N_ADULTS[k,,t-1,g,,])
				FWA_DISTRIBUTION[is.na(FWA_DISTRIBUTION)] = 0
				for (gg in 1:G) {
					for (z in 1:2) {
						N_ADULTS[k,jj,t,g,z,i-5] = N_ADULTS[k,jj,t,g,z,i-5] + (Post_Spawn_Returns_Anadromous[k,i,g,z]* FWA_DISTRIBUTION)
					}
				}				
			}
            }     
if (0) {
print("Add returners")
for (jj in 1:5) {
	ADULTS1[jj] = sum(N_FISH[,jj+7,t,,,])
	ADULTS2[jj] = sum(N_ADULTS[,,t,,,jj+1])
}
print(ADULTS1)
print(ADULTS2)
}           
            ############
            # NT: Mature Fish (ready to Spawn, calc's as % of ocean ages 0-5, indexes 1-6)
          	for (i in 7:I) {

                  CandN_M = apply(N_FISH[k,i,t,,,1],1,sum) * (Mat8Plus_M[k,i-6,t]) 
                  CandN_F = apply(N_FISH[k,i,t,,,2],1,sum) * (Mat8Plus_M[k,i-6,t])
                  CandN = CandN_M+ CandN_F
  
			# CandN are the fish leaving the ocean.

                  for (g in 1:G) {
				# NT_FISH -- mature fish (mature meaning "trying to spawn")
				NT_FISH[k,i-6,t,g,1] = CandN_M[g] * Rel_Surv[k, i,t,g]
				NT_FISH[k,i-6,t,g,2] = CandN_F[g] * Rel_Surv[k, i,t,g]
                  }
            }
      } # End of cycle through K's for mature fish....

	N_FISH[is.na(N_FISH)] = 0 
      
      ###########################################################################
      ################### apply Cross-Site Migration for Spawners NT #############
      ### Will need to adjust F's and imprints every time I cross site-migrate!!
      ### Except for the spawners...they're allowed to re-imprint, by definition
      #####################

	NT_FISH_MIGRATE = array(rep(0, K*OCEAN_AGES*G*2),c(K, OCEAN_AGES, G, 2))
      
      for (k1 in 1:K) {
            for (k2 in 1:K){
                  NT_FISH_MIGRATE[k1,,,] = NT_FISH_MIGRATE[k1,,,] + 
	 			NT_FISH[k2,,t,,] *Spawner.x.siteMigration[k2, k1,t]		
            }
      }
     
	NT_FISH[,,t,,] = apply(NT_FISH_MIGRATE[,,,], c(1,2,3,4), sum)

	# calculate recruits for reconstruction
	for (jj in 1:FRESH_AGES) {
		FWA_DISTRIBUTION = sum(N_ADULTS[,jj,t-1,,,])/sum(N_ADULTS[,,t-1,,,])
		FWA_DISTRIBUTION[is.na(FWA_DISTRIBUTION)] = 0
		for (kk in 1:OCEAN_AGES) {
			N_RECRUITS[t,jj+kk-1] = N_RECRUITS[t,jj+kk-1] +
							sum(NT_FISH[,kk,t,,] * FWA_DISTRIBUTION)
		}				
	}
if (0) {
print(apply(NT_FISH[,,t,,],2,sum))
print(N_RECRUITS[t,])
}	
      #### OK, now I've got NT_FISH - the fish attempting to spawn by age and G-type
      #### I should be able to figure out fecundity for each group and come up
      #### eggs by G-type
      ### apply harvest rates
      ### apply survival rates
      ### go through one G-type at a time and figure out how many of each offspring 
      ### type it'll make assuming males distributed according to their distribution,
      ### then add up across all G-types
      ### Add in rainbow spawners
      ### That is all
      
      #### Unlike below, we won't  use N1 by itself, as we need to keep track of
      #### age and G-type.  
      #### Re-Do all this below
      
      
      ############################################################################
      for (k in 1:K) {
            for (g in 1:G){
                  if (g==2) { 
				NT_FISH[k,,t,g,] = NT_FISH[k,,t,g,] * max(0,(1-harvest.hatch[k,t]))
                  } else {
				NT_FISH[k,,t,g,] = NT_FISH[k,,t,g,] * max(0,(1-harvest.wild[k,t]))
                  }

                  ##############################################
                  #Now NT is represents spawners for anadromous - survived and will try to spawn and, in O.Mykiss, perhaps
                  #even return to where they came from
                  #
                  
			N_ESCAPEMENT[k,t,g,1] =  sum(NT_FISH[k,,t,g,1])
			N_ESCAPEMENT[k,t,g,2] =  sum(NT_FISH[k,,t,g,2])
			N_FISH[k,1,t,g,k,1] = sum(NT_FISH[k,,t,g,1])
			N_FISH[k,1,t,g,k,2] = sum(NT_FISH[k,,t,g,2])
            } # end of "for (g in...)"
      
            # The array below gives the g-number for the offspring for mothers 
            # columns) and father (rows)
            g.index = array(
            c(1,	3,	5,	6,	1,	1,	1,	5,	5,	1,	1,
            3,	4,	8,	9,	3,	3,	3,	8,	8,	3,	3,
            5,	8,	11,	7,	5,	5,	5,	11,	11,	5,	5,
            6,	9,	7,	10,	6,	6,	6,	7,	7,	6,	6,
            1,	3,	5,	6,	1,	1,	1,	5,	5,	1,	1,
            1,	3,	5,	6,	1,	1,	1,	5,	5,	1,	1,
            1,	3,	5,	6,	1,	1,	1,	5,	5,	1,	1,
            5,	8,	11,	7,	5,	5,	5,	11,	11,	5,	5,
            5,	8,	11,	7,	5,	5,	5,	11,	11,	5,	5,
            1,	3,	5,	6,	1,	1,	1,	5,	5,	1,	1,
            1,	3,	5,	6,	1,	1,	1,	5,	5,	1,	1), c(11,11))
            
            
		# Calculate distribution of male geno-types (hatch fish)
            Male_GDist=apply(NT_FISH[k,,t,,1],2,sum)/(sum(NT_FISH[k,,t,,1])+.00001)
            Male_GDist[is.na(Male_GDist)] = 0
      
             #####################################################################################
             ##### Begin Pete Feb 2016 Meddling
             #####################################################################################      

            Rainbow.Male_GDist = 
            		apply(N_RAINBOW_SPAWNERS[k,,t,,1],2,sum)/(sum(N_RAINBOW_SPAWNERS[k,,t,,1])+.00001)

             #####################################################################################
             ##### End Pete Feb 2016 Meddling
             #####################################################################################      

            Rainbow.Male_GDist[is.na(Rainbow.Male_GDist)] = 0
            
            
            ################################
            # Calculate fish that will return after surviving trip to spawn (SR * Rel_Surv) and then surviving the actual spawning and return journy (PSSA and PSSR)
            # And add them back into the current year at OnePlus stages (for rainbow) and adult stages(for anadromous)
                      
            for (i in 1:(OCEAN_AGES-1)){
                  Post_Spawn_Returns_Anadromous[k,i+1,,1]=   NT_FISH[k, i, t,,1] *  (Sr[k,2,t])* Rel_Surv[k,1,t,] * Post_Spawn_Survival_Anadromous_M[k,i,t,] 
                  Post_Spawn_Returns_Anadromous[k,i+1,,2]=   NT_FISH[k, i, t,,2] *  (Sr[k,2,t])* Rel_Surv[k,1,t,] * Post_Spawn_Survival_Anadromous_F[k,i,t,] 
            }
      
            for (i5 in 1:I5){       #Pete Nov 2015
                  Post_Spawn_Returns_Rainbow[k,i5,,1]=   N_RAINBOW_SPAWNERS[k, i5, t-1,,1] *  (Sr[k,2,t])* Rel_Surv[k,1,t,]* Post_Spawn_Survival_Rainbow_M[k,i5,t,] 
                  Post_Spawn_Returns_Rainbow[k,i5,,2]=   N_RAINBOW_SPAWNERS[k, i5, t-1,,2] *  (Sr[k,2,t])* Rel_Surv[k,1,t,]* Post_Spawn_Survival_Rainbow_F[k,i5,t,] 
                  
			N5_FISH[k,i5,t,,k,1] = N5_FISH[k,i5,t,,k,1] + Post_Spawn_Returns_Rainbow[k,i5,,1]
                  N5_FISH[k,i5,t,,k,2] = N5_FISH[k,i5,t,,k,2] + Post_Spawn_Returns_Rainbow[k,i5,,2]
            } # end i5
     
            #############################################
                      
            # Calculating These since they're an output later on.... not used in later calculations.
            # Note:  this is just used for reporting purposes.  The actual surival and
            # capacity and productivity impacts are all linked in the escapement to egg
            # calculation, which doesn't explicity account for "spawners" before they
            # lay their eggs

            for (g in 1:G) {
			N_SPAWNERS[k,t,g,] = (N_ESCAPEMENT[k,t,g,]) * Sr[k,(2),t] * Rel_Surv[k,(1),t,g]

                  Rainbow_Spawners[k,t,g] =
				apply(N_RAINBOW_SPAWNERS[k,,t,,], 2, sum)[g]  * Sr[k,(2),t] * Rel_Surv[k,(1),t,g]
                  Rainbow_Female_Spawners[k,t,g] = 
				(apply(N_RAINBOW_SPAWNERS[k,,t,,2], 2, sum)[g]) * Sr[k,(2),t] * Rel_Surv[k,(1),t,g]
            }
            
            ######################################
            ## Now calculate "candidate eggs"
      
            Cand.Egg.N=rep(0,G)
            Rainbow.Cand.Egg.N = Cand.Egg.N*0
      
            # Speed Improvement
            # could take out for (i in 1:OCEAN_AGES) and for (i5 in 1:I5), below, easily
            #for (k in 1:K) {
            for (g1 in 1:11) {
                  for (g2 in 1:11) {
                        for (i in 1:OCEAN_AGES) {    # cycle through all adult ages possible
                              Cand.Egg.N[g.index[g1,g2]]= Cand.Egg.N[g.index[g1,g2]]+
                              	Female_Fecundity[k,i,t,g1]*NT_FISH[k,i,t,g1,2]*Male_GDist[g2] 
                        }
                              
                        for (i5 in 1:I5) { # cycle through all OnePlus spawner ages
					Rainbow.Cand.Egg.N[g.index[g1,g2]] = Rainbow.Cand.Egg.N[g.index[g1,g2]]+
						(N5.Rainbow.Fecundity[k,i5,t] * 
						(N_RAINBOW_SPAWNERS[k,i5,t,g1,2])*Rainbow.Male_GDist[g2])
                        }
                  }
            } # end of "for (g1, g2...)"

      
            ##############################################
            # Assume eggs are equal size... distribute capacity equally across
            # all g-types, and apply BH equation separately for each g-type to
            # figure out number of eggs by g-type.
            
            # Added "divide by Sr[k,2,t] so spawner-egg survival isn't double counted (along with adult-to-spawner)
            # Oops.  That was a screw up.  Rs[k,2,t] isn't used prior to this, since I don't
            # use "spawners" but NT to calculate candidate eggs.

            # Eggs!  Chinook Steelhead Only (TBD)

            for (g in 1:G) {
			N_FISH[k,2,t,g,k,] = .5 * Cand.Egg.N[g]*Rel_Surv[k,1,t,g] /
                  				(1/(p[k,1,t])+  1/c[k,1,t]* sum(Rel_Surv[k,1,t,]* Cand.Egg.N))
            }
      
            ##############################
            # Eggs (Rainbow Spawners) # Don't yet compete for same space
            # Eggs!  Steelhead Only (TBD)
            for (g in 1:G) {
			Candidates = Rainbow.Cand.Egg.N[g]*Rel_Surv[k,1,t,g] /
						(1/(p[k,1,t]) + 1/c[k,1,t] * sum(Rel_Surv[k,1,t,]* Rainbow.Cand.Egg.N))
			N_RAINBOW[k,2,t,g,k,] = .5 * Candidates 
            }
      } # end K
      
	############
	#N3 (fry, based on prior year's eggs) 
	###########

      for (k in 1:K) {
          	for (g in 1:G) {
 		
			FrySurvivors = sum(N_FISH[k,2,t,g,,])*Rel_Surv[k,2,t,g] / 
                     (1/p[k,2,t]+ 1/c[k,2,t] * 
                             sum(Rel_Surv[k,2,t,]*apply(N_FISH[k,2,t,,,],1,sum)))
              
			# Same M/F ratio as prior step
   			MALE_PORTION = sum(N_FISH[k,2,t,g,,1])/sum(N_FISH[k,2,t,g,,])
			MALE_PORTION[is.na(MALE_PORTION)] = 0

                  if (is.na(MALE_PORTION)) MALE_PORTION = 0

			N_FISH[k,3,t+1,g,k,1] = MALE_PORTION * FrySurvivors 
			N_FISH[k,3,t+1,g,k,2] = (1 - MALE_PORTION) * FrySurvivors 
            } # of of g
      } # end of K for fry

      N_RAINBOW[,3,t+1,,,] = N_RAINBOW[,3,t+1,,,]*0
       
      for (k in 1:K) {
		for (g in 1:G) {
			# No hatchery fish to add for rainbow spanwers....Assume they're added with (and therefore compete
			# with) anadromous fish.

			Candidates = sum(N_RAINBOW[k,2,t,g,,] * Rel_Surv[k,2,t,g] / 
                  	(1/p[k,2,t]+ 1/c[k,2,t] * sum(Rel_Surv[k,2,t,] * apply(N_RAINBOW[k,2,t,,,], 1, sum))))
			Candidates[is.na(Candidates)] = 0

			MALE_PORTION = N_RAINBOW[k,3,t+1,g,k,1] / (sum(N_RAINBOW[k,3,t+1,g,k,]))
			MALE_PORTION[is.na(MALE_PORTION)] = 0

			N_RAINBOW[k,3,t+1,g,k,1] = MALE_PORTION * Candidates
			N_RAINBOW[k,3,t+1,g,k,2] = (1 - MALE_PORTION) * Candidates
		}
	} # end of K for fry
                  
      ###################################################
      # Now we've calculated pops seperately for rainbow and anadromous fish,
      # let's combine them before they try to become parr (and migrate them
      # together as well

      N_FISH[,3,t+1,,,1] = N_FISH[,3,t+1,,,1] + N_RAINBOW[,3,t+1,,,1] 
      N_FISH[,3,t+1,,,2] = N_FISH[,3,t+1,,,2] + N_RAINBOW[,3,t+1,,,2]
      
      ################ Move X Site Fry Migration Here for Parr##############
      ################### apply Cross-Site Migration for Fry #############
      
      if (K >1) {
            # Initialize Migration matrics: to k1 from k2 imprinted to k3
		N_FRY_Migrate = array(0, c(K,K,K,G,2)) # to k1 from k2 imprinted to k3
            
            for (k1 in 1:K) {
                  for (k2 in 1:K){
                        for (k3 in 1:K) {
					N_FRY_Migrate[k1, k2, k3, ,] = 
						N_FISH[k2,3,t+1,,k3,] * Fry.x.siteMigration[k2, k1,t+1]
                        }
                  }
            }

            # Re-assign to N and N_Imprint for male and female
            for (k in 1:K) {
                  for (g in 1:G) {
                        N_FISH[k,3,t+1,g,,] = apply(N_FRY_Migrate[k,,,g,],c(2,3),sum)
                  }
            }
      } # end of x-site-migrate
      
      ###########
      #N4 (par, based on same year's fry)
      ###########
      
	ParSurvivors = array(0, G)
	MALE_PORTION = array(0, G)
          
      for (k in 1:K) {
		ParSurvivors = apply(N_FISH[k,3,t,,,],1,sum)*Rel_Surv[k,3,t,] / 
                    (1/p[k,3,t]+ 1/c[k,3,t] * 
                            sum(Rel_Surv[k,3,t,]*apply(N_FISH[k,3,t,,,],1,sum)))

		for (kk in 1:K) {
			# divvy up ParSurvivors per gender and imprint location as in previous step
                  PORTION = sum(N_FISH[k,3,t,,kk,1])/sum(N_FISH[k,3,t,,,])
                  if (is.na(PORTION)) PORTION  = 0
            	N_FISH[k,4,t,,kk,1] = ParSurvivors * PORTION 
                  PORTION = sum(N_FISH[k,3,t,,kk,2])/sum(N_FISH[k,3,t,,,])
                  if (is.na(PORTION)) PORTION  = 0
            	N_FISH[k,4,t,,kk,2] = ParSurvivors * PORTION
		}

		# NEW Add in hatchery fish
		N_FISH[k,4,t,2,k,] = N_FISH[k,4,t,2,k,] + 0.5* Hatch_Fish[k,4,t] 
      } # end of K for parr
      
      
      #####################################################################
      #Move X Site Migration of Parr here for OnePlus
      ################ Move X Site Fry Migration Here for Parr##############
      ################### apply Cross-Site Migration for Par #############
      
      ################## apply Cross-Site Migration for Fry #############
      #Par.x.siteMigration[, ,t] = t(array(c(.9,.1,0,0,.95, .05, 0, .3, .7), c(3,3)))
      #Par.x.siteMigration[, ,t]
      if (K >1) {
            
            # Initialize Migrattion matrics: to k1 from k2 imprinted to k3
            Par_Migrate = array(0, c(K,K,K,G)) # to k1 from k2 imprinted to k3
            Par_Migrate_F = array(0, c(K,K,K,G)) # to k1 from k2 imprinted to k3
            N_Par_Migrate = array(0, c(K,K,K,G,2)) # to k1 from k2 imprinted to k3
            
            for (k1 in 1:K) {
                  #for (g in 1:G) {
                  for (k2 in 1:K){
                        for (k3 in 1:K) {
					N_Par_Migrate[k1, k2, k3, ,] =
						N_FISH[k2,4,t,,k3,] *Par.x.siteMigration[k2, k1,t]
                       }
                  }
            }
            
            for (k in 1:K) {
                  for (g in 1:G){
                        N_FISH[k,4,t,g,,] = apply(N_Par_Migrate[k,,,g,],c(2,3),sum)
                  }
            }

      } # end of x-site migrate
      
      ###############################################################
      ###############################################################
      # OnePlus -- Steelhead only
      # N5 (OnePlus)
      # N5 is complicated because of steelhead
      # N5 is based on number of pars entering system and number of prior
      # year's OnePlus, at each age, staying for another year
      # Capacity is weighted based on age distribution and survival probability
      # of fish trying to occupy given space (older fish require more space)
      ###########

	if (species == "steelhead") {		
      	Cand_M = array(rep(0, K*G*K*I5), c(K,G,K,I5)) #in K1, G, imprint K2, i5, Sr, Cap_Scale)
      	Cand_F = Cand_M
		Cand_M[,,,1] = N_FISH[,4,t,,,1]
      	Cand_F[,,,1] = N_FISH[,4,t,,,2]

      	for (k in 1:K) {
            	for (i5 in 2:I5) {
				Cand_M[k,,,i5] = N5_FISH[k,i5-1,t,,,1] * N5.Pstay_M[k,i5-1,t]
 				Cand_F[k,,,i5] = N5_FISH[k,i5-1,t,,,2] * N5.Pstay_F[k,i5-1,t]
            	}
      	}

      	### Figure out Candidate Equivalent First Year Fish (i.e. adjusted by cap scalar)
      	### Note:  the (3-1*(K==1)) garbage is due to the fact that R has the
      	### annoying "feature" of dropping a dimension if it's of length 1
      
      	Cand_Y1_Equivalent = rep(0, K)
      	for (k in 1:K) {
            	Cand_Y1_Equivalent[k] = sum(
            		apply(Cand_M[k,,,],(3-1*(K==1)), sum)*N5.cap[k,,t] +
            		apply(Cand_F[k,,,],(3-1*(K==1)), sum)*N5.cap[k,,t] )
      	}
      
      	### Figure out Candidate Fish Weight Survival Probability Average and Productivity
      
      	SR5.Candidate = rep(0, K)
      	pN5 = rep(0,K)
      
      	# Use temp.Cand_x to multiply Cand_x by surivival probabilities to get
      	# relative abundnace of each type, by gen-subtype, gender, imprint, which will later
      	# be scaled to match actual equivalent 1st year abundance
      
      	temp.Cand_M = Cand_M*0
      	temp.Cand_F = Cand_F*0
      
      	for (k in 1:K) {
            	dim(Cand_M)
            	# Weighted average for Survival (SR5.Candidate)
            	# And future relative proportion of candidate fish after Bev-Holt application
            	SR_Sum = 0
            	for (i5 in 1:I5) {
                  	for (g in 1:G) {
                        	temp.Cand_M[k,g,,i5]= Cand_M[k,g,,i5]* SR5[k,i5,t]*Rel_Surv[k,5,t,g]
                        	temp.Cand_F[k,g,,i5]= Cand_F[k,g,,i5]* SR5[k,i5,t]*Rel_Surv[k,5,t,g]
                  	}
            	}
            
            	SR_Sum = SR_Sum +  sum((temp.Cand_M + temp.Cand_F)[k,,,])
            	SR5.Candidate[k] = (SR_Sum / (sum((Cand_M+Cand_F)[k,,,])))
            	SR5.Candidate[is.na(SR5.Candidate)] = 0
            
            	# Productivity for use in stage 5
            	pN5[k] = SR5.Candidate[k]*(sum(Prod_Scalar[k,,5,t]*L[k,,t])/sum(L[k,,t]))
      	}
      
      
      	### Figure out Equivalent First Year fish at time t+1
      
      	# Equivalent first year fish
      	N_Equiv = rep(0,K)
      	#changed to capacity at stage 4
      
      	for (k in 1:K) {
          		N_Equiv[k]= Cand_Y1_Equivalent[k]  / (1/pN5[k] + 1/c[k,4,t] *
 				Cand_Y1_Equivalent[k])
            	if (is.na(N_Equiv[k])) {N_Equiv[k]=0}
      	}
      
      	### Figure out Actual fish at t+1
      	# Equvialent 1st year fish of Temp.Cand.Fish # which are in correct relative proportion
      	Y1_Equiv_Temp = rep(0,K)
      	for (k in 1:K) {
            	Y1_Equiv_Temp[k] = sum(apply(((temp.Cand_M+temp.Cand_F)*1)[k,,,],(3-1*(K==1)),sum)*N5.cap[k,,t])
      	}
     
		Correction = N_Equiv/(Y1_Equiv_Temp+.000000000001)
      	for (k in 1:K) {
            	for (i5 in 1:I5) {
                  	for (g in 1:G) {
					N5_FISH[k,i5,t+1,g,,1] = temp.Cand_M[k,g,,i5] * Correction[k]
                        	N5_FISH[k,i5,t+1,g,,2] = temp.Cand_F[k,g,,i5] * Correction[k]

					N_FISH[k,5,t+1,g,,1] = apply(N5_FISH[k,,t+1,g,,1],2,sum) 
					N_FISH[k,5,t+1,g,,2] = apply(N5_FISH[k,,t+1,g,,2],2,sum)
                  	}
            	}
      	}

      	# Pete Feb 2016 Attempt to get Rainbow trout/Resident fish maturation & spawning
      	# to occur in the same time step; previously there was a one-year lag which did 
      	# not mesh well with the biology of the fish; fingers crossed...   

		TempRBS = N5_FISH*0

      	for (k in 1:K) {
			TempRBS[k,,t,,,1] = N5_FISH[k,,t+1,,,1] * N5.Pspawn_M[k,,t+1]
			TempRBS[k,,t,,,2] = N5_FISH[k,,t+1,,,2] * N5.Pspawn_F[k,,t+1]
      	}
  
      	for (k2 in 1:K) {
			# Here's the problem when there's only one site - TempRBS_F and TempRBS_M
			N_RAINBOW_SPAWNERS[k2, , t+1, ,1] = apply(TempRBS[,,t,,k2,1],c((1+1*(K>1)),(2+1*(K>1))),sum)
			N_RAINBOW_SPAWNERS[k2, , t+1, ,2] = apply(TempRBS[,,t,,k2,2],c((1+1*(K>1)),(2+1*(K>1))),sum)
      	}

      	# Add Hatchery Introductions
      	# Add in hatchert fish (g=2, imprinted to site at which added.
      	for (k in 1:K) {
			N5_FISH[k,1,t+1,2,k,] = N5_FISH[k,1,t+1,2,k,] + 0.5*(Hatch_Fish[k,5,t+1])
			N_FISH[k,5,t+1,2,k,] = apply(N5_FISH[k,,t+1,2,k,],2,sum)
      	} # end of K for OnePlus
      
      	if (K >1) {
      
            	# Initialize Migrattion matrics: to k1 from k2 imprinted to k3
			N_PS_Migrate =  array(0, c(K,K,K,I5,G,2)) # to k1 from k2 imprinted to k3
         
            	for (k1 in 1:K) {
                  	for (k2 in 1:K){
                        	for (k3 in 1:K) {
                              	for (i5 in 1:I5) {
                                    	N_PS_Migrate[k1, k2, k3,i5, ,] = 
                                    		N5_FISH[k2,i5,t+1,,k3,]*OnePlus.x.siteMigration[k2, k1,t+1] 
                              	}
                        	}
                  	}
            	}

            	# Re-assign to N and N_Imprint for male and female
            	for (k in 1:K) {
                  	for (i5 in 1:I5) {
                        	for (g in 1:G) {
						N5_FISH[k,i5,t+1,g,,1] = apply(N_PS_Migrate[k,,,i5,g,1],2,sum)
						N5_FISH[k,i5,t+1,g,,2] = apply(N_PS_Migrate[k,,,i5,g,2],2,sum)
                        	}
                  	}
            	}
      	} # end of x-site migrate
      
      	for (k in 1:K) {
			N_FISH[k,5,t+1,,,] = apply(N5_FISH[k,,t+1,,,],c(2,3,4),sum)
      	}
      
      } # end of Steelhead-only block
      
      ###########
      # N6 (smolt, based on same year's OnePlus that decide to smolt )
      # Also complicated slightly by N5's are a mixture of mutiple ages 
      # of OnePlus, at least in the Steelhead case
      
	if (species == "steelhead") {	
      	CandN6_Imprint_M = array(0, c(K,I5,G,K))
      	CandN6_Imprint_F = array(0, c(K,I5,G,K))
      
		CandidateN6 = array(rep(0,(K*I5*G)), c(K,I5,G))

		# CandN6_Imprint_M/F - fish trying to advance from N5 to N6 life stage.
		# N5.Psmolt_F/M - probability of smolting (as opposed to staying or spawning as rainbow spanwers).  
		# They are user defined and read from the input fies         

		for (k in 1:K) {
			for (i5 in 1:I5) {
				for (g in 1:G) {
					CandN6_Imprint_M[k,i5,g,]=N5_FISH[k,i5,t,g,,1]*N5.Psmolt_M[k,i5,t]
					CandN6_Imprint_F[k,i5,g,]=N5_FISH[k,i5,t,g,,2]*N5.Psmolt_F[k,i5,t]

					CandidateN6[k,i5,g] = sum((CandN6_Imprint_M+CandN6_Imprint_F)[k,i5,g,])
					Candidate_Smolt[k,t,g] = sum((CandN6_Imprint_M+CandN6_Imprint_F)[k,,g,])
            	}
			}
      	}

      	SmoltSurvivors = array(0, c(K,I5,Tr,G))
      	# Assign Imprints and M/F in same ratios as incoming candidate smolts
      	ImprintShare =apply((CandN6_Imprint_M+CandN6_Imprint_F),c(1,3,4),sum)
      	FemaleImprintShare = apply((CandN6_Imprint_F),c(1,3,4),sum)
      	FemaleShare = apply(CandN6_Imprint_F,c(1,3),sum)
 		TotalShare = apply((CandN6_Imprint_M+CandN6_Imprint_F),c(1,3),sum)

      	for (k in 1:K) {
            	for (g in 1:G) {
				for (i5 in 1:I5) {
					SmoltSurvivors[k,i5,t,g] = (CandidateN6[k,i5,g]*Rel_Surv[k,5,t,g]) / 
                  			(1/Sr[k,6,t]+ 1/c[k,5,t] * sum(Rel_Surv[k,5,t,]*CandidateN6[k,i5,]))

					# AGE RECONSTRUCTION
					FemaleRatio = FemaleShare[k,g]/TotalShare[k,g] 
					N_SMOLTS[k,i5,t,g,1] = SmoltSurvivors[k,i5,t,g] * (1-FemaleRatio)
					N_SMOLTS[k,i5,t,g,2] = SmoltSurvivors[k,i5,t,g] * FemaleRatio
				}
            	}
      	} # end of K
		N_SMOLTS[is.na(N_SMOLTS)] = 0

		# sum over the i5 term for this t (leaving k, g)
		TotalSmoltSurvivors = apply(SmoltSurvivors[,,t,],c(1,3),sum)
     
      	for (k2 in 1:K) {
			if (K==1) {
				N_FISH[,6,t,,k2,1] = ImprintShare[,,k2] * TotalSmoltSurvivors[,] / as.vector(ImprintShare ) -
                               FemaleImprintShare [,,k2] * TotalSmoltSurvivors[,]/ as.vector(ImprintShare )
				N_FISH[,6,t,,k2,2] = FemaleImprintShare[,,k2] * TotalSmoltSurvivors[,]/ as.vector(ImprintShare )
     			} 
			else 
			{
				N_FISH[,6,t,,k2,1] = ImprintShare[,,k2] * TotalSmoltSurvivors[,]/  apply(ImprintShare [,,],c(1,2),sum) -
                               FemaleImprintShare[,,k2] * TotalSmoltSurvivors[,]/  apply(ImprintShare [,,],c(1,2),sum)
				N_FISH[,6,t,,k2,2] = FemaleImprintShare[,,k2] * TotalSmoltSurvivors[,]/ apply(ImprintShare [,,],c(1,2),sum)
			}
		}
		N_FISH[is.na(N_FISH)] = 0      
			
      	for (k in 1:K) {
			N_FISH[k,6,t,2,k,1] = N_FISH[k,6,t,2,k,1] + 0.5*Hatch_Fish[k,6,t]
			N_FISH[k,6,t,2,k,2] = N_FISH[k,6,t,2,k,2] + 0.5*Hatch_Fish[k,6,t]

			# ** AGE RECONSTRUCTION.  Pretend hatchery fish are all two years old?
			N_SMOLTS[k,2,t,2,1] = N_SMOLTS[k,2,t,2,1] + 0.5*Hatch_Fish[k,6,t]
			N_SMOLTS[k,2,t,2,2] = N_SMOLTS[k,2,t,2,2] + 0.5*Hatch_Fish[k,6,t]
     		}

	} else {
		# Chinook
		TempSubYearlingN6 = array(0, c(K,2))  # imprint location and sex
		TempYearlingN6 = array(0, c(K,2))  # imprint location and sex
		SubYearlingN6Candidates  = array(0, c(G)) 
		YearlingN6Candidates = array(0, c(G))
		SubYearlingN6Survivors = array(0, c(G))
		YearlingN6Survivors = array(0, c(G))

		for (k in 1:K) {

			# used to determine how to divvy up further down
			TempSubYearlingN6[,1] = apply(N_FISH[k,4,t,,,1],2,sum) * Frac.Subyear.M[k,t]
			TempSubYearlingN6[,2] = apply(N_FISH[k,4,t,,,1],2,sum) * Frac.Subyear.F[k,t]
			TempYearlingN6[,1] = apply(N_FISH[k,4,t,,,1],2,sum)*(1-Frac.Subyear.M[k,t])
			TempYearlingN6[,2] = apply(N_FISH[k,4,t,,,1],2,sum)*(1-Frac.Subyear.F[k,t])

			# candidates by genetic type
			SubYearlingN6Candidates = apply(N_FISH[k,4,t,,,1],1,sum)*Frac.Subyear.M[k,t] +
								apply(N_FISH[k,4,t,,,2],1,sum)*Frac.Subyear.F[k,t]
			YearlingN6Candidates = apply(N_FISH[k,4,t,,,1],1,sum)*(1-Frac.Subyear.M[k,t]) +
								apply(N_FISH[k,4,t,,,2],1,sum)*(1-Frac.Subyear.F[k,t])

	          	for (g in 1:G) {
				# Survivors by genetic type
				SubYearlingN6Survivors[g] = SubYearlingN6Candidates[g] *Rel_Surv[k,4,t,g] / 
					(1/p[k,5,t]+ 1/c[k,5,t] * sum(Rel_Surv[k,4,t,]* SubYearlingN6Candidates ))
				YearlingN6Survivors[g] = YearlingN6Candidates[g] *Rel_Surv[k,4,t,g] / 
					(1/p[k,6,t]+ 1/c[k,6,t] * sum(Rel_Surv[k,4,t,]* YearlingN6Candidates ))

				# Last step: distribute survivors to match ratios of Temp matrices above
				for (kk in 1:K) {
					# subyearling males imprinted at kk
                  		PORTION = sum(TempSubYearlingN6[kk,1])/sum(TempSubYearlingN6[,])
                  		if (is.na(PORTION)) PORTION  = 0
            			N_FISH[k,6,t,,kk,1] = SubYearlingN6Survivors * PORTION 

					# subyearling females imprinted at kk
                  		PORTION = sum(TempSubYearlingN6[kk,2])/sum(TempSubYearlingN6[,])
                  		if (is.na(PORTION)) PORTION  = 0
            			N_FISH[k,6,t,,kk,2] = SubYearlingN6Survivors * PORTION 

					# yearling males imprinted at kk
                  		PORTION = sum(TempYearlingN6[kk,1])/sum(TempYearlingN6[,])
                  		if (is.na(PORTION)) PORTION  = 0
            			N_FISH[k,6,t,,kk,1] = N_FISH[k,6,t,,kk,1] + YearlingN6Survivors * PORTION 

					# yearling females imprinted at kk
                  		PORTION = sum(TempYearlingN6[kk,2])/sum(TempYearlingN6[,])
                  		if (is.na(PORTION)) PORTION  = 0
            			N_FISH[k,6,t,,kk,2] = N_FISH[k,6,t,,kk,2] + YearlingN6Survivors * PORTION 
				}

				# AGE RECONSTRUCTION.  All rainbow smolt are 1 year old.
				# Used to create N_ADULTS, which is used for age reconstruction
				N_SMOLTS[k,1,t,g,1] = sum(N_FISH[k,6,t,g,,1])
				N_SMOLTS[k,1,t,g,2] = sum(N_FISH[k,6,t,g,,2])
			}
		}
	}

if (0) {
	print("6E")
	TOTAL_SMOLTS1 = sum(N_FISH[1,6,t,,,])
	TOTAL_SMOLTS2 = sum(N_SMOLTS[1,,t,,])
	print(TOTAL_SMOLTS1)
	print(TOTAL_SMOLTS2)
}
      ###########
      # N7:  Smolt to Adult 0 (Dam survival rate dependent only, c[k,6,t] assumed negligible (Eq 4)
      ### For Adjust Stages, Move Everybody Back to site of Imprint
    	# sum across sites, assign (via c(3,2) to imprinted site

	tempN_M = apply(N_FISH[,6,t,,,1],c(3,2),sum)
	tempN_F = apply(N_FISH[,6,t,,,2],c(3,2),sum)

      for(k in 1:K) {
            for (g in 1:G) {

			N_FISH[k,7,t,g,,] = 0  
			N_FISH[k,7,t,g,k,1] = tempN_M[k,g] /  (1/Sr[k,7,t] ) * Rel_Surv[k,6,t,g]
			N_FISH[k,7,t,g,k,2] = tempN_F[k,g] /  (1/Sr[k,7,t] ) * Rel_Surv[k,6,t,g]	

			# AGE RECONSTRUCTION.  
			# new adults from smolts. fresh water age is smolt age, ocean age = 1.
			for (i5 in 1:I5) {
			 	N_ADULTS[k,i5,t,g,1,1] = N_SMOLTS[k,i5,t,g,1] /  (1/Sr[k,7,t] ) * Rel_Surv[k,6,t,g]
			 	N_ADULTS[k,i5,t,g,2,1] = N_SMOLTS[k,i5,t,g,2] /  (1/Sr[k,7,t] ) * Rel_Surv[k,6,t,g]
			}
           }
      }
if (0) {
	print("7A")
	NEW_ADULTS1 = sum(N_FISH[1,7,t,,,])
	NEW_ADULTS2 = sum(N_ADULTS[1,,t,,,1])
	print(NEW_ADULTS1)
	print(NEW_ADULTS2)
}

      ######################################
      # end of cycle through watersheds
      ######################################

} 


#print(N_RECRUITS)     
	N_FISH[is.na(N_FISH)] = 0

###################################
# end of cycle through years (t)
###################################

N = apply(N_FISH[,,,,,], c(1,2,3,4), sum)
Rainbow_N = apply(N_RAINBOW[,,,,,], c(1,2,3,4), sum)
Rainbow_N_F = apply(N_RAINBOW[,,,,,2], c(1,2,3,4), sum)
Spawners = apply(N_SPAWNERS[,,,], c(1,2,3), sum)
Male_Spawners = apply(N_SPAWNERS[,,,1], c(1,2,3), sum)
Female_Spawners = apply(N_SPAWNERS[,,,2], c(1,2,3), sum)
Escapement = apply(N_ESCAPEMENT[,,,], c(1,2,3), sum)
Female_Escapement = apply(N_ESCAPEMENT[,,,2], c(1,2,3), sum)
NT = apply(NT_FISH[,,,,], c(1,2,3,4), sum)
N5 = apply(N5_FISH[,,,,,], c(1,2,3,4), sum)

detach(Var.data)
detach(Parameter.data)
detach(header.data)

return(list(
  "N"=N, 
  "N5"=N5, 
  "Rainbow_N"=Rainbow_N,
  "Rainbow_N_F"=Rainbow_N_F,
  "Female_Spawners"=Female_Spawners, 
  "Spawners" = Spawners,
  "Female_Escapement" = Female_Escapement,
  "Escapement" = Escapement,
  "N_RECRUITS" = N_RECRUITS,
  "p"=p, "c"=c,
  "Candidate_Smolt"=Candidate_Smolt
#   ###***Pete May 2015 Addition***
  ,"Male_Spawners"=Male_Spawners,
  "NT"=NT,
  "RainbowSpawners"=Rainbow_Spawners,
  "RainbowFemSpawners"=Rainbow_Female_Spawners
#   ###***Pete May 2015 Addition***
)
)


} #End of function



