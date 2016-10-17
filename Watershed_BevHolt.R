BevHolt <- function(Parameter.data, Var.data, header.data) {

######
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
PreSmolt_Migrate.T=rep(0,K)
PreSmoltN5_Migrate.T = array(rep(0, K*I5), c(K, I5))
Candidate_Smolt=array(rep(0,K*Tr*G), c(K,Tr,G))

Spawners = array(rep(0, K*Tr*G), c(K,Tr,G))
Female_Spawners = array(rep(0, K*Tr*G),c(K,Tr,G))
Rainbow_Spawners = array(rep(0, K*Tr*G), c(K,Tr,G))
Rainbow_Female_Spawners  = array(rep(0, K*Tr*G),c(K,Tr,G))

# ###***Pete May 2015 Addition***
 Male_Spawners = array(rep(0, K*Tr*G),c(K,Tr,G))
 Candidate_Smolt_Age=array(rep(0,K*I5*Tr*G), c(K,I5,Tr,G))
# ###***Pete May 2015 Addition***

Female_Escapement  = array(rep(0, K*Tr*G),c(K,Tr,G))
Escapement = array(rep(0, K*Tr*G),c(K,Tr,G))

#N_Imprint = number of NR "imprinted" (destined to return to spawn) in site k
#N5_Imprint number of NR5 "imprinted" (destined to return to spawn) in site k
N_Imprint = array(rep(0, K*I*Tr*G*K), c(K,I,Tr,G,K))
N5_Imprint = array(rep(0,K*10*Tr*G*K), c(K,10,Tr,G,K))
N_Imprint_F = array(rep(0, K*I*Tr*G*K), c(K,I,Tr,G,K))
N5_Imprint_F = array(rep(0,K*10*Tr*G*K), c(K,10,Tr,G,K))
RainbowSpawners_F = array(rep(0, K*I5*Tr*G), c(K,I5,Tr,G))
RainbowSpawners_M = array(rep(0, K*I5*Tr*G), c(K,I5,Tr,G))

#dim(RainbowSpawners_F)
# Need to track rainbows separately for egg and fry life stage
Rainbow_N = array(rep(0,K*3*Tr*G), c(K,3,Tr,G))
Rainbow_N_F = array(rep(0,K*3*Tr*G), c(K,3,Tr,G))
Rainbow_N_Imprint = array(rep(0,K*3*Tr*G*K),c(K,3,Tr,G,K))
Rainbow_N_Imprint_F = array(rep(0,K*3*Tr*G*K),c(K,3,Tr,G,K))


#######################################################
#### temp initialization for Imprints ****
#### Will need to figure out how to initialize this via
#### initial values files...
#### Same issue will be true w/ Male / Female splits
#######################################################
dim(N_Imprint)
k=1
for (k in 1:K) {
      N_F = .5*N
      N_Imprint[k,,,,k]=N[k,,,]
      N5_Imprint[k,,,,k]=N5[k,,,]
      N_Imprint_F[k,,,,k]=N[k,,,]*.5
      N5_Imprint_F[k,,,,k]=N5[k,,,]*.5
}

N_F = .5*N
N5_F = .5* N5
N5_M = .5* N5
NT = array(0, c(K, 10, Tr, G)) 
NT_F= NT*.5
NT_M= NT*.5

#N5.Pspawn_F = N5.Pspawn
#N5.Pspawn_M = N5.Pspawn
#N5.Psmolt_F = N5.Psmolt
#N5.Psmolt_M = N5.Psmolt
#N5.Pstay_F = N5.Pstay
#N5.Pstay_M = N5.Pstay

################################
################################
#################################


# Smolts_by_BroodYear = array(rep(0, K*Tr*G),c(K,Tr,G))

#Initialize these
Post_Spawn_Returns_Anadromous_F = array(0, c(K,10,G))
Post_Spawn_Returns_Anadromous_M = array(0, c(K,10,G))
Post_Spawn_Returns_Rainbow_F = array(0, c(K,I5,G))
Post_Spawn_Returns_Rainbow_M = array(0, c(K,I5,G))


#######################################################
#######################################################
###################################################
# Start Time Loops (t=1 to Tr) for each MC iteration
####################################################

t=2
#for (t in 2:6) {
for (t in 2:(Tr-1)){
  
      print(paste("t=",t))
      
      #print(paste("t=",t,"a"))
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
      
            #	# see cell q17 on the model worksheet in excel VB model
            #	H[k,J+1,t]=H[k,4,t]/2
            #      # replace above with 10% of fast turbulent, per Chris Beasley e-mail 12/13/12
            #      H[k,J+1,t]=H[k,2,t]/10
            #      # replace above with....
            # Assume all land is applicable as spawning gravel, and use productivity
            # multiplier to adjust for less than 100% being "real" value, per
            # conversation with Jody, 5/13/2013.
            #      H[k,J+1,t] = sum(H[k, ,t])
      
      }
      
      
      # D is capacity per sq. meter
    	# Note: per document description of table 2-4, assume nearly "infinite"
    	# density and resulting capacity for fry-to-par, and thus only productivity
    	# parameter will affect this life stage cycle.  But this way, the hooks are
    	# in to add this density when appropriate
      
    	# Capacity by life-stage i, in watershed k, at time t (Equation 14)
      
      
      for (k in 1:K) {
            for (i in 1:5){c[k,i,t]=
                  (sum(H[k,,t]*D[k,,i,t]*frac[k,i,,t])+.0000000000000001)
            }
      }
      
      
      # OK, we have carrying capacity for first four stages:
      # spawning, egg, fry, pre-smolt
            
            
            
      #calculate productivity for time t, site k
      for (k in 1:K) {
            for (i in 1:5) {
            # Note the (i+1) in the Sr below.  For outdated reasons, the first entry
            # in that array is a zero, so the i+1 is to fix the indexing error.  Sloppy.
            # will multiply be Rel_Fecund at egg stage, but still need a productivity
                p[k,i,t] = Sr[k,(i+1),t]*(sum(Prod_Scalar[k,,i,t]*L[k,,t])/(sum(L[k,,t])+.0000000000001))
            }	
      
      #print(paste("p=", p[k,4,t]))
      #print(paste("Sr=", Sr[k,5,t]))
      
      }
      
      ###################################################################
      ###################################################################
      
      
      ##############################################################
      # Cycle through sites
      ##############################################################
      
      k=1
      for (k in 1:K) { 
      
            ###################################################################
            ###################################################################
            
            # starting at time step 2.... so I'm going to start at adult, mature fish
            # from time step 1 to calculate number of spawners and adjust adult
            # fish counts from back in step 1.  Gotta start somewhere...
            
            
            ## Move to top of function
            #Fecund = rep(0, 11)
            
            ###############
            # N9, N10, etc...: ocean ages 2 ->10 (taking into account loss due to maturation rates of prior years)
      
            for (i in 8:16) {
                  
                  #Mat8Plus_F = Mat8Plus
                  #Mat8Plus_M = Mat8Plus
                  ####################
                  
                  
                  CandN_M = (N[k,i,t-1,]-N_F[k,i,t-1,])*(1-Mat8Plus_M[k,i-7,t-1]) 
                  CandN_F = N_F[k,i,t-1,] * (1-Mat8Plus_F[k, i-7, t-1])
                  # Here's where I should be adding the surviving spawners???  Otherwise,
                  # 1-Mat8Plus gets rid of them all.....  initialize to zero than carry from
                  # previous time loop and add here.  Do similar for resident rainbow.
                  
                  CandN = CandN_M+ CandN_F
                  
                  for (g in 1:G) {
                         
                        N[k,i+1,t,g]= (CandN[g]*Rel_Surv[k,i,t-1,g]) / 
                                        (1/Sr[k,i+1,t-1]+ 1/(C_ocean[k,i-7,t-1]) *  
                                           sum(Rel_Surv[k,i,t-1,]* CandN))
                  } 
                  
                  # No differential survival by sex, so:
                  N_F[k,i+1,t,] = N[k,i+1,t,]*(CandN_F/(CandN))  # +.000000001))
                  N_F[is.na(N_F)] = 0
                  
            } # end of i in (8:16)
      
            ## Add back surviving spawners from before...
            
            for (i in 1:10){
                  N[k,i+7,t,] = N[k,i+7,t,]+Post_Spawn_Returns_Anadromous_M[k,i,]+Post_Spawn_Returns_Anadromous_F[k,i,]
                  N_F[k,i+7,t,] = N_F[k,i+7,t,]+Post_Spawn_Returns_Anadromous_F[k,i,]
                  N_Imprint[k, i+7,t,,k] = N_Imprint[k, i+7,t,,k]+Post_Spawn_Returns_Anadromous_M[k,i,]+Post_Spawn_Returns_Anadromous_F[k,i,]
                  N_Imprint_F[k, i+7,t,,k] = N_Imprint_F[k, i+7,t,,k]+Post_Spawn_Returns_Anadromous_F[k,i,]
            }     
            
            
            
            #print(N[k,,t,1])
            
            ############
            # NT: Mature Fish (ready to Spawn, calc's as % of ocean ages 1-10)
          	for (i in 8:17) {
                  #***************************************
                  #PETE FIX!!! gets spawners in correct time step
                  CandN_M =  (N[k,i,t,]-N_F[k,i,t,])*(Mat8Plus_M[k,i-7,t]) 
                  CandN_F = N_F[k,i,t,] * (Mat8Plus_F[k, i-7, t])
                  CandN = CandN_M+ CandN_F
                  #***************************************
                  # 	  
                  #       CandN_M =  (N[k,i,t-1,]-N_F[k,i,t-1,])*(Mat8Plus_M[k,i-7,t-1]) 
                  #       CandN_F = N_F[k,i,t-1,] * (Mat8Plus_F[k, i-7, t-1])
                  #       CandN = CandN_M+ CandN_F
                  #print("k=")
                  #print(k)
                  #print(CandN)
                  for (g in 1:G) {
                          #Pete May 2015 Fix
                          NT[k,i-7,t,g] =  (CandN[g]*Rel_Surv[k, i,t,g]) / 
                            (1/(1+0*(Sr[k,2,t])) + 1/(C_ocean[k,i-7,t]) *  #Sr[k,2,t] doesn't get implemented here
                            sum(CandN[]*Rel_Surv[k,i,t,]))
                          
#                         NT[k,i-7,t,g] =  (CandN[g]*Rel_Surv[k, i,t-1,g]) / 
#                         (1/(1+0*(Sr[k,2,t-1])) + 1/(C_ocean[k,i-7,t-1]) * 
#                         sum(CandN[]*Rel_Surv[k,i,t-1,]))	
                              
                            
                        # No differential survival by sex, so:
                        NT_F[k,i-7,t,g] = NT[k,i-7,t,g]*(CandN_F[g]/(CandN[g]))
                  }
            }
      } # End of cycle through K's for mature fish....
      
      NT_F[is.na(NT_F)] = 0
      
      #print(NT_F[,,t,1])
      
      #print(paste("t=",t,"b1"))
      
      ###########################################################################
      ################### apply Cross-Site Migration for Spawners NT #############
      ### Will need to adjust F's and imprints every time I cross site-migrate!!
      ### Except for the spawners...they're allowed to re-imprint, by definition
      #####################
      
      NT_Migrate_F = array(rep(0, K*10*G),c(K, 10, G))
      NT_Migrate_M = array(rep(0, K*10*G),c(K, 10, G))
      
      ###############################
      
      
      NT_Migrate_F = array(rep(0, K*10*G),c(K, 10, G))
      NT_Migrate_M = array(rep(0, K*10*G),c(K, 10, G))
      
      for (k1 in 1:K) {
            for (k2 in 1:K){
                  NT_Migrate_F[k1, , ] = NT_Migrate_F[k1,,]+ 
                  NT_F[k2,,t,] *Spawner.x.siteMigration[k2, k1,t]
                  NT_Migrate_M[k1, , ] = NT_Migrate_M[k1,,]+ 
                  (NT-NT_F)[k2,,t,] *Spawner.x.siteMigration[k2, k1,t]
                  }
#             for(g in 1:G){
#               for(i in 1:10) {
#                 if(NT_Migrate_F[k1,i,g]>0){ #DEBUG VB
#                   print(paste("t = ",t," k = ",k," i = ",i," g = ",g," SpawnMig = " , NT_Migrate_F[k1,i,g]))
#                   
#               }
#             }
#             }
                  
                  #print(paste(k1,k2,t,Spawner.x.siteMigration[k2, k1,t],NT_F[k2,,t,]))
            
      }

      


      
      
      NT_M[,,t,] = NT_Migrate_M
      NT_F[,,t,] = NT_Migrate_F
      NT[,,t,] = NT_Migrate_M + NT_Migrate_F
      
      
      # print(paste("t=",t,"b2"))
      
      ############################################################
      
      #### OK, now I've got NT and NT_F - the fish attempting to spawn by age and G-type
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
                        NT_F[k,,t,g] = NT_F[k,,t,g] * max(0,(1-harvest.hatch[k,t]))
                        NT_M[k,,t,g] = NT_M[k,,t,g] * max(0,(1-harvest.hatch[k,t]))
                  } else {
                        NT_F[k,,t,g] = NT_F[k,,t,g] * max(0,(1-harvest.wild[k,t])) 
                        NT_M[k,,t,g] = NT_M[k,,t,g] * max(0,(1-harvest.wild[k,t]))
                  }
#print(paste(sum(NT_F[k,,t,]),sum(NT_M[k,,t,])))
                  ##############################################
                  #Now NT is represents spawners for anadromous - survived and will try to spawn and, in O.Mykiss, perhaps
                  #even return to where they came from
                  #
                  
                  Female_Escapement[k,t,g] =  sum(NT_F[k,,t,g])
                  Escapement[k,t,g] = sum(NT_F[k,,t,g]) + sum(NT_M[k,,t,g])
                  
                  # Female_Spawners is just females
                  #Female_Spawners[k,t,g]=sum(NT_F[,g])
                  # Now we have Spawners surviving Harvest - i.e. "Escapement"
                  
                  
                  # N1 is returning spawners (Escapement) male+female. This
                  # is duplicated by "Escapement", but later modified.
                  
                  ###### HERE HERE HERE HERE HERE!!!!
                  ### It looks like something's mucked up with the NT_M[k,,t,g] for some stage--there's an NaN...
                  N[k,1,t,g] = sum(NT_M[k,,t,g])+sum(NT_F[k,,t,g])
                  N_F[k,1,t,g] =sum(NT_F[k,,t,g])
            } # end of "for (g in...)"
            
    
            ## have to calc this later after adding rainbow spawners....
            #Male_GDist = apply(NT_M[k,,t,],2,sum)/(sum(NT_M[k,,t,])+.0000001)
            
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
            
            
            #### Add Rainbow Spawner Eggs and Subtract Rainbow Spawners from Pre-Smolts
            # Now take out a portion of the pre-smolts that have decided to become
            # "resident rainbows" and go to spawn prior to ever going to sea.  These
            # spawners will have significantly less fecundity than Steelhead returning
            # from the sea.
            #print(N5[k,,t+1,])
            
            
            # RainbowSpawners can be used to calculate fraction returning resident rainbows after spawning
            
            
            # Calculate distribution of male geno-types (hatch fish)
            
            #Male_GDist=(apply(NT_M[k,,t,],2,sum)/apply(NT_M[k,,t,],2,sum))
            Male_GDist=apply(NT_M[k,,t,],2,sum)/(sum(NT_M[k,,t,])+.00001)
            
            #print("male_gdist")
            #print(Male_GDist)
            Male_GDist[is.na(Male_GDist)] = 0
      
#             #####################################################################################
#             ##### Begin Pete Feb 2016 edits -- gets rainbow spawners into correct time step
#             #####################################################################################      
#             # Rainbow.Male_GDist calc's separately since they won't compete while spawning
#             Rainbow.Male_GDist = 
#             apply(RainbowSpawners_M[k,,t-1,],2,sum)/(sum(RainbowSpawners_M[k,,t-1,])+.00001)
            Rainbow.Male_GDist = 
            apply(RainbowSpawners_M[k,,t,],2,sum)/(sum(RainbowSpawners_M[k,,t,])+.00001)
#             #####################################################################################
#             ##### End Pete Feb 2016 edits
#             #####################################################################################      

            Rainbow.Male_GDist[is.na(Rainbow.Male_GDist)] = 0
            
            #} # end k
            
            ################################
            # Calculate fish that will return after surviving trip to spawn (SR * Rel_Surv) and then surviving the actual spawning and return journy (PSSA and PSSR)
            # And add them back into the current year at pre-smolt stages (for rainbow) and adult stages(for anadromous)
            
            #for (k in 1:K) {
            #Post_Spawn_Returns_Anadromous_F = array(0, c(K,10,G))
            #Post_Spawn_Returns_Anadromous_M = array(0, c(K,10,G))
            #Post_Spawn_Returns_Rainbow_F = array(0, c(K,I5,G))
            #Post_Spawn_Returns_Rainbow_M = array(0, c(K,I5,G))



            
            for (i in 1:9){
                  Post_Spawn_Returns_Anadromous_M[k,i+1,]=   NT_M[k, i, t,] *  (Sr[k,2,t])* Rel_Surv[k,1,t,] * Post_Spawn_Survival_Anadromous_M[k,i,t,] 
                  Post_Spawn_Returns_Anadromous_F[k,i+1,]=   NT_F[k, i, t,] *  (Sr[k,2,t])* Rel_Surv[k,1,t,] * Post_Spawn_Survival_Anadromous_F[k,i,t,] 
            }
      
      
      
      
            for (i5 in 1:I5){       #Pete Nov 2015
                  Post_Spawn_Returns_Rainbow_M[k,i5,]=   RainbowSpawners_M[k, i5, t-1,] *  (Sr[k,2,t])* Rel_Surv[k,1,t,]* Post_Spawn_Survival_Rainbow_M[k,i5,t,] 
                  Post_Spawn_Returns_Rainbow_F[k,i5,]=   RainbowSpawners_F[k, i5, t-1,] *  (Sr[k,2,t])* Rel_Surv[k,1,t,]* Post_Spawn_Survival_Rainbow_F[k,i5,t,] 
                  
                  ## Pete May 2015 -- minor fix noted below
                  N5[k,i5,t,] = N5[k,i5,t,] + Post_Spawn_Returns_Rainbow_M[k,i5,]+Post_Spawn_Returns_Rainbow_F[k,i5,] #Pete May 2015, fixed last term to be Rainbow and not anadromous
                  N5_F[k,i5,t,] = N5_F[k,i5,t,] + Post_Spawn_Returns_Rainbow_F[k,i5,]
                  N5_Imprint[k,i5,t,,k] = N5_Imprint[k,i5,t,,k]+ Post_Spawn_Returns_Rainbow_M[k,i5,]+Post_Spawn_Returns_Rainbow_F[k,i5,] #Pete May 2015, fixed last term to be Rainbow and not anadromous
                  N5_Imprint_F[k,i5,t,,k] = N5_Imprint_F[k,i5,t,,k]+ Post_Spawn_Returns_Rainbow_F[k,i5,] 
                  
            } # end i5
            # fixed bug here.  Can't end k loop, because g.dist is only defined
            # for current k...
            # } # end k

      
            #############################################
                      
            # Calculating These since they're an output later on.... not used in later calculations.
            # Note:  this is just used for reporting purposes.  The actual surival and
            # capacity and productivity impacts are all linked in the escapement to egg
            # calculation, which doesn't explicity account for "spawners" before they
            # lay their eggs
                      
            # for (k in 1:K) {
      
      
            # Old Way
            for (g in 1:G) {
            
                  Spawners[k,t,g] =   (Escapement[k,t,g]) *  Sr[k,(2),t] * Rel_Surv[k,(1),t,g]
                  Female_Spawners[k,t,g] = 
                    (Female_Escapement[k,t,g]) * 
                     Sr[k,(2),t] * Rel_Surv[k,(1),t,g]
                  
                  ###***Pete May 2015 Addition***
                  Male_Spawners[k,t,g] =   (Escapement[k,t,g]) * 
                    Sr[k,(2),t] * Rel_Surv[k,(1),t,g] - Female_Spawners[k,t,g]
                  ###***Pete May 2015 Addition***

#####################################################################################
##### Begin Pete Feb 2016 edits -- commented out = original
#####################################################################################      
                  
#                   #print("rb spawners 1")
#                   #print(RainbowSpawners_F[k,,t-1,1])
#                   Rainbow_Spawners[k,t,g] =
#                     (apply(RainbowSpawners_M[k,,t-1,], 2, sum)[g] + apply(RainbowSpawners_F[k,,t-1,], 2, sum))[g]  *
#                        Sr[k,(2),t] * Rel_Surv[k,(1),t,g]
#                   Rainbow_Female_Spawners[k,t,g] = 
#                     (apply(RainbowSpawners_F[k,,t-1,], 2, sum)[g]) * 
#                      Sr[k,(2),t] * Rel_Surv[k,(1),t,g]

#                   print("rb spawners 1")
#                   print(RainbowSpawners_F[k,,t,1])
                  Rainbow_Spawners[k,t,g] =
                    (apply(RainbowSpawners_M[k,,t,], 2, sum)[g] + apply(RainbowSpawners_F[k,,t,], 2, sum)[g])  *
                       Sr[k,(2),t] * Rel_Surv[k,(1),t,g]
                  Rainbow_Female_Spawners[k,t,g] = 
                    (apply(RainbowSpawners_F[k,,t,], 2, sum)[g]) * 
                     Sr[k,(2),t] * Rel_Surv[k,(1),t,g]
#####################################################################################
##### End Pete Feb 2016 edits
##################################################################################### 

            
          }

            # New Way
            #
            #Spawners[k,t,] =   (Escapement[k,t,]) *  Sr[k,(2),t] * Rel_Surv[k,(1),t,]
            #Female_Spawners[k,t,] =   (Female_Escapement[k,t,]) *    Sr[k,(2),t] * Rel_Surv[k,(1),t,]
            #
            #Rainbow_Spawners[k,t,] =
            #  (apply(RainbowSpawners_M[k,,t,], 2, sum)[] + apply(RainbowSpawners_F[k,,t,], 2, sum))[]  *
            #     Sr[k,(2),t] * Rel_Surv[k,(1),t,]
            #
            #Rainbow_Female_Spawners[k,t,] = 
            #  (apply(RainbowSpawners_F[k,,t,], 2, sum)[]) *    Sr[k,(2),t] * Rel_Surv[k,(1),t,]
            
            ### HERE could add some of these spawners, multiplied by
            ### spawning survival probability, back to populations from where
            ## they came (rainbow or anadromous)... except I haven't retained age
            ## of origin
            #}
            
            ######################################
            ## Now calculate "candidate eggs"
      
            Cand.Egg.N=rep(0,G)
            Rainbow.Cand.Egg.N = Cand.Egg.N*0
      
            # Speed Improvement
            # could take out for (i in 1:10) and for (i5 in 1:I5), below, easily
            #for (k in 1:K) {
            for (g1 in 1:11) {
                  for (g2 in 1:11) {
                        for (i in 1:10) {    # cycle through all adult ages possible
                              Cand.Egg.N[g.index[g1,g2]]= Cand.Egg.N[g.index[g1,g2]]+
                              Female_Fecundity[k,i, t,g1]*NT_F[k,i,t,g1]*Male_GDist[g2] 
                        }
                              
                        for (i5 in 1:I5) { # cycle through all pre-smolt spawner ages
                              #   Rainbow.Cand.Egg.N[g.index[g1,g2]] = Cand.Egg.N[g.index[g1,g2]]+
                              #      (N5.Rainbow.Fecundity[k,,t] * 
                              #          (RainbowSpawners_F[k,,t,g1])*Male_GDist[g2])
                              # fixed bug, here!!!!
                          
#####################################################################################
##### Begin Pete Feb 2016 Edits -- commented = original
#####################################################################################
#                               Rainbow.Cand.Egg.N[g.index[g1,g2]] = Rainbow.Cand.Egg.N[g.index[g1,g2]]+
#                               (N5.Rainbow.Fecundity[k,i5,t] * 
#                               (RainbowSpawners_F[k,i5,t-1,g1])*Rainbow.Male_GDist[g2])
                              Rainbow.Cand.Egg.N[g.index[g1,g2]] = Rainbow.Cand.Egg.N[g.index[g1,g2]]+
                                (N5.Rainbow.Fecundity[k,i5,t] * 
                                (RainbowSpawners_F[k,i5,t,g1])*Rainbow.Male_GDist[g2])
                              #print(paste("t=",t,RainbowSpawners_F[1,,t,]))
#####################################################################################
##### End Pete Feb 2016 edits
#####################################################################################


                        }
                  }
            } # end of "for (g1, g2...)"

      
            ##############################################
            # Assume eggs are equal size... distribute capacity equally across
            # all g-types, and apply BH equation separately for each g-type to
            # figure out number of eggs by g-type.
            
            
            
            ##############
            ##############
            #############
            ##############
      
            # Added "divide by Sr[k,2,t] so spawner-egg survival isn't double counted (along with adult-to-spawner)
            # Oops.  That was a screw up.  Rs[k,2,t] isn't used prior to this, since I don't
            # use "spawners" but NT to calculate candidate eggs.

            # Eggs!  Chinook Steelhead Only (TBD)
            for (g in 1:G) {
                  N[k,2,t,g]= Cand.Egg.N[g]*Rel_Surv[k,1,t,g]/
                  (1/(p[k,1,t])+  1/c[k,1,t]*
                  sum(Rel_Surv[k,1,t,]* Cand.Egg.N)) 
                  # Assume 50% Male and 50% Female Eggs
                  N_F[k,2,t,g] = .5* N[k,2,t,g]
            }
      
            ##############################
            # Eggs (Rainbow Spawners) # Don't yet compete for same space
            # Eggs!  Steelhead Only (TBD)
            for (g in 1:G) {
                  Rainbow_N[k,2,t,g]= Rainbow.Cand.Egg.N[g]*Rel_Surv[k,1,t,g]/
                  (1/(p[k,1,t])+  1/c[k,1,t]*
                  sum(Rel_Surv[k,1,t,]* Rainbow.Cand.Egg.N)) 
                  # Assume 50% Male and 50% Female Eggs
                  Rainbow_N_F[k,2,t,g] = .5* Rainbow_N[k,2,t,g]
            }

      
      } # end K
      
      
      #############################
      #########################################################################
      #########################################################################
      
    	############
    	#N3 (fry, based on prior year's eggs) 
    	###########
      N_Imprint[,3,t+1,,]=N_Imprint[,3,t+1,,]*0
      N_Imprint_F[,3,t+1,,]=N_Imprint[,3,t+1,,]*0
      
      for (k in 1:K) {
          	for (g in 1:G) {
                  N[k,3,t+1,g]=N[k,2,t,g]*Rel_Surv[k,2,t,g] / 
                  (1/p[k,2,t]+ 1/c[k,2,t] * 
                  sum(Rel_Surv[k,2,t,]*N[k,2,t,]))
                  
                  #print(N[k,3,t,1])
                  #print(N[k,3,t+1,1])
                  # Same M/F ratio as prior step
                  N_F[k,3,t+1,g] = N[k,3,t+1,g] * (N_F[k,2,t,g]/N[k,2,t,g]) #,.00000001) ) 
                  N_F[is.na(N_F)] = 0
                  
                  # add Hatchery Fish
                  N[k,3,t+1,g] = N[k,3,t+1,g] + Hatch_Fish[k,3,t]*(g==2)
                  N_F[k,3,t+1,g] = N_F[k,3,t+1,g] + 0.5 * Hatch_Fish[k,3,t]*(g==2)
                  
                
                  
            } # of of g
      } # end of K for fry
      
      
      # Initialize Imprint for Future Spawning HERE!
      for (k in 1:K) {
            N_Imprint_F[k,3,t+1,,k]= N_F[k,3,t+1,]
            N_Imprint[k,3,t+1,,k] = N[k,3,t+1,]
      }
      
      #print(paste("t=",t,"c1"))
      
      ######################################################################
      ############
      #N3 (fry, based on prior year's eggs), for rainbows only (not yet intermixed, so
      # not yet competing for space)
      ###########
      
      
      ############
      #N3 (fry, based on prior year's eggs) 
      ###########
      Rainbow_N_Imprint[,3,t+1,,]=Rainbow_N_Imprint[,3,t+1,,]*0
            Rainbow_N_Imprint_F[,3,t+1,,]=Rainbow_N_Imprint[,3,t+1,,]*0
            
            
            for (k in 1:K) {
              for (g in 1:G) {
                Rainbow_N[k,3,t+1,g]=Rainbow_N[k,2,t,g]*Rel_Surv[k,2,t,g] / 
                  (1/p[k,2,t]+ 1/c[k,2,t] * 
                     sum(Rel_Surv[k,2,t,]*Rainbow_N[k,2,t,]))
                
                # Same M/F ratio as prior step
                Rainbow_N_F[k,3,t+1,g] = Rainbow_N[k,3,t+1,g] * (Rainbow_N_F[k,2,t,g]/Rainbow_N[k,2,t,g]) #+.00000001) ) 
                Rainbow_N_F[is.na(Rainbow_N_F)] = 0
                # No hatchery fish to add for rainbow spanwers....Assume they're added with (and therefore compete
                # with) anadromous fish.
              }
            } # end of K for fry
            # Initialize Imprint for Future Spawning HERE!
            
            min(N)
            for (k in 1:K) {
              Rainbow_N_Imprint_F[k,3,t+1,,k]= Rainbow_N_F[k,3,t+1,]
              Rainbow_N_Imprint[k,3,t+1,,k] = Rainbow_N[k,3,t+1,]
            }
      #print(paste("t=",t,"c2"))
      
      
      ###################################################
      # Now we've calculated pops seperately for rainbow and anadromous fish,
      # let's combine them before they try to become parr (and migrate them
      # together as well
      
      #print(paste("rainbow", Rainbow_N[,3,t+1,1]))
      
      N[,3,t+1,] = N[,3,t+1,] + Rainbow_N[,3,t+1,]
      N_F[,3,t+1,] = N_F[,3,t+1,] + Rainbow_N_F[,3,t+1,]
      
      N_Imprint[,3,t+1,,] = N_Imprint[,3,t+1,,] + Rainbow_N_Imprint[,3,t+1,,]
      N_Imprint_F[,3,t+1,,] = N_Imprint_F[,3,t+1,,] + Rainbow_N_Imprint_F[,3,t+1,,]
      
      
      #######################################################################

      
      ################ Move X Site Fry Migration Here for Parr##############
      ################### apply Cross-Site Migration for Fry #############
      #Fry.x.siteMigration[, ,t+1] = t(array(c(.9,.1,0,0,.95, .05, 0, .3, .7), c(3,3)))
      #Fry.x.siteMigration[, ,t+1]
      
      if (K >1) {
            # Initialize Migrattion matrics: to k1 from k2 imprinted to k3
            FRY_Migrate = array(0, c(K,K,K,G)) # to k1 from k2 imprinted to k3
            FRY_Migrate_F = array(0, c(K,K,K,G)) # to k1 from k2 imprinted to k3
            
            for (k1 in 1:K) {
                  #for (g in 1:G) {
                  for (k2 in 1:K){
                        for (k3 in 1:K) {
                              FRY_Migrate[k1, k2, k3, ] = 
                              N_Imprint[k2,3,t+1,,k3]*Fry.x.siteMigration[k2, k1,t+1] # FRY_Migrate[to k1 from k2 for g]
                              FRY_Migrate_F[k1, k2, k3,] = 
                              N_Imprint_F[k2,3,t+1,,k3]*Fry.x.siteMigration[k2, k1,t+1]
                        }
                  }
            }
            #}
            
            # Re-assign to N and N_Imprint for male and female
            for (k in 1:K) {
                  for (g in 1:G) {
                      	N[k,3,t+1,g] =sum(FRY_Migrate[k,,,g])
                        N_F[k,3,t+1,g] =sum(FRY_Migrate_F[k,,,g])
                        N_Imprint[k,3,t+1,g,] = apply(FRY_Migrate[k,,,g],2,sum)
                        N_Imprint_F[k,3,t+1,g,] = apply(FRY_Migrate_F[k,,,g],2,sum)
#                         if(N[k,3,t+1,g]>0){ #DEBUG VB
#                           print(paste("t = ",t," k = ",k," FryMig = " , N[k,3,t+1,g]))
#                         }
                  }
            }
      
      } # end of x-site-migrate
      
  
      
      #min(N)
      #min(N5)
      #print(paste("t=",t,"d2"))
      
      ###############################################################
      ###############################################################
      ####################################################################
      ###########
      #N4 (par, based on same year's fry)
      ###########
      # Old Way
      #for (k in 1:K) {
      #	for (g in 1:G) {
      #      N[k,4,t,g]=N[k,3,t,g]*Rel_Surv[k,3,t,g] / 
      #                (1/p[k,3,t]+ 1/c[k,3,t] * 
      #                   sum(Rel_Surv[k,3,t,]*N[k,3,t,]))
      # 
      ## Same M/F ratio as prior step
      ## Same proportion of imprints as prior step (prior to x-site migration
      #	N_F[k,4,t,g] = N[k,4,t,g] * (N_F[k,3,t,g]/N[k,3,t,g]) 
      #	N_Imprint[k,4,t,g,] = N_Imprint[k,3,t,g,]*(N[k,4,t,g]/N[k,3,t,g]) #,.00000001) ) 
      #	N_Imprint_F[k,4,t,g,] = N_Imprint_F[k,3,t,g,]*(N[k,4,t,g]/N[k,3,t,g]) #,.00000001) ) 
      #N_F[is.na(N_F)] = 0
      #N_Imprint[is.na(N_Imprint)] = 0
      #N_Imprint_F[is.na(N_Imprint_F)] = 0
      #
      #
      # }  #end of G for Parr
      ## Add in hatchery fish (g=2, imprinted to site at which added.
      #   N[k,4,t,2] = N[k,4,t,2] +(Hatch_Fish[k,4,t])
      #   N_Imprint[k,4,t,2,k] = N_Imprint[k,4,t,2,k] + Hatch_Fish[k,4,t]
      #   N_Imprint_F[k,4,t,2,k] = N_Imprint_F[k,4,t,2,k] + 0.5*Hatch_Fish[k,4,t]
      #} # end of K for parr
      #
      #savedN_Imprint = N_Imprint
      #savedN_Imprint_F = N_Imprint_F
      
      
      # New Way
      for (k in 1:K) {
            #	for (g in 1:G) {
            N[k,4,t,]=N[k,3,t,]*Rel_Surv[k,3,t,] / 
            (1/p[k,3,t]+ 1/c[k,3,t] * 
            sum(Rel_Surv[k,3,t,]*N[k,3,t,]))
             
            # Same M/F ratio as prior step
            # Same proportion of imprints as prior step (prior to x-site migration
            N_F[k,4,t,] = N[k,4,t,] * (N_F[k,3,t,]/N[k,3,t,]) 
            N_Imprint[k,4,t,,] = N_Imprint[k,3,t,,]*(N[k,4,t,]/N[k,3,t,]) #,.00000001) ) 
            N_Imprint_F[k,4,t,,] = N_Imprint_F[k,3,t,,]*(N[k,4,t,]/N[k,3,t,]) #,.00000001) ) 
            N_F[is.na(N_F)] = 0
            N_Imprint[is.na(N_Imprint)] = 0
            N_Imprint_F[is.na(N_Imprint_F)] = 0
            
            
            # }  #end of G for Parr
            # Add in hatchery fish (g=2, imprinted to site at which added.
            N[k,4,t,2] = N[k,4,t,2] +(Hatch_Fish[k,4,t])
            N_Imprint[k,4,t,2,k] = N_Imprint[k,4,t,2,k] + Hatch_Fish[k,4,t]
            N_Imprint_F[k,4,t,2,k] = N_Imprint_F[k,4,t,2,k] + 0.5*Hatch_Fish[k,4,t]
      } # end of K for parr
      
      #max(savedN_Imprint - N_Imprint)
      #min(savedN_Imprint_F - N_Imprint_F)
      #print(paste("t=",t,"d3"))
      
      
      #####################################################################
      #Move X Site Migration of Parr here for Pre-Smolt
      ################ Move X Site Fry Migration Here for Parr##############
      ################### apply Cross-Site Migration for Par #############
      #print(paste("t=",t,"e1"))
      
      ################## apply Cross-Site Migration for Fry #############
      #Par.x.siteMigration[, ,t] = t(array(c(.9,.1,0,0,.95, .05, 0, .3, .7), c(3,3)))
      #Par.x.siteMigration[, ,t]
      if (K >1) {
            
            # Initialize Migrattion matrics: to k1 from k2 imprinted to k3
            Par_Migrate = array(0, c(K,K,K,G)) # to k1 from k2 imprinted to k3
            Par_Migrate_F = array(0, c(K,K,K,G)) # to k1 from k2 imprinted to k3
            
            for (k1 in 1:K) {
                  #for (g in 1:G) {
                  for (k2 in 1:K){
                        for (k3 in 1:K) {
                              Par_Migrate[k1, k2, k3, ] = 
                              N_Imprint[k2,4,t,,k3]*Par.x.siteMigration[k2, k1,t] # Par_Migrate[to k1 from k2 for g]
                              Par_Migrate_F[k1, k2, k3,] = 
                              N_Imprint_F[k2,4,t,,k3]*Par.x.siteMigration[k2, k1,t]
                        }
                  }
            }
            #}
            
            
            # Re-assign to N and N_Imprint for male and female
            for (k in 1:K) {
                  for (g in 1:G){
                        N[k,4,t,g] =sum(Par_Migrate[k,,,g])
                        N_F[k,4,t,g] =sum(Par_Migrate_F[k,,,g])
                        N_Imprint[k,4,t,g,] = apply(Par_Migrate[k,,,g],2,sum)
                        N_Imprint_F[k,4,t,g,] =  apply(Par_Migrate_F[k,,,g],2,sum)
                  }
            }
            
            
#             for(g in 1:G){#DEBUG VB
#               for(k in 1:K) {
#                 if(N[k,4,t,g]>0){ 
#                   print(paste("t = ",t," k = ",k," g = ",g," ParMig = " , N[k,4,t,g]))
#                   
#                 }
#               }
#             }
              
            
      } # end of x-site migrate
      
      #print(paste("t=",t,"e2"))
      
      ###############################################################
      ###############################################################
      # Pre-Smolt (Re-Writted August 22 2013 
      # N5 (presmolt)
      # N5 is complicated because of steelhead
      # N5 is based on number of pass entering system and number of prior
      # year's presmolt, at each age, staying for another year
      # Capcity is weighted based on age distribution and survival probability
      # of fish trying to occupy given space (older fish require more space)
      ###########
      
      
      # Candidate Fish
      # Cand_M
      
      
      Cand_M = array(rep(0, K*G*K*10), c(K,G,K,10)) #in K1, G, imprint K2, i5, Sr, Cap_Scale)
      Cand_F = Cand_M
      
       
      
      Cand_M[,,,1] = N_Imprint[,4,t,,]-N_Imprint_F[,4,t,,]
      Cand_F[,,,1] = N_Imprint_F[,4,t,,]
      
      
      for (k in 1:K) {
            for (i5 in 1:9) {
                  Cand_M[k,,,i5+1] = (N5_Imprint[k,i5,t,,]-N5_Imprint_F[k,i5,t,,]) *N5.Pstay_M[k,i5,t]
                  Cand_F[k,,,i5+1] = N5_Imprint_F[k,i5,t,,] *N5.Pstay_F[k,i5,t]
            }
      }
      
#####################################################################################
##### Pete Feb 2016 Commented out to shift RBT spawning to correct time step
#####################################################################################      

#       ##### A brief aside to calculdate rainbow spawners, since we're here... similar code to be used
#       ##### to calculate smolters for next life step, exept we'll integrate over all ages
#       
#       TempRBS_F = N5_Imprint_F*0
#       TempRBS_M = N5_Imprint_F*0
#       
#       
#       ## Old Way
#       ## Sum Rainbow Spawners across all sites, assign to imprint site
#       #for (i5 in 1:10) {
#       #   for (g in 1:G) {
#       #    for (k in 1:K) {
#       #	    TempRBS_F[k,i5,t,g,] = N5_Imprint_F[k,i5,t,g,] * N5.Pspawn_F[k,i5,t]
#       #	    TempRBS_M[k,i5,t,g,] = (N5_Imprint-N5_Imprint_F)[k,i5,t,g,] * N5.Pspawn_M[k,i5,t]
#       #        }
#       # for (k2 in 1:K) {
#       #	 RainbowSpawners_F[k2, i5, t, g]= sum(TempRBS_F[,i5,t,g,k2])
#       #	 RainbowSpawners_M[k2, i5, t, g]= sum(TempRBS_M[,i5,t,g,k2])
#       #    }
#       #}}
#       #RainbowSpawners_F_Saved  = RainbowSpawners_F
#       #RainbowSpawners_M_Saved  = RainbowSpawners_M
#       
#       # New Way
#       # Sum Rainbow Spawners across all sites, assign to imprint site
#       #for (i5 in 1:10) {
#       #   for (g in 1:G) {
#       for (k in 1:K) {
#             TempRBS_F[k,,t,,] = N5_Imprint_F[k,,t,,] * N5.Pspawn_F[k,,t]
#             TempRBS_M[k,,t,,] = (N5_Imprint-N5_Imprint_F)[k,,t,,] * N5.Pspawn_M[k,,t]
#       }
#        for (k2 in 1:K) {
#             # Here's the problem when there's only one site - TempRBS_F and TempRBS_M
#             RainbowSpawners_F[k2, , t, ]= apply(TempRBS_F[,,t,,k2],c((1+1*(K>1)),(2+1*(K>1))),sum)
#             RainbowSpawners_M[k2, , t, ]= apply(TempRBS_M[,,t,,k2],c((1+1*(K>1)),(2+1*(K>1))),sum)
#       }
#       #}}
#       #max(RainbowSpawners_F-RainbowSpawners_F_Saved)
#       #min(RainbowSpawners_F-RainbowSpawners_F_Saved)
#####################################################################################
##### End Pete Feb 2016 Commented out stuff
#####################################################################################      


      
      
      #####################
      ### Figure out Candidate Equivalent First Year Fish (i.e. adjusted by cap scalar)
      ### Note:  the (3-1*(K==1)) garbage is due to the fact that R has the
      ### annoying "feature" of dropping a dimension if it's of length 1
      
      Cand_Y1_Equivalent = rep(0, K)
      for (k in 1:K) {
            Cand_Y1_Equivalent[k] = sum(
            apply(Cand_M[k,,,],(3-1*(K==1)), sum)*N5.cap[k,,t] +
            apply(Cand_F[k,,,],(3-1*(K==1)), sum)*N5.cap[k,,t]
            )
      }
      
      # print(paste("Cand_Y1_E",Cand_Y1_Equivalent))
      
      
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
            #pN5[k] = SR5.Candidate[k]*(sum(Prod_Scalar[k,,5,t]*L[k,,t])/sum(L[k,,t]))
            
              pN5[k] = SR5.Candidate[k]*(sum(Prod_Scalar[k,,5,t]*L[k,,t])/sum(L[k,,t]))
      }
      
      
      #print(paste("pN5=", pN5))
      #print(Cand_Y1_Equivalent)
      
      ### Figure out Equivalent First Year fish at time t+1
      
      # Equivalent first year fish
      N_Equiv = rep(0,K)
      #changed to capacity at stage 4
      
      for (k in 1:K) {
          	N_Equiv[k]= Cand_Y1_Equivalent[k]  / (1/pN5[k] + 1/c[k,4,t] * Cand_Y1_Equivalent[k])
            if (is.na(N_Equiv[k])) {N_Equiv[k]=0}
      }
      
      
      #ratio = N_Equiv/(Cand_Y1_Equivalent[k]+.00000001)
      #print(paste("ratio=",ratio))
      #print(paste("pre-smolts=", N_Equiv))
      
      ### Figure out Actual fish at t+1
      # Equvialent 1st year fish of Temp.Cand.Fish # which are in correct relative proportion
      
      k=1
      Y1_Equiv_Temp = rep(0,K)
      for (k in 1:K) {
            Y1_Equiv_Temp[k] = sum(
              apply(((temp.Cand_M+temp.Cand_F)*1)[k,,,],(3-1*(K==1)),sum)*N5.cap[k,,t]
              )
      }
      
      
      #print(paste("Y1_Equiv", Cand_Y1_Equivalent))
      #print(paste("Y1_Equiv_Temp", Y1_Equiv_Temp[k]))
      #print(temp.Cand_M)
      #print(temp.Cand_F)
      # Capacity Correction to ensure capacity usage of remaining fish equals equivalant capacity of
      # equivalent first year fish
      
      Correction = N_Equiv/(Y1_Equiv_Temp+.000000000001)
      for (k in 1:K) {
            for (i5 in 1:10) {
                  for (g in 1:G) {
                        N5_Imprint_F[k,i5,t+1,g,] = temp.Cand_F[k,g,,i5] * Correction[k]
                        N5_Imprint[k,i5,t+1,g,] = temp.Cand_M[k,g,,i5] * Correction[k] + N5_Imprint_F[k,i5,t+1,g,]
                        
                        N5[k,i5,t+1,g] = sum(N5_Imprint[k, i5, t+1,g,])
                        N5_F[k,i5,t+1,g] = sum(N5_Imprint_F[k, i5, t+1,g,])
                        
                        
                        N[k,5,t+1,g] = sum(  N5[k,,t+1,g])
                        N_F[k,5,t+1,g] = sum(N5_F[k,,t+1,g])
                  }
            }
            #print(paste("pre-smolts final",N[1,5,t+1,1]))
      }
      


      #####################################################################################
      #####################################################################################
      ##### Pete Feb 2016 Attempt to get Rainbow trout/Resident fish maturation & spawning
      ##### to occur in the same time step; previously there was a one-year lag which did 
      ##### not capture the biology of the fish
      #####################################################################################      

      TempRBS_F = N5_Imprint_F*0
      TempRBS_M = N5_Imprint_F*0

      for (k in 1:K) {
        #for (i5 in 1:10) {
            TempRBS_F[k,,t,,] = N5_Imprint_F[k,,t+1,,] * N5.Pspawn_F[k,,t+1]
            #print(paste(N5_Imprint_F[k,i5,t+1,,]*N5.Pspawn_F[k,i5,t+1]))
            TempRBS_M[k,,t,,] = (N5_Imprint-N5_Imprint_F)[k,,t+1,,] * N5.Pspawn_M[k,,t+1]
            #print(paste("t=",t,N5_Imprint_F[1,1:3,t+1,1,1],N5.Pspawn_F[1,1:3,t+1]))
            #print(paste("t=",t,TempRBS_F[k,,t,,]))#,N5.Pspawn_F[1,1:3,t+1]))
            #print(max(TempRBS_F[,,,,]))
        #}
      }
  
      for (k2 in 1:K) {
        # Here's the problem when there's only one site - TempRBS_F and TempRBS_M
        
        RainbowSpawners_F[k2, , t+1, ]= apply(TempRBS_F[,,t,,k2],c((1+1*(K>1)),(2+1*(K>1))),sum)
        RainbowSpawners_M[k2, , t+1, ]= apply(TempRBS_M[,,t,,k2],c((1+1*(K>1)),(2+1*(K>1))),sum)
        #RainbowSpawners[k2, , t+1, ]=RainbowSpawners_M[k2, , t+1, ]+RainbowSpawners_F[k2, , t+1, ]
      }
      #print(apply(TempRBS_F[,,t,,1],c((1+1*(1>1)),(2+1*(1>1))),sum))
      #print(paste(RainbowSpawners_F[1,1:3,t+1,1]))

      #####################################################################################
      ##### End Pete Feb 2016 edits
      #####################################################################################







      
      # Add Hatchery Introductions
      # Add in hatchert fish (g=2, imprinted to site at which added.
      for (k in 1:K) {
            N5[k,1,t+1,2] = N5[k,1,t+1,2] + (Hatch_Fish[k,5,t+1])
            N5_F[k,1,t+1,2] = N5_F[k,1,t+1,2] + 0.5*(Hatch_Fish[k,5,t+1])
            N5_Imprint[k,1,t+1,2,k] = N5_Imprint[k,1,t+1,2,k] + Hatch_Fish[k,5,t+1]
            #Error? Pete March 2016, next line should read N5_Imprint_F[k,1,t+1,2,k], right?
            #BAD code: N5_Imprint_F[k,4,t,2,k] = N5_Imprint_F[k,1,t+1,2,k] + 0.5*Hatch_Fish[k,5,t+1]
            N5_Imprint_F[k,1,t+1,2,k] = N5_Imprint_F[k,1,t+1,2,k] + 0.5*Hatch_Fish[k,5,t+1] #Fixed Code
            N[k,5,t+1,2] =  N[k,5,t+1,2] + Hatch_Fish[k,5,t+1]
            N_F[k,5,t+1,2] =  N_F[k,5,t+1,2] + 0.5*Hatch_Fish[k,5,t+1]
      } # end of K for pre-smolt
      
      
      ######################################################################
      ###################################################
      #########################################################
      # Move X-Site Migration of Pre-Smolt's Here for N6
      #print(paste("t=",t,"f1"))
      
      ################### apply Cross-Site Migration for Pre-Smolts #############
      ################## apply Cross-Site Migration for Fry #############
      #Presmolt.x.siteMigration[, ,t+1] = t(array(c(.9,.1,0,0,.95, .05, 0, .3, .7), c(3,3)))
      
      if (K >1) {
      
            ### Extra Complexity due to 10 pre-smolt "sub" life stages....
            #savedN5_Imprint = N5_Imprint
            #savedN5_Imprint_F = N5_Imprint_F
            
            # Initialize Migrattion matrics: to k1 from k2 imprinted to k3
            PS_Migrate = array(0, c(K,K,K,I5,G)) # to k1 from k2 imprinted to k3
            PS_Migrate_F = array(0, c(K,K,K,I5,G)) # to k1 from k2 imprinted to k3
            
            
            for (k1 in 1:K) {
                  #for (g in 1:G) {
                  for (k2 in 1:K){
                        for (k3 in 1:K) {
                              for (i5 in 1:I5) {
                                    PS_Migrate[k1, k2, k3,i5, ] = 
                                    N5_Imprint[k2,i5,t+1,,k3]*Presmolt.x.siteMigration[k2, k1,t+1] # Presmolt_Migrate[to k1 from k2 for g]
                                    PS_Migrate_F[k1, k2, k3,i5,] = 
                                    N5_Imprint_F[k2,i5,t+1,,k3]*Presmolt.x.siteMigration[k2, k1,t+1]
                              }
                        }
                  }
            }
            #}
            
            # Re-assign to N and N_Imprint for male and female
            for (k in 1:K) {
                  for (i5 in 1:I5) {
                        for (g in 1:G) {
                              N5[k,i5,t+1,g] =sum(PS_Migrate[k,,,i5,g])
                              N5_F[k,i5,t+1,g] =sum(PS_Migrate_F[k,,,i5,g])
                              N5_Imprint[k,i5,t+1,g,] = apply(PS_Migrate[k,,,i5,g],2,sum)
                              N5_Imprint_F[k,i5,t+1,g,] = apply(PS_Migrate_F[k,,,i5,g],2,sum)
                   
#                               if(N5[k,i5,t+1,g] >0){ #DEBUG VB
#                                 print(paste("t = ",t," k = ",k,"i = ", i, " g = ",g," PSMig = " , N5[k,i5,t+1,g]))
#                                 
#                               }
                              
                              
                        }
                          
                  }
            }
            
            
      } # end of x-site migrate
      
      ### HERE!!!!!
      for (k in 1:K) {
            N[k,5,t+1,] = apply(N5[k,,t+1,], 2, sum)
            N_F[k,5,t+1,] = apply(N5_F[k,, t+1,],2, sum)
      }
      
      #print(paste("t=",t,"f2"))
      
      ###############################################################
      
      ############################################################################
      ############################################################
      
      
      ###########
      #N6 (smolt, based on same year's presmolts that decide to smolt )
      # Also complicated slightly by N5's are a mixture of mutiple ages 
      # of pre-smolts, at least in the Steelhead case
      
      # Old Way
      
      #      CandN6_Imprint_M = array(0, c(K,I5,G,K))
      #      CandN6_Imprint_F = array(0, c(K,I5,G,K))
      #
      #for (k in 1:K) {
      #        for (i5 in 1:I5) {
      #          for (g in 1:G) {
      #           CandN6_Imprint_M[k,i5,g,]=(N5_Imprint-N5_Imprint_F)[k,i5,t,g,]*N5.Psmolt_M[k,i5,t]
      #           CandN6_Imprint_F[k,i5,g,]=N5_Imprint_F[k,i5,t,g,]*N5.Psmolt_F[k,i5,t]
      #      }}
      #}
      #CandN6_Imprint_M.Saved = CandN6_Imprint_M
      #CandN6_Imprint_F.Saved = CandN6_Imprint_F
      
      # New Way
      
      CandN6_Imprint_M = array(0, c(K,I5,G,K))
      CandN6_Imprint_F = array(0, c(K,I5,G,K))
      
      for (k in 1:K) {
            for (i5 in 1:I5) {
                  for (g in 1:G) {
                        CandN6_Imprint_M[,i5,g,]=(N5_Imprint-N5_Imprint_F)[,i5,t,g,]*N5.Psmolt_M[,i5,t]
                        CandN6_Imprint_F[,i5,g,]=N5_Imprint_F[,i5,t,g,]*N5.Psmolt_F[,i5,t]
                  }
            }
      }
##***Pete May 2015 Addition***
      for (k in 1:K) {
        for (i5 in 1:I5) {
          for (g in 1:G) {
            Candidate_Smolt_Age[k,i5,t,g]=CandN6_Imprint_F[k,i5,g,k]+CandN6_Imprint_M[k,i5,g,k]
            Candidate_Smolt_Age[k,i5,t,g]=Candidate_Smolt_Age[k,i5,t,g]*Sr[k,6,t] #to get them into JDA equivalents
          }
        }
      }
##***Pete May 2015 Addition***
      
      #print(max(CandN6_Imprint_M.Saved - CandN6_Imprint_M))
      #print(min(CandN6_Imprint_M.Saved - CandN6_Imprint_M))
      #print(max(CandN6_Imprint_F.Saved - CandN6_Imprint_F))
      #print(min(CandN6_Imprint_F.Saved - CandN6_Imprint_F))
      
      
      CandidateN6= array(rep(0,(K*G)), c(K,G))
      
      for (k in 1:K) {
            for (g in 1:G) {
                  CandidateN6[k,g] = sum((CandN6_Imprint_M+CandN6_Imprint_F)[k,,g,])
            }
      }
      
      Candidate_Smolt[,t,]=CandidateN6
      
      ## why is pn5 used below????  Need to fix this in case all pre-smolts smolt
      ## should address smolting fish, not fish that remained unsmolted!!
      ## add code here!!!!
      for (k in 1:K) {
            for (g in 1:G) {
                  N[k,6,t,g]=(CandidateN6[k,g]*Rel_Surv[k,5,t,g]) / 
                  (1/Sr[k,6,t]+ 1/c[k,5,t] * 
                  sum(Rel_Surv[k,5,t,]*CandidateN6[k,])) # +(Hatch_Fish[k,5,t]*(g==2))	
            }
      } # end of K
      
      # Assign Imprints and M/F in same ratios as incoming candidate smolts
      temp =apply((CandN6_Imprint_M+CandN6_Imprint_F),c(1,3,4),sum)
      tempF = apply((CandN6_Imprint_F),c(1,3,4),sum)

      
      
      #New Way
      #########
      #  for (k in 1:K) {
      #      for (g in 1:G) {
      for (k2 in 1:K) {
      if (K==1) {
         N_Imprint[,6,t,,k2] = temp[,,k2] * N[,6,t,]/ as.vector(temp)
         N_Imprint_F[,6,t,,k2] = tempF[,,k2] * N[,6,t,]/ as.vector(temp)} else 
      {
               N_Imprint[,6,t,,k2] = temp[,,k2] * N[,6,t,]/ apply(temp[,,],c(1,2),sum)
               N_Imprint_F[,6,t,,k2] = tempF[,,k2] * N[,6,t,]/ apply(temp[,,],c(1,2),sum) # +.0000000001)
           }
               N_Imprint[is.na(N_Imprint)]= 0
               N_Imprint_F[is.na(N_Imprint_F)]= 0
      }
      
      #}
      ###########
      
      for (k in 1:K) {
            N[k,6,t,2] = N[k,6,t,2]+ Hatch_Fish[k,6,t]
            N_Imprint[k,6,t,2,k] = N_Imprint[k,6,t,2,k]+ Hatch_Fish[k,6,t]
            N_Imprint_F[k,6,t,2,k] = N_Imprint_F[k,6,t,2,k] + 0.5*Hatch_Fish[k,6,t]
            
            if (K>1) {
                  N[k,6,t,] = apply(N_Imprint[k,6,t,,],1,sum)
                  N_F[k,6,t,] = apply(N_Imprint_F[k,6,t,,],1,sum)
            } else {
                  N[k,6,t,] = N_Imprint[1,6,t,,1]
                  N_F[k,6,t,] = N_Imprint_F[1,6,t,,1]
            }
      }
      
      #print(max(saved.N_Imprint - N_Imprint))
      #print(min(saved.N_Imprint - N_Imprint))
      
      ###################################################
      ### For Adjust Stages, Move Everybody Back to site of Imprint
      # Call Smolt stage, adjusted back to imprint site, "Smolt_Imprint"
      #Smolt_Imprint = N[k,6,t,g]*0
      #Smolt_Imprint_F = N_F[k,6,t,g]*0
      
      
      ###########
      # N7:  Smolt to Adult (Dam survival rate dependent only, c[k,6,t] assumed negligible (Eq 4)
      
      ### For Adjust Stages, Move Everybody Back to site of Imprint
    	# sum across sites, assign (via c(3,2) to imprinted site
      if (K>1) {
            tempN=apply(N_Imprint[,6,t,,],c(3,2),sum) 
            tempN_F = apply(N_Imprint_F[,6,t,,],c(3,2),sum) 
      } else {
            tempN = array(N[1,6,t,], c(1,G))
            tempN_F = array(N_F[1,6,t,],c(1,G)) 
      }
      
      for(k in 1:K) {
            for (g in 1:G) {
                  N[k,7,t,g] = tempN[k,g] / (1/Sr[k,7,t] ) * Rel_Surv[k,6,t,g]
                  N_F[k,7,t,g] = N[k,7,t,g] * tempN_F[k,g]/tempN[k,g]
                  N_F[is.na(N_F)] = 0
                  #if(k==1){print(paste(N[k,7,t,g],Sr[k,7,t],Rel_Surv[k,6,t,g]))}
            }
      }
      
      # Ignore imprinting from now on - assigned site IS imprinted site
      
      ###########
      # N8: Adult (Ocean age 1) Fish, based on prior year's ocean year 
      
      for (g in 1:G) {
            for (k in 1:K) {
                  N[k,8,t+1,g]=N[k,7,t,g]*Rel_Surv[k,7,t,g] / 
                  (1/Sr[k,8,t]+ 1/C_ocean[k,1,t] * 
                  sum(Rel_Surv[k,7,t,]*N[k,7,t,]))
                  # Calculate Females, assuming same proportion as previously
                  N_F[k,8,t+1,g] = N[k,8,t+1,g] * N_F[k,7,t,g]/N[k,7,t,g]
                  N_F[is.na(N_F)] = 0
            }
      }
      
      
      ######################################
      # end of cycle through watersheds
      ######################################
      
      
      #########################################################  
} 

###################################
# end of cycle through years (t)
###################################




#plot(seq(1:50), N[1,17,])
#plot(seq(1:50), N[1,1,])
#dim(N)




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
  "p"=p, "c"=c,
  "Candidate_Smolt"=Candidate_Smolt
#   ###***Pete May 2015 Addition***
  ,"Male_Spawners"=Male_Spawners,
  "NT"=NT,
  "CandSmoltsByAge"=Candidate_Smolt_Age,
  "RainbowSpawners"=Rainbow_Spawners,
  "RainbowFemSpawners"=Rainbow_Female_Spawners
#   ###***Pete May 2015 Addition***
)
)


} #End of function



