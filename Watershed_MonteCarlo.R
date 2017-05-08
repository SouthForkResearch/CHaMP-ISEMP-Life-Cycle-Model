
### This File contains two functions:
#
# Initialize.Variables(header)
# MonteCarlo(Inputs, variables, header)

###################################################

Initialize.Variables <- function(header.data) {
attach(header.data)
#detach(header.data)
#detach(header)
#attach(header)

	# Initial Values for N
 	N=array(rep(0, K*Tr*I),c(K,I,Tr,G))
	N5=array(rep(0,K*I5*Tr*G),c(K,I5,Tr,G))

#     Read initial values for csv files

k=1
I
for (k in 1:K) {

inits = read.csv(Init.file.names[k], skip=1, header=F, nrows=11)[,3:19]
inits
N[k,1,1,] = rep(0, G)
for (i in 2:I) {  
N[k,i,1,]=inits[,(i-1)]
N[k,i,2,]=inits[,(i-1)]  # Initialze time 2 = time 1 
}
N[k,,1,]

dim(N5)
for (i5 in 1:I5) {  
 for (g in 1:G) {
N5[k,i5,1,g]=inits[g,(12+i5)]
N5[k,i5,2,g]=inits[g,(12+i5)] # Initialize time 2 = time 1
}}
N5[k,,,1]

rm(inits)

}

vars = list(
#      "Fecund" = Fecund,
	"N"=N,	
	"N5"=N5,	
	"NT"=array(rep(0, K*Tr*10*G),c(K,10,Tr,G)),
	"c"=array(rep(0,K*(I)*Tr),c(K,I,Tr)),
	"p"=array(rep(0,I*K*Tr),c(K,I,Tr)),
	"H"=array(rep(0,K*(J+1)*Tr),c(K,(J),Tr)),
	"L"=array(rep(0,Q*K*Tr),c(K,Q,Tr))
)


detach(header.data)
return(vars)
}


##############################



##############################################################
MonteCarlo <- function(Input.Data, Var.data, header.data) {
attach(Input.Data)
attach(Var.data)
attach(header.data)

# comment these out
#detach(header)
#detach(variables)
#detach(Inputs)

# comment these out
#attach(header)
#attach(variables)
#attach(Inputs)

Frac.Subyear.M= Frac.Subyear.M.mu
Frac.Subyear.F= Frac.Subyear.F.mu

	Sr=Sr.mu
	SR5=SR5.mu
#	Sr[5]=SR5[1]

	Sr=Sr
	SR5 =SR5

	
	N5.Psmolt_F=N5.Psmolt_Female.mu
      N5.Pstay_F= N5.Pstay_Female.mu
      N5.Pspawn_F=N5.Pspawn_Female.mu    
	N5.Psmolt_M=N5.Psmolt_Male.mu
      N5.Pstay_M= N5.Pstay_Male.mu
      N5.Pspawn_M=N5.Pspawn_Male.mu    

	N5.cap= N5.cap.mu
	N5.cap=N5.cap.mu
	Mat8Plus_F=Mat8Plus_Female.mu
	Mat8Plus_M=Mat8Plus_Male.mu

# Removed 4/10/2017
#      C_ocean = C_ocean.mu

	D=D.mu
	Prod_Scalar=Prod_Scalar.mu
	frac = frac.mu
#	N5.cap= N5.cap.mu

N5.Rainbow.Fecundity=N5.Rainbow.Fecundity
	Hatch_Fish = Hatch_Fish.mu
	Rel_Surv = Rel_Surv.mu
	Rel_Comp = Rel_Comp.mu
#	Rel_Fecund = Rel_Fecund.mu


#	Female_Frac= Female_Frac.mu
      Female_Fecundity = Female_Fecundity.mu

Post_Spawn_Survival_Anadromous_F = Post_Spawn_Survival_Anadromous_F.mu
Post_Spawn_Survival_Anadromous_M = Post_Spawn_Survival_Anadromous_M.mu

Post_Spawn_Survival_Rainbow_F = Post_Spawn_Survival_Rainbow_F.mu 
Post_Spawn_Survival_Rainbow_M = Post_Spawn_Survival_Rainbow_M.mu 




##################################################################
##################################################################
#
#### Dirichlet Variable Monte-Carlo #################################
#
##################################################################
##################################################################


#	#################################################
#	# Randomly Pull from distributions for MC set 1
#	#################################################
#	if (MCsim1 ==1) {
#
#	# 1). Generate Table 2-3 and Table 2-4 (attachement 1) parameters based 
#	# on means and uncertainties specified in input files
#	# Sample from distributions defined in Table 2-3
#	# currently NOT time dependent, so M(t=1) =M(t=2, 3, 4, 5...)
 

#############################################################
#### randomly correlated Dirichlet (or Beta) probabilities
# for M 
# for J-dimensional RV

# Calculate M[k,q,j,t]
# Initialize some stuff
	M.alphaR=array(rep(0,K*Q*J*Tr), c(K,Q,J,Tr))
	M.alphaT=array(rep(0,K*Q*J*Tr), c(K,Q,J,Tr))
	M.alphaS=array(rep(0,K*Q*J*Tr), c(K,Q,J,Tr))
	M.alpha=array(rep(0,K*Q*J*Tr), c(K,Q,J,Tr))
	M.target.alphaR=array(rep(0,K*Q*J*Tr), c(K,Q,J,Tr))
  M.target.r = M.target
	M=M.mu


# Run to Run Component (i.e. initialize for the run
# Initialize M for the run

# assume we want to include run-run variability if R > 1
if ((R > 1) * (MCsim1==1)) { 
	for (j in 1:J) {
		M.alphaR[,,j,]=M.mu[,,j,] * M.alphaR.N[,,]
		M.target.alphaR[,,j,]=M.target[,,j,] * M.alphaR.N[,,]
	}
       

	R.unif=runif(J)
	for (t in 1:Tr) {
	for (q in 1:Q) {
	for (k in 1:K) {
	# using a gamma than then taking %'s in this way will generate
	# a random dirichlet distribution.
		y.j = round(qgamma(R.unif,shape=M.alphaR[k,q,,t]+.000001),5)
            ytarget.j =round(qgamma(R.unif,shape=M.target.alphaR[k,q,,t]+.000001),5)
	 	for (j in 1:J){
			M[k,q,j,t]= y.j[j] / (sum(y.j)+.000000001)
			M.target.r[k,q,j,t]= ytarget.j[j] / (sum(ytarget.j)+.0000000001)
		}
          	}
		}
		}

	}


# Year-Year Component

####################################
# Temporal Drift (Always include this)

	for (g in 1:N.input.files) {
       if (N.input.files==1) 
           {T.lo=1
            T.hi=(Tr+1)} else if (g==N.input.files) {
		  T.lo=T.step.change[g]
	 	  T.hi= (Tr+1) } else {
              T.lo=T.step.change[g]
		  T.hi=T.step.change[g+1]}



#print(paste("g=",g, "T.hi=", T.hi))
# Add temporal drift
      for (t in (T.lo+1):T.hi) {
       for (j in 1:J) {
# print(paste("t=",t,"T.lo=", T.lo))
	Diff = (M.target.r[,,j,t-1]-M[,,j,t-1])*exp(-1/((t-T.lo)*(.000000001+M.rate[,,t-1]))) 
      M[,,j,t-1] = M[,,j,t-1]+ Diff
 }
}
}

##############################################################
# year to year, site to site, within site variability ##########
#plot(seq(1:Tr), M[1,1,1,])

if ((MCsim1==1)*(MCsim4==1))  {

	for (j in 1:J) {M.alphaT[,,j,]=M[,,j,] * M.alphaT.N[,,] }

	for (t in 2:Tr) {

	# Here's the shared random bit
		T.unif=runif(J)
	for (q in 1:Q) {
	for (k in 1:K) {
	# using a gamma than then taking %'s in this way will generate
	# a random dirichlet distribution.
		y.j = round(qgamma(T.unif,shape=M.alphaT[k,q,,t]+.000001),5)
	 	for (j in 1:J){M[k,q,j,t]= y.j[j] / (sum(y.j)+.000000001)}
          }
	}

# Now for the site-site component

	for (j in 1:J) {M.alphaS[,,j,]=M[,,j,] * M.alphaS.N[,,]}
	for (k in 1:K) {
		
	S.unif=runif(J)
	for (q in 1:Q) {
		y.j = round(qgamma(S.unif,shape=M.alphaS[k,q,,t]+.00001),5)
	 	for (j in 1:J){M[k,q,j,t]= y.j[j] / (sum(y.j)+.000000001)}
	  }
	}

# Now for the within site component
	for (j in 1:J) {M.alpha[,,j,]=M[,,j,] * M.alpha.N[,,]}
	for (k in 1:K) {
	for (q in 1:Q) {
		withinS.unif=runif(J)
		y.j = round(qgamma(withinS.unif,shape=M.alpha[k,q,,t]+.00001),5)
 		for (j in 1:J){M[k,q,j,t]= y.j[j] / (sum(y.j)+.000000001)}
	  }
# May want to change nestoing to make it equivalent to how I did Sr, below
# i.e. site-site and within-site variability not nested within time, but
# constant over time.  Not sure which makes most sense physically. 
	}
} # close time loop
} # end if "If (MCsim2==1) statemente


#plot(M[1,1,1,])
#plot(M.mu, M)
rm(M.alphaR)
rm(M.alphaT)
rm(M.alphaS)
rm(M.alpha)
################################################################



############################################################
#### randomly correlated Dirichlet (or Beta) probabilities
# for OnePlus:  Pspawn, Pstay, Psmolt 
# for J-dimensional RV

# Note: males and females will be 100% correlated year-year and site-site, but independent within site,
# for smolt/spawn/stay probabilities
# Also, males/females 100% correlated for run-run (Monte-Carlo) simulations

# Initialize some stuff

  N5P_F =array(rep(0,K*I5*3*Tr), c(K,I5,3,Tr))
	N5P_F.target = array(rep(0,K*I5*3*Tr), c(K,I5,3,Tr))
	N5P_F.alphaR=array(rep(0,K*I5*3*Tr), c(K,I5,3,Tr))
	N5P_F.alphaT=array(rep(0,K*I5*3*Tr), c(K,I5,3,Tr))
	N5P_F.alphaS=array(rep(0,K*I5*3*Tr), c(K,I5,3,Tr))
	N5P_F.alpha=array(rep(0,K*I5*3*Tr), c(K,I5,3,Tr))
	N5P_F.target.alphaR=array(rep(0,K*I5*3*Tr), c(K,I5,3,Tr))
  N5P_M =array(rep(0,K*I5*3*Tr), c(K,I5,3,Tr))
	N5P_M.target = array(rep(0,K*I5*3*Tr), c(K,I5,3,Tr))
	N5P_M.alphaR=array(rep(0,K*I5*3*Tr), c(K,I5,3,Tr))
	N5P_M.alphaT=array(rep(0,K*I5*3*Tr), c(K,I5,3,Tr))
	N5P_M.alphaS=array(rep(0,K*I5*3*Tr), c(K,I5,3,Tr))
	N5P_M.alpha=array(rep(0,K*I5*3*Tr), c(K,I5,3,Tr))
	N5P_M.target.alphaR=array(rep(0,K*I5*3*Tr), c(K,I5,3,Tr))

N5P_F[,,1,] = N5.Psmolt_Female.mu 
N5P_F[,,2,] = N5.Pspawn_Female.mu
N5P_F[,,3,] = N5.Pstay_Female.mu
N5P_F.target[,,1,] = N5.Psmolt_Female.target
N5P_F.target[,,2,] = N5.Pspawn_Female.target
N5P_F.target[,,3,] = N5.Pstay_Female.target

N5P_M[,,1,] = N5.Psmolt_Male.mu 
N5P_M[,,2,] = N5.Pspawn_Male.mu
N5P_M[,,3,] = N5.Pstay_Male.mu
N5P_M.target[,,1,] = N5.Psmolt_Male.target
N5P_M.target[,,2,] = N5.Pspawn_Male.target
N5P_M.target[,,3,] = N5.Pstay_Male.target
     



# Run to Run Component (i.e. initialize for the run
# Initialize M for the run

#dim(N5.Psmolt.target)

# Run-Run Component
	R.unif=runif(3)
	
	N5P_F.targetR = N5P_F.target
    	N5P_F.alphaR[,,1,]=N5.Psmolt_Female.mu[,,] * N5.P.alphaR_Female.N[,,]
    	N5P_F.alphaR[,,2,]=N5.Pspawn_Female.mu[,,] * N5.P.alphaR_Female.N[,,]
    	N5P_F.alphaR[,,3,]=N5.Pstay_Female.mu[,,] * N5.P.alphaR_Female.N[,,]
      N5P_F.target.alphaR[,,1,]=N5.Psmolt_Female.target[,,] * N5.P.alphaR_Female.N[,,]
	N5P_F.target.alphaR[,,2,]=N5.Pspawn_Female.target[,,] * N5.P.alphaR_Female.N[,,]
	N5P_F.target.alphaR[,,3,]=N5.Pstay_Female.target[,,] * N5.P.alphaR_Female.N[,,]

	N5P_M.targetR = N5P_M.target
    	N5P_M.alphaR[,,1,]=N5.Psmolt_Male.mu[,,] * N5.P.alphaR_Male.N[,,]
    	N5P_M.alphaR[,,2,]=N5.Pspawn_Male.mu[,,] * N5.P.alphaR_Male.N[,,]
    	N5P_M.alphaR[,,3,]=N5.Pstay_Male.mu[,,] * N5.P.alphaR_Male.N[,,]
      N5P_M.target.alphaR[,,1,]=N5.Psmolt_Male.target[,,] * N5.P.alphaR_Male.N[,,]
	N5P_M.target.alphaR[,,2,]=N5.Pspawn_Male.target[,,] * N5.P.alphaR_Male.N[,,]
	N5P_M.target.alphaR[,,3,]=N5.Pstay_Male.target[,,] * N5.P.alphaR_Male.N[,,]




if ((R>1)* (MCsim3==1)) {
	for (t in 1:Tr) {
	for (i5 in 1:I5) {
	for (k in 1:K) {
	# using a gamma than then taking %'s in this way will generate
 	# a random dirichlet distribution.
      # start wiih females
     	  y.j = round(qgamma(R.unif,shape=N5P_F.alphaR[k,i5,,t]+.00001),5)
        ytarget.j = round(qgamma(R.unif,shape=N5P_F.target.alphaR[k,i5,,t]+.00001),5)
	 	for (j in 1:3){
              N5P_F[k,i5,j,t]= y.j[j] / (sum(y.j)+.00000001)
              N5P_F.targetR[k,i5,j,t]= ytarget.j[j] / (sum(ytarget.j)+.000001)}

	  #Now males
   	  y.j = round(qgamma(R.unif,shape=N5P_M.alphaR[k,i5,,t]+.00001),5)
        ytarget.j = round(qgamma(R.unif,shape=N5P_M.target.alphaR[k,i5,,t]+.00001),5) #Corrected (from F to M) Pete Aug 2015
	 	for (j in 1:3){
              N5P_M[k,i5,j,t]= y.j[j] / (sum(y.j)+.00000001)
              N5P_M.targetR[k,i5,j,t]= ytarget.j[j] / (sum(ytarget.j)+.000001)}
          }
	}
}
}


##################
# Temporal Drift
# need to track "T.hi-T.lo" as T in exponential decay


	for (g in 1:N.input.files) {
if (N.input.files==1) {T.lo=1; T.hi=Tr+1} else {
           if (g==N.input.files) {
		  T.lo=T.step.change[g]
	 	  T.hi= Tr+1 } else {
              T.lo=T.step.change[g]
		  T.hi=T.step.change[g+1]} 
}
#print(T.lo)
# Add temporal drift
	for (j in 1:3) { 
      for (t in (T.lo+1):T.hi) {
# Females
	Diff = (N5P_F.targetR[,,j,t-1]-N5P_F[,,j,t-1])*exp(-1/((t-T.lo)*(.00000001+N5.P_Female.rate[,,t-1]))) 
      N5P_F[,,j,t-1] = N5P_F[,,j,t-1]+ Diff
# Males
	Diff = (N5P_M.targetR[,,j,t-1]-N5P_M[,,j,t-1])*exp(-1/((t-T.lo)*(.00000001+N5.P_Male.rate[,,t-1]))) 
      N5P_M[,,j,t-1] = N5P_M[,,j,t-1]+ Diff
     }}
}

############################
################################

if ((MCsim3 ==1)*(MCsim4==1)) {

# Year-Year Component
	for (j in 1:3) {N5P_F.alphaT[,,j,]=N5P_F[,,j,] * N5.P.alphaT_Female.N[,,]}
	for (j in 1:3) {N5P_M.alphaT[,,j,]=N5P_M[,,j,] * N5.P.alphaT_Male.N[,,]} #Corrected (from F to M) Pete Aug 2015

	for (t in 1:Tr) {

	# Here's the shared random bit
		T.unif=runif(3)
	for (i5 in 1:I5) {
	for (k in 1:K) {
	# using a gamma than then taking %'s in this way will generate
	# a random dirichlet distribution.
# Females
		y.j = round(qgamma(T.unif,shape=N5P_F.alphaT[k,i5,,t]+.00001),5)
	 	for (j in 1:3){N5P_F[k,i5,j,t]= y.j[j] / (sum(y.j)+.0000000001)}
# Males
		y.j = round(qgamma(T.unif,shape=N5P_M.alphaT[k,i5,,t]+.00001),5)
	 	for (j in 1:3){N5P_M[k,i5,j,t]= y.j[j] / (sum(y.j)+.0000000001)}
          }
	}


# Now for the site-site component

	for (j in 1:3) {N5P_F.alphaS[,,j,]=N5P_F[,,j,] * N5.P.alphaS_Female.N[,,]}
	for (j in 1:3) {N5P_M.alphaS[,,j,]=N5P_M[,,j,] * N5.P.alphaS_Male.N[,,]}

	for (k in 1:K) {
		
	S.unif=runif(3)
	for (i5 in 1:I5) {
# Females
		y.j = round(qgamma(S.unif,shape=N5P_F.alphaS[k,i5,,t]+.00001),5)
	 	for (j in 1:3){N5P_F[k,i5,j,t]= y.j[j] / (sum(y.j)+.00000001)}
# Males
		y.j = round(qgamma(S.unif,shape=N5P_M.alphaS[k,i5,,t]+.00001),5)
	 	for (j in 1:3){N5P_M[k,i5,j,t]= y.j[j] / (sum(y.j)+.00000001)}
	  }
	}

# Now for the within site component

	for (j in 1:3) {N5P_F.alpha[,,j,]=N5P_F[,,j,] * N5.P.alpha_Female.N[,,]}
	for (j in 1:3) {N5P_M.alpha[,,j,]=N5P_M[,,j,] * N5.P.alpha_Male.N[,,]}

	for (k in 1:K) {
#Females
	for (i5 in 1:I5) {
		withinS.unif=runif(3)
		y.j = round(qgamma(withinS.unif,shape=N5P_F.alpha[k,i5,,t]+.000001,1),5)
 		for (j in 1:3){N5P_F[k,i5,j,t]= y.j[j] / (sum(y.j)+.00000001)}
	  }
#Males
	for (i5 in 1:I5) {
		withinS.unif=runif(3)
		y.j = round(qgamma(withinS.unif,shape=N5P_M.alpha[k,i5,,t]+.000001,1),5)
 		for (j in 1:3){N5P_M[k,i5,j,t]= y.j[j] / (sum(y.j)+.00000001)}
	  }
	}
} # close time loop
} #close if MCsim3 ==1



N5.Psmolt_F[,,] =N5P_F[,,1,] 
N5.Pspawn_F[,,] = N5P_F[,,2,]
N5.Pstay_F[,,] = N5P_F[,,3,]

N5.Psmolt_M[,,] =N5P_M[,,1,] 
N5.Pspawn_M[,,] = N5P_M[,,2,]
N5.Pstay_M[,,] = N5P_M[,,3,]


#plot(N5P[1,2,1,])
#############################################


################################################################
### All other Dirichlets and Normal variables can be monte-carlo'd
### along these same lines.  Will need to include temporal drift
### for others where appropriate ################################

# for Sr, SR5

#
# Calculate Sr[k,i,t]
# Initialize some stuff
	Sr.alphaR=array(rep(0,K*I*Tr), c(K,I,Tr))
	Sr.alphaT=array(rep(0,K*I*Tr), c(K,I,Tr))
	Sr.alphaS=array(rep(0,K*I*Tr), c(K,I,Tr))
	Sr.alpha=array(rep(0,K*I*Tr), c(K,I,Tr))
	SR5.alphaR=array(rep(0,K*I5*Tr), c(K,I5,Tr))
	SR5.alphaT=array(rep(0,K*I5*Tr), c(K,I5,Tr))
	SR5.alphaS=array(rep(0,K*I5*Tr), c(K,I5,Tr))
	SR5.alpha=array(rep(0,K*I5*Tr), c(K,I5,Tr))

	Sr.target.R = Sr.target
	Sr.taget.alphaR = array(rep(0,K*I*Tr), c(K,I,Tr))

      SR5.target.R = SR5.target
 	SR5.target.alphaR = array(rep(0,K*I*Tr), c(K,I5,Tr))


# Run to Run Component (i.e. initialize for the run
# Initialize Sr for the run

# Don't worry about Sr[k,1,t] or Sr[k,5,t] as they're special
# cases that get calc'd elsewhere
  Sr.alphaR= Sr.mu * Sr.alphaR.N
  SR5.alphaR = SR5.mu * SR5.alphaR.N
  Sr.target.alphaR = Sr.target * Sr.alphaR.N
  SR5.target.alphaR = SR5.target * SR5.alphaR.N
  Sr =Sr.mu
  SR5 = SR5.mu

#print(Sr.mu)
if ((R>1)* (MCsim3==1)) {
# Run-Run Component
     R.unif=runif(2)
	for (t in 1:Tr) {
	for (k in 1:K) {		

# Sr
      for (i in 2:I) {
	# using a gamma than then taking %'s in this way will generate
	# a random but correted beta distribution.
      if (i != 99) {
   y.i = round(qgamma(R.unif,shape=c(Sr.alphaR[k,i,t]+.00001, 
         (Sr.alphaR.N[k,i,t]-Sr.alphaR[k,i,t]+.00001)),1),5)
	 Sr[k,i,t]= y.i[1] / (sum(y.i)+.000000001)
   y.target.i = round(qgamma(R.unif,shape=c(Sr.target.alphaR[k,i,t]+.00001, 
         (Sr.alphaR.N[k,i,t]-Sr.target.alphaR[k,i,t]+.00001)),1),5)

	 Sr[k,i,t]= y.i[1] / (sum(y.i)+.000000001)
	 Sr.target.R[k,i,t] = y.target.i[1] / (sum(y.target.i)+.000000001)
        }} 
 # SR5
      for (i in 1:I5) {
	# using a gamma than then taking %'s in this way will generate
	# a random but correted beta distribution.
   y.i = round(qgamma(R.unif,shape=c(SR5.alphaR[k,i,t]+.000001, 
         (SR5.alphaR.N[k,i,t]-SR5.alphaR[k,i,t]+.00001)),1),5)
   y.target.i = round(qgamma(R.unif,shape=c(SR5.target.alphaR[k,i,t]+.000001, 
         (SR5.alphaR.N[k,i,t]-SR5.target.alphaR[k,i,t]+.00001)),1),5)
	   SR5[k,i,t]= y.i[1] / sum(y.i)
	   SR5.target.R[k,i,t]= y.target.i[1] / sum(y.target.i)
       } 	
}  # close site loop
} # close time loop
} # close MC loop if R > 1

Sr.mu[1,1:5,1:5]
Sr[1,1:5,1:5]
##############################################3333
# Temporal Drift - always include
# need to track "T.hi-T.lo" as T in exponential decay

for (g in 1:N.input.files) {
if (N.input.files==1) {T.lo=1; T.hi=Tr+1} else {

           if (g==N.input.files) {
		  T.lo=T.step.change[g]
	 	  T.hi= Tr+1 } else {
              T.lo=T.step.change[g]
		  T.hi=T.step.change[g+1]}}

# Add temporal drift
      for (t in (T.lo+1):(T.hi)) {
	SrDiff = (Sr.target.R[,,t-1]-Sr[,,t-1])*exp(-1/((t-T.lo)*(.00000001+Sr.rate[,,t-1]))) 
 	SR5Diff = (SR5.target.R[,,t-1]-SR5[,,t-1])*exp(-1/((t-T.lo)*(.00000001+SR5.rate[,,t-1]))) 
        Sr[,,t-1] = Sr[,,t-1]+ SrDiff 
      SR5[,,t-1]=SR5[,,t-1]+SR5Diff
	}}

#plot(Sr[1,2,])
#plot(seq(1:Tr), SR5[1,5,])

################################

if ((MCsim3 ==1)*(MCsim4==1)) {
# Year-Year Component
	Sr.alphaT= Sr * Sr.alphaT.N
      SR5.alphaT = SR5 * SR5.alphaT.N
	for (t in 1:Tr) {
	# Here's the shared random bit
		T.unif=runif(2)
	for (k in 1:K) {		

# Sr
      for (i in 2:I) {
	# using a gamma than then taking %'s in this way will generate
	# a random but correted beta distribution.
      if (i != 99) {
   y.i = round(qgamma(T.unif,shape=c(Sr.alphaT[k,i,t]+.00001, 
         (Sr.alphaT.N[k,i,t]-Sr.alphaT[k,i,t]+.00001)),1),5)
	 Sr[k,i,t]= y.i[1] / sum(y.i)
       } }	
# SR5
      for (i in 1:I5) {
	# using a gamma than then taking %'s in this way will generate
	# a random but correted beta distribution.
   y.i = round(qgamma(T.unif,shape=c(SR5.alphaT[k,i,t]+.000001, 
         (SR5.alphaT.N[k,i,t]-SR5.alphaT[k,i,t]+.00001)),1),5)
	   SR5[k,i,t]= y.i[1] / sum(y.i)
       } 	
}  # close site loop
} # close time loop





# Site-Site Component
	Sr.alphaS= Sr * Sr.alphaS.N
      SR5.alphaS = SR5 * SR5.alphaS.N

	# Here's the shared random bit
	for (t in 1:Tr) {
	for (k in 1:K) {	
	S.unif=runif(2)


# Sr    
  for (i in 2:I) {
	# using a gamma than then taking %'s in this way will generate
	# a random corrletation beta distribution.
      if (i != 99) {
   y.i = round(qgamma(S.unif,shape=c(Sr.alphaS[k,i,t]+.00001, 
         (Sr.alphaS.N[k,i,t]-Sr.alphaS[k,i,t]+.00001)),1),5)
	 Sr[k,i,t]= y.i[1] / sum(y.i)
       } }

# SR5    
  for (i in 1:I5) {
   y.i = round(qgamma(S.unif,shape=c(SR5.alphaS[k,i,t]+.00001, 
         (SR5.alphaS.N[k,i,t]-SR5.alphaS[k,i,t]+.00001)),1),5)
	 SR5[k,i,t]= y.i[1] / sum(y.i)
        }} #close i, k
} # close time loop



# Within Site Component
	Sr.alpha= Sr * Sr.alpha.N
      SR5.alpha = SR5 * SR5.alpha.N

	for (t in 1:Tr) {
	# Here's the shared random bit
	for (k in 1:K) {	

      for (i in 2:I) {
	withinS.unif=runif(2)
	# using a gamma than then taking %'s in this way will generate
	# a random corrletation beta distribution.
      if (i != 99) {
   y.i = round(qgamma(withinS.unif,shape=c(Sr.alpha[k,i,t]+.00001, 
         (Sr.alpha.N[k,i,t]-Sr.alpha[k,i,t]+.00001)),1),5)
	 Sr[k,i,t]= y.i[1] / sum(y.i)
       }  }	


      for (i in 1:I5) {
	withinS.unif=runif(2)
	# using a gamma than then taking %'s in this way will generate
	# a random corrletation beta distribution.
      y.i = round(qgamma(withinS.unif,shape=c(SR5.alpha[k,i,t]+.00001, 
         (SR5.alpha.N[k,i,t]-SR5.alpha[k,i,t]+.0001)),1),5)
	 SR5[k,i,t]= y.i[1] / sum(y.i)
         }
	}
} # close time loop
} # close if MCsim3==1 loop
#plot(SR5[1,2,])
##################################################################


# Mat8Plus
#
# Calculate Sr[k,i,t]
# Initialize some stuff
	Mat8Plus_F.alphaR=array(rep(0,K*(I-7)*Tr), c(K,(I-7),Tr))
	Mat8Plus_F.alphaT=array(rep(0,K*(I-7)*Tr), c(K,(I-7),Tr))
	Mat8Plus_F.alphaS=array(rep(0,K*(I-7)*Tr), c(K,(I-7),Tr))
	Mat8Plus_F.alpha=array(rep(0,K*(I-7)*Tr), c(K,(I-7),Tr))
	Mat8Plus_F.target.R = Mat8Plus_Female.target
	Mat8Plus_F.target.alphaR = array(rep(0,K*(I-7)*Tr), c(K,(I-7),Tr))
	Mat8Plus_M.alphaR=array(rep(0,K*(I-7)*Tr), c(K,(I-7),Tr))
	Mat8Plus_M.alphaT=array(rep(0,K*(I-7)*Tr), c(K,(I-7),Tr))
	Mat8Plus_M.alphaS=array(rep(0,K*(I-7)*Tr), c(K,(I-7),Tr))
	Mat8Plus_M.alpha=array(rep(0,K*(I-7)*Tr), c(K,(I-7),Tr))
	Mat8Plus_M.target.R = Mat8Plus_Male.target
	Mat8Plus_M.target.alphaR = array(rep(0,K*(I-7)*Tr), c(K,(I-7),Tr))


# Run to Run Component (i.e. initialize for the run
# Initialize Sr for the run


  Mat8Plus_F.alphaR = Mat8Plus_Female.mu * Mat8Plus_Female.alphaR.N
  Mat8Plus_F.target.alphaR = Mat8Plus_Female.target * Mat8Plus_Female.alphaR.N

  Mat8Plus_M.alphaR = Mat8Plus_Male.mu * Mat8Plus_Male.alphaR.N
  Mat8Plus_M.target.alphaR = Mat8Plus_Male.target * Mat8Plus_Male.alphaR.N



# run to run variability
if ((R > 1)* (MCsim3==1)) {
	# Here's the shared random bit
		R.unif=runif(2)
	for (t in 1:Tr) {
	for (k in 1:K) {		
      for (i in 1:6) {
	# using a gamma than then taking %'s in this way will generate
	# a random but correted beta distribution.
# Females
   y.i = round(qgamma(R.unif,shape=c(Mat8Plus_F.alphaR[k,i,t]+.00001, 
         (Mat8Plus_Female.alphaR.N[k,i,t]-Mat8Plus_F.alphaR[k,i,t]+.00001)),1),5)
	   Mat8Plus_F[k,i,t]= y.i[1] / sum(y.i)
   y.target.i = round(qgamma(R.unif,shape=c(Mat8Plus_F.target.alphaR[k,i,t]+.00001, 
         (Mat8Plus_Female.alphaR.N[k,i,t]-Mat8Plus_F.target.alphaR[k,i,t]+.00001)),1),5)
	   Mat8Plus_F.target.R[k,i,t]= y.target.i[1] / sum(y.i)

   y.i = round(qgamma(R.unif,shape=c(Mat8Plus_M.alphaR[k,i,t]+.00001, 
         (Mat8Plus_Male.alphaR.N[k,i,t]-Mat8Plus_M.alphaR[k,i,t]+.00001)),1),5)
	   Mat8Plus_M[k,i,t]= y.i[1] / sum(y.i)
   y.target.i = round(qgamma(R.unif,shape=c(Mat8Plus_M.target.alphaR[k,i,t]+.00001, 
         (Mat8Plus_Male.alphaR.N[k,i,t]-Mat8Plus_M.target.alphaR[k,i,t]+.00001)),1),5)
	   Mat8Plus_M.target.R[k,i,t]= y.target.i[1] / sum(y.i)
       } 	
}  # close site loop
} # close time loop
} # close run to run variability

##########################################
# Temporal Drift (always include)
# need to track "T.hi-T.lo" as T in exponential decay

	for (g in 1:N.input.files) {
if (N.input.files==1) {T.lo=1; T.hi=Tr+1} else {
           if (g==N.input.files) {
		  T.lo=T.step.change[g]
	 	  T.hi= Tr+1 } else {
              T.lo=T.step.change[g]
		  T.hi=T.step.change[g+1]} }

# Add temporal drift
      for (t in (T.lo+1):T.hi) {
# Females
	Diff = (Mat8Plus_F.target.R[,,t-1]-Mat8Plus_F[,,t-1])*exp(-1/((t-T.lo)*(.000000001+Mat8Plus_Female.rate[,,t-1]))) 
      Mat8Plus_F[,,t-1] = Mat8Plus_F[,,t-1]+ Diff 
# Males
	Diff = (Mat8Plus_M.target.R[,,t-1]-Mat8Plus_M[,,t-1])*exp(-1/((t-T.lo)*(.000000001+Mat8Plus_Male.rate[,,t-1]))) 
      Mat8Plus_M[,,t-1] = Mat8Plus_M[,,t-1]+ Diff 
	}}

# plot(seq(1:Tr), Mat8Plus[1,2,])
################################


# other variability
if ((MCsim3 ==1)*(MCsim4==1)) {
# Year-Year Component
      Mat8Plus_F.alphaT = Mat8Plus_F * Mat8Plus_Female.alphaT.N
      Mat8Plus_M.alphaT = Mat8Plus_M * Mat8Plus_Male.alphaT.N

	for (t in 1:Tr) {
	# Here's the shared random bit
		T.unif=runif(2)
	for (k in 1:K) {		
      for (i in 1:6) {
	# using a gamma than then taking %'s in this way will generate
	# a random but correted beta distribution.
# Females
   y.i = round(qgamma(T.unif,shape=c(Mat8Plus_F.alphaT[k,i,t]+.00001, 
         (Mat8Plus_Female.alphaT.N[k,i,t]-Mat8Plus_F.alphaT[k,i,t]+.00001)),1),5)
	   Mat8Plus_F[k,i,t]= y.i[1] / sum(y.i)
# Males
   y.i = round(qgamma(T.unif,shape=c(Mat8Plus_M.alphaT[k,i,t]+.00001, 
         (Mat8Plus_Male.alphaT.N[k,i,t]-Mat8Plus_M.alphaT[k,i,t]+.00001)),1),5)
	   Mat8Plus_M[k,i,t]= y.i[1] / sum(y.i)

       } 	

}  # close site loop
} # close time loop




# Site-Site Component
      Mat8Plus_F.alphaS = Mat8Plus_F * Mat8Plus_Female.alphaS.N
      Mat8Plus_M.alphaS = Mat8Plus_M * Mat8Plus_Male.alphaS.N

	# Here's the shared random bit
	for (t in 1:Tr) {
	for (k in 1:K) {	
	S.unif=runif(2)


# SR5    
  for (i in 1:6) {
   y.i = round(qgamma(S.unif,shape=c(Mat8Plus_F.alphaS[k,i,t]+.00001, 
         (Mat8Plus_Female.alphaS.N[k,i,t]-Mat8Plus_F.alphaS[k,i,t]+.000001)),1),5)
	 Mat8Plus_F[k,i,t]= y.i[1] / sum(y.i)

  y.i = round(qgamma(S.unif,shape=c(Mat8Plus_M.alphaS[k,i,t]+.00001, 
         (Mat8Plus_Male.alphaS.N[k,i,t]-Mat8Plus_M.alphaS[k,i,t]+.000001)),1),5)
	 Mat8Plus_M[k,i,t]= y.i[1] / sum(y.i)

        }} #close i, k
} # close time loop


# Within Site Component

      Mat8Plus_F.alpha = Mat8Plus_F * Mat8Plus_Female.alpha.N
      Mat8Plus_M.alpha = Mat8Plus_M * Mat8Plus_Male.alpha.N

	for (t in 1:Tr) {
	# Here's the shared random bit
	for (k in 1:K) {	

# for Females
      for (i in 1:6) {
	withinS.unif=runif(2)
	# using a gamma than then taking %'s in this way will generate
	# a random corrletation beta distribution.
      y.i = round(qgamma(withinS.unif,shape=c(Mat8Plus_F.alpha[k,i,t]+.00001, 
         (Mat8Plus_Female.alpha.N[k,i,t]-Mat8Plus_F.alpha[k,i,t]+.00001)),1),5)
	 Mat8Plus_F[k,i,t]= y.i[1] / sum(y.i)
         }

# and males 
      for (i in 1:I5) {
	withinS.unif=runif(2)
	# using a gamma than then taking %'s in this way will generate
	# a random corrletation beta distribution.
      y.i = round(qgamma(withinS.unif,shape=c(Mat8Plus_M.alpha[k,i,t]+.00001, 
         (Mat8Plus_Male.alpha.N[k,i,t]-Mat8Plus_M.alpha[k,i,t]+.00001)),1),5)
	 Mat8Plus_M[k,i,t]= y.i[1] / sum(y.i)
         }


	}
} # close time loop
} # close if MCsim3 ==1


#Mat8Plus
#plot(Mat8Plus[1,2,])







##################################################################
##################################################################
#
#### Normal Variable Monte-Carlo #################################
#
##################################################################
##################################################################


### Ak_x_Lqk #############
Ak_x_Lqk=Ak_x_Lqk.mu
Ak_x_Lqk.targetR = Ak_x_Lqk.target


# run to run variation (uncertainty in estimates)

if ((R >1)* (MCsim2==1)) {

  R.unif = runif(1) 
	for (t in 1:Tr) {
		 for (k in 1:K) {
		   for (q in 1:Q) {
	  
     	Ak_x_Lqk[k,q,t] =   max(0.001, 
   (Ak_x_Lqk[k,q,t] + qnorm(R.unif, 0, Ak_x_Lqk.sigmaR[k,q,t]+.00001))) 

     	Ak_x_Lqk.targetR[k,q,t] =   max(0.001, 
   (Ak_x_Lqk.targetR[k,q,t] + qnorm(R.unif, 0, Ak_x_Lqk.sigmaR[k,q,t]+.00001))) 

	 }}}
} # close run to run variability if R > 1



# Always include temporal drift

	for (g in 1:N.input.files) {
if (N.input.files==1) {T.lo=1; T.hi=Tr+1} else {
           if (g==N.input.files) {
		  T.lo=T.step.change[g]
	 	  T.hi= Tr+1 } else {
              T.lo=T.step.change[g]
		  T.hi=T.step.change[g+1]} }

	for (t in (T.lo+1):T.hi) {
	 T.unif = runif(1)
		 for (k in 1:K) {
		   S.unif = runif(1)
		  for (q in 1:Q) {
	   withinS.unif=runif(1)

     	Ak_x_Lqk[k,q,t-1] =   max(0.001, 
   (Ak_x_Lqk[k,q,t-1] +
        (Ak_x_Lqk.targetR[k,q,t-1]-Ak_x_Lqk[k,q,t-1])*exp(-1/((t-T.lo)*(.000000001+Ak_x_Lqk.rate[k,q,t-1]))) +
         ((MCsim2 == 1)*(MCsim4==1)) * #only add variability below if MCsim2==1)
          (qnorm(T.unif, 0 ,Ak_x_Lqk.sigmaT[k,q,t-1]+.00001) + #year to year
            qnorm(S.unif, 0, Ak_x_Lqk.sigmaS[k,q,t-1]+.00001) + # site-to-site
              qnorm(withinS.unif, 0, Ak_x_Lqk.sigma[k,q,t-1]+.00001)))
) # within-site
}}
}}

#MCsim2=0




#plot(seq(1:Tr), Ak_x_Lqk[1,1,], main = "Ak_x_Lqk for Site 1",ylab="Area (sq. m)", xlab = "time (years)")

#plot(Ak_x_Lqk[1,1,1:40], Ak_x_Lqk[2,1,1:40])
#cor(Ak_x_Lqk[1,1,1:40], Ak_x_Lqk[1,2,1:40])
#cor(Ak_x_Lqk[1,1,1:40], Ak_x_Lqk[2,1,1:40])


######################################################









### D (capacity per unit area #############

D.targetR = D.target

# run-run variation
if ((R>1)* (MCsim1==1)) {
    R.unif= runif(1)
	for (t in 1:Tr) {
		 for (k in 1:K) {
		  for (j in 1:(J)) 
             for (i in 1:5) {
{
     	D[k,j,i,t] =   max(0.0, 
   (D[k,j,i,t] +
              qnorm(R.unif, 0, D.sigma[k,j,i,t]))) 

     	D.targetR[k,j,i,t] =   max(0.0, 
   (D.target[k,j,i,t] +
              qnorm(R.unif, 0, D.sigma[k,j,i,t]))) 

# within-site
	 }}}}
} # close run to run variability if R >1 

# temporal drift and variability (if including)

	for (g in 1:N.input.files) {
if (N.input.files==1) {T.lo=1; T.hi=Tr+1} else {
           if (g==N.input.files) {
		  T.lo=T.step.change[g]
	 	  T.hi= Tr+1 } else {
              T.lo=T.step.change[g]
		  T.hi=T.step.change[g+1]}}

	for (t in (T.lo+1):T.hi) {
	 T.unif = runif(1)
		 for (k in 1:K) {
		   S.unif = runif(1)
		  for (j in 1:(J)) {
             for (i in 1:5) {

	   withinS.unif=runif(1)

    	D[k,j,i,t-1] =   max(0.0, 
   (D[k,j,i,t-1] +
       (D.targetR[k,j,i,t-1]-D[k,j,i,t-1])*exp(-1/((t-T.lo)*(.000000001+D.rate[k,j,i,t-1]))) +
         ((MCsim1 ==1)*(MCsim4==1)) * (
          qnorm(T.unif, 0 ,D.sigmaT[k,j,i,t-1]) + #year to year
            qnorm(S.unif, 0, D.sigmaS[k,j,i,t-1]) + # site-to-site
              qnorm(withinS.unif, 0, D.sigma[k,j,i,t-1]))
)) # within-site
}}
}}
}

#plot(D[1,1,2,])

######################################################

# OK to here... need to add T.lo / T.hi stuff from here on!



### Productivity Scalar Prod_Scalar #############


Prod_Scalar.targetR = Prod_Scalar.target

# Run-run variation
if ((R>1)* (MCsim2==1)) {
   R.unif = runif(1)
	for (t in 1:Tr) {
		 for (k in 1:K) {
	  for (q in 1:Q) 
# Changed 5 to 6, below (MTN, 5/8/2017
             for (i in 1:6) {

     	Prod_Scalar[k,q,i,t] =   max(0.0, 
   (Prod_Scalar[k,q,i,t] +
       qnorm(R.unif, 0, Prod_Scalar.sigma[k,q,i,t])))

     	Prod_Scalar.targetR[k,q,i,t] =   max(0.0, 
   (Prod_Scalar.target[k,q,i,t] +
       qnorm(R.unif, 0, Prod_Scalar.sigma[k,q,i,t])))
# within-site
	 }}}
} # close run to run variability if R > 1

# temporal trends and variability if including

	for (g in 1:N.input.files) {
if (N.input.files==1) {T.lo=1; T.hi=Tr+1} else {
           if (g==N.input.files) {
		  T.lo=T.step.change[g]
	 	  T.hi= Tr+1 } else {
              T.lo=T.step.change[g]
		  T.hi=T.step.change[g+1]}}

	for (t in (T.lo+1):T.hi) {
	 T.unif = runif(1)
		 for (k in 1:K) {
		   S.unif = runif(1)
		  for (q in 1:Q) 
             for (i in 1:5) {

	   withinS.unif=runif(1)
     	Prod_Scalar[k,q,i,t-1] =   max(0.0, 
   (Prod_Scalar[k,q,i,t-1] +
   (Prod_Scalar.targetR[k,q,i,t-1]-Prod_Scalar[k,q,i,t-1])*
         exp(-1/((t-T.lo)*(.000000001+Prod_Scalar.rate[k,q,i,t-1]))) +
         ((MCsim2 ==1)*(MCsim4==1)) * (
            qnorm(T.unif, 0 ,Prod_Scalar.sigmaT[k,q,i,t-1]) + #year to year
            qnorm(S.unif, 0, Prod_Scalar.sigmaS[k,q,i,t-1]) + # site-to-site
              qnorm(withinS.unif, 0, Prod_Scalar.sigma[k,q,i,t-1]))
)) # within-site
	 }}}
}

#cor(Prod_Scalar[1,1,1,], Prod_Scalar[2,1,1,])
#cor(Prod_Scalar[,1,1,1], Prod_Scalar[,1,1,4])

# plot(Prod_Scalar[1,1,2,]) ### Here's the problem!!!
#for (t in 1:Tr) { points(Prod_Scalar[,1,1,1], Prod_Scalar[,1,1,t], col=t)}
######################################################





### frac #############


### frac #############

frac.targetR = frac.target


# run-run variation
if ((R>1)* (MCsim2==1)) {
   R.unif = runif(1)	
   for (t in 1:Tr) {
	 for (k in 1:K) {
		for (j in 1:(J)) 
             for (i in 1:6) {
     	frac[k,i,j,t] =   max(0.0, 
   (frac[k,i,j,t] +
        qnorm(R.unif, 0, frac.sigma[k,i,j,t])))
   frac.targetR[k,i,j,t] =   max(0.0, (frac.target[k,i,j,t] +
        qnorm(R.unif, 0, frac.sigma[k,i,j,t])))


	 }}
}
} # end of run to run variability 


# temporal drift, and if including temporal variation

	for (g in 1:N.input.files) {
if (N.input.files==1) {T.lo=1; T.hi=Tr+1} else {
           if (g==N.input.files) {
		  T.lo=T.step.change[g]
	 	  T.hi= Tr+1 } else {
              T.lo=T.step.change[g]
		  T.hi=T.step.change[g+1]}}


	for (t in (T.lo+1):T.hi) {
	 T.unif = runif(1)
		 for (k in 1:K) {
		   S.unif = runif(1)
		  for (j in 1:(J)) 
             for (i in 1:5) {

	   withinS.unif=runif(1)
     	frac[k,i,j,t-1] =   max(0.0, 
   (frac[k,i,j,t-1] +
      (frac.targetR[k,i,j,t-1]-frac[k,i,j,t-1])*
        exp(-1/((t-T.lo)*(.0000000001+frac.rate[k,i,j,t-1]))) +
         ((MCsim2 ==2)*(MCsim4==1)) * (
          qnorm(T.unif, 0 ,frac.sigmaT[k,i,j,t-1]) + #year to year
            qnorm(S.unif, 0, frac.sigmaS[k,i,j,t-1]) + # site-to-site
              qnorm(withinS.unif, 0, frac.sigma[k,i,j,t-1]))
)) # within-site
	 }}}
}




#plot(frac[1,4,4,])
#plot(frac.target[1,4,4,])
#cor(frac[1,1,1,], frac[2,1,1,])



########################################



### N5.cap  #############

 N5.cap.targetR = N5.cap.target


# run to run variation (uncertainty in estimates)
if ((R>1)* (MCsim2==1)) {
R.unif = runif(1)
	for (t in 1:Tr) {
	 for (k in 1:K) {
		    for (i in 1:I5) {
	  N5.cap[k,i,t] =   max(0.0, 
   (N5.cap[k,i,t] +
          qnorm(R.unif, 0, N5.cap.sigma[k,i,t]))) 
	  N5.cap.targetR[k,i,t] =   max(0.0, 
   (N5.cap.target[k,i,t] +
          qnorm(R.unif, 0, N5.cap.sigma[k,i,t]))) 
# within-site
	 }}}
} # close run to run variability if R > 1


#  temporal trends, and if including temporal variation
	for (g in 1:N.input.files) {
if (N.input.files==1) {T.lo=1; T.hi=Tr+1} else {
           if (g==N.input.files) {
		  T.lo=T.step.change[g]
	 	  T.hi= Tr+1 } else {
              T.lo=T.step.change[g]
		  T.hi=T.step.change[g+1]}}

	for (t in (T.lo+1):T.hi) {
	 T.unif = runif(1)
		 for (k in 1:K) {
		   S.unif = runif(1)
             for (i in 1:I5) {
	   withinS.unif=runif(1)
     	N5.cap[k,i,t-1] =   max(0.0, 
   (N5.cap[k,i,t-1] +
  (N5.cap.targetR[k,i,t-1]-N5.cap[k,i,t-1])*
        exp(-1/((t-T.lo)*(.0000000001+N5.cap.rate[k,i,t-1]))) +
        ((MCsim2 ==1)*(MCsim4==1)) * (
          qnorm(T.unif, 0 ,N5.cap.sigmaT[k,i,t-1]) + #year to year
            qnorm(S.unif, 0, N5.cap.sigmaS[k,i,t-1]) + # site-to-site
              qnorm(withinS.unif, 0, N5.cap.sigma[k,i,t-1]))
)) # within-site
	 }}
}}

##plot(N5.cap[1,1,])
##cor(N5.cap[1,1,], N5.cap[2,1,])
#


# Removed 4/10/2017
#### C_ocean  #############
#
# C_ocean.targetR = C_ocean.target
## run to run variation (uncertainty in estimates)
#if ((R>1)* (MCsim2==1)) {
#R.unif = runif(1)
#	for (t in 1:Tr) {
#	 for (k in 1:K) {
#		    for (i in 1:5) { # only 5 ocean life stages now
#	  C_ocean[k,i,t] =   max(0.0, 
#  (C_ocean[k,i,t] +
#          qnorm(R.unif, 0, C_ocean.sigma[k,i,t]))) 
#	  C_ocean.targetR[k,i,t] =   max(0.0, 
#   (C_ocean.target[k,i,t] +
#          qnorm(R.unif, 0, C_ocean.sigma[k,i,t]))) 
## within-site
#	 }}}
#} # close run to run variability if R > 1
#
#
##  temporal trends, and if including temporal variation
#	for (g in 1:N.input.files) {
#if (N.input.files==1) {T.lo=1; T.hi=Tr+1} else {
#           if (g==N.input.files) {
#		  T.lo=T.step.change[g]
#	 	  T.hi= Tr+1 } else {
#              T.lo=T.step.change[g]
#		  T.hi=T.step.change[g+1]}}
#
#	for (t in (T.lo+1):T.hi) {
#	 T.unif = runif(1)
#		 for (k in 1:K) {
#		   S.unif = runif(1)
#             for (i in 1:5) { #only 5 ocean stages now
#	   withinS.unif=runif(1)
#     	C_ocean[k,i,t-1] =   max(0.0, 
#   (C_ocean[k,i,t-1] +
#  (C_ocean.targetR[k,i,t-1]-C_ocean[k,i,t-1])*
#        exp(-1/((t-T.lo)*(.0000000001+C_ocean.rate[k,i,t-1]))) +
#        ((MCsim2 ==1)*(MCsim4==1)) * (
#          qnorm(T.unif, 0 ,C_ocean.sigmaT[k,i,t-1]) + #year to year
#            qnorm(S.unif, 0, C_ocean.sigmaS[k,i,t-1]) + # site-to-site
#              qnorm(withinS.unif, 0, C_ocean.sigma[k,i,t-1]))
#)) # within-site
#	 }}
#}}

##plot(C_ocean[1,1,])
##cor(C_ocean[1,1,], C_ocean[2,1,])
#

#######################################




#######################################





#harvest.hatch.sigmaS = harvest.wild.sigmaS
#harvest.hatch.sigmaR = harvest.wild.sigmaR
#harvest.hatch.sigmaT = harvest.wild.sigmaT
#harvest.hatch.sigma = harvest.wild.sigma
#harvest.hatch.target = harvest.wild.target
#harvest.wild.rate
#harvest.hatch.rate=harvest.wild.rate

### Wild Harvest #############
harvest.wild = harvest.wild.mu
harvest.hatch = harvest.hatch.mu
harvest.wild.targetR = harvest.wild.target
harvest.hatch.targetR = harvest.hatch.target

#harvest.wild.mu
#harvest.wild
# run to run variation (uncertainty in estimates)
if ((R >1)* (MCsim3==1)) {
R.unif = runif(1)
	for (t in 1:Tr) {
		 for (k in 1:K) {
	
       	harvest.wild[k,t] =   max(0.0, 
   (harvest.wild[k,t] +
                   qnorm(R.unif, 0, harvest.wild.sigma[k,t])))
       	harvest.wild.targetR[k,t] =   max(0.0, 
   (harvest.wild.target[k,t] +
                   qnorm(R.unif, 0, harvest.wild.sigma[k,t])))

     	harvest.hatch[k,t] =   max(0.0, 
   (harvest.hatch[k,t] +
      qnorm(R.unif, 0, harvest.hatch.sigma[k,t])))
     	harvest.hatch.targetR[k,t] =   max(0.0, 
   (harvest.hatch.target[k,t] +
      qnorm(R.unif, 0, harvest.hatch.sigma[k,t])))
	 }}
} # close run to run variability


# if including temporal variation
	for (g in 1:N.input.files) {
if (N.input.files==1) {T.lo=1; T.hi=Tr+1} else {
           if (g==N.input.files) {
		  T.lo=T.step.change[g]
	 	  T.hi= Tr+1 } else {
              T.lo=T.step.change[g]
		  T.hi=T.step.change[g+1]}}

	for (t in (T.lo+1):T.hi) {

	 T.unif = runif(1)
		 for (k in 1:K) {
		   S.unif = runif(1)
  	         withinS.wild.unif=runif(1)
		  withinS.hatch.unif=runif(1)

     	harvest.wild[k,t-1] =   max(0.0, 
   (harvest.wild[k,t-1] +
 (harvest.wild.targetR[k,t-1]-harvest.wild[k,t-1])*
        exp(-1/((t-T.lo)*(.000000000000000001+harvest.wild.rate[k,t-1]))) +
        ((MCsim3==1)*(MCsim4==1))* (
          qnorm(T.unif, 0 ,harvest.wild.sigmaT[k,t-1]) + #year to year
            qnorm(S.unif, 0, harvest.wild.sigmaS[k,t-1]) + # site-to-site
              qnorm(withinS.wild.unif, 0, harvest.wild.sigma[k,t-1]))
        ))


     	harvest.hatch[k,t-1] =   max(0.0, 
   (harvest.hatch[k,t-1] +
 (harvest.hatch.targetR[k,t-1]-harvest.hatch[k,t-1])*
        exp(-1/((t-T.lo)*(.000000000000000001+harvest.hatch.rate[k,t-1]))) +
          ((MCsim3 == 1 )*(MCsim4==1)) * (
          qnorm(T.unif, 0 ,harvest.hatch.sigmaT[k,t-1]) + #year to year
            qnorm(S.unif, 0, harvest.hatch.sigmaS[k,t-1]) + # site-to-site
              qnorm(withinS.hatch.unif, 0, harvest.hatch.sigma[k,t-1]))))

	 }}
}

#plot(harvest.wild[1,])
#plot(harvest.hatch[1,])



######################################################
######################################################
#Add stochasticity to cross site migration natrix

# Initialize some stuff
	Fry.x.siteMigration.alphaR=array(rep(0,K*K*Tr), c(K,K,Tr))
	Fry.x.siteMigration.alphaT=array(rep(0,K*K*Tr), c(K,K,Tr))
	Fry.x.siteMigration.alphaS=array(rep(0,K*K*Tr), c(K,K,Tr))
 	Fry.x.siteMigration.alpha=array(rep(0,K*K*Tr), c(K,K,Tr))
  Fry.x.siteMigration = Fry.x.siteMigration.mu
  Fry.x.siteMigration.target.alphaR =array(rep(0,K*K*Tr), c(K,K,Tr))
	Fry.x.siteMigration.target.r = Fry.x.siteMigration.target
	Par.x.siteMigration.alphaR=array(rep(0,K*K*Tr), c(K,K,Tr))
	Par.x.siteMigration.alphaT=array(rep(0,K*K*Tr), c(K,K,Tr))
	Par.x.siteMigration.alphaS=array(rep(0,K*K*Tr), c(K,K,Tr))
 	Par.x.siteMigration.alpha=array(rep(0,K*K*Tr), c(K,K,Tr))
  Par.x.siteMigration = Par.x.siteMigration.mu
  Par.x.siteMigration.target.alphaR =array(rep(0,K*K*Tr), c(K,K,Tr))
	Par.x.siteMigration.target.r = Par.x.siteMigration.target
	OnePlus.x.siteMigration.alphaR=array(rep(0,K*K*Tr), c(K,K,Tr))
	OnePlus.x.siteMigration.alphaT=array(rep(0,K*K*Tr), c(K,K,Tr))
	OnePlus.x.siteMigration.alphaS=array(rep(0,K*K*Tr), c(K,K,Tr))
 	OnePlus.x.siteMigration.alpha=array(rep(0,K*K*Tr), c(K,K,Tr))
  OnePlus.x.siteMigration = OnePlus.x.siteMigration.mu
  OnePlus.x.siteMigration.target.alphaR =array(rep(0,K*K*Tr), c(K,K,Tr))
	OnePlus.x.siteMigration.target.r = OnePlus.x.siteMigration.target
	Spawner.x.siteMigration.alphaR=array(rep(0,K*K*Tr), c(K,K,Tr))
	Spawner.x.siteMigration.alphaT=array(rep(0,K*K*Tr), c(K,K,Tr))
	Spawner.x.siteMigration.alphaS=array(rep(0,K*K*Tr), c(K,K,Tr))
 	Spawner.x.siteMigration.alpha=array(rep(0,K*K*Tr), c(K,K,Tr))
  Spawner.x.siteMigration = Spawner.x.siteMigration.mu
  Spawner.x.siteMigration.target.alphaR =array(rep(0,K*K*Tr), c(K,K,Tr))
	Spawner.x.siteMigration.target.r = Spawner.x.siteMigration.target


# Run to Run Component (i.e. initialize for the run
# Initialize M for the run

#Fry.x.siteMigration.alphaR.N
#Fry.x.siteMigration.alphaS.N

#R=2
#MCsim1=3
# assume we want to include run-run variability if R > 1
if ((R > 1) * (MCsim4==1)) { 
	for (k1 in 1:K) {
        for (k2 in 1:K) {
        Fry.x.siteMigration.alphaR[k1,k2,] = Fry.x.siteMigration.mu[k1,k2,] * Fry.x.siteMigration.alphaR.N[k1,]
       Fry.x.siteMigration.target.alphaR[k1,k2,] = Fry.x.siteMigration.target[k1,k2,] * Fry.x.siteMigration.alphaR.N[k1,]
        Par.x.siteMigration.alphaR[k1,k2,] = Par.x.siteMigration.mu[k1,k2,] * Fry.x.siteMigration.alphaR.N[k1,]
       Par.x.siteMigration.target.alphaR[k1,k2,] = Par.x.siteMigration.target[k1,k2,] * Fry.x.siteMigration.alphaR.N[k1,]
        OnePlus.x.siteMigration.alphaR[k1,k2,] = OnePlus.x.siteMigration.mu[k1,k2,] * Fry.x.siteMigration.alphaR.N[k1,]
       OnePlus.x.siteMigration.target.alphaR[k1,k2,] = OnePlus.x.siteMigration.target[k1,k2,] * Fry.x.siteMigration.alphaR.N[k1,]
        Spawner.x.siteMigration.alphaR[k1,k2,] = Spawner.x.siteMigration.mu[k1,k2,] * Fry.x.siteMigration.alphaR.N[k1,]
       Spawner.x.siteMigration.target.alphaR[k1,k2,] = Spawner.x.siteMigration.target[k1,k2,] * Fry.x.siteMigration.alphaR.N[k1,]


}
}




	R.unif=runif(K)
#t=1
#k1=1

	for (t in 1:Tr) {
	for (k1 in 1:K) {

	# using a gamma than then taking %'s in this way will generate
	# a random dirichlet distribution.
		yfry.j = round(qgamma(R.unif,shape=Fry.x.siteMigration.alphaR[k1,,t]+.000001),5)
            yfrytarget.j =round(qgamma(R.unif,shape=Fry.x.siteMigration.target.alphaR[k1,,t]+.000001),5)
		ypar.j = round(qgamma(R.unif,shape=Par.x.siteMigration.alphaR[k1,,t]+.000001),5)
            ypartarget.j =round(qgamma(R.unif,shape=Par.x.siteMigration.target.alphaR[k1,,t]+.000001),5)
		yOnePlus.j = round(qgamma(R.unif,shape=OnePlus.x.siteMigration.alphaR[k1,,t]+.000001),5)
            yOnePlustarget.j =round(qgamma(R.unif,shape=OnePlus.x.siteMigration.target.alphaR[k1,,t]+.000001),5)
		yspawner.j = round(qgamma(R.unif,shape=Spawner.x.siteMigration.alphaR[k1,,t]+.000001),5)
            yspawnertarget.j =round(qgamma(R.unif,shape=Spawner.x.siteMigration.target.alphaR[k1,,t]+.000001),5)

	 	for (k2 in 1:K){
			Fry.x.siteMigration[k1,k2,t]= yfry.j[k2] / sum(yfry.j)
			Fry.x.siteMigration.target.r[k1,k2,t]= yfrytarget.j[k2] / sum(yfrytarget.j)
			Par.x.siteMigration[k1,k2,t]= ypar.j[k2] / sum(ypar.j)
			Par.x.siteMigration.target.r[k1,k2,t]= ypartarget.j[k2] / sum(ypartarget.j)
			OnePlus.x.siteMigration[k1,k2,t]= yOnePlus.j[k2] / sum(yOnePlus.j)
			OnePlus.x.siteMigration.target.r[k1,k2,t]= yOnePlustarget.j[k2] / sum(yOnePlustarget.j)
			Spawner.x.siteMigration[k1,k2,t]= yspawner.j[k2] / sum(yspawner.j)
			Spawner.x.siteMigration.target.r[k1,k2,t]= yspawnertarget.j[k2] / sum(yspawnertarget.j)

		}
          	}
		}
}

#plot(Fry.x.siteMigration[1,1,])
#plot(Fry.x.siteMigration.target.r[1,1,])
#plot(Par.x.siteMigration[1,1,])
#plot(OnePlus.x.siteMigration[1,1,])
#plot(Spawner.x.siteMigration[1,1,])


#### Update Starting Here ###########################
# Year-Year Component

####################################
# Temporal Drift (Always include this)

	for (g in 1:N.input.files) {
       if (N.input.files==1) 
           {T.lo=1
            T.hi=(Tr+1)} else if (g==N.input.files) {
		  T.lo=T.step.change[g]
	 	  T.hi= (Tr+1) } else {
              T.lo=T.step.change[g]
		  T.hi=T.step.change[g+1]}


#print(paste("g=",g, "T.hi=", T.hi))
# Add temporal driftn
      for (t in (T.lo+1):(T.hi)) {
       for (k in 1:K) {


	Diff = (Fry.x.siteMigration.target.r[k,,t-1]-Fry.x.siteMigration[k,,t-1])*exp(-1/((t-T.lo)*(.000000001+Fry.x.siteMigration.rate[,t-1]))) 
      Fry.x.siteMigration[k,,t-1] = Fry.x.siteMigration[k,,t-1]+ Diff
	Diff = (Par.x.siteMigration.target.r[k,,t-1]-Par.x.siteMigration[k,,t-1])*exp(-1/((t-T.lo)*(.0000000001+Par.x.siteMigration.rate[,t-1]))) 
      Par.x.siteMigration[k,,t-1] = Par.x.siteMigration[k,,t-1]+ Diff
	Diff = (OnePlus.x.siteMigration.target.r[k,,t-1]-OnePlus.x.siteMigration[k,,t-1])*exp(-1/((t-T.lo)*(.00000000001+OnePlus.x.siteMigration.rate[,t-1]))) 
      OnePlus.x.siteMigration[k,,t-1] = OnePlus.x.siteMigration[k,,t-1]+ Diff
	Diff = (Spawner.x.siteMigration.target.r[k,,t-1]-Spawner.x.siteMigration[k,,t-1])*exp(-1/((t-T.lo)*(.000000001+Spawner.x.siteMigration.rate[,t-1]))) 
      Spawner.x.siteMigration[k,,t-1] = Spawner.x.siteMigration[k,,t-1]+ Diff

### duplicate above lines for Par, OnePlus, and spawners 

}
}
}





##############################################################
# year to year, site to site, within site variability ##########
#plot(seq(1:Tr), M[1,1,1,])

     if ((MCsim1==1)*(MCsim4==1))  {

	for (k in 1:K) {
	Fry.x.siteMigration.alphaT[k,,]=Fry.x.siteMigration[k,,] * Fry.x.siteMigration.alphaT.N[,] 
	Par.x.siteMigration.alphaT[k,,]=Par.x.siteMigration[k,,] * Par.x.siteMigration.alphaT.N[,]
	OnePlus.x.siteMigration.alphaT[k,,]=OnePlus.x.siteMigration[k,,] * OnePlus.x.siteMigration.alphaT.N[,]
	Spawner.x.siteMigration.alphaT[k,,]=Spawner.x.siteMigration[k,,] * Spawner.x.siteMigration.alphaT.N[,]

### duplicate above lines for Par, OnePlus, and spawners
	}

#plot(Fry.x.siteMigration.alphaT[1,1,])
#plot(Fry.x.siteMigration.alphaT[1,2,])
#plot(Fry.x.siteMigration.alphaT[1,3,])
#plot(Fry.x.siteMigration.alphaT[1,4,])


	for (t in 2:Tr) {

	# Here's the shared random bit
		T.unif=runif(K)
	for (k1 in 1:K) {
	# using a gamma than then taking %'s in this way will generate
	# a random dirichlet distribution.
		yfry.j = round(qgamma(T.unif,shape=Fry.x.siteMigration.alphaT[k1,,t]+.000001),5)
		ypar.j = round(qgamma(T.unif,shape=Par.x.siteMigration.alphaT[k1,,t]+.000001),5)
		yOnePlus.j = round(qgamma(T.unif,shape=OnePlus.x.siteMigration.alphaT[k1,,t]+.000001),5)
		yspawner.j = round(qgamma(T.unif,shape=Spawner.x.siteMigration.alphaT[k1,,t]+.000001),5)


      for (k2 in 1:K){          
    	 	Fry.x.siteMigration[k1,k2,t]= yfry.j[k2] / sum(yfry.j)
		Par.x.siteMigration[k1,k2,t]= ypar.j[k2] / sum(ypar.j)
		OnePlus.x.siteMigration[k1,k2,t]= yOnePlus.j[k2] / sum(yOnePlus.j)
		Spawner.x.siteMigration[k1,k2,t]= yspawner.j[k2] / sum(yspawner.j)
          }	}  
}


#plot(Fry.x.siteMigration[1,1,])

# Now for the site-site component
	for (k in 1:K) {
	Fry.x.siteMigration.alphaS[k,,]=Fry.x.siteMigration[k,,] * Fry.x.siteMigration.alphaS.N[,] 
	Par.x.siteMigration.alphaS[k,,]=Par.x.siteMigration[k,,] * Par.x.siteMigration.alphaS.N[,]
	OnePlus.x.siteMigration.alphaS[k,,]=OnePlus.x.siteMigration[k,,] * OnePlus.x.siteMigration.alphaS.N[,]
	Spawner.x.siteMigration.alphaS[k,,]=Spawner.x.siteMigration[k,,] * Spawner.x.siteMigration.alphaS.N[,]


### duplicate above lines for Par, OnePlus, and spawners
	}

	for (t in 2:Tr) {
	for (k1 in 1:K) {
	# Here's the shared random bit
		S.unif=runif(K)

	# using a gamma than then taking %'s in this way will generate
	# a random dirichlet distribution.
		yfry.j = round(qgamma(S.unif,shape=Fry.x.siteMigration.alphaS[k1,,t]+.000001),5)
		ypar.j = round(qgamma(S.unif,shape=Par.x.siteMigration.alphaS[k1,,t]+.000001),5)
		yOnePlus.j = round(qgamma(S.unif,shape=OnePlus.x.siteMigration.alphaS[k1,,t]+.000001),5)
		yspawner.j = round(qgamma(S.unif,shape=Spawner.x.siteMigration.alphaS[k1,,t]+.000001),5)

 	for (k2 in 1:K){          
   	 	Fry.x.siteMigration[k1,k2,t]= yfry.j[k2] / sum(yfry.j)
		Par.x.siteMigration[k1,k2,t]= ypar.j[k2] / sum(ypar.j)
		OnePlus.x.siteMigration[k1,k2,t]= yOnePlus.j[k2] / sum(yOnePlus.j)
		Spawner.x.siteMigration[k1,k2,t]= yspawner.j[k2] / sum(yspawner.j)

### duplicate above lines for Par, OnePlus, and spawners
         }	}  }




# Now for the within site component
### Below I'll need separate runifs for each type of migration
	for (k in 1:K) {
	Fry.x.siteMigration.alpha[k,,]=Fry.x.siteMigration[k,,] * Fry.x.siteMigration.alpha.N[,] 
	Par.x.siteMigration.alpha[k,,]=Par.x.siteMigration[k,,] * Par.x.siteMigration.alpha.N[,]
	OnePlus.x.siteMigration.alpha[k,,]=OnePlus.x.siteMigration[k,,] * OnePlus.x.siteMigration.alpha.N[,]
	Spawner.x.siteMigration.alpha[k,,]=Spawner.x.siteMigration[k,,] * Spawner.x.siteMigration.alpha.N[,]

	}


#plot(Fry.x.siteMigration.alpha[1,1,])
	for (t in 2:Tr) {
	for (k1 in 1:K) {

#############	
# Duplicate Below for each life stage w/ cross site migration
		withinSfry.unif=runif(K)
		withinSpar.unif=runif(K)
		withinSOnePlus.unif=runif(K)
		withinSspawner.unif=runif(K)


	# using a gamma than then taking %'s in this way will generate
	# a random dirichlet distribution.
		yfry.j = round(qgamma(withinSfry.unif,shape=Fry.x.siteMigration.alpha[k1,,t]+.000001),5)
		ypar.j = round(qgamma(withinSpar.unif,shape=Par.x.siteMigration.alpha[k1,,t]+.000001),5)
		yOnePlus.j = round(qgamma(withinSOnePlus.unif,shape=OnePlus.x.siteMigration.alpha[k1,,t]+.000001),5)
		yspawner.j = round(qgamma(withinSspawner.unif,shape=Spawner.x.siteMigration.alpha[k1,,t]+.000001),5)



 	for (k2 in 1:K){          
   	 	Fry.x.siteMigration[k1,k2,t]= yfry.j[k2] / sum(yfry.j)
		Par.x.siteMigration[k1,k2,t]= ypar.j[k2] / sum(ypar.j)
		OnePlus.x.siteMigration[k1,k2,t]= yOnePlus.j[k2] / sum(yOnePlus.j)
		Spawner.x.siteMigration[k1,k2,t]= yspawner.j[k2] / sum(yspawner.j)



         }	}  	
### duplicate above lines for Par, OnePlus, and spawners (see note above)
	
} # close time loop

} # end if "If (MCsim4==1) statemente



#plot(Fry.x.siteMigration[1,1,])
#plot(Par.x.siteMigration[1,1,])
#plot(OnePlus.x.siteMigration[1,1,])
#plot(Spawner.x.siteMigration[1,1,])

################################################################




















######################################################
#Ak_x_Lqk
#M
#SR5
#N5.Psmolt

data = list(
	"Ak_x_Lqk"=Ak_x_Lqk,
 	"M"=M,
	"Sr"=Sr,
	"SR5" =SR5,
#	"FC.by.O.Age"=C_ocean,
	
	"D"=D,
 	"Prod_Scalar"=Prod_Scalar,

	"Frac.Subyear.M" = Frac.Subyear.M,
	"Frac.Subyear.F" = Frac.Subyear.F,

	"N5.Psmolt_F"=N5.Psmolt_F,
      "N5.Pstay_F" = N5.Pstay_F,
      "N5.Pspawn_F" = N5.Pspawn_F,

	"N5.Psmolt_M"=N5.Psmolt_M,
      "N5.Pstay_M" = N5.Pstay_M,
      "N5.Pspawn_M" = N5.Pspawn_M,

	"N5.cap"= N5.cap,
# Removed 4/10/2017
#	"C_ocean"=C_ocean,
	"Mat8Plus_F"=Mat8Plus_F,
	"Mat8Plus_M"=Mat8Plus_M,

#	"C_ocean"= C_ocean,
      "frac" = frac,
      "harvest.wild" = harvest.wild,
      "harvest.hatch" = harvest.hatch,

"Hatch_Fish" = Hatch_Fish,
"Rel_Surv" = Rel_Surv,
#"Rel_Comp" = Rel_Comp,
#"Rel_Fecund" = Rel_Fecund,

#"Female_Frac" = Female_Frac,
"Female_Fecundity" = Female_Fecundity,

"Fry.x.siteMigration"=Fry.x.siteMigration,
"Par.x.siteMigration"=Par.x.siteMigration,
"OnePlus.x.siteMigration"=OnePlus.x.siteMigration,
"Spawner.x.siteMigration"=Spawner.x.siteMigration,
"N5.Rainbow.Fecundity"=N5.Rainbow.Fecundity,

"Post_Spawn_Survival_Anadromous_M" = Post_Spawn_Survival_Anadromous_M,
"Post_Spawn_Survival_Anadromous_F" = Post_Spawn_Survival_Anadromous_F,

"Post_Spawn_Survival_Rainbow_M" = Post_Spawn_Survival_Rainbow_M,
"Post_Spawn_Survival_Rainbow_F" = Post_Spawn_Survival_Rainbow_F

)

detach(Input.Data)
detach(Var.data)
detach(header.data)
return(data)

}



