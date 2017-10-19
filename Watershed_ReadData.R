

##################################################################
Read.Header <- function(Header.File) {
##################################################################

I=12 #(number of life stages) ### Need to Read This in DATA FILE
I5=5 # Number of potential OnePlus years (Steelhead)


Header.File = "Watershed_Header_File.csv"

# Read in Watershed Information Table 2.3

species = as.character(read.csv(Header.File, nrows=1, header=F)[,3])
# correct for capitalization differences
if (species == "Chinook") {species = "chinook"}
if (species == "Steelhead")  {species = "steelhead"}

WshedKJQ = read.csv(Header.File, skip=1, nrows=10, header=F)[,3]

	#K, Q, and J: Number of Sites, Land Use Cats per site, Habitat Types per Site
	K=as.numeric(WshedKJQ[1])
	N.input.files=as.numeric(WshedKJQ[2])
	Q=as.numeric(WshedKJQ[3])
	J=as.numeric(WshedKJQ[4])

	G=2

	Tr=as.numeric(WshedKJQ[5]) 
	R=as.numeric(WshedKJQ[6])
	MCsim1=as.numeric(WshedKJQ[7])
	MCsim2=as.numeric(WshedKJQ[8])
	MCsim3=as.numeric(WshedKJQ[9])
	MCsim4=as.numeric(WshedKJQ[10])

rm(WshedKJQ)
N.input.files
Tr

T.step.change = as.numeric(read.csv(Header.File,
          skip=13, nrows=1, header=F)[,4:(4+N.input.files-1)])
T.step.change

# read in input file names (one input file for each step change in inputs)

file.names = 
as.matrix((
read.csv(Header.File, 
  skip = 15, nrows = K, 
  header=F, 
  colClasses = rep("character", (1+N.input.files)))[1:K, 3:(N.input.files+3)]
))

Input.file.names = array(file.names[,2:(1+N.input.files)], c(K, N.input.files))
Init.file.names = file.names[,1]


Cross.site.migration.file.names = 
c(
read.csv(Header.File, 
  skip = 26, nrows = 1, 
  header=F, 
  colClasses = rep("character", (1+N.input.files)))[1, 4:(N.input.files+3)]
)
Cross.site.migration.file.names


Site.Names = read.csv(Header.File, skip=15, nrows=K, header=F,
  colClasses="character")[,2]
Site.Names

# Will have to use Input.file.names in read data function to change to reading
# different input files for each site, rather than different worksheets
# for each seet, due to the switch to .csv files


# Won't even read site names in header file, as each input sheet
# is it's own site, so the name will be read from the input sheet
# or "site profile" directly.

return(
	list(
      "species" = species,
      "I"=I, "I5"=I5,"G"=G,
	"K"=K, "N.input.files"=N.input.files, "Q"=Q, "J"=J, "Tr"=Tr, "R"=R, 
	"MCsim1"=MCsim1, "MCsim2"=MCsim2,"MCsim3"=MCsim3,"MCsim4"=MCsim4,
	"T.step.change" = T.step.change,
	"Input.file.names" = Input.file.names,
      "Init.file.names" = Init.file.names,
      "Cross.site.migration.file.names" = Cross.site.migration.file.names,
      "Site.Names"=Site.Names
	)
)

}# end of function




## Finished to here - updated Reading Header File from .csv.  
## Need to continue, reading input file(s) from .csv as well.
## Will now have multiple input files names - will have to update
## input file name for each site.

##################################################################
##################################################################
Read.Input.File <- function(header) {

attach(header)

######################

# Initialize Vectors
M.mu = array(rep(0,(K*J*Q*Tr)),c(K,Q,J,Tr))
M.target = M.mu

M.alphaR.N =  array(rep(0,(K*Q*Tr)),c(K,Q,Tr))
M.alphaT.N = M.alphaR.N
M.alphaS.N = M.alphaR.N
M.alpha.N = M.alphaR.N
M.rate = M.alphaR.N


Ak_x_Lqk.mu=array(rep(0,K*Q*Tr),c(K,Q,Tr))
Ak_x_Lqk.sigmaR=Ak_x_Lqk.mu
Ak_x_Lqk.sigmaT=Ak_x_Lqk.mu
Ak_x_Lqk.sigmaS=Ak_x_Lqk.mu
Ak_x_Lqk.sigma=Ak_x_Lqk.mu
Ak_x_Lqk.target=Ak_x_Lqk.mu
Ak_x_Lqk.rate=Ak_x_Lqk.mu



# The six are for:  (1)egg, (2)Fry, (3)Parr, (4)Pre-Smolt, 
# (5)subyearling chinook or NA for steelhead, (6) yearling chinook or Steelhead
D.mu=array(rep(0, K* 6*(J+1)*Tr), c(K, J, 6, Tr))
D.sigmaR=D.mu
D.sigmaT=D.mu
D.sigmaS=D.mu
D.sigma=D.mu
D.target=D.mu
D.rate = D.mu


# The six are for:  (1)egg, (2)Fry, (3)Parr, (4)Pre-Smolt, 
# (5)subyearling chinook or NA for steelhead, (6) yearling chinook or Steelhead
Prod_Scalar.mu=array(rep(0, K*6*Q*Tr), c(K, Q, 6, Tr))
Prod_Scalar.sigmaR=Prod_Scalar.mu 
Prod_Scalar.sigmaT=Prod_Scalar.mu
Prod_Scalar.sigmaS=Prod_Scalar.mu
Prod_Scalar.sigma=Prod_Scalar.mu
Prod_Scalar.target = Prod_Scalar.mu
Prod_Scalar.rate = Prod_Scalar.mu


Sr.mu = array(rep(0, K*(I+3)*Tr), c(K,(I+3),Tr))
Sr.alphaR.N= Sr.mu
Sr.alphaT.N= Sr.mu
Sr.alphaS.N= Sr.mu
Sr.alpha.N = Sr.mu
Sr.target = Sr.mu
Sr.rate = Sr.mu

#initialize fraction subyearlings
Frac.Subyear.M.mu = array(rep(0,K*Tr), c(K, Tr))
Frac.Subyear.F.mu = array(rep(0,K*Tr), c(K, Tr))
Frac.Subyear.M.alphaR.N =  Frac.Subyear.M.mu
Frac.Subyear.M.alphaT.N =  Frac.Subyear.M.mu
Frac.Subyear.M.alphaS.N =  Frac.Subyear.M.mu
Frac.Subyear.M.alpha.N =  Frac.Subyear.M.mu
Frac.Subyear.F.alphaR.N =  Frac.Subyear.F.mu
Frac.Subyear.F.alphaT.N =  Frac.Subyear.F.mu
Frac.Subyear.F.alphaS.N =  Frac.Subyear.F.mu
Frac.Subyear.F.alpha.N =  Frac.Subyear.F.mu


# OnePlus Stuff...
SR5.mu = array(rep(0, K*I5*Tr), c(K, I5, Tr))
SR5.alphaR=SR5.mu
SR5.alphaT=SR5.mu
SR5.alphaS=SR5.mu
SR5.alpha=SR5.mu
SR5.target = SR5.mu
SR5.rate = SR5.mu


N5.Psmolt_Female.mu = SR5.mu
N5.Pspawn_Female.mu = SR5.mu
N5.Pstay_Female.mu = SR5.mu
N5.P.alphaR_Female.N = SR5.mu
N5.P.alphaT_Female.N = SR5.mu
N5.P.alphaS_Female.N = SR5.mu
N5.P.alpha_Female.N = SR5.mu
N5.Psmolt_Female.target = SR5.mu
N5.Pspawn_Female.target = SR5.mu
N5.Pstay_Female.target = SR5.mu
N5.P_Female.rate = SR5.mu

N5.Psmolt_Male.mu = SR5.mu
N5.Pspawn_Male.mu = SR5.mu
N5.Pstay_Male.mu = SR5.mu
N5.P.alphaR_Male.N = SR5.mu
N5.P.alphaT_Male.N = SR5.mu
N5.P.alphaS_Male.N = SR5.mu
N5.P.alpha_Male.N = SR5.mu
N5.Psmolt_Male.target = SR5.mu
N5.Pspawn_Male.target = SR5.mu
N5.Pstay_Male.target = SR5.mu
N5.P_Male.rate = SR5.mu

N5.Rainbow.Fecundity = array(rep(0, K*I5*Tr), c(K, I5, Tr))

N5.cap.mu = SR5.mu
N5.cap.sigmaR = SR5.mu
N5.cap.sigmaT = SR5.mu
N5.cap.sigmaS = SR5.mu
N5.cap.sigma = SR5.mu
N5.cap.target = SR5.mu
N5.cap.rate = SR5.mu


# Adult (ocean) fish by ocean age parameters (track up to 5 ocean years)
	Mat8Plus_Female.mu = array(rep(0,K*6*Tr), c(K, 6, Tr))
	Mat8Plus_Female.alphaR.N = Mat8Plus_Female.mu
	Mat8Plus_Female.alphaT.N = Mat8Plus_Female.mu
	Mat8Plus_Female.alphaS.N = Mat8Plus_Female.mu
	Mat8Plus_Female.alpha.N = Mat8Plus_Female.mu
	Mat8Plus_Female.target = Mat8Plus_Female.mu
	Mat8Plus_Female.rate = Mat8Plus_Female.mu

	Mat8Plus_Male.mu = array(rep(0,K*6*Tr), c(K, 6, Tr))
	Mat8Plus_Male.alphaR.N = Mat8Plus_Female.mu
	Mat8Plus_Male.alphaT.N = Mat8Plus_Female.mu
	Mat8Plus_Male.alphaS.N = Mat8Plus_Female.mu
	Mat8Plus_Male.alpha.N = Mat8Plus_Female.mu
	Mat8Plus_Male.target = Mat8Plus_Female.mu
	Mat8Plus_Male.rate = Mat8Plus_Female.mu

# frac
	frac.mu = array(rep(0, K*6*(J)*Tr), c(K, 6, (J), Tr))
	frac.sigmaR = frac.mu
	frac.sigmaT = frac.mu
	frac.sigmaS = frac.mu
	frac.sigma = frac.mu
	frac.target = frac.mu
	frac.rate = frac.mu

harvest.wild.minspawn.mu = array(rep(0,K*Tr), c(K, Tr))
harvest.wild.minharvest.mu = harvest.wild.minspawn.mu
harvest.wild.maxharvest.mu = harvest.wild.minspawn.mu
harvest.wild.ratepharvest.mu = harvest.wild.minspawn.mu

harvest.hatch.minspawn.mu = harvest.wild.minspawn.mu
harvest.hatch.minharvest.mu = harvest.wild.minspawn.mu
harvest.hatch.maxharvest.mu = harvest.wild.minspawn.mu
harvest.hatch.ratepharvest.mu = harvest.wild.minspawn.mu

Hatch_Fish.mu = array(rep(0, K*I*Tr), c(K, I, Tr))
Hatch_Fish.sigmaR = Hatch_Fish.mu
Hatch_Fish.sigmaT = Hatch_Fish.mu
Hatch_Fish.sigmaS = Hatch_Fish.mu
Hatch_Fish.sigma = Hatch_Fish.mu
Hatch_Fish.target = Hatch_Fish.mu
Hatch_Fish.rate = Hatch_Fish.mu



# Rel_Surv (G categories)
Rel_Surv.mu = array(rep(0, K*I*Tr*G), c(K, I, Tr, G))
Rel_Surv.sigmaR = Rel_Surv.mu
Rel_Surv.sigmaT = Rel_Surv.mu
Rel_Surv.sigmaS = Rel_Surv.mu
Rel_Surv.sigma = Rel_Surv.mu
Rel_Surv.target= Rel_Surv.mu
Rel_Surv.rate= Rel_Surv.mu

#Male Female Ratio
# 5 is for 5 year ocean life
Post_Spawn_Survival_Anadromous_M.mu =  array(rep(0, K*5*Tr*G), c(K,5,Tr,G))
Post_Spawn_Survival_Anadromous_F.mu =  array(rep(0, K*5*Tr*G), c(K,5,Tr,G))

Post_Spawn_Survival_Rainbow_M.mu =  array(rep(0, K*5*Tr*G), c(K,5,Tr,G))
Post_Spawn_Survival_Rainbow_F.mu =  array(rep(0, K*5*Tr*G), c(K,5,Tr,G))

#Female_Frac.mu = array(rep(0, K*5*Tr*G), c(K,5,Tr,G))


#Fecundity of Female Spawners by Ocean Age)
Female_Fecundity = array(rep(0, K*6*Tr*G), c(K,6,Tr,G))


# Rel_Com (G categories)
Rel_Comp.mu = array(rep(0, K*I*Tr*G), c(K, I, Tr, G))
Rel_Comp.sigmaR = Rel_Comp.mu
Rel_Comp.sigmaT = Rel_Comp.mu
Rel_Comp.sigmaS = Rel_Comp.mu
Rel_Comp.sigma = Rel_Comp.mu
Rel_Comp.target= Rel_Comp.mu
Rel_Comp.rate= Rel_Comp.mu

# Rel_Fecund
Rel_Fecund.mu = array(rep(0,K*Tr*G), c(K, Tr, G))
Rel_Fecund.simgaR = Rel_Fecund.mu
Rel_Fecund.simgaT = Rel_Fecund.mu
Rel_Fecund.simgaS = Rel_Fecund.mu
Rel_Fecund.simga = Rel_Fecund.mu
Rel_Fecund.target = Rel_Fecund.mu
Rel_Fecund.rate = Rel_Fecund.mu


Fry.x.siteMigration.mu = array(rep(0, K*K*Tr), c(K,K,Tr))
Par.x.siteMigration.mu = array(rep(0, K*K*Tr), c(K,K,Tr))
OnePlus.x.siteMigration.mu = array(rep(0, K*K*Tr), c(K,K,Tr))
Spawner.x.siteMigration.mu = array(rep(0, K*K*Tr), c(K,K,Tr))

Fry.x.siteMigration.target = array(rep(0, K*K*Tr), c(K,K,Tr))
Par.x.siteMigration.target = array(rep(0, K*K*Tr), c(K,K,Tr))
OnePlus.x.siteMigration.target = array(rep(0, K*K*Tr), c(K,K,Tr))
Spawner.x.siteMigration.target = array(rep(0, K*K*Tr), c(K,K,Tr))

Fry.x.siteMigration.alphaR = array(rep(0, K*Tr), c(K, Tr))
Fry.x.siteMigration.alphaT = array(rep(0,K*Tr), c(K, Tr))
Fry.x.siteMigration.alphaS = array(rep(0,K*Tr), c(K, Tr))
Fry.x.siteMigration.alpha = array(rep(0,K*Tr), c(K, Tr))

Par.x.siteMigration.alphaR = array(rep(0,K*Tr), c(K, Tr))
Par.x.siteMigration.alphaT = array(rep(0,K*Tr), c(K, Tr))
Par.x.siteMigration.alphaS = array(rep(0,K*Tr), c(K, Tr))
Par.x.siteMigration.alpha = array(rep(0,K*Tr), c(K, Tr))

OnePlus.x.siteMigration.alphaR = array(rep(0,K*Tr), c(K, Tr))
OnePlus.x.siteMigration.alphaT = array(rep(0,K*Tr), c(K, Tr))
OnePlus.x.siteMigration.alphaS = array(rep(0,K*Tr), c(K, Tr))
OnePlus.x.siteMigration.alpha = array(rep(0,K*Tr), c(K, Tr))

Spawner.x.siteMigration.alphaR = array(rep(0,K*Tr), c(K, Tr))
Spawner.x.siteMigration.alphaT = array(rep(0,K*Tr), c(K, Tr))
Spawner.x.siteMigration.alphaS = array(rep(0,K*Tr), c(K, Tr))
Spawner.x.siteMigration.alpha = array(rep(0,K*Tr), c(K, Tr))

Fry.x.siteMigration.rate = array(rep(0, K*Tr), c(K, Tr))
Par.x.siteMigration.rate = array(rep(0, K*Tr), c(K, Tr))
OnePlus.x.siteMigration.rate = array(rep(0, K*Tr), c(K, Tr))
Spawner.x.siteMigration.rate = array(rep(0, K*Tr), c(K, Tr))

################################
################################
# loop through each site within input file
k=1
n.step=1


for (n.step in 1:N.input.files) {
	T.lo= as.numeric(T.step.change[n.step])
	if (n.step==N.input.files) {
		T.hi=Tr
	} else {
		T.hi= as.numeric(T.step.change[n.step+1])-1
	}
				
# loop through each input file
#Watershed.Input.File=Input.file.names[n.step] 

	for (k in 1:K) {
    		Watershed.Input.File = as.character(Input.file.names[k, n.step])

		T2.3 <- as.matrix(read.csv(Watershed.Input.File, header=F,skip=27, nrows=Q)[,3:(3+J-1)])

		T2.3Nalpha <- as.matrix(read.csv(Watershed.Input.File, header=F,
         		skip=27, nrows=Q)[,15:18])

		T2.3target <- as.matrix(read.csv(Watershed.Input.File, header=F,
        		 skip=27, nrows=Q)[,19:(31)])

		T2.3rate <- read.csv(Watershed.Input.File, header=F,
         		skip=27, nrows=Q)[,31]


#as.numeric(T.lo):as.numeric(T.hi)
for (t in as.numeric(T.lo):as.numeric(T.hi)) {
		M.alphaR.N[k,,t]=T2.3Nalpha[,1]
		M.alphaT.N[k,,t]=T2.3Nalpha[,2]
		M.alphaS.N[k,,t]=T2.3Nalpha[,3]
            M.alpha.N[k,,t]= T2.3Nalpha[,4]
	      M.rate[k,,t]=T2.3target[,13]

for (j in 1:J) {
		M.mu[k,,j,t]=as.numeric(T2.3[,j])
		M.target[k,,j,t] = as.numeric(T2.3target[,j])
	
	} #close j
}# close t

dim(M.mu)
M.mu[1,1,1,1:10]
M.target[1,1,1,1:10]
M.alphaR.N[k,,t]


# Read Ak_x_Lqk_vectors



Ak <- read.csv(Watershed.Input.File, header=F,
         skip=9, nrows=Q)[,3:9]

for (t in T.lo:T.hi) {
	Ak_x_Lqk.mu[k, ,t] <-Ak[,1]
	Ak_x_Lqk.sigmaR[k, ,t] <-Ak[,2]
	Ak_x_Lqk.sigmaT[k, ,t] <-Ak[,3]
	Ak_x_Lqk.sigmaS[k, ,t] <-Ak[,4]
	Ak_x_Lqk.sigma[k, ,t] <-Ak[,5]
	Ak_x_Lqk.target[k, ,t] <-Ak[,6]
	Ak_x_Lqk.rate[k, ,t] <- Ak[,7]
} # end t

#########################################
# Read in Table 2_4 (to get to D matrix)

Dtable = read.csv(Watershed.Input.File, header=F,
         skip=44, nrows=12)[,3:44]

# Note - this has been updated so that capcity of spawning gravel is input directly in "spawner to egg" category

for (t in T.lo:T.hi) {
	for (i in 1:6) { 
		# 6 is for 6 life stages (including one each for subyearlings or yearling chinook

		D.mu[k,1:J,i,t] = Dtable[1:J,i] 
		D.sigmaR[k,1:J,i,t] = Dtable[1:J,(i+6)] 
		D.sigmaT[k,1:J,i,t] = Dtable[1:J,(i+12)] 
		D.sigmaS[k,1:J,i,t] = Dtable[1:J,(i+18)] 
		D.sigma[k,1:J,i,t] = Dtable[1:J,(i+24)] 
		D.target[k,1:J,i,t] = Dtable[1:J,(i+30)]
		D.rate[k,1:J,i,t] = Dtable[1:J, (i+36)]
	}
}

####### Productivity Scalars

Etable = read.csv(Watershed.Input.File, header=F,
        skip=62, nrows=Q)[,3:44]

for (t in T.lo:T.hi) {
	for (i in 1:6) {
		Prod_Scalar.mu[k,1:Q,i,t] = Etable[1:Q,i] 
		Prod_Scalar.sigmaR[k,1:Q,i,t] = Etable[1:Q,(i+6)] 
		Prod_Scalar.sigmaT[k,1:Q,i,t] = Etable[1:Q,(i+12)] 
		Prod_Scalar.sigmaS[k,1:Q,i,t] = Etable[1:Q,(i+18)] 
		Prod_Scalar.sigma[k,1:Q,i,t] = Etable[1:Q,(i+24)] 
		Prod_Scalar.target[k,1:Q,i,t] = Etable[1:Q,(i+30)]
		Prod_Scalar.rate[k,1:Q,i,t] = Etable[1:Q,(i+36)]
	} #close i
} # close t
rm(Etable)

SrTable = read.csv(Watershed.Input.File, header=F,
        skip= 79, nrows = I+2)[,4:10]

for (t in T.lo:T.hi) {
	Sr.mu[k,2:(I+3) ,t] = (SrTable[,1])
	Sr.alphaR.N[k,2:(I+3) ,t]= SrTable[,2]
	Sr.alphaT.N[k,2:(I+3) ,t]= SrTable[,3]
	Sr.alphaS.N[k,2:(I+3) ,t]= SrTable[,4]
	Sr.alpha.N[k,2:(I+3) ,t]= SrTable[,5]
	Sr.target[k, 2:(I+3), t] = SrTable[,6]
	Sr.rate[k, 2:(I+3), t] = SrTable[,7]
}
rm(SrTable)

# Fraction Subyearling Inputs
# HERE!!
SRLTable = read.csv(Watershed.Input.File, header=F,
        skip= 98, nrows = 1)[4:13]
	
for (t in T.lo:T.hi) {
	Frac.Subyear.M.mu[k, t] =SRLTable[1,1] 
	Frac.Subyear.F.mu[k, t] = SRLTable[1,2] 
	Frac.Subyear.M.alphaR.N[k, t] = SRLTable[1,3] 
	Frac.Subyear.M.alphaT.N[k, t] = SRLTable[1,4]  
	Frac.Subyear.M.alphaS.N[k, t] = SRLTable[1,5]  
	Frac.Subyear.M.alpha.N[k, t] = SRLTable[1,6]  
	Frac.Subyear.F.alphaR.N[k, t] = SRLTable[1,7] 
	Frac.Subyear.F.alphaT.N[k, t] = SRLTable[1,8] 
	Frac.Subyear.F.alphaS.N[k, t] = SRLTable[1,9] 
	Frac.Subyear.F.alpha.N[k, t] = SRLTable[1,10] 
} # end for t....


### OnePlus Inputs
PSinputs = read.csv(Watershed.Input.File, header=F,
      skip=103, nrows = I5)[, 3:39]

for (t in T.lo:T.hi) {
	SR5.mu[k, ,t] = PSinputs[,1] 
	SR5.alphaR[k, ,t] = PSinputs[,2]   
	SR5.alphaT[k, ,t] = PSinputs[,3]
	SR5.alphaS[k, ,t]= PSinputs[,4]
	SR5.alpha[k, ,t]= PSinputs[,5]
	SR5.target[k, ,t] = PSinputs[,25]
	SR5.rate[k, ,t] = PSinputs[,26]

	N5.Psmolt_Female.mu[k, ,t]= PSinputs[,6]
	N5.Pspawn_Female.mu[k, ,t] = PSinputs[,7]
	N5.Pstay_Female.mu[k, ,t] = PSinputs[,8]

	N5.P.alphaR_Female.N[k, ,t]= PSinputs[,9] 
	N5.P.alphaT_Female.N[k, ,t]= PSinputs[,10]
	N5.P.alphaS_Female.N[k, ,t]= PSinputs[,11]
	N5.P.alpha_Female.N[k, ,t] = PSinputs[,12]

	N5.Psmolt_Male.mu[k, ,t]= PSinputs[,13]
	N5.Pspawn_Male.mu[k, ,t] = PSinputs[,14]
	N5.Pstay_Male.mu[k, ,t] = PSinputs[,15]

	N5.P.alphaR_Male.N[k, ,t]= PSinputs[,16] 
	N5.P.alphaT_Male.N[k, ,t]= PSinputs[,17]
	N5.P.alphaS_Male.N[k, ,t]= PSinputs[,18]
	N5.P.alpha_Male.N[k, ,t] = PSinputs[,19]

	N5.Psmolt_Female.target[k, ,t]=PSinputs[,27]
	N5.Pspawn_Female.target[k, ,t]=PSinputs[,28]
	N5.Pstay_Female.target[k, ,t]=PSinputs[,29]
	N5.P_Female.rate[k, ,t] = PSinputs[,33]

	N5.Psmolt_Male.target[k, ,t]=PSinputs[,30]
	N5.Pspawn_Male.target[k, ,t]=PSinputs[,31]
	N5.Pstay_Male.target[k, ,t]=PSinputs[,32]
	N5.P_Male.rate[k, ,t] = PSinputs[,34]
	PSinputs
	N5.P_Male.rate

	N5.cap.mu[k, ,t] = PSinputs[,20]
	N5.cap.sigmaR[k, ,t]= PSinputs[,21]
	N5.cap.sigmaT[k, ,t]= PSinputs[,22]
	N5.cap.sigmaS[k, ,t]= PSinputs[,23]
	N5.cap.sigma[k, ,t] = PSinputs[,24]

	N5.cap.target[k, ,t] = PSinputs[,35]
	N5.cap.rate[k, ,t] = PSinputs[,36]

	N5.Rainbow.Fecundity[k, ,t] = PSinputs[,37]
}
rm(PSinputs)

o.inputs = read.csv(Watershed.Input.File, header=F,
         skip=113, nrow=6)[, 4:17]

for (t in T.lo:T.hi) {
	Mat8Plus_Female.mu[k, ,t] = o.inputs[,1]
	Mat8Plus_Female.alphaR.N[k, ,t] = o.inputs[,2]
	Mat8Plus_Female.alphaT.N[k, ,t] = o.inputs[,3]
	Mat8Plus_Female.alphaS.N[k, ,t] = o.inputs[,4]
	Mat8Plus_Female.alpha.N[k, ,t] = o.inputs[,5]
	Mat8Plus_Female.target[k, ,t] = o.inputs[,11]
	Mat8Plus_Female.rate[k, ,t] = o.inputs[,12]

	Mat8Plus_Male.mu[k, ,t] = o.inputs[,6]
	Mat8Plus_Male.alphaR.N[k, ,t] = o.inputs[,7]
	Mat8Plus_Male.alphaT.N[k, ,t] = o.inputs[,8]
	Mat8Plus_Male.alphaS.N[k, ,t] = o.inputs[,9]
	Mat8Plus_Male.alpha.N[k, ,t] = o.inputs[,10]
	Mat8Plus_Male.target[k, ,t] = o.inputs[,13]
	Mat8Plus_Male.rate[k, ,t] = o.inputs[,14]
}


rm(o.inputs)
### read "frac"

fractions = read.csv(Watershed.Input.File, header=F,
          skip=123, nrows = 6)[,4:87]

fractions
#dim(frac.mu)

for (t in T.lo:T.hi) {
	for (j in 1:J) {
		frac.mu[k, ,j,t] = fractions[,j]
		frac.sigmaR[k, ,j,t] = fractions[,j+12]
		frac.sigmaT[k, ,j,t] = fractions[,j+24]
		frac.sigmaS[k, ,j,t] = fractions[,j+36]
		frac.sigma[k, ,j,t] = fractions[,j+48]
		frac.target[k, ,j,t] = fractions[,j+60]
		frac.rate[k, , j,t] = fractions[,j+72]
	} # close j
} #close t
rm(fractions)

###################

harvest = read.csv(Watershed.Input.File, header=F, 
         skip = 132, nrows = 2)[, 3:6]

for (t in T.lo:T.hi) {
	harvest.wild.minspawn.mu[k,t] = harvest[1,1]
	harvest.wild.minharvest.mu[k,t] = harvest[1,2]
	harvest.wild.maxharvest.mu[k,t] = harvest[1,3]
	harvest.wild.ratepharvest.mu[k,t] = harvest[1,4]

	harvest.hatch.minspawn.mu[k,t] = harvest[2,1]
	harvest.hatch.minharvest.mu[k,t] = harvest[2,2]
	harvest.hatch.maxharvest.mu[k,t] = harvest[2,3]
	harvest.hatch.ratepharvest.mu[k,t] = harvest[2,4]
} # close t
rm(harvest)

################################################

	
Hatch_Fish_Inputs = read.csv(Watershed.Input.File, header=F,
   skip=138, nrows=1)[, 2:5]

for (t in T.lo:T.hi) {
	Hatch_Fish.mu[k, 1:2, t]=0
	Hatch_Fish.mu[k, 6:I, t] = 0
	Hatch_Fish.mu[k,6,t]= Hatch_Fish_Inputs[1, 2]
} 

Rel_Surv_Inputs = read.csv(Watershed.Input.File, header=F,
                           skip=143, nrow=G)[, 4:9] 
for (t in T.lo:T.hi) {
	for (g in 1:G) {
		for (i in 1:I) {
			Rel_Surv.mu[k,i,t,g]<-(Rel_Surv_Inputs[g, min(i,6)])
		}
	}
}
rm(Rel_Surv_Inputs)


Fecund_Inputs = read.csv(Watershed.Input.File, header=F,
        skip=150, nrow=G)[, 4:9]
for (t in T.lo:T.hi) {
     Female_Fecundity[k,,t,] = t(Fecund_Inputs)
}
rm(Fecund_Inputs)

Post_Spawn_Survival_Anadromous_Inputs = 
			 read.csv(Watershed.Input.File, header=F,
                   skip=157, nrow=G)[, 4:13]
for (t in T.lo:T.hi) {
     Post_Spawn_Survival_Anadromous_M.mu[k,,t,] = t(Post_Spawn_Survival_Anadromous_Inputs[,1:5])
     Post_Spawn_Survival_Anadromous_F.mu[k,,t,] = t(Post_Spawn_Survival_Anadromous_Inputs[,6:10])
}
rm(Post_Spawn_Survival_Anadromous_Inputs)

Post_Spawn_Survival_Rainbow_Inputs =
	 read.csv(Watershed.Input.File, header=F,
                   skip=163, nrow=G)[, 4:13]  #Pete October 2015 Fix--was previously referencing the wrong row...
for (t in T.lo:T.hi) {
     Post_Spawn_Survival_Rainbow_M.mu[k,,t,] = t(Post_Spawn_Survival_Rainbow_Inputs[,1:5])
     Post_Spawn_Survival_Rainbow_F.mu[k,,t,] = t(Post_Spawn_Survival_Rainbow_Inputs[,6:10])
}

rm(Post_Spawn_Survival_Rainbow_Inputs)

} # close site



# Cross Site Migration Matrix

Cross.Site.Mig = read.csv(as.character(Cross.site.migration.file.names[n.step]), header=F,
    skip= 6, nrows=43)[, 3:27]

Cross.Site.Mig
for (t in T.lo:T.hi) {
	for (k1 in 1:K) {
 		for (k2 in 1:K) {
			Fry.x.siteMigration.mu[k1, k2,t] = Cross.Site.Mig[k1,k2]
			Par.x.siteMigration.mu[k1, k2,t] = Cross.Site.Mig[k1+11,k2]
			OnePlus.x.siteMigration.mu[k1, k2,t] = Cross.Site.Mig[k1+22,k2]
			Spawner.x.siteMigration.mu[k1, k2,t] = Cross.Site.Mig[k1+33,k2]
			Fry.x.siteMigration.target[k1, k2,t] = Cross.Site.Mig[k1,k2+14]
			Par.x.siteMigration.target[k1, k2,t] = Cross.Site.Mig[k1+11,k2+14]
			OnePlus.x.siteMigration.target[k1, k2,t] = Cross.Site.Mig[k1+22,k2+14]
			Spawner.x.siteMigration.target[k1, k2,t] = Cross.Site.Mig[k1+33,k2+14]
 		}
		Fry.x.siteMigration.alphaR[k1, t] = as.numeric(as.character(Cross.Site.Mig[k1, 11]))
		Fry.x.siteMigration.alphaT[k1, t] =  as.numeric(as.character(Cross.Site.Mig[k1, 12]))
		Fry.x.siteMigration.alphaS[k1, t] =  as.numeric(as.character(Cross.Site.Mig[k1, 13]))
		Fry.x.siteMigration.alpha[k1, t] =  as.numeric(as.character(Cross.Site.Mig[k1, 14]))
		Fry.x.siteMigration.rate[k1, t]  = as.numeric(as.character(Cross.Site.Mig[k1, 25]))

		Par.x.siteMigration.alphaR[k1,t] =  as.numeric(as.character(Cross.Site.Mig[k1+11,11]))
		Par.x.siteMigration.alphaT[k1,t] =  as.numeric(as.character(Cross.Site.Mig[k1+11,12]))
		Par.x.siteMigration.alphaS[k1,t] =  as.numeric(as.character(Cross.Site.Mig[k1+11,13]))
		Par.x.siteMigration.alpha[k1,t] =  as.numeric(as.character(Cross.Site.Mig[k1+11,14]))
		Par.x.siteMigration.rate[k1, t]  = as.numeric(as.character(Cross.Site.Mig[k1+11, 25]))

		OnePlus.x.siteMigration.alphaR[k1,t] =  as.numeric(as.character(Cross.Site.Mig[k1+22,11]))
		OnePlus.x.siteMigration.alphaT[k1,t] =  as.numeric(as.character(Cross.Site.Mig[k1+22,12]))
		OnePlus.x.siteMigration.alphaS[k1,t] =  as.numeric(as.character(Cross.Site.Mig[k1+22,13]))
		OnePlus.x.siteMigration.alpha[k1,t] =  as.numeric(as.character(Cross.Site.Mig[k1+22,14]))
		OnePlus.x.siteMigration.rate[k1, t]  = as.numeric(as.character(Cross.Site.Mig[k1+22, 25]))

		Spawner.x.siteMigration.alphaR[k1,t] =  as.numeric(as.character(Cross.Site.Mig[k1+33,11]))
		Spawner.x.siteMigration.alphaT[k1,t] =  as.numeric(as.character(Cross.Site.Mig[k1+33,12]))
		Spawner.x.siteMigration.alphaS[k1,t] =  as.numeric(as.character(Cross.Site.Mig[k1+33,13]))
		Spawner.x.siteMigration.alpha[k1,t] =  as.numeric(as.character(Cross.Site.Mig[k1+33,14]))
		Spawner.x.siteMigration.rate[k1, t]  = as.numeric(as.character(Cross.Site.Mig[k1+33, 25]))
	}
}

} # close cycling through number of input files



# Need to return EVERYTHING!!!


Inputs = 

list(
"frac.mu"=frac.mu, "frac.sigmaR"=frac.sigmaR,  "frac.sigmaT"=frac.sigmaT,  
   "frac.sigmaS"=frac.sigmaS,  "frac.sigma"=frac.sigma, 
	"frac.target" = frac.target, "frac.rate" = frac.rate,


"harvest.wild.minspawn.mu"= harvest.wild.minspawn.mu,
 "harvest.wild.minharvest.mu"= harvest.wild.minharvest.mu,
   "harvest.wild.maxharvest.mu"= harvest.wild.maxharvest.mu,
      "harvest.wild.ratepharvest.mu"= harvest.wild.ratepharvest.mu,
"harvest.hatch.minspawn.mu"= harvest.hatch.minspawn.mu,
 "harvest.hatch.minharvest.mu"= harvest.hatch.minharvest.mu,
   "harvest.hatch.maxharvest.mu"= harvest.hatch.maxharvest.mu,
      "harvest.hatch.ratepharvest.mu"= harvest.hatch.ratepharvest.mu,

"Prod_Scalar.mu"=Prod_Scalar.mu, "Prod_Scalar.sigmaR"=Prod_Scalar.sigmaR,
 "Prod_Scalar.sigmaT"=Prod_Scalar.sigmaT,
 "Prod_Scalar.sigmaS"=Prod_Scalar.sigmaS,
 "Prod_Scalar.sigma"=Prod_Scalar.sigma,
 "Prod_Scalar.target" = Prod_Scalar.target,
 "Prod_Scalar.rate" = Prod_Scalar.rate,

"M.mu"= M.mu, "M.alphaR.N" = M.alphaR.N,
 "M.alphaT.N" = M.alphaT.N, "M.alphaS.N" = M.alphaS.N, 
   "M.alpha.N" = M.alpha.N,
"M.target"=M.target, "M.rate" = M.rate,



"Ak_x_Lqk.mu"=Ak_x_Lqk.mu, "Ak_x_Lqk.sigmaR"=Ak_x_Lqk.sigmaR,
"Ak_x_Lqk.sigmaT"=Ak_x_Lqk.sigmaT,"Ak_x_Lqk.sigmaS"=Ak_x_Lqk.sigmaS,
"Ak_x_Lqk.sigma"=Ak_x_Lqk.sigma,
"Ak_x_Lqk.target"=Ak_x_Lqk.target,
"Ak_x_Lqk.rate"= Ak_x_Lqk.rate,


"D.mu"= D.mu, "D.sigmaR" = D.sigmaR, "D.sigmaT" = D.sigmaT,
 "D.sigmaS" = D.sigmaS, "D.sigma" = D.sigma,
   "D.target" = D.target, "D.rate" = D.rate,

"Sr.mu" = Sr.mu, "Sr.alphaR.N" = Sr.alphaR.N,  "Sr.alphaT.N" = Sr.alphaT.N,
 "Sr.alphaS.N" = Sr.alphaS.N, "Sr.alpha.N" = Sr.alpha.N,
  "Sr.target" = Sr.target, "Sr.rate"=Sr.rate,

"Frac.Subyear.M.mu" = Frac.Subyear.M.mu, 
"Frac.Subyear.F.mu" = Frac.Subyear.F.mu,
"Frac.Subyear.M.alphaR.N" = Frac.Subyear.M.alphaR.N,
"Frac.Subyear.M.alphaT.N" = Frac.Subyear.M.alphaT.N,
"Frac.Subyear.M.alphaS.N" =  Frac.Subyear.M.alphaS.N,
"Frac.Subyear.M.alpha.N" = Frac.Subyear.M.alpha.N, 
"Frac.Subyear.F.alphaR.N" =  Frac.Subyear.F.alphaR.N,
"Frac.Subyear.F.alphaT.N" =  Frac.Subyear.F.alphaT.N,
"Frac.Subyear.F.alphaS.N" =  Frac.Subyear.F.alphaS.N,
"Frac.Subyear.F.alpha.N" =  Frac.Subyear.F.alpha.N,



#"C_ocean.mu" = C_ocean.mu, "C_ocean.sigmaR" = C_ocean.sigmaR, 
# "C_ocean.sigmaT" = C_ocean.sigmaT,
# "C_ocean.sigmaS" = C_ocean.sigmaS, "C_ocean.sigma" = C_ocean.sigma,
#  "C_ocean.target"  =  C_ocean.target, "C_ocean.rate" = C_ocean.rate,

"SR5.mu" = SR5.mu, "SR5.alphaR.N" = SR5.alphaR,  "SR5.alphaT.N" = SR5.alphaT,
 "SR5.alphaS.N" = SR5.alphaS, "SR5.alpha.N" = SR5.alpha,
   "SR5.target" = SR5.target, "SR5.rate" = SR5.rate,


"N5.Psmolt_Female.mu" = N5.Psmolt_Female.mu, 
"N5.Pspawn_Female.mu" = N5.Pspawn_Female.mu,
"N5.Pstay_Female.mu" = N5.Pstay_Female.mu,
"N5.Psmolt_Female.target" = N5.Psmolt_Female.target,
"N5.Pspawn_Female.target" = N5.Pspawn_Female.target,
"N5.Pstay_Female.target" = N5.Pstay_Female.target,
"N5.P_Female.rate" = N5.P_Female.rate,
"N5.P.alphaR_Female.N" = N5.P.alphaR_Female.N,
"N5.P.alphaT_Female.N" = N5.P.alphaT_Female.N,
"N5.P.alphaS_Female.N" = N5.P.alphaS_Female.N,
"N5.P.alpha_Female.N" = N5.P.alpha_Female.N,


"N5.Psmolt_Male.mu" = N5.Psmolt_Male.mu, 
"N5.Pspawn_Male.mu" = N5.Pspawn_Male.mu,
"N5.Pstay_Male.mu" = N5.Pstay_Male.mu,
"N5.Psmolt_Male.target" = N5.Psmolt_Male.target,
"N5.Pspawn_Male.target" = N5.Pspawn_Male.target,
"N5.Pstay_Male.target" = N5.Pstay_Male.target,
"N5.P_Male.rate" = N5.P_Male.rate,
"N5.P.alphaR_Male.N" = N5.P.alphaR_Male.N,
"N5.P.alphaT_Male.N" = N5.P.alphaT_Male.N,
"N5.P.alphaS_Male.N" = N5.P.alphaS_Male.N,
"N5.P.alpha_Male.N" = N5.P.alpha_Male.N,


"N5.cap.mu" = N5.cap.mu, "N5.cap.sigmaR" = N5.cap.sigmaR,
"N5.cap.sigmaT" = N5.cap.sigmaT,"N5.cap.sigmaS" = N5.cap.sigmaS,
"N5.cap.sigma" = N5.cap.sigma,
"N5.cap.target" = N5.cap.target, "N5.cap.rate" = N5.cap.rate,

"Mat8Plus_Female.mu" = Mat8Plus_Female.mu, 
"Mat8Plus_Female.alphaR.N" = Mat8Plus_Female.alphaR.N,
"Mat8Plus_Female.alphaT.N" = Mat8Plus_Female.alphaT.N, 
"Mat8Plus_Female.alphaS.N" = Mat8Plus_Female.alphaS.N,
"Mat8Plus_Female.alpha.N" = Mat8Plus_Female.alpha.N,
"Mat8Plus_Female.target" = Mat8Plus_Female.target, 
"Mat8Plus_Female.rate" = Mat8Plus_Female.rate,


"Mat8Plus_Male.mu" = Mat8Plus_Male.mu, 
"Mat8Plus_Male.alphaR.N" = Mat8Plus_Male.alphaR.N,
"Mat8Plus_Male.alphaT.N" = Mat8Plus_Male.alphaT.N, 
"Mat8Plus_Male.alphaS.N" = Mat8Plus_Male.alphaS.N,
"Mat8Plus_Male.alpha.N" = Mat8Plus_Male.alpha.N,
"Mat8Plus_Male.target" = Mat8Plus_Male.target, 
"Mat8Plus_Male.rate" = Mat8Plus_Male.rate,


### will add variabilities here later for below, if needed/wanted....

"Hatch_Fish.mu"=Hatch_Fish.mu,
"Rel_Surv.mu"=Rel_Surv.mu,
"Rel_Comp.mu"=Rel_Comp.mu,
"Rel_Fecund.mu"=Rel_Fecund.mu,



"Female_Fecundity.mu"=Female_Fecundity,

"Post_Spawn_Survival_Anadromous_M.mu" = Post_Spawn_Survival_Anadromous_M.mu, 
"Post_Spawn_Survival_Anadromous_F.mu" = Post_Spawn_Survival_Anadromous_F.mu, 
"Post_Spawn_Survival_Rainbow_M.mu" = Post_Spawn_Survival_Rainbow_M.mu ,
"Post_Spawn_Survival_Rainbow_F.mu" = Post_Spawn_Survival_Rainbow_F.mu ,


#"Female_Frac.mu"= Female_Frac.mu,


"Fry.x.siteMigration.mu"=Fry.x.siteMigration.mu,
"Par.x.siteMigration.mu"=Par.x.siteMigration.mu,
"OnePlus.x.siteMigration.mu"=OnePlus.x.siteMigration.mu,
"Spawner.x.siteMigration.mu"=Spawner.x.siteMigration.mu,

"Fry.x.siteMigration.target"=Fry.x.siteMigration.target,
"Par.x.siteMigration.target"=Par.x.siteMigration.target,
"OnePlus.x.siteMigration.target"=OnePlus.x.siteMigration.target,
"Spawner.x.siteMigration.target"=Spawner.x.siteMigration.target,

"Fry.x.siteMigration.alphaR.N" = Fry.x.siteMigration.alphaR,
"Fry.x.siteMigration.alphaT.N" =Fry.x.siteMigration.alphaT,
"Fry.x.siteMigration.alphaS.N" = Fry.x.siteMigration.alphaS,
"Fry.x.siteMigration.alpha.N" = Fry.x.siteMigration.alpha,
"Fry.x.siteMigration.rate" = Fry.x.siteMigration.rate,

"Par.x.siteMigration.alphaR.N" = Par.x.siteMigration.alphaR,
"Par.x.siteMigration.alphaT.N" = Par.x.siteMigration.alphaT,
"Par.x.siteMigration.alphaS.N" = Par.x.siteMigration.alphaS,
"Par.x.siteMigration.alpha.N" = Par.x.siteMigration.alpha,
"Par.x.siteMigration.rate" = Par.x.siteMigration.rate,

"OnePlus.x.siteMigration.alphaR.N" = OnePlus.x.siteMigration.alphaR,
"OnePlus.x.siteMigration.alphaT.N" = OnePlus.x.siteMigration.alphaT,
"OnePlus.x.siteMigration.alphaS.N" = OnePlus.x.siteMigration.alphaS,
"OnePlus.x.siteMigration.alpha.N" = OnePlus.x.siteMigration.alpha,
"OnePlus.x.siteMigration.rate" = OnePlus.x.siteMigration.rate,

"Spawner.x.siteMigration.alphaR.N" = Spawner.x.siteMigration.alphaR,
"Spawner.x.siteMigration.alphaT.N" = Spawner.x.siteMigration.alphaT,
"Spawner.x.siteMigration.alphaS.N" = Spawner.x.siteMigration.alphaS,
"Spawner.x.siteMigration.alpha.N" = Spawner.x.siteMigration.alpha,
"Spawner.x.siteMigration.rate" = Spawner.x.siteMigration.rate,
"N5.Rainbow.Fecundity" = N5.Rainbow.Fecundity

)


Inputs
detach(header)
return(Inputs)
}
# End of Read Data Function

#### End of Function #################
######################################
######################################


#######
#header<- Read.Header("Watershed_Header_File.xlsx") 
#Inputs<-Read.Input.File(header)
