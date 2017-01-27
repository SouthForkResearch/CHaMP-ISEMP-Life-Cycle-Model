

##################################################################
Read.Header <- function(Header.File) {
##################################################################

I=12 #(number of life stages) ### Need to Read This in DATA FILE
I5=5 # Number of potential OnePlus years (Steelhead)


Header.File = "Watershed_Header_File.csv"

# Read in Watershed Information Table 2.3

#WshedKJQ=read.xlsx2(Header.File, sheetName="Header",
#         startRow=2, endRow=11, colClasses=c("numeric"),
#         colIndex=3:3, header=F)

species = as.character(read.csv(Header.File, nrows=1, header=F)[,3])
# correct for capitalization differences
if (species == "Chinook") {species = "chinook"}
if (species == "Steelhead")  {species = "steelhead"}

WshedKJQ = read.csv(Header.File, skip=1, nrows=10, header=F)[,3]
WshedKJQ



	#K, Q, and J: Number of Sites, Land Use Cats per site, Habitat Types per Site
	K=as.numeric(WshedKJQ[1])
	N.input.files=as.numeric(WshedKJQ[2])
	Q=as.numeric(WshedKJQ[3])
	J=as.numeric(WshedKJQ[4])

	G=11

	Tr=as.numeric(WshedKJQ[5]) 
	R=as.numeric(WshedKJQ[6])
	MCsim1=as.numeric(WshedKJQ[7])
	MCsim2=as.numeric(WshedKJQ[8])
	MCsim3=as.numeric(WshedKJQ[9])
	MCsim4=as.numeric(WshedKJQ[10])

rm(WshedKJQ)
N.input.files
Tr

#T.step.change = read.xlsx2(Header.File, sheetName="Header",
#          startRow=16, endRow=round(16+N.input.files-1),colIndex=3:3, header=F,
#           colClasses=c("numeric"))[,1]

T.step.change = as.numeric(read.csv(Header.File,
          skip=13, nrows=1, header=F)[,4:(4+N.input.files-1)])
T.step.change

# read in input file names (one input file for each step change in inputs)
#Input.file.names = as.character(read.xlsx(Header.File, sheetName="Header",
#         rowIndex=16:(16+N.input.files-1), colIndex=2:2, header=F)[,1])
#Input.file.names = as.character(read.xlsx2(Header.File, sheetName="Header",
#         startRow=16, endRow=(16+N.input.files-1), 
#         colClasses=c("character"),
#colIndex=2:2, header=F)[,1])


file.names = 
as.matrix((
read.csv(Header.File, 
  skip = 15, nrows = K, 
  header=F, 
  colClasses = rep("character", (1+N.input.files)))[1:K, 3:(N.input.files+3)]
))

Input.file.names= array(file.names[,2:(1+N.input.files)], c(K, N.input.files))
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

#watersheds = read.xlsx(Header.File, sheetName="Header",
#         rowIndex=31:(31+K-1), colIndex=2:6, header=F)
#watersheds = read.xlsx2(Header.File, sheetName="Header",
#         startRow= 31, endRow=(31+K-1), 
#         colClasses=rep("character",5),
#       colIndex=2:6, header=F)

#watersheds
#	Watershed.index = as.character(watersheds[,1])
#	River.index=as.character(watersheds[,2])
#	Stream.index=as.character(watersheds[,3])
#	Reach.index=as.character(watersheds[,4])
#	Site.index=as.character(watersheds[,5])
#rm(watersheds)

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

#attach(header)

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



Sr.mu = array(rep(0, K*I*Tr), c(K,I,Tr))
Sr.alphaR.N= Sr.mu
Sr.alphaT.N= Sr.mu
Sr.alphaS.N= Sr.mu
Sr.alpha.N = Sr.mu
Sr.target = Sr.mu
Sr.rate = Sr.mu

#harvest.wild.mu = array(rep(0,K*Tr), c(K, Tr))


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
	Mat8Plus_Female.mu = array(rep(0,K*5*Tr), c(K, 5, Tr))
	Mat8Plus_Female.alphaR.N = Mat8Plus_Female.mu
	Mat8Plus_Female.alphaT.N = Mat8Plus_Female.mu
	Mat8Plus_Female.alphaS.N = Mat8Plus_Female.mu
	Mat8Plus_Female.alpha.N = Mat8Plus_Female.mu
	Mat8Plus_Female.target = Mat8Plus_Female.mu
	Mat8Plus_Female.rate = Mat8Plus_Female.mu

	Mat8Plus_Male.mu = array(rep(0,K*5*Tr), c(K, 5, Tr))
	Mat8Plus_Male.alphaR.N = Mat8Plus_Female.mu
	Mat8Plus_Male.alphaT.N = Mat8Plus_Female.mu
	Mat8Plus_Male.alphaS.N = Mat8Plus_Female.mu
	Mat8Plus_Male.alpha.N = Mat8Plus_Female.mu
	Mat8Plus_Male.target = Mat8Plus_Female.mu
	Mat8Plus_Male.rate = Mat8Plus_Female.mu



#	Fc.by.O.Age.mu = Mat8Plus.mu
#	Fc.by.O.Age.sigmaR = Mat8Plus.mu
#	Fc.by.O.Age.sigmaT = Mat8Plus.mu
#	Fc.by.O.Age.sigmaS = Mat8Plus.mu
#	Fc.by.O.Age.sigma = Mat8Plus.mu
#	Fc.by.O.Age.target = Fc.by.O.Age.mu
#	Fc.by.O.Age.rate = Fc.by.O.Age.mu


	C_ocean.mu = Mat8Plus_Female.mu 
	C_ocean.sigmaR = Mat8Plus_Female.mu
	C_ocean.sigmaT = Mat8Plus_Female.mu
	C_ocean.sigmaS = Mat8Plus_Female.mu
	C_ocean.sigma = Mat8Plus_Female.mu
	C_ocean.target = C_ocean.mu
	C_ocean.rate = C_ocean.mu


# frac
	frac.mu = array(rep(0, K*5*(J)*Tr), c(K, 5, (J), Tr))
	frac.sigmaR = frac.mu
	frac.sigmaT = frac.mu
	frac.sigmaS = frac.mu
	frac.sigma = frac.mu
	frac.target = frac.mu
	frac.rate = frac.mu

harvest.wild.mu = array(rep(0,K*Tr), c(K, Tr))
harvest.wild.sigmaR = harvest.wild.mu
harvest.wild.sigmaT = harvest.wild.mu
harvest.wild.sigmaS = harvest.wild.mu
harvest.wild.sigma = harvest.wild.mu

harvest.hatch.mu = harvest.wild.mu
harvest.hatch.sigmaR = harvest.wild.mu
harvest.hatch.sigmaT = harvest.wild.mu
harvest.hatch.sigmaS = harvest.wild.mu
harvest.hatch.sigma = harvest.wild.mu

harvest.wild.target = harvest.wild.mu
harvest.wild.rate = harvest.wild.mu
harvest.hatch.target = harvest.wild.mu
harvest.hatch.rate = harvest.wild.mu

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
Female_Fecundity = array(rep(0, K*5*Tr*G), c(K,5,Tr,G))




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

###########################

################################
################################
# loop through each site within input file
k=1
n.step=1


for (n.step in 1:N.input.files) {
	T.lo= as.numeric(T.step.change[n.step])
	if (n.step==N.input.files) {T.hi=Tr} else {T.hi= as.numeric(T.step.change[n.step+1])-1}
T.lo
T.hi
n.step
N.input.files
T.step.change


# loop through each input file
#Watershed.Input.File=Input.file.names[n.step] 
k=1
	for (k in 1:K) {
    #print(k)
#	Site=paste("Site",k,sep="")
Watershed.Input.File = as.character(Input.file.names[k, n.step])
Watershed.Input.File
# Read the M's
#T2.3 <-read.xlsx2(Watershed.Input.File, sheetName=Site,
#         startRow=26, endRow=(26+Q-1), colClasses=rep("numeric",J),
#         rowIndex=26:(26+Q-1), 
#  colIndex=3:(3+J-1), header=F)
T2.3 <- as.matrix(read.csv(Watershed.Input.File, header=F,skip=27, nrows=Q)[,3:(3+J-1)])
T2.3
#T2.3Nalpha <- read.xlsx2(Watershed.Input.File, sheetName=Site,
#         startRow=26, endRow=(26+Q-1), colClasses=rep("numeric",4),
##         rowIndex=26:(26+Q-1), 
#         colIndex=15:18, header=F)
T2.3Nalpha <- as.matrix(read.csv(Watershed.Input.File, header=F,
         skip=27, nrows=Q)[,15:18])
T2.3Nalpha
#T2.3target <- read.xlsx2(Watershed.Input.File, sheetName=Site,
#          startRow=26, endRow=(26+Q-1), colClasses= rep("numeric",13),
##         rowIndex=26:(26+Q-1), 
#          colIndex=19:31, header=F)
T2.3target <- as.matrix(read.csv(Watershed.Input.File, header=F,
         skip=27, nrows=Q)[,19:(31)])
T2.3target

#T2.3rate <- read.xlsx(Watershed.Input.File, sheetName=Site,
#         rowIndex=26:(26+Q-1), colIndex=19:3, header=F)

T2.3rate <- read.csv(Watershed.Input.File, header=F,
         skip=27, nrows=Q)[,31]
T2.3rate


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

#} # close site 
#} # close time

##### OK to here... repeat for all other variables (7/8/2013)
#######################################################
# Read Ak_x_Lqk_vectors


#Ak <-read.xlsx2(Watershed.Input.File, sheetName=Site,
#         startRow=9, endRow=(9+Q-1), colClasses=rep("numeric",7),
##         rowIndex=9:(9+Q-1), 
#colIndex=3:9, header=F)

Ak <- read.csv(Watershed.Input.File, header=F,
         skip=9, nrows=Q)[,3:9]
Ak

for (t in T.lo:T.hi) {
	Ak_x_Lqk.mu[k, ,t] <-Ak[,1]
	Ak_x_Lqk.sigmaR[k, ,t] <-Ak[,2]
	Ak_x_Lqk.sigmaT[k, ,t] <-Ak[,3]
	Ak_x_Lqk.sigmaS[k, ,t] <-Ak[,4]
	Ak_x_Lqk.sigma[k, ,t] <-Ak[,5]
	Ak_x_Lqk.target[k, ,t] <-Ak[,6]
	Ak_x_Lqk.rate[k, ,t] <- Ak[,7]
} # end t
dim(Ak_x_Lqk.mu)
Ak_x_Lqk.mu[,,1:10]
Ak_x_Lqk.target[,,1:10]
rm(Ak)


#### OK to here 7/8/2013 3:03 pm #######




#########################################
# Read in Table 2_4 (to get to D matrix)




#Dtable= read.xlsx2(Watershed.Input.File, sheetName=Site,
#         startRow=43, endRow=55, colClasses=rep("numeric",35),
##         rowIndex=43:55, 
#colIndex=3:37, header=F)

Dtable = read.csv(Watershed.Input.File, header=F,
         skip=44, nrows=12)[,3:44]
Dtable

# Note - this has been updated so that capcity of spawning gravel is input directly in "spawner to egg" category

t=1
i=1
J
for (t in T.lo:T.hi) {
for (i in 1:6) { 
# 6 is for 6 life stages (including one each for subyearlings or yearling chinook
D.mu[k,1:J,i,t] = Dtable[1:J,i] 
#D.mu[k, (J+1),i ,t] = Dtable[13, i]
D.sigmaR[k,1:J,i,t] = Dtable[1:J,(i+6)] 
#D.sigmaR[k, (J+1),i ,t] = Dtable[13, (i+6)]
D.sigmaT[k,1:J,i,t] = Dtable[1:J,(i+12)] 
#D.sigmaT[k, (J+1),i ,t] = Dtable[13, (i+12)]
D.sigmaS[k,1:J,i,t] = Dtable[1:J,(i+18)] 
#D.sigmaS[k, (J+1),i ,t] = Dtable[13, (i+18)] 
D.sigma[k,1:J,i,t] = Dtable[1:J,(i+24)] 
#D.sigma[k, (J+1),i ,t] = Dtable[13,(i+24)] 
D.target[k,1:J,i,t] = Dtable[1:J,(i+30)]
#D.target[k,(J+1),i,t] = Dtable[13,(i+30)]
D.rate[k,1:J,i,t] = Dtable[1:J, (i+36)]
#D.rate[k,(J+1),i,t] = Dtable[13, (i+36)]
}
}
dim(D.mu)

D.mu[1,1,1,1:10]
D.target[1,1,1,1:10]
rm(Dtable)


####### Productivity Scalars

#Etable= read.xlsx2(Watershed.Input.File, sheetName=Site,
#         startRow=61, endRow=(61+Q-1), colClasses=rep("numeric",36),
##         rowIndex=61:(61+Q-1), 
#         colIndex=3:38, header=F)

Etable = read.csv(Watershed.Input.File, header=F,
        skip=62, nrows=Q)[,3:44]


Etable
for (t in T.lo:T.hi) {
for (i in 1:5) {
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

Prod_Scalar.mu[1,1,1,1:10]
Prod_Scalar.target[1,1,1,1:10]
dim(Prod_Scalar.mu)
#?read.xlsx



I
#SrTable = read.xlsx2(Watershed.Input.File, sheetName=Site,
#         startRow=78, endRow=(78+I-1-1), colClasses=rep("numeric",7),
##         rowIndex=78:(78+I-1-1), 
#         colIndex=4:10, header=F)

SrTable = read.csv(Watershed.Input.File, header=F,
        skip= 79, nrows = I-1)[,4:10]


SrTable
for (t in T.lo:T.hi) {
Sr.mu[k,2:I ,t] = (SrTable[,1])
Sr.alphaR.N[k,2:I ,t]= SrTable[,2]
Sr.alphaT.N[k,2:I ,t]= SrTable[,3]
Sr.alphaS.N[k,2:I ,t]= SrTable[,4]
Sr.alpha.N[k,2:I ,t]= SrTable[,5]
Sr.target[k, 2:I, t] = SrTable[,6]
Sr.rate[k, 2:I, t] = SrTable[,7]
}

rm(SrTable)
dim(Sr.mu)
Sr.mu[1,1:5,1:10]
Sr.target[1,1:5, 1:10]

# Fraction Subyearling Inputs
# HERE!!
SRLTable = read.csv(Watershed.Input.File, header=F,
        skip= 98, nrows = 1)[4:13]
SRLTable
dim(SRLTable)
dim(Frac.Subyear.M.mu)
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

Frac.Subyear.M.mu



### OnePlus Inputs
I5
#PSinputs = read.xlsx2(Watershed.Input.File, sheetName=Site,
#       startRow=99, endRow=(99+I5-1),colClasses=rep("numeric", 26),
##       rowIndex=99:(99+I5-1), 
#        colIndex=3:28, header=F)


PSinputs = read.csv(Watershed.Input.File, header=F,
      skip=103, nrows = I5)[, 3:39]
PSinputs

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


N5.Rainbow.Fecundity
N5.cap.mu

rm(PSinputs)
#o.inputs = read.xlsx2(Watershed.Input.File, sheetName=Site,
#         startRow=113, endRow=(113+10-1), colClasses=rep("numeric",21),
##         rowIndex=113:(113+10-1), 
#          colIndex=4:24, header=F)

o.inputs = read.csv(Watershed.Input.File, header=F,
         skip=113, nrow=5)[, 4:24]
o.inputs
for (t in T.lo:T.hi) {
	Mat8Plus_Female.mu[k, ,t] = o.inputs[,1]
	Mat8Plus_Female.alphaR.N[k, ,t] = o.inputs[,2]
	Mat8Plus_Female.alphaT.N[k, ,t] = o.inputs[,3]
	Mat8Plus_Female.alphaS.N[k, ,t] = o.inputs[,4]
	Mat8Plus_Female.alpha.N[k, ,t] = o.inputs[,5]
	Mat8Plus_Female.target[k, ,t] = o.inputs[,16]
	Mat8Plus_Female.rate[k, ,t] = o.inputs[,17]

	Mat8Plus_Male.mu[k, ,t] = o.inputs[,6]
	Mat8Plus_Male.alphaR.N[k, ,t] = o.inputs[,7]
	Mat8Plus_Male.alphaT.N[k, ,t] = o.inputs[,8]
	Mat8Plus_Male.alphaS.N[k, ,t] = o.inputs[,9]
	Mat8Plus_Male.alpha.N[k, ,t] = o.inputs[,10]
	Mat8Plus_Male.target[k, ,t] = o.inputs[,18]
	Mat8Plus_Male.rate[k, ,t] = o.inputs[,19]


	C_ocean.mu[k, ,t] = o.inputs[,11]
	C_ocean.sigmaR[k, ,t] = o.inputs[,12]
	C_ocean.sigmaT[k, ,t] = o.inputs[,13]
	C_ocean.sigmaS[k, ,t] = o.inputs[,14]
	C_ocean.sigma[k, ,t] = o.inputs[,15]
	C_ocean.target[k, ,t] = o.inputs[, 20]
	C_ocean.rate[k, ,t] = o.inputs[,21]

}


rm(o.inputs)
### read "frac"

#fractions = read.xlsx2(Watershed.Input.File, sheetName=Site,
#         startRow=128, endRow=(128+5-1),colClasses=rep("numeric", 84),
##         rowIndex=128:(128+5-1), 
#          colIndex=4:87, header=F)

fractions = read.csv(Watershed.Input.File, header=F,
          skip=123, nrows = 5)[,4:87]

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
frac.rate[k, , ,t]
rm(fractions)

dim(frac.mu)
frac.mu[1,,,1:10]
frac.target[1,,,1:10]



###################


#harvest = read.xlsx2(Watershed.Input.File, sheetName=Site,
#         startRow = 137, endRow=138, colClasses=rep("numeric", 7),
##         rowIndex=137:138, 
#          colIndex=3:9, header=F)

harvest = read.csv(Watershed.Input.File, header=F, 
         skip = 132, nrows = 2)[, 3:9]

harvest
for (t in T.lo:T.hi) {
harvest.wild.mu[k,t] = harvest[1,1]
harvest.wild.sigmaR[k,t] = harvest[1,2]
harvest.wild.sigmaT[k,t] = harvest[1,3]
harvest.wild.sigmaS[k,t] = harvest[1,4]
harvest.wild.sigma[k,t] = harvest[1,5]
harvest.wild.target[k,t] = harvest[1,6]
harvest.wild.rate[k,t] = harvest[1,7]

harvest.hatch.mu[k,t] = harvest[2,1]
harvest.hatch.sigmaR[k,t] = harvest[2,2]
harvest.hatch.sigmaT[k,t] = harvest[2,3]
harvest.hatch.sigmaS[k,t] = harvest[2,4]
harvest.hatch.sigma[k,t] = harvest[2,5]
harvest.hatch.target[k,t] = harvest[2,6]
harvest.hatch.rate[k,t] = harvest[2,7]


} # close t
rm(harvest)


################################################

#Hatch_Fish_Inputs = read.xlsx2(Watershed.Input.File, sheetName=Site,
#         startRow=145, endRow=145, colClasses= rep("numeric",4),
##         rowIndex=145:145, 
#          colIndex=4:7, header=F)


Hatch_Fish_Inputs = read.csv(Watershed.Input.File, header=F,
   skip=140, nrows=1)[, 2:5]
Hatch_Fish_Inputs
#dim(Hatch_Fish_Inputs)
for (t in T.lo:T.hi) {
   Hatch_Fish.mu[k, 1:2, t]=0
   Hatch_Fish.mu[k, 6:I, t] = 0
 for (i in 3:6) {
   Hatch_Fish.mu[k,i,t]= Hatch_Fish_Inputs[1, i-2]
 }
}  
Hatch_Fish.mu    


#Rel_Surv_Inputs = read.xlsx2(Watershed.Input.File, sheetName=Site,
#         startRow=149, endRow=160, colClasses=rep("numeric",8),
##         rowIndex=149:160, 
#          colIndex=4:10, header=F)

Rel_Surv_Inputs = read.csv(Watershed.Input.File, header=F,
                           skip=145, nrow=11)[, 4:9] #Pete Feb 2016, this be nrow = 11 not 12, right?
#                           skip=145, nrow=12)[, 4:9] #Pete Feb 2016, this be nrow = 11 not 12, right?

#Rel_Surv_Inputs
# Will add variability at a later time ---M@


for (t in T.lo:T.hi) {
  for (g in 1:G) {
   for (i in 1:I) {



# the "min" is used below to assign all adult stages the same Rel_Surv
# and Rel_Comp values
Rel_Surv.mu[k,i,t,g]<-(Rel_Surv_Inputs[g, min(i,6)])
#Rel_Comp.mu[k,i,t,g]<-Rel_Comp_Inputs[g, min(i,6)]
#Rel_Comp.mu[k,i,t,g]
#Rel_Comp_Inputs[g, min(i,6)]


 }
}

}
Rel_Surv_Inputs
Rel_Surv_Inputs[g, min(i,6)]
Rel_Surv.mu[k,,t,]

rm(Rel_Surv_Inputs)
#rm(Rel_Comp_Inputs)




Fecund_Inputs = read.csv(Watershed.Input.File, header=F,
        skip=161, nrow=11)[, 4:8]
Fecund_Inputs
for (t in T.lo:T.hi) {
     Female_Fecundity[k,,t,] = t(Fecund_Inputs)
}
rm(Fecund_Inputs)



#Post_Spawn_Survival_Anadromous =  array(rep(0, K*10*Tr*G), c(K,10,Tr,G))
#Post_Spawn_Survival_Rainbow =  array(rep(0, K*10*Tr*G), c(K,10,Tr,G))


Post_Spawn_Survival_Anadromous_Inputs = 
			 read.csv(Watershed.Input.File, header=F,
                   skip=177, nrow=11)[, 4:13]
Post_Spawn_Survival_Anadromous_Inputs
for (t in T.lo:T.hi) {
     Post_Spawn_Survival_Anadromous_M.mu[k,,t,] = t(Post_Spawn_Survival_Anadromous_Inputs[,1:5])
     Post_Spawn_Survival_Anadromous_F.mu[k,,t,] = t(Post_Spawn_Survival_Anadromous_Inputs[,6:10])
}
rm(Post_Spawn_Survival_Anadromous_Inputs)



Post_Spawn_Survival_Rainbow_Inputs =
	 read.csv(Watershed.Input.File, header=F,
                   skip=192, nrow=11)[, 4:13]  #Pete October 2015 Fix--was previously referencing the wrong row...
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

#Cross.Site.Mig = read.xlsx2("Cross_Site_Migration.csv", 
#    startRow = 8, endRow=50, colClasses = rep("numeric", 25),     
##   rowIndex=8:50, 
#    colIndex=4:28, header=F,)


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

}}

Fry.x.siteMigration.target
Fry.x.siteMigration.alphaT

} # close cycling through number of input files



# Need to return EVERYTHING!!!


Inputs = 

list(
"frac.mu"=frac.mu, "frac.sigmaR"=frac.sigmaR,  "frac.sigmaT"=frac.sigmaT,  
   "frac.sigmaS"=frac.sigmaS,  "frac.sigma"=frac.sigma, 
	"frac.target" = frac.target, "frac.rate" = frac.rate,


"harvest.wild.mu"= harvest.wild.mu,
 "harvest.wild.sigmaR"= harvest.wild.sigmaR,
   "harvest.wild.sigmaT"= harvest.wild.sigmaT,
      "harvest.wild.sigmaS"= harvest.wild.sigmaS,
        "harvest.wild.sigma"= harvest.wild.sigma,
		"harvest.wild.target" = harvest.wild.target,
			"harvest.wild.rate" = harvest.wild.rate,
"harvest.hatch.mu"= harvest.hatch.mu,
 "harvest.hatch.sigmaR"= harvest.hatch.sigmaR,
   "harvest.hatch.sigmaT"= harvest.hatch.sigmaT,
      "harvest.hatch.sigmaS"= harvest.hatch.sigmaS,
        "harvest.hatch.sigma"= harvest.hatch.sigma,
		"harvest.hatch.target" = harvest.hatch.target,
			"harvest.hatch.rate" = harvest.hatch.rate,


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



"C_ocean.mu" = C_ocean.mu, "C_ocean.sigmaR" = C_ocean.sigmaR, 
 "C_ocean.sigmaT" = C_ocean.sigmaT,
 "C_ocean.sigmaS" = C_ocean.sigmaS, "C_ocean.sigma" = C_ocean.sigma,
  "C_ocean.target"  =  C_ocean.target, "C_ocean.rate" = C_ocean.rate,

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
