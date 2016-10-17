# Create Output Files and Plots
PostProcessing <- function(header, NR, N5R, Female_Spawners_NR) {


if (dev.cur() >1) {dev.off()}

if(.Platform$OS.type=="windows") {
  windows(record = TRUE) } else {quartz()}

#######################################################
### Post Processing #####
attach(header)

# create output folders to store results

dir.create("Output Plots",showWarnings = FALSE)
dir.create("Output Files", showWarnings = FALSE)


# Round off all global results data to nearest integer value
	NR = round(NR)
	N5R = round(N5R)
  Female_Spawners_NR = round(Female_Spawners_NR)
	Spawners_NR = round(Spawners_NR)
	Escapement_NR = round(Escapement_NR)
	Female_Escapement_NR = round(Female_Escapement_NR)



#############################################################################
## Write Final Time Step Results to a file, which
## can be renamed "Initial_Values.xlsx" and will then
## be read as the initial values for a future set of simulations
## with the same set of sites
##





NRFinal = array(0, c(K, 17,G))
N5Final = array(0, c(K, 10,G))
k=1
margin=c(1,2)

for (k in 1:K) {
	NRFinal[k,,] = apply(NR[k,,(header$Tr-2),,], margin, mean)
      NRFinal[k,2,] = apply(NR[k,,(header$Tr-1),,], margin, mean)[2,]
      NRFinal[k,4,] = apply(NR[k,,(header$Tr-1),,], margin, mean)[4,]
      NRFinal[k,6,] = apply(NR[k,,(header$Tr-1),,], margin, mean)[6,]
      NRFinal[k,7,] = apply(NR[k,,(header$Tr-1),,], margin, mean)[7,]

	N5Final[k,,] = apply(N5R[k,,(header$Tr-2),,], margin, mean)

	NRFinalNames=c("Adult Escapement","Egg","Fry","Par","PreSmolt",
	"Smolt","Adult_Y0","Adult_Y1","Adult_Y2","Adult_Y3","Adult_Y4",
	"Adult_Y5","Adult_Y6","Adult_Y7","Adult_Y8","Adult_Y9", "Adult_Y10")

}
#	rownames(NRFinal)=c(as.character(seq(1:header$K)))

	N5FinalNames = c("Pre-SmoltY1","Pre-SmoltY2","Pre-SmoltY3",
	"Pre-SmoltY4","Pre-SmoltY5","Pre-SmoltY6","Pre-SmoltY7","Pre-SmoltY8",
	"Pre-SmoltY9","Pre-SmoltY10")


#	rownames(N5Final)=c(as.character(seq(1:header$K)))

	subpop.names=c("Natural","H1","N.H2","H2","N-N.H3","N-H3","N.H3-H3","N.H3-H2",
               "H3-H2", "H3","N.H3-N.H3")

	
	for (k in 1:K) {

	NRout.data=t(NRFinal[k,2:17,])
	colnames(NRout.data)=NRFinalNames[2:17]
	N5out.data=t(N5Final[k,,])
	colnames(N5out.data)=N5FinalNames

	write.csv(
		as.data.frame(
		list("Site"=rep(Site.Names[k],G),
	      "SubPop"= subpop.names,
		NRout.data, N5out.data ))

            ,
	 	paste("Output Files/TFinal_", Site.Names[k],".csv", sep=""),
    	       row.names=FALSE)

}
#detach(header)


###############################################################
#	# Can copy the above file as input file for next round Inputs
##############################################################



##############################################
# Write full output file of results

LifeStageMeans = array(rep(0, header$Tr * 17*G), c(header$Tr,17*G))
LifeStageSds = array(rep(0, header$Tr * 17*G), c(header$Tr,17*G))


N1 = rep(subpop.names[1],17)
for (g in 2:G) {N1= c(N1, rep(subpop.names[g],17))}

colnames(LifeStageMeans) = paste(rep(NRFinalNames,G),"_",N1,"_Mean",sep="")
colnames(LifeStageSds)= paste(rep(NRFinalNames,G),"_",N1,"_sd",sep="")

for (k in 1:header$K) {

for (n in 1:17) {
for (g in 1:G)  {
for (t in 1:Tr) {
LifeStageMeans[t,(g-1)*17 +n] = mean(NR[k,n,t,g,])
LifeStageSds[t,(g-1)*17 +n] = sd(NR[k,n,t,g,])
}}}
#Old way
# write.csv(
# as.data.frame(list("year"=seq(1:header$Tr), LifeStageMeans[,],
#     LifeStageSds[,])), 
#    paste("Output Files/Results_",Site.Names[k],".csv",sep=""))
#New way
year<-seq(1:header$Tr)
write.csv(
  cbind(year, LifeStageMeans[,],
                     LifeStageSds[,]), 
  paste("Output Files/Results_",Site.Names[k],".csv",sep=""))

}  # end of sites


#########################################################


##############################################
# Write full output file of results for all pre-smolts all ages of pre-smolts

PS.LifeStageMeans = array(rep(0, header$Tr * I5*G), c(header$Tr,I5*G))
PS.LifeStageSds = array(rep(0, header$Tr * I5*G), c(header$Tr,I5*G))

NR5Names = c("PS.1", "PS.2", "PS.3","PS.4","PS.5","PS.6",
"PS.7","PS.8","PS.9","PS.10")


N1 = rep(subpop.names[1],I5)
g=2
k=1
for (g in 2:G) {N1= c(N1, rep(subpop.names[g],I5))}

colnames(PS.LifeStageMeans) = paste(rep(NR5Names,G),"_",N1,"_Mean",sep="")
colnames(PS.LifeStageSds)= paste(rep(NR5Names,G),"_",N1,"_sd",sep="")

k=3
n5=1
g=1
t=3
for (k in 1:header$K) {


for (n5 in 1:I5) {
for (g in 1:G)  {
for (t in 3:Tr) {
PS.LifeStageMeans[t,(g-1)*I5 +n5] = mean(N5R[k,n5,t,g,])
PS.LifeStageSds[t,(g-1)*I5 +n5] = sd(N5R[k,n5,t,g,])
}}}

#mean(N5R[k,n5,t,g,])
#mean(N5R[3,1,3,1,])
#dim(PS.LifeStageMeans)
#PS.LifeStageMeans[3,1:10]
#dim(N5R)
#Old way
# write.csv(
# as.data.frame(list("year"=seq(1:header$Tr), PS.LifeStageMeans[,],
#     PS.LifeStageSds[,])), 
#    paste("Output Files/PreSmolt_Results_",Site.Names[k],".csv",sep=""))
# New Way
year<-seq(1:header$Tr)
write.csv(
  cbind(year, PS.LifeStageMeans[,],
                     PS.LifeStageSds[,]), 
  paste("Output Files/PreSmolt_Results_",Site.Names[k],".csv",sep=""))

}  # end of sites


#########################################################




# Write file for  All Sites Combined
Total_by_LS_gtype_r = array(0,c(17,Tr,G,R))
Total_by_LS_gtype_mean = array(0,c(Tr,17*G))
Total_by_LS_gtype_sd = array(0,c(Tr,17*G))

#Pete Fix Feb 2016
N1 = rep(subpop.names[1],17)
for (g in 2:G) {N1= c(N1, rep(subpop.names[g],17))}



for (t in 1:Tr){
for (g in 1:G){
for (n in 1:17) {
for (r in 1:R) {
Total_by_LS_gtype_r[n,t,g,r] =sum(NR[,n,t,g,r]) }
Total_by_LS_gtype_mean[t,(g-1)*17+n] = mean(Total_by_LS_gtype_r[n,t,g,])
Total_by_LS_gtype_sd[t,(g-1)*17 +n] = sd(Total_by_LS_gtype_r[n,t,g,])
}}}



colnames(Total_by_LS_gtype_mean) = paste(rep(NRFinalNames,G),"_",N1,"_Mean",sep="")
colnames(Total_by_LS_gtype_sd)= paste(rep(NRFinalNames,G),"_",N1,"_sd",sep="")
year<-seq(1:Tr)
out<-cbind(year,Total_by_LS_gtype_mean,Total_by_LS_gtype_sd)
#Old Way
#write.csv(
# as.data.frame(list("year"=seq(1:header$Tr),Total_by_LS_gtype_mean[,],
#    Total_by_LS_gtype_sd[,])), 
#    paste("Output Files/Results_All_Sites_Combined.csv",sep=""))
#Pete New Way [Feb 2016]
write.csv(out, 
  paste("Output Files/Results_All_Sites_Combined.csv",sep=""))



##########################################################

#############################################3
# Write output file for returning spawners, average across all
# MC simulations and s_dev across all MC simulations, by Subpop Type


#attach(header)
for (k in 1:K) {

Spawner.Mean.by.Subpop= apply(Spawners_NR[k, ,,],c(1,2), mean)
Spawner.Sd.by.Subpop= apply(Spawners_NR[k, ,,],c(1,2), sd)

Smolt.Mean.by.Subpop= apply(NR[k,6, ,,],c(1,2), mean)
Smolt.Sd.by.Subpop= apply(NR[k,6, ,,],c(1,2), sd)


colnames(Spawner.Mean.by.Subpop)=c("Nat", "H1", "N-H2", "H2", "N-N.H3", "N-H3", "N.H3-H3",
         "N.H3-H2","H3-H2", "H3", "N.H3-N-H3")
colnames(Spawner.Sd.by.Subpop) = paste(rep("Sd.",11), c("Nat", "H1", "N-H2", "H2", "N-N.H3", "N-H3", "N.H3-H3",
         "N.H3-H2","H3-H2", "H3", "N.H3-N-H3"), sep="")
colnames(Smolt.Mean.by.Subpop)=c("Nat", "H1", "N-H2", "H2", "N-N.H3", "N-H3", "N.H3-H3",
         "N.H3-H2","H3-H2", "H3", "N.H3-N-H3")
colnames(Smolt.Sd.by.Subpop) = paste(rep("Sd.",11), c("Nat", "H1", "N-H2", "H2", "N-N.H3", "N-H3", "N.H3-H3",
         "N.H3-H2","H3-H2", "H3", "N.H3-N-H3"), sep="")


# Old way
# Spawners.by.Subpop = data.frame("Year"=seq(1:Tr), Spawner.Mean.by.Subpop, 
# Spawner.Sd.by.Subpop)

#New way
year<-seq(1:Tr)
Spawners.by.Subpop = cbind(year, Spawner.Mean.by.Subpop, 
                                Spawner.Sd.by.Subpop)

write.csv(Spawners.by.Subpop, 
paste("Output Files/Spawners_by_Subpop",Site.Names[k],".csv",sep=""))

# Old way
# Smolts.by.Subpop = data.frame("Year"=seq(1:Tr), Smolt.Mean.by.Subpop, 
# Smolt.Sd.by.Subpop)
#New way
Smolts.by.Subpop = cbind(year, Smolt.Mean.by.Subpop, 
                              Smolt.Sd.by.Subpop)


write.csv(Smolts.by.Subpop, 
paste("Output Files/Smolts_by_Subpop",Site.Names[k],".csv",sep=""))
}

#detach(header)

#########################################3

# HERE!!!!!























##########################################################

# Instruct the code to write to output .jpg files

# Do this twice... once to screen, once to files

k=1
for (p in 1:2) {

################################################
# Plot Results
###############################################

	for (k in 1:K) {


if (p==1)
 {jpeg(paste("Output Plots/","Returning Spawners Site ",Site.Names[k],".jpg",sep="")
, 8,6, units='in', res=300)}

# Find Upper Limit for plot
if (R>1) {
ploty= apply(apply(NR[k,1,1:Tr,,],c(1,3),sum),1, mean)
UL =1.2*(max((apply(apply(NR[k,1,1:Tr,,],c(1,3),sum),1, mean)+
    1.96* apply(apply(NR[k,1,1:Tr,,],c(1,3),sum), 1, sd))[2:(Tr-1)]))} else {
ploty= apply(NR[k,1,1:Tr,,],c(1),sum)
UL= max(ploty)
}




plot(seq(2:(Tr-1)), 
ploty[2:(Tr-1)], type="l",ylab="Spawning Fish",main=
	 paste("Number of Fish Returning to Spawn in Site ", Site.Names[k]), xlab="Time (years)",
	  ylim=c(0, UL))


if (R >1){

lines(seq(2:(Tr-1)), 
(apply(apply(NR[k,1,1:Tr,,],c(1,3),sum),1, mean)+
    1.96* apply(apply(NR[k,1,1:Tr,,],c(1,3),sum), 1, sd))[2:(Tr-1)]
, lt=2, col="red")

lines(seq(2:(Tr-1)), 
(apply(apply(NR[k,1,1:Tr,,],c(1,3),sum),1, mean)+
    -1.96* apply(apply(NR[k,1,1:Tr,,],c(1,3),sum), 1, sd))[2:(Tr-1)]
, lt=2, col="red")

legend("topright", c("Mean of MC Simulations", "95% Limits"), lt=c(1,2),
col=c("black", "red"))
}
        

if (p==1) {
dev.off()}
}


par(mfrow=c(1,1))



################################################
# Plot Results - different scales for each site
###############################################
#with(header, {
#attach(header)
#	par(mfrow=c(2,2))
	for (k in 1:K) {

if (p==1)
 {jpeg(paste("Output Plots/","Adult Escapement ",Site.Names[k],".jpg",sep="")
, 8,6, units='in', res=300)}


# Find Upper Limit for plot
if (R>1) {
ploty= apply(apply(Escapement_NR[k,1:Tr,,],c(1,3),sum),1, mean)
UL =1.2* (max((apply(apply(Escapement_NR[k,1:Tr,,],c(1,3),sum),1, mean)+
    1.96* apply(apply(Escapement_NR[k,1:Tr,,],c(1,3),sum), 1, sd))[2:(Tr-1)]))} else {
ploty= apply(Escapement_NR[k,1:Tr,,],c(1),sum)
UL= max(ploty)
}




plot(seq(2:(Tr-1)), ploty[2:(Tr-1)],
type="l",ylab="Adult Escapement",main=
	 paste("Adult Escapement for Site", Site.Names[k]), xlab="time (years)",
	  ylim=c(0, UL))

        

if (R >1){

lines(seq(2:(Tr-1)), 
(apply(apply(Escapement_NR[k,1:Tr,,],c(1,3),sum),1, mean)+
    +1.96* apply(apply(Escapement_NR[k,1:Tr,,],c(1,3),sum), 1, sd))[2:(Tr-1)]
, lt=2, col="red")

lines(seq(2:(Tr-1)), 
(apply(apply(Escapement_NR[k,1:Tr,,],c(1,3),sum),1, mean)+
    -1.96* apply(apply(Escapement_NR[k,1:Tr,,],c(1,3),sum), 1, sd))[2:(Tr-1)]
, lt=2, col="red")

legend("topright", c("Mean of MC Simulations", "95% Limits"), lt=c(1,2),
col=c("black","red"))
}
       


if (p==1) {
#print("dev off 2")
dev.off()}
}

par(mfrow=c(1,1))
#detach(header)



############################################




######################################
# Plot by hatchery type
######################################
#attach(header)
linetype = c(1,1,1,1,1,1,2,2,2,2,2)
par(mfrow=c(1,1))
ls=1 # plot returning spawners

k=1
for (k in 1:K){

#N=NR[,,,,ls]

legendtext=c("Nat", "H1", "N-H2", "H2", "N-N.H3", "N-H3", "N.H3-H3",
         "N.H3-H2","H3-H2", "H3", "N.H3-N-H3")

dim(NR)
dim(Spawners_NR)

if (R==1) { plotY= Spawners_NR[k, ,1,]} else
          { plotY = apply(Spawners_NR[k, ,1,],1, mean)}

if (p==1)
 {jpeg(paste("Output Plots/","Returning Spawners by Subspecies for Site",Site.Names[k],".jpg",sep="")
, 8,6, units='in', res=300)}

plot(seq(2:(Tr-1)),plotY[2:(Tr-1)], type="l", lwd=2, 
   xlim=c(0, Tr+40),ylim=
c(0, max(apply(Spawners_NR[k,,,], c(1,2),mean)+1)),
   xlab="years", ylab="# of fish",
   main=paste(Site.Names[k],":", 
"Number of Returning Spawners by 
SubSpecies Group, Average of All MC Runs"))
   
 
legend("topright", legendtext, col=seq(1:G), lt=linetype,
   title="Sub-Group")

for (g in 2:11){
if (R==1) { plotY= Spawners_NR[k, ,g,]} else
          { plotY = apply(Spawners_NR[k, ,g,],1, mean)}

lines(seq(2:(Tr-1)), plotY[2:(Tr-1)], col=g, lt=linetype[g])

}
if (p==1) {
#print("dev off 3")
dev.off()}
}





# calculate Egg-Smolt Survival Rate
# Averaged over all simulations
#attach(header)

#################################################################

dim(Escapement_NR)
dim(NR)
# Plot adult escapement, summed over all locations
dim(NR)
AE_sum_R = array(0, c(Tr,R))
for (r in 1:R) {
 for (t in 1:Tr) {
AE_sum_R[t,r] = sum(Escapement_NR[,t,,r])
}}
AE_sum = apply(AE_sum_R,1,mean)
AE_sd = apply(AE_sum_R, 1, sd)



if (R>1) { UL = 1.2*max(AE_sum+1.96* AE_sd)}else {UL=max(AE_sum)}

if (p==1)
 {jpeg(paste("Output Plots/","Adult Escapement Sum of All Locations.jpg",sep="")
, 8,6, units='in', res=300)}


plot(c(2:(Tr-1)),AE_sum[2:(Tr-1)], type="l",
main="Adult Escapement, Sum of All Locations", ylab="Adult Fish",
xlab="Year",
ylim=c(0,UL))


if (R >1){

lines(c(2:(Tr-1)), 
(AE_sum+1.96*AE_sd)[2:(Tr-1)]
, lt=2, col="red")

lines(c(2:(Tr-1)), 
(AE_sum-1.96*AE_sd)[2:(Tr-1)]
, lt=2, col="red")


legend("topright", c("Mean of MC Simulations", "95% Limits"), lt=c(1,2),
col=c("black","red"))
}




if (p==1) {
#print("dev off 4")
dev.off()}




#### Still need to fix this #####
# print egg-smolt survival rate, excluding hatchery fish
if (p==2) {
print(paste("Egg-Smolt Survival Rate (across all locations, Hatchery Fish Not Included):",
sum(apply(NR[,6,1:Tr,c(1,3:11),],2,sum)[20:Tr-1])/
sum(apply(NR[,2,1:Tr,c(1,3:11),],2,sum)[20:Tr-1])))
} 


####################################################################
# Plot Number of Non-hatchery smolts, average of all runs


NHS_Total_by_Site = array(0, c(K, Tr))
NHS_Total_by_Site_by_Run = array(0, c(K,Tr,R))
for (k in 1:K) {
 for (r in 1:R) {
NHS_Total_by_Site_by_Run[k,,R] = 
apply(NR[k,6,,c(1,3:11),r],1  , sum)
 }
NHS_Total_by_Site[k,] = 
apply(NHS_Total_by_Site_by_Run[k,,,drop=F]
,2,mean)
}

UL = max(NHS_Total_by_Site)


color = c(1:K)
lt = c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,3,3)

if (p==1)
 {jpeg(paste("Output Plots/","Non-Hatchery Smolts All Sites.jpg",sep="")
, 8,6, units='in', res=300)}


plot(c(1:(Tr-1)),NHS_Total_by_Site[1,][1:(Tr-1)], type="l",
lty=lt[1], col=color[1], ylim = c(0, UL*1.1), xlim=c(0, Tr*1.2),
xlab= "Time (Years)", ylab="Non-Hatchery Smolts",
main="Non-Hatchery Smolts Per Year", lwd=1.2)

#if (R>1) {
#lines(c(1:(Tr-1)),(NHS_mean+1.96*NHS_sd)[1:(Tr-1)], col="red", lt=2)
#lines(c(1:(Tr-1)),(NHS_mean-1.96*NHS_sd)[1:(Tr-1)], col="red", lt=2)
#}

Tot = rep(0, Tr)
for (k in 1:K) {
lines(c(1:(Tr-1)),NHS_Total_by_Site[k,][1:(Tr-1)],
lt=lt[k], col=color[k], lwd=1.2)
Tot = Tot + NHS_Total_by_Site[k,]
} 

#lines(c(1:(Tr-1)),Tot[1:(Tr-1)],
#lt=lt[1], col=color[1], lwd=1.2)

#lines(c(1:(Tr-1)),rep(mean(Tot), (Tr-1)),
#lt=2, col=color[1], lwd=1.2)

legend.text =Site.Names
legend("topright", legend.text, lty= c(lt), col=c(1:K), lwd=c(rep(1,K)))
#legend.text =c("Total All Sites", "Mean All Sites",Site.Names)
#legend("topright", legend.text, lty= c(1,2,lt), col=c(1,1,1:K), lwd=c(2,2, rep(1,K)))



if (p==1) {
# print("dev off 5")
dev.off()}

##############################################
###############################################
####################################################################
# Plot Number of Non-hatchery smolts per Female Spawner, average of all runs
# Calculate over all sites, since migration makes per-site smolts per
# spawner sort've meaningless
dim(NR)
dim(Female_Spawners_NR)
smolts_sum_all_locations= array(0, c(Tr, R))
f.spawners_sum_all_locations= array(0, c(Tr, R))
for (r in 1:R) {
 for (t in 1: Tr) {
smolts_sum_all_locations[t,r] = sum(NR[,6,t,c(1,3:11),r])
f.spawners_sum_all_locations[t,r] = sum(Female_Spawners_NR[,t,c(1,3:11),r])


}}

f.spawners_sum_all_locations
spf = smolts_sum_all_locations *0
temp = spf
len = length(spf)
for (l in 6:(len-5)) {
   temp[l] = mean(f.spawners_sum_all_locations[(l-5):(l+5)])
}

temp[1] = mean(f.spawners_sum_all_locations[1:10])
temp[2] = mean(f.spawners_sum_all_locations[1:10])
temp[3] = mean(f.spawners_sum_all_locations[1:10])
temp[4] = mean(f.spawners_sum_all_locations[1:10])
temp[5] = mean(f.spawners_sum_all_locations[1:10])


temp[len] = mean(f.spawners_sum_all_locations[(n-10):n])
temp[(len-1)] = mean(f.spawners_sum_all_locations[(n-10):n])
temp[(len-2)] = mean(f.spawners_sum_all_locations[(n-10):n])
temp[(len-3)] = mean(f.spawners_sum_all_locations[(n-10):n])
temp[(len-4)] = mean(f.spawners_sum_all_locations[(n-10):n])



#print(temp)

spf[3: length(spf)]
spf = smolts_sum_all_locations/ temp # f.spawners_sum_all_locations
spf_sd=0

spf_mean=apply(spf,1,mean)
spf

UL = max(spf_mean[10:(Tr-1)])
UL

if (R >1) {
spf_sd = apply(spf,1,sd)
UL = max(1.2*(spf_mean + 1.96* spf_sd)[10:(Tr-1)])
}


if (p==1)
 {jpeg(paste("Output Plots/","Smolts per Female Spawner Sum of All Sites.jpg",sep="")
, 8,6, units='in', res=300)}



plot(c(2:(Tr-1)),
spf_mean[2:(Tr-1)],type="l",
ylim = c(0, UL*1.5), xlim=c(0, Tr),
xlab= "Time (Years)", ylab="Non-Hatchery Smolts per Female Spawner
Sum of All Locations",
main="Non-Hatchery Smolts Per Female Spawner")


if (R >1){

lines(2:(Tr-1), 
(spf_mean+1.96* spf_sd)[2:(Tr-1)]
, lt=2, col="red")

lines(2:(Tr-1), 
(spf_mean-1.96* spf_sd)[2:(Tr-1)]
, lt=2, col="red")


legend("topright", c("Mean of MC Simulations", "95% Limits"), lt=c(1,2),
col=c("black","red"))
}



if (p==1) {
#print("dev off 6")
dev.off()}

######################################################

} # end of p=1:2

detach(header)

}


