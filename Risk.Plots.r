Reconstruction <- function(header.data) {

BURN_IN=40
#RUNS <- 500 #
#YEARS <- 50 # years modeled
#QET <- 100 # Quasi Extinction Threshold
#ALTS <- 6 # alternate versions of the model (can be 1?)
#ALTS=1
#load("results.to.Rich2.RData")
#ls()
#ext.array1 #Not used
#ext.array2 #Not used
#dim(ext.array1)
#dim(rich.out1)
#dim(rich.out2)

#dim(rich.out1)
#rich.out1[,1:4,]

#x <- rich.out1 # The only thing used is this!
#dim(x)
#x=x[,,1, drop=F]
#dim(x)
#dim(x)
# Matt Notes
#x[1,,1] # spawners run 1, alternative 1, years 1:50
#x[501,,1] # recruits
#x[1001,,1] #Phos - percent hatchery orgin spawners
#x[1501,,1] # PNI

#PNI ~ Pnob / (Pnob + Phos)
#where  Pnob is the proportion pf natural-origin fish in the broodstock and 
#Phos  is the proportion of hatchery-origin fish on the spawning grounds.


RUNS = header$R
YEARS = header$Tr-BURN_IN-10 # Was 20
QET = 10
ALTS = 1
YEARS
x = array(0, c(RUNS*4, YEARS, ALTS))
RUNS
for (run in 1:RUNS) {
	for (year in 1:YEARS) {
		x[run,year,1] = N_SPAWNER_RECRUIT_NR[year+BURN_IN,1,run]      # spawners
		x[RUNS+run,year,1] = N_SPAWNER_RECRUIT_NR[year+BURN_IN,2,run] # recruits
		x[2*RUNS+run,year,1] = 0                              # Phos=0 ?
		x[3*RUNS+run,year,1] = 1                              # Pnob=1 ?          
	}
}



#  x is a 3 dimensional matrix; each row represents an individual trajectory referenced
#  to brood year and has dimension YEARS;  There are RUNS trajectories that represent 
#  Spawners;  and then  RUNS trajectories that represent Recruits; and then RUNS 
#  trajectories that represent Phos; and then RUNS trajectories that represent PNI.
#  The 3rd dimension of the array repesents an Alternative, and there are ALTS of those.

P <- array(0,ALTS*RUNS)  # Vector to store Productivity
A <- array(0,ALTS*RUNS)  # Vector to store Abundance
Phos <- array(0,ALTS*RUNS)  # Vector to store Phos
PNI <- array(0,ALTS*RUNS)  # Vector to store PNI
X.array <- array(0,ALTS*RUNS) # Vector to store Extinction (0 or 1)
RUNS
X.array
# First, go through all aternatives to create Response Surface
k <- 0
for (j in 1:ALTS){  #1:6
	for (i in 1:RUNS){ #1:500
		k <- k+1
            x[1,,1]
		
		N <- x[i,,j] # Abundance for iteration i in alt j
		A[k] <- mean(as.numeric(log(N[26:50]))) # Geomean spawners years 26-50
		
		R <- x[RUNS+i,,j] # Recruits for iteration i in alt j
	
		fit <- glm(log(R+.1) ~ log(N+.1)) # Gompertz fit for a single model iteration
		# Productivity is measure as productivity (via Gompertz model) at 
		# 50 spawners
		P[k] <- as.numeric(fit$coeff[1]) + log(50)*as.numeric(fit$coeff[2])
		# Determine whether iteration went extinct -- Did pop'n fall below mean QET
		# for 4 years in a row
		for (yr in 1:(YEARS-3)){
			S <- sum(N[yr:(yr+3)])
#            	      print(S)	
        	if (S < 4*QET) # if yes, mark X.vec[i] as 1, otherwise it stays as 0
				X.array[k] <- 1
#		print(X.array[k])

		Ph <- x[((2*RUNS)+i),,j]
		Phos[k] <- mean(Ph)  # mean Phos for the iteration
		
		Pn <- x[((3*RUNS)+i),,j]
		PNI[k] <- mean(Pn)  # mean PNI for the iteration
		}	
	}
}

X.array
YEARS
# Diagnostics
#print(cor(P,A))
#print(cor(P,X.array))
#print(cor(A, X.array))

# Fit Response Surface for Productivity and Abundance
fit <- glm(X.array ~ P + A, family = binomial)
#print(summary(fit))

# Ranges for Responce Surface Plot
x.min <- 0
x.max <- quantile(P, 0.90)
y.min <- 0
y.max <- quantile(A, 0.95)

X <- seq(x.min,x.max, 0.1)
LX <- length(X)
Y <- seq(y.min,y.max, 0.1)
LY <- length(Y)

#  Matrix to store predicted extinction probabiliites
Z <- matrix(0,LX,LY)
i <- 0
for(x in X){
	i <- i+1
	j <- 0
	for(y in Y){
		j <- j+1
		new.x <- data.frame(P = x, A = y)
		# Predict extinction probabilities under the model
		pred <- predict(fit, newdata=new.x, type = "link")
		# Anti-logit
		Z[i,j] <- as.numeric(exp(pred)/(1+exp(pred)))
	}
}

# Produce a plot of the contour surface
pdf("contourPlot.pdf", width = 6, height = 6)
par(oma=c(2,2,1,1))
contour(exp(X), exp(Y), Z, levels = c(0,0.01,0.05, 0.1,0.25,0.5,0.75,0.9,0.95, 0.99,1.0),
		xlab = "",
		ylab = "")
mtext("R/S at Low Abundance", side =1, line = 1, outer = T, cex = 1.0)
mtext("Mean Abundance", side = 2, line = 1, outer = T, cex = 1.0)
dev.off("contourPlot.pdf")

# Now produce a plot for each alternative with points representing individual iterations
pdf("RiskPlot.pdf", width = 7, height = 8.5)
#par(mfrow=c(3,2), mar=c(3,3,1,1), oma=c(6,4,1,1))
par(mfrow=c(1,1), mar=c(3,3,1,1), oma=c(6,4,1,1))


for(k in 1:ALTS){  # loop through alternatives
	contour(exp(X), exp(Y), Z, levels = c(0,0.01,0.05, 0.1,0.25,0.5,0.75,0.9,0.95, 0.99,1.0),
		xlab = "",
		ylab = "")
	
	#  Extract points from the kth alternative
	p <- P[((k-1)*RUNS+1):((k-1)*RUNS+RUNS)] 
	a <- A[((k-1)*RUNS+1):((k-1)*RUNS+RUNS)]
	x <- X.array[((k-1)*RUNS+1):((k-1)*RUNS+RUNS)]
	
	print(mean(x))
	points(exp(p),exp(a))
	points(median(exp(p)), median(exp(a)), col = "red", pch = 16, cex = 1.2)
	
	
}
mtext("R/S at Low Abundance", side =1, line = 1, outer = T, cex = 1.0)
mtext("Mean Abundance", side = 2, line = 1, outer = T, cex = 1.0)
dev.off()

# Convert Extinction probs to VSP score for P&A, and then barplot
pdf("VSP.plot.pdf", width = 9, height = 8.5)
#par(mfrow=c(3,2), mar=c(3,3,1,1), oma=c(6,4,1,1))
par(mfrow=c(1,1), mar=c(3,3,1,1), oma=c(6,4,1,1))

for(j in 1:ALTS){
	V <- array(0,RUNS)
	X.r <- array(0,RUNS)
	
	p <- P[((j-1)*RUNS+1):((j-1)*RUNS+RUNS)]
	a <- A[((j-1)*RUNS+1):((j-1)*RUNS+RUNS)]
	x <- X.array[((j-1)*RUNS+1):((j-1)*RUNS+RUNS)]
	
	for(r in 1:RUNS){
			new.x <- data.frame(P = p[r], A = a[r])
			pred <- predict(fit, newdata=new.x, type = "link")
			X.r[r] <- as.numeric(exp(pred)/(1+exp(pred)))
			
			if (X.r[r] <= 0.05) V[r] <- 4-20*X.r[r]
			if (X.r[r] > 0.05 && X.r[r] <= 0.25) V[r] <- 3.25-5*X.r[r]
			if (X.r[r] > 0.25 && X.r[r] <= 0.5) V[r] <- 3 -4*X.r[r]
			if (X.r[r] > 0.5) V[r] <- 2 - 2*X.r[r]
	}
	hist(V, breaks = seq(0.0,4.0,0.5),
		main="")
		
}
mtext("VSP (Productivity & Abundance)", side =1, line = 1, outer = T, cex = 1.0)
mtext("Frequency", side = 2, line = 1, outer = T, cex = 1.0)
dev.off()

# Barplot of Phos
pdf("Phos.plot.pdf", width = 9, height = 8.5)
#par(mfrow=c(3,2), mar=c(3,3,1,1), oma=c(6,4,1,1))
par(mfrow=c(1,1), mar=c(3,3,1,1), oma=c(6,4,1,1))

for(j in 1:ALTS){
	
	p <- Phos[((j-1)*RUNS+1):((j-1)*RUNS+RUNS)]
	hist(p, breaks <- seq(0,1.0,0.1),
		main = "")
}
mtext("Phos", side =1, line = 1, outer = T, cex = 1.0)
mtext("Frequency", side = 2, line = 1, outer = T, cex = 1.0)
dev.off()

# Barplot of PNI
pdf("PNI.plot.pdf", width = 9, height = 8.5)

par(mfrow=c(1,1), mar=c(3,3,1,1), oma=c(6,4,1,1))

for(j in 1:ALTS){
	
	p <- PNI[((j-1)*RUNS+1):((j-1)*RUNS+RUNS)]
	hist(p, breaks <- seq(0,1.0,0.1),
		main = "")
}
mtext("PNI", side =1, line = 1, outer = T, cex = 1.0)
mtext("Frequency", side = 2, line = 1, outer = T, cex = 1.0)
dev.off()
	
}

