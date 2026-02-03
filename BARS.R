.packageName <- "BARS"
#
# /**********************************************************************
#		
#			Rwrapper for BARSN v. 1.0
#
#
#
# This program is free software; you can distribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.
#  
# These functions are distributed in the hope that they will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# The text of the GNU General Public License, version 2, is available
# as http://www.gnu.org/copyleft or by writing to the Free Software
# Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
#
#
#
# Credits and Acknowledgements:
#
#   This Rwrapper is designed to work with Wallstrom and Kass's
#   Barslib, (C) 2003.
#
#   Barslib uses Hansen and Kooperberg's Logspline, (C) 1997, as the
#   default method for selecting initial knots.
#
#   Barslib uses Bates and Venables Routines for manipulating B-splines, (C)
#   1998. The routines were released under the same GNU General Public
#   License referred to above.
#
#
# ************************************************************************/


bars<-function(x,y,iknots=25,prior="uniform",
priorparam=c(1,60),burnin=200,sims=2000,tau=50.0,c=0.4,fits=TRUE,
peak=FALSE,conf=0.95,gridfit=FALSE,ngridpoints=150){
x<-as.double(x)
y<-as.double(y)
n<-length(x)
n<-as.integer(n)
if(length(y)!=n){
print("x and y are not the same length")
break}

burnin<-as.integer(burnin)
sims<-as.integer(sims)
iknots<-as.integer(iknots)
prior<-as.character(prior)
tau<-as.double(tau)
c<-as.double(c)
if(c>0.5 || c<=0){
print("Value of c must be greater than 0 and no greater than 0.5")
break}
ngridpoints<-as.integer(ngridpoints)

# writing data into file "bars_points"
filed<-file("bars_points","w")
for(i in 1:n){
cat(x[i],y[i],"\n",file=filed)
}
close(filed)

# writing parameters into file "bars_params"
f<-file("bars_params","w")
cat("SET burn-in_iterations = ",burnin,"\n",file=f)
cat("SET sample_iterations = ", sims,"\n",file=f)
cat("SET initial_number_of_knots = ",iknots,"\n",file=f)
cat("SET beta_iterations = 3\n","\n",file=f)
cat("SET beta_threshhold = -10.0\n","\n",file=f)
cat("SET proposal_parameter_tau = ",tau,"\n",file=f)
cat("SET reversible_jump_constant_c = ",c,"\n",file=f)
cat("SET confidence_level = ",conf,"\n",file=f)
cat("SET number_of_grid_points = ",ngridpoints,"\n",file=f)
cat("SET sampled_knots_file = samp_knots\n",file=f)
cat("SET sampled_fits_file = samp_fits\n",file=f)
cat("SET sampled_grid_file = samp_grid\n",file=f)
cat("SET sampled_params_file = samp_params\n",file=f)
cat("SET summary_fits_file = summ_fits\n",file=f)
cat("SET summary_grid_file = summ_grid\n",file=f)
cat("SET summary_params_file = summ_params\n",file=f)
cat("SET verbose = false\n",file=f)
close(f) 
z<-0

if(prior=="Poisson" || prior=="Pois" || prior=="poisson" || prior=="pois"){
priorparam<-as.double(priorparam)
f<-file("bars_params","a")
cat("SET prior_form = Poisson\n",file=f)
cat("SET Poisson_parameter_lambda = ",priorparam,"\n",file=f)
close(f)
z<-1}

if(prior=="Uniform" || prior=="Unif" || prior=="uniform" || prior=="unif"){
if(length(priorparam)!=2){
print("Uniform prior requires a vector of length 2 - a minimum number")
print("of knots and a maximum number of knots")
break}
upper<-priorparam[2]
lower<-priorparam[1]
if(lower>upper){
print("Lower bound is greater than upper bound for number of knots")
break}
if(lower<1){
print("Lower bound is less than 1 and has been reset to 1 knot.")
lower<-1}
upper<-as.integer(upper)
lower<-as.integer(lower)
f<-file("bars_params","a")
cat("SET prior_form = Uniform\n",file=f)
cat("SET Uniform_parameter_L = ",upper,"\n",file=f)
cat("SET Uniform_parameter_R = ",lower,"\n",file=f)
close(f)
z<-1}

if(prior=="User"||
prior=="user"||prior=="defined"||prior=="other"||prior=="Other"){
if(ncol(priorparam)!=2){
print("For user defined prior, priorparam must be of the form")
print("of an nx2 matrix, with the number of knots in the first")
print("column and the probability of obtaining that number of knots")
print("in the second column.")
break}
pknots<-as.integer(priorparam[,1])
probknots<-as.double(priorparam[,2])
knotlength<-as.integer(length(pknots))

# writing user-defined prior info into prior_file
filed2<-file("prior_file","w")
for(i in 1:knotlength){
cat(pknots[i],probknots[i],"\n",file=filed2)
}
close(filed2)

# updating bars_params to reflect user-defined prior
f<-file("bars_params","a")
cat("SET prior_form = User\n")
close(f)

z<-2}

if(z==0){
print("Prior must be Poisson, uniform, or user-defined.")
break}

# running program
# pkgpath<-.path.package("BARS")
# cmdpath<-file.path(pkgpath,'exec/barsN.out')


if(z==1) cmd<-paste('barsN.exe', "bars_points", "bars_params", sep=" ")
if(z==2) cmd<-paste('barsN.exe', "bars_points", "bars_params", "prior_file", sep=" ")

system(cmd)

# reading output of program back into R
v<-scan("samp_params")
if(length(v)==0){
	print("Program failed to generate correct knots.")
	print("Problem is likely due to poor constraints on prior.")
	print("Try again with smaller lower bound on prior.")
break}

sumfits<-as.matrix(read.table("summ_fits"))
sampmeans<-as.matrix(read.table("samp_params"))
summar<-as.matrix(read.table("summ_params"))
if(gridfit==TRUE){
	gridfits<-as.matrix(read.table("summ_grid"))}
if(fits==TRUE){
	sampfit<-as.matrix(read.table("samp_fits"))}
lengths<-count.fields("samp_knots")
knots1<-scan("samp_knots")
knots<-matrix(NA,ncol=max(lengths),nrow=length(lengths))
index<-c(0,cumsum(lengths))
for(i in 1:length(lengths)){
knots[i,1:lengths[i]]<-knots1[ (index[i]+1):index[(i+1)] ]
}

if(gridfit==FALSE){
if(fits==TRUE){
if(peak==TRUE){
g<-list(sampfits=sampfit,postmeans=(sumfits[2,]),
	postmodes=(sumfits[3,]),
	sims=sampmeans[,1],no.knots=sampmeans[,6],sampknots=knots,
	sampBICs=sampmeans[,2],
	sampllikes=sampmeans[,3],samplpeaks=sampmeans[,4],
	samphpeaks=(sampmeans[,5]),
	peaklocationquantile=summar[1,1:2],
	peaklocationmean=summar[1,3],peaklocationmode=summar[1,4],
	peakheightquantile=(summar[2,1:2]),
	peakheightmean=(summar[2,3]),
	peakheightmode=(summar[2,4]))}
if(peak==FALSE){
g<-list(sampfits=sampfit,postmeans=(sumfits[2,]),
	postmodes=(sumfits[3,]),
	sims=sampmeans[,1],no.knots=sampmeans[,6],sampknots=knots,
	sampBICs=sampmeans[,2],
	sampllikes=sampmeans[,3])
}
}

if(fits==FALSE){
if(peak==TRUE){
g<-list(postmeans=(sumfits[2,]),
	postmodes=(sumfits[3,]),
	sims=sampmeans[,1],no.knots=sampmeans[,6],sampknots=knots,
	sampBICs=sampmeans[,2],
	sampllikes=sampmeans[,3],samplpeaks=sampmeans[,4],
	samphpeaks=(sampmeans[,5]),
	peaklocationquantile=summar[1,1:2],
	peaklocationmean=summar[1,3],peaklocationmode=summar[1,4],
	peakheightquantile=(summar[2,1:2]),
	peakheightmean=(summar[2,3]),
	peakheightmode=(summar[2,4]))}
if(peak==FALSE){
g<-list(postmeans=(sumfits[2,]),
	postmodes=(sumfits[3,]),
	sims=sampmeans[,1],no.knots=sampmeans[,6],sampknots=knots,
	sampBICs=sampmeans[,2],
	sampllikes=sampmeans[,3])
}
}
}

if(gridfit==TRUE){
if(fits==TRUE){
if(peak==TRUE){
g<-list(sampfits=sampfit,postmeans=(sumfits[2,]),
	postmodes=(sumfits[3,]),gridmeans=gridfits[2,],gridmodes=gridfits[3,],
	simnum=sampmeans[,1],nknots=sampmeans[,6],sampknots=knots,
	sampBICs=sampmeans[,2],
	sampllikes=sampmeans[,3],samplpeaks=sampmeans[,4],
	samphpeaks=(sampmeans[,5]),
	peaklocationquantile=summar[1,1:2],
	peaklocationmean=summar[1,3],peaklocationmode=summar[1,4],
	peakheightquantile=(summar[2,1:2]),
	peakheightmean=(summar[2,3]),
	peakheightmode=(summar[2,4]))}
if(peak==FALSE){
g<-list(sampfits=sampfit,postmeans=(sumfits[2,]),
	postmodes=(sumfits[3,]),gridmeans=gridfits[2,],gridmodes=gridfits[3,],
	simnum=sampmeans[,1],nknots=sampmeans[,6],sampknots=knots,
	sampBICs=sampmeans[,2],
	sampllikes=sampmeans[,3])
}
}

if(fits==FALSE){
if(peak==TRUE){
g<-list(postmeans=(sumfits[2,]),
	postmodes=(sumfits[3,]),
	simnum=sampmeans[,1],nknots=sampmeans[,6],sampknots=knots,
	gridmeans=gridfits[2,],gridmodes=gridfits[3,],
	sampBICs=sampmeans[,2],
	sampllikes=sampmeans[,3],samplpeaks=sampmeans[,4],
	samphpeaks=(sampmeans[,5]),
	peaklocationquantile=summar[1,1:2],
	peaklocationmean=summar[1,3],peaklocationmode=summar[1,4],
	peakheightquantile=(summar[2,1:2]),
	peakheightmean=(summar[2,3]),
	peakheightmode=(summar[2,4]))}
if(peak==FALSE){
g<-list(postmeans=(sumfits[2,]),
	postmodes=(sumfits[3,]),
	gridmeans=gridfits[2,],gridmodes=gridfits[3,],
	simnum=sampmeans[,1],nknots=sampmeans[,6],sampknots=knots,
	sampBICs=sampmeans[,2],
	sampllikes=sampmeans[,3])
}
}	
}	

return(g)
}
# /**********************************************************************
#		
#			Rwrapper for BARSP v. 1.0
#
#
#
# This program is free software; you can distribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.
#  
# These functions are distributed in the hope that they will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# The text of the GNU General Public License, version 2, is available
# as http://www.gnu.org/copyleft or by writing to the Free Software
# Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
#
#
#
# Credits and Acknowledgements:
#
#   This Rwrapper is designed to work with Wallstrom and Kass's
#   Barslib, (C) 2003.
#
#   Barslib uses Hansen and Kooperberg's Logspline, (C) 1997, as the
#   default method for selecting initial knots.
#
#   Barslib uses Bates and Venables Routines for manipulating B-splines, (C)
#   1998. The routines were released under the same GNU General Public
#   License referred to above.
#
#
# ************************************************************************/

barsP<-function(x,y,initial="logspline",iknots=3,prior="uniform",
priorparam=c(1,60),burnin=200,nsims=2000,tau=50.0,c=0.4,fits=TRUE,
peak=FALSE,conf=0.95,gridfit=FALSE,ngridpoints=150){
x<-as.double(x)
y<-as.integer(y)
trials<-1
n<-length(x)
n<-as.integer(n)
if(length(y)!=n){
print("x and y are not the same length")
break}
burnin<-as.integer(burnin)
nsims<-as.integer(nsims)
iknots<-as.integer(iknots)
prior<-as.character(prior)
tau<-as.double(tau)
c<-as.double(c)
if(c>0.5 || c<=0){
print("Value of c must be greater than 0 and no greater than 0.5")
break}
ngridpoints<-as.integer(ngridpoints)
initial<-as.character(initial)

# writing data into file "bars_points"
filed<-file("bars_points","w")
cat(n, trials,"\n",file=filed)
for(i in 1:n){
cat(x[i],y[i],"\n",file=filed)
}
close(filed)

# writing parameters into file "bars_params"
f<-file("bars_params","w")
cat("SET burn-in_iterations = ",burnin,"\n",file=f)
cat("SET sample_iterations = ", nsims,"\n",file=f)
cat("SET initial_number_of_knots = ",iknots,"\n",file=f)
cat("SET beta_iterations = 3\n","\n",file=f)
cat("SET beta_threshhold = -10.0\n","\n",file=f)
cat("SET proposal_parameter_tau = ",tau,"\n",file=f)
cat("SET reversible_jump_constant_c = ",c,"\n",file=f)
cat("SET confidence_level = ",conf,"\n",file=f)
cat("SET number_of_grid_points = ",ngridpoints,"\n",file=f)
cat("SET sampled_knots_file = samp_knots\n",file=f)
cat("SET sampled_fits_file = samp_fits\n",file=f)
cat("SET sampled_grid_file = samp_grid\n",file=f)
cat("SET sampled_params_file = samp_params\n",file=f)
cat("SET summary_fits_file = summ_fits\n",file=f)
cat("SET summary_grid_file = summ_grid\n",file=f)
cat("SET summary_params_file = summ_params\n",file=f)
close(f) 
z<-0

if(prior=="Poisson" || prior=="Pois" || prior=="poisson" || prior=="pois"){
priorparam<-as.double(priorparam)
f<-file("bars_params","a")
cat("SET prior_form = Poisson\n",file=f)
cat("SET Poisson_parameter_lambda = ",priorparam,"\n",file=f)
close(f)
z<-1}

if(prior=="Uniform" || prior=="Unif" || prior=="uniform" || prior=="unif"){
if(length(priorparam)!=2){
print("Uniform prior requires a vector of length 2 - a minimum number")
print("of knots and a maximum number of knots")
break}
upper<-priorparam[2]
lower<-priorparam[1]
if(lower>upper){
print("Lower bound is greater than upper bound for number of knots")
break}
if(lower<1){
print("Lower bound is less than 1 and has been reset to 1 knot.")
lower<-1}
upper<-as.integer(upper)
lower<-as.integer(lower)
f<-file("bars_params","a")
cat("SET prior_form = Uniform\n",file=f)
cat("SET Uniform_parameter_U = ",upper,"\n",file=f)
cat("SET Uniform_parameter_L = ",lower,"\n",file=f)
close(f)
z<-1}

if(prior=="User"||
prior=="user"||prior=="defined"||prior=="other"||prior=="Other"){
if(ncol(priorparam)!=2){
print("For user defined prior, priorparam must be of the form")
print("of an nx2 matrix, with the number of knots in the first")
print("column and the probability of obtaining that number of knots")
print("in the second column.")
break}
pknots<-as.integer(priorparam[,1])
probknots<-as.double(priorparam[,2])
knotlength<-as.integer(length(pknots))

# writing user-defined prior info into prior_file
filed2<-file("prior_file","w")
for(i in 1:knotlength){
cat(pknots[i],probknots[i],"\n",file=filed2)
}
close(filed2)

# updating bars_params to reflect user-defined prior
f<-file("bars_params","a")
cat("SET prior_form = User\n")
close(f)
z<-2}

if(z==0){
print("Prior must be Poisson, uniform, or user-defined.")
break}

# updating bars_params to reflect whether logspline or evenly spaced knots
if(initial=="Logspline" || initial=="logspline" || initial=="Log" ||
initial=="log"){
f<-file("bars_params","a")
cat("SET Use_Logspline = true\n",file=f)
cat("SET verbose = false\n",file=f)
close(f)
}
if(initial=="Even" || initial=="even" || initial=="Evenly" ||
initial=="evenly"  || initial=="equal" || initial=="Equal" ||
initial=="Equally" || initial=="equally"){
f<-file("bars_params","a")
cat("SET Use_Logspline = false\n",file=f)
cat("SET verbose = false\n",file=f)
close(f)
}

# running program
pkgpath<-.path.package("BARS")
cmdpath<-file.path(pkgpath,'exec/barsP.out')

if (z==1) cmd<-paste(cmdpath, "bars_points", "bars_params", sep=" ")
if (z==2) cmd<-paste(cmdpath, "bars_points", "bars_params",
			      "prior_file", sep=" ")

system(cmd)

# reading output of program back into R
v<-scan("samp_params")
if(length(v)==0){
	print("Program failed to generate correct knots.")
	print("Problem is likely due to poor constraints on prior.")
	print("Try again with smaller lower bound on prior.")
break}

sumfits<-as.matrix(read.table("summ_fits"))
sampmeans<-as.matrix(read.table("samp_params"))
summar<-as.matrix(read.table("summ_params"))
if(gridfit==TRUE){
	gridfits<-as.matrix(read.table("summ_grid"))}
if(fits==TRUE){
	sampfit<-as.matrix(read.table("samp_fits"))}
lengths<-count.fields("samp_knots")
knots1<-scan("samp_knots")
knots<-matrix(NA,ncol=max(lengths),nrow=length(lengths))
index<-c(0,cumsum(lengths))
for(i in 1:length(lengths)){
knots[i,1:lengths[i]]<-knots1[ (index[i]+1):index[(i+1)] ]
}

if(gridfit==FALSE){
if(fits==TRUE){
if(peak==TRUE){
g<-list(sampfits=sampfit,postmeans=(sumfits[2,]),
	postmodes=(sumfits[3,]),
	simnum=sampmeans[,1],nknots=sampmeans[,6],sampknots=knots,
	sampBICs=sampmeans[,2],
	sampllikes=sampmeans[,3],samplpeaks=sampmeans[,4],
	samphpeaks=(sampmeans[,5]),
	peaklocationquantile=summar[1,1:2],
	peaklocationmean=summar[1,3],peaklocationmode=summar[1,4],
	peakheightquantile=(summar[2,1:2]),
	peakheightmean=(summar[2,3]),
	peakheightmode=(summar[2,4]))}
if(peak==FALSE){
g<-list(sampfits=sampfit,postmeans=(sumfits[2,]),
	postmodes=(sumfits[3,]),
	simnum=sampmeans[,1],nknots=sampmeans[,6],sampknots=knots,
	sampBICs=sampmeans[,2],
	sampllikes=sampmeans[,3])
}
}

if(fits==FALSE){
if(peak==TRUE){
g<-list(postmeans=(sumfits[2,]),
	postmodes=(sumfits[3,]),
	simnum=sampmeans[,1],nknots=sampmeans[,6],sampknots=knots,
	sampBICs=sampmeans[,2],
	sampllikes=sampmeans[,3],samplpeaks=sampmeans[,4],
	samphpeaks=(sampmeans[,5]),
	peaklocationquantile=summar[1,1:2],
	peaklocationmean=summar[1,3],peaklocationmode=summar[1,4],
	peakheightquantile=(summar[2,1:2]),
	peakheightmean=(summar[2,3]),
	peakheightmode=(summar[2,4]))}
if(peak==FALSE){
g<-list(postmeans=(sumfits[2,]),
	postmodes=(sumfits[3,]),
	simnum=sampmeans[,1],nknots=sampmeans[,6],sampknots=knots,
	sampBICs=sampmeans[,2],
	sampllikes=sampmeans[,3])
}
}
}

if(gridfit==TRUE){
if(fits==TRUE){
if(peak==TRUE){
g<-list(sampfits=sampfit,postmeans=(sumfits[2,]),
	postmodes=(sumfits[3,]),gridmeans=gridfits[2,],gridmodes=gridfits[3,],
	simnum=sampmeans[,1],nknots=sampmeans[,6],sampknots=knots,
	sampBICs=sampmeans[,2],
	sampllikes=sampmeans[,3],samplpeaks=sampmeans[,4],
	samphpeaks=(sampmeans[,5]),
	peaklocationquantile=summar[1,1:2],
	peaklocationmean=summar[1,3],peaklocationmode=summar[1,4],
	peakheightquantile=(summar[2,1:2]),
	peakheightmean=(summar[2,3]),
	peakheightmode=(summar[2,4]))}
if(peak==FALSE){
g<-list(sampfits=sampfit,postmeans=(sumfits[2,]),
	postmodes=(sumfits[3,]),gridmeans=gridfits[2,],gridmodes=gridfits[3,],
	simnum=sampmeans[,1],nknots=sampmeans[,6],sampknots=knots,
	sampBICs=sampmeans[,2],
	sampllikes=sampmeans[,3])
}
}

if(fits==FALSE){
if(peak==TRUE){
g<-list(postmeans=(sumfits[2,]),
	postmodes=(sumfits[3,]),
	simnum=sampmeans[,1],nknots=sampmeans[,6],sampknots=knots,
	gridmeans=gridfits[2,],gridmodes=gridfits[3,],
	sampBICs=sampmeans[,2],
	sampllikes=sampmeans[,3],samplpeaks=sampmeans[,4],
	samphpeaks=(sampmeans[,5]),
	peaklocationquantile=summar[1,1:2],
	peaklocationmean=summar[1,3],peaklocationmode=summar[1,4],
	peakheightquantile=(summar[2,1:2]),
	peakheightmean=(summar[2,3]),
	peakheightmode=(summar[2,4]))}
if(peak==FALSE){
g<-list(postmeans=(sumfits[2,]),
	postmodes=(sumfits[3,]),
	gridmeans=gridfits[2,],gridmodes=gridfits[3,],
	simnum=sampmeans[,1],nknots=sampmeans[,6],sampknots=knots,
	sampBICs=sampmeans[,2],
	sampllikes=sampmeans[,3])
}
}
}

return(g)
}
