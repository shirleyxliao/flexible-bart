#continuous p3 analysis
#### Paper 3 using Clas's data
library(bartMachine)
library(readr)

Crime_data <- read_csv("~/Downloads/Crime-data.csv")
#Crime_data <- read.csv("~/shared_space/ci3_analysis/zigler_lab/projects/Ozone_Aggression/Crime-data.csv")

#################
#glms
names(Crime_data) = c("a.assault","s.assault","larceny","auto.theft",
                      "date","max.oz","avg.pm","precip","temp.max","uv",
                      "holidays","fri","sat","sun","chicago","nyc","la","philly",
                      "phoenix","atlanta")
Crime_data$date2 = as.Date(Crime_data$date,"%m/%d/%Y")
Crime_data$max.oz = 100*Crime_data$max.oz

confound = c("date2","max.oz","avg.pm","precip","temp.max","uv",
             "holidays","fri","sat","sun")
confound2 = c("date2","max.oz","avg.pm","precip","temp.max","uv",
              "holidays","fri","sat","sun","chicago","nyc","la","philly",
              "phoenix","atlanta")

run.bart = function(data,crime,confounders){
  # fit tree on data
  X = data[,names(data)%in%confounders]
  Y = data[,names(data)==crime]
  
  tree = build_bart_machine(X=X,y=Y,use_missing_data = TRUE,
                                  use_missing_data_dummies_as_covars = TRUE)  
  ## predict counterfactuals
  high.X = X
  high.X$max.oz = X$max.oz + 1

  pred = bart_machine_get_posterior(tree,high.X)$y_hat_posterior_samples
  
  #put effects together
  v.effects = apply(pred,2,function(x){x-Y})
  v.means = apply(v.effects,2,mean)
  #v.sds = mean(apply(v.effects,1,sd))
  v.sds = sd(v.means)
  v.lower = quantile(v.means,p=0.025)
  v.upper = quantile(v.means,p=0.975)
  
  c(mean(v.means),v.sds,v.lower,v.upper)
  
}


##########################################

#all cities
assault = lm(a.assault~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+chicago+nyc+la+philly+phoenix+atlanta+max.oz,
             data=Crime_data)
summary(assault)
#max.oz       6.438e-01  6.964e-02   9.245  < 2e-16 ***
#max.oz       5.073199e+01  7.803279e+01

glm.assault = glm(a.assault~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+chicago+nyc+la+philly+phoenix+atlanta+max.oz,
                  family="poisson",data=Crime_data)
summary(glm.assault)
#max.oz       1.426e-02  1.272e-03  11.211  < 2e-16 *** .
#[1] 1.014362
#max.oz      1.011837e+00 1.016895e+00

run.bart(data=Crime_data,crime="a.assault",confounders = confound2)

#                               2.5%       97.5% 
# 0.30464986 0.07258154 0.15065460 0.43684114   

larceny = lm(larceny~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+chicago+nyc+la+philly+phoenix+atlanta+max.oz,
             ,data=Crime_data)
summary(larceny)
#max.oz      -4.376e+00  2.174e-01 -20.129  < 2e-16 ***

glm.larceny = glm(larceny~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+chicago+nyc+la+philly+phoenix+atlanta+max.oz,
                  family="poisson",data=Crime_data)
summary(glm.larceny)

#max.oz      -2.524e-02  5.453e-04  -46.292   <2e-16 ***

run.bart(data=Crime_data,crime="larceny",confounders = confound2)
#-2.6237430  0.2217753 -3.0627297 -2.2161670

#

auto.theft = glm(auto.theft~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+chicago+nyc+la+philly+phoenix+atlanta+max.oz,
                 data=Crime_data)
summary(auto.theft)
#max.oz      -5.607e-01  5.911e-02   -9.486  < 2e-16 *** 

glm.auto.theft = glm(auto.theft~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+chicago+nyc+la+philly+phoenix+atlanta+max.oz,
                     family="poisson",data=Crime_data)
summary(glm.auto.theft)
#max.oz      -1.897e-02  1.311e-03  -14.471  < 2e-16 ***

run.bart(data=Crime_data,crime="auto.theft",confounders = confound2)
#-0.43582277  0.08705613 -0.61142297 -0.27504921 

############################################
#chicago
data_chicago = subset(Crime_data,chicago==1)
chicago.assault = lm(a.assault~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                     data=data_chicago)
summary(chicago.assault)
#max.oz       1.066e+00  1.406e-01   7.577 4.06e-14 ***
#max.oz       7.899991e-01  1.341414e+00
#sig pos effect

glm.chicago.assault = glm(a.assault~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                          family="poisson",data=data_chicago)
summary(glm.chicago.assault)
#max.oz       8.334e-03  2.034e-03    4.097 4.18e-05 *** 
#1.008369
#max.oz      1.004357e+00 1.012397e+00
#non-sig pos effect

run.bart(data=data_chicago,crime="a.assault",confounders = confound)

#0.3138130 0.1425234 0.0282611 0.5938988  
#non-sig pos effect

#
chicago.larceny = lm(larceny~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                     data=data_chicago)
summary(chicago.larceny)
#max.oz      -6.063e+00  4.407e-01 -13.756  < 2e-16 ***  
#max.oz      -6.926703e+00 -5.198691e+00

glm.chicago.larceny = glm(larceny~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                          family="poisson",data=data_chicago)
summary(glm.chicago.larceny)
#max.oz      -3.077e-02  9.276e-04  -33.171  < 2e-16 *** 
#sig neg effect

run.bart(data=data_chicago,crime="larceny",confounders = confound)

#-3.4538708  0.5233334 -4.5602085 -2.4846217
#non-sig pos effect

#
chicago.auto.theft = lm(auto.theft~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                         data=data_chicago)
summary(chicago.auto.theft)
#max.oz      -7.206e-01  1.401e-01  -5.142 2.81e-07 ***
#max.oz      -9.953276e-01 -4.458723e-01
#non-sig pos effect

glm.chicago.auto.theft = glm(auto.theft~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                             family="poisson",data=data_chicago)
summary(glm.chicago.auto.theft)
#max.oz      -1.605e-02  1.958e-03   -8.195 2.50e-16 *** 
#non-sig neg effect

run.bart(data=data_chicago,crime="auto.theft",confounders = confound)
#-0.42482015  0.19546369 -0.80483930 -0.03692338
#non-sig neg effect

################################################

#nyc
data_nyc = subset(Crime_data,nyc==1)

nyc.assault = lm(a.assault~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                 data=data_nyc)
summary(nyc.assault)
#max.oz       6.065e-01  1.893e-01   3.204 0.001363 **
#max.oz       2.354295e-01  9.775339e-01
#non-sig neg effect

glm.nyc.assault = glm(a.assault~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                      family="poisson",data=data_nyc)
summary(glm.nyc.assault)
#max.oz       1.008e-02  2.277e-03   4.427 9.56e-06 ***
#[1] 1.010131
#max.oz      1.005634e+00 1.014651e+00

run.bart(data=data_nyc,crime="a.assault",confounders = confound)

#0.02512465  0.19734933 -0.33033307  0.43918466 
#non-sig pos effect

#
nyc.larceny = glm(larceny~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                  data=data_nyc)
summary(nyc.larceny)
#max.oz      -8.780e+00  6.504e-01 -13.498  < 2e-16 ***
#max.oz      -1.005447e+01 -7.504831e+00
#non-sig neg effect

glm.nyc.larceny = glm(larceny~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                      family="poisson",data=data_nyc)
summary(glm.nyc.larceny)
#max.oz      -2.612e-02  8.858e-04 -29.485  < 2e-16 *** 
#sig neg effect

run.bart(data=data_nyc,crime="larceny",confounders = confound)
#-3.4216375  0.8595181 -4.9415856 -1.6150659 
#non-sig pos

#
nyc.auto.theft = lm(auto.theft~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                    data=data_nyc)
summary(nyc.auto.theft)
#max.oz      -6.371e-01  1.010e-01  -6.311 3.06e-10 ***
#max.oz      -8.350242e-01 -4.391744e-01
#non-sig neg effect

glm.nyc.auto.theft = glm(auto.theft~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                         family="poisson",data=data_nyc)
summary(glm.nyc.auto.theft)
#max.oz      -2.409e-02  3.175e-03  -7.589 3.22e-14 ***
#non-sig neg effect

run.bart(data=data_nyc,crime="auto.theft",confounders = confound)
# -0.1232962  0.1219511 -0.3650647  0.1146264 
# non-sig neg effect

###############################################################

#la
data_la = subset(Crime_data,la==1)

la.assault = lm(a.assault~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                data=data_la)
summary(la.assault)
#max.oz       4.201e-01  1.276e-01   3.292 0.001005 **  
#max.oz       1.699217e-01  6.703316e-01

glm.la.assault = glm(a.assault~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                     family="poisson",data=data_la)
summary(glm.la.assault)
#max.oz       1.759e-02  4.701e-03   3.742 0.000182 ***
#[1] 1.017746
#max.oz      1.008415e+00 1.027171e+00
#non sig pos effect

run.bart(data=data_la,crime="a.assault",confounders = confound)
# 0.01211139  0.14005589 -0.25196356  0.30103998
# non sig pos effect

#
la.larceny = lm(larceny~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                data=data_la)
summary(la.larceny)
#max.oz      -4.842e-01  1.485e-01  -3.260  0.00113 **   
#max.oz      -7.755091e-01 -1.929885e-01
#non-sig neg effect

glm.la.larceny = glm(larceny~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                     family="poisson",data=data_la)
summary(glm.la.larceny)
#max.oz      -2.334e-02  5.042e-03  -4.630 3.66e-06 ***
#sig neg effect

run.bart(data=data_la,crime="larceny",confounders = confound)

#-0.06760719  0.12902252 -0.31052999  0.19767472
#non-sig neg effect

#
la.auto.theft = lm(auto.theft~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                   data=data_la)
summary(la.auto.theft)
#max.oz      -2.361e-01  7.064e-02  -3.343  0.00084 ***  
#max.oz      -3.746478e-01 -9.762573e-02
#non-sig neg effect

glm.la.auto.theft = glm(auto.theft~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                        family="poisson",data=data_la)
summary(glm.la.auto.theft)
#max.oz      -3.486e-02  8.840e-03  -3.943 8.03e-05 *** 
#non-sig neg effect

run.bart(data=data_la,crime="auto.theft",confounders = confound)
#-0.05344718  0.08816575 -0.23906415  0.09813750 
#non-sig neg effect

###############################################################
#philly
data_philly = subset(Crime_data,philly==1)

philly.assault = lm(a.assault~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                    data=data_philly)
summary(philly.assault)
#max.oz       1.075e-01  9.428e-02   1.140   0.2543
#max.oz      -7.736006e-02  2.923313e-01

glm.philly.assault = glm(a.assault~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                         family="poisson",data=data_philly)
summary(glm.philly.assault)
#max.oz       2.228e-03  3.182e-03   0.700  0.48378  
#[1] 1.00223
#max.oz      9.959998e-01 1.008501e+00

run.bart(data=data_philly,crime="a.assault",confounders = confound)
# 0.1615438  0.1292130 -0.1043159  0.4176418 
#non-sig pos effect

#

philly.larceny = lm(larceny~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                     data=data_philly)
summary(philly.larceny)
#max.oz      -4.165e+00  3.204e-01 -13.000  < 2e-16 *** 
#max.oz      -4.792956e+00 -3.536762e+00

glm.philly.larceny = glm(larceny~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                         family="poisson",data=data_philly)
summary(glm.philly.larceny)
#max.oz      -3.196e-02  1.342e-03 -23.812  < 2e-16 ***
#sig neg effect

run.bart(data=data_philly,crime="larceny",confounders = confound)
#-1.9464693  0.5092755 -2.8885434 -1.0093770 
#non-sig pos effect

#

philly.auto.theft = lm(auto.theft~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                       data=data_philly)
summary(philly.auto.theft)
#max.oz      -4.921e-01  1.414e-01  -3.480 0.000506 ***
#max.oz      -7.693380e-01 -2.149055e-01
#non-sig pos effect

glm.philly.auto.theft = glm(auto.theft~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                            family="poisson",data=data_philly)
summary(glm.philly.auto.theft)
#max.oz      -1.725e-02  2.627e-03  -6.565 5.21e-11 ***   
#sig pos effect

run.bart(data=data_philly,crime="auto.theft",confounders = confound)
#-0.23010973  0.15600053 -0.52576882  0.05948139 
#non-sig pos effect

#################################################

#phoenix
data_phoenix = subset(Crime_data,phoenix==1)

#
phoenix.assault = lm(a.assault~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                     data=data_phoenix)
summary(phoenix.assault)
#max.oz       5.280e-01  1.772e-01   2.980  0.00298 ** 
#max.oz       1.801504e-01 8.758786e-01

glm.phoenix.assault = glm(a.assault~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                          family="poisson",data=data_phoenix)
summary(glm.phoenix.assault)
#max.oz       4.057e-02  1.349e-02   3.008  0.00263 **
#[1] 1.041404
#max.oz       1.014261e+00  1.069324e+00

run.bart(data=data_phoenix,crime="a.assault",confounders = confound)
# -0.04138256  0.28472599 -0.61058731  0.49334948
#non-sig neg effect

#
phoenix.larceny = lm(larceny~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                     data=data_phoenix)
summary(phoenix.larceny)
#max.oz      -9.863e-01  5.795e-01  -1.702   0.0891    
#max.oz      -2.123851e+00  1.511598e-01

glm.phoenix.larceny = glm(larceny~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                          family="poisson",data=data_phoenix)
summary(glm.phoenix.larceny)
#max.oz      -2.344e-02  2.093e-03 -11.200  < 2e-16 ***   
#non-sig pos effect

run.bart(data=data_phoenix,crime="larceny",confounders = confound)
#-0.5797293  0.2258182 -1.0093339 -0.1198367 
#non-sig pos effect

#
phoenix.auto.theft = lm(auto.theft~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                        data=data_phoenix)
summary(phoenix.auto.theft)
#max.oz      -1.628e-01  7.396e-02  -2.201   0.0278 *
#max.oz      -8.794052e-01 -3.249634e-04

glm.phoenix.auto.theft = glm(auto.theft~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                             family="poisson",data=data_phoenix)
summary(glm.phoenix.auto.theft)
#max.oz      -1.419e-02  4.763e-03  -2.980  0.00288 ** 
#sig pos effect

run.bart(data=data_phoenix,crime="auto.theft",confounders = confound)
#-0.08574352  0.10051487 -0.28452349  0.09274324
#non-sig pos effect


#########################################################3

#atlanta
data_atlanta = subset(Crime_data,atlanta==1)

atlanta.assault = lm(a.assault~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                     data=data_atlanta)
summary(atlanta.assault)
#max.oz       4.038e-02  4.459e-02   0.906  0.36522 
#max.oz      -4.705119e-02  1.278192e-01

glm.atlanta.assault = glm(a.assault~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                          family="poisson",data=data_atlanta)
summary(glm.atlanta.assault)
#max.oz       5.467e-03  6.548e-03   0.835  0.40379 
#[1] 1.005482
#max.oz      9.926621e-01 1.018473e+00

run.bart(data=data_atlanta,crime="a.assault",confounders = confound)
# 0.05319763  0.06197024 -0.07124614  0.16555845 
#non-sig pos effect

#
atlanta.larceny = lm(larceny~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                     data=data_atlanta)
summary(atlanta.larceny)
#max.oz      -1.442e+00  1.971e-01  -7.318 3.12e-13 ***    
#max.oz      -1.828868e+00 -1.055968e+00

glm.atlanta.larceny = glm(larceny~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                          family="poisson",data=data_atlanta)
summary(glm.atlanta.larceny)
#max.oz      -2.344e-02  2.093e-03 -11.200  < 2e-16 ***   
#non-sig pos effect

run.bart(data=data_atlanta,crime="larceny",confounders = confound)
#-0.5797293  0.2258182 -1.0093339 -0.1198367 
#non-sig pos effect

#
atlanta.auto.theft = lm(auto.theft~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                        data=data_atlanta)
summary(atlanta.auto.theft)
#max.oz      -1.628e-01  7.396e-02  -2.201   0.0278 *
#max.oz      -3.077529e-01 -1.774856e-02

glm.atlanta.auto.theft = glm(auto.theft~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+max.oz,
                             family="poisson",data=data_atlanta)
summary(glm.atlanta.auto.theft)
#max.oz      -1.419e-02  4.763e-03  -2.980  0.00288 ** 
#sig pos effect

run.bart(data=data_atlanta,crime="auto.theft",confounders = confound)
#-0.08574352  0.10051487 -0.28452349  0.09274324
#non-sig pos effect

