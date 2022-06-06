#### Paper 3 using Clas's data
library(bartMachine)
library(readr)

Crime_data <- read_csv("~/Downloads/Crime-data.csv")
#Crime_data <- read.csv("~/shared_space/ci3_analysis/zigler_lab/projects/Ozone_Aggression/Crime-data.csv")

################
# dichotomize treatment 

Crime_data$dicho_oz = as.numeric(Crime_data$O3>0.07)

#################
#glms
names(Crime_data) = c("a.assault","s.assault","larceny","auto.theft",
                      "date","max.oz","avg.pm","precip","temp.max","uv",
                      "holidays","fri","sat","sun","chicago","nyc","la","philly",
                      "phoenix","atlanta","dicho.oz")
Crime_data$date2 = as.Date(Crime_data$date,"%m/%d/%Y")
confound = c("date2","max.oz","avg.pm","precip","temp.max","uv","cloud",
             "holidays","fri","sat","sun")
confound2 = c("date2","max.oz","avg.pm","precip","temp.max","uv","cloud",
              "holidays","fri","sat","sun","chicago","nyc","la","philly",
              "phoenix","atlanta")

run.bart = function(data,crime,confounders){
  # high ozone trees
  high.data = subset(data,dicho.oz==1)
  high.v.y = high.data[,names(high.data)==crime]
  
  high.X = high.data[,names(high.data)%in%confounders]
  
  high.v.tree = build_bart_machine(X=high.X,y=high.v.y,use_missing_data = TRUE,
                                   use_missing_data_dummies_as_covars = TRUE)
  
  low.data = subset(data,dicho.oz==0)
  low.v.y = low.data[,names(low.data)==crime]
  
  low.X = low.data[,names(low.data)%in%confounders]
  
  low.v.tree = build_bart_machine(X=low.X,y=low.v.y,use_missing_data = TRUE,
                                  use_missing_data_dummies_as_covars = TRUE)  
  ## predict counterfactuals
  
  #predict high ozone outcome x 2000 times
  
  pred.high.v = bart_machine_get_posterior(high.v.tree,low.X)$y_hat_posterior_samples
  
  #predict low ozone outcome
  pred.low.v = bart_machine_get_posterior(low.v.tree,high.X)$y_hat_posterior_samples
  
  #put effects together
  n = length(high.v.y)+length(low.v.y)
  v.effects = matrix(NA,nrow=n,ncol=1000)
  
  for(i in 1:1000){
    all.high.v = c(high.v.y,pred.high.v[,i])
    all.low.v = c(pred.low.v[,i],low.v.y)
    v.effects[,i] = all.high.v - all.low.v
  }
  
  v.means = apply(v.effects,2,mean)
  #v.sds = mean(apply(v.effects,1,sd))
  v.sds = sd(v.means)
  v.lower = quantile(v.means,p=0.025)
  v.upper = quantile(v.means,p=0.975)

c(mean(v.means),v.sds,v.lower,v.upper)
  
}


##########################################

#all cities
assault = lm(a.assault~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+chicago+nyc+la+philly+phoenix+atlanta+dicho.oz,
            data=Crime_data)
summary(assault)
#dicho.oz    -3.213e-01  5.311e-01  -0.605    0.545
#dicho.oz    -1.362257e+00  7.196536e-01

glm.assault = glm(a.assault~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+chicago+nyc+la+philly+phoenix+atlanta+dicho.oz,
              family="poisson",data=Crime_data)
summary(glm.assault)
#dicho.oz    -1.671e-02  9.070e-03  -1.842   0.0655 .
#[1] 0.9834288
#dicho.oz    9.660623e-01 1.001028e+00

run.bart(data=Crime_data,crime="a.assault",confounders = confound2)

#                               2.5%       97.5% 
# 7.529237  2.053647  3.321703 11.630257   

larceny = lm(larceny~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+chicago+nyc+la+philly+phoenix+atlanta+dicho.oz,
                  ,data=Crime_data)
summary(larceny)
#dicho.oz    -4.154e+00  1.670e+00  -2.488   0.0129 *

glm.larceny = glm(larceny~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+chicago+nyc+la+philly+phoenix+atlanta+dicho.oz,
              family="poisson",data=Crime_data)
summary(glm.larceny)

#dicho.oz    -1.250e-02  4.112e-03   -3.039  0.00237 ** #sig neg effect

run.bart(data=Crime_data,crime="larceny",confounders = confound2)
#11.6907789  6.4202789  0.4446488 24.6576343

#

auto.theft = glm(auto.theft~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+chicago+nyc+la+philly+phoenix+atlanta+dicho.oz,
                     data=Crime_data)
summary(auto.theft)
#dicho.oz     2.202e-01  4.508e-01    0.488   0.6252 

glm.auto.theft = glm(auto.theft~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+chicago+nyc+la+philly+phoenix+atlanta+dicho.oz,
                     family="poisson",data=Crime_data)
summary(glm.auto.theft)
#dicho.oz     7.083e-03  9.662e-03    0.733 0.463526 $non-sig pos effect

run.bart(data=Crime_data,crime="auto.theft",confounders = confound2)
#2.568257  2.798280 -2.062918  8.748001 

############################################
#chicago
data_chicago = subset(Crime_data,chicago==1)
chicago.assault = lm(a.assault~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                          data=data_chicago)
summary(chicago.assault)
#dicho.oz     3.855e+00  1.122e+00   3.437 0.000593 ***
#dicho.oz     1.656153e+00  6.053967e+00

glm.chicago.assault = glm(a.assault~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                  family="poisson",data=data_chicago)
summary(glm.chicago.assault)
#dicho.oz     8.352e-03  1.390e-02    0.601    0.548 
#1.008387
#dicho.oz    9.812000e-01 1.036130e+00

run.bart(data=data_chicago,crime="a.assault",confounders = confound)

#14.625961  5.701216  3.716225 25.218489 
#non-sig pos effect

#
chicago.larceny = lm(larceny~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                          data=data_chicago)
summary(chicago.larceny)
#dicho.oz    -9.587e-01  3.557e+00  -0.270    0.788  
#dicho.oz    -7.931279e+00  6.013852e+00
#non-sig neg effect

glm.chicago.larceny = glm(larceny~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                          family="poisson",data=data_chicago)
summary(glm.chicago.larceny)
#dicho.oz    -1.697e-02  6.936e-03   -2.446   0.0144 *  


run.bart(data=data_chicago,crime="larceny",confounders = confound)

# 13.51134  17.69995 -22.11024  51.18172 
#non-sig pos effect

#
chicago.auto.theft = lm(auto.theft~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                             data=data_chicago)
summary(chicago.auto.theft)
#dicho.oz     3.320e-02  1.116e+00   0.030   0.9763
#dicho.oz    -2.154416e+00  2.220818e+00

glm.chicago.auto.theft = glm(auto.theft~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                          family="poisson",data=data_chicago)
summary(glm.chicago.auto.theft)
#dicho.oz    -1.133e-02  1.490e-02   -0.761   0.4469   
#non-sig neg effect

run.bart(data=data_chicago,crime="auto.theft",confounders = confound)
# 0.8871138  5.2573273 -9.6882582 10.8416388
#non-sig neg effect

################################################

#nyc
data_nyc = subset(Crime_data,nyc==1)

nyc.assault = lm(a.assault~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                      data=data_nyc)
summary(nyc.assault)
#dicho.oz    -2.842e+00  1.536e+00  -1.850   0.0643 .  
#dicho.oz    -5.853386e+00  1.693889e-01

glm.nyc.assault = glm(a.assault~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                          family="poisson",data=data_nyc)
summary(glm.nyc.assault)
#dicho.oz    -5.485e-02  1.748e-02  -3.138  0.00170 ** 
#[1] 0.9466271
#dicho.oz    9.146061e-01 9.794762e-01

run.bart(data=data_nyc,crime="a.assault",confounders = confound)

# 6.847674  4.624099 -1.849772 15.406755
#non-sig pos effect

#
nyc.larceny = glm(larceny~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                      data=data_nyc)
summary(nyc.larceny)
#dicho.oz    -6.655e+00  5.385e+00  -1.236  0.21653 
#dicho.oz    -1.720910e+01  3.898282e+00
#non-sig neg effect

glm.nyc.larceny = glm(larceny~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                          family="poisson",data=data_nyc)
summary(glm.nyc.larceny)
#dicho.oz    -2.053e-02  6.996e-03  -2.935  0.00334 ** 

#sig neg effect

run.bart(data=data_nyc,crime="larceny",confounders = confound)
#34.335368 15.535424  5.610472 65.403855
#non-sig pos

#
nyc.auto.theft = lm(auto.theft~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                         data=data_nyc)
summary(nyc.auto.theft)
#dicho.oz    -1.213e+00  8.223e-01  -1.475  0.14026 
#dicho.oz    -2.825017e+00  3.991334e-01
#non-sig neg effect

glm.nyc.auto.theft = glm(auto.theft~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                             family="poisson",data=data_nyc)
summary(glm.nyc.auto.theft)
#dicho.oz    -3.941e-02  2.518e-02  -1.565 0.117597 
#non-sig neg effect

run.bart(data=data_nyc,crime="auto.theft",confounders = confound)
# -0.6631583  2.7388409 -5.6235359  4.7930293 
# non-sig neg effect

###############################################################

#la
data_la = subset(Crime_data,la==1)

la.assault = lm(a.assault~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                     data=data_la)
summary(la.assault)
#dicho.oz     7.178e-01  9.192e-01   0.781 0.434956  
#dicho.oz    -1.084616e+00  2.520144e+00

glm.la.assault = glm(a.assault~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                      family="poisson",data=data_la)
summary(glm.la.assault)
#dicho.oz     2.394e-02  3.202e-02   0.748  0.45475 
#[1] 1.024229
#dicho.oz    9.613516e-01 1.089948e+00

run.bart(data=data_la,crime="a.assault",confounders = confound)
# 3.8142170 1.7584501 0.2881058 7.2996768 
# non sig pos effect

#
la.larceny = lm(larceny~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                     data=data_la)
summary(la.larceny)
#dicho.oz    -1.458e+00  1.070e+00  -1.363  0.17310  
#dicho.oz    -3.555307e+00  6.398821e-01

glm.la.larceny = glm(larceny~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                      family="poisson",data=data_la)
summary(glm.la.larceny)
#dicho.oz    -8.439e-02  3.949e-02  -2.137 0.032583 * 
#sig neg effect

run.bart(data=data_la,crime="larceny",confounders = confound)

#-3.068789  2.277505 -7.969551  1.249312
#non-sig neg effect

#
la.auto.theft = lm(auto.theft~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                        data=data_la)
summary(la.auto.theft)
#dicho.oz    -6.888e-01  5.088e-01  -1.354  0.17592  
#dicho.oz    -1.686404e+00  3.088469e-01

glm.la.auto.theft = glm(auto.theft~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                         family="poisson",data=data_la)
summary(glm.la.auto.theft)
#dicho.oz    -9.921e-02  6.412e-02  -1.547  0.12180  
#non-sig neg effect

run.bart(data=data_la,crime="auto.theft",confounders = confound)
#-0.1382788  1.0550707 -2.1467350  1.9344022 
#non-sig neg effect

###############################################################
#philly
data_philly = subset(Crime_data,philly==1)

philly.assault = lm(a.assault~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                         data=data_philly)
summary(philly.assault)
#dicho.oz    -2.042e+00  6.948e-01  -2.940  0.00330 ** 
#dicho.oz    -3.404569e+00 -6.803052e-01
#sig neg effect

glm.philly.assault = glm(a.assault~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                     family="poisson",data=data_philly)
summary(glm.philly.assault)
#dicho.oz    -8.182e-02  2.250e-02  -3.637 0.000276 ***
#[1] 0.9214378
#dicho.oz    8.814690e-01 9.627469e-01

run.bart(data=data_philly,crime="a.assault",confounders = confound)
#  3.329270  2.440768 -1.450402  8.142573
#non-sig pos effect

#

philly.larceny = lm(larceny~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                         data=data_philly)
summary(philly.larceny)
#dicho.oz    -4.649e+00  2.408e+00  -1.931   0.0536 .
#dicho.oz    -9.368260e+00  7.087551e-02

glm.philly.larceny = glm(larceny~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                     family="poisson",data=data_philly)
summary(glm.philly.larceny)
#dicho.oz    -3.448e-02  9.625e-03  -3.583  0.00034 ***
#sig neg effect

run.bart(data=data_philly,crime="larceny",confounders = confound)
#  2.347810   7.451026 -12.873609  17.557702 
#non-sig pos effect

#

philly.auto.theft = lm(auto.theft~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                            data=data_philly)
summary(philly.auto.theft)
#dicho.oz     1.913e+00  1.044e+00   1.832  0.06697 . 
#non-sig pos effect
#dicho.oz    -1.338124e-01  3.959490e+00

glm.philly.auto.theft = glm(auto.theft~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                        family="poisson",data=data_philly)
summary(glm.philly.auto.theft)
#dicho.oz     4.251e-02  1.812e-02   2.346   0.0190 *   
#sig pos effect

run.bart(data=data_philly,crime="auto.theft",confounders = confound)
# 3.351907  3.848718 -3.984712 11.051042 
#non-sig pos effect

#################################################

#phoenix
data_phoenix = subset(Crime_data,phoenix==1)

#
phoenix.assault = lm(a.assault~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                          data=data_phoenix)
summary(phoenix.assault)
#dicho.oz    -2.161e+00  1.141e+00  -1.894   0.0586 .  
#dicho.oz    -4.401324e+00 7.889895e-02

glm.phoenix.assault = glm(a.assault~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                         family="poisson",data=data_phoenix)
summary(glm.phoenix.assault)
#dicho.oz    -1.728e-01  9.087e-02  -1.902   0.0572 .
#[1] 0.8413059
#dicho.oz     7.004874e-01  1.000516e+00

run.bart(data=data_phoenix,crime="a.assault",confounders = confound)
#-1.0335801  1.0665222 -3.2235975  0.9672345 
#non-sig neg effect

#

phoenix.larceny = lm(larceny~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                          data=data_phoenix)
summary(phoenix.larceny)
#dicho.oz     5.973e+00  3.720e+00   1.606  0.10870  
#dicho.oz    -1.328420e+00  1.327529e+01

glm.phoenix.larceny = glm(larceny~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                         family="poisson",data=data_phoenix)
summary(glm.phoenix.larceny)
#dicho.oz     6.660e-02  3.209e-02   2.075 0.037950 *  


run.bart(data=data_phoenix,crime="larceny",confounders = confound)
# 5.331983  3.603540 -1.621486 12.493348 
#non-sig pos effect

#
phoenix.auto.theft = lm(auto.theft~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                             data=data_phoenix)
summary(phoenix.auto.theft)
#dicho.oz    -3.541e+00  1.435e+00  -2.467  0.01382 * 
#dicho.oz    -6.357587e+00 -7.238085e-01

glm.phoenix.auto.theft = glm(auto.theft~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                            family="poisson",data=data_phoenix)
summary(glm.phoenix.auto.theft)
#dicho.oz    -1.961e-01  7.594e-02  -2.582  0.00981 **   
#sig neg effect

run.bart(data=data_phoenix,crime="auto.theft",confounders = confound)
#-2.9290467  1.1091477 -5.1280815 -0.7124537 
#non-sig neg effect

#########################################################3

#atlanta
data_atlanta = subset(Crime_data,atlanta==1)

atlanta.assault = lm(a.assault~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                          data=data_atlanta)
summary(atlanta.assault)
#dicho.oz    -1.737e-01  3.110e-01  -0.559  0.57652 
#dicho.oz    -7.833785e-01  4.360102e-01

glm.atlanta.assault = glm(a.assault~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                          family="poisson",data=data_atlanta)
summary(glm.atlanta.assault)
#dicho.oz    -3.195e-02  4.325e-02  -0.739  0.46006 
#[1] 0.968555
#dicho.oz    8.889766e-01 1.053251e+00

run.bart(data=data_atlanta,crime="a.assault",confounders = confound)
#  1.4598420  0.8883924 -0.1693492  3.2401279
#non-sig pos effect

#
atlanta.larceny = lm(larceny~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                          data=data_atlanta)
summary(atlanta.larceny)
#dicho.oz     3.527e-01  1.385e+00   0.255   0.7990    
#dicho.oz    -2.362825e+00  3.068154e+00

glm.atlanta.larceny = glm(larceny~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                          family="poisson",data=data_atlanta)
summary(glm.atlanta.larceny)
#dicho.oz     9.456e-04  1.394e-02   0.068 0.945922   
#non-sig pos effect

run.bart(data=data_atlanta,crime="larceny",confounders = confound)
#3.552331  4.374494 -4.942096 11.668484 
#non-sig pos effect

#
atlanta.auto.theft = lm(auto.theft~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                             data=data_atlanta)
summary(atlanta.auto.theft)
#dicho.oz     1.392e+00  5.155e-01   2.701  0.00695 ** 
#dicho.oz     3.815412e-01  2.402899e+00

glm.atlanta.auto.theft = glm(auto.theft~date2+avg.pm+precip+temp.max+uv+holidays+fri+sat+dicho.oz,
                             family="poisson",data=data_atlanta)
summary(glm.atlanta.auto.theft)
#dicho.oz     9.745e-02  3.042e-02   3.204  0.00136 **  
#sig pos effect

run.bart(data=data_atlanta,crime="auto.theft",confounders = confound)
# 0.2811322  1.9750594 -3.8754468  4.1929319 
#non-sig pos effect
