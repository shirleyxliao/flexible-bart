## figures
library(ggplot2)
####### data description
## summary stats table 

load("~/shared_space/ci3_analysis/zigler_lab/projects/Ozone_Aggression/edited.crime.data.R")

summary.table = aggregate(a.assault~city,data=Crime_data,mean)
names(summary.table) = c("City","Avg.AA")

summary.table$Avg.SA = aggregate(s.assault~city,data=Crime_data,mean)$s.assault
summary.table$Avg.Larceny = aggregate(larceny~city,data=Crime_data,mean)$larceny
summary.table$Avg.Auto.Theft = aggregate(auto.theft~city,data=Crime_data,mean)$auto.theft
summary.table$Avg.PM = aggregate(avg.pm~city,data=Crime_data,mean)$avg.pm
summary.table$Avg.Precip = aggregate(precip~city,data=Crime_data,mean)$precip
summary.table$Avg.Temp = aggregate(temp.max~city,data=Crime_data,mean)$temp.max
summary.table$Avg.UV = aggregate(uv~city,data=Crime_data,mean)$uv
summary.table$N.years = aggregate(year~city,data=Crime_data,function(x){length(unique(x))})$year
summary.table$Avg.Pop = aggregate(population~city,data=Crime_data,mean)$population
summary.table$Avg.Oz = aggregate(max.oz~city,data=Crime_data,mean)$max.oz
summary.table$Prop.high.Oz = aggregate(dicho.oz~city,data=Crime_data,mean)$dicho.oz
library(xtable)
xtable(summary.table,digits=3)

###### main analysis
load("~/shared_space/ci3_analysis/zigler_lab/projects/Ozone_Aggression/crime_results.R")
row.names(results) = c()
#####

#table w TE + 95\% CI for within-city effects
aa.results = subset(results,crime=="a.assault" &
                      method %in% c("BART","POISS") &
                      analysis %in% c("dicho.oz"),
                    select = c(city,method,TE,lower.interval,upper.interval))
aa.results$TE = (aa.results$TE - 1)*100
aa.results$lower.interval = (aa.results$lower.interval- 1)*100
aa.results$upper.interval = (aa.results$upper.interval - 1)*100

library(xtable)
print(xtable(aa.results,digits=3),include.rownames=FALSE)

##############################################################
# RMSE

rmse.results = subset(results,crime=="a.assault" &
                        method %in% c("BART","POISS") &
                        analysis %in% c("dicho.oz"),
                      select = c(city,method,high.RMSE,low.RMSE))

#####

png("~/shared_space/ci3_analysis/zigler_lab/projects/Ozone_Aggression/final_fig.png")

ggplot(subset(aa.results,city!="all"),aes(y=city,x=TE,color=method)) +
  geom_point(data=subset(aa.results,city=="all"), aes(color=method), shape=18, size=4)+
  geom_errorbarh(aes(xmin=lower.interval,xmax=upper.interval)) + 
  geom_vline(xintercept = 0,linetype="dashed") + 
  geom_errorbarh(data=subset(aa.results,city=="all"),aes(xmin=lower.interval,xmax=upper.interval,color=method)) +
  labs(title="Percent increase in AA rate due to high oz",y="City",x="Percent increase in AA",color="Analysis method")

dev.off()


######## sensitivity analyses

library(ggplot2)

####### figure for dog bites + sa
load("~/shared_space/ci3_analysis/zigler_lab/projects/Ozone_Aggression/crime_results.R")
sa.results = subset(results,crime=="s.assault" &
                      method %in% c("BART","POISS") &
                      analysis %in% c("dicho.oz"),
                    select = c(crime,city,method,TE,lower.interval,upper.interval))
sa.results$TE = (sa.results$TE - 1)*100
sa.results$lower.interval = (sa.results$lower.interval- 1)*100
sa.results$upper.interval = (sa.results$upper.interval - 1)*100

load("~/shared_space/ci3_analysis/zigler_lab/projects/Ozone_Aggression/dog_dicho_results.R")

dog.results = subset(results,
                      method %in% c("BART","POISS"),
                      
                    select = c(crime,city,method,TE,lower.interval,upper.interval))

dog.results$TE = (dog.results$TE - 1)*100
dog.results$lower.interval = (dog.results$lower.interval- 1)*100
dog.results$upper.interval = (dog.results$upper.interval - 1)*100

violent.results = rbind(sa.results,dog.results)

library(xtable)
print(xtable(violent.results,digits=2),include.rownames=FALSE)

png("~/shared_space/ci3_analysis/zigler_lab/projects/Ozone_Aggression/violent_crime_fig.png")

ggplot(subset(violent.results,city!="all"),aes(y=city,x=TE,color=method)) + 
  geom_point(data=subset(violent.results,city=="all"), aes(color=method), shape=18, size=4)+
  geom_errorbarh(aes(xmin=lower.interval,xmax=upper.interval)) + 
  geom_errorbarh(data=subset(violent.results,city=="all"),aes(xmin=lower.interval,xmax=upper.interval,color=method)) +
  geom_vline(xintercept = 0) + facet_wrap(~crime) + coord_cartesian(xlim=c(-200,200))+
  labs(title="Percent increase in SA and dog bites due to high oz",y="City",x="Percent increase in crime",color="Analysis method")

dev.off()

######## figure for other crimes
load("~/shared_space/ci3_analysis/zigler_lab/projects/Ozone_Aggression/crime_results.R")

other.results = subset(results,!(crime%in% c("a.assault","s.assault"))& method %in% c("BART","POISS") & analysis == "dicho.oz")
other.results$TE = (other.results$TE - 1)*100
other.results$lower.interval = (other.results$lower.interval- 1)*100
other.results$upper.interval = (other.results$upper.interval - 1)*100
print(xtable(other.results,digits=2),include.rownames=FALSE)

png("~/shared_space/ci3_analysis/zigler_lab/projects/Ozone_Aggression/other_crime_fig.png")

ggplot(subset(other.results,city!="all"),aes(y=city,x=TE,color=method)) + 
  geom_point(data=subset(other.results,city=="all"), aes(color=method), shape=18, size=4)+
  geom_errorbarh(aes(xmin=lower.interval,xmax=upper.interval)) + 
  geom_errorbarh(data=subset(other.results,city=="all"),aes(xmin=lower.interval,xmax=upper.interval,color=method)) +
  geom_vline(xintercept = 0) + facet_wrap(~crime) + 
  labs(title="Percent increase in nonviolent crime due to high oz",y="City",x="Percent increase in crime",color="Analysis method")

dev.off()


####### dose-response curve + poisson regression line
load("~/shared_space/ci3_analysis/zigler_lab/projects/Ozone_Aggression/dose.response.results.R")
load("~/shared_space/ci3_analysis/zigler_lab/projects/Ozone_Aggression/poisson.continuous.results.R")

results$analysis = "BART"
results2$analysis = "POISS"
results3 = cbind(results2[,1:5],data.frame(upper = results2$upper,lower = results2$lower,RMSE = results2$RMSE,analysis = "POISS"))
results = rbind(results,results3)
results.aa = subset(results,crime=="a.assault" & city == "all")

results.aa$TE = (results.aa$TE - 1)*100
results.aa$lower = (results.aa$lower - 1)*100
results.aa$upper = (results.aa$upper - 1)*100

print(xtable(results.aa,digits=2),include.rownames=FALSE)

png("~/shared_space/ci3_analysis/zigler_lab/projects/Ozone_Aggression/aa_oz_curve.png")

ggplot(subset(results.aa,type=="oz"),aes(x=increment,y=TE,color=analysis)) + 
  geom_errorbar(aes(ymin=lower,ymax=upper)) + 
  geom_hline(yintercept = 0,linetype=2) + xlab("Incremental increase in oz concentration (ppm/m3)")+
  ylab("Percent increase in AA rate corresponding to increase in exposure")
dev.off()

png("~/shared_space/ci3_analysis/zigler_lab/projects/Ozone_Aggression/aa_temp_curve.png")

ggplot(subset(results.aa,type=="temp"),aes(x=increment,y=TE,color=analysis)) + 
  geom_errorbar(aes(ymin=lower,ymax=upper)) + 
  geom_hline(yintercept = 0) + xlab("Incremental increase in temperature (degrees F)")+
  ylab("Percent increase in AA rate corresponding to increase in exposure")

dev.off()

#################################################
##### rmse table
rmse.results$cont.RMSE = NA

for (i in 1:length(rmse.results$cont.RMSE)){
  city = rmse.results$city[i]
  method = rmse.results$method[i]
  
  rmse.results$cont.RMSE[i] = c(results$RMSE[results$crime == "a.assault" & results$type=="oz" & results$city==city & results$analysis==method])[1]
}


xtable(rmse.results,digits=3)

################################################

## dose-response curve - sa 
results.sa = subset(results,crime=="s.assault" & city == "all")
results.sa$TE = (results.sa$TE - 1)*100
results.sa$lower = (results.sa$lower - 1)*100
results.sa$upper = (results.sa$upper - 1)*100

png("~/shared_space/ci3_analysis/zigler_lab/projects/Ozone_Aggression/sa_oz_curve.png")

ggplot(subset(results.sa,type=="oz"),aes(x=increment,y=TE,color=analysis)) + 
  geom_errorbar(aes(ymin=lower,ymax=upper)) + 
  geom_hline(yintercept = 0) + xlab("Incremental increase in oz concentration (ppm/m3)")+
  ylab("Percent increase in SA rate corresponding to increase in exposure")
dev.off()

png("~/shared_space/ci3_analysis/zigler_lab/projects/Ozone_Aggression/sa_temp_curve.png")

ggplot(subset(results.sa,type=="temp"),aes(x=increment,y=TE,color=analysis)) + 
  geom_errorbar(aes(ymin=lower,ymax=upper)) + 
  geom_hline(yintercept = 0) + xlab("Incremental increase in temperature (degrees F)")+
  ylab("Percent increase in SA rate corresponding to increase in exposure")

dev.off()


## dose-response curve - dog bites
load("~/shared_space/ci3_analysis/zigler_lab/projects/Ozone_Aggression/dog.dose.response.results.R")
load("~/shared_space/ci3_analysis/zigler_lab/projects/Ozone_Aggression/dog.poisson.continuous.results.R")

results$analysis = "BART"
results2$analysis = "POISS"
results3 = cbind(results2[,1:5],data.frame(upper = results2$upper,lower = results2$lower,RMSE = results2$RMSE,analysis = "POISS"))
results = rbind(results,results3)

results.dog = subset(results,city == "all")
results.dog$TE = (results.dog$TE - 1)*100
results.dog$lower = (results.dog$lower - 1)*100
results.dog$upper = (results.dog$upper - 1)*100

png("~/shared_space/ci3_analysis/zigler_lab/projects/Ozone_Aggression/dog_oz_curve.png")

ggplot(subset(results.dog,type=="oz"),aes(x=increment,y=TE,color=analysis)) + 
  geom_errorbar(aes(ymin=lower,ymax=upper)) + 
  geom_hline(yintercept = 0,linetype=2) + xlab("Incremental increase in oz concentration (ppm/m3)")+
  ylab("Percent increase in dog bite rate corresponding to increase in exposure")
dev.off()

png("~/shared_space/ci3_analysis/zigler_lab/projects/Ozone_Aggression/dog_temp_curve.png")

ggplot(subset(results.dog,type=="temp"),aes(x=increment,y=TE,color=analysis)) + 
  geom_errorbar(aes(ymin=lower,ymax=upper)) + 
  geom_hline(yintercept = 0,linetype=2) + xlab("Incremental increase in temperature (degrees F)")+
  ylab("Percent increase in dog bite rate corresponding to increase in exposure")

dev.off()
#######

