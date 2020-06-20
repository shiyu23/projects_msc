data=read.csv("dual fund prices.csv")

# prepare data 
start_2015 = which(data$Date=='2015/1/5')
start_2019 = which(data$Date=='2019/1/2')
end_2015 = which(data$Date=='2015/12/31')
end_2019_1 = which(data$Date=='2019/6/28')
end_2019_2 = which(data$Date=='2019/5/28')
reset1 = which(data$Date=='2015/5/27') # happens on fund 2
reset2 = which(data$Date=='2015/9/15') # happens on fund 2
date = as.Date(data$Date)
fund_1 = data$X161812
fund_1_A = data$X150018
fund_1_B = data$X150019
fund_2 = data$X163209
fund_2_A = data$X150073
fund_2_B = data$X150075
plot(data[start_2015:end_2015,2],type = "l",ylim = c(0.7,2))
par(new=TRUE)
plot(start_2015:end_2015,matrix(1,end_2015-start_2015+1,1),type = "l",ylim = c(0.7,2))
plot(data[start_2015:end_2015,5],type = "l",ylim = c(0.3,2))
par(new=TRUE)
plot(start_2015:end_2015,matrix(1,end_2015-start_2015+1,1),type = "l",ylim = c(0.3,2))

plot(data[start_2019:end_2019_1,2],type = "l",ylim = c(0.7,2))
par(new=TRUE)
plot(start_2019:end_2019_1,matrix(1,end_2019_1-start_2019+1,1),type = "l",ylim = c(0.7,2))
plot(data[start_2019:end_2019_2,5],type = "l",ylim = c(0.3,2))
par(new=TRUE)
plot(start_2019:end_2019_2,matrix(1,end_2019_2-start_2019+1,1),type = "l",ylim = c(0.3,2))

# function to calculate sigma
sigma=function(t_start,t_end,type){
  start=which(data$Date==t_start)
  end=which(data$Date==t_end)
  date = as.Date(data$Date)
  fund_1 = data$X161812
  fund_2 = data$X163209
  if(type == 1){tmp_sig1 = matrix(0,end-start,1)
  for (i in 1:dim(tmp_sig1)[1]){tmp_sig1[i]=(log(fund_1[i+start]/fund_1[i-1+start]))^2}
  sigma = sqrt(mean(tmp_sig1)*244)}
  else if (type == 2){tmp_sig2 = matrix(0,end-start,1)
  for (i in 1:dim(tmp_sig2)[1]){tmp_sig2[i]=(log(fund_2[i+start]/fund_2[i-1+start]))^2}
  sigma = sqrt(mean(tmp_sig2)*244)}
}

# sigma for 2015 and 2019
sig_1_2015 = sigma('2014/1/2','2014/12/31',1)
sig_2_2015 = sigma('2014/1/2','2014/12/31',2)
sig_1_2019 = sigma('2018/1/2','2018/12/28',1)
sig_2_2019 = sigma('2018/1/2','2018/12/28',2)

# monte carlo for Va01
mcv01_start = Sys.time()

path=getwd()
source(paste(path,"/MonteCarloAtV01.R",sep=""))
v01_1_2015=MonteCarloAtV01(maxt=100,nm=1000,nt=365,sub_nt=1,
                           r=0.04,c=log(1+0.0122),R=0.0275+0.03,C=0.0122,
                           sig=sig_1_2015,hu=2,hd=0.25,threshold=0.01,alpha=0.5)
v01_1_2019=MonteCarloAtV01(maxt=100,nm=1000,nt=365,sub_nt=1,
                           r=0.04,c=log(1+0.0122),R=0.015+0.03,C=0.0122,
                           sig=sig_1_2019,hu=2,hd=0.25,threshold=0.01,alpha=0.5)
v01_2_2015=MonteCarloAtV01(maxt=100,nm=1000,nt=365,sub_nt=1,
                           r=0.04,c=log(1+0.0122),R=0.0275+0.035,C=0.0122,
                           sig=sig_2_2015,hu=2,hd=0.25,threshold=0.01,alpha=0.4)
v01_2_2019=MonteCarloAtV01(maxt=100,nm=1000,nt=365,sub_nt=1,
                           r=0.04,c=log(1+0.0122),R=0.015+0.035,C=0.0122,
                           sig=sig_2_2019,hu=2,hd=0.25,threshold=0.01,alpha=0.4)

mcv01_end = Sys.time()

# monte carlo for pricing
mc_start = Sys.time()

path=getwd()
source(paste(path,"/MonteCarloApproach.R",sep=""))
source(paste(path,"/IRMonteCarloApproach.R",sep=""))
Va_cal_fund_1_2015_mc = matrix(0,end_2015-start_2015+1,1)
Va_cal_fund_2_2015_mc = matrix(0,end_2015-start_2015+1,1)
for(ii in start_2015:end_2015 ){
  nt = 365
  t = (unclass(as.Date(date[ii]))-unclass(as.Date(date[start_2015]))) / nt
  s1 = fund_1[ii]
  s2 = fund_2[ii]
  
  Va_cal_fund_1_2015_mc[ii+1-start_2015] = MonteCarloApproach(s=s1,t=t,V01=v01_1_2015,
                                                              maxt=100,nt=365,sub_nt=1,nm=1000,hu=2,hd=0.25,
                                                              R=0.0275+0.03,C=0.0122,r=0.04,c=log(1+0.0122),
                                                              sig=sig_1_2015,alpha=0.5)

  if ((unclass(as.Date(date[ii]))-unclass(as.Date(date[reset1])))<0) {
  Va_cal_fund_2_2015_mc[ii+1-start_2015] = MonteCarloApproach(s=s2,t=t,V01=v01_2_2015,
                                                              maxt=100,nt=365,sub_nt=1,nm=1000,hu=2,hd=0.25,
                                                              R=0.0275+0.035,C=0.0122,r=0.04,c=log(1+0.0122),
                                                              sig=sig_2_2015,alpha=0.4)
  }
  # after reset
  if (((unclass(as.Date(date[ii]))-unclass(as.Date(date[reset1])))>=0)&&((unclass(as.Date(date[ii]))-unclass(as.Date(date[reset2])))<0)) {Va_cal_fund_2_2015_mc[ii+1-start_2015] = IRMonteCarloApproach(s=s2,t=t,tr=(unclass(as.Date(date[reset1]))-unclass(as.Date(date[start_2015])))/365,V01=v01_2_2015,
                                                                                                                maxt=100,nt=365,sub_nt=1,nm=1000,hu=2,hd=0.25,
                                                                                                                R=0.0275+0.035,C=0.0122,r=0.04,c=log(1+0.0122),
                                                                                                                sig=sig_2_2015,alpha=0.4)}
  
  if ((unclass(as.Date(date[ii]))-unclass(as.Date(date[reset2])))>=0) {Va_cal_fund_2_2015_mc[ii+1-start_2015] = IRMonteCarloApproach(s=s2,t=t,tr=(unclass(as.Date(date[reset2]))-unclass(as.Date(date[start_2015])))/365,V01=v01_2_2015,
                                                                                        maxt=100,nt=365,sub_nt=1,nm=1000,hu=2,hd=0.25,
                                                                                        R=0.0275+0.035,C=0.0122,r=0.04,c=log(1+0.0122),
                                                                                        sig=sig_2_2015,alpha=0.4)}
}


Va_cal_fund_1_2019_mc = matrix(0,end_2019_1-start_2019+1,1)
Va_cal_fund_2_2019_mc = matrix(0,end_2019_2-start_2019+1,1)
for(ii in start_2019:end_2019_1 ){
  nt = 365
  t = (unclass(as.Date(date[ii]))-unclass(as.Date(date[start_2019]))) / nt
  s1 = fund_1[ii]
  Va_cal_fund_1_2019_mc[ii+1-start_2019] = MonteCarloApproach(s=s1,t=t,V01=v01_1_2019,
                                                              maxt=100,nt=365,sub_nt=1,nm=1000,hu=2,hd=0.25,
                                                              R=0.015+0.03,C=0.0122,r=0.04,c=log(1+0.0122),
                                                              sig=sig_1_2019,alpha=0.5)
}
for(ii in start_2019:end_2019_2 ){
  nt = 365
  t = (unclass(as.Date(date[ii]))-unclass(as.Date(date[start_2019]))) / nt
  s2 = fund_2[ii]
  Va_cal_fund_2_2019_mc[ii+1-start_2019] = MonteCarloApproach(s=s2,t=t,V01=v01_2_2019,
                                                              maxt=100,nt=365,sub_nt=1,nm=1000,hu=2,hd=0.25,
                                                              R=0.015+0.035,C=0.0122,r=0.04,c=log(1+0.0122),    
                                                              sig=sig_2_2019,alpha=0.4)
}

mc_end = Sys.time()

# time used
mcv01_end - mcv01_start
mc_end - mc_start

# the difference of Va01 from mc and lattice
((ceiling(1/3*50)-1/3*50)/(ceiling(1/3*50)-floor(1/3*50))*Va_fund_1_2015_Im[floor(1/3*50),1] - (floor(1/3*50)-1/3*50)/(ceiling(1/3*50)-floor(1/3*50))*Va_fund_1_2015_Im[ceiling(1/3*50),1]) - v01_1_2015
((ceiling(1/3*50)-1/3*50)/(ceiling(1/3*50)-floor(1/3*50))*Va_fund_2_2015_Im[floor(1/3*50),1] - (floor(1/3*50)-1/3*50)/(ceiling(1/3*50)-floor(1/3*50))*Va_fund_2_2015_Im[ceiling(1/3*50),1]) - v01_2_2015
((ceiling(1/3*50)-1/3*50)/(ceiling(1/3*50)-floor(1/3*50))*Va_fund_1_2019_Im[floor(1/3*50),1] - (floor(1/3*50)-1/3*50)/(ceiling(1/3*50)-floor(1/3*50))*Va_fund_1_2019_Im[ceiling(1/3*50),1]) - v01_1_2019
((ceiling(1/3*50)-1/3*50)/(ceiling(1/3*50)-floor(1/3*50))*Va_fund_2_2019_Im[floor(1/3*50),1] - (floor(1/3*50)-1/3*50)/(ceiling(1/3*50)-floor(1/3*50))*Va_fund_2_2019_Im[ceiling(1/3*50),1]) - v01_2_2019

#compare between A share of fund by Monte-Carlo and actual
png(paste(path,"/comparison_Ashare_by_Monte-Carlo_actual.png",sep=""),
    width=10,height=5,units='in',res=500)
par(mfrow=c(2,2))
min_=min(fund_1_A[start_2015:end_2015],Va_cal_fund_1_2015_mc)
max_=max(fund_1_A[start_2015:end_2015],Va_cal_fund_1_2015_mc)
plot(Va_cal_fund_1_2015_mc,ylab='',xlab='',ylim=c(min_,max_),type='l',
     main='A share of fund1 in 2015, blue: by Monte-Carlo, red: actual',col='blue')
par(new=TRUE)
plot(fund_1_A[start_2015:end_2015],ylab='',xlab='',ylim=c(min_,max_),type='l',col='red')

min_=min(fund_2_A[start_2015:end_2015],Va_cal_fund_2_2015_mc)
max_=max(fund_2_A[start_2015:end_2015],Va_cal_fund_2_2015_mc)
plot(Va_cal_fund_2_2015_mc,ylab='',xlab='',ylim=c(min_,max_),type='l',
     main='A share of fund2 in 2015, blue: by Monte-Carlo, red: actual',col='blue')
par(new=TRUE)
plot(fund_2_A[start_2015:end_2015],ylab='',xlab='',ylim=c(min_,max_),type='l',col='red')

min_=min(fund_1_A[start_2019:end_2019_1],Va_cal_fund_1_2019_mc)
max_=max(fund_1_A[start_2019:end_2019_1],Va_cal_fund_1_2019_mc)
plot(Va_cal_fund_1_2019_mc,ylab='',xlab='',ylim=c(min_,max_),type='l',
     main='A share of fund1 in 2019, blue: by Monte-Carlo, red: actual',col='blue')
par(new=TRUE)
plot(fund_1_A[start_2019:end_2019_1],ylab='',xlab='',ylim=c(min_,max_),type='l',col='red')

min_=min(fund_2_A[start_2019:end_2019_2],Va_cal_fund_2_2019_mc)
max_=max(fund_2_A[start_2019:end_2019_2],Va_cal_fund_2_2019_mc)
plot(Va_cal_fund_2_2019_mc,ylab='',xlab='',ylim=c(min_,max_),type='l',
     main='A share of fund2 in 2019, blue: by Monte-Carlo, red: actual',col='blue')
par(new=TRUE)
plot(fund_2_A[start_2019:end_2019_2],ylab='',xlab='',ylim=c(min_,max_),type='l',col='red')

dev.off()

#compare between A share of fund by Monte-Carlo and actual 
png(paste(path,"/comparison_Ashare_by_Monte-Carlo_actual_diag.png",sep=""),
    width=10,height=10,units='in',res=500)
par(mfrow=c(2,2))
min_=min(fund_1_A[start_2015:end_2015],Va_cal_fund_1_2015_mc)
max_=max(fund_1_A[start_2015:end_2015],Va_cal_fund_1_2015_mc)
plot(Va_cal_fund_1_2015_mc~fund_1_A[start_2015:end_2015],
     pch=8,cex=0.5,
     xlab='actual data', ylab='calculation by Monte-Carlo',
     xlim=c(min_,max_),ylim=c(min_,max_),col='black',
     main='A share of fund1 in 2015')

min_=min(fund_2_A[start_2015:end_2015],Va_cal_fund_2_2015_mc)
max_=max(fund_2_A[start_2015:end_2015],Va_cal_fund_2_2015_mc)
plot(Va_cal_fund_2_2015_mc~fund_2_A[start_2015:end_2015],
     pch=8,cex=0.5,
     xlab='actual data', ylab='calculation by Monte-Carlo',
     xlim=c(min_,max_),ylim=c(min_,max_),col='black',
     main='A share of fund2 in 2015')

min_=min(fund_1_A[start_2019:end_2019_1],Va_cal_fund_1_2019_mc)
max_=max(fund_1_A[start_2019:end_2019_1],Va_cal_fund_1_2019_mc)
plot(Va_cal_fund_1_2019_mc~fund_1_A[start_2019:end_2019_1],
     pch=8,cex=0.5,
     xlab='actual data', ylab='calculation by Monte-Carlo',
     xlim=c(min_,max_),ylim=c(min_,max_),col='black',
     main='A share of fund1 in 2019')

min_=min(fund_2_A[start_2019:end_2019_2],Va_cal_fund_2_2019_mc)
max_=max(fund_2_A[start_2019:end_2019_2],Va_cal_fund_2_2019_mc)
plot(Va_cal_fund_2_2019_mc~fund_2_A[start_2019:end_2019_2],
     pch=8,cex=0.5,
     xlab='actual data', ylab='calculation by Monte-Carlo',
     xlim=c(min_,max_),ylim=c(min_,max_),col='black',
     main='A share of fund2 in 2019')

dev.off()


#compare between A share of fund by Monte-Carlo and lattice
png(paste(path,"/comparison_Ashare_by_Monte-Carlo_Lattce.png",sep=""),
    width=10,height=5,units='in',res=500)
par(mfrow=c(2,2))
min_=min(fund_1_A[start_2015:end_2015],Va_cal_fund_1_2015_mc,Va_cal_fund_1_2015_Im)
max_=max(fund_1_A[start_2015:end_2015],Va_cal_fund_1_2015_mc,Va_cal_fund_1_2015_Im)
plot(Va_cal_fund_1_2015_mc,ylab='',xlab='',ylim=c(min_,max_),type='l',cex.main=0.8,
     main='A share of fund1 in 2015, blue: by Monte-Carlo, green: by Implicit, red: actual',col='blue')
par(new=TRUE)
plot(fund_1_A[start_2015:end_2015],ylab='',xlab='',ylim=c(min_,max_),type='l',col='red')
par(new=TRUE)
plot(Va_cal_fund_1_2015_Im,ylab='',xlab='',ylim=c(min_,max_),type='l',col='green')


min_=min(fund_2_A[start_2015:end_2015],Va_cal_fund_2_2015_mc,Va_cal_fund_2_2015_Im)
max_=max(fund_2_A[start_2015:end_2015],Va_cal_fund_2_2015_mc,Va_cal_fund_2_2015_Im)
plot(Va_cal_fund_2_2015_mc,ylab='',xlab='',ylim=c(min_,max_),type='l',cex.main=0.8,
     main='A share of fund2 in 2015, blue: by Monte-Carlo, green: by Implicit, red: actual',col='blue')
par(new=TRUE)
plot(fund_2_A[start_2015:end_2015],ylab='',xlab='',ylim=c(min_,max_),type='l',col='red')
par(new=TRUE)
plot(Va_cal_fund_2_2015_Im,ylab='',xlab='',ylim=c(min_,max_),type='l',col='green')


min_=min(fund_1_A[start_2019:end_2019_1],Va_cal_fund_1_2019_mc,Va_cal_fund_1_2019_Im)
max_=max(fund_1_A[start_2019:end_2019_1],Va_cal_fund_1_2019_mc,Va_cal_fund_1_2019_Im)
plot(Va_cal_fund_1_2019_mc,ylab='',xlab='',ylim=c(min_,max_),type='l',cex.main=0.8,
     main='A share of fund1 in 2019, blue: by Monte-Carlo, green: by Implicit, red: actual',col='blue')
par(new=TRUE)
plot(fund_1_A[start_2019:end_2019_1],ylab='',xlab='',ylim=c(min_,max_),type='l',col='red')
par(new=TRUE)
plot(Va_cal_fund_1_2019_Im,ylab='',xlab='',ylim=c(min_,max_),type='l',col='green')


min_=min(fund_2_A[start_2019:end_2019_2],Va_cal_fund_2_2019_mc,Va_cal_fund_2_2019_Im)
max_=max(fund_2_A[start_2019:end_2019_2],Va_cal_fund_2_2019_mc,Va_cal_fund_2_2019_Im)
plot(Va_cal_fund_2_2019_mc,ylab='',xlab='',ylim=c(min_,max_),type='l',cex.main=0.8,
     main='A share of fund2 in 2019, blue: by Monte-Carlo, green: by Implicit, red: actual',col='blue')
par(new=TRUE)
plot(fund_2_A[start_2019:end_2019_2],ylab='',xlab='',ylim=c(min_,max_),type='l',col='red')
par(new=TRUE)
plot(Va_cal_fund_2_2019_Im,ylab='',xlab='',ylim=c(min_,max_),type='l',col='green')

dev.off()

