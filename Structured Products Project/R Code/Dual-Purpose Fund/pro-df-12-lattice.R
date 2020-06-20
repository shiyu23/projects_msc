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


# function to calculate sigma
sigma=function(t_start,t_end,type){
  start=which(data$Date==t_start)
  end=which(data$Date==t_end)
  date = as.Date(data$Date)
  fund_1 = data$X161812
  fund_2 = data$X163209
  print(start)
  print(end)
  if(type == 1){tmp_sig1 = matrix(0,end-start,1)
  for (i in 1:dim(tmp_sig1)[1]){tmp_sig1[i]=(log(fund_1[i+start]/fund_1[i-1+start]))^2}
  sigma = sqrt(mean(tmp_sig1)*244)}
  else if (type == 2){tmp_sig2 = matrix(0,end-start,1)
  for (i in 1:dim(tmp_sig2)[1]){tmp_sig2[i]=(log(fund_2[i+start]/fund_2[i-1+start]))^2}
  sigma = sqrt(mean(tmp_sig2)*244)}
}



#parameter setting
sig_1_2015 = sigma('2014/1/2','2014/12/31',1)
sig_2_2015 = sigma('2014/1/2','2014/12/31',2)
sig_1_2019 = sigma('2018/1/2','2018/12/28',1)
sig_2_2019 = sigma('2018/1/2','2018/12/28',2)




# lattice
path=getwd()
source(paste(path,"/ExplicitFiniteDiff.R",sep=""))
source(paste(path,"/IRExplicitFiniteDiff.R",sep=""))
##2015-161812
ex_start = Sys.time()

nt=365
Va_fund_1_2015_Ex = ExplicitFiniteDiff(ns=50,nt=365,sub_nt=1,hu=2,hd=0.25,
                        r=0.04, c=log(1+0.0122), R=0.0275+0.03,C=0.0122,
                        sig=sig_1_2015,threshold=10^-6,alpha=0.5)
##2019-161812  half
Va_fund_1_2019_Ex = ExplicitFiniteDiff(ns=50,nt=365,sub_nt=1,hu=2,hd=0.25,
                        r=0.04, c=log(1+0.0122), R=0.015+0.03,C=0.0122,
                        sig=sig_1_2019,threshold=10^-6,alpha=0.5)
##2015-163209
Va_fund_2_2015_Ex = ExplicitFiniteDiff(ns=50,nt=365,sub_nt=1,hu=2,hd=0.25,
                        r=0.04, c=log(1+0.0122), R=0.0275+0.035,C=0.0122, 
                        sig=sig_2_2015,threshold=10^-6,alpha=0.4)
##2019-163209  half
Va_fund_2_2019_Ex = ExplicitFiniteDiff(ns=50,nt=365,sub_nt=1,hu=2,hd=0.25, 
                        r=0.04, c=log(1+0.0122), R=0.015+0.035,C=0.0122, 
                        sig=sig_2_2019,threshold=10^-6,alpha=0.4)

## after reset
Va_fund_2_2015_Ex_re1 = IRExplicitFiniteDiff(ns=50,tr=(unclass(as.Date(date[reset1]))-unclass(as.Date(date[start_2015])))/365,sub_nt=1,hu=2,hd=0.25,
                                              r=0.04, c=log(1+0.0122), R=0.0275+0.035,C=0.0122, 
                                              sig=sig_2_2015,alpha=0.4,v0=Va_fund_2_2015_Ex[,1])

Va_fund_2_2015_Ex_re2 = IRExplicitFiniteDiff(ns=50,tr=(unclass(as.Date(date[reset2]))-unclass(as.Date(date[start_2015])))/365,sub_nt=1,hu=2,hd=0.25,
                                              r=0.04, c=log(1+0.0122), R=0.0275+0.035,C=0.0122, 
                                              sig=sig_2_2015,alpha=0.4,v0=Va_fund_2_2015_Ex[,1])

Va_fund_2_2015_Ex[,(nt-dim(Va_fund_2_2015_Ex_re1)[2]+1):nt] = Va_fund_2_2015_Ex_re1
Va_fund_2_2015_Ex[,(nt-dim(Va_fund_2_2015_Ex_re2)[2]+1):nt] = Va_fund_2_2015_Ex_re2


ex_end = Sys.time()


path=getwd()
source(paste(path,"/ImplicitFiniteDiff.R",sep=""))
source(paste(path,"/IRImplicitFiniteDiff.R",sep=""))
##2015-161812
im_start = Sys.time()
Va_fund_1_2015_Im = ImplicitFiniteDiff(ns=50,nt=365,sub_nt=1,hu=2,hd=0.25,
                                       r=0.04, c=log(1+0.0122), R=0.0275+0.03,C=0.0122,
                                       sig=sig_1_2015,threshold=10^-6,alpha=0.5)
##2019-161812  half
Va_fund_1_2019_Im = ImplicitFiniteDiff(ns=50,nt=365,sub_nt=1,hu=2,hd=0.25,
                                       r=0.04, c=log(1+0.0122), R=0.015+0.03,C=0.0122,
                                       sig=sig_1_2019,threshold=10^-6,alpha=0.5)
##2015-163209
Va_fund_2_2015_Im = ImplicitFiniteDiff(ns=50,nt=365,sub_nt=1,hu=2,hd=0.25,
                                       r=0.04, c=log(1+0.0122), R=0.0275+0.035,C=0.0122, 
                                       sig=sig_2_2015,threshold=10^-6,alpha=0.4)
##2019-163209  half
Va_fund_2_2019_Im = ImplicitFiniteDiff(ns=50,nt=365,sub_nt=1,hu=2,hd=0.25, 
                                       r=0.04, c=log(1+0.0122), R=0.015+0.035,C=0.0122, 
                                       sig=sig_2_2019,threshold=10^-6,alpha=0.4)

# after reset
Va_fund_2_2015_Im_re1 = IRImplicitFiniteDiff(ns=50,tr=(unclass(as.Date(date[reset1]))-unclass(as.Date(date[start_2015])))/365,sub_nt=1,hu=2,hd=0.25,
                                              r=0.04, c=log(1+0.0122), R=0.0275+0.035,C=0.0122, 
                                              sig=sig_2_2015,alpha=0.4,v0=Va_fund_2_2015_Im[,1])

Va_fund_2_2015_Im_re2 = IRImplicitFiniteDiff(ns=50,tr=(unclass(as.Date(date[reset2]))-unclass(as.Date(date[start_2015])))/365,sub_nt=1,hu=2,hd=0.25,
                                              r=0.04, c=log(1+0.0122), R=0.0275+0.035,C=0.0122, 
                                              sig=sig_2_2015,alpha=0.4,v0=Va_fund_2_2015_Im[,1])

Va_fund_2_2015_Im[,(nt-dim(Va_fund_2_2015_Im_re1)[2]+1):nt] = Va_fund_2_2015_Im_re1
Va_fund_2_2015_Im[,(nt-dim(Va_fund_2_2015_Im_re2)[2]+1):nt] = Va_fund_2_2015_Im_re2

im_end = Sys.time()



# # interpolate the value from matrix
path=getwd()
source(paste(path,"/InterpolateVa.R",sep=""))
Va_cal_fund_1_2015_Ex = matrix(0,end_2015-start_2015+1,1)
Va_cal_fund_2_2015_Ex = matrix(0,end_2015-start_2015+1,1)
Va_cal_fund_1_2015_Im = matrix(0,end_2015-start_2015+1,1)
Va_cal_fund_2_2015_Im = matrix(0,end_2015-start_2015+1,1)
for(ii in start_2015:end_2015 ){
  nt = 365
  t = (unclass(as.Date(date[ii]))-unclass(as.Date(date[start_2015]))) / nt
  s1 = fund_1[ii]
  s2 = fund_2[ii]
  Va_cal_fund_1_2015_Ex[ii+1-start_2015] = InterpolateVa(Va_matrix=Va_fund_1_2015_Ex,s1,t,hu=2,hd=0.25,sub_nt=1,nt=365)
  Va_cal_fund_2_2015_Ex[ii+1-start_2015] = InterpolateVa(Va_matrix=Va_fund_2_2015_Ex,s2,t,hu=2,hd=0.25,sub_nt=1,nt=365)
  Va_cal_fund_1_2015_Im[ii+1-start_2015] = InterpolateVa(Va_matrix=Va_fund_1_2015_Im,s1,t,hu=2,hd=0.25,sub_nt=1,nt=365)
  Va_cal_fund_2_2015_Im[ii+1-start_2015] = InterpolateVa(Va_matrix=Va_fund_2_2015_Im,s2,t,hu=2,hd=0.25,sub_nt=1,nt=365)
}

Va_cal_fund_1_2019_Ex = matrix(0,end_2019_1-start_2019+1,1)
Va_cal_fund_2_2019_Ex = matrix(0,end_2019_2-start_2019+1,1)
Va_cal_fund_1_2019_Im = matrix(0,end_2019_1-start_2019+1,1)
Va_cal_fund_2_2019_Im = matrix(0,end_2019_2-start_2019+1,1)
for(ii in start_2019:end_2019_1 ){
  nt = 365
  t = (unclass(as.Date(date[ii]))-unclass(as.Date(date[start_2019]))) / nt
  s1 = fund_1[ii]
  Va_cal_fund_1_2019_Ex[ii+1-start_2019] = InterpolateVa(Va_matrix=Va_fund_1_2019_Ex,s1,t,hu=2,hd=0.25,sub_nt=1,nt=365)
  Va_cal_fund_1_2019_Im[ii+1-start_2019] = InterpolateVa(Va_matrix=Va_fund_1_2019_Im,s1,t,hu=2,hd=0.25,sub_nt=1,nt=365)
}
for(ii in start_2019:end_2019_2 ){
  nt = 365
  t = (unclass(as.Date(date[ii]))-unclass(as.Date(date[start_2019]))) / nt
  s2 = fund_2[ii]
  Va_cal_fund_2_2019_Ex[ii+1-start_2019] = InterpolateVa(Va_matrix=Va_fund_2_2019_Ex,s2,t,hu=2,hd=0.25,sub_nt=1,nt=365)
  Va_cal_fund_2_2019_Im[ii+1-start_2019] = InterpolateVa(Va_matrix=Va_fund_2_2019_Im,s2,t,hu=2,hd=0.25,sub_nt=1,nt=365)
}


# compare the results between Explicit and Implicit
png(paste(path,"/comparison_Ashare_by_Explicit_Implicit.png",sep=""),
    width=10,height=10,units='in',res=500)
par(mfrow=c(2,2))
min_=min(Va_cal_fund_1_2015_Im,Va_cal_fund_1_2015_Ex)
max_=max(Va_cal_fund_1_2015_Im,Va_cal_fund_1_2015_Ex)
plot(Va_cal_fund_1_2015_Im~Va_cal_fund_1_2015_Ex,
     xlim=c(min_,max_),ylim=c(min_,max_),col='black',
     main='A share of fund1 in 2015',xlab='by Explicit',ylab='by Implicit')

min_=min(Va_cal_fund_2_2015_Im,Va_cal_fund_2_2015_Ex)
max_=max(Va_cal_fund_2_2015_Im,Va_cal_fund_2_2015_Ex)
plot(Va_cal_fund_2_2015_Im~Va_cal_fund_2_2015_Ex,
     xlim=c(min_,max_),ylim=c(min_,max_),col='black',
     main='A share of fund2 in 2015',xlab='by Explicit',ylab='by Implicit')

min_=min(Va_cal_fund_1_2019_Im,Va_cal_fund_1_2019_Ex)
max_=max(Va_cal_fund_1_2019_Im,Va_cal_fund_1_2019_Ex)
plot(Va_cal_fund_1_2019_Im~Va_cal_fund_1_2019_Ex,
     xlim=c(min_,max_),ylim=c(min_,max_),col='black',
     main='A share of fund1 in 2019',xlab='by Explicit',ylab='by Implicit')

min_=min(Va_cal_fund_2_2019_Im,Va_cal_fund_2_2019_Ex)
max_=max(Va_cal_fund_2_2019_Im,Va_cal_fund_2_2019_Ex)
plot(Va_cal_fund_2_2019_Im~Va_cal_fund_2_2019_Ex,
     xlim=c(min_,max_),ylim=c(min_,max_),col='black',
     main='A share of fund2 in 2019',xlab='by Explicit',ylab='by Implicit')

dev.off()


#compare between A share of fund by explicit and actual 
png(paste(path,"/comparison_Ashare_by_Explicit_actual.png",sep=""),
    width=10,height=5,units='in',res=500)
par(mfrow=c(2,2))
min_=min(fund_1_A[start_2015:end_2015],Va_cal_fund_1_2015_Ex)
max_=max(fund_1_A[start_2015:end_2015],Va_cal_fund_1_2015_Ex)
plot(Va_cal_fund_1_2015_Ex,ylab='',xlab='',ylim=c(min_,max_),type='l',
     main='A share of fund1 in 2015, blue: by explicit, red: actual',col='blue')
par(new=TRUE)
plot(fund_1_A[start_2015:end_2015],ylab='',xlab='',ylim=c(min_,max_),type='l',col='red')

min_=min(fund_2_A[start_2015:end_2015],Va_cal_fund_2_2015_Ex)
max_=max(fund_2_A[start_2015:end_2015],Va_cal_fund_2_2015_Ex)
plot(Va_cal_fund_2_2015_Ex,ylab='',xlab='',ylim=c(min_,max_),type='l',
     main='A share of fund2 in 2015, blue: by explicit, red: actual',col='blue')
par(new=TRUE)
plot(fund_2_A[start_2015:end_2015],ylab='',xlab='',ylim=c(min_,max_),type='l',col='red')

min_=min(fund_1_A[start_2019:end_2019_1],Va_cal_fund_1_2019_Ex)
max_=max(fund_1_A[start_2019:end_2019_1],Va_cal_fund_1_2019_Ex)
plot(Va_cal_fund_1_2019_Ex,ylab='',xlab='',ylim=c(min_,max_),type='l',
     main='A share of fund1 in 2019, blue: by explicit, red: actual',col='blue')
par(new=TRUE)
plot(fund_1_A[start_2019:end_2019_1],ylab='',xlab='',ylim=c(min_,max_),type='l',col='red')

min_=min(fund_2_A[start_2019:end_2019_2],Va_cal_fund_2_2019_Ex)
max_=max(fund_2_A[start_2019:end_2019_2],Va_cal_fund_2_2019_Ex)
plot(Va_cal_fund_2_2019_Ex,ylab='',xlab='',ylim=c(min_,max_),type='l',
     main='A share of fund2 in 2019, blue: by explicit, red: actual',col='blue')
par(new=TRUE)
plot(fund_2_A[start_2019:end_2019_2],ylab='',xlab='',ylim=c(min_,max_),type='l',col='red')

dev.off()




#compare between A share of fund by explicit and actual 
png(paste(path,"/comparison_Ashare_by_Explicit_actual_diag.png",sep=""),
    width=10,height=10,units='in',res=500)
par(mfrow=c(2,2))
min_=min(fund_1_A[start_2015:end_2015],Va_cal_fund_1_2015_Ex)
max_=max(fund_1_A[start_2015:end_2015],Va_cal_fund_1_2015_Ex)
plot(Va_cal_fund_1_2015_Ex~fund_1_A[start_2015:end_2015],
     pch=8,cex=0.5,
     xlab='actual data', ylab='calculation by explicit',
     xlim=c(min_,max_),ylim=c(min_,max_),col='black',
     main='A share of fund1 in 2015')

min_=min(fund_2_A[start_2015:end_2015],Va_cal_fund_2_2015_Ex)
max_=max(fund_2_A[start_2015:end_2015],Va_cal_fund_2_2015_Ex)
plot(Va_cal_fund_2_2015_Ex~fund_2_A[start_2015:end_2015],
     pch=8,cex=0.5,
     xlab='actual data', ylab='calculation by explicit',
     xlim=c(min_,max_),ylim=c(min_,max_),col='black',
     main='A share of fund2 in 2015')

min_=min(fund_1_A[start_2019:end_2019_1],Va_cal_fund_1_2019_Ex)
max_=max(fund_1_A[start_2019:end_2019_1],Va_cal_fund_1_2019_Ex)
plot(Va_cal_fund_1_2019_Ex~fund_1_A[start_2019:end_2019_1],
     pch=8,cex=0.5,
     xlab='actual data', ylab='calculation by explicit',
     xlim=c(min_,max_),ylim=c(min_,max_),col='black',
     main='A share of fund1 in 2019')

min_=min(fund_2_A[start_2019:end_2019_2],Va_cal_fund_2_2019_Ex)
max_=max(fund_2_A[start_2019:end_2019_2],Va_cal_fund_2_2019_Ex)
plot(Va_cal_fund_2_2019_Ex~fund_2_A[start_2019:end_2019_2],
     pch=8,cex=0.5,
     xlab='actual data', ylab='calculation by explicit',
     xlim=c(min_,max_),ylim=c(min_,max_),col='black',
     main='A share of fund2 in 2019')

dev.off()

im_end - im_start
ex_end - ex_start