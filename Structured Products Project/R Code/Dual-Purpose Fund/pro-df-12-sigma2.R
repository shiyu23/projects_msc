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
source(paste(path,"/ImplicitFiniteDiff.R",sep=""))
source(paste(path,"/IRExplicitFiniteDiff.R",sep=""))
##2015-161812
nt=365
Va_fund_1_2015_sig30 = ImplicitFiniteDiff(ns=50,nt=365,sub_nt=1,hu=2,hd=0.25,
                                       r=0.04, c=log(1+0.0122), R=0.0275+0.03,C=0.0122,
                                       sig=0.30,threshold=10^-6,alpha=0.5)
Va_fund_1_2015_sig25 = ImplicitFiniteDiff(ns=50,nt=365,sub_nt=1,hu=2,hd=0.25,
                                         r=0.04, c=log(1+0.0122), R=0.0275+0.03,C=0.0122,
                                         sig=0.25,threshold=10^-6,alpha=0.5)
Va_fund_1_2015_sig20 = ImplicitFiniteDiff(ns=50,nt=365,sub_nt=1,hu=2,hd=0.25,
                                         r=0.04, c=log(1+0.0122), R=0.0275+0.03,C=0.0122,
                                         sig=0.20,threshold=10^-6,alpha=0.5)
Va_fund_1_2015_sig15 = ImplicitFiniteDiff(ns=50,nt=365,sub_nt=1,hu=2,hd=0.25,
                                         r=0.04, c=log(1+0.0122), R=0.0275+0.03,C=0.0122,
                                         sig=0.15,threshold=10^-6,alpha=0.5)
Va_fund_1_2015_sig10 = ImplicitFiniteDiff(ns=50,nt=365,sub_nt=1,hu=2,hd=0.25,
                                         r=0.04, c=log(1+0.0122), R=0.0275+0.03,C=0.0122,
                                         sig=0.10,threshold=10^-6,alpha=0.5)
Va_fund_1_2015_sig5 = ImplicitFiniteDiff(ns=50,nt=365,sub_nt=1,hu=2,hd=0.25,
                                         r=0.04, c=log(1+0.0122), R=0.0275+0.03,C=0.0122,
                                         sig=0.05,threshold=10^-6,alpha=0.5)



# # interpolate the value from matrix
path=getwd()
source(paste(path,"/InterpolateVa.R",sep=""))
Va_cal_fund_1_2015_sig30 = matrix(0,end_2015-start_2015+1,1)
Va_cal_fund_1_2015_sig25 = matrix(0,end_2015-start_2015+1,1)
Va_cal_fund_1_2015_sig20 = matrix(0,end_2015-start_2015+1,1)
Va_cal_fund_1_2015_sig15 = matrix(0,end_2015-start_2015+1,1)
Va_cal_fund_1_2015_sig10 = matrix(0,end_2015-start_2015+1,1)
Va_cal_fund_1_2015_sig5 = matrix(0,end_2015-start_2015+1,1)
for(ii in start_2015:end_2015 ){
  nt = 365
  t = (unclass(as.Date(date[ii]))-unclass(as.Date(date[start_2015]))) / nt
  s1 = fund_1[ii]
  s2 = fund_2[ii]
  Va_cal_fund_1_2015_sig30[ii+1-start_2015] = InterpolateVa(Va_matrix=Va_fund_1_2015_sig30,s1,t,hu=2,hd=0.25,sub_nt=1,nt=365)
  Va_cal_fund_1_2015_sig25[ii+1-start_2015] = InterpolateVa(Va_matrix=Va_fund_1_2015_sig25,s1,t,hu=2,hd=0.25,sub_nt=1,nt=365)
  Va_cal_fund_1_2015_sig20[ii+1-start_2015] = InterpolateVa(Va_matrix=Va_fund_1_2015_sig20,s1,t,hu=2,hd=0.25,sub_nt=1,nt=365)
  Va_cal_fund_1_2015_sig15[ii+1-start_2015] = InterpolateVa(Va_matrix=Va_fund_1_2015_sig15,s1,t,hu=2,hd=0.25,sub_nt=1,nt=365)
  Va_cal_fund_1_2015_sig10[ii+1-start_2015] = InterpolateVa(Va_matrix=Va_fund_1_2015_sig10,s1,t,hu=2,hd=0.25,sub_nt=1,nt=365)
  Va_cal_fund_1_2015_sig5[ii+1-start_2015] = InterpolateVa(Va_matrix=Va_fund_1_2015_sig5,s1,t,hu=2,hd=0.25,sub_nt=1,nt=365)
}

# compare the results between Explicit and Implicit
png(paste(path,"/comparison_sigma.png",sep=""),
    width=10,height=10,units='in',res=1000)
mycolor=rainbow(6)
par(mfrow=c(1,1))
min_=min(fund_1_A[start_2015:end_2015],Va_cal_fund_1_2015_sig30,Va_cal_fund_1_2015_sig25,Va_cal_fund_1_2015_sig20,Va_cal_fund_1_2015_sig15,Va_cal_fund_1_2015_sig10,Va_cal_fund_1_2015_sig5)
max_=max(fund_1_A[start_2015:end_2015],Va_cal_fund_1_2015_sig30,Va_cal_fund_1_2015_sig25,Va_cal_fund_1_2015_sig20,Va_cal_fund_1_2015_sig15,Va_cal_fund_1_2015_sig10,Va_cal_fund_1_2015_sig5)
plot(Va_cal_fund_1_2015_sig5,ylim=c(min_,max_),type='l',ylab='',col=mycolor[1],
     main='Comparison of different sigma, based on A share of fund1 in 2015')
par(new=TRUE)
plot(Va_cal_fund_1_2015_sig10,ylim=c(min_,max_),type='l',ylab='',col=mycolor[2])
par(new=TRUE)     
plot(Va_cal_fund_1_2015_sig15,ylim=c(min_,max_),type='l',ylab='',col=mycolor[3])
par(new=TRUE)     
plot(Va_cal_fund_1_2015_sig20,ylim=c(min_,max_),type='l',ylab='',col=mycolor[4])
par(new=TRUE)     
plot(Va_cal_fund_1_2015_sig25,ylim=c(min_,max_),type='l',ylab='',col=mycolor[5])
par(new=TRUE)     
plot(Va_cal_fund_1_2015_sig30,ylim=c(min_,max_),type='l',ylab='',col=mycolor[6])
par(new=TRUE)
plot(fund_1_A[start_2015:end_2015],ylim=c(min_,max_),type='l',ylab='',col='black')


legend('bottomright',legend=c('sig=0.05','sig=0.10','sig=0.15','sig=0.20','sig=0.25','sig=0.30','actual'),
       col=c(mycolor[1],mycolor[2],mycolor[3],mycolor[4],mycolor[5],mycolor[6],'black'),
       lty=1,cex=1)
dev.off()



