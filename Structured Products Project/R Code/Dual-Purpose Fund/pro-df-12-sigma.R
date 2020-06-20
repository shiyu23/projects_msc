#test sigma
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


sig_1_2015_roll = matrix(0,end_2015-start_2015+1,1)
sig_2_2015_roll = matrix(0,end_2015-start_2015+1,1)
for(ii in start_2015:end_2015 ){
  sig_1_2015_roll[ii+1-start_2015]=sigma(data$Date[ii-60],data$Date[ii-1],1)
  sig_2_2015_roll[ii+1-start_2015]=sigma(data$Date[ii-60],data$Date[ii-1],2)
}


sig_1_2019_roll = matrix(0,end_2019_1-start_2019+1,1)
sig_2_2019_roll = matrix(0,end_2019_2-start_2019+1,1)
for(ii in start_2019:end_2019_1 ){
  sig_1_2019_roll[ii+1-start_2019]=sigma(data$Date[ii-60],data$Date[ii-1],1)
}
for(ii in start_2019:end_2019_2 ){
  sig_2_2019_roll[ii+1-start_2019]=sigma(data$Date[ii-60],data$Date[ii-1],2)
}


path=getwd()
png(paste(path,"/sigma_rolling_60.png",sep=""),
    width=10,height=5,units='in',res=500)

par(mfrow=c(2,2))
max_=max(sig_1_2015_roll,sig_1_2015)
min_=min(sig_1_2015_roll,sig_1_2015)
plot(sig_1_2015_roll,ylab='',xlab='',type='l', cex.main=0.8, ylim=c(min_,max_),
     main='sigma of fund1 in 2015, blue: by constant, green: by rolling-60',col='green')
par(new=TRUE)
plot(rep(sig_1_2015,dim(sig_1_2015_roll)[1]),ylab='',xlab='',ylim=c(min_,max_),type='l',col='blue')

max_=max(sig_2_2015_roll,sig_2_2015)
min_=min(sig_2_2015_roll,sig_2_2015)
plot(sig_2_2015_roll,ylab='',xlab='',type='l', cex.main=0.8, ylim=c(min_,max_),
     main='sigma of fund2 in 2015, blue: by constant, green: by rolling-60',col='green')
par(new=TRUE)
plot(rep(sig_2_2015,dim(sig_2_2015_roll)[1]),ylab='',xlab='',ylim=c(min_,max_),type='l',col='blue')

max_=max(sig_1_2019_roll,sig_1_2019)
min_=min(sig_1_2019_roll,sig_1_2019)
plot(sig_1_2019_roll,ylab='',xlab='',type='l', cex.main=0.8, ylim=c(min_,max_),
     main='sigma of fund1 in 2019, blue: by constant, green: by rolling-60',col='green')
par(new=TRUE)
plot(rep(sig_1_2019,dim(sig_1_2019_roll)[1]),ylab='',xlab='',ylim=c(min_,max_),type='l',col='blue')

max_=max(sig_2_2019_roll,sig_2_2019)
min_=min(sig_2_2019_roll,sig_2_2019)
plot(sig_2_2019_roll,ylab='',xlab='',type='l', cex.main=0.8, ylim=c(min_,max_),
     main='sigma of fund2 in 2019, blue: by constant, green: by rolling-60',col='green')
par(new=TRUE)
plot(rep(sig_2_2019,dim(sig_2_2019_roll)[1]),ylab='',xlab='',ylim=c(min_,max_),type='l',col='blue')

dev.off()