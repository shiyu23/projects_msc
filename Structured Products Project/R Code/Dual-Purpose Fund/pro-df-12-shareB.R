#compare between B share of fund by lattice and actual
#exp
path=getwd()
png(paste(path,"/comparison_Bshare_by_Explicit_actual.png",sep=""),
    width=10,height=5,units='in',res=500)
par(mfrow=c(2,2))
min_=min(fund_1_B[start_2015:end_2015],2*fund_1[start_2015:end_2015]-Va_cal_fund_1_2015_Ex)
max_=max(fund_1_B[start_2015:end_2015],2*fund_1[start_2015:end_2015]-Va_cal_fund_1_2015_Ex)
plot(2*fund_1[start_2015:end_2015]-Va_cal_fund_1_2015_Ex,ylab='',xlab='',ylim=c(min_,max_),type='l',
    cex.main=0.8, main='B share of fund1 in 2015, blue: by Explicit, red: actual',col='blue')
par(new=TRUE)
plot(fund_1_B[start_2015:end_2015],ylab='',xlab='',ylim=c(min_,max_),type='l',col='red')

min_=min(fund_2_B[start_2015:end_2015],(fund_2[start_2015:end_2015]-0.4*Va_cal_fund_2_2015_Ex)/0.6)
max_=max(fund_2_B[start_2015:end_2015],(fund_2[start_2015:end_2015]-0.4*Va_cal_fund_2_2015_Ex)/0.6)
plot((fund_2[start_2015:end_2015]-0.4*Va_cal_fund_2_2015_Ex)/0.6,ylab='',xlab='',ylim=c(min_,max_),type='l',
    cex.main=0.8, main='B share of fund2 in 2015, blue: by Explicit, red: actual',col='blue')
par(new=TRUE)
plot(fund_2_B[start_2015:end_2015],ylab='',xlab='',ylim=c(min_,max_),type='l',col='red')

min_=min(fund_1_B[start_2019:end_2019_1],2*fund_1[start_2019:end_2019_1]-Va_cal_fund_1_2019_Ex)
max_=max(fund_1_B[start_2019:end_2019_1],2*fund_1[start_2019:end_2019_1]-Va_cal_fund_1_2019_Ex)
plot(2*fund_1[start_2019:end_2019_1]-Va_cal_fund_1_2019_Ex,ylab='',xlab='',ylim=c(min_,max_),type='l',
    cex.main=0.8, main='B share of fund1 in 2019, blue: by Explicit, red: actual',col='blue')
par(new=TRUE)
plot(fund_1_B[start_2019:end_2019_1],ylab='',xlab='',ylim=c(min_,max_),type='l',col='red')

min_=min(fund_2_B[start_2019:end_2019_2],(fund_2[start_2019:end_2019_2]-0.4*Va_cal_fund_2_2019_Ex)/0.6)
max_=max(fund_2_B[start_2019:end_2019_2],(fund_2[start_2019:end_2019_2]-0.4*Va_cal_fund_2_2019_Ex)/0.6)
plot((fund_2[start_2019:end_2019_2]-0.4*Va_cal_fund_2_2019_Ex)/0.6,ylab='',xlab='',ylim=c(min_,max_),type='l',
    cex.main=0.8, main='B share of fund2 in 2019, blue: by Explicit, red: actual',col='blue')
par(new=TRUE)
plot(fund_2_B[start_2019:end_2019_2],ylab='',xlab='',ylim=c(min_,max_),type='l',col='red')

dev.off()

#imp
png(paste(path,"/comparison_Bshare_by_Implicit_actual.png",sep=""),
    width=10,height=5,units='in',res=500)
par(mfrow=c(2,2))
min_=min(fund_1_B[start_2015:end_2015],2*fund_1[start_2015:end_2015]-Va_cal_fund_1_2015_Im)
max_=max(fund_1_B[start_2015:end_2015],2*fund_1[start_2015:end_2015]-Va_cal_fund_1_2015_Im)
plot(2*fund_1[start_2015:end_2015]-Va_cal_fund_1_2015_Im,ylab='',xlab='',ylim=c(min_,max_),type='l',
    cex.main=0.8, main='B share of fund1 in 2015, blue: by Implicit, red: actual',col='blue')
par(new=TRUE)
plot(fund_1_B[start_2015:end_2015],ylab='',xlab='',ylim=c(min_,max_),type='l',col='red')

min_=min(fund_2_B[start_2015:end_2015],(fund_2[start_2015:end_2015]-0.4*Va_cal_fund_2_2015_Im)/0.6)
max_=max(fund_2_B[start_2015:end_2015],(fund_2[start_2015:end_2015]-0.4*Va_cal_fund_2_2015_Im)/0.6)
plot((fund_2[start_2015:end_2015]-0.4*Va_cal_fund_2_2015_Im)/0.6,ylab='',xlab='',ylim=c(min_,max_),type='l',
    cex.main=0.8, main='B share of fund2 in 2015, blue: by Implicit, red: actual',col='blue')
par(new=TRUE)
plot(fund_2_B[start_2015:end_2015],ylab='',xlab='',ylim=c(min_,max_),type='l',col='red')

min_=min(fund_1_B[start_2019:end_2019_1],2*fund_1[start_2019:end_2019_1]-Va_cal_fund_1_2019_Im)
max_=max(fund_1_B[start_2019:end_2019_1],2*fund_1[start_2019:end_2019_1]-Va_cal_fund_1_2019_Im)
plot(2*fund_1[start_2019:end_2019_1]-Va_cal_fund_1_2019_Im,ylab='',xlab='',ylim=c(min_,max_),type='l',
    cex.main=0.8, main='B share of fund1 in 2019, blue: by Implicit, red: actual',col='blue')
par(new=TRUE)
plot(fund_1_B[start_2019:end_2019_1],ylab='',xlab='',ylim=c(min_,max_),type='l',col='red')

min_=min(fund_2_B[start_2019:end_2019_2],(fund_2[start_2019:end_2019_2]-0.4*Va_cal_fund_2_2019_Im)/0.6)
max_=max(fund_2_B[start_2019:end_2019_2],(fund_2[start_2019:end_2019_2]-0.4*Va_cal_fund_2_2019_Im)/0.6)
plot((fund_2[start_2019:end_2019_2]-0.4*Va_cal_fund_2_2019_Im)/0.6,ylab='',xlab='',ylim=c(min_,max_),type='l',
    cex.main=0.8, main='B share of fund2 in 2019, blue: by Implicit, red: actual',col='blue')
par(new=TRUE)
plot(fund_2_B[start_2019:end_2019_2],ylab='',xlab='',ylim=c(min_,max_),type='l',col='red')

dev.off()

#compare between B share of fund by Monte-Carlo and actual
png(paste(path,"/comparison_Bshare_by_Monte-Carlo_actual.png",sep=""),
    width=10,height=5,units='in',res=500)
par(mfrow=c(2,2))
min_=min(fund_1_B[start_2015:end_2015],2*fund_1[start_2015:end_2015]-Va_cal_fund_1_2015_mc)
max_=max(fund_1_B[start_2015:end_2015],2*fund_1[start_2015:end_2015]-Va_cal_fund_1_2015_mc)
plot(2*fund_1[start_2015:end_2015]-Va_cal_fund_1_2015_mc,ylab='',xlab='',ylim=c(min_,max_),type='l',
    cex.main=0.8, main='B share of fund1 in 2015, blue: by Monte-Carlo, red: actual',col='blue')
par(new=TRUE)
plot(fund_1_B[start_2015:end_2015],ylab='',xlab='',ylim=c(min_,max_),type='l',col='red')

min_=min(fund_2_B[start_2015:end_2015],(fund_2[start_2015:end_2015]-0.4*Va_cal_fund_2_2015_mc)/0.6)
max_=max(fund_2_B[start_2015:end_2015],(fund_2[start_2015:end_2015]-0.4*Va_cal_fund_2_2015_mc)/0.6)
plot((fund_2[start_2015:end_2015]-0.4*Va_cal_fund_2_2015_mc)/0.6,ylab='',xlab='',ylim=c(min_,max_),type='l',
    cex.main=0.8, main='B share of fund2 in 2015, blue: by Monte-Carlo, red: actual',col='blue')
par(new=TRUE)
plot(fund_2_B[start_2015:end_2015],ylab='',xlab='',ylim=c(min_,max_),type='l',col='red')

min_=min(fund_1_B[start_2019:end_2019_1],2*fund_1[start_2019:end_2019_1]-Va_cal_fund_1_2019_mc)
max_=max(fund_1_B[start_2019:end_2019_1],2*fund_1[start_2019:end_2019_1]-Va_cal_fund_1_2019_mc)
plot(2*fund_1[start_2019:end_2019_1]-Va_cal_fund_1_2019_mc,ylab='',xlab='',ylim=c(min_,max_),type='l',
    cex.main=0.8, main='B share of fund1 in 2019, blue: by Monte-Carlo, red: actual',col='blue')
par(new=TRUE)
plot(fund_1_B[start_2019:end_2019_1],ylab='',xlab='',ylim=c(min_,max_),type='l',col='red')

min_=min(fund_2_B[start_2019:end_2019_2],(fund_2[start_2019:end_2019_2]-0.4*Va_cal_fund_2_2019_mc)/0.6)
max_=max(fund_2_B[start_2019:end_2019_2],(fund_2[start_2019:end_2019_2]-0.4*Va_cal_fund_2_2019_mc)/0.6)
plot((fund_2[start_2019:end_2019_2]-0.4*Va_cal_fund_2_2019_mc)/0.6,ylab='',xlab='',ylim=c(min_,max_),type='l',
    cex.main=0.8, main='B share of fund2 in 2019, blue: by Monte-Carlo, red: actual',col='blue')
par(new=TRUE)
plot(fund_2_B[start_2019:end_2019_2],ylab='',xlab='',ylim=c(min_,max_),type='l',col='red')

dev.off()


#compare between A share of fund by Monte-Carlo and lattice
png(paste(path,"/comparison_Bshare_by_Monte-Carlo_Lattce.png",sep=""),
    width=10,height=5,units='in',res=500)
par(mfrow=c(2,2))
min_=min(fund_1_B[start_2015:end_2015],2*fund_1[start_2015:end_2015]-Va_cal_fund_1_2015_mc,2*fund_1[start_2015:end_2015]-Va_cal_fund_1_2015_Im)
max_=max(fund_1_B[start_2015:end_2015],2*fund_1[start_2015:end_2015]-Va_cal_fund_1_2015_mc,2*fund_1[start_2015:end_2015]-Va_cal_fund_1_2015_Im)
plot(2*fund_1[start_2015:end_2015]-Va_cal_fund_1_2015_mc,ylab='',xlab='',ylim=c(min_,max_),type='l',cex.main=0.8,
    cex.main=0.8, main='B share of fund1 in 2015, blue: by Monte-Carlo, green: by Implicit, red: actual',col='blue')
par(new=TRUE)
plot(fund_1_B[start_2015:end_2015],ylab='',xlab='',ylim=c(min_,max_),type='l',col='red')
par(new=TRUE)
plot(2*fund_1[start_2015:end_2015]-Va_cal_fund_1_2015_Im,ylab='',xlab='',ylim=c(min_,max_),type='l',col='green')


min_=min(fund_2_B[start_2015:end_2015],(fund_2[start_2015:end_2015]-0.4*Va_cal_fund_2_2015_mc)/0.6,(fund_2[start_2015:end_2015]-0.4*Va_cal_fund_2_2015_Im)/0.6)
max_=max(fund_2_B[start_2015:end_2015],(fund_2[start_2015:end_2015]-0.4*Va_cal_fund_2_2015_mc)/0.6,(fund_2[start_2015:end_2015]-0.4*Va_cal_fund_2_2015_Im)/0.6)
plot((fund_2[start_2015:end_2015]-0.4*Va_cal_fund_2_2015_mc)/0.6,ylab='',xlab='',ylim=c(min_,max_),type='l',cex.main=0.8,
    cex.main=0.8, main='B share of fund2 in 2015, blue: by Monte-Carlo, green: by Implicit, red: actual',col='blue')
par(new=TRUE)
plot(fund_2_B[start_2015:end_2015],ylab='',xlab='',ylim=c(min_,max_),type='l',col='red')
par(new=TRUE)
plot((fund_2[start_2015:end_2015]-0.4*Va_cal_fund_2_2015_Im)/0.6,ylab='',xlab='',ylim=c(min_,max_),type='l',col='green')


min_=min(fund_1_B[start_2019:end_2019_1],2*fund_1[start_2019:end_2019_1]-Va_cal_fund_1_2019_mc,2*fund_1[start_2019:end_2019_1]-Va_cal_fund_1_2019_Im)
max_=max(fund_1_B[start_2019:end_2019_1],2*fund_1[start_2019:end_2019_1]-Va_cal_fund_1_2019_mc,2*fund_1[start_2019:end_2019_1]-Va_cal_fund_1_2019_Im)
plot(2*fund_1[start_2019:end_2019_1]-Va_cal_fund_1_2019_mc,ylab='',xlab='',ylim=c(min_,max_),type='l',cex.main=0.8,
    cex.main=0.8, main='B share of fund1 in 2019, blue: by Monte-Carlo, green: by Implicit, red: actual',col='blue')
par(new=TRUE)
plot(fund_1_B[start_2019:end_2019_1],ylab='',xlab='',ylim=c(min_,max_),type='l',col='red')
par(new=TRUE)
plot(2*fund_1[start_2019:end_2019_1]-Va_cal_fund_1_2019_Im,ylab='',xlab='',ylim=c(min_,max_),type='l',col='green')


min_=min(fund_2_B[start_2019:end_2019_2],(fund_2[start_2019:end_2019_2]-0.4*Va_cal_fund_2_2019_mc)/0.6,(fund_2[start_2019:end_2019_2]-0.4*Va_cal_fund_2_2019_Im)/0.6)
max_=max(fund_2_B[start_2019:end_2019_2],(fund_2[start_2019:end_2019_2]-0.4*Va_cal_fund_2_2019_mc)/0.6,(fund_2[start_2019:end_2019_2]-0.4*Va_cal_fund_2_2019_Im)/0.6)
plot((fund_2[start_2019:end_2019_2]-0.4*Va_cal_fund_2_2019_mc)/0.6,ylab='',xlab='',ylim=c(min_,max_),type='l',cex.main=0.8,
    cex.main=0.8, main='B share of fund2 in 2019, blue: by Monte-Carlo, green: by Implicit, red: actual',col='blue')
par(new=TRUE)
plot(fund_2_B[start_2019:end_2019_2],ylab='',xlab='',ylim=c(min_,max_),type='l',col='red')
par(new=TRUE)
plot((fund_2[start_2019:end_2019_2]-0.4*Va_cal_fund_2_2019_Im)/0.6,ylab='',xlab='',ylim=c(min_,max_),type='l',col='green')

dev.off()
