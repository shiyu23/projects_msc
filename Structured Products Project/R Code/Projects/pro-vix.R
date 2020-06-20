data1 = read.csv('Options-on-SPX1.csv')
data2 = read.csv('Options-on-SPX2.csv')
data3 = read.csv('Options-on-SPX3.csv')

# implied vol
T1 = (854+900+24*60*6)/525600
T2 = (854+900+24*60*19)/525600
T3 = (854+510+24*60*34)/525600

s0 = 2727.72
ind1 = which.min(abs(data1[,4]-data1[,10]))
ind2 = which.min(abs(data2[,4]-data2[,10]))
ind3 = which.min(abs(data3[,4]-data3[,10]))
r1 = log(data1[ind1,1]/(s0-data1[ind1,4]+data1[ind1,10]))/T1  ## if there is dividend, r1 = r1-q1
r2 = log(data2[ind2,1]/(s0-data2[ind2,4]+data2[ind2,10]))/T2
r3 = log(data3[ind3,1]/(s0-data3[ind3,4]+data3[ind3,10]))/T3
F1 = exp(r1*T1)*(data1[ind1,4]-data1[ind1,10])+data1[ind1,1]
F2 = exp(r2*T2)*(data2[ind2,4]-data2[ind2,10])+data2[ind2,1]
F3 = exp(r3*T3)*(data3[ind3,4]-data3[ind3,10])+data3[ind3,1]

impvol1 = matrix(0, nrow(data1),1)
impvol2 = matrix(0, nrow(data2),1)
impvol3 = matrix(0, nrow(data3),1)

Iter = 50

for(i in 1:nrow(data1)){
  if(data1[i,1]>F1){ ## out of money
f<-function(sig){
  d1 = (log(s0/data1[i,1])+(r1+sig^2/2)*T1)/sig/sqrt(T1)
  d2 = (log(s0/data1[i,1])+(r1-sig^2/2)*T1)/sig/sqrt(T1)
  f = s0*pnorm(d1,0,1)-data1[i,1]*exp(-r1*T1)*pnorm(d2,0,1) - data1[i,4]
}

a = 0
b = 2
thed = 10^-6
q = 0
x = 0
repeat{
    if(f((a+b)/2)==0){impvol1[i] = (a+b)/2}
    if(f((a+b)/2)*f(b)<0){a = (a+b)/2}
    if(f((a+b)/2)*f(b)>0){b = (a+b)/2}
    if(abs(b-a)<thed){q=1; impvol1[i] = (a+b)/2}
    x = x+1
  if((q==1)||(x>=Iter)) break
}
  }
  
else{
f<-function(sig){
  d1 = (log(s0/data1[i,1])+(r1+sig^2/2)*T1)/sig/sqrt(T1)
  d2 = (log(s0/data1[i,1])+(r1-sig^2/2)*T1)/sig/sqrt(T1)
  f = -s0*pnorm(-d1,0,1)+data1[i,1]*exp(-r1*T1)*pnorm(-d2,0,1) - data1[i,10]
}

a = 0
b = 2
thed = 10^-6
q = 0
x = 0
repeat{
  if(f((a+b)/2)==0){impvol1[i] = (a+b)/2}
  if(f((a+b)/2)*f(b)<0){a = (a+b)/2}
  if(f((a+b)/2)*f(b)>0){b = (a+b)/2}
  if(abs(b-a)<thed){q=1; impvol1[i] = (a+b)/2}
  x = x+1
  if((q==1)||(x>=Iter)) break
}
  }
}
impvol1 = 100*impvol1
plot(data1[,1], impvol1, xlab = "strike", ylab = "implied volatility for option (call, 18-May-2018)")

## local vol
T=matrix(0, 3,1); T[1]=T1; T[2]=T2; T[3]=T3

f1<-function(k){
  l=0
  for (i in 1:nrow(data1)){if (data1[i,1]>=k){l=i; break}}
  if (l==1){f1=data1[i,4]}
  if (l==0){f1=data1[nrow(data1),4]}
  if ((l!=0)&&(l!=1)) {f1=(k-data1[l-1,1])/(data1[l,1]-data1[l-1,1])*data1[l,4]+(data1[l,1]-k)/(data1[l,1]-data1[l-1,1])*data1[l-1,4]}
}

f2<-function(k){
  l=0
  for (i in 1:nrow(data2)){if (data2[i,1]>=k){l=i; break}}
  if (l==1){f2=data2[i,4]}
  if (l==0){f2=data2[nrow(data2),4]}
  if ((l!=0)&&(l!=1)) {f2=(k-data2[l-1,1])/(data2[l,1]-data2[l-1,1])*data2[l,4]+(data2[l,1]-k)/(data2[l,1]-data2[l-1,1])*data2[l-1,4]}
}

f3<-function(k){
  l=0
  for (i in 1:nrow(data3)){if (data3[i,1]>=k){l=i; break}}
  if (l==1){f3=data3[i,4]}
  if (l==0){f3=data3[nrow(data3),4]}
  if ((l!=0)&&(l!=1)) {f3=(k-data3[l-1,1])/(data3[l,1]-data3[l-1,1])*data3[l,4]+(data3[l,1]-k)/(data3[l,1]-data3[l-1,1])*data3[l-1,4]}
}

ff<-function(t,k){
  l=0
  for (i in 1:3){if (T[i]>=t){l=i; break}}
  if (l==1){ff=f1(k)}
  if (l==0){ff=f3(k)}
  if (l==2){ff=(t-T[1])/(T[2]-T[1])*f2(k)+(T[2]-t)/(T[2]-T[1])*f1(k)}
  if (l==3){ff=(t-T[2])/(T[3]-T[2])*f3(k)+(T[3]-t)/(T[3]-T[2])*f2(k)}
  return (ff)
}

rr<-function(t){
  l=0
  for (i in 1:3){if (T[i]>=t){l=i; break}}
  if (l==1){rr=r1}
  if (l==0){rr=r3}
  if (l==2){rr=(t-T[1])/(T[2]-T[1])*r2+(T[2]-t)/(T[2]-T[1])*r1}
  if (l==3){rr=(t-T[2])/(T[3]-T[2])*r3+(T[3]-t)/(T[3]-T[2])*r2}
  return (rr)
}

t = 0.05
k = 3000
dt=0.01
dk=50
locvol = sqrt((((ff(t+dt,k)-ff(t-dt,k))/2/dt)+rr(t)*k*((ff(t,k+dk)-ff(t,k-dk))/2/dk))/(k^2*((ff(t,k+dk)+ff(t,k-dk)-2*ff(t,k))/dk^2)))


## vix
sig2 = 0
sig3 = 0

for(i in 1:nrow(data2))
{if(data2[i,1]>F2&&data2[i,2]!=0){
  if(data2[i+1,1]!=0){sig2 = sig2+2/T2*((data2[i+1,1]-data2[i-1,1])/2/data2[i,1]^2)*exp(r2*T2)*((data2[i,4]+data2[i,10])/2)}
  else{sig2 = sig2+2/T2*((data2[i,1]-data2[i-1,1])/data2[i,1]^2)*exp(r2*T2)*((data2[i,4]+data2[i,10])/2)}
}
  if(data2[i,1]<F2&&data2[i,8]!=0){
    if(data2[i-1,1]!=0){sig2 = sig2+2/T2*((data2[i+1,1]-data2[i-1,1])/2/data2[i,1]^2)*exp(r2*T2)*((data2[i,4]+data2[i,10])/2)}
    else{sig2 = sig2+2/T2*((data2[i+1,1]-data2[i,1])/data2[i,1]^2)*exp(r2*T2)*((data2[i,4]+data2[i,10])/2)}
  }
  
  fi = 0
  for (j in nrow(data2):1){
    if (data2[j,1]<=F2) {k0 = data2[j,1]; fi=1}
    if (fi==1) break
    }
  sig2 = sig2-1/T2*(F2/k0-1)^2
}

for(i in 1:nrow(data3))
{if(data3[i,1]>F3&&data3[i,2]!=0){
  if(data3[i+1,1]!=0){sig3 = sig3+2/T3*((data3[i+1,1]-data3[i-1,1])/2/data3[i,1]^2)*exp(r3*T3)*((data3[i,4]+data3[i,10])/2)}
  else{sig3 = sig3+2/T3*((data3[i,1]-data3[i-1,1])/data3[i,1]^2)*exp(r3*T3)*((data3[i,4]+data3[i,10])/2)}
}
  if(data3[i,1]<F3&&data3[i,8]!=0){
    if(data3[i-1,1]!=0){sig3 = sig3+2/T3*((data3[i+1,1]-data3[i-1,1])/2/data3[i,1]^2)*exp(r3*T3)*((data3[i,4]+data3[i,10])/2)}
    else{sig3 = sig3+2/T3*((data3[i+1,1]-data3[i,1])/data3[i,1]^2)*exp(r3*T3)*((data3[i,4]+data3[i,10])/2)}
  }
  
  fi = 0
  for (j in nrow(data3):1){
    if (data3[j,1]<=F3) {k0 = data3[j,1]; fi=1}
    if (fi==1) break
  }
  sig3 = sig3-1/T3*(F3/k0-1)^2
}

int = ((854+510+24*60*34)-43200)/((854+510+24*60*34)-(854+900+24*60*19))
VIX = 100*sqrt(525600/43200*(T2*sig2*int+T3*sig3*(1-int)))