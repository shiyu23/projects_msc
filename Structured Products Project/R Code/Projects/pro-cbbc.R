# implied vol
s0 = 13.48
r = 0.015

# º£ÓÍûœãyÁãÒ»Ù£Á (17446)
k0 = 16.88
T = 1.5
c = 0.094

Iter = 50
f<-function(sig){
  d1 = (log(s0/k0)+(r+sig^2/2)*T)/sig/sqrt(T)
  d2 = (log(s0/k0)+(r-sig^2/2)*T)/sig/sqrt(T)
  f = s0*pnorm(d1,0,1)-k0*exp(-r*T)*pnorm(d2,0,1) - c
}

a = 0
b = 2
thed = 10^-6
q = 0
x = 0
repeat{
  if(f((a+b)/2)==0){impvol = (a+b)/2}
  if(f((a+b)/2)*f(b)<0){a = (a+b)/2}
  if(f((a+b)/2)*f(b)>0){b = (a+b)/2}
  if(abs(b-a)<thed){q=1; impvol = (a+b)/2}
  x = x+1
  if((q==1)||(x>=Iter)) break
}



## monte-carlo
nm = 100
r = 0.015
q = 0.005
sig = impvol
s0 = 13.48
k = 11.8
H = 12
dt = 1/365*1/48  # half hour interval
T = 0.5
nt = T/dt
price = matrix(0, nm,1)


for (i in 1:nm){
  ram=rnorm(nt-1, 0,1); spath=matrix(0, nt,1); spath[1]=s0
  for (j in 1:(nt-1)){spath[j+1]=spath[j]*exp((r-q-sig^2/2)*dt+sig*sqrt(dt)*ram[j])}
  
  rep = 0
  for (j in 1:(nt-48)){
    if ((((j%%48)>=1)&&((j%%48)<=5))||(((j%%48)>=8)&&((j%%48)<=13))){
    if (spath[j]<=H) {
    if (((j%%48)>=1)&&((j%%48)<=5)) {t=(j-j%%48+13)*dt; price[i] = exp(-r*t)*max(min(min(spath[(j-j%%48+1):(j-j%%48+5)]),min(spath[(j-j%%48+8):(j-j%%48+13)]))-k,0)}  ## rebate happens at am
    else {t=(j-j%%48+48+5)*dt; price[i] = exp(-r*t)*max(min(min(spath[(j-j%%48+8):(j-j%%48+13)]),min(spath[(j-j%%48+48+1):(j-j%%48+48+5)]))-k,0)} ## rebate happens at pm, when gives the rebate
    rep = 1 
    break}
    }
  }
  if (rep==0) {price[i] = exp(-r*T)*max(spath[nt]-k,0)}
}

mean(price)


## binomial
dt = 1/365*1/6  # 4 hours interval
T = 0.5
nt = T/dt
u = exp(sig*sqrt(dt))
d = 1/u
v = matrix(0, nt,nt)
s = matrix(0, nt,1)
p = (exp((r-q)*dt)-d)/(u-d)

for (i in 1:nt) {s[i] = s0*u^(2*i-nt)}

for (i in 1:nt) {v[i,nt] = max(s[i]-k,0)}

for (i in (nt-1):1) {
  for (j in 1:i) {v[j,i] = exp(-r*dt)*(p*v[j+1,i+1] + (1-p)*v[j,i+1])}
  if (((i%%6)==1)||((i%%6)==2)){
    if ((i%%6)==1) {
      for (j in 1:i) {
        sl = s0*u^(2*j-i)
        if (sl<=H) {v[j,i] = exp(-r*dt)*(p*max(sl-k,0)+(1-p)*max(sl*d-k,0))}
      }
    }
    else {
      for (j in 1:i) {
        sl = s0*u^(2*j-i)
        if (sl<=H) {
          u1 = exp(sig*sqrt(5*dt))
          d1 = 1/u1
          p1 = (exp((r-q)*5*dt)-d1)/(u1-d1)
          v[j,i] = exp(-r*5*dt)*(p1*max(sl-k,0)+(1-p1)*max(sl*d1-k,0))
        }
      }
    }
  }
}

price = v[1,1]
