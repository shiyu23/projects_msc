bond<-read.csv("Convertible-110030SH.csv")
stock<-read.csv("Convertible-600185SH.csv")
F = 100
X = 7.24
r = 0.04
q = 0.005
nt = 540  ## one and half year
dt = 1/nt
s0 = 5.28
pp = 103
bc = 1.3*X
sig = 0.3


# (1) no dividend, no put clause, no call clause, no bonus coupon clause
# (2) no dividend, no put clause, no call clause, with bonus coupon clause
# (3) no dividend, no put clause, with call clause, with bonus coupon clause
# (4) no dividend, no put clause, with soft call clause, with bonus coupon clause
# (5) no dividend, with soft put clause, with soft call clause, with bonus coupon clause
# (6) with dividend, no put clause, no call clause, with bonus coupon clause
# (7) with dividend, with soft put clause, with soft call clause, with bonus coupon clause

# (1)-(7) lattice
nm = 100
u = exp(sig*sqrt(dt))
d = 1/u
v = matrix(0, nt,nt)

f <- function(div,c,sc,sp,bo){ 
  if (div==1) {k = F/X/exp(-q*(1.5+3))} else {k = F/X}
  
  if (c==1) {payoff<-function(n) {if (n<=180) {payoff = F + exp(-r*(n+180)/365)} else {payoff = F + exp(-r*(n-180)/365)}}}
  
  if (sc==1) {
    H = matrix(0, nm,1)
    
    for (i in 1:nm)
    {ram = rnorm(nt-1, 0,1)
    spath=matrix(0, nt,1)
    spath[1]=s0
    for (j in 1:(nt-1)){spath[j+1]=spath[j]*exp((r-sig^2/2)*dt+sig*sqrt(dt)*ram[j])}
    
    c=0
    for (j in 1:(nt-30)) {
      day = matrix(0, 30,1)
      for (h in j:(j+29)) {if (spath[h]>=bc) {day[h-j+1] = 1}
      if (sum(day)>=15) {H[i] = spath[j]; c=1; break}
      }
    
    if (c==0){H[i] = spath[nt]}
    }
    }
    
    Hc = mean(H)  ## valid for .. (date)
    payoff<-function(n) {if (n<=180) {payoff = F + exp(-r*(n+180)/365)} else {payoff = F + exp(-r*(n-180)/365)}}
  }
  
  if (sp==1) {
    H = matrix(0, nm,1)
    
    for (i in 1:nm)
    {ram = rnorm(nt-1, 0,1)
    spath=matrix(0, nt,1)
    spath[1]=s0
    for (j in 1:(nt-1)){spath[j+1]=spath[j]*exp((r-sig^2/2)*dt+sig*sqrt(dt)*ram[j])}
    
    c=0
    for (j in 1:(nt-30)) {
      day = matrix(0, 30,1)
      for (h in j:(j+29)) {if (spath[h]<=0.7*X) {day[h-j+1] = 1}
      if (sum(day)>=30) {H[i] = spath[j]; c=1; break}
    }
    
    if (c==0){H[i] = spath[nt]}
    }
    }
    
    Hp = mean(H)
  }
  
  
  if (bo==0){
  for (i in 1:nt) {v[i,nt] = max(F*(1+0.02), k*s0*u^(2*i-nt))}
  if (c==1) {for (i in 1:nt) {v[i,nt] = max(min(F*(1+0.02), payoff(nt)), k*s0*u^(2*i-nt))}}
  if (sc==1) {for (i in 1:nt) {if((s0*u^(2*i-nt))>=Hc) {v[i,nt] = max(min(F*(1+0.02), payoff(nt)), k*s0*u^(2*i-nt))}}}
  if (sp==1) {for (i in 1:nt) {if((s0*u^(2*i-nt))<=Hp) {v[i,nt] = max(F*(1+0.02), k*s0*u^(2*i-nt), pp)}}}
    
    for (i in (nt-1):1){
      for (j in 1:i){
        if (div==1) {
          k = F/X/exp(-q*(1.5+3-i/365))
          p = (exp((r-q)*dt)-d)/(u-d)
          v[j,i] = exp(-r*dt)*(p*v[j+1,i+1] + (1-p)*v[j,i+1])
        }
        else {
          k = F/X
          p = (exp(r*dt)-d)/(u-d)
          v[j,i] = exp(-r*dt)*(p*v[j+1,i+1] + (1-p)*v[j,i+1])
        }
        
        if (c==1) {v[j,i] = max(min(v[j,i], payoff(i)), k*s0*u^(2*j-i))}
        if ((sc==1)&&(s0*u^(2*j-i)>=Hc)) {v[j,i] = max(min(v[j,i], payoff(i)), k*s0*u^(2*j-i))}
        if ((sp==1)&&(s0*u^(2*j-i)<=Hp)) {v[j,i] = max(v[j,i], k*s0*u^(2*j-i), pp)}
        if (i==(nt-365)) {v[j,i] = v[j,i] + F*0.015}
      }
    }
  }else{for (i in 1:nt) {v[i,nt] = max(F*(1+0.06), k*s0*u^(2*i-nt))}
  if (c==1) {for (i in 1:nt) {v[i,nt] = max(min(F*(1+0.06), payoff(nt)), k*s0*u^(2*i-nt))}}
  if (sc==1) {for (i in 1:nt) {if((s0*u^(2*i-nt))>=Hc) {v[i,nt] = max(min(F*(1+0.06), payoff(nt)), k*s0*u^(2*i-nt))}}}
  if (sp==1) {for (i in 1:nt) {if((s0*u^(2*i-nt))<=Hp) {v[i,nt] = max(F*(1+0.06), k*s0*u^(2*i-nt), pp)}}}

    for (i in (nt-1):1){
      
      for (j in 1:i){
        if (div==1) {
          k = F/X/exp(-q*(1.5+3-i/365))
          p = (exp((r-q)*dt)-d)/(u-d)
          v[j,i] = exp(-r*dt)*(p*v[j+1,i+1] + (1-p)*v[j,i+1])
        }
        else {
          k = F/X
          p = (exp(r*dt)-d)/(u-d)
          v[j,i] = exp(-r*dt)*(p*v[j+1,i+1] + (1-p)*v[j,i+1])
        }
        
        if (c==1) {v[j,i] = max(min(v[j,i], payoff(i)), k*s0*u^(2*j-i))}
        if ((sc==1)&&(s0*u^(2*j-i)>=Hc)) {v[j,i] = max(min(v[j,i], payoff(i)), k*s0*u^(2*j-i))}
        if ((sp==1)&&(s0*u^(2*j-i)<=Hp)) {v[j,i] = max(v[j,i], k*s0*u^(2*j-i), pp)}
        if (i==(nt-365)) {v[j,i] = v[j,i] + F*0.015}
      }
    }
  }
  
  
  f=v[1,1]
}

# (1) no dividend, no put clause, no call clause, no bonus coupon clause
price1 = f(0,0,0,0,0)
# (2) no dividend, no put clause, no call clause, with bonus coupon clause
price2 = f(0,0,0,0,1)
# (3) no dividend, no put clause, with call clause, with bonus coupon clause
price3 = f(0,1,0,0,1)
# (4) no dividend, no put clause, with soft call clause, with bonus coupon clause
price4 = f(0,0,1,0,1)
# (5) no dividend, with soft put clause, with soft call clause, with bonus coupon clause
price5 = f(0,0,1,1,1)
# (6) with dividend, no put clause, no call clause, with bonus coupon clause
price6 = f(1,0,0,0,1)
# (7) with dividend, with soft put clause, with soft call clause, with bonus coupon clause
price7 = f(1,0,1,1,1)
