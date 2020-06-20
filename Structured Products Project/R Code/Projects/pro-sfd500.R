data=read.csv("SFD500-stock_price-002727.csv")
s0=5.10
up=1.03*s0
down=0.7*s0
R=0.31
k=1
N=1
r=0.04
sig=0.45


# lattice
ns=60
ds=(3-0.05)*s0/ns
s=matrix(0, ns,1)
smin=s0*0.05
for (i in 1:ns){s[i]=smin+(i-1)*ds}
sd=which.min(abs(s-down))
su=which.min(abs(s-up))
dt=1/365
nt=1/dt
v=matrix(1, ns,1)
vc=matrix(0, ns,1)


v=N*(1+R*357/365)*v
for (i in 1:sd){v[i]=N*min(s[i]/s0,1)}
for (i in su:ns){v[i]=N*(1+R*nt/nt)}

m=matrix(0, ns,ns)
m[1,1]=1
m[ns,ns]=1
D=diag(ns)
D[1,1]=0
D[ns,ns]=0
for (i in 2:(ns-1)){m[i,i-1]=r*s[i]/2/ds-sig^2*s[i]^2/2/ds^2; m[i,i]=r+sig^2*s[i]^2/ds^2; m[i,i+1]=-sig^2*s[i]^2/2/ds^2-r*s[i]/2/ds}

upd=matrix(0, 13,1)
upd[1]=1; upd[2]=23; upd[3]=57; upd[4]=82; upd[5]=110; upd[6]=146; upd[7]=174
upd[8]=203; upd[9]=231; upd[10]=259; upd[11]=294; upd[12]=322; upd[13]=nt

# implicit
for (j in 13:2){
  for (i in (upd[j]-1):upd[j-1]){
    v=solve(D+dt*m)%*%v
    # pay-off changed and have up-barrier
    for (h in 1:(su-1)) {vc[h]=N*min(s[h]/s0,1)}
    for (h in su:ns) {vc[h]=N*(1+R*nt/nt)}
    for (h in (nt-1):i) {
      vc=solve(D+dt*m)%*%vc
      vc[ns]=exp(-r*(upd[which((upd-h)>=0)[1]]-h)/nt)*N*(1+R*upd[which((upd-h)>=0)[1]]/nt)
      vc[1]=exp(-r*(nt-h)/nt)*N*smin/s0
      for (hh in 13:j){if (h==upd[hh]) {for (hhh in su:ns) {vc[hhh]=N*(1+R*h/nt)}}}
    }
    for (h in 1:sd) {v[h]=vc[h]}
    v[ns]=exp(-r*(upd[j]-i)/nt)*N*(1+R*upd[j]/nt)
  }
  if (j!=2) {for (i in su:ns) {v[i]=N*(1+R*upd[j-1]/nt)}}
}

int=which(s>=s0)[1]
pri = (s[int]-s0)/(s[int]-s[int-1])*v[int-1]+(s0-s[int-1])/(s[int]-s[int-1])*v[int]


# monte-carlo
nm=10000
price=matrix(0, nm,1)
expsur=matrix(0, nm,1)

for (i in 1:nm)
{ram=rnorm(nt-1, 0,1)
spath=matrix(0, nt,1)
spath[1]=s0
for (j in 1:(nt-1)){spath[j+1]=spath[j]*exp((r-sig^2/2)*dt+sig*sqrt(dt)*ram[j])}

c=0
for (j in 2:13) {if (spath[upd[j]]>=up) {price[i]=exp(-r*upd[j]/nt)*N*(1+R*upd[j]/nt); c=1; break}}

if (c==0){
  if (min(spath)<=down){price[i]=exp(-r)*N*min(spath[nt]/s0,1)}
  else {price[i]=exp(-r)*N*(1+R*nt/nt)}
}

for (j in 1:nt){if (spath[j]>=up) {expsur[i]=j/nt; break}; expsur[i]=j/nt}
}

median(price)
mean(price)
cov(price)
mean(expsur)