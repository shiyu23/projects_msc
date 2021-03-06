IRExplicitFiniteDiff = function(ns,tr,sub_nt,hu,hd,r,c,R,C,sig,alpha,v0)
{
  #Notes:
  #This function uses explicit finite difference to calculate 
  #the value of Share A in a dual-purpose fund,
  #according to a PDE for Share A value
  
  #Inputs:
  #ns: the grid number of the dual-purpose fund's value (price)
  #tr: time when reset happens in a year
  #sub_nt: the trading grid in a day
  #nt*sub_nt: the grid number of time
  #hu: upside reset value for fund price
  #hd: downside reset value for share B value
  #R: coupon rate
  #sig: volatility
  #alpha: shares of A / (shares of A+shares of B)
  #v0: shares of A at time 0
  
  #Outputs:
  #matrix, the value of Share A at gridded time between 0 and 1, 
  #with gridded dual fund prices between min(0 by default) and max(hu by default) 
  
  #Parameters setting:
  nt = round((1-tr)*365) #nt: the number of days from reset date to year end
  max = 3  #by default
  min = 0   #by default
  ds = (max-min)/(ns-1) #the price delta for each price grid
  tnt = nt*sub_nt #total number of time grids 
  dt=1/tnt #the time delta for each time grid
  
  v=matrix(0, ns,tnt) # value of A share at time t with fund price s, 
  
  s=matrix(0, ns,1)   # value of fund price from min to max
  for (i in 1:ns) {s[i]=min+(i-1)*ds}
  
  
  Va_0_1 = quantile(v0,1/max) # the value of share A at time 0 with fund price 1
  # since price index of v is increasing with fund price 
  # if max=2, then just get the 50% or median price
  for (i in 1:ns){
    # initializing time 1 for 3 categories
    ht_1 = alpha*(1+R*(1-tr))+(1-alpha)*hd  # H(t) function, when t=1 
    if (s[i]>=hu){
      v[i,tnt]=R*(1-tr)+Va_0_1
    }else if (s[i]<=ht_1){
      v[i,tnt]=R*(1-tr)+1-hd+hd*Va_0_1
    }else { # interpolate S-alpha*R 
      target_s = s[i]-alpha*R*(1-tr)
      ix_2 = which(s>=target_s)[1]  # get ix_1, ix_2 to satisfy
      ix_1 = ix_2-1                 # s[ix_1]<= target_s <= s[ix_2]
      Va_0_target_s = (v0[ix_1]-v0[ix_2]) / (s[ix_1,1]-s[ix_2,1]) * (target_s - s[ix_1,1]) + v0[ix_1]
      v[i,tnt]=R*(1-tr)+Va_0_target_s
    }
  }
  
  # preparation of the matrix coefs for explicit finite difference
  m=matrix(0, ns,ns)
  m[1,1]=1
  m[ns,ns]=1
  D=diag(ns)
  D[1,1]=0
  D[ns,ns]=0
  for (i in 2:(ns-1)){ # need double check ???
    m[i,i-1]=(r-c)*s[i]/2/ds-sig^2*s[i]^2/2/ds^2;
    m[i,i]=r+sig^2*s[i]^2/ds^2; 
    m[i,i+1]=-sig^2*s[i]^2/2/ds^2-(r-c)*s[i]/2/ds; 
  }
  
  # back computation of the valuation of A share
  for (i in (tnt-1):1){
    v[,i]=(D-dt*m)%*%v[,i+1]
    for (j in 1:ns)
      if (s[j]>=hu){
        v[j,i]=R*(1-tr)*(i/tnt)+Va_0_1
      }else if (s[j]<=(alpha*(1+R*(1-tr)*(i/tnt))+(1-alpha)*hd)){
        v[j,i]=R*(1-tr)*(i/tnt)+1-hd+hd*Va_0_1
      }else{}
  }
  
  return(v)
}

