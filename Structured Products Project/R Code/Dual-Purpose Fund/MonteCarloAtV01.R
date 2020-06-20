MonteCarloAtV01 = function(maxt,nm,nt,sub_nt,r,c,R,C,sig,hu,hd,threshold,alpha)
{
  #Notes:
  #This function uses Monte Carlo Approach to calculate 
  #the value of Share A in a dual-purpose fund 
  #at time 0 and underlying value 1
  
  #Inputs:
  #nm: number of samples for Monte Carlo
  #nt: the number of days in a year, 365 for default
  #sub_nt: the trading grid in a day
  #nt*sub_nt: the grid number of time in one year
  #r: continuous interest rate (coupon rate)
  #c: continuous management fee
  #R: annual interest rate (coupon rate)
  #C: annual management fee
  #sig: volatility
  #hu: upside reset value for fund price
  #hd: downside reset value for share B value
  #threshold: the precision of the return
  #alpha: A shares / (A shares + B shares)
  
  #Outputs:
  #v: value of share A at time 0, underlying value 1
  
  x=0
  dt = 1/(nt*sub_nt)  
  tnt = maxt*nt*sub_nt  # maxt years to ensure boundary touched

  v0=0
  v=0
  repeat{
    
      price=matrix(0, nm,1)
      
      for (i in 1:nm)
      { ram=rnorm(tnt-1, 0,1); spath=matrix(0, tnt,1); spath[1]=1
      for (j in 1:(tnt-1)){
        if (j%%365==0) {spath[j+1]=spath[j]*exp((r-c-sig^2/2)*dt+sig*sqrt(dt)*ram[j]) - alpha*R}
        else {spath[j+1]=spath[j]*exp((r-c-sig^2/2)*dt+sig*sqrt(dt)*ram[j])}
      }
      q=0
      for (j in 1:(tnt-1)){if((spath[j]>=hu)||(spath[j]<=(alpha*(1+R*(j*dt-floor(j*dt)))+(1-alpha)*hd))){q=1; break}}
      if (q==1)
      {j=1
      repeat {t1=j;if ((spath[j]>=hu)||(j>=tnt)) break;j=j+1}
      j=1
      repeat {t2=j;if ((spath[j]<=(alpha*(1+R*(j*dt-floor(j*dt)))+(1-alpha)*hd))||(j>=tnt)) break;j=j+1}
      
      if(floor(t1*dt)==0){accR1 = 0} else {accR1 = 0; for(j in 1:floor(t1*dt)){accR1 = accR1 + exp(-r*j)*R}}
      if(floor(t2*dt)==0){accR2 = 0} else {accR2 = 0; for(j in 1:floor(t2*dt)){accR2 = accR2 + exp(-r*j)*R}}
      if (t1<t2){price[i] = accR1 + exp(-r*t1*dt)*(R*(t1*dt-floor(t1*dt))+v0)}
      else {price[i] = accR2 + exp(-r*t2*dt)*(R*(t2*dt-floor(t2*dt))+1-hd+hd*v0)}
      }
      }
      v = mean(price)

    if (abs(v-v0)<threshold){x=1}
    if (x==1) {return (v)}
    else{v0=v}
  }
  
}

  
