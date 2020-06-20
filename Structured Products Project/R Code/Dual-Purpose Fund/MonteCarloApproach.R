MonteCarloApproach = function(s,t,V01,maxt,nt,sub_nt,nm,hu,hd,R,C,r,c,sig,alpha)
{
  #Notes:
  #This function uses Monte Carlo Approach to calculate 
  #the value of Share A in a dual-purpose fund,
  #at a given time t(0-1) and given price s of dual-purpose fund
  #according to a Risk Neutral Expectation equation
  
  #Inputs:
  #s: the price of dual-purpose fund at time t
  #t: current time, 1 for one year, 0 for the beginning of the year
  #V01: the value of share A at time 0 and underlying value 1, by MonteCarlo
  #nt: the number of days in a year, 365 for default
  #sub_nt: the trading grid in a day
  #nt*sub_nt: the grid number of time in one year
  #nm: number of samples for Monte Carlo
  #sub_nt: how many breaks during a trading day, 
  #         the larger, the more paths for a Monte Carlo
  #hu: upside reset value for fund price
  #hd: downside reset value for share B value
  #R: coupon rate
  #sig: volatility
  #alpha: A shares / (A shares + B shares)
  
  #Outputs:
  #value of share A with given price s for dual-purpose fund at time t
  
  #parameter seeting
  dt=1/nt/sub_nt
  tnt=floor((maxt-t)/dt)
  deltat=(maxt-t)/tnt # delta of time for each grid

  price=matrix(0, nm, 1)
  
  for (i in 1:nm){
    
    #generate fund price from t to 1
    ram=rnorm(tnt-1,0,1); 
    spath=matrix(0, tnt,1); 
    spath[1]=s
    for (j in 1:(tnt-1)){
      if (j%%365==0) {spath[j+1]=spath[j]*exp((r-c-sig^2/2)*deltat+sig*sqrt(deltat)*ram[j]) - alpha*R}
      else {spath[j+1]=spath[j]*exp((r-c-sig^2/2)*deltat+sig*sqrt(deltat)*ram[j])}
    }
    
    q=0
    for (j in 1:(tnt-1)){ # check if upside or downside is touched
      if((spath[j]>=hu)||(spath[j]<=(alpha*(1+R*(j*deltat+t-floor(j*deltat+t)))+(1-alpha)*hd))){q=1; break}
    }
    if (q==1){ # touch upside or downside
      j=1
      repeat {t1=j;if ((spath[j]>=hu)||(j>=tnt)) break;j=j+1}
      j=1
      repeat {t2=j;if ((spath[j]<=(alpha*(1+R*(j*deltat+t-floor(j*deltat+t)))+(1-alpha)*hd))||(j>=tnt)) break;j=j+1}
      
      if(floor(t1*deltat+t)==0){accR1 = 0} else {accR1 = 0; for(j in 1:floor(t1*deltat+t)){accR1 = accR1 + exp(-r*(j-t))*R}}
      if(floor(t2*deltat+t)==0){accR2 = 0} else {accR2 = 0; for(j in 1:floor(t2*deltat+t)){accR2 = accR2 + exp(-r*(j-t))*R}}
      if (t1<t2){ # upside is touched first
        price[i] = accR1 + exp(-r*t1*deltat)*(R*(t1*deltat+t-floor(t1*deltat+t))+V01)
      }else { # downside is touched first
        price[i] = accR2 + exp(-r*t2*deltat)*(R*(t2*deltat+t-floor(t2*deltat+t))+1-hd+hd*V01)
      }
    }
  }

  return(mean(price))
}


