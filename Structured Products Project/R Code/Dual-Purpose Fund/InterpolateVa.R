InterpolateVa = function(Va_matrix,s,t,hu,hd,sub_nt,nt)
{
  #Notes:
  #This function uses gridded value of share A to
  #interpolate to get the value of share A
  #with given s, price of fund and given time t
  
  #Inputs:
  #Va_matrix: gridded matrix of share A value
  #s: given price of fund
  #t: time between 0 and 1
  #hu: upside reset value for fund price
  #hd: downside reset value for share B value
  #sub_nt: the trading grid in a day
  #nt*sub_nt: the grid number of time
  
  #Outputs:
  #the interpolated value of share A
  
  ns = dim(Va_matrix)[1]
  tnt = dim(Va_matrix)[2]
  
  max = 3  #by default
  min = 0   #by default
  ds = (max-min)/(ns-1) #the price delta for each price grid
  s_vector = matrix(0, ns, 1)   # value of fund price from min to max
  for (i in 1:ns) {s_vector[i]=min+(i-1)*ds}
  
  t_vector = matrix(0,tnt,1)
  dt = 1/tnt
  for (i in 1:tnt) {t_vector[i]=(i-1)*dt}  
  
  idx_t_2_ = which(t_vector>=t)[1] 
  idx_t_1_ = idx_t_2_ - 1
  idx_s_2_ = which(s_vector>=s)[1] 
  idx_s_1_ = idx_s_2_ - 1
  
  idx_s_1 = max(1,idx_s_1_)
  idx_s_2 = min(ns,idx_s_2_)
  idx_t_1 = max(1,idx_t_1_)
  idx_t_2 = min(tnt,idx_t_2_)
  
  
  
  v11 = Va_matrix[idx_s_1,idx_t_1]
  v12 = Va_matrix[idx_s_1,idx_t_2]
  v21 = Va_matrix[idx_s_2,idx_t_1]
  v22 = Va_matrix[idx_s_2,idx_t_2]
  
  s1 = s_vector[idx_s_1]
  s2 = s_vector[idx_s_2]
  t1 = t_vector[idx_t_1]
  t2 = t_vector[idx_t_2]
  
  ###########################
  ## t     from 0 to 1
  ## s     
  ## min
  ##       v11        v12
  ## to    v_1   v__  v_2
  ##       v21        v22  
  ## max   
  ##########################


  if ((t==t2)&&(s==s2)) {return(v22)}
  else if ((t==t2)&&(s!=s2)) {return((v22-v12) / (s2-s1) * (s-s1) + v12)}
  else if ((t!=t2)&&(s==s2)) {return((v22-v21) / (t2-t1) * (t-t1) + v21)}
  else{
    v_1 = (v21-v11) / (s2-s1) * (s-s1) + v11
    v_2 = (v22-v12) / (s2-s1) * (s-s1) + v12
    
    v__ = (v_2-v_1) / (t2-t1) * (t-t1) + v_1
    
    return(v__)
  }
  
}