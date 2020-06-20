# compare error
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

# lattice share A
print("lattice share A")
mean(abs(fund_1_A[start_2015:end_2015]-Va_cal_fund_1_2015_Ex))
max(abs(fund_1_A[start_2015:end_2015]-Va_cal_fund_1_2015_Ex))
mean(abs(fund_1_A[start_2019:end_2019_1]-Va_cal_fund_1_2019_Ex))
max(abs(fund_1_A[start_2019:end_2019_1]-Va_cal_fund_1_2019_Ex))
mean(abs(fund_2_A[start_2015:end_2015]-Va_cal_fund_2_2015_Ex))
max(abs(fund_2_A[start_2015:end_2015]-Va_cal_fund_2_2015_Ex))
mean(abs(fund_2_A[start_2019:end_2019_2]-Va_cal_fund_2_2019_Ex))
max(abs(fund_2_A[start_2019:end_2019_2]-Va_cal_fund_2_2019_Ex))

cor(fund_1_A[start_2015:end_2015],Va_cal_fund_1_2015_Ex)[1]
cor(fund_1_A[start_2019:end_2019_1],Va_cal_fund_1_2019_Ex)[1]
cor(fund_2_A[start_2015:end_2015],Va_cal_fund_2_2015_Ex)[1]
cor(fund_2_A[start_2019:end_2019_2],Va_cal_fund_2_2019_Ex)[1]

# lattice share B
print("lattice share B")
Va_cal_fund_1_2015_Ex_B = (fund_1[start_2015:end_2015] - 0.5*Va_cal_fund_1_2015_Ex)/0.5
Va_cal_fund_1_2019_Ex_B = (fund_1[start_2019:end_2019_1] - 0.5*Va_cal_fund_1_2019_Ex)/0.5
Va_cal_fund_2_2015_Ex_B = (fund_2[start_2015:end_2015] - 0.4*Va_cal_fund_2_2015_Ex)/0.6
Va_cal_fund_2_2019_Ex_B = (fund_2[start_2019:end_2019_2] - 0.4*Va_cal_fund_2_2019_Ex)/0.6

mean(abs(fund_1_B[start_2015:end_2015]-Va_cal_fund_1_2015_Ex_B))
max(abs(fund_1_B[start_2015:end_2015]-Va_cal_fund_1_2015_Ex_B))
mean(abs(fund_1_B[start_2019:end_2019_1]-Va_cal_fund_1_2019_Ex_B))
max(abs(fund_1_B[start_2019:end_2019_1]-Va_cal_fund_1_2019_Ex_B))
mean(abs(fund_2_B[start_2015:end_2015]-Va_cal_fund_2_2015_Ex_B))
max(abs(fund_2_B[start_2015:end_2015]-Va_cal_fund_2_2015_Ex_B))
mean(abs(fund_2_B[start_2019:end_2019_2]-Va_cal_fund_2_2019_Ex_B))
max(abs(fund_2_B[start_2019:end_2019_2]-Va_cal_fund_2_2019_Ex_B))


cor(fund_1_B[start_2015:end_2015],Va_cal_fund_1_2015_Ex_B)[1]
cor(fund_1_B[start_2019:end_2019_1],Va_cal_fund_1_2019_Ex_B)[1]
cor(fund_2_B[start_2015:end_2015],Va_cal_fund_2_2015_Ex_B)[1]
cor(fund_2_B[start_2019:end_2019_2],Va_cal_fund_2_2019_Ex_B)[1]



print("MC share A")
mean(abs(fund_1_A[start_2015:end_2015]-Va_cal_fund_1_2015_mc))
max(abs(fund_1_A[start_2015:end_2015]-Va_cal_fund_1_2015_mc))
mean(abs(fund_1_A[start_2019:end_2019_1]-Va_cal_fund_1_2019_mc))
max(abs(fund_1_A[start_2019:end_2019_1]-Va_cal_fund_1_2019_mc))
mean(abs(fund_2_A[start_2015:end_2015]-Va_cal_fund_2_2015_mc))
max(abs(fund_2_A[start_2015:end_2015]-Va_cal_fund_2_2015_mc))
mean(abs(fund_2_A[start_2019:end_2019_2]-Va_cal_fund_2_2019_mc))
max(abs(fund_2_A[start_2019:end_2019_2]-Va_cal_fund_2_2019_mc))


cor(fund_1_A[start_2015:end_2015],Va_cal_fund_1_2015_mc)
cor(fund_1_A[start_2019:end_2019_1],Va_cal_fund_1_2019_mc)
cor(fund_2_A[start_2015:end_2015],Va_cal_fund_2_2015_mc)
cor(fund_2_A[start_2019:end_2019_2],Va_cal_fund_2_2019_mc)


# lattice share B
print("MC share B")
Va_cal_fund_1_2015_mc_B = (fund_1[start_2015:end_2015] - 0.5*Va_cal_fund_1_2015_mc)/0.5
Va_cal_fund_1_2019_mc_B = (fund_1[start_2019:end_2019_1] - 0.5*Va_cal_fund_1_2019_mc)/0.5
Va_cal_fund_2_2015_mc_B = (fund_2[start_2015:end_2015] - 0.4*Va_cal_fund_2_2015_mc)/0.6
Va_cal_fund_2_2019_mc_B = (fund_2[start_2019:end_2019_2] - 0.4*Va_cal_fund_2_2019_mc)/0.6

mean(abs(fund_1_B[start_2015:end_2015]-Va_cal_fund_1_2015_mc_B))
max(abs(fund_1_B[start_2015:end_2015]-Va_cal_fund_1_2015_mc_B))
mean(abs(fund_1_B[start_2019:end_2019_1]-Va_cal_fund_1_2019_mc_B))
max(abs(fund_1_B[start_2019:end_2019_1]-Va_cal_fund_1_2019_mc_B))
mean(abs(fund_2_B[start_2015:end_2015]-Va_cal_fund_2_2015_mc_B))
max(abs(fund_2_B[start_2015:end_2015]-Va_cal_fund_2_2015_mc_B))
mean(abs(fund_2_B[start_2019:end_2019_2]-Va_cal_fund_2_2019_mc_B))
max(abs(fund_2_B[start_2019:end_2019_2]-Va_cal_fund_2_2019_mc_B))


cor(fund_1_B[start_2015:end_2015],Va_cal_fund_1_2015_mc_B)
cor(fund_1_B[start_2019:end_2019_1],Va_cal_fund_1_2019_mc_B)
cor(fund_2_B[start_2015:end_2015],Va_cal_fund_2_2015_mc_B)
cor(fund_2_B[start_2019:end_2019_2],Va_cal_fund_2_2019_mc_B)
