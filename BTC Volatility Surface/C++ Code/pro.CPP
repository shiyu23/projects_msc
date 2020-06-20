#include "ImpliedVol.h"
#include "BSAnalytics.h"
#include "Date.h"
#include <vector>
#include <utility>
#include <iostream>
#include <functional>
#include <cassert>
#include <cmath>
#include <fstream>
#include <sstream>
#include <string>
#include <stdlib.h>


double rfbisect(std::function<double(double)> f, double a, double b, double tol)
{
  assert(a < b && f(a) * f(b) < 0);
  double c;
  while( (b-a)/2 > tol ) {
    c = (a+b) / 2.0;
    if(std::abs(f(c)) < tol)
      return c;
    else {
      if(f(a)*f(c) < 0)
	b = c;
      else
	a = c;
    }
  }
  return c;
}

bool RootBracketing(std::function<double(double)> f, double &a, double &b)
{
  const int NTRY=50;
  const double FACTOR=1.6;
  if (a >= b) throw("wrong input a and b in RootBracketing");
  double f1 = f(a);
  double f2 = f(b);
  for (int j=0;j<NTRY;j++) {
    if (f1*f2 < 0.0) return true;
    if (std::abs(f1) < std::abs(f2))
      f1=f(a += FACTOR*(a-b));
    else
      f2=f(b += FACTOR*(b-a));
  }
  return false;
}

double rootfinding1(double first, double t, double ave, double third){
	auto fun = [=](double vol){return bsPricer(Call, first, t, ave, vol, 0) - third*3874.98;}; // 3,874.98 is BTC-USD exchange rate on March 7
	double a = 0, b = 3;
	RootBracketing(fun, a, b);
	return rfbisect(fun, a, b, 1e-6);
}

double rootfinding2(double first, double t, double ave, double third){
	auto fun = [=](double vol){return bsPricer(Put, first, t, ave, vol, 0) - third*3874.98;};
	double a = 0, b = 3;
	RootBracketing(fun, a, b);
	return rfbisect(fun, a, b, 1e-6);
}

double voladj(string Type, double str, double t, double ave, double bid1, double bid2, double ask1, double ask2){
	double vb1=0, va1=0, vb2=0, va2=0, max1=0, min1=0, max2=0, min2=0;
	double vol=0;
	if(Type=="call"){vb1 = rootfinding1(str, t, ave, bid1);
	                 va1 = rootfinding1(str, t, ave, ask1);
					 vb2 = rootfinding2(str, t, ave, bid2);
	                 va2 = rootfinding2(str, t, ave, ask2);}
	                 else{
                     vb1 = rootfinding2(str, t, ave, bid1);
	                 va1 = rootfinding2(str, t, ave, ask1);
					 vb2 = rootfinding1(str, t, ave, bid2);
	                 va2 = rootfinding1(str, t, ave, ask2);}
					 if(vb1>=va1){max1=vb1;min1=va1;}else{max1=va1;min1=vb1;}
					 if(vb2>=va2){max2=vb2;min2=va2;}else{max2=va2;min2=vb2;}
					 if((min2>=min1)&&(max2<=max1)){vol = (max2+min2)/2;}
				     else if((min2<=min1)&&(max2>=max1)){vol = (max1+min1)/2;}
				     else if((min2<=min1)&&(max2>=min1)&&(max2<=max1)){vol = (min1+max2)/2;}
				     else if((min2>=min1)&&(min2<=max1)&&(max2>=max1)){vol = (min2+max1)/2;}
					 else{std::cout << "There is a call-put arbitage when strike is " << str << ", and maturity " << t <<std::endl;}
	return vol;
							 
}

using namespace std;

int main()
{
// read data
string Type[30][6];
double ONE[12][6];
double TWO[22][6];
double THREE[30][6];
double FOUR[27][6];
double FIVE[29][6];
double SIX[24][6];
	
	int l=0;
	int p=1;
	std::ifstream fin1("2019-03-08 08_00_00 GMT.csv");
    string line; 
	getline(fin1, line);
	while (getline(fin1, line)){istringstream sin(line);string st;
	getline(sin, st, ','); Type[l][0] = st;
	while (getline(sin, st, ',')){ONE[l][p] = atof(st.c_str()); p+=1;} p=1; l+=1;}
	
	l=0; p=1;
	std::ifstream fin2("2019-03-15 08_00_00 GMT.csv");
	getline(fin2, line);
	while (getline(fin2, line)){istringstream sin(line);string st;
	getline(sin, st, ','); Type[l][1] = st;
	while (getline(sin, st, ',')){TWO[l][p] = atof(st.c_str()); p+=1;} p=1; l+=1;}
	
	l=0; p=1;
	std::ifstream fin3("2019-03-29 08_00_00 GMT.csv");
	getline(fin3, line);
	while (getline(fin3, line)){istringstream sin(line);string st;
	getline(sin, st, ','); Type[l][2] = st;
	while (getline(sin, st, ',')){THREE[l][p] = atof(st.c_str()); p+=1;} p=1; l+=1;}
	
	l=0; p=1;
	std::ifstream fin4("2019-04-26 08_00_00 GMT.csv");
	getline(fin4, line);
	while (getline(fin4, line)){istringstream sin(line);string st;
	getline(sin, st, ','); Type[l][3] = st;
	while (getline(sin, st, ',')){FOUR[l][p] = atof(st.c_str()); p+=1;} p=1; l+=1;}
	
	l=0; p=1;
	std::ifstream fin5("2019-06-28 08_00_00 GMT.csv"); 
	getline(fin5, line);
	while (getline(fin5, line)){istringstream sin(line);string st;
	getline(sin, st, ','); Type[l][4] = st;
	while (getline(sin, st, ',')){FIVE[l][p] = atof(st.c_str()); p+=1;} p=1; l+=1;}
	
	l=0; p=1;
	std::ifstream fin6("2019-09-27 08_00_00 GMT.csv");
	getline(fin6, line);
	while (getline(fin6, line)){istringstream sin(line);string st;
	getline(sin, st, ','); Type[l][5] = st;
	while (getline(sin, st, ',')){SIX[l][p] = atof(st.c_str()); p+=1;} p=1; l+=1;}
	

// forward prices average
double price1=0, price2=0, price3=0, price4=0, price5=0, price6=0;
for (int i=0; i<=11; i++){ price1 = price1 + ONE[i][5];} price1=price1/12;
for (int i=0; i<=21; i++){ price2 = price2 + TWO[i][5];} price2=price2/22;
for (int i=0; i<=29; i++){ price3 = price3 + THREE[i][5];} price3=price3/30;
for (int i=0; i<=26; i++){ price4 = price4 + FOUR[i][5];} price4=price4/27;
for (int i=0; i<=28; i++){ price5 = price5 + FIVE[i][5];} price5=price5/29;
for (int i=0; i<=23; i++){ price6 = price6 + SIX[i][5];} price6=price6/24;

// find implied vol
double V1[12], V2[22], V3[30], V4[27], V5[29], V6[24];
for (int i=0; i<=11; i++){
  if(Type[i][0]=="call"){V1[i] = rootfinding1(ONE[i][1], 1.0/365.0, price1, (ONE[i][2]+ONE[i][3])/2);}
  else{V1[i] = rootfinding2(ONE[i][1], 1.0/365.0, price1, (ONE[i][2]+ONE[i][3])/2);}
  }
for (int i=0; i<=21; i++){
  if(Type[i][1]=="call"){V2[i] = rootfinding1(TWO[i][1], 9.0/365.0, price2, (TWO[i][2]+TWO[i][3])/2);}
  else{V2[i] = rootfinding2(TWO[i][1], 9.0/365.0, price2, (TWO[i][2]+TWO[i][3])/2);}
  }
for (int i=0; i<=29; i++){
  if(Type[i][2]=="call"){V3[i] = rootfinding1(THREE[i][1], 23.0/365.0, price3, (THREE[i][2]+THREE[i][3])/2);}
  else{V3[i] = rootfinding2(THREE[i][1], 23.0/365.0, price3, (THREE[i][2]+THREE[i][3])/2);}
  }
for (int i=0; i<=26; i++){
  if(Type[i][3]=="call"){V4[i] = rootfinding1(FOUR[i][1], 51.0/365.0, price4, (FOUR[i][2]+FOUR[i][3])/2);}
  else{V4[i] = rootfinding2(FOUR[i][1], 51.0/365.0, price4, (FOUR[i][2]+FOUR[i][3])/2);}
  }
for (int i=0; i<=28; i++){
  if(Type[i][4]=="call"){V5[i] = rootfinding1(FIVE[i][1], 113.0/365.0, price5, (FIVE[i][2]+FIVE[i][3])/2);}
  else{V5[i] = rootfinding2(FIVE[i][1], 113.0/365.0, price5, (FIVE[i][2]+FIVE[i][3])/2);}
  }
for (int i=0; i<=23; i++){
  if(Type[i][5]=="call"){V6[i] = rootfinding1(SIX[i][1], 204.0/365.0, price6, (SIX[i][2]+SIX[i][3])/2);}
  else{V6[i] = rootfinding2(SIX[i][1], 204.0/365.0, price6, (SIX[i][2]+SIX[i][3])/2);}
  }
  
// call and put with same strike, design the adjustment of the volatility

int j;
for (int i=0; i<11; i++){
	j = i+1;
	if(ONE[i][1]==ONE[j][1]){V1[i] = voladj(Type[i][0], ONE[i][1], 1.0/365.0, price1, ONE[i][2], ONE[j][2], ONE[i][3], ONE[j][3]);V1[j]=V1[i];}
}

for (int i=0; i<21; i++){
	j = i+1;
	if(TWO[i][1]==TWO[j][1]){V2[i] = voladj(Type[i][1], TWO[i][1], 9.0/365.0, price2, TWO[i][2], TWO[j][2], TWO[i][3], TWO[j][3]);V2[j]=V2[i];}
}

for (int i=0; i<29; i++){
	j = i+1;
	if(THREE[i][1]==THREE[j][1]){V3[i] = voladj(Type[i][2], THREE[i][1], 23.0/365.0, price3, THREE[i][2], THREE[j][2], THREE[i][3], THREE[j][3]);V3[j]=V3[i];}
}

for (int i=0; i<26; i++){
	j = i+1;
	if(FOUR[i][1]==FOUR[j][1]){V4[i] = voladj(Type[i][3], FOUR[i][1], 51.0/365.0, price4, FOUR[i][2], FOUR[j][2], FOUR[i][3], FOUR[j][3]);V4[j]=V4[i];}
}

for (int i=0; i<28; i++){
	j = i+1;
	if(FIVE[i][1]==FIVE[j][1]){V5[i] = voladj(Type[i][4], FIVE[i][1], 113.0/365.0, price5, FIVE[i][2], FIVE[j][2], FIVE[i][3], FIVE[j][3]);V5[j]=V5[i];}
}

for (int i=0; i<23; i++){
	j = i+1;
	if(SIX[i][1]==SIX[j][1]){V6[i] = voladj(Type[i][5], SIX[i][1], 204.0/365.0, price6, SIX[i][2], SIX[j][2], SIX[i][3], SIX[j][3]);V6[j]=V6[i];}
}


// build smile
  vector< pair<double, double> > marks;
  for (int i=0; i<=11; i++){if((i==0)||(ONE[i][1]!=ONE[i-1][1])){marks.push_back(pair<double, double>(ONE[i][1], V1[i]));};}
  Smile sm1(marks);
  marks.clear();
  for (int i=0; i<=21; i++){if((i==0)||(TWO[i][1]!=TWO[i-1][1])){marks.push_back(pair<double, double>(TWO[i][1], V2[i]));};}
  Smile sm2(marks);
  marks.clear();
  for (int i=0; i<=29; i++){if((i==0)||(THREE[i][1]!=THREE[i-1][1])){marks.push_back(pair<double, double>(THREE[i][1], V3[i]));};}
  Smile sm3(marks);
  marks.clear();
  for (int i=0; i<=26; i++){if((i==0)||(FOUR[i][1]!=FOUR[i-1][1])){marks.push_back(pair<double, double>(FOUR[i][1], V4[i]));};}
  Smile sm4(marks);
  marks.clear();
  for (int i=0; i<=28; i++){if((i==0)||(FIVE[i][1]!=FIVE[i-1][1])){marks.push_back(pair<double, double>(FIVE[i][1], V5[i]));};}
  Smile sm5(marks);
  marks.clear();
  for (int i=0; i<=23; i++){if((i==0)||(SIX[i][1]!=SIX[i-1][1])){marks.push_back(pair<double, double>(SIX[i][1], V6[i]));};}
  Smile sm6(marks);

  vector< pair<double, Smile> > pillarSmiles;
  pillarSmiles.push_back( pair<double, Smile>(1.0/365.0, sm1) );
  pillarSmiles.push_back( pair<double, Smile>(9.0/365.0, sm2) );  
  pillarSmiles.push_back( pair<double, Smile>(23.0/365.0, sm3) );
  pillarSmiles.push_back( pair<double, Smile>(51.0/365.0, sm4) );  
  pillarSmiles.push_back( pair<double, Smile>(113.0/365.0, sm5) );
  pillarSmiles.push_back( pair<double, Smile>(204.0/365.0, sm6) );  
  ImpliedVol iv(pillarSmiles);
  
// interpolate for forward price
  double Year;
  double Month;
  double Day;
  double t;
  double k;
  std::cout << "Enter maturity (Year) (format: 2019): ";
  std::cin >> Year;
  std::cout << "Enter maturity (Month) (format: 4): ";
  std::cin >> Month;
  std::cout << "Enter maturity (Day) (format: 19): ";
  std::cin >> Day;
  t = Date(Year,Month,Day) - Date(2019, 3, 7);


  std::cout << "Enter strike (USD): ";
  std::cin >> k;

  double s;
  int i;
  for (i=0; i < pillarSmiles.size(); i++ )
  if (t < pillarSmiles[i].first ) break; // i stores the index of the right end of the bracket
  if (i == 0){s = price1;}
  if (i == pillarSmiles.size()){s = price6;}
  double t1 = pillarSmiles[i-1].first;
  double t2 = pillarSmiles[i].first;
  double price[6]; price[1]=price1; price[2]=price2; price[3]=price3; price[4]=price4; price[5]=price5; price[6]=price6;
  s = ((t2 - t) / (t2 - t1))*price[i-1] + (1-((t2 - t) / (t2 - t1)))*price[i];

  double exchange_rate = 3874.98;

  std::cout << "The mid price for call is (BTC)" << bsPricer(Call, k, t, s, iv.Vol(t, k), 0.0)/exchange_rate << std::endl;
  std::cout << "The ask price for call is (BTC)" << bsPricer(Call, k, t, s, iv.Vol(t, k)+0.005, 0.0)/exchange_rate << std::endl;
  std::cout << "The bid price for call is (BTC)" << bsPricer(Call, k, t, s, iv.Vol(t, k)-0.005, 0.0)/exchange_rate << std::endl;
  std::cout << "The delta for call is (BTC)" << (bsPricer(Call, k, t, s+0.001, iv.Vol(t, k), 0.0)-bsPricer(Call, k, t, s-0.001, iv.Vol(t, k), 0.0))/exchange_rate/0.002 << std::endl;
  std::cout << "The vega for call is (USD)" << (bsPricer(Call, k, t, s, iv.Vol(t, k)+0.001, 0.0)-bsPricer(Call, k, t, s, iv.Vol(t, k)-0.001, 0.0))/0.002 << std::endl;
  std::cout << "The theta for call is (USD)" << (bsPricer(Call, k, t+0.001, s, iv.Vol(t+0.001, k), 0.0)-bsPricer(Call, k, t-0.001, s, iv.Vol(t-0.001, k), 0.0))/0.002 << std::endl;
  
  std::cout << "The mid price for put is (BTC)" << bsPricer(Put, k, t, s, iv.Vol(t, k), 0.0)/exchange_rate << std::endl;
  std::cout << "The ask price for put is (BTC)" << bsPricer(Put, k, t, s, iv.Vol(t, k)+0.005, 0.0)/exchange_rate << std::endl;
  std::cout << "The bid price for put is (BTC)" << bsPricer(Put, k, t, s, iv.Vol(t, k)-0.005, 0.0)/exchange_rate << std::endl;
  std::cout << "The delta for Put is (BTC)" << (bsPricer(Put, k, t, s+0.001, iv.Vol(t, k), 0.0)-bsPricer(Put, k, t, s-0.001, iv.Vol(t, k), 0.0))/exchange_rate/0.002 << std::endl;
  std::cout << "The vega for Put is (USD)" << (bsPricer(Put, k, t, s, iv.Vol(t, k)+0.001, 0.0)-bsPricer(Put, k, t, s, iv.Vol(t, k)-0.001, 0.0))/0.002 << std::endl;
  std::cout << "The theta for Put is (USD)" << (bsPricer(Put, k, t+0.001, s, iv.Vol(t+0.001, k), 0.0)-bsPricer(Put, k, t-0.001, s, iv.Vol(t-0.001, k), 0.0))/0.002 << std::endl;
  
  
  // test
  ofstream fout("ImpliedVol--Peng Shiyu.txt");
  for (int i = 0; i < 50; i++) { // row fixed time
    double tstep = 0.01 * i;
    for (int j = 2000; j < 10000; j++) { // column fixed strike
      double k = j;
      fout << iv.Vol(tstep, k) << "\t";
    }
	  fout << "\n";
  }
} 