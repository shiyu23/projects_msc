hx=0.1; hxrf=0.1; ht=0.01; N=50; mu=0.1; r=0.05; sig=0.3; xmax=10; xrfmin=-5; xrfmax=10; alphag=0.8; pimin=0; pimax=1; cmax=10; thred=0.05;
Nxrf=ceil((xrfmax-xrfmin)/hxrf); 
Nx=ceil(xmax/hx); 

% value
% expectation
k=0.5; alphal=0.6; lambda=1.5;
phi=T(hx, hxrf, ht, N, mu, r, sig, xmax, xrfmin, xrfmax, lambda, alphag, alphal, pimin, pimax, cmax, thred, k);
figure(1)
[x,y]=meshgrid(-4.8:0.1:((Nxrf-1)/10-5),0.2:0.1:((Nx-1)/10));
z=zeros(Nx-2,Nxrf-2);
for i=1:(Nx-2)
    for j=1:(Nxrf-2)
z(i,j)=phi(i+1,j+1,1);
    end
end
surf(x,y,z);
% loss-aversion
N0=25;
arg=(1:N0)*0.02+1;
x=1; k=0.5; alphal=0.6; xrf=1;
v=zeros(N0,1);
for n=1:N0   
    lambda = arg(n);
    phi=T(hx, hxrf, ht, N, mu, r, sig, xmax, xrfmin, xrfmax, lambda, alphag, alphal, pimin, pimax, cmax, thred, k);
    v(n)=phi(round(x/hx),round((xrf-xrfmin)/hxrf),1);
end
figure(2)
plot(arg,v);
% adaptation
arg=(1:N0)*0.02+0.3;
x=1; k=0.5; lambda=1.5; xrf=1;
v=zeros(N0,1);
for n=1:N0   
    alphal = arg(n);
    phi=T(hx, hxrf, ht, N, mu, r, sig, xmax, xrfmin, xrfmax, lambda, alphag, alphal, pimin, pimax, cmax, thred, k);
    v(n)=phi(round(x/hx),round((xrf-xrfmin)/hxrf),1);
end
figure(3)
plot(arg,v);
% k   %%%%%%
arg=(1:N0)*0.04;
x=1; lambda=1.5; alphal=0.6; xrf=1;
v=zeros(N0,1);
for n=1:N0   
    k = arg(n);
    phi=T(hx, hxrf, ht, N, mu, r, sig, xmax, xrfmin, xrfmax, lambda, alphag, alphal, pimin, pimax, cmax, thred, k);
    v(n)=phi(round(x/hx),round((xrf-xrfmin)/hxrf),1);
end
figure(4)
plot(arg,v);



% loss-aversion
k=0.5; alphal=0.6;
% loss-aversion // pi //c
  lambda = 1.5;  
  phi=T(hx, hxrf, ht, N, mu, r, sig, xmax, xrfmin, xrfmax, lambda, alphag, alphal, pimin, pimax, cmax, thred, k);
  figure(5)
  heatmap(phi(2:(Nx-1),2:(Nxrf-1),2));
  figure(6)
  heatmap(phi(2:(Nx-1),2:(Nxrf-1),3));
  lambda = 2;  
  phi=T(hx, hxrf, ht, N, mu, r, sig, xmax, xrfmin, xrfmax, lambda, alphag, alphal, pimin, pimax, cmax, thred, k);
  figure(7)
  heatmap(phi(2:(Nx-1),2:(Nxrf-1),2));
  figure(8)
  heatmap(phi(2:(Nx-1),2:(Nxrf-1),3));



% adaptation
k=0.5; lambda=1.5;
% adaptation // pi //c
  alphal = 0.4;  
  phi=T(hx, hxrf, ht, N, mu, r, sig, xmax, xrfmin, xrfmax, lambda, alphag, alphal, pimin, pimax, cmax, thred, k);
  figure(9)
  heatmap(phi(2:(Nx-1),2:(Nxrf-1),2));
  figure(10)
  heatmap(phi(2:(Nx-1),2:(Nxrf-1),3));
  alphal = 0.7;  
  phi=T(hx, hxrf, ht, N, mu, r, sig, xmax, xrfmin, xrfmax, lambda, alphag, alphal, pimin, pimax, cmax, thred, k);
  figure(11)
  heatmap(phi(2:(Nx-1),2:(Nxrf-1),2));
  figure(12)
  heatmap(phi(2:(Nx-1),2:(Nxrf-1),3));



% k
lambda=1.5; alphal=0.6;
% k // pi //c
  k = 0.5;  
  phi=T(hx, hxrf, ht, N, mu, r, sig, xmax, xrfmin, xrfmax, lambda, alphag, alphal, pimin, pimax, cmax, thred, k);
  figure(13)
  heatmap(phi(2:(Nx-1),2:(Nxrf-1),2));
  figure(14)
  heatmap(phi(2:(Nx-1),2:(Nxrf-1),3));
  k = 0.75;  
  phi=T(hx, hxrf, ht, N, mu, r, sig, xmax, xrfmin, xrfmax, lambda, alphag, alphal, pimin, pimax, cmax, thred, k);
  figure(15)
  heatmap(phi(2:(Nx-1),2:(Nxrf-1),2));
  figure(16)
  heatmap(phi(2:(Nx-1),2:(Nxrf-1),3));


% orig
k=0; alphal=0; alphag=0; lambda=1;
phi=TO(hx, hxrf, ht, N, mu, r, sig, xmax, xrfmin, xrfmax, lambda, alphag, alphal, pimin, pimax, cmax, thred, k);
[x,y]=meshgrid(-4.8:0.1:((Nxrf-1)/10-5),0.2:0.1:((Nx-1)/10));
z=zeros(Nx-2,Nxrf-2);
for i=1:(Nx-2)
    for j=1:(Nxrf-2)
z(i,j)=phi(i+1,j+1,1);
    end
end
figure(17)
surf(x,y,z);
figure(18)
heatmap(phi(2:(Nx-1),2:(Nxrf-1),2));
figure(19)
heatmap(phi(2:(Nx-1),2:(Nxrf-1),3));
