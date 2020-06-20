function f=MT(ht, N, mu, r, sig, xrfs, lambda, alphag, alphal, pis, cmax, x, k)

Nmax=ceil(10*x/cmax/ht);
A=zeros(N,1);

for i=1:N
    ram=randn(1,Nmax);
    A1=zeros(Nmax,1);
    A2=zeros(Nmax,1);
    A1(1)=x;
    A2(1)=xrfs;
    
    for j=2:Nmax
        A1(j)=A1(j-1)+A1(j-1)*((r+pis*(mu-r))*ht+sig*pis*sqrt(ht)*ram(j))-cmax*ht;
        A2(j)=A2(j-1)+((alphag-alphal)*ind(A1(j-1),A2(j-1))+alphal)*(A1(j-1)-A2(j-1))*ht-cmax*ht;
    end
    
        in=0;
    for j=1:Nmax
        if A1(j)<=0 
            break;
        else
            in=in+1;
        end
    end

    for j=1:in
   A(i)=A(i)+exp(-r*j*ht)*((1+lambda)*ind(cmax,k*A2(j))-lambda)*abs(cmax-k*A2(j))*ht;
    end

    
end
    
f=mean(A);  
