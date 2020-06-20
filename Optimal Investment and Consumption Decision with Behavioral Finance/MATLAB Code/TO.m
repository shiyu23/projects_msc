function f=TO(hx, hxrf, ht, N, mu, r, sig, xmax, xrfmin, xrfmax, lambda, alphag, alphal, pimin, pimax, cmax, thred, k)
Nxrf=ceil((xrfmax-xrfmin)/hxrf); 
Nx=ceil(xmax/hx); 
phi=zeros(Nx,Nxrf);
c=cmax*ones(Nx,Nxrf);
pi=zeros(Nx,Nxrf);
cir=0;

for i=1:Nxrf
    phi(1,i)=0;
    phi(Nx,i)=MT(ht, N, mu, r, sig, xrfmin+(i-1)*hxrf, lambda, alphag, alphal, pimin, cmax, xmax, k);
end
for i=2:(Nx-1)
    phi(i,1)=MT(ht, N, mu, r, sig, xrfmin, lambda, alphag, alphal, pimin, cmax, xmax*i/Nx, k);
end


while cir==0  

    co=zeros(Nx*Nxrf);
    F=zeros(Nx*Nxrf,1);
    phin=phi;
for i=2:(Nx-1) 
    for j=2:(Nxrf-1)
co((i-1)*Nxrf+j, (i-1)*Nxrf+j)=-r-(i-1)*r+((i-1)*hx-(j-1)*hxrf)*(-ind((i-1)*hx,(j-1)*hxrf)*alphag/hxrf+(1-ind((i-1)*hx,(j-1)*hxrf))*alphal/hxrf)-pi(i,j)*(i-1)*(mu-r)-pi(i,j)^2*sig^2*(i-1)^2-c(i,j)/hx-c(i,j)/hxrf;
co((i-1)*Nxrf+j, i*Nxrf+j)=(i-1)*r+pi(i,j)*(i-1)*(mu-r)+1/2*pi(i,j)^2*sig^2*(i-1)^2;
co((i-1)*Nxrf+j, (i-2)*Nxrf+j)=1/2*pi(i,j)^2*sig^2*(i-1)^2+c(i,j)/hx;
co((i-1)*Nxrf+j, (i-1)*Nxrf+j+1)=ind((i-1)*hx,(j-1)*hxrf)*((i-1)*hx-(j-1)*hxrf)*alphag/hxrf;
co((i-1)*Nxrf+j, (i-1)*Nxrf+j-1)=-(1-ind((i-1)*hx,(j-1)*hxrf))*((i-1)*hx-(j-1)*hxrf)*alphal/hxrf+c(i,j)/hxrf;
F((i-1)*Nxrf+j)=-((1+lambda)*ind(c(i,j),k*(j-1)*hxrf)-lambda)*abs(c(i,j)-k*(j-1)*hxrf);
    end
end
for i=2:(Nx-1)
    j=Nxrf;
co((i-1)*Nxrf+j, (i-1)*Nxrf+j)=-r-(i-1)*r+((i-1)*hx-(j-1)*hxrf)*alphal/hxrf-pi(i,j)*(i-1)*(mu-r)-pi(i,j)^2*sig^2*(i-1)^2-c(i,j)/hx-c(i,j)/hxrf;
co((i-1)*Nxrf+j, i*Nxrf+j)=(i-1)*r+pi(i,j)*(i-1)*(mu-r)+1/2*pi(i,j)^2*sig^2*(i-1)^2;
co((i-1)*Nxrf+j, (i-2)*Nxrf+j)=1/2*pi(i,j)^2*sig^2*(i-1)^2+c(i,j)/hx;
co((i-1)*Nxrf+j, (i-1)*Nxrf+j-1)=((i-1)*hx-(j-1)*hxrf)*alphal/hxrf+c(i,j)/hxrf;
F((i-1)*Nxrf+j)=-((1+lambda)*ind(c(i,j),k*(j-1)*hxrf)-lambda)*abs(c(i,j)-k*(j-1)*hxrf);
end
for j=1:Nxrf
co(j, j)=1;
F(j)=phi(1,j);
co((Nx-1)*Nxrf+j, (Nx-1)*Nxrf+j)=1;
F((Nx-1)*Nxrf+j)=phi(Nx,j);
end
for i=2:(Nx-1)
co((i-1)*Nxrf+1, (i-1)*Nxrf+1)=1;
F((i-1)*Nxrf+1)=phi(i,1);
end
phi=transpose(reshape(co\F, Nxrf, Nx));

for i=2:(Nx-1)
    for j =2:(Nxrf-1)
if pimin*(i-1)*(mu-r)*(phi(i+1,j)-phi(i-1,j))/2+1/2*pimin^2*sig^2*(i-1)^2*(phi(i+1,j)+phi(i-1,j)-2*phi(i,j)) == max([pimin*(i-1)*(mu-r)*(phi(i+1,j)-phi(i-1,j))/2+1/2*pimin^2*sig^2*(i-1)^2*(phi(i+1,j)+phi(i-1,j)-2*phi(i,j)),pimax*(i-1)*(mu-r)*(phi(i+1,j)-phi(i-1,j))/2+1/2*pimax^2*sig^2*(i-1)^2*(phi(i+1,j)+phi(i-1,j)-2*phi(i,j)),-(mu-r)^2*(phi(i+1,j)-phi(i-1,j))^2/4/2/sig^2/(phi(i+1,j)+phi(i-1,j)-2*phi(i,j))/hx])*ind1(-(mu-r)*(phi(i+1,j)-phi(i-1,j))/2/sig^2/((i-1)*hx)/(phi(i+1,j)+phi(i-1,j)-2*phi(i,j)),pimin,pimax) + max([pimin*(i-1)*(mu-r)*(phi(i+1,j)-phi(i-1,j))/2+1/2*pimin^2*sig^2*(i-1)^2*(phi(i+1,j)+phi(i-1,j)-2*phi(i,j)),pimax*(i-1)*(mu-r)*(phi(i+1,j)-phi(i-1,j))/2+1/2*pimax^2*sig^2*(i-1)^2*(phi(i+1,j)+phi(i-1,j)-2*phi(i,j))])*(1-ind1(-(mu-r)*(phi(i+1,j)-phi(i-1,j))/2/sig^2/((i-1)*hx)/(phi(i+1,j)+phi(i-1,j)-2*phi(i,j)),pimin,pimax))
    pi(i,j)=pimin;
elseif pimax*(i-1)*(mu-r)*(phi(i+1,j)-phi(i-1,j))/2+1/2*pimax^2*sig^2*(i-1)^2*(phi(i+1,j)+phi(i-1,j)-2*phi(i,j)) == max([pimin*(i-1)*(mu-r)*(phi(i+1,j)-phi(i-1,j))/2+1/2*pimin^2*sig^2*(i-1)^2*(phi(i+1,j)+phi(i-1,j)-2*phi(i,j)),pimax*(i-1)*(mu-r)*(phi(i+1,j)-phi(i-1,j))/2+1/2*pimax^2*sig^2*(i-1)^2*(phi(i+1,j)+phi(i-1,j)-2*phi(i,j)),-(mu-r)^2*(phi(i+1,j)-phi(i-1,j))^2/4/2/sig^2/(phi(i+1,j)+phi(i-1,j)-2*phi(i,j))/hx])*ind1(-(mu-r)*(phi(i+1,j)-phi(i-1,j))/2/sig^2/((i-1)*hx)/(phi(i+1,j)+phi(i-1,j)-2*phi(i,j)),pimin,pimax) + max([pimin*(i-1)*(mu-r)*(phi(i+1,j)-phi(i-1,j))/2+1/2*pimin^2*sig^2*(i-1)^2*(phi(i+1,j)+phi(i-1,j)-2*phi(i,j)),pimax*(i-1)*(mu-r)*(phi(i+1,j)-phi(i-1,j))/2+1/2*pimax^2*sig^2*(i-1)^2*(phi(i+1,j)+phi(i-1,j)-2*phi(i,j))])*(1-ind1(-(mu-r)*(phi(i+1,j)-phi(i-1,j))/2/sig^2/((i-1)*hx)/(phi(i+1,j)+phi(i-1,j)-2*phi(i,j)),pimin,pimax))
    pi(i,j)=pimax;
else
    pi(i,j)=-(mu-r)*(phi(i+1,j)-phi(i-1,j))/2/sig^2/((i-1)*hx)/(phi(i+1,j)+phi(i-1,j)-2*phi(i,j));
end
if 0 == max(0, cmax*(1-(phi(i+1,j)-phi(i-1,j))/2))
    c(i,j)=0;
else
    c(i,j)=cmax;
end
    end
end
disp(max(max(abs(phin-phi))));
if (max(max(abs(phin-phi)))<thred)
    break
end

end


mat=ones(Nx,Nxrf,3);
mat(:,:,1)=phi;
mat(:,:,2)=pi;
mat(:,:,3)=c;
f=mat;

