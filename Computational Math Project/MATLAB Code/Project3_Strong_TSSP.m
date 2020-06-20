function f=Project3_Strong_TSSP(h, k, epsilon, gammay, k2, gammaz, delta, T)
N=ceil(20/h); % spatial discretization
Nt=T/k; % time discretization
k2=(delta*gammaz)^(4/5)*epsilon^(4/2)*(4*pi/(15*gammay))^(1/5)*5/7; % k2 in strong case
phi=zeros(N+1,N+1);
s=1;
star1=zeros(N+1,N+1);
starhatsingle=zeros(N+1,N+1);
starhat=zeros(N,N);
star2single=zeros(N,N);
star2=zeros(N+1,N+1);

% Initial conditions
for q=0:N
    for j=0:N
phi(q+s,j+s)=exp(-((q*h-10)^2+(j*h-10)^2)/2/epsilon)/sqrt(pi*epsilon);
    end
end

% forward recursive through time
for t=0:Nt-1
    % phi star 1 (come from solving ODE)
    for q=0:N
        for j=0:N
    star1(q+s,j+s)=exp(-1i*(((q*h-10)^2/2+gammay^2*(j*h-10)^2/2)+k2*abs(phi(q+s,j+s))^2)*k/2/epsilon)*phi(q+s,j+s);
        end
    end
    
    % the fourier coefficient of phi star 1
    for l1=-N/2:N/2-1
        for l2=-N/2:N/2-1 
    for q=0:N
        for j=0:N
    starhatsingle(q+s,j+s)=star1(q+s,j+s)*exp(-1i*(2*pi*l1/20)*(q*h)-1i*(2*pi*l2/20)*(j*h));
        end
    end
    starhat(floor(l1+N/2)+s,floor(l2+N/2)+s)=sum(starhatsingle(:)); 
        end
    end
    
    for q=0:N
        for j=0:N
    for l1=-N/2:N/2-1
        for l2=-N/2:N/2-1
    star2single(floor(l1+N/2)+s,floor(l2+N/2)+s)=(1/N^2)*exp(-1i*epsilon*k*(2*pi*l1/20)^2/2-1i*epsilon*k*(2*pi*l2/20)^2/2)*starhat(floor(l1+N/2)+s,floor(l2+N/2)+s)*exp(1i*(2*pi*l1/20)*(q*h)+1i*(2*pi*l2/20)*(j*h));
        end
    end
    star2(q+s,j+s)=sum(star2single(:));
        end
    end
    
    % substitute the star 2 to the solution of the ODE, gain the value of the next time step
    for q=0:N
        for j=0:N
    phi(q+s,j+s)=exp(-1i*(((q*h-10)^2/2+gammay^2*(j*h-10)^2/2)+k2*abs(star2(q+s,j+s))^2)*k/2/epsilon)*star2(q+s,j+s);
        end
    end
end
    f=abs(phi).^2;

% plot the spatial figure (time fixed)
x=(0:N)*h-10;  
y=(0:N)*h-10;  
[X,Y]=meshgrid(x,y);  
Z=f(round((x+10)/h)+s,round((y+10)/h)+s);
mesh(X,Y,Z)