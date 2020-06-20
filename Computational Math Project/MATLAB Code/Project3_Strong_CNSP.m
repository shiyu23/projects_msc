function f=Project3_Strong_CNSP(h, k, epsilon, gammay, k2, gammaz, delta, T)
N=ceil(20/h); % spatial discretization
Nt=T/k; % time discretization
k2=(delta*gammaz)^(4/5)*epsilon^(4/2)*(4*pi/(15*gammay))^(1/5)*5/7; % k2 in strong case
phi=zeros(N+2,N+2,Nt+1);
A=zeros((N+2)^2,(N+2)^2);
F=zeros((N+2)^2,1);
s=1;
phihatsingle=zeros(N,N);
phihat=zeros(N,N);
dxxphisingle=zeros(N,N);
dxxphi=zeros(N+2,N+2);
dyyphisingle=zeros(N,N);
dyyphi=zeros(N+2,N+2);
xxsingle=zeros(N,N);
xx=zeros(N,N,N,N);
yysingle=zeros(N,N);
yy=zeros(N,N,N,N);

% Initial conditions
for q=0:N+1
    for j=0:N+1
phi(q+s,j+s)=exp(-((q*h-10)^2+(j*h-10)^2)/2/epsilon)/sqrt(pi*epsilon);
    end
end

% forward recursion
for t=0:Nt

for q=0:N+1
    for j=0:N+1
        
% xx spectral differential operator approximation
for l1=-N/2:N/2-1
    for l2=-N/2:N/2-1
for q1=0:N-1
    for j1=0:N-1
    phihatsingle(q1+s,j1+s)=exp(-1i*(2*pi*l1/20)*(q1*h)-1i*(2*pi*l2/20)*(j1*h))*phi(q1+s,j1+s);
    end
end
    phihat(floor(l1+N/2+s),floor(l2+N/2+s))=sum(phihatsingle(:));
    end
end

for l1=-N/2:N/2-1
    for l2=-N/2:N/2-1
    dxxphisingle(floor(l1+N/2+s),floor(l2+N/2+s))=(2*pi*l1/20)^2*phihat(floor(l1+N/2+s),floor(l2+N/2+s))*exp(1i*(20*pi*l1/20)*(q*h)+1i*(20*pi*l2/20)*(j*h));
    end
end
    dxxphi(q+s,j+s)=sum(dxxphisingle(:));

% yy spectral differential operator approximation
for l1=-N/2:N/2-1
    for l2=-N/2:N/2-1
    dyyphisingle(floor(l1+N/2+s),floor(l2+N/2+s))=(2*pi*l2/20)^2*phihat(floor(l1+N/2+s),floor(l2+N/2+s))*exp(1i*(20*pi*l1/20)*(q*h)+1i*(20*pi*l2/20)*(j*h));
    end
end
    dyyphi(q+s,j+s)=sum(dyyphisingle(:));
    end
end

% define matrix A
for q=1:N
    for j=1:N

for q1=0:N-1
    for j1=0:N-1
        
for l1=-N/2:N/2-1
    for l2=-N/2:N/2-1
    xxsingle(floor(l1+N/2+s),floor(l2+N/2+s))=(2*pi*l1/20)^2*exp(-1i*(2*pi*l1/20)*(q1*h)-1i*(2*pi*l2/20)*(j1*h))*exp(1i*(2*pi*l1/20)*(q*h)+1i*(2*pi*l2/20)*(j*h));
    end
end
    xx(q+s,j+s,q1+s,j1+s)=sum(xxsingle(:));
            
for l1=-N/2:N/2-1
    for l2=-N/2:N/2-1
    yysingle(floor(l1+N/2+s),floor(l2+N/2+s))=(2*pi*l2/20)^2*exp(-1i*(2*pi*l1/20)*(q1*h)-1i*(2*pi*l2/20)*(j1*h))*exp(1i*(2*pi*l1/20)*(q*h)+1i*(2*pi*l2/20)*(j*h));
    end
end
    yy(q+s,j+s,q1+s,j1+s)=sum(yysingle(:));
    
    A(q*(N+2)+j+s,q1*(N+2)+j1+s)=epsilon^2/4*(xx(q+s,j+s,q1+s,j1+s)+yy(q+s,j+s,q1+s,j1+s));
    end
end
    A(q*(N+2)+j+s,q*(N+2)+j+s)=A(q*(N+2)+j+s,q*(N+2)+j+s)-1i*epsilon/k+((q*h-10)^2+gammay^2*(j*h-10)^2)/4+k2/2*abs(phi(q+s,j+s))^2;
    
    % define vector F
    F(q*(N+2)+j+s)=-1i*epsilon/k*phi(q+s,j+s)-epsilon^2/4*(dxxphi(q+s,j+s)+dyyphi(q+s,j+s))-((q*h-10)^2+gammay^2*(j*h-10)^2)/4*phi(q+s,j+s)-k2/2*abs(phi(q+s,j+s))^2*phi(q+s,j+s);
    end
end

% Boundary conditions
for q=0:N-1
A((q+1)*(N+2)+s,q*(N+2)+s)=1;
A((q+1)*(N+2)+s,q*(N+2)+N+s)=-1;
A((q+1)*(N+2)+N+1+s,q*(N+2)+1+s)=1;
A((q+1)*(N+2)+N+1+s,q*(N+2)+(N+1)+s)=-1;
end
    
for j=0:N+1
A(j+s,j+s)=1;
A(j+s,N*(N+2)+j+s)=-1;
A((N+1)*(N+2)+j+s,1*(N+2)+j+s)=1;
A((N+1)*(N+2)+j+s,(N+1)*(N+2)+j+s)=-1;
end
    
    % reshape the solution of the linear system
    P=reshape(A\F,N+2,[]);
    phi=P';
end
    f=abs(phi).^2;

% plot the spatial figure (time fixed)
x=(0:N)*h-10;  
y=(0:N)*h-10;  
[X,Y]=meshgrid(x,y);
Z=f(round((x+10)/h)+s,round((y+10)/h)+s);
mesh(X,Y,Z)