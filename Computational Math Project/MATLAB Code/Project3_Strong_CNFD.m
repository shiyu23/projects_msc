function f=Project3_Strong_CNFD(h, k, epsilon, gammay, k2, gammaz, delta, T)
N=ceil(20/h); % spatial discretization
Nt=T/k; % time discretization
k2=(delta*gammaz)^(4/5)*epsilon^(4/2)*(4*pi/(15*gammay))^(1/5)*5/7; % k2 in strong case
phi=zeros(N+2,N+2);
A=zeros((N+2)^2,(N+2)^2);
F=zeros((N+2)^2,1);
s=1;

% Initial conditions
for q=0:N+1
    for j=0:N+1
phi(q+s,j+s)=exp(-((q*h-10)^2+(j*h-10)^2)/2/epsilon)/sqrt(pi*epsilon);
    end
end

% forward recursion
for t=0:Nt    
    % define matrix A
    for q=1:N
        for j=1:N
 A(q*(N+2)+j+s,(q-1)*(N+2)+j+s)=epsilon^2/4/h^2;
 A(q*(N+2)+j+s,q*(N+2)+j-1+s)=epsilon^2/4/h^2;
 A(q*(N+2)+j+s,q*(N+2)+j+s)=1i*epsilon/k-epsilon^2/h^2-(((q*h-10)^2+gammay^2*(j*h-10)^2)/2+k2*abs(phi(q+s,j+s))^2)/2;
 A(q*(N+2)+j+s,q*(N+2)+j+1+s)=epsilon^2/4/h^2;
 A(q*(N+2)+j+s,(q+1)*(N+2)+j+s)=epsilon^2/4/h^2;
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
    
    % define vector F
    for q=1:N
        for j=1:N
F(q*(N+2)+j+s)=(1i*epsilon/k+epsilon^2/h^2+(((q*h-10)^2+gammay^2*(j*h-10)^2)/2+k2*abs(phi(q+s,j+s))^2)/2)*phi(q+s,j+s)-epsilon^2/4/h^2*(phi(q+1+s,j+s)-epsilon^2/4/h^2*phi(q-1+s,j+s))-epsilon^2/4/h^2*phi(q+s,j+1+s)-epsilon^2/4/h^2*phi(q+s,j-1+s);
        end
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