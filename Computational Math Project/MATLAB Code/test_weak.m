% O(1)-interations case
h=0.1; k=0.0025; epsilon=1.0; gammay=1.0; k2=-2.0; gammaz=10.0; delta=-1.586; T=0.075;
figure(1)
Project3_Weak_TSSP(h, k, epsilon, gammay, k2, gammaz, delta, T);

h=0.4; k=0.0025; epsilon=1.0; gammay=1.0; k2=-2.0; gammaz=10.0; delta=-1.586; T=0.075;
figure(2)
Project3_Weak_CNFD(h, k, epsilon, gammay, k2, gammaz, delta, T);

h=1; k=0.00005; epsilon=1.0; gammay=1.0; k2=-2.0; gammaz=10.0; delta=-1.586; T=0.00025;
figure(3)
Project3_Weak_CNSP(h, k, epsilon, gammay, k2, gammaz, delta, T);