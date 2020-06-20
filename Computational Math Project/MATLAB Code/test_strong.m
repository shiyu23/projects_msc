% Strong interations
h=1/51.2; k=0.00005; epsilon=0.3; gammay=1.0; k2=-1.9718; gammaz=10.0; delta=-7.545; T=40;
figure(1)
Project3_Strong_TSSP(h, k, epsilon, gammay, k2, gammaz, delta, T);
figure(2)
Project3_Strong_CNFD(h, k, epsilon, gammay, k2, gammaz, delta, T);
figure(3)
Project3_Strong_CNSP(h, k, epsilon, gammay, k2, gammaz, delta, T);