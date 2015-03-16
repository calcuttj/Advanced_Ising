# Advanced_Ising

plotted magnetization vs Temp
but only 1 flip at a time. Take long time to converge
Want algorithm to treat all particles at once
sample over full phase space
speeds up sampling

Wolff (Swindson- Wang)
- Grow cluster and then flip the cluster

Cluster growing procedure

start from arbitrary site inside lattice
grow to a neighbor if has same spin

1-e^(-2J/kT) compare to random number. If > then grow. < kill it.
Once it's killed, that direction is stopped. 

Detailed balance
P_{i} = e^{\epsilon_i / k_B T} / Z

dP/dt = sum(j=/= i) (W_{j->i} P_j - W_{i->j} P_i)  = 0 at equilibrium.\
Set inside of sum to 0. -> detailed balance

Best way to visualize lattice
After we build the cluster, do we have to test if the whole cluster flips based on energy calculation
What is the criteria for critical temperature

