# INVASIBILITY MARKOV CHAIN --------------------------
# AIM: PREDICVT PRESENCE-ABSENCE VECTOR FOR SPECIES I 
#-----------------------------------------------------

using Random
using Plots
using QuantEcon #TOREAD: https://julia.quantecon.org/introduction_dynamics/finite_markov.html

# 1. DATA: Time-dependent distance matrix and presence-absence matrix 


S=1 
for s = 1:2 #S species
psi = rand(Float64, (1, 3))

# 2. INITIAL EXPECTED STATE = [0.0 0.2 0.8] # initial distribution (presence absence) = one species 


# 3. TIME-DEPENDENT MIGRATION TRANSITION MATRIX Pond X Pond: 1/distance where distacnce comes from distance matrix
# STATES: Ps, Pns, where P pond, s sampled, ns, nonsampled, temporary and permanent

P = [0.971 0.029 0.000
     0.145 0.778 0.077
     0.000 0.508 0.492] # stochastic matrix (Dispersal matrix) == P square matrix, size P = number ponds
#P = [0.9 0.1 0.0; 0.4 0.4 0.2; 0.1 0.1 0.8];#Dispersal matrix at time t


mc = MarkovChain(P)
is_irreducible(mc)


t = 20 # path length
x_vals = zeros(t)
y_vals = similar(x_vals)
z_vals = similar(x_vals)
colors = [repeat([:red], 20); :black] # for plotting

for i in 1:t
    x_vals[i] = psi[1]
    y_vals[i] = psi[2]
    z_vals[i] = psi[3]
    psi = psi * P # update distribution
end

end
mc = MarkovChain(P)
psi_star = stationary_distributions(mc)[1]
x_star, y_star, z_star = psi_star # unpack the stationary dist
plt = scatter([x_vals; x_star], [y_vals; y_star], [z_vals; z_star], color = colors,gridalpha = 0.5, legend = :none)
plot!(plt, camera = (45, 45))

