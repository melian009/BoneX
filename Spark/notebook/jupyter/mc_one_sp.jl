using Plots
using QuantEcon 
#TOREAD: https://julia.quantecon.org/introduction_dynamics/finite_markov.html
#TOREAD: https://docs.julialang.org/en/v1/stdlib/LinearAlgebra/
#P = [0.9 0.1 0.0; 0.4 0.4 0.2; 0.1 0.1 0.8];#Dispersal matrix at time t
P = [0.971 0.029 0.000
     0.145 0.778 0.077
     0.000 0.508 0.492] # stochastic dispersal matrix

mc = MarkovChain(P)
is_irreducible(mc)
global psi = [0.0 0.2 0.8] # initial distribution (presence absence) = one species

t = 20 # path length
x_vals = zeros(t)
y_vals = similar(x_vals)
z_vals = similar(x_vals)
colors = [repeat([:red], 20); :black] # for plotting

for i in 1:t
    x_vals[i] = psi[1]
    y_vals[i] = psi[2]
    z_vals[i] = psi[3]
    global psi = psi * P # update distribution
end

mc = MarkovChain(P)
psi_star = stationary_distributions(mc)[1]
x_star, y_star, z_star = psi_star # unpack the stationary dist
plt = scatter([x_vals; x_star], [y_vals; y_star], [z_vals; z_star], color = colors,gridalpha = 0.5, legend = :none)
plot!(plt, camera = (45, 45))
savefig("trajectory1.png")

