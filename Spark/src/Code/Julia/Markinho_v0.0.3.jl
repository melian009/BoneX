# SymbolicMarkovBooleanNetwork.jl
using Symbolics
using LinearAlgebra # Needed for the identity matrix I

## 1. Define Core Structures and State Mapping (reusing the same logic)
struct BooleanNetwork
    num_nodes::Int
    rules::Vector{Function}
end

function state_to_index(state::Vector{Bool})
    index = 1
    for i in 1:length(state)
        index += state[i] * 2^(i-1)
    end
    return Int(index)
end

function index_to_state(index::Int, num_nodes::Int)
    return reverse(digits(Bool, index - 1, base=2, pad=num_nodes))
end

## 2. Define Time-Dependent Perturbation and TPM with Symbolic Logic
# Now, we define the perturbation probability `p` as a symbolic variable
@variables p

# Helper function to calculate transition probability for a given a perturbation
function calculate_perturbation_probability_symbolic(
    deterministic_next_state::Vector{Num}, # Note: now works on symbolic variables
    perturbed_state_vec::Vector{Num},
    p_sym::Num
)
    num_nodes = length(deterministic_next_state)
    prob_expression = 1
    for k in 1:num_nodes
        # Use symbolic `ifelse` to handle the conditional logic
        # and symbolic `==` for comparison
        node_prob = ifelse(deterministic_next_state[k] == perturbed_state_vec[k], 1 - p_sym, p_sym)
        prob_expression *= node_prob
    end
    return prob_expression
end

# Builds the Symbolic Transition Probability Matrix (TPM)
function build_symbolic_tpm(network::BooleanNetwork, p_sym::Num)
    num_states = 2^network.num_nodes
    T_sym = Array{Num}(undef, num_states, num_states)

    for i_idx in 1:num_states
        current_state_vec = index_to_state(i_idx, network.num_nodes)
        
        deterministic_next_state_vec = Vector{Num}(undef, network.num_nodes)
        for j in 1:network.num_nodes
            deterministic_next_state_vec[j] = network.rules[j](current_state_vec)
        end
        
        for j_idx in 1:num_states
            # The next state vector for which we are calculating the transition probability
            perturbed_state_vec = index_to_state(j_idx, network.num_nodes)
            
            # --- FIX: Convert the Vector{Bool} to Vector{Num} here ---
            T_sym[i_idx, j_idx] = calculate_perturbation_probability_symbolic(
                deterministic_next_state_vec,
                Num.(perturbed_state_vec), # Corrected line
                p_sym
            )
        end
    end
    return T_sym
end


## 3. The Core Symbolic Steady-State Calculation
# Define the number of nodes for our example

#2-Nodes network
#num_nodes = 2 #Key for speed
#num_states = 2^num_nodes

#4-Nodes network
num_nodes = 4 
num_states = 2^num_nodes # This will now be 16

# Define symbolic variables for the steady-state probabilities (π₁, π₂, π₃, π₄)
@variables pi[1:num_states]

# Define a simple non-interacting network for demonstration

#2-Nodes
# Node 1: Always stays ON
#rule1(state) = true

# Node 2: Always flips its own state
#rule2(state) = !state[2]

#rules = [rule1, rule2]
#my_network = BooleanNetwork(num_nodes, rules)

#4-Nodes: Given interaction network dependencies
# Node 1: Activation by Node 4
rule1(state) = state[4]
# Node 2: Inhibition by Node 1
rule2(state) = !state[1]
# Node 3: Dependent on Node 1 AND Node 2
rule3(state) = state[1] && state[2]
# Node 4: Flips based on Node 3
rule4(state) = !state[3]

rules = [rule1, rule2, rule3, rule4]
my_network = BooleanNetwork(num_nodes, rules)

# Build the symbolic Transition Probability Matrix
@variables p_val
T_sym = build_symbolic_tpm(my_network, p_val)

println("Symbolic Transition Probability Matrix:")
display(T_sym)

# Formulate the system of linear equations: π_ss * T = π_ss
# The core equation is: π_ss * T = π_ss
# This can be rewritten as: π_ss * (T - I) = 0
# We use the transpose (T') to make it a standard column-vector system: (T' - I) * π_ss' = 0

# Create the symbolic matrix for the system (new version)
A = T_sym' - I  # <--- Use T_sym here to fix the error

# Build the vector of linear equations
equations = Vector{Num}(undef, num_states)

# Correctly map the first n-1 equations [cite: 8]
for i in 1:num_states-1
    equations[i] = (A * pi)[i] 
end

# Add the normalization condition [cite: 8]
equations[num_states] = sum(pi) - 1

# Solve and simplify
analytical_solution = Symbolics.solve_for(equations, pi)


#--------- OLD version ----------------------------------------
# Create the symbolic matrix for the system

#A = T' - I

# Calculate the full symbolic vector `A * pi` once to avoid ambiguity

#symbolic_equations_vector = A * pi

# Build a vector of the linear equations. We take the first num_states-1 equations from the result.

#equations = Vector{Num}(undef, num_states)

#for i in 1:num_states-1
#    equations[i] = symbolic_equations_vector[i]
#end

# Add the normalization condition: Σ(pi_i) = 1

#equations[num_states] = sum(pi) - 1


# We use the first `num_states - 1` equations from the system

#for i in 1:num_states-1
#    equations[i] = A[i, :]' * pi
#end

# Add the normalization condition: Σ(pi_i) = 1

#equations[num_states] = sum(pi) - 1

# Solve the system symbolically for the steady-state probabilities

#analytical_solution = Symbolics.solve_for(equations, pi)
#-----------------------------------------------

## 4. Print the Analytical Result

#2-Node network
#println("\nAnalytical Steady-State Probabilities (π_ss):")
#println("π₁ = ", simplify(analytical_solution[1]))
#println("π₂ = ", simplify(analytical_solution[2]))
#println("π₃ = ", simplify(analytical_solution[3]))
#println("π₄ = ", simplify(analytical_solution[4]))

# Now you can substitute a numerical value for `p` to get the numerical result
#p_numerical = 0.05
#numerical_solution = Symbolics.substitute.(analytical_solution, (Dict(p_val => p_numerical),))
#println("\nNumerical Steady-State Probabilities with p = $p_numerical:")
#println("π₁ = ", numerical_solution[1])
#println("π₂ = ", numerical_solution[2])
#println("π₃ = ", numerical_solution[3])
#println("π₄ = ", numerical_solution[4])

#4-Node network
println("\nAnalytical Steady-State Probabilities (π_ss):")
for i in 1:num_states
    println("π$i = ", simplify(analytical_solution[i]))
end


# Now you can substitute a numerical value for `p` to get the numerical result
p_numerical = 0.05
numerical_solution = Symbolics.substitute.(analytical_solution, (Dict(p_val => p_numerical),))
println("\nNumerical Steady-State Probabilities with p = $p_numerical:")
println("π₁ = ", numerical_solution[1])
println("π₂ = ", numerical_solution[2])
println("π₃ = ", numerical_solution[3])
println("π₄ = ", numerical_solution[4])
println("π5 = ", numerical_solution[5])
println("π6 = ", numerical_solution[6])
println("π7 = ", numerical_solution[7])
println("π8 = ", numerical_solution[8])
println("π9 = ", numerical_solution[9])
println("π10 = ", numerical_solution[10])
println("π11 = ", numerical_solution[11])
println("π12 = ", numerical_solution[12])
println("π13 = ", numerical_solution[13])
println("π14 = ", numerical_solution[14])
println("π15 = ", numerical_solution[15])
println("π16 = ", numerical_solution[16])



# Adding Ecosystem services

# 1. Define services for each species (e.g., Species 1 provides 10 units, Species 2 provides 5)
#2-Nodes network
#species_services = [10.0, 5.0] 

#4-Nodes network
species_services = [10.0, 5.0, 2.0, 8.0] 

# 2. Map those services to each of the 16 states
state_services = zeros(num_states)
for i in 1:num_states
    state_vec = index_to_state(i, num_nodes) # Uses your existing mapping logic [cite: 1]
    # Sum services of active species in this state
    state_services[i] = sum(state_vec .* species_services)
end

# 3. Calculate the Expected Ecosystem Service (EES)
# EES = Σ (Probability of State i * Service of State i)
expected_service = sum(numerical_solution .* state_services)
