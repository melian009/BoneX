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
num_nodes = 2
num_states = 2^num_nodes

# Define symbolic variables for the steady-state probabilities (π₁, π₂, π₃, π₄)
@variables pi[1:num_states]

# Define a simple non-interacting network for demonstration
# Node 1: Always stays ON
rule1(state) = true
# Node 2: Always flips its own state
rule2(state) = !state[2]

rules = [rule1, rule2]
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

# Create the symbolic matrix for the system
A = T' - I

# Calculate the full symbolic vector `A * pi` once to avoid ambiguity
symbolic_equations_vector = A * pi

# Build a vector of the linear equations. We take the first num_states-1 equations from the result.
equations = Vector{Num}(undef, num_states)
for i in 1:num_states-1
    equations[i] = symbolic_equations_vector[i]
end

# Add the normalization condition: Σ(pi_i) = 1
equations[num_states] = sum(pi) - 1


# We use the first `num_states - 1` equations from the system
for i in 1:num_states-1
    equations[i] = A[i, :]' * pi
end

# Add the normalization condition: Σ(pi_i) = 1
equations[num_states] = sum(pi) - 1

# Solve the system symbolically for the steady-state probabilities
analytical_solution = Symbolics.solve_for(equations, pi)

## 4. Print the Analytical Result
println("\nAnalytical Steady-State Probabilities (π_ss):")
println("π₁ = ", simplify(analytical_solution[1]))
println("π₂ = ", simplify(analytical_solution[2]))
println("π₃ = ", simplify(analytical_solution[3]))
println("π₄ = ", simplify(analytical_solution[4]))

# Now you can substitute a numerical value for `p` to get the numerical result
p_numerical = 0.05
numerical_solution = Symbolics.substitute.(analytical_solution, (Dict(p_val => p_numerical),))
println("\nNumerical Steady-State Probabilities with p = $p_numerical:")
println("π₁ = ", numerical_solution[1])
println("π₂ = ", numerical_solution[2])
println("π₃ = ", numerical_solution[3])
println("π₄ = ", numerical_solution[4])




#Output
julia> include("Markinho_v0.0.2.jl")
Symbolic Transition Probability Matrix:
4×4 Matrix{Num}:
           p_val^2  p_val*(1 - p_val)  p_val*(1 - p_val)      (1 - p_val)^2
 p_val*(1 - p_val)            p_val^2      (1 - p_val)^2  p_val*(1 - p_val)
           p_val^2  p_val*(1 - p_val)  p_val*(1 - p_val)      (1 - p_val)^2
 p_val*(1 - p_val)            p_val^2      (1 - p_val)^2  p_val*(1 - p_val)
ERROR: LoadError: TypeError: non-boolean (Num) used in boolean context
A symbolic expression appeared in a Boolean context. This error arises in situations where Julia expects a Bool, like 
if boolean_condition		 use ifelse(boolean_condition, then branch, else branch)
x && y				 use x & y
boolean_condition ? a : b	 use ifelse(boolean_condition, a, b)
but a symbolic expression appeared instead of a Bool. For help regarding control flow with symbolic variables, see https://docs.sciml.ai/ModelingToolkit/dev/basics/FAQ/#How-do-I-handle-if-statements-in-my-symbolic-forms?
Stacktrace:
 [1] copyto!(dest::OffsetArrays.OffsetMatrix{Any, Matrix{Any}}, src::Symbolics.Arr{Num, 2})
   @ Base ./abstractarray.jl:1055
 [2] copymutable_oftype(A::Symbolics.Arr{Num, 2}, ::Type{Any})
   @ LinearAlgebra /snap/julia/145/share/julia/stdlib/v1.11/LinearAlgebra/src/LinearAlgebra.jl:447
 [3] +(A::Symbolics.Arr{Num, 2}, J::UniformScaling{Int64})
   @ LinearAlgebra /snap/julia/145/share/julia/stdlib/v1.11/LinearAlgebra/src/uniformscaling.jl:216
 [4] -(A::Symbolics.Arr{Num, 2}, J::UniformScaling{Bool})
   @ LinearAlgebra /snap/julia/145/share/julia/stdlib/v1.11/LinearAlgebra/src/uniformscaling.jl:159
 [5] top-level scope
   @ ~/Research/projects/BoneX/Spark/src/Code/Julia/Markinho_v0.0.2.jl:103
 [6] include(fname::String)
   @ Main ./sysimg.jl:38
 [7] top-level scope
   @ REPL[6]:1
in expression starting at /home/melihan/Research/projects/BoneX/Spark/src/Code/Julia/Markinho_v0.0.2.jl:103


