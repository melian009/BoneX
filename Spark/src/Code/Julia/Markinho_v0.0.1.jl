using LinearAlgebra # Needed for `vec` and matrix operations

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


# Defines the time-dependent perturbation probability p(t)
# We'll use a sinusoidal function for demonstration.
function get_perturbation_prob(t::Int)
    # The `abs()` ensures the probability is always non-negative.
    # The 0.1 and 0.05 are just example parameters.
    return 0.5 + 0.05 * sin(t)
end

# Calculates the probability of transitioning from a deterministic_next_state
# to a specific perturbed_state_vec, given a time-dependent perturbation probability `p(t)`.
function calculate_perturbation_probability(
    deterministic_next_state::Vector{Bool},
    perturbed_state_vec::Vector{Bool},
    p_t::Float64
)
    num_nodes = length(deterministic_next_state)
    prob = 1.0
    for k in 1:num_nodes
        if deterministic_next_state[k] == perturbed_state_vec[k]
            prob *= (1.0 - p_t)
        else
            prob *= p_t
        end
    end
    return prob
end

# Builds the Time-Dependent Transition Probability Matrix T(t)
function build_time_dependent_tpm(network::BooleanNetwork, p_t::Float64)
    num_states = 2^network.num_nodes
    T = zeros(Float64, num_states, num_states)

    for i_idx in 1:num_states
        current_state_vec = index_to_state(i_idx, network.num_nodes)
        
        deterministic_next_state_vec = Vector{Bool}(undef, network.num_nodes)
        for j in 1:network.num_nodes
            deterministic_next_state_vec[j] = network.rules[j](current_state_vec)
        end
        
        for j_idx in 1:num_states
            perturbed_state_vec = index_to_state(j_idx, network.num_nodes)
            
            T[i_idx, j_idx] = calculate_perturbation_probability(
                deterministic_next_state_vec,
                perturbed_state_vec,
                p_t
            )
        end
    end
    return T
end

# Calculates the next state probability vector using the Markov chain equation
function update_probability_vector(pi_t::Vector{Float64}, T::Matrix{Float64})
    return vec(pi_t' * T)
end

# Runs the time-dependent Markov chain simulation for a specified number of steps
function run_time_dependent_simulation(
    network::BooleanNetwork,
    initial_pi::Vector{Float64},
    num_steps::Int
)
    println("Starting Time-Dependent Simulation for $num_steps steps:")
    current_pi = initial_pi
    
    println("  Initial Probability Distribution: ", round.(current_pi, digits=4))

    for t in 1:num_steps
        p_t = get_perturbation_prob(t)
        T_t = build_time_dependent_tpm(network, p_t) # TPM is re-calculated for each time step
        
        current_pi = update_probability_vector(current_pi, T_t)
        println("  Time step $t (p=$p_t): ", round.(current_pi, digits=4))
    end
    println("Simulation complete.")
    return current_pi
end

# --- Complete Example ---

# Define non-interacting logical rules for a 4-node network (n=4)
rule_node1(state) = !state[2]
rule_node2(state) = !state[2]
rule_node3(state) = !state[2]
rule_node4(state) = !state[2]
#rule_node5(state) = !state[2]

# Create a BooleanNetwork instance
num_nodes = 4
rules = [rule_node1, rule_node2, rule_node3, rule_node4, rule_node5]
my_network = BooleanNetwork(num_nodes, rules)

# Define an initial probability distribution (uniform)
num_states = 2^num_nodes
initial_probability_distribution = ones(num_states) / num_states

# Run the simulation for 10 time steps
final_pi = run_time_dependent_simulation(my_network, initial_probability_distribution, 1000)
