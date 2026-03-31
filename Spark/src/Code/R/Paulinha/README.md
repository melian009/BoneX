---
editor_options: 
  markdown: 
    wrap: 72
---

# BoneX

-   Ecological networks as boolean networks in dynamic landscapes

\*\* Boolean binary networks represent species states, presence/absence,
making interactions-connectivity time-dependent. We explore the dynamics
of time-dependent on-off interactions on the dynamics of ecological
networks. A second stage includes the on-off environmental state as a
function of dynamic landscapes, i.e., a periodic function determines the
probability and strength of the interaction if the environment is on,
otherwise the interaction can not be realized \*\*\*

# Manuscript (ms folder)

Summary of the discussions - All summary of the discussions are
available at
[Overleaf](https://overleaf.ifisc.uib-csic.es/6116321349smynngdcnqdw#3f3ec5) -
A report of the work in progress is available at
[Overleaf](https://www.overleaf.com/project/69a095b3380590f49d4f7d48)

# Codes (src folder)

# Extended BoneX

## Spark SNSF "Boolean networks as a way to explore dynamics in ecological assemblages under environmental change"

## ❓ Research Questions

-   How cost and benefit functions structure the coexistence of species.
-   How ecosystem services respond to environmental changes?

## 📚 References

-   All reference studies (PDFs) are available at
    [Switchdrive](https://drive.switch.ch/index.php/s/ugTzvLiNUEnUsil)

## Extended literature

-   All additional reference studies available at
    [Switchdrive](https://drive.switch.ch/index.php/s/HqAVT5vOfb4LDzb)

## 🧾 Data

-   All empirical data are available at
    [Switchdrive](https://drive.switch.ch/index.php/s/lSkaJI2bSeHuzMc)

## 🧑‍💻 Code

## R/Paulinha folder

### **Supportive functions**

-   **functions.R** contains the main functions to setup the system:

    -   *unit_vector()* normalizes a vector to sum to one (used when
        building ES matrix)

    -   *build_random_graph()* creates a bipartite random graph based on
        number of nodes in each set and expected connectance; can return
        either the bipartite network or the adjacency (square) matrix

    -   *define_alpha()* defines the fixed environmental effect; input
        is species' optimum; the function defines the deviation from a
        uniformly distributed environmental effect

    -   *define_fitness()* estimates the biotic and abiotic fitness
        components separately given a matrix of benefits, costs and a
        vector (of physiological costs)

    -   *create_ES_random_matrix()* the random ecosystem service network
        assumes a proportion of species contribute to each service; it
        takes as input the number of species, number of services, and
        the proportion of species contributing to each service

-   **functions_betaD.R** additional supportive functions when modeling
    costs and benefits using the Beta Distribution

    -   *kl_beta()* estimates the Kullback-Leibler Divergence between
        two beta distributions; takes as input parameters alpha and beta
        from both distributions -- all positive values; Reference doi:
        10.3390/e26110959

    -   *mean_beta()* estimates the first moment (mean) of a beta
        distribution given input parameters alpha and beta

    -   *diff_mean_beta()* estimates the difference between the mean two
        beta distributions (input are parameters alpha and beta of each
        distribution)

    -   *create_CB_beta()* creates the cost/benefit matrix based on the
        adjancency matrix following a beta distribution; input is the
        interaction matrix, which can be bipartite or not, shape1 is the
        alpha of the beta distribution and shape2 is the beta parameter

    -   *boolean_model()* runs the model, takes as input the number of
        species in each category of a bipartite system; the expected
        connectance; alpha (environmental cost); and the parameters of
        the beta distribution associated with costs, benefits and the
        physiological (abiotic) cost; the output is a list with

        (i) the community matrix where rows represent time steps and
            columns are species, 1 represents presence of species, 0
            absence;
        (ii) A matrix of interactions (square); (iii) B matrix of
             benefits;
        (iii) C matrix of costs; and (v) costs_p physiological costs for
              each species.

### **Basic analysis**

-   **diff_beta.R** simulates the model given different parameterizations of 
    the beta distribution and computes the proportion of surviving species 
    given differences between the distribution of costs and benefits. The 
    difference between the distributions is given by the difference between
    the first moment of the distribution (mean expected value)
    
-   **diff_lognorm.R** simulates the model given different parameterizations of 
    the beta distribution and computes the proportion of surviving species 
    given differences between the distribution of costs and benefits.
    
    
    
### **Advanced analysis**: Null models, dynamic simulations, and pattern

```         
detection (part of the summary)
```

## ✏️ Draft

*This section is open for sketches, conceptual models, or future
directions.* - [Manuscript draft](link%20to%20draft) - [Conceptual
diagram](link%20to%20flow%20charts%20and%20diagrams)
