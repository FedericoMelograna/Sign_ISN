# Sign_ISN
Project on the significance assessment of ISNs
The goal of the project is to assess the ability to detect outlier -  based on modular structure - of various literature and newly developed methods.

Firstly, we need to run the simulations. On a server with slurm integrated:
```
./Simulations/Microbiome_simulations/Code/sbatch_Server_microbiome_run_parallel.sh #For Microbiome
# It RUNS 
./Simulations/Microbiome_simulations/Code/Server_microbiome_run_parallel.R
```
To simulate data that mimic microbiome data and, on those data, evaluate the various methods of our analysis. kNN, Cook's based, OTS methdos, OPTICS, SSN-m, MultiLOO-ISN and LOO-ISN.
A similar pipeline can be run for Normality-based simulations

Then, we need to wrap up all the parallelized runs with 

```
# step 1
./Simulations/Microbiome_simulations/Code/Wrapper_runs/Wrapper_all_runs_togheter_1.R
# step 2 calculating the median values
./Simulations/Microbiome_simulations/Code/Wrapper_runs/Computing_median_values_2.R
# step 3
./Simulations/Microbiome_simulations/Code/Wrapper_runs/Results_byk_3
```

Hence, to create graphical representation of the performance, we can run 
```
./Simulations/Microbiome_simulations/Code/Plotting_graphs_byk

```

