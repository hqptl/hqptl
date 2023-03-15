# Reproduce Experiments from the Paper

This file contains instructions on how to reproduce the evaluation results for HQPTL.
To do so, you do *not* need to build HQPTL yourself; everything will be installed in a separate Docker image. 
Make sure you have installed the following:

- [Docker](https://www.docker.com/) (tested with version 20.10.17)
- [python](https://www.python.org/) (tested with version 3.10.9)
- pythons' [matplotlib](https://matplotlib.org/) Install this by, e.g., running `pip install matplotlib`


## Building Docker Images for HQPTl and Goal

We first construct a Docker image for each executable. 
When in the main directory of this repository run the following four commands.
This will construct 4 Docker images that run the HQPTL frontend, QPTLTrans, Goal, and spot's randltl respectively.
Note that building the images takes about 10-20min.

```
docker build -t frontend -f docker/Dockerfile_Frontend .
docker build -t qptltrans -f docker/Dockerfile_QPTLTrans .
docker build -t goal -f docker/Dockerfile_goal .   
docker build -t randltl -f docker/Dockerfile_randltl .    
```

Afterwards, you can run `docker image ls` to list all current Docker images and should see the three images `hqptl`, `qptltrans`, `goal`, and `randltl`. 



## Evaluate HQPTL Model Checking on SYNTCOMP Benchmarks (Figure 1)

To check promptness on the synthesized syntcomp benchmarks (displayed in Figure 1) run

```
python eval/eval_mc_promptness.py
```

This will load the systems for the syntcomp evaluation (located in `eval/data/system_instances_syntcomp.json`) and, for each system, generate 5 random request-response event pairs and use the Frontend to check the HyperQPTL formula. 
The resulting times and sizes will be written to `eval/out/mc_results_promptness_syntcomp.json` as a `.json` file and plotted (as displayed in Figure 1) in `eval/out/mc_results_promptness_syntcomp.pdf`.


## Evaluate QPTL-to-NBA translation on SYNTCOMP (Figure 2)

To translate QPTL formulas stemming from the SYNTCOMP benchmarks into NBAs (using both HQPTL and GOAL, as displayed in Figure 2) run

```
python eval/eval_translation_syntcomp.py
```

This will load the systems for the syntcomp evaluation (located in `eval/data/formula_instances_syntcomp.json`) and translate each formula into an NBA using both HQPTL and various algorithms implemented in GOAL. 
The resulting times by each solver will be written to `eval/out/translation_results_syntcomp.json` and plotted (as displayed in Figure 2) in `eval/out/translation_results_syntcomp.pdf`.


## Evaluate QPTL-to-NBA translation on random formulas (Figures 3 and 4)

To translate random QPTL formulas (generated using spot's randltl) into NBAs (using both HQPTL and GOAL's couvreur, as displayed in Figures 3 and 4) run

```
python eval/eval_translation_random.py
```

For a varying number of quantifier (in this case 0 and 3), it generates QPTL instances using spot's randltl.
The resulting times and sized by each solver will be written to `eval/out/translation_results_random_<n>.json` where `<n>` is the number of quantifiers.
The times are plotted in `eval/out/translation_results_random_time_<n>.pdf` (as in Figure 3) and the sizes are plotted in `eval/out/translation_results_random_size_<n>.pdf` (as in Figure 4).