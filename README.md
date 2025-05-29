# Causal Interventions Reveal Shared Structure Across English Filler–Gap Constructions 

This repository houses all the code necessary to run the experiments and analyses for the paper [Causal Interventions Reveal Shared Structure Across English Filler–Gap Constructions](https://www.arxiv.org/abs/2505.16002$0).

In particular, it includes the code for:

- Training DAS interventions for single-source and leave-one-out construction variants (both in the single-clause and embedded-clause case).
- Evaluating DAS interventions on all constructions, including the single-clause interventions on multi-clause construction variants.
- Performing statistical analyses and generating the plots in the paper.

> [!IMPORTANT]
> We note that this project is built upon the fantastic work of [CausalGym](https://github.com/aryamanarora/causalgym) (Arora et al. 2024), with much of the code in `causalgym/` being forked directly from the original repository. Any updates of our own are duly marked. Please cite [CausalGym](https://github.com/aryamanarora/causalgym) (citation below) in addition to our work if you find this repository useful.

## Instructions

### Set-Up

First, install the requirements:

`pip install -r requirements.txt`

### Run All Experiments

To generate the data for all experiments, please run:

`commands/run_experiments.sh`

The code defaults to using the `pythia 1.4b` variant, with 25 batches of size 16. However, if you want to run with different presets, you can specify as follows:

`commands/run_experiments.sh YOUR_DESIRED_MODEL_SIZE YOUR_DESIRED_NUM_BATCHES YOUR_DESIRED_BATCH_SIZE`

You further may need to make the scripts in the `commands` directory executable by running `chmod -R +x commands` from the main directory.

### Run Just Specific Experiments

To generate the data for specific experiments you can run the following commands (for all of these you can again specify your desired model size, number of batches and batch size using the above commands):

- Experiment 1: `commands/loo_pipeline.sh`
- Experiment 2: `commands/single_pipeline.sh`
- Experiment 3: `commands/eval_interventions/eval_single_double.sh` -- ***Note:*** *You must first have trained all single-clause interventions to run this experiment*

If you run in this manner, you will also need to aggregate the csv's manually. This can be done as follows:

- Experiment 1: `python results/generalization/process_csv.py -lo`
- Experiment 2: `python results/generalization/process_csv.py -c`
- Experiment 3: `python results/generalization/process_csv.py -sd`


### Analysis

Once you have generated your data (either through the single command used to run all experiments or that which is used to run specific experiments), you can run the provided R analysis scripts in the `analysis` folder. We also provide our parquet outputs in `results/generalization/` in case you want to skip the experiments and merely reproduce the analysis.

***Note:*** *For most of these R scripts, you must specify at the top of the file which model size you are intending to analyze. This detail is also discussed in `analysis/README.md`*.

#### Experiment 1:

- Generate Aggregating Bar Plots: `loo_gap_generalization.R`, `embedded_loo_gap_generalization.R`
- Generate Mechanistic Heatmaps: `mech_plots.R`
- Statistical Analysis (linear mixed effects model): `lmem_loo.R`

#### Experiment 2:

- Generate centrality scatter plots and bar charts:
    - First, generate aggregated csvs: `gap_generalization_hm.R`, `embedded_gap_generalization_hm.R`
    - Next, generate centrality data: `constructional_analysis.ipynb`
    - Finally, generate plots: `centrality.R`
- Generate chord plot: `chord.R`
- Statistical Analysis (linear mixed effects model): `lmem_single.R`

***Note:*** *We also provide the code used for our frequency analysis in `frequency.ipynb`, but it is not needed to re-run this to perform this analysis*.

#### Experiment 3:
- Generate aggregating bar plots:  `gap_generalization_single_double.R`
- Generate mechanistic plots: `sd_mech_plots.R`

#

If you are having any issues running any code, please do not hesitate to reach out or file an issue!


## Citation
If you use any of our code in 
```bibtex
@misc{boguraev2025causalinterventionsrevealshared,
      title={Causal Interventions Reveal Shared Structure Across English Filler-Gap Constructions}, 
      author={Sasha Boguraev and Christopher Potts and Kyle Mahowald},
      year={2025},
      eprint={2505.16002},
      archivePrefix={arXiv},
      primaryClass={cs.CL},
      url={https://arxiv.org/abs/2505.16002}, 
}
```
Also please cite the original CausalGym paper.
```bibtex
@inproceedings{arora-etal-2024-causalgym,
    title = "{C}ausal{G}ym: Benchmarking causal interpretability methods on linguistic tasks",
    author = "Arora, Aryaman and Jurafsky, Dan and Potts, Christopher",
    editor = "Ku, Lun-Wei and Martins, Andre and Srikumar, Vivek",
    booktitle = "Proceedings of the 62nd Annual Meeting of the Association for Computational Linguistics (Volume 1: Long Papers)",
    month = aug,
    year = "2024",
    address = "Bangkok, Thailand",
    publisher = "Association for Computational Linguistics",
    url = "https://aclanthology.org/2024.acl-long.785",
    doi = "10.18653/v1/2024.acl-long.785",
    pages = "14638--14663"
}
```