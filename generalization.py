import sys, os
sys.path.append(".")

import os, sys ,torch, warnings, argparse
import pandas as pd
from tqdm import tqdm
from transformers import GPTNeoXForCausalLM, AutoTokenizer

from pyvene.models.intervenable_base import IntervenableModel
import pyvene as pv
from causalgym.interventions import intervention_config
from utils import load_data, load_templates, get_loo_name, single_double_pos
from causalgym.eval import eval
import glob
from causalgym.data import list_datasets
import gc

DEVICE = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")

def eval_generalizations(datasources, trainset, eval_sets, model_id, eval_dataset_name, train_dataset_name, 
                         revision, eval_seed, single_double=False, trainsource=None, strategy = "last", 
                         force_recompute=False): 
    """
    Evaluate model generalization by applying low-rank interventions across multiple datasets.
    This function loads a pretrained causal language model, iterates over a set of evaluation
    datasets and intervention sites (position x layer), applies a specified intervention
    configuration, and computes performance summaries and raw evaluation data. Results are
    saved to disk as both detailed per-intervention data and aggregated summary CSVs.
    Parameters
    ----------
    datasources : dict[str, DataSource]
        Mapping from evaluation dataset name to a DataSource object containing sequence
        length and variable-position metadata.
    trainset : Optional[DataSource]
        The DataSource used during training. Required if `single_double=True`.
    eval_sets : dict[str, Sequence]
        Mapping from evaluation dataset name to the actual examples (e.g., list of prompts).
    model_id : str
        Identifier of the pretrained model (e.g., HuggingFace repo name or local checkpoint path).
    eval_dataset_name : str
        Name of the evaluation dataset category (used for filepath organization).
    train_dataset_name : str
        Name of the training dataset category (used for filepath organization).
    revision : str or int
        Model revision or commit hash to load (use "main" to load default).
    eval_seed : int
        Random seed used for reproducibility in evaluation and summary filenames.
    single_double : bool, default=False
        If True, restricts evaluation to the "single vs. double" filler-gap phenomenon
        and requires `trainsource` to be set.
    trainsource : Optional[DataSource], default=None
        DataSource object for the training phenomenon; required when `single_double=True`.
    strategy : str, default="last"
        Evaluation strategy or heuristic name passed to the `eval` function.
    force_recompute : bool, default=False
        If True, re-runs evaluation even if output files already exist.
    Returns
    -------
    None
        Saves per-intervention evaluation data (pickle/JSON) and aggregated summary CSV
        files under a structured output directory:
            results/generalization/[single_double/]<train_dataset_name>/
    Side Effects
    ------------
    - Loads and caches the model to GPU and frees memory between runs.
    - Writes files to disk; may overwrite existing results if `force_recompute=True`.
    - Logs progress via tqdm progress bars.
    """
    # Make sure that trainsource is not none if single_double is True
    if single_double:
        assert trainsource is not None, "Train source must be provided for single double eval"
    
    # Suppress warnings from transformers about deprecated features
    warnings.simplefilter(action='ignore', category=FutureWarning)
    
    # Load the model
    datasets = list(datasources.keys())
    model = GPTNeoXForCausalLM.from_pretrained(model_id) if revision == "main" \
        else GPTNeoXForCausalLM.from_pretrained(model_id, revision=revision)

    # Get save folder based on train_dataset_name and single_double flag
    save_folder = f"results/generalization/{train_dataset_name}/" if not single_double \
        else f"results/generalization/single_double/{train_dataset_name}/"
    os.makedirs(save_folder, exist_ok=True)

    # Loop through and evaluate each dataset
    for idx, eval_source in enumerate((pbar_es := tqdm(datasets, desc="Evaluation Datasets", leave=False))):
        pbar_es.set_postfix_str(eval_source)

        # Initialize data structures for storing results
        all_generalizations = {}
        evalset = eval_sets[eval_source]
        datasource = datasources[eval_source]
        eval_data = {}

        # Get first and last variable positions based on whether single_double is used
        if not single_double:
            first = datasource.first_var_pos
            last = datasource.length
        else:
            warnings.warn("This functionality is only implemented for the filler gap phenomenon in the paper.")
            first = trainsource.first_var_pos
            last = trainsource.length
       
        # Make an array with the intervention model subfolders in order of loops
        subfolder_in_order = [
            f"pos_{pos_i}_layer_{layer_i}"
            for pos_i in range(first, last)
            for layer_i in range(model.config.num_hidden_layers)
        ]

        # If single_double is True, we need to use the trainset for the single double eval
        train_source = train_dataset_name.split("/")[-1]
        short_model_id = model_id.split("/")[-1]

        # Check if the file already exists, if not, compute the generalizations
        # or if force_recompute is True, always compute.
        if (not f"{short_model_id}_from_{train_source}_to_{eval_source}_seed{eval_seed}.csv" in os.listdir(save_folder)) \
            or (force_recompute):

            # Loop through positions
            for pos_i in (pbar_pos := tqdm(range(datasource.first_var_pos, datasource.length), 
                                           desc="Positions", leave=False, disable=False)):
                pbar_pos.set_postfix_str(f"pos {pos_i}")
                
                # Loop through layers
                for layer_i in (pbar_layer := tqdm(range(model.config.num_hidden_layers), 
                                                   desc="Layers", leave=False, disable=False)):
                    pbar_layer.set_postfix_str(f"layer {layer_i}")
                    torch.cuda.empty_cache()

                    # Load the intervention configuration for the current layer and position
                    intervenable_config = intervention_config(
                        intervention_site="block_output",
                        intervention_type=pv.LowRankRotatedSpaceIntervention,
                        layer=layer_i,
                        num_dims=1
                    )
                    # get the intervention position
                    pos_intervention = single_double_pos(pos_i) if single_double else pos_i
                    
                    # Checks if it is an item to ignore or if the position is aligned with nothing
                    if pos_intervention == -1 or evalset[0].compute_pos("last")[0][0][pos_intervention][0] == -1: 
                        continue
                    elif trainset is not None:
                        if trainset[0].compute_pos("last")[0][0][pos_intervention][0] == -1: 
                            continue
                            
                    # If we have an intervenable model already (left over from previous loop), delete it to free memory
                    if 'intervenable_model' in locals():
                        del intervenable_model
                        gc.collect()
                        torch.cuda.empty_cache()

                    # Load the intervention model from the specified directory
                    intervenable_model = IntervenableModel(config=intervenable_config, model=model)
                    subfolder_idx = ((pos_intervention - first) * model.config.num_hidden_layers) + layer_i
                    subfolder = subfolder_in_order[subfolder_idx]
                    intervenable_model.load_intervention(
                        load_directory=f"./intervention_models/{short_model_id}/{train_dataset_name}/{subfolder}/",
                        include_model=False
                    )
                    intervenable_model.set_device(DEVICE)
                    intervenable_model.disable_model_gradients()

                    # Evaluate the model with the loaded intervention
                    more_data, summary, eval_activation = eval(intervenable_model, evalset, layer_i, pos_i, strategy) 
                    
                    summary.update({
                        "layer": layer_i,
                        "pos": pos_i,
                        "seed": eval_seed,
                        "from": train_source,
                        "to": eval_source,
                        "revision": 143000 if revision == "main" else revision,
                        "model_id": short_model_id
                    })
                    for key, value in summary.items():
                        if key not in all_generalizations:
                            all_generalizations[key] = []
                        all_generalizations[key].append(value)

                    eval_data[f"layer{layer_i}_pos{pos_i}"] = more_data

                    del intervenable_model
                    torch.cuda.empty_cache()
                    gc.collect()

            save_eval_data(eval_data, save_folder, short_model_id, train_source, eval_source, eval_seed, revision)
            save_generalizations(all_generalizations, save_folder, short_model_id, revision, train_source, eval_source, force_recompute, eval_seed)
            del eval_data, all_generalizations
            gc.collect()
            torch.cuda.empty_cache()
    
    del model
    torch.cuda.empty_cache()
    gc.collect()



def save_eval_data(eval_data, save_folder, short_model_id, train_source, eval_source, seed, revision):
    """
    Save the evaluation data to a CSV file.
    Parameters
    ----------
    eval_data : dict
        Dictionary containing evaluation data for each layer and position.
    save_folder : str
        Folder where the evaluation data will be saved.
    short_model_id : str
        Shortened model identifier for the filename.
    train_source : str
        Name of the training source dataset.
    eval_source : str
        Name of the evaluation source dataset.
    seed : int
        Random seed used for evaluation.
    revision : str
        Model revision or commit hash.
    Returns
    -------
    None
    Side Effects
    ------------
    - Writes a CSV file containing the evaluation data.
    """
    df_data = pd.DataFrame(eval_data)
    if revision == "main":
        df_data.to_csv(f"{save_folder}/{short_model_id}_from_{train_source}_to_{eval_source}_seed{seed}.csv")
    else:
        df_data.to_csv(f"{save_folder}/{short_model_id}_from_{train_source}_to_{eval_source}_seed{seed}.csv")
    del df_data


def save_generalizations(all_generalizations, save_folder, short_model_id, revision, train_source, 
                         eval_source, force_recompute, eval_seed):
    """
    Save a list of generalization records to a Parquet file, optionally merging with or
    overwriting existing data based on evaluation settings.
    Parameters:
        all_generalizations (list of dict):
            A list of dictionaries, each representing a generalization record with keys
            matching the desired DataFrame columns.
        save_folder (str):
            Path to the directory where the Parquet file will be written.
        short_model_id (str):
            Identifier for the model; used to construct the Parquet filename.
        revision (str):
            Model revision identifier.
        train_source (str):
            Name or identifier of the training data source. Used to filter out existing
            records when force_recompute is True.
        eval_source (str):
            Name or identifier of the evaluation data source. Used to filter out existing
            records when force_recompute is True.
        force_recompute (bool):
            If True and the target file already exists, remove any existing records for
            the given train_source, eval_source, and eval_seed before appending new data.
        eval_seed (int):
            Seed value for the evaluation run. Used in conjunction with force_recompute
            to identify which existing records to drop.
    Returns:
        None
    Side Effects:
        - Writes or updates a Parquet file named
            "{save_folder}/{short_model_id}_all_generalizations.parquet".
        - If force_recompute is True and the file exists, older records matching the
            specified train_source, eval_source, and eval_seed are removed before saving.
    """
    parquet_path = f"{save_folder}/{short_model_id}_all_generalizations.parquet"
    df_generalizations_so_far = pd.DataFrame(all_generalizations)
    if os.path.exists(parquet_path):
        existing_df = pd.read_parquet(parquet_path)
        if force_recompute:
            existing_df = existing_df[~((existing_df['from'] == train_source) \
                                        & (existing_df['to'] == eval_source)) \
                                        & (existing_df['seed'] == eval_seed)]
        combined_df = pd.concat([existing_df, df_generalizations_so_far], ignore_index=True)
        combined_df.to_parquet(parquet_path)
    else:
        df_generalizations_so_far.to_parquet(parquet_path)
    del df_generalizations_so_far



def get_eval_sets(model_id, datasets, eval_seed, batch_size, num_batches=25):
    """
    Load evaluation datasets and their corresponding data sources for a given model.
    Parameters
    ----------
    model_id : str
        Identifier of the model to be evaluated (e.g., HuggingFace repo name or local path).
    datasets : list of str
        List of dataset identifiers to load for evaluation.
    eval_seed : int
        Random seed used for reproducibility in evaluation.
    batch_size : int
        Batch size for loading the datasets.
    num_batches : int, default=25
        Number of batches to use for evaluation.
    Returns
    -------
    datasources : dict
        Dictionary mapping dataset names to their corresponding DataSource objects.
    evalsets : dict
        Dictionary mapping dataset names to their corresponding evaluation sets.
    Side Effects
    ------------
    - Loads the model tokenizer and sets the pad token to the end-of-sequence token.
    - Initializes empty dictionaries for evaluation sets and data sources.
    """
    evalsets = {}
    datasources = {}
    tokenizer = AutoTokenizer.from_pretrained(model_id)
    tokenizer.pad_token = tokenizer.eos_token

    for dataset in datasets:
        datasource, trainset, evalset = load_data(
            dataset = dataset, 
            tokenizer = tokenizer, 
            batch_size = batch_size, 
            steps = 1, 
            device = DEVICE,
            eval_seed = eval_seed,
            num_batches = num_batches
        )
        
        evalsets[dataset.split("/")[-1]] = evalset
        datasources[dataset.split("/")[-1]] = datasource
        
    return datasources, evalsets


def eval_self(eval_seeds, model_id, batch_size, eval_sources, source_set, revision, fill_in=False, 
              left_out=None, force_recompute=False):
    """
    Evaluate model generalization by applying low-rank interventions across multiple datasets.
    This function loads a pretrained causal language model, iterates over a set of evaluation
    datasets and intervention sites (position x layer), applies a specified intervention
    configuration, and computes performance summaries and raw evaluation data. Results are
    saved to disk as both detailed per-intervention data and aggregated summary CSVs.
    Parameters
    ----------
    eval_seeds : list of int
        List of random seeds for evaluation, used for reproducibility.
    model_id : str
        Identifier of the pretrained model (e.g., HuggingFace repo name or local checkpoint path).
    batch_size : int
        Batch size for evaluation.
    eval_sources : list of str
        List of evaluation dataset names to use for generalization.
    source_set : list of str
        List of training dataset names to generalize from. If None, defaults to a placeholder.
    revision : str
        Model revision or commit hash to load (use "main" to load default).
    left_out : str, default=None
        If specified, leaves out certain datasets from the evaluation. If "all", no datasets are left out.
    force_recompute : bool, default=False
        If True, forces recomputation of generalizations even if output files already exist.
    Returns
    -------
    None
    Side Effects
    ------------
    - Loads and caches the model to GPU and frees memory between runs.
    - Writes files to disk; may overwrite existing results if `force_recompute=True`.
    - Logs progress via tqdm progress bars.
    """
    datasets = load_templates()

    # Check if source_set is provided, if not, use a placeholder (will not be provided in leave-out cases)
    if source_set is not None:
        assert len(source_set) == 1, "Only one source set can be used for generalization"
        if '/' not in source_set[0]:
            sources = [d for d in list_datasets() if d.startswith(source_set[0]+"/")]
        else:
            sources = source_set
    else:
        sources = ['placeholder']
        
    # Loop through evaluation seeds and sources
    for eval_seed in (pbar_seed := tqdm(eval_seeds, desc="Eval Seeds", leave=False)):
        pbar_seed.set_postfix_str(eval_seed)

        # Loop through sources
        for source in (pbar_ts := tqdm(sources, desc="Train Sources", leave=False)):
            pbar_ts.set_postfix_str(source)

            # Loop through evaluation sets in eval_sources
            for eval_set in eval_sources:
                dataset = datasets[eval_set] if '/' not in eval_set else [eval_set]
                
                # Datasources are evalsets
                datasources, eval_sets = get_eval_sets(model_id, dataset, eval_seed, batch_size)
                
                # If source_set is provided, get the trainset for the source, otherwise use None
                if source_set is not None:
                    _, trainsets = get_eval_sets(model_id, [source], eval_seed, batch_size)
                    trainset = trainsets[list(trainsets.keys())[0]]
                else:
                    trainsets, trainset = None, None

                # If left_out is specified, we need to handle it
                if left_out!="all":
                    loo_path = get_loo_name(left_out)
                    train_dataset_name = f"leave_out_{loo_path}"
                else:
                    train_dataset_name = source

                # Evaluate generalizations from the trainset to the eval_set
                eval_generalizations(datasources=datasources, # eval_set
                                    trainset=trainset,
                                    eval_sets=eval_sets, 
                                    model_id=model_id, 
                                    eval_dataset_name=eval_set, 
                                    train_dataset_name=train_dataset_name, 
                                    revision=revision, 
                                    eval_seed=eval_seed,
                                    force_recompute=force_recompute,)
                
                del datasources, eval_sets, trainsets, trainset
                torch.cuda.empty_cache()


def eval_single_double(eval_seeds, model_id, batch_size, eval_sources, source_set, revision, 
                       force_recompute=False, num_batches=25):
    """
    Evaluate model generalization for the single-clause interventions on embedded clauses.
    This function loads a pretrained causal language model, iterates over a set of evaluation
    datasets and intervention sites (position x layer), applies a specified intervention
    configuration, and computes performance summaries and raw evaluation data. Results are
    saved to disk as both detailed per-intervention data and aggregated summary CSVs.
    Parameters
    ----------
    eval_seeds : list of int
        List of random seeds for evaluation, used for reproducibility.
    model_id : str
        Identifier of the pretrained model (e.g., HuggingFace repo name or local checkpoint path).
    batch_size : int
        Batch size for evaluation.
    eval_sources : list of str
        List of evaluation dataset names to use for generalization.
    source_set : list of str
        List of training dataset names to generalize from. Should contain only one source set.
    revision : str
        Model revision or commit hash to load (use "main" to load default).
    force_recompute : bool, default=False
        If True, forces recomputation of generalizations even if output files already exist.
    num_batches : int, default=25
        Number of batches to use for evaluation.
    Returns
    -------
    None
    Side Effects
    ------------
    - Loads and caches the model to GPU and frees memory between runs.  
    - Writes files to disk; may overwrite existing results if `force_recompute=True`.
    - Logs progress via tqdm progress bars.
    """
    assert len(source_set) == 1, "Only one source set can be used for generalization"

    datasets = load_templates()

    if '/' not in source_set[0]:
        sources = [d for d in list_datasets() if d.startswith(source_set[0]+"/")]
    else:
        sources = source_set
        
    # Loop through evaluation seeds
    for eval_seed in (pbar_seed := tqdm(eval_seeds, desc="Eval Seeds", leave=False)):
        pbar_seed.set_postfix_str(eval_seed)

        # Loop through sources
        for source in (pbar_ts := tqdm(sources, desc="Train Sources", leave=False)):
            pbar_ts.set_postfix_str(source)

            # Loop through evaluation sets in eval_sources
            for eval_set in eval_sources:
                
                # Get evalsets
                dataset = datasets[eval_set] if '/' not in eval_set else [eval_set]
                datasources, eval_sets = get_eval_sets(model_id, dataset, eval_seed, batch_size, num_batches=num_batches)
                
                # Get trainsets
                if source_set is not None:
                    train_source, trainsets = get_eval_sets(model_id, [source], eval_seed, batch_size, num_batches=1)
                    trainset = trainsets[list(trainsets.keys())[0]]
                else:
                    trainsets, trainset = None, None

                # Evaluate generalizations from the trainset to the eval_set
                eval_generalizations(datasources=datasources, # eval_set
                                    trainset = trainset,
                                    eval_sets=eval_sets, 
                                    model_id=model_id, 
                                    eval_dataset_name=eval_set, 
                                    train_dataset_name=source, 
                                    revision=revision, 
                                    eval_seed=eval_seed,
                                    single_double=True,
                                    trainsource=list(train_source.values())[0],
                                    force_recompute=force_recompute)
                
                del datasources, eval_sets, trainsets, trainset
                torch.cuda.empty_cache()


def main(eval_seeds, model_id, batch_size, eval_sources, source_set, revision, left_out=None, 
         single_double=False, force_recompute=False):
    """
    Main function to evaluate generalizations of a model.
    """
    if single_double:
        assert left_out == "all", "Left out cannot be used with single double eval"
        eval_single_double(eval_seeds, model_id, batch_size, eval_sources, source_set, revision, force_recompute=force_recompute)
    else:
        eval_self(eval_seeds, model_id, batch_size, eval_sources, source_set, revision, left_out=left_out, force_recompute=force_recompute)
            

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Evaluate generalizations of a model.")
    parser.add_argument("--eval_seeds", type=int, nargs="+", default=[42], help="Seed for evaluation.")
    parser.add_argument("--model_id", type=str, required=True, help="Model identifier.")
    parser.add_argument("--batch_size", type=int, default=32, help="Batch size for evaluation.")
    parser.add_argument("--source_set", type=str, nargs="+", required=False, help="Train set to generalize from.")
    parser.add_argument("--eval_set", type=str, required=False, nargs="+", help="Evaluation sets to use.")
    parser.add_argument("--revision", type=str, required=False, default="main", help="Revision of the model.")
    parser.add_argument("--left_out", type=str, default="all",help="Leave out missing generalizations.")
    parser.add_argument("--single_double", "-sd", action="store_true", help="Use single clause interventions on embedded clauses.")
    parser.add_argument("--force_recompute", "-f", action="store_true", help="Force recompute of generalizations.")
    parser.add_argument("--num_batches", type=int, default=25, help="Number of batches to use for evaluation.")

    args = parser.parse_args()

    main(args.eval_seeds, 
         args.model_id, 
         args.batch_size, 
         args.eval_set, 
         args.source_set, 
         args.revision, 
         args.left_out, 
         args.single_double, 
         args.force_recompute)