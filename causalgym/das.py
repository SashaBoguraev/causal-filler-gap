from numpy import add
import torch
import os
import argparse
from transformers import AutoTokenizer, AutoModelForCausalLM, GPTNeoXForCausalLM
from utils import WEIGHTS
from data import Dataset, get_combined_batch
from eval import eval, augment_data
from train import train_das, train_feature_direction
import datetime
import json
from typing import Union

from pyvene.models.intervenable_base import IntervenableModel
from interventions import *


# make das subdir
if not os.path.exists("logs/das"):
    os.makedirs("logs/das")


def experiment(
    model: str,
    dataset: Union[str, list[str]],
    steps: int,
    eval_steps: int,
    grad_steps: int,
    batch_size: int,
    intervention_site: str,
    strategy: str,
    lr: float,
    left: str = "all",
    leave_out: bool=False,
    only_das: bool=False,
    hparam_non_das: bool=False,
    das_label: str=None,
    revision: str="main",
    log_folder: str="das",
    manipulate: Union[str, None]=None,
    tokenizer: Union[AutoTokenizer, None]=None,
    gpt: Union[AutoModelForCausalLM, None]=None,
):
    """Run a feature-finding experiment."""

    #===== Modified =====
    # Arg processing for the leave-out vs. single-source cases.
    if leave_out:
        assert isinstance(dataset, list), "dataset must be a list of strings if leave_out is True"
    else:
        assert isinstance(dataset, str), "dataset must be a string if leave_out is False"
    #===== End modified =====

    # load model
    total_data = []
    diff_vectors = []
    NOW = datetime.datetime.now().strftime("%Y%m%d%H%M%S%f")
    device = "cuda:0" if torch.cuda.is_available() else "cpu"
    if tokenizer is None:
        tokenizer = AutoTokenizer.from_pretrained(model)
        tokenizer.pad_token = tokenizer.eos_token
    if gpt is None:
        weight_type = WEIGHTS.get(model, torch.float16) if device == "cuda:0" else torch.float32
        gpt = GPTNeoXForCausalLM.from_pretrained(
            model,
            revision=revision,
            torch_dtype=weight_type,
            use_flash_attention_2=(weight_type in [torch.bfloat16, torch.float16] and device == "cuda:0"),
        ).to(device)
        
    gpt.eval()

    eval_seed = 420 if hparam_non_das else 1
    

    #===== Modified =====
    # If leave_out is False, we only need to load one dataset.
    if not leave_out:
        # make dataset, ensuring examples in trainset are not in evalset
        data_source = Dataset.load_from(dataset)
        trainset = data_source.sample_batches(tokenizer, batch_size, steps, device, seed=42, manipulate=manipulate)
        discard = set()
        for batch in trainset:
            for pair in batch.pairs:
                discard.add(''.join(pair.base))
        
        # evalset
        evalset = data_source.sample_batches(tokenizer, batch_size, 25, device, seed=eval_seed, discard=discard, manipulate=manipulate)
    # If leave_out is True, we need to load multiple datasets and manipulate the left variable.
    else:
        datasources = [Dataset.load_from(d) for d in dataset]
        embed = 'embedded_clause' in left
        inanimate = 'inanimate' in left
        
        match (embed, inanimate):
            case (True, True):
                left = "embedded_clause_inanimate/"+left
            case (True, False):
                left = "embedded_clause_animate/"+left
            case (False, True):
                left = "single_clause_inanimate/"+left
            case (False, False):
                left = "single_clause_animate/"+left
            
        dataset = f"leave_out_{left}"
        data_source = datasources[0]
        trainsets = get_combined_batch(datasources, tokenizer, batch_size, steps, device, seed=42, manipulate=manipulate)
        discard = set()
        for key, tset in trainsets.items():
            for batch in tset:
                for pair in batch.pairs:
                    discard.add(''.join(pair.base))
        evalsets = get_combined_batch(datasources, tokenizer, batch_size, 25, device, seed=eval_seed, discard=discard, manipulate=manipulate)
    #===== End modified =====


    # entering train loops
    for pos_i in range(data_source.first_var_pos, data_source.length):
        # ===== Modified =====
        # If leave_out is True, we need to select the appropriate trainset and evalset.
        trainset = trainsets[pos_i] if leave_out else trainset
        evalset = evalsets[pos_i] if leave_out else evalset

        # If the position is not computable, skip it.
        if -1 in [trainset[i].compute_pos(strategy)[0][0][pos_i][0] for i in range(len(trainset))]:
            print(f"skipping position {pos_i} ({data_source.span_names[pos_i]})")
            continue
        # ===== End modified =====

        # per-layer training loop
        iterator = range(gpt.config.num_hidden_layers)
        for layer_i in iterator:
            print(f"position {pos_i} ({data_source.span_names[pos_i]}), layer {layer_i}")
            data = []

            # vanilla intervention
            if strategy != "all" and not only_das:
                intervenable_config = intervention_config(
                    intervention_site, pv.VanillaIntervention, layer_i, 0
                )
                intervenable = IntervenableModel(intervenable_config, gpt)
                intervenable.set_device(device)
                intervenable.disable_model_gradients()

                more_data, summary, _ = eval(intervenable, evalset, layer_i, pos_i, strategy)
                intervenable._cleanup_states()
                data.extend(augment_data(more_data, {"method": "vanilla", "step": -1}))
                print(f"vanilla: {summary}")
                
            # DAS intervention
            intervenable_config = intervention_config(
                intervention_site,
                pv.LowRankRotatedSpaceIntervention if strategy != "all" else PooledLowRankRotatedSpaceIntervention,
                layer_i, 1
            )
            intervenable = IntervenableModel(intervenable_config, gpt)
            intervenable.set_device(device)
            intervenable.disable_model_gradients()

            _, more_data, activations, eval_activations, diff_vector = train_das(
                intervenable, trainset, evalset, layer_i, pos_i, strategy,
                eval_steps, grad_steps, lr=lr, das_label="das" if das_label is None else das_label,
                model_id=model, dataset = dataset)
            diff_vectors.append({"method": "das" if das_label is None else das_label,
                                 "layer": layer_i, "pos": pos_i, "vec": diff_vector})
            data.extend(more_data)

            
            # store all data
            total_data.extend(augment_data(data, {"layer": layer_i, "pos": pos_i}))

    # make data dump
    short_dataset_name = dataset.split('/')[-1]
    short_model_name = model.split('/')[-1] + (f"_{revision}" if revision != "main" else "")
    filedump = {
        "metadata": {
            "model": model + (f"_{revision}" if revision != "main" else ""),
            "dataset": dataset,
            "steps": steps,
            "eval_steps": eval_steps,
            "grad_steps": grad_steps,
            "batch_size": batch_size,
            "intervention_site": intervention_site,
            "strategy": strategy,
            "lr": lr,
            "span_names": data_source.span_names,
            "manipulate": manipulate,
        },
        "data": total_data,
        "vec": diff_vectors,
    }

    # log
    if manipulate is None:
        manipulate = "orig"
    log_file = f"logs/{log_folder}/{NOW}__{short_model_name}__{short_dataset_name}__{manipulate}.json"
    print(f"logging to {log_file}")
    with open(log_file, "w") as f:
        json.dump(filedump, f)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--model", type=str, default="EleutherAI/pythia-70m")
    parser.add_argument("--dataset", type=str, default="syntaxgym/agr_gender")
    parser.add_argument("--steps", type=int, default=100)
    parser.add_argument("--eval-steps", type=int, default=25)
    parser.add_argument("--grad-steps", type=int, default=1)
    parser.add_argument("--batch-size", type=int, default=4)
    parser.add_argument("--intervention-site", type=str, default="block_output")
    parser.add_argument("--strategy", type=str, default="last")
    parser.add_argument("--lr", type=float, default=5e-3)
    parser.add_argument("--only-das", action="store_true")
    parser.add_argument("--hparam-non-das", action="store_true")
    parser.add_argument("--das-label", type=str, default=None)
    parser.add_argument("--revision", type=str, default="main")
    parser.add_argument("--log-folder", type=str, default="das")
    parser.add_argument("--manipulate", type=str, default=None)
    args = parser.parse_args()
    experiment(**vars(args))


if __name__ == "__main__":
    main()
