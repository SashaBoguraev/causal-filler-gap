import sys, os
from causalgym.data import Dataset, Batch, Pair
from torch import arange
import torch, json
import pyvene as pv
import warnings

def load_data(dataset, tokenizer, batch_size, steps, device, manipulate=False, eval_seed = 420, num_batches=25):
    """make dataset, ensuring examples in trainset are not in evalset"""
    data_source = Dataset.load_from(dataset)
    trainset = data_source.sample_batches(tokenizer, batch_size, steps, device, seed=42, manipulate=manipulate)
    
    discard = set()
    for batch in trainset:
        for pair in batch.pairs:
            discard.add(''.join(pair.base))

    # evalset
    evalset = data_source.sample_batches(tokenizer, 
                                         batch_size, 
                                         num_batches=num_batches, 
                                         device=device, 
                                         seed=eval_seed, 
                                         discard=discard, 
                                         manipulate=manipulate)

    return data_source, trainset, evalset


def get_last_token(logits, attention_mask):
    """
    Extract the logits corresponding to the last token in each sequence of the batch.
    """
    last_token_indices = attention_mask.sum(1) - 1
    batch_indices = torch.arange(logits.size(0)).unsqueeze(1).to(logits.device)
    return logits[batch_indices, last_token_indices.unsqueeze(1).to(logits.device)].squeeze(1)


def compute_logit_diff(logits: torch.tensor, batch, device):
    """
    Compute the difference in logits between the correct label and the other label for each example in the batch.
    """
    base_logit = get_last_token(logits, batch["base"]['attention_mask'].to(device))
    base_label = batch["base"]['labels'].to(device)
    src_label = batch["src"]['labels'].to(device)
    logit_diffs = []
    for batch_i in range(base_logit.size(0)):
        correct_name = base_label[batch_i]
        other_name = src_label[batch_i]
        logit_diffs.append(base_logit[batch_i, correct_name] - base_logit[batch_i, other_name])
    return logit_diffs


def compute_odds(base_logits, counterfactual_logits, batch):
    """
    Compute the odds ratio between the base and counterfactual logits for each example in the batch.
    """
    base_logit = get_last_token(base_logits, batch.base['attention_mask'])
    counter_logit = get_last_token(counterfactual_logits, batch.base['attention_mask'])

    base_prob = base_logit.log_softmax(-1)
    counter_prob = counter_logit.log_softmax(-1)

    odds = []
    for batch_i in range(base_logit.size(0)):
        base_label = batch.base_labels[batch_i]
        src_label = batch.src_labels[batch_i]
        
        left = base_prob[batch_i, base_label] - base_prob[batch_i, src_label]
        right = counter_prob[batch_i, src_label] - counter_prob[batch_i, base_label]

        odds.append(left + right)

    return odds


def load_templates(template_folder = "data/templates/"):
    """
    Load templates from a specified folder and return a dictionary mapping template names to their keys.
    """
    datasets = {}
    
    for template_file in os.listdir(template_folder):
        if template_file.endswith(".json"):
            with open(os.path.join(template_folder, template_file), 'r') as f:
                template_data = json.load(f)
                datasets[template_file.split(".")[0]] = [f"{template_file.split('.')[0]}/{key}" for key in template_data.keys()]

    return datasets


def make_consistent_batch(batch, batch_size, final_order, device, tokenizer):
    # Initialize new lists to hold the updated tensors
    new_base_inputs = []
    new_base_attention_mask = []
    new_base_labels = []
    new_src_inputs = []
    new_src_attention_mask = []
    new_src_labels = []

    compute_pos_src = batch.compute_pos("all")[0]
    compute_pos_base = batch.compute_pos("all")[1]

    new_pos_src = []
    new_pos_base = []

    # Iterate through each element in the batch
    for i in range(batch_size):
        if batch.base_types[i] == final_order[0]:
            # If base type matches the first type in final_order, keep as is
            new_base_inputs.append(batch.base['input_ids'][i])
            new_base_attention_mask.append(batch.base['attention_mask'][i])
            new_base_labels.append(batch.base_labels[i])
            new_pos_base.append(compute_pos_base[i])

            new_src_inputs.append(batch.src['input_ids'][i])
            new_src_attention_mask.append(batch.src['attention_mask'][i])
            new_src_labels.append(batch.src_labels[i])
            new_pos_src.append(compute_pos_src[i])
        else:
            # Swap base and src values
            new_base_inputs.append(batch.src['input_ids'][i])
            new_base_attention_mask.append(batch.src['attention_mask'][i])
            new_base_labels.append(batch.src_labels[i])
            new_pos_base.append(compute_pos_src[i])

            new_src_inputs.append(batch.base['input_ids'][i])
            new_src_attention_mask.append(batch.base['attention_mask'][i])
            new_src_labels.append(batch.base_labels[i])
            new_pos_src.append(compute_pos_base[i])

    # After looping, replace the old batch values with the new ones
    base = {
        'input_ids': torch.stack(new_base_inputs).to(device),
        'attention_mask': torch.stack(new_base_attention_mask).to(device),
        'labels': torch.stack(new_base_labels).to(device),
        'pos': new_pos_base
    }

    src = { 
        'input_ids': torch.stack(new_src_inputs).to(device),
        'attention_mask': torch.stack(new_src_attention_mask).to(device),
        'labels': torch.stack(new_src_labels).to(device),
        'pos': new_pos_src
    }

    new_batch = {
        'base': base,
        'src': src
    }

    return new_batch


def get_pos_interv(eval_dataset_name, train_dataset_name, diff, eval_first_var_pos, train_first_var_pos):
    # Get the position of the variable in the evaluation dataset
    eval_pos_base = eval_first_var_pos + diff
    pos_i = eval_pos_base

    if "filler_gap_wh" in train_dataset_name or "wh_question_embedded" in train_dataset_name:
        if "filler_gap_wh" in eval_dataset_name or eval_dataset_name == "embedded_wh" or eval_dataset_name == "controls":
            return pos_i
        elif eval_dataset_name == "preposing_in_pp" or eval_dataset_name == "wh_question":
            return pos_i + 1 if pos_i > eval_first_var_pos else pos_i
        else:
            raise ValueError(f"Unsupported eval dataset: {eval_dataset_name}")
    
    elif "preposing_in_pp" in train_dataset_name or "wh_question" in train_dataset_name:
        if pos_i == train_first_var_pos:
            return pos_i
        elif pos_i == train_first_var_pos + 1:
            return -1
        else:
            if eval_dataset_name == "preposing_in_pp" or eval_dataset_name == "wh_question":
                return pos_i
            elif "filler_gap_wh" in eval_dataset_name or eval_dataset_name == "embedded_wh" or eval_dataset_name == "controls":
                return pos_i - 1
            else:
                raise ValueError(f"Unsupported eval dataset: {eval_dataset_name}")
    
    else:
        raise ValueError(f"Unsupported train dataset: {train_dataset_name}")
    

def get_generalization_save_name(train_dataset_name):
    """
    Get a standardized name for saving the dataset based on the training dataset name.
    This function handles different naming conventions for datasets that involve leaving out certain clauses.
    """
    if 'leave_out' in train_dataset_name:
        if 'inanimate' in train_dataset_name and 'embedded' in train_dataset_name:
            save_dataset_name = 'leave_out_embedded_clause_inanimate'
        elif 'inanimate' in train_dataset_name:
            save_dataset_name = 'leave_out_single_clause_inanimate'
        elif 'embedded' in train_dataset_name:
            save_dataset_name = 'leave_out_embedded_clause_animate'
        else:
            save_dataset_name = 'leave_out_single_clause_animate'
    else:
        save_dataset_name = train_dataset_name
    
    return save_dataset_name


def get_loo_name(left):
    """
    Get a standardized name for the dataset based on the left variable.
    This function categorizes the dataset into embedded or single clauses and inanimate or animate categories.
    """
    embed = 'embedded_clause' in left
    inanimate = 'inanimate' in left
    
    match (embed, inanimate):
        case (True, True):
            name = "embedded_clause_inanimate/"+left
        case (True, False):
            name = "embedded_clause_animate/"+left
        case (False, True):
            name = "single_clause_inanimate/"+left
        case (False, False):
            name = "single_clause_animate/"+left
    
    return name


def single_double_pos(pos):
    """
    Function for the evaluation of single clause interventions on embedded clauses.
    Convert a position in the range 2-10 to a single or double clause position.
    Positions 2-6 map to themselves, position 7 maps to 2, and positions 8-10 map to positions 4-6 respectively.
    """
    assert (pos >= 2 and pos <= 10) or pos == -1, "Position must be between 2 and 10 (or invalid -1)"
    if pos < 7:
        return pos
    elif pos == 7:
        return 2
    else:
        return pos - 4

            
        