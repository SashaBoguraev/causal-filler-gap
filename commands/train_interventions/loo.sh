#!/bin/bash

# Check if model size argument is provided, default to 2.8b if not
MODEL_SIZE=${1:-2.8b}

# Set the model based on the input
if [[ "$MODEL_SIZE" == "2.8b" ]]; then
    MODEL="EleutherAI/pythia-2.8b"
elif [[ "$MODEL_SIZE" == "1.4b" ]]; then
    MODEL="EleutherAI/pythia-1.4b"
elif [[ "$MODEL_SIZE" == "14m" ]]; then
    MODEL="EleutherAI/pythia-14m"
elif [[ "$MODEL_SIZE" == "6.9b" ]]; then
    MODEL="EleutherAI/pythia-6.9b"
else
    echo "Invalid model size. Use '2.8b' or '1.4b' or '14m'."
    exit 1
fi

# Train the leave one out model for single_clause_inanimate
python causalgym/leave_one_out.py --model $MODEL --template single_clause_inanimate --left wh_question_inanimate
python causalgym/leave_one_out.py --model $MODEL --template single_clause_inanimate --left embedded_wh_finite_inanimate
python causalgym/leave_one_out.py --model $MODEL --template single_clause_inanimate --left embedded_wh_nonfinite_inanimate
python causalgym/leave_one_out.py --model $MODEL --template single_clause_inanimate --left restricted_rc_inanimate
python causalgym/leave_one_out.py --model $MODEL --template single_clause_inanimate --left cleft_inanimate
python causalgym/leave_one_out.py --model $MODEL --template single_clause_inanimate --left pseudo_cleft_inanimate
python causalgym/leave_one_out.py --model $MODEL --template single_clause_inanimate --left topicalization_inanimate

# Train the leave one out model for single_clause_animate
python causalgym/leave_one_out.py --model $MODEL --template single_clause_animate --left wh_question_animate
python causalgym/leave_one_out.py --model $MODEL --template single_clause_animate --left embedded_wh_finite_animate
python causalgym/leave_one_out.py --model $MODEL --template single_clause_animate --left embedded_wh_nonfinite_animate
python causalgym/leave_one_out.py --model $MODEL --template single_clause_animate --left restricted_rc_animate
python causalgym/leave_one_out.py --model $MODEL --template single_clause_animate --left cleft_animate
python causalgym/leave_one_out.py --model $MODEL --template single_clause_animate --left pseudo_cleft_animate
python causalgym/leave_one_out.py --model $MODEL --template single_clause_animate --left topicalization_animate

# Train the leave one out model for embedded_clause_animate
python causalgym/leave_one_out.py --model $MODEL --template embedded_clause_animate --left wh_question_animate_embedded_clause
python causalgym/leave_one_out.py --model $MODEL --template embedded_clause_animate --left embedded_wh_finite_animate_embedded_clause
python causalgym/leave_one_out.py --model $MODEL --template embedded_clause_animate --left embedded_wh_nonfinite_animate_embedded_clause
python causalgym/leave_one_out.py --model $MODEL --template embedded_clause_animate --left restricted_rc_animate_embedded_clause
python causalgym/leave_one_out.py --model $MODEL --template embedded_clause_animate --left cleft_animate_embedded_clause
python causalgym/leave_one_out.py --model $MODEL --template embedded_clause_animate --left pseudo_cleft_animate_embedded_clause
python causalgym/leave_one_out.py --model $MODEL --template embedded_clause_animate --left topicalization_animate_embedded_clause

# Train the leave one out model for embedded_clause_inanimate
python causalgym/leave_one_out.py --model $MODEL --template embedded_clause_inanimate --left wh_question_inanimate_embedded_clause
python causalgym/leave_one_out.py --model $MODEL --template embedded_clause_inanimate --left embedded_wh_finite_inanimate_embedded_clause
python causalgym/leave_one_out.py --model $MODEL --template embedded_clause_inanimate --left embedded_wh_nonfinite_inanimate_embedded_clause
python causalgym/leave_one_out.py --model $MODEL --template embedded_clause_inanimate --left restricted_rc_inanimate_embedded_clause
python causalgym/leave_one_out.py --model $MODEL --template embedded_clause_inanimate --left cleft_inanimate_embedded_clause
python causalgym/leave_one_out.py --model $MODEL --template embedded_clause_inanimate --left pseudo_cleft_inanimate_embedded_clause
python causalgym/leave_one_out.py --model $MODEL --template embedded_clause_inanimate --left topicalization_inanimate_embedded_clause

