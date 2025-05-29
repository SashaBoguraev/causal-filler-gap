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

# Single Clause Animate
python causalgym/test_all.py --model $MODEL --only-das --dataset single_clause_animate/wh_question_animate
python causalgym/test_all.py --model $MODEL --only-das --dataset single_clause_animate/embedded_wh_finite_animate
python causalgym/test_all.py --model $MODEL --only-das --dataset single_clause_animate/embedded_wh_nonfinite_animate
python causalgym/test_all.py --model $MODEL --only-das --dataset single_clause_animate/restricted_rc_animate
python causalgym/test_all.py --model $MODEL --only-das --dataset single_clause_animate/cleft_animate
python causalgym/test_all.py --model $MODEL --only-das --dataset single_clause_animate/pseudo_cleft_animate
python causalgym/test_all.py --model $MODEL --only-das --dataset single_clause_animate/topicalization_animate

# Single Clause Inanimate
python causalgym/test_all.py --model $MODEL --only-das --dataset single_clause_inanimate/wh_question_inanimate
python causalgym/test_all.py --model $MODEL --only-das --dataset single_clause_inanimate/embedded_wh_finite_inanimate
python causalgym/test_all.py --model $MODEL --only-das --dataset single_clause_inanimate/embedded_wh_nonfinite_inanimate
python causalgym/test_all.py --model $MODEL --only-das --dataset single_clause_inanimate/restricted_rc_inanimate
python causalgym/test_all.py --model $MODEL --only-das --dataset single_clause_inanimate/cleft_inanimate
python causalgym/test_all.py --model $MODEL --only-das --dataset single_clause_inanimate/pseudo_cleft_inanimate
python causalgym/test_all.py --model $MODEL --only-das --dataset single_clause_inanimate/topicalization_inanimate

# Embedded Clause Animate
python causalgym/test_all.py --model $MODEL --only-das --dataset embedded_clause_animate/wh_question_animate_embedded_clause
python causalgym/test_all.py --model $MODEL --only-das --dataset embedded_clause_animate/embedded_wh_finite_animate_embedded_clause
python causalgym/test_all.py --model $MODEL --only-das --dataset embedded_clause_animate/embedded_wh_nonfinite_animate_embedded_clause
python causalgym/test_all.py --model $MODEL --only-das --dataset embedded_clause_animate/restricted_rc_animate_embedded_clause
python causalgym/test_all.py --model $MODEL --only-das --dataset embedded_clause_animate/cleft_animate_embedded_clause
python causalgym/test_all.py --model $MODEL --only-das --dataset embedded_clause_animate/pseudo_cleft_animate_embedded_clause
python causalgym/test_all.py --model $MODEL --only-das --dataset embedded_clause_animate/topicalization_animate_embedded_clause

# Embedded Clause Inanimate
python causalgym/test_all.py --model $MODEL --only-das --dataset embedded_clause_inanimate/wh_question_inanimate_embedded_clause
python causalgym/test_all.py --model $MODEL --only-das --dataset embedded_clause_inanimate/embedded_wh_finite_inanimate_embedded_clause
python causalgym/test_all.py --model $MODEL --only-das --dataset embedded_clause_inanimate/embedded_wh_nonfinite_inanimate_embedded_clause
python causalgym/test_all.py --model $MODEL --only-das --dataset embedded_clause_inanimate/restricted_rc_inanimate_embedded_clause
python causalgym/test_all.py --model $MODEL --only-das --dataset embedded_clause_inanimate/cleft_inanimate_embedded_clause
python causalgym/test_all.py --model $MODEL --only-das --dataset embedded_clause_inanimate/pseudo_cleft_inanimate_embedded_clause
python causalgym/test_all.py --model $MODEL --only-das --dataset embedded_clause_inanimate/topicalization_inanimate_embedded_clause

# Controls
python causalgym/test_all.py --model $MODEL --only-das --dataset controls/control
python causalgym/test_all.py --model $MODEL --only-das --dataset lexical_controls/control_lexical_animate
python causalgym/test_all.py --model $MODEL --only-das --dataset lexical_controls/control_lexical_inanimate
python causalgym/test_all.py --model $MODEL --only-das --dataset embedded_controls/embedded_control
python causalgym/test_all.py --model $MODEL --only-das --dataset embedded_lexical_controls/embedded_control_lexical_animate
python causalgym/test_all.py --model $MODEL --only-das --dataset embedded_lexical_controls/embedded_control_lexical_inanimate