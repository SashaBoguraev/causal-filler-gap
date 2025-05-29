#!/bin/bash

# Check if model size argument is provided, default to 2.8b if not
MODEL_SIZE=${1:-1.4b}

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
    echo "Invalid model size. Use '6.9b', '2.8b', '1.4b' or '14m'."
    exit 1
fi

SOURCE_SETS_SINGLE_CLAUSE_INANIMATE=(
    "single_clause_inanimate/wh_question_inanimate"
    "single_clause_inanimate/embedded_wh_finite_inanimate"
    "single_clause_inanimate/embedded_wh_nonfinite_inanimate"
    "single_clause_inanimate/restricted_rc_inanimate"
    "single_clause_inanimate/cleft_inanimate"
    "single_clause_inanimate/pseudo_cleft_inanimate"
    "single_clause_inanimate/topicalization_inanimate"
    "lexical_controls/control_lexical_inanimate"
)

SOURCE_SETS_SINGLE_CLAUSE_ANIMATE=(
    "single_clause_animate/wh_question_animate"
    "single_clause_animate/embedded_wh_finite_animate"
    "single_clause_animate/embedded_wh_nonfinite_animate"
    "single_clause_animate/restricted_rc_animate"
    "single_clause_animate/cleft_animate"
    "single_clause_animate/pseudo_cleft_animate"
    "single_clause_animate/topicalization_animate"
    "lexical_controls/control_lexical_animate"
)

SOURCE_SETS_EMBEDDED_CLAUSE_INANIMATE=(
    "embedded_clause_inanimate/wh_question_inanimate_embedded_clause"
    "embedded_clause_inanimate/embedded_wh_finite_inanimate_embedded_clause"
    "embedded_clause_inanimate/embedded_wh_nonfinite_inanimate_embedded_clause"
    "embedded_clause_inanimate/restricted_rc_inanimate_embedded_clause"
    "embedded_clause_inanimate/cleft_inanimate_embedded_clause"
    "embedded_clause_inanimate/pseudo_cleft_inanimate_embedded_clause"
    "embedded_clause_inanimate/topicalization_inanimate_embedded_clause"
    "embedded_lexical_controls/embedded_control_lexical_inanimate"
)

SOURCE_SETS_EMBEDDED_CLAUSE_ANIMATE=(
    "embedded_clause_animate/wh_question_animate_embedded_clause"
    "embedded_clause_animate/embedded_wh_finite_animate_embedded_clause"
    "embedded_clause_animate/embedded_wh_nonfinite_animate_embedded_clause"
    "embedded_clause_animate/restricted_rc_animate_embedded_clause"
    "embedded_clause_animate/cleft_animate_embedded_clause"
    "embedded_clause_animate/pseudo_cleft_animate_embedded_clause"
    "embedded_clause_animate/topicalization_animate_embedded_clause"
    "embedded_lexical_controls/embedded_control_lexical_animate"
)

NUM_BATCHES=${2:-25}

BATCH_SIZE=${3:-16}

EVAL_SEEDS=(41)

# Run the generalization script for each source set and evaluation set
for SEED in "${EVAL_SEEDS[@]}"; do
    for SOURCE_SET in "${SOURCE_SETS_SINGLE_CLAUSE_ANIMATE[@]}"; do
        python generalization.py --model_id $MODEL --eval_seeds $SEED --source_set $SOURCE_SET --eval_set lexical_controls/control_lexical_animate --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES
    done
    for SOURCE_SET in "${SOURCE_SETS_SINGLE_CLAUSE_INANIMATE[@]}"; do
        python generalization.py --model_id $MODEL --eval_seeds $SEED --source_set $SOURCE_SET --eval_set lexical_controls/control_lexical_inanimate --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES
    done
    for SOURCE_SET in "${SOURCE_SETS_EMBEDDED_CLAUSE_ANIMATE[@]}"; do
        python generalization.py --model_id $MODEL --eval_seeds $SEED --source_set $SOURCE_SET --eval_set embedded_lexical_controls/embedded_control_lexical_animate --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES
    done
    for SOURCE_SET in "${SOURCE_SETS_EMBEDDED_CLAUSE_INANIMATE[@]}"; do
        python generalization.py --model_id $MODEL --eval_seeds $SEED --source_set $SOURCE_SET --eval_set embedded_lexical_controls/embedded_control_lexical_inanimate --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES
    done
done