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
    echo "Invalid model size.  Use '6.9b', '2.8b', '1.4b' or '14m'."
    exit 1
fi

NUM_BATCHES=${2:-25}

BATCH_SIZE=${3:-16}

CLAUSE_TEMPLATE=(
    "single_clause_"
    "embedded_clause_"
)

ANIMACY=(
    "inanimate"
    "animate"
)

CONSTRUCTIONS=(
    "wh_question"
    "embedded_wh_finite"
    "embedded_wh_nonfinite"
    "restricted_rc"
    "cleft"
    "pseudo_cleft"
    "topicalization"
)

EVAL_SEEDS=(41)

# Single Clause Animate and Inanimate
for SEED in "${EVAL_SEEDS[@]}"; do
    for CLAUSE in "${CLAUSE_TEMPLATE[@]}"; do
        for ANIMACY in "${ANIMACY[@]}"; do
            for CONSTRUCTION in "${CONSTRUCTIONS[@]}"; do
                SOURCE_SET="single_clause_${ANIMACY}/${CONSTRUCTION}_${ANIMACY}"
                EVAL_SET="embedded_clause_${ANIMACY}"
                python generalization.py --model_id $MODEL --eval_seeds $SEED --source_set $SOURCE_SET --eval_set $EVAL_SET --batch_size $BATCH_SIZE -sd --num_batches $NUM_BATCHES
            done
        done
    done
done

# Controls
for SEED in "${EVAL_SEEDS[@]}"; do
    python generalization.py --model_id $MODEL --eval_seeds $SEED --source_set "controls/control" --eval_set "embedded_clause_animate" --batch_size $BATCH_SIZE -sd --num_batches $NUM_BATCHES
    python generalization.py --model_id $MODEL --eval_seeds $SEED --source_set "controls/control" --eval_set "embedded_clause_inanimate" --batch_size $BATCH_SIZE -sd --num_batches $NUM_BATCHES
    python generalization.py --model_id $MODEL --eval_seeds $SEED --source_set "lexical_controls/control_lexical_animate" --eval_set "embedded_clause_animate" --batch_size $BATCH_SIZE -sd --num_batches $NUM_BATCHES
    python generalization.py --model_id $MODEL --eval_seeds $SEED --source_set "lexical_controls/control_lexical_inanimate" --eval_set "embedded_clause_inanimate" --batch_size $BATCH_SIZE -sd --num_batches $NUM_BATCHES
done