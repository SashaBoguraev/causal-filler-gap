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

EVAL_SEEDS=(41)

for SEED in "${EVAL_SEEDS[@]}"; do
    python generalization.py --model_id $MODEL --eval_seeds $SEED --left wh_question_inanimate --eval_set lexical_controls/control_lexical_inanimate  --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES
    python generalization.py --model_id $MODEL --eval_seeds $SEED --left embedded_wh_finite_inanimate --eval_set lexical_controls/control_lexical_inanimate --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES
    python generalization.py --model_id $MODEL --eval_seeds $SEED --left embedded_wh_nonfinite_inanimate --eval_set lexical_controls/control_lexical_inanimate --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES
    python generalization.py --model_id $MODEL --eval_seeds $SEED --left restricted_rc_inanimate --eval_set lexical_controls/control_lexical_inanimate --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES
    python generalization.py --model_id $MODEL --eval_seeds $SEED --left cleft_inanimate --eval_set lexical_controls/control_lexical_inanimate --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES
    python generalization.py --model_id $MODEL --eval_seeds $SEED --left pseudo_cleft_inanimate --eval_set lexical_controls/control_lexical_inanimate --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES
    python generalization.py --model_id $MODEL --eval_seeds $SEED --left topicalization_inanimate --eval_set lexical_controls/control_lexical_inanimate --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES

    python generalization.py --model_id $MODEL --eval_seeds $SEED --left wh_question_animate --eval_set lexical_controls/control_lexical_animate --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES
    python generalization.py --model_id $MODEL --eval_seeds $SEED --left embedded_wh_finite_animate --eval_set lexical_controls/control_lexical_animate --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES
    python generalization.py --model_id $MODEL --eval_seeds $SEED --left embedded_wh_nonfinite_animate --eval_set lexical_controls/control_lexical_animate --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES
    python generalization.py --model_id $MODEL --eval_seeds $SEED --left restricted_rc_animate --eval_set lexical_controls/control_lexical_animate --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES
    python generalization.py --model_id $MODEL --eval_seeds $SEED --left cleft_animate --eval_set lexical_controls/control_lexical_animate --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES
    python generalization.py --model_id $MODEL --eval_seeds $SEED --left pseudo_cleft_animate --eval_set lexical_controls/control_lexical_animate --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES
    python generalization.py --model_id $MODEL --eval_seeds $SEED --left topicalization_animate --eval_set lexical_controls/control_lexical_animate --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES

    python generalization.py --model_id $MODEL --eval_seeds $SEED --left wh_question_inanimate_embedded_clause --eval_set embedded_lexical_controls/embedded_control_lexical_inanimate --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES
    python generalization.py --model_id $MODEL --eval_seeds $SEED --left embedded_wh_finite_inanimate_embedded_clause --eval_set embedded_lexical_controls/embedded_control_lexical_inanimate --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES
    python generalization.py --model_id $MODEL --eval_seeds $SEED --left embedded_wh_nonfinite_inanimate_embedded_clause --eval_set embedded_lexical_controls/embedded_control_lexical_inanimate --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES
    python generalization.py --model_id $MODEL --eval_seeds $SEED --left restricted_rc_inanimate_embedded_clause --eval_set embedded_lexical_controls/embedded_control_lexical_inanimate --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES
    python generalization.py --model_id $MODEL --eval_seeds $SEED --left cleft_inanimate_embedded_clause --eval_set embedded_lexical_controls/embedded_control_lexical_inanimate --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES
    python generalization.py --model_id $MODEL --eval_seeds $SEED --left pseudo_cleft_inanimate_embedded_clause --eval_set embedded_lexical_controls/embedded_control_lexical_inanimate --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES
    python generalization.py --model_id $MODEL --eval_seeds $SEED --left topicalization_inanimate_embedded_clause --eval_set embedded_lexical_controls/embedded_control_lexical_inanimate --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES

    python generalization.py --model_id $MODEL --eval_seeds $SEED --left wh_question_animate_embedded_clause --eval_set embedded_lexical_controls/embedded_control_lexical_animate --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES
    python generalization.py --model_id $MODEL --eval_seeds $SEED --left embedded_wh_finite_animate_embedded_clause --eval_set embedded_lexical_controls/embedded_control_lexical_animate --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES
    python generalization.py --model_id $MODEL --eval_seeds $SEED --left embedded_wh_nonfinite_animate_embedded_clause --eval_set embedded_lexical_controls/embedded_control_lexical_animate --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES
    python generalization.py --model_id $MODEL --eval_seeds $SEED --left restricted_rc_animate_embedded_clause --eval_set embedded_lexical_controls/embedded_control_lexical_animate --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES
    python generalization.py --model_id $MODEL --eval_seeds $SEED --left cleft_animate_embedded_clause --eval_set embedded_lexical_controls/embedded_control_lexical_animate --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES
    python generalization.py --model_id $MODEL --eval_seeds $SEED --left pseudo_cleft_animate_embedded_clause --eval_set embedded_lexical_controls/embedded_control_lexical_animate --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES
    python generalization.py --model_id $MODEL --eval_seeds $SEED --left topicalization_animate_embedded_clause --eval_set embedded_lexical_controls/embedded_control_lexical_animate --batch_size $BATCH_SIZE --num_batches $NUM_BATCHES
done