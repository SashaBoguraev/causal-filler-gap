#!/bin/bash

# Check if model size argument is provided, default to 2.8b if not
MODEL_SIZE=${1:-1.4b}
NUM_BATCHES=${2:-25}
BATCH_SIZE=${3:-16}

commands/train_interventions/loo.sh $MODEL_SIZE $NUM_BATCHES $BATCH_SIZE
commands/eval_interventions/loo.sh $MODEL_SIZE $NUM_BATCHES $BATCH_SIZE
commands/eval_interventions/loo_lexical_controls.sh $MODEL_SIZE $NUM_BATCHES $BATCH_SIZE
