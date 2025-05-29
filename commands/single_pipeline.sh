#!/bin/bash

MODEL_SIZE=${1:-1.4b}
NUM_BATCHES=${2:-25}
BATCH_SIZE=${3:-16}

commands/train_interventions/single.sh $MODEL_SIZE $NUM_BATCHES $BATCH_SIZE
commands/eval_interventions/single.sh $MODEL_SIZE $NUM_BATCHES $BATCH_SIZE
commands/eval_interventions/single_lexical_controls.sh $MODEL_SIZE $NUM_BATCHES $BATCH_SIZE