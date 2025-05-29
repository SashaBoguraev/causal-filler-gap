MODEL_SIZE=${1:-1.4b}
NUM_BATCHES=${2:-25}
BATCH_SIZE=${3:-16}

# Experiment 1
commands/loo_pipeline.sh $MODEL_SIZE $NUM_BATCHES $BATCH_SIZE
python results/generalization/process_csv.py -lo

# Experiment 2
commands/single_pipeline.sh $MODEL_SIZE $NUM_BATCHES $BATCH_SIZE
python results/generalization/process_csv.py -c

# Experiment 3
commands/eval_interventions/eval_single_double.sh $MODEL_SIZE $NUM_BATCHES $BATCH_SIZE
python results/generalization/process_csv.py -sd