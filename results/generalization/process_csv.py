import pandas as pd
import os
import argparse

def process_csv(file_path, leave_out, single_double):
    """
    Process a single CSV file and return a DataFrame with the processed results.
    :param file_path: The path to the CSV file to process.
    :param leave_out: Boolean indicating if the leave out experiment is being processed.
    :param single_double: Boolean indicating if the single double experiment is being processed.
    :return: A DataFrame with the processed results.
    """
    # Read the CSV file
    df = pd.read_csv(file_path, index_col="Unnamed: 0")
    model, from_construction, to_construction, seed = get_meta_info(file_path)

    # Precompute constant values to avoid redundant computation
    constant_values = {
        'from': from_construction,
        'to': to_construction,
        'seed': seed,
        'model': model,
        'leave_out': leave_out,
        'single_double': single_double,
    }

    # Flatten the DataFrame and process in bulk
    rows = []
    for r_idx, row in df.iterrows():
        for c_idx, col in enumerate(df.columns):
            val_dictionary = eval(row[col])

            # Compute derived values
            val_dictionary['iia'] = val_dictionary['p_src'] > val_dictionary['p_base']
            val_dictionary['iia_flip'] = val_dictionary['p_src'] > val_dictionary['p_base'] and val_dictionary['base_p_base'] > val_dictionary['base_p_src']
            val_dictionary['odds_ratio'] = val_dictionary['base_p_base'] - val_dictionary['base_p_src'] + val_dictionary['p_src'] - val_dictionary['p_base']
            val_dictionary['eval_loss'] = val_dictionary['loss']

            # Add constant values
            val_dictionary.update(constant_values)

            rows.append(val_dictionary)

    # Create the output DataFrame in one step
    output_df = pd.DataFrame(rows)
    return output_df


def get_meta_info(file_path):
    """
    Extract meta information from the file path.
    :param file_path: The file path to extract information from.
    :return: A tuple containing the model, from_construction, to_construction, and seed.
    """
    model_idx = file_path.index('pythia')
    model = file_path[model_idx: file_path.index('_', model_idx)]
    from_construction = file_path[file_path.index('from_') + 5: file_path.index('_to_')]
    to_construction = file_path[file_path.index('to_') + 3: file_path.index('_seed')]
    seed = file_path[file_path.index('_seed') + 5: file_path.index('.csv')]
    return model, from_construction, to_construction, seed


def process_directory(directory_path, directories, experiment):
    """
    Process all CSV files in the given directory and its subdirectories recursively.
    :param directory_path: A list with paths to the directories containing the CSV files.
    :return: A DataFrame with the processed results.
    """
    output_df = pd.DataFrame()

    for entry in os.listdir(directory_path):

        entry_path = os.path.join(directory_path, entry)
        
        in_dir = [directory in entry_path for directory in directories]
        if not any(in_dir):
            continue

        if os.path.isfile(entry_path) and entry.endswith('.csv'):
            print(f"Processing {entry_path}")
            if entry == 'conjunct_accuracy.csv':
                continue
            leave_out = 'leave_out' in experiment
            single_double = 'single_double' in experiment

            output_df = pd.concat(
                [output_df, process_csv(entry_path, leave_out=leave_out, single_double=single_double)],
                ignore_index=True
            )
        elif os.path.isdir(entry_path):
            output_df = pd.concat(
                [output_df, process_directory(entry_path, directories, experiment)],
                ignore_index=True
            )

    return output_df


def save_df_to_parquet(df, output_path):
    """
    Save the DataFrame to a parquet file.
    :param df: The DataFrame to save.
    :param output_path: The path to save the parquet file to.
    """
    df.to_parquet(output_path, index=False)


def main(leave_out, single_double, prep_punct, classic):
    assert not (leave_out and single_double and prep_punct and classic), "Only one experiment can be selected at a time."
    if leave_out:
        experiment = 'leave_out'
    elif single_double:
        experiment = 'single_double'
    elif classic:
        experiment = 'classic'
    else:
        print("No experiment selected. Running classic by default.")
        experiment = 'classic'

    
    file_lists = {
        'single_double': ['results/generalization/single_double'],
        'leave_out': [
            'results/generalization/leave_out_single_clause_animate',
            'results/generalization/leave_out_single_clause_inanimate',
            'results/generalization/leave_out_embedded_clause_animate',
            'results/generalization/leave_out_embedded_clause_inanimate'
        ],
        'classic': [
            'results/generalization/single_clause_animate',
            'results/generalization/single_clause_inanimate',
            'results/generalization/embedded_clause_animate',
            'results/generalization/embedded_clause_inanimate'
        ]
    }

    parent_directory = 'results/generalization'
    directories = file_lists[experiment]
    output_file = f"results/generalization/{experiment}.parquet"

    # Process the directory and save the results to a parquet file
    output_df = process_directory(parent_directory, directories, experiment)
    save_df_to_parquet(output_df, output_file)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Process CSV files and save to parquet.")
    parser.add_argument('--leave_out', '-lo', action='store_true', help="Process leave out files")
    parser.add_argument('--single_double', '-sd', action='store_true', help="Process single double files")
    parser.add_argument('--classic', '-c', action='store_true', help="Process classic files")
    args = parser.parse_args()
    main(**vars(args))
