# Analysis

This is the folder which we use to run the analysis of our results. Some notes:

- For most of the R scripts in this folder, you must specify the model size you want to use, with the default being `1.4b` (as used in the main text).  
- To run `constructional_analysis.ipynb`, first generate the relevant Parquet files from `gap_generalization_hm.R` and `embedded_gap_generalization_hm.R`.  
- To run `centrality.R`, execute `constructional_analysis.ipynb` first.  
- To run `frequency.ipynb`, download [UD release v2.16](https://lindat.mff.cuni.cz/repository/xmlui/handle/11234/1-5901$0) and place it in `analysis/ud/`.  