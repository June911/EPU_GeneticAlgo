# EPU_GeneticAlgo
Developing Category-specific Economic Policy Uncertainty Indices through Genetic Algorithm

1. Extend keywords with a pre-trained word embeddings space
2. Select keywords with most discriminative power towards target variable  
3. This method can be applied to a lot of economic varible and be used to build reasonable proxy from text. 


- This project presents a new approach to building category-specific Economic Policy Uncertainty (EPU) indices by extracting keywords from texts via the genetic algorithm. 
- Knowing that the EPU indices vary with different sets of EPU terms, we let the EPU terms vary to fit a specific target variable to obtain the final optimized EPU index. 
- We proceed in three steps: 
  - first, with initial EPU terms, we use a pre-trained word embedding space to extend the set of candidate keywords. 
  - Second, using the genetic algorithm, we search for the best subset of candidate keywords that matches the dynamics of a pre-determined target variable. 
  - Last, following the same steps proposed by \href{https://www.policyuncertainty.com/media/EPU_BBD_Mar2016.pdf}{Baker et al. (2016)}, we build the new category-specific EPU index. 