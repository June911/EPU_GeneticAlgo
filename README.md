# EPU_GeneticAlgo

- This project presents a new approach to building category-specific Economic Policy Uncertainty (EPU) indices by extracting keywords from texts via the genetic algorithm. 
- Knowing that the EPU indices vary with different sets of EPU terms, we let the EPU terms vary to fit a specific target variable to obtain the final optimized EPU index. 
- We proceed in three steps: 
  - First, with initial EPU terms, we use a pre-trained word embedding space to extend the set of candidate keywords. 
    - A word embedding space maps words into high dimensional vectors. Words with similar semantic meanings are close to each other in the space. We use the pre-trained embedding space from [Pennington et al.](https://github.com/stanfordnlp/GloVe) with GloVe Technique. 
  - Second, using the genetic algorithm, we search for the best subset of candidate keywords that matches the dynamics of a pre-determined target variable. 
  - Last, following the same steps proposed by [Baker et al. (2016)](https://www.policyuncertainty.com/media/EPU_BBD_Mar2016.pdf), we build the new category-specific EPU index. 
