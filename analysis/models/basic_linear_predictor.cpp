#include <TMB.hpp>
using namespace density;
using Eigen::SparseMatrix; 


// Objective function
template<class Type>
Type objective_function<Type>::operator() () {
  
  /////////////////////////////////////////////////////////////////////////////////////////////////////////
  // MODEL INPUTS
  /////////////////////////////////////////////////////////////////////////////////////////////////////////
  
  // DATA, INPUTS, AND INDICES 
  //""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  // Define data supplied to the model
  DATA_VECTOR(outcome_i);        // charge ratio
  DATA_MATRIX(X_ij);            // covariates (at minimum, a column for the intercept)

    DATA_INTEGER( n_i );          // Total number of observations
  
  
  // MODEL PARAMETERS
  //""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  // fixed effects (intercept, covariate effects)
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  PARAMETER_VECTOR(betas_j);
  PARAMETER(log_SDmod);
  
  
  /////////////////////////////////////////////////////////////////////////////////////////////////////////
  // FITTING MODEL
  /////////////////////////////////////////////////////////////////////////////////////////////////////////
  //setting the starting joint negative log liklihood to 0
  Type jnll = 0; 
  // Structure for setting up the joint Negative Log liklihood to have multiple parts (if desired):
  //Type jnll = 0; //setting the starting joint negative log liklihood to 0
  //vector<Type> jnll_comp(3); // Making the jnll_comp have 3 different parts (fixed, random effects)
  //jnll_comp.setZero(); // Setting those pieces to initial start values of 0
  //Later in the code:  Type jnll = jnll_comp.sum(); return jnll;
  
  //parallel_accumulator<Type> jnll(this);
  
  // COMPUTER PARAMETERS
  //""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""    
  //How much computing power do we have at our dipsosal?
  //max_parallel_regions = omp_get_max_threads(); 
  
  // Using covariates to predict the log mortality rate
  vector<Type> linpred = X_ij * betas_j;  

  // How likely are the ratio values we observe compared to the expected value of given our linear predictor and random effects, on a normal distribution?
  for(size_t i = 0; i < n_i; i++){ // For each data point, starting at the first data point...
    jnll -= dnorm(outcome_i[i], linpred[i], exp(log_SDmod), true); //calculate the likelihood of the data given the expected value
  }
  
  ADREPORT(betas_j);
  ADREPORT(log_SDmod);
  
  REPORT(jnll);
  REPORT(betas_j);
 
  return jnll;
}
