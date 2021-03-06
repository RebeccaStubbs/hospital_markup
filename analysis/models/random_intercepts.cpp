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
  
  DATA_IVECTOR(state_i);        // state indicator
  DATA_IVECTOR(county_i);       // county indicator
  DATA_IVECTOR(prov_i);     // Provider indicator
  DATA_IVECTOR(year_i);     // year indicator
  
  // Indices
  DATA_INTEGER( n_i );          // Total number of observations
  DATA_INTEGER( n_state );      // Number of states
  DATA_INTEGER( n_county );      // Number of counties
  DATA_INTEGER( n_prov );      // Number of counties
  DATA_INTEGER( n_year );      // Number of years
  
  //DATA_IVECTOR(states_counties_long_i);        // state indicator with repeats for each county
  //DATA_IVECTOR(counties_providers_long_i);       // county indicator with repeats for each provider
  
  
  
  // MODEL PARAMETERS
  //""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  // fixed effects (intercept, covariate effects)
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  PARAMETER_VECTOR(betas_j);
  PARAMETER(log_SDmod);
  
  // STATE
  PARAMETER_VECTOR(state_re); // RE for each state
  PARAMETER(log_sd_state_re); // SD of the states (from national)
  
  // COUNTY
  PARAMETER_VECTOR(county_re); // RE for each county, which is a deviation from the state mean
  PARAMETER(log_sd_county_re); // SD of RE

  // FACILITY
  PARAMETER_VECTOR(prov_re); // RE for each facility
  PARAMETER(log_sd_prov_re); // SD of RE
  
  // YEAR
  PARAMETER_VECTOR(year_re); // RE for each year
  PARAMETER(log_sd_year_re); // SD of RE

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
  
  // ADDING IN RANDOM EFFECTS
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Probability of random effects...
    
    // States deviating from national
    for( int i=0; i<n_state; i++){
      jnll -= dnorm( state_re(i), Type(0.0), exp(log_sd_state_re), true );
    }
    
    // Counties deviating from states
    for( int i=0; i<n_county; i++){
      jnll -= dnorm( county_re(i), Type(0.0), exp(log_sd_county_re), true );
    }
  
    // Facilities deviating from counties
    for( int i=0; i<n_prov; i++){
      jnll -= dnorm( prov_re(i), Type(0.0), exp(log_sd_prov_re), true );
    }
    
    // Facilities deviating from counties
    for( int i=0; i<n_year; i++){
      jnll -= dnorm( year_re(i), Type(0.0), exp(log_sd_year_re), true );
    }

  // Get the random effect values, and add them onto the expected linear predictor
      for(size_t i = 0; i < n_i; i++){
        linpred[i] += state_re[state_i[i]]+county_re[county_i[i]]+prov_re[prov_i[i]]+year_re[year_i[i]];
      }

  // How likely are the ratio values we observe compared to the expected value of given our linear predictor and random effects, on a normal distribution?
  for(size_t i = 0; i < n_i; i++){ // For each data point, starting at the first data point...
    jnll -= dnorm(outcome_i[i], linpred[i], exp(log_SDmod), true); //calculate the likelihood of the data given the expected value
  }
  
  ADREPORT( betas_j );
  ADREPORT( log_SDmod );
  ADREPORT( log_sd_state_re );
  ADREPORT( log_sd_county_re );
  ADREPORT( log_sd_prov_re );
  ADREPORT( state_re );
  ADREPORT( county_re );
  ADREPORT( prov_re );
  ADREPORT( year_re );
  

  REPORT(jnll);
  REPORT(betas_j);
  
  REPORT( log_sd_state_re );
  REPORT( log_sd_county_re );
  REPORT( log_sd_prov_re );
  REPORT( state_re );
  REPORT( county_re );
  REPORT( prov_re );
  REPORT( year_re );
  
 
  return jnll;
}
