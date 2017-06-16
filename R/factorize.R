factorize = function(df, ref_df, var, new_var) {
  # ref_df has labels associated with it.
  # Note: can pipe with magrittr pipe, a la: df %>% factorize(ref_df, var, new_var)
  
  # -- check var is within both df and ref_df --
  if(!var %in% colnames(df)) {
    stop('variable is not in the current dataset (argument df)')
  }
  
  if(!var %in% colnames(ref_df)) {
    stop('variable is not in the reference dataset (argument ref_df)')
  }
  
  # -- pull out the label values --
  codebk = data.frame(code = attr(ref_df[[var]], 'labels'))
  
  # -- pull out the label names --
  codebk = codebk %>% mutate(names =  row.names(codebk))
  
  # -- create a factor with the labels from the original dataset -- 
  # levels will be sorted by the frequency of occurance (high to low)
  df = df %>% 
    mutate_(.dots = setNames(
      list(paste0('forcats::fct_infreq(
                  factor(', var, ',',
                  'levels = ', list(codebk$code), ',',
                  'labels = ', list(codebk$names),'))'
      )), new_var 
    ))
  
  return(df)
}