# Calculate shock estimate + standard errors

# calculate average shock value by EA to determine if shock is covariate.
# caclulate raster
# calculate consumption lag
# find the panel


# calculate covariate shock -----------------------------------------------


ea = shk %>% 
  group_by(ea, cause_cat) %>% 
  count(shocked) %>% 
  ungroup() %>% 
  group_by(ea, cause_cat) %>% 
  mutate(pct = n/sum(n)) %>% 
  filter(shocked == 1) %>% 
  arrange(desc(pct))