
# Make outputs

If you want to get outputs of the model, you will have to recover them at each loop for each year_actual :

1. In *get_data_outputs_model()* add the line in the for loop over areas to create your variable with the following architecture :

```R
data_outputs[[zone]][[year_actual]][[s]]$variable_name =  dictionnary (list in R) #where you can access your variable such as dataset_revenues or dataset
```

2. In *build_from_data_outputs()* add the lines :

```R
variable_name = list() # initialise the variable to store your outputs

for area, year, s in ... : 

  variable_name[[area]][[year]][[s]]  = data_outputs[[area]][[year]][[s]]$variable_name #Use this line if the variable is a a table or anything more complex than a scalar
  
  variable_name[[area]][[year]] = c(variable_name[[area]][[year]], data_outputs[[area]][[year]][[s]]$variable_name) #Use this line if your variable is a scalar
  
#Let's melt the data to move from a dictionnary of dictionnary to a simple table which concatenate all the element from the variable_name dictionnary
#If your variable is a scalar only

variable_name[[area]] %<>% as.data.frame(.) %>% setNames(names(data_outputs[[area]])) 
variable_name %<>% melt(., id.vars = NULL)  %>%  mutate(loop = rep(rownames(variable_name[[1]]), length(colnames(variable_name[[1]])) * length(areas)))   %>%setNames(c('year', 'variable_name', 'area', 'loop'))

#For tables: 
variable_name %<>% melt(., id.vars = to_complete) %>% setNames(to_complete)

#At the end, return your variable_name in the returned list R.

```

3. In *make_outputs* add the lines :

```R
#Recover your variable and build your own graphic:

out$variable_name

buid_variable_name_graphic(variable_name) #Create your function in 6_2_make_graphs.R

file_variable_name = paste(study_dir, 'variable_name.xlsx', sep = '/')
write.xlsx(variable_name, file_variable_name, row.names = FALSE) #Create the output in excel file
```