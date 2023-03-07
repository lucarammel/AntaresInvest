## Maintenance

To maintain the code over the time, please follow the instructions below in order to remain comprehensive, and keep good practices in mind :

* Functionalize : **1 function = 1 action** or goal. Otherwhise, it is so difficult to understand the aim of what you are doing. No more scripts with more than 1000 lines.
* Names : names has to be **short but self-sufficient** and comprehensive. **Keep the same case style** for the whole code.
* Parameters : **no hardcode**, everything has to be parametered and always think about the **scalability** of your code. It makes your code **robust**.
* Data structure : before going in a liste, a vector, or dataframe, please **think about what is the best data structure** in case you change, adapt, scale up your code or your data.
* Docstring : every function has to be documented ! Do it, no documentation means a loss of time for your readers. Make it clear as possible.
* Space : Please, when you read a book, you love spaces, same story when you read a code.
* **Advice for R** :
  * use pipes `%>%` from `magrittr` or `dplyr` packages. It avoids this kind of code : `fun(fun(fun(fun(object))))` but we got this instead : `object %>% fun(.) %>% fun(.)` ..
  * use `lapply` or `sapply` as soon as you can. It is very useful to make for loop in one liner.
  * To print messages with a variable inside or build very short path, DON'T use `paste0(..)`. Instead use, as f-string in Python, `glue('blabla {variable} blabla')` or `paste` for paths.
  * Data types :
    * `character`, `numeric`, `integer`..
    * `vector` -> `c(element1, element2)` , elements have to be of the same type
    * `list` -> like Python dictionnaries.  Every keys is associated with an element. Very useful to store many datatypes, with a structure and access it shortly doing list$key1, be brave and make list of lists : `list$key1$key2`. To access, you can also use `list[[key1]][[key2]]`
    * `dataframe`, `tibble`, `data.table` .. -> Tables -> Every table types have its own specific syntax.
  * There are 3 types of tables in R, tibble from dplyr, data.table from data.table and dataframe from base R. Best are tibble and data.table for their quickness and their functionalities implemented. Dataframe from base R works as Python, Tibble is only a layer of base dataframe and everything works with base dataframe.  Transtypes are very easy so don't hesitate and take advantage of the best you can from them.
  * use `=` ou `<-` to assign value to variable but keep the same style! If you want to make global variables, use `<<-`
  * If you want to update the package documentation, use `devtools::check(<path_to_the_package>)`

To put it in a nutshell, at every time, don't forget that :

> *Any fool can write code that a computer can understand. Good programmers write code that humans can understand*
  
