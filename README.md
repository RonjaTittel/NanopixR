# First-time setup

Before running *any* analysis, run the *interactive* setup once: 

```{r}
library(CellpixR)
setup()
```

This configures the required Python / 'Cellpose' environment.

# in Session setup

Please run at the beginning of *each* R Session:

```{r}
library(CellpixR)
setup()
```

to ensure that all dependencies are connected correctly. Do not use any 
'retuculate::' functions before. That will lead to a connection with an incorrect 
Python environment.
