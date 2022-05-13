To make the code more understandable, we make a few flowcharts using the following R packages:

- flow (flowchart of if/else/for/while):
    > flow_view(<function_name>)
    
- proftools (flowchart with main function calls during actual run):
    [Add Rprof(<file.out>) before call to func_run_model, then Rprof(NULL) just after]
    > Rprofile_out <- readProfileData(filename = "rprof.out")
    > plotProfileCallGraph(Rprofile_out, nodeDetails = TRUE, edgeDetails = TRUE, mergeCycles = TRUE)

- DependenciesGraphs (flowchart with functions interdependencies, as an interactive HTML app):
    > deps <- funDependencies(".GlobalEnv", "func_run_model")
    > plot(deps)
    [Then export as HTML from RStudio]

We do this on the model version of 2022/05/13.
