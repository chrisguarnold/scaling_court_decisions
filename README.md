# Scaling Courts with Citation Networks

## Project Team
* Chris Arnold, Cardiff University, arnoldc6@cardiff.ac.uk
* Benjamin Engst, University of Mannheim, engst@uni-mannheim.de
* Thomas Gschwend, University of Mannheim, gschwend@uni-mannheim.de



## Setup
For interoperability with the data base that was indexed with ElasticSearch, we were using first Python, and then R and STAN. Since multiple languages may easily cause a headache, we did our best in being as consistent as possible:
* We use R as a wrapper to run the whole code. AEG_replication_master.r will generate all figures and tables from scratch.
* All data management up to the data for estimation is implemented in Python. The antitrust case is a bit of an exception, since it used the Juris frontend.
* Once all data for estimation is written, R takes over.
* We implemented the bayesian estimation with STAN.

For more information on the overall structure of the code, please see the section on the [Code](#code) below.

### Python Environment
We suggest you create a python environment that looks just like ours. The necessary information for the relevant libraries can be found in the file ```environment.yml```. If you need further help, the [conda documentation](https://docs.conda.io/projects/conda/en/latest/user-guide/tasks/manage-environments.html#creating-an-environment-from-an-environment-yml-file) is your friend.


You can create the environment from ``AEG_slc.yml``
```
      conda env create -f AEG_slc.yml
```
And then activate the new environment:
```
      conda activate AEG_slc
```


For calling Python from within R, the [reticulate documentation](https://rstudio.github.io/reticulate/index.html) will offer a good introduction. Don't forget to adapt the python path to your conda environment in the project-```.Rprofile```.

### R Environment

## Code
The whole replication files will run from beginning to the end via executing ```AEG_replication_master.r```. It will
* read in the pickles;
* generate .csvs for estimation;
* estimate the models;
* generate results;
* implement posterior predictive checks.

All files are relative to ```AEG_replication_master.r```. You can run it
* via RStudio: Open ```replication_AEG.Rproj```. RStudio then sets the correct working directory;
* from Terminal: Make sure to set the working directory as fit.


## Questions?
For all questions regarding the code, feel free to reach out to Chris Arnold. 
