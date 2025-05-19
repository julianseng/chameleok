# Description
This is a R package on GitHub that implements the DataFly algorithm for achieving k-anonymity. DataFly is a heuristic that runs quickly but does not achieve optimal generalisation. As far as I know, when I first wrote this code in 2021, there was no package on CRAN. I used the code as part of a large simulation study for my PhD. I hope you find it useful.
The easiest way to use it is to clone the repository and then run the vignette, which will install the package on your computer.

# Install
## Option 1: Install directly from GitHub
devtools::install_github("username/ChameleoK")

## Option 2: Clone the repository and install locally
```{shell}
git clone https://github.com/username/ChameleoK.git
cd ChameleoK
```

then
```{r}
devtools::install()
```
