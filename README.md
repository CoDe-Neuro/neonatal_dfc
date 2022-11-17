# Neonatal brain dynamic functional connectivity: impact of preterm birth and association with early childhood neurodevelopment

![GitHub](https://img.shields.io/github/license/CoDe-Neuro/neonatal_dfc)

## Download

The easiest way to obtain the data used in our analysis is as follows:

### Github repository

First, clone this Github repository with the command:

```
git clone https://github.com/CoDe-Neuro/neonatal_dfc
```

### Additional data

You will need to download some additional datasets we made available via Zenodo. [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7053984.svg)](https://doi.org/10.5281/zenodo.7053984)

```
cd neonatal_dfc

curl -o data/2020_07_MASTER_connectomes90_All_select.mat https://zenodo.org/record/7053984/files/2020_07_MASTER_connectomes90_All_select.mat\?download\=1 
```

:warning: If you do not want to (or cannot) perform the BOLD signal analyses in Python, you can use the following command and jump directly to the figures and stats sections (in R).

```
cd neonatal_dfc

curl -o data/2020_07_MASTER_connectomes90_All_select.mat https://zenodo.org/record/7053984/files/2020_07_MASTER_connectomes90_All_select.mat\?download\=1 
     -o data/df_clus_LEiDa_dim.csv https://zenodo.org/record/7053984/files/df_clus_LEiDa_dim.csv?download=1 
     -o data/df_kura.csv https://zenodo.org/record/7053984/files/df_kura.csv?download=1 
     -o data/df_LEiDa.csv https://zenodo.org/record/7053984/files/df_LEiDa.csv?download=1
```

## BOLD signal LEiDA and KOP analyses (Python)

First we need to install the dynfc libs.

```
pip install pip install dynfc==0.0.3a1
```

The following commands will obtain both LEiDA and KOP for every subject in the dataset.

```
cd src

for i in {1..390}
do
   python run_bstates.py $i
   python run_kuramoto.py $i
done
```

:warning: This can take a while to calculate. Consider running in parallel to reduce time consumed.

```
python run_pca.py 390 LEiDa 0
python run_bigClus.py 390 LEiDa 0
python merge_km.py 390 kura 0
```
:warning: The routines above should demand a considerable amount of RAM.

## Figures + stats (R)

To guarantee reproducibility and reduce issues involved in solving dependencies, all the routines available here run in a Docker container. Please download the last version of the software at their website: https://www.docker.com/

To run the Docker container with the defined settings

```
docker-compose up
```

All routines for the analyses in this article are available in the R Markdown document to ease reproduction and visualisation. Open the link http://localhost:8787 and then the file index.Rmd when the RStudio loads. The web page should work as a regular RStudio IDE.

:warning: The login and password are 'rstudio' and 'letmein', respectively.

:warning: We performed several permutation tests (n = 10,000) which can take a while to run.

When you are finished with your analysis stop the Docker container with the command:

```
docker-compose down
```

## Funding

<img src="https://upload.wikimedia.org/wikipedia/commons/5/58/Wellcome_Trust_logo.svg" width="300">

This project was funded by a Wellcome Trust Seed Award in Science [[217316/Z/19/Z]](https://europepmc.org/grantfinder/grantdetails?query=pi%3A%22Batalle%2BD%22%2Bgid%3A%22217316%22%2Bga%3A%22Wellcome%20Trust%22).

## License

MIT License

Copyright (c) 2022

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
