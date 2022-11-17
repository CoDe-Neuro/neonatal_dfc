# Neonatal brain dynamic functional connectivity: impact of preterm birth and association with early childhood neurodevelopment


## Download

### Github repository

```
git clone https://github.com/CoDe-Neuro/neonatal_dfc
```

### Additional data

```
curl -o data/2020_07_MASTER_connectomes90_All_select.mat https://zenodo.org/record/7053984/files/2020_07_MASTER_connectomes90_All_select.mat\?download\=1 
     -o data/df_clus_LEiDa_dim.csv https://zenodo.org/record/7053984/files/df_clus_LEiDa_dim.csv?download=1 
     -o data/df_kura.csv https://zenodo.org/record/7053984/files/df_kura.csv?download=1 
     -o data/df_LEiDa.csv https://zenodo.org/record/7053984/files/df_LEiDa.csv?download=1
```

## BOLD signal LEiDA and KOP analyses (Python)

```
pip install pip install dynfc==0.0.3a1
```

```
cd src

for i in {1..390}
do
   python run_bstates.py $i
   python run_kuramoto.py $i
done
```

```
python run_pca.py 390 LEiDa 0
python run_bigClus.py 390 LEiDa 0
python merge_km.py 390 kura 0
```

## 

## Funding

<img src="https://upload.wikimedia.org/wikipedia/commons/5/58/Wellcome_Trust_logo.svg" width="300">

This project is funded by a Wellcome Trust Seed Award in Science [[217316/Z/19/Z]](https://europepmc.org/grantfinder/grantdetails?query=pi%3A%22Batalle%2BD%22%2Bgid%3A%22217316%22%2Bga%3A%22Wellcome%20Trust%22).

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