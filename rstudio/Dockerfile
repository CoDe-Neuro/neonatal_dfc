FROM rocker/verse:4.1.1

USER root

ADD git_config.sh github_libs.R nightowlish.rstheme rstudio-prefs.json /rstudio/

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    fonts-dejavu \
    tzdata \
    apt-utils \
    gfortran \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libnlopt-dev \
    libxml2-dev \
    gcc && apt-get clean && \
    rm -rf /var/lib/apt/lists/*



RUN R -e "install.packages('cowplot',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('janitor',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('kableExtra',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggExtra',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('rjson',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('broom.mixed',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plotly',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggsci',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shadowtext',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('effsize',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggimage',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggpubr',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('modelsummary',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('RColorBrewer',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('patchwork',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggbeeswarm',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('reshape2',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('tidymodels',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggtext',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('lmerTest',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('gt',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggpmisc',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggforce',dependencies=TRUE, repos='http://cran.rstudio.com/')"

RUN Rscript -e "remotes::install_github('LCBC-UiO/ggseg', build_vignettes = FALSE)"
RUN Rscript -e "remotes::install_github('LCBC-UiO/ggseg3d', build_vignettes = FALSE)"
RUN Rscript -e "remotes::install_github('LCBC-UiO/ggsegDesterieux')"
RUN Rscript -e "remotes::install_github('slowkow/ggrepel')"
RUN Rscript -e "remotes::install_github('sctyner/geomnet')"
RUN Rscript -e "remotes::install_github('BlakeRMills/MetBrewer')"
RUN Rscript -e "remotes::install_github('CoDe-Neuro/stateR', build_vignettes = FALSE)"

COPY nightowlish.rstheme /etc/rstudio/themes
COPY rstudio-prefs.json /etc/rstudio/

COPY git_config.sh /etc/cont-init.d/gitconfig
