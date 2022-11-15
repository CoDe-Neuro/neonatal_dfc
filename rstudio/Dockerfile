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

RUN install2.r -e cowplot \
    janitor \
    kableExtra \
    ggExtra \
    rjson \
    broom.mixed \
    plotly \
    ggsci \
    shadowtext \
    effsize \
    ggimage \
    ggpubr \
    modelsummary \
    cowplot \
    RColorBrewer \
    kableExtra \
    rjson \
    broom \
    plotly \
    ggsci \
    shadowtext \
    effsize \
    ggimage \
    ggpubr \
    patchwork \
    modelsummary \
    ggbeeswarm \
    reshape2

RUN install2.r -s --error \
    tidymodels \
    ggtext \
    lmerTest \
    gt \
    ggpmisc \
    ggforce

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
