

FROM rocker/r-ver:latest

ENV REQD_PKGS="\
    libicu-dev \
    g++ \
    gcc \
    gfortran \
    gsfonts \
    zlib1g-dev \
    make \
    libblas-dev \
    libssl-dev \
    pandoc \
    libxml2-dev \
    libcurl4-openssl-dev\
    git\
    libgit2-dev\
    pandoc-citeproc\
    libv8-dev\
    libsodium-dev\
    libgsl-dev\
    libpng-dev\
    libfontconfig1-dev\
    libopenblas-dev\
    liblapack-dev\
    libgmp3-dev\
    libfribidi-dev\
    libharfbuzz-dev\
    libjpeg-dev\
    libfreetype6-dev\
    libtiff5-dev \
    wget\
    gdebi-core \
    qpdf \
    devscripts \
    tk-dev \
    cmake \
    "

ENV CMDSTAN="/opt/cmdstan"


RUN apt-get update -y --fix-missing && \
    apt-get upgrade -y && \
    apt-get install -y $REQD_PKGS


COPY Rprofile.site /usr/local/lib/R/etc/Rprofile.site


# Install cmdstanr
RUN mkdir $CMDSTAN
RUN Rscript -e 'options(warn=2); install.packages(\
        "cmdstanr",\
        repos = c("https://mc-stan.org/r-packages/", getOption("repos"))\
    )'
RUN Rscript -e 'cmdstanr::install_cmdstan(Sys.getenv("CMDSTAN"))'


# Install other required libraries
RUN Rscript -e 'options(warn=2); install.packages(c(\
    "rstan",\
    "tidyverse",\
    "devtools",\
    "pec",\
    "flexsurv",\
    "survminer",\
    "broom",\
    "patchwork",\
    "tictoc",\
    "rriskDistributions",\
    "posterior",\
    "bayesplot",\
    "SurvMetrics",\
    "igraph",\
    "nimble",\
    "mvtnorm",\
    "coda",\
    "pracma",\
    "bfp",\
    "fields",\
    "jinjar",\
    "psfmi"\
    ))'


# Install latex for R CMD CHECK man conversions to pdf
RUN wget -qO- "https://yihui.org/tinytex/install-unx.sh" | sh -s - --admin --no-path && \
    ~/.TinyTeX/bin/*/tlmgr path add && \
    tlmgr install makeindex && \
    ln -s ~/.TinyTeX/bin/*/makeindex  /usr/local/bin

