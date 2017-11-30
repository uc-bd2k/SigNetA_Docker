FROM shiny.ilincs.org:5000/shiny:3.3.1-1.5.1
#FROM shiny.ilincs.org:5000/shiny
COPY . /srv/shiny-server

# RUN apt-get update
# RUN apt-get install libmariadb-client-lgpl-dev 
# RUN R -e "source('https://bioconductor.org/biocLite.R'); biocLite('Biobase'); biocLite('ComplexHeatmap'); biocLite('circlize'); 
#install.packages('shinyjs'); install.packages('RMySQL'); "
#RUN apt-get update
#RUN apt-get install -y libmariadb-client-lgpl-dev


RUN apt-get update -qq && \
    apt-get install -y \
    pkg-config \
    libnlopt-dev \
    libmariadb-client-lgpl-dev \
    zlib1g-dev \
    libssh2-1-dev \
    libxml2-dev \
    curl \
    libpng-dev \
    sudo \
    wget \
    git \
    r-base

#RUN R -e "source('https://bioconductor.org/biocLite.R'); biocLite('Biobase'); biocLite('ComplexHeatmap'); biocLite('circlize'); "
RUN R -e "source('https://bioconductor.org/biocLite.R'); biocLite('Biobase'); biocLite('BioNet'); biocLite('circlize'); biocLite('DLBCL'); "
RUN R -e "install.packages(c('shinyjs', 'RMySQL'), repos = 'http://cran.us.r-project.org');"

RUN apt-get install -y libxml2-dev libx11-dev
RUN apt-get install -y libglu1-mesa-dev freeglut3-dev mesa-common-dev
RUN R -e "install.packages(c('devtools', 'rsconnect','DT','igraph','networkD3','visNetwork','shinydashboard'), repos = 'http://cran.us.r-project.org');"
RUN R -e "install.packages(c('shinyRGL', 'rgl'), repos = 'http://cran.us.r-project.org');"


RUN R -e "devtools::install_github('uc-bd2k/CLEAN')"

RUN R -e "devtools::install_github('uc-bd2k/CLEAN.Hs')"


RUN R -e "devtools::install_github('Saadman/SigNetA')"



