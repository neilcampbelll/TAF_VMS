
library(TAF)

# Download the VMStools .tar.gz file from GitHub
url <- "https://github.com/nielshintzen/vmstools/releases/download/0.77/vmstools_0.77.tar.gz"
download(url)

# install.dependencies ...

# Install the library from the downloaded .tar.gz file
taf.install("vmstools_0.77.tar.gz")
