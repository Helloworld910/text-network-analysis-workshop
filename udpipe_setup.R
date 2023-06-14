#Install Packages(Only Once)
install.packages(udpipe)

#Load Libraries
library(udpipe)

#Download Model
udmodel <- udpipe_download_model(language = "english")
