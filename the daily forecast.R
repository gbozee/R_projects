# i think we only use eia daily data ,sice most of the oil price are the same
require(EIAdata)
# Fetch datasets
key<-("95365B2462BFD45A96D4EB0DBC60E59A")
oil<-getEIA("PET.RBRTE.D",key)
oil







