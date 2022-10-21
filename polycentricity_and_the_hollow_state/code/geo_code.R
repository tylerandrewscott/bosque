## Compute the distances between all US Zipcodes.
## To save time we just compute the diagonal and use multiple cores.
require(parallel)
library(geosphere)
library(MASS)
library(zipcode)
data(zipcode)
## Uncode for testing
## zipcode <- zipcode[1:10,]
zips.num = dim(zipcode)[1]
zips.range = 1:zips.num
head(zipcode)


data(zips)
head(zips)
epa_systems$`Zip Code`[1]
library(RgoogleMaps)




summary(net ~ b1nodematch('COUNTY'))
summary(net ~ b1nodematch('COUNTY',beta = 0.5))

warnings()
test = do.call(rbind,zip_address)
library(ergm.userterms)


summary()

table(is.na(epa_systems$`Address Line1`[match(system_df$PWS_ID,epa_systems$PWS_ID)]))



## d <- matrix(0, zips.num, zips.num)
computeDists <- function(x) {
  cat(paste("Row", x, paste("(", zipcode$zip[x], ")", sep=""), "\n"))
  m <- matrix(0, 1, zips.num)
  for (j in x:zips.num) {
    # cat(paste(x, j, "\n"))
    m[1, j] <- distHaversine(zipcode[j, c(5,4)],
                             zipcode[x, c(5,4)])
  }
  return(m)
}
l <- mclapply(zips.range, computeDists, mc.cores=4, mc.preschedule=TRUE)
dd <- do.call(rbind, l)
dimnames(dd) <- list(zipcode$zip, zipcode$zip)
write.matrix(dd, file="zipdist.tsv", sep="\t")