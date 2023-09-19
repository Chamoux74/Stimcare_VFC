library(tibble)
library(purrr)
library(dplyr)
library(readr)
library(TimeDomainHRV)
library(varian)
library(RHRV)


# vfcplacebo <-
#   list.files(
#     path = "C:/Users/maxch/Git/VFC/data/PLACEBO",
#     pattern = "\\.txt",
#     all.files = TRUE,
#     full.names = TRUE
#   )

vfcplacebo <-
  list.files(
    path = "C:/Users/maxch/Git/VFC/data/PLACEBO_sec",
    pattern = "\\.txt",
    all.files = TRUE,
    full.names = TRUE
  )

listdfvfcplacebo <- lapply(vfcplacebo , read.table)
names(listdfvfcplacebo) <- tools::file_path_sans_ext(basename(vfcplacebo))

#transformation des données en secondes

# listdfvfcplacebo <- lapply(listdfvfcplacebo, function(bob){as.data.frame(bob$V1/1000)})
#
# listdfvfcplacebo <- lapply(listdfvfcplacebo, setNames, "V1")
#
# #filtrer 4 minutes de la 5 à la 9 pour être sur d'avoir une période stable
#
# listdfvfcplacebo <-
#   lapply(listdfvfcplacebo, function(bob) {
#     filter(bob, cumsum(bob$V1) > 240 &
#              cumsum(bob$V1) < 540)
#   })

#listdfvfcpatch <- lapply(listdfvfcpatch, function (bob){filter(bob, cumsum(c(0, V1)) > 240 & cumsum(c(0, V1)) < 540})

#export du fichier en seconde en format txt

# mapply(FUN = function(bob, bob_name) {
#   write.table(bob, file = paste0("C:/Users/maxch/Git/VFC/output/PLACEBO_sec/", bob_name, ".txt"))
# },
# bob = listdfvfcplacebo,
# bob_name = names(listdfvfcplacebo))

#import de la nouvelle liste de données



