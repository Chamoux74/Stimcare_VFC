library(tibble)
library(purrr)
library(dplyr)
library(readr)
library(TimeDomainHRV)
library(varian)
library(RHRV)


# vfcpatch <-
#   list.files(
#     path = "C:/Users/maxch/Git/VFC/data/PATCH",
#     pattern = "\\.txt",
#     all.files = TRUE,
#     full.names = TRUE
#   )

vfcpatch <-
  list.files(
    path = "C:/Users/maxch/Git/VFC/data/PATCH_sec",
    pattern = "\\.txt",
    all.files = TRUE,
    full.names = TRUE
  )

listdfvfcpatch <- lapply(vfcpatch , read.table)
names(listdfvfcpatch) <- tools::file_path_sans_ext(basename(vfcpatch))

#transformation des données en secondes

# listdfvfcpatch <- lapply(listdfvfcpatch, function(bob){as.data.frame(bob$V1/1000)})
#
# listdfvfcpatch <- lapply(listdfvfcpatch, setNames, "V1")
#
# #filtrer 4 minutes de la 5 à la 9 pour être sur d'avoir une période stable
#
# listdfvfcpatch <-
#   lapply(listdfvfcpatch, function(bob) {
#     filter(bob, cumsum(bob$V1) > 240 &
#              cumsum(bob$V1) < 540)
#   })

#listdfvfcpatch <- lapply(listdfvfcpatch, function (bob){filter(bob, cumsum(c(0, V1)) > 240 & cumsum(c(0, V1)) < 540})

#export du fichier en seconde en format txt

# mapply(FUN = function(bob, bob_name) {
#   write.table(bob, file = paste0("C:/Users/maxch/Git/VFC/output/PATCH_sec/", bob_name, ".txt"))
# },
# bob = listdfvfcpatch,
# bob_name = names(listdfvfcpatch))

#import de la nouvelle liste de données



