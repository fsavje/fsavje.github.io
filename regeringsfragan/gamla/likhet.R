rm(list = ls())

mandat <- read.table("mandat.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
# V och FP har en valkrets för Stockholms lön och kommun
mandat[17, "FP"] <- mandat[17, "FP"] + mandat[16, "FP"] 
mandat[17, "V"] <- mandat[17, "V"] + mandat[16, "V"] 
mandat[16, "FP"] <- mandat[16, "V"] <- 0
# SD har en valkrets för hela landet
SD.temp <- sum(mandat[, "SD"])
mandat[, "SD"] <- 0
mandat <- rbind(mandat, c(list("Hela landet 1"), as.list(rep(0, 7)), list(SD.temp)))

kandidater <- read.table("kandidater.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
kandidater$valkrets[kandidater$valkrets == "Stockholms stad och län"] <- "Stockholms län"

valda <- data.frame()
invisible(lapply(setdiff(colnames(mandat), "dist"), function (p) {
  lapply(1:nrow(mandat), function (d) {
    if (mandat[d, p] > 0) {
      valbara <- kandidater[(kandidater$parti == p & 
                               kandidater$valkrets == mandat[d, "dist"] &
                               !(kandidater$person_id %in% valda$person_id)), ]
      valda <<- rbind(valda, valbara[order(valbara$position)[1:mandat[d, p]], ])
    }
  })
}))

enkat <- read.table("fragor.csv", header = TRUE, sep = ";")

svarat <- intersect(enkat$Id, valda$person_id)

valda <- valda[valda$person_id %in% svarat, ]
enkat <- enkat[enkat$Id %in% svarat, ]

enkat <- as.data.frame(lapply(enkat, function (c) {
  unlist(lapply(c, function(a) {
    if (is.numeric(a))
      return(a)
    if (a  == "Mycket bra förslag")
      return(2)
    if (a  == "Ganska bra förslag")
      return(1)
    if (a  == "Ganska dåligt förslag")
      return(-1)
    if (a  == "Mycket dåligt förslag")
      return(-2)
    if (a  == "Ingen åsikt")
      return(0)
    stop("error")
  } ))
}))

resp <- as.matrix(enkat[, paste("X", 1:45, sep = "")])
resp.prio <- as.matrix(enkat[, paste("X", 1:45, "P", sep = "")])
dist <- matrix(c(do.call(c, lapply(1:(nrow(resp) - 1), function (i) {
  c(rep(0, i), rowSums(t(t(resp.prio[(i+1):nrow(resp), , drop = FALSE]) + resp.prio[i, ] + 1) * 
                         abs(t(t(resp[(i+1):nrow(resp), , drop = FALSE]) - resp[i, ]))))
})), rep(0, nrow(resp))), ncol = nrow(resp))
dist <- dist + t(dist)

partier <- c("V", "S", "MP", "C", "FP", "KD", "M", "SD")
parti.dist <- matrix(do.call(c, lapply(1:length(partier), function(p1) {
  p1.index <- enkat$Id %in% valda$person_id[valda$parti == partier[p1]]
  c(rep(0, p1 - 1), unlist(lapply(p1:length(partier), function(p2) {
    mean(dist[p1.index, enkat$Id %in% valda$person_id[valda$parti == partier[p2]]])
  })))
})), ncol = length(partier))
parti.dist <- parti.dist + t(parti.dist) - 1 * diag(diag(parti.dist))
colnames(parti.dist) <- rownames(parti.dist) <- partier

parti.dist <- 100 - 100 * parti.dist / max(parti.dist)
save(parti.dist, file = "likhet.RData", compression_level = 9)



tabell <- matrix(character(length = 0), ncol = ncol(parti.dist) + 1, nrow = nrow(parti.dist) + 1)
tabell[1, 1] <- "<td></td>\n"
tabell[2:ncol(tabell), 1] <- tabell[1, 2:ncol(tabell)] <- paste0("<td>", colnames(parti.dist), "</td>\n")
tabell[2:ncol(tabell), 2:ncol(tabell)] <- unlist(lapply(round(parti.dist), function(c) {
  paste0("<td style=\"background: rgb(100%, ", floor(20 + (100 - c) * 0.8),"%, ", floor(20 + (100 - c) * 0.8),"%);\">", c, "</td>\n")
}))
cat(paste0("<table>\n", paste0(apply(tabell, 2, function(c) {
  paste0("<tr>\n", paste0(c, collapse = ""), "</tr>\n")
}), collapse = ""), "</table>"))
