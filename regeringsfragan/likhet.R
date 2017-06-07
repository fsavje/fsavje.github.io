rm(list = ls())

mandat <- read.table("slutmandat.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)

mandat$Ledamot <- unlist(lapply(mandat$Ledamot, function (n) {
  if (substr(n, nchar(n) - 12, nchar(n)) == " (personvald)")
    n <- substr(n, 1, nchar(n) - 13)
  n
}))

kandidater <- read.table("kandidater.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)

kandidater$namn <- paste(kandidater$förnamn, kandidater$efternamn)

kandidater$parti <- c("Folkpartiet" = "FP",
  "Moderaterna" = "M",
  "Socialdemokraterna" = "S",
  "Sverigedemokraterna" = "SD",
  "Miljöpartiet" = "MP",
  "Kristdemokraterna" = "KD",
  "Vänsterpartiet" = "V",
  "Centerpartiet" = "C",
  "Feministiskt initiativ" = "FI",
  "Piratpartiet" = "PP")[kandidater$parti]

valda.id <- apply(mandat, 1, function (x) {
  valda <- unique(kandidater$person_id[kandidater$parti == x["Parti"] & kandidater$namn == x["Ledamot"]])
  if (length(valda) > 1) {
    valda <- kandidater[kandidater$person_id %in% valda, ]
    valda <- valda$person_id[valda$valkrets == x["Valkrets"]]
  }
  return(valda)
})

enkat <- read.table("fragor.csv", header = TRUE, sep = ";")

svarat <- intersect(enkat$Id, valda.id)

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
  p1.index <- enkat$Id %in% kandidater$person_id[kandidater$parti == partier[p1]]
  c(rep(0, p1 - 1), unlist(lapply(p1:length(partier), function(p2) {
    mean(dist[p1.index, enkat$Id %in% kandidater$person_id[kandidater$parti == partier[p2]]])
  })))
})), ncol = length(partier))
parti.dist <- parti.dist + t(parti.dist) - 1 * diag(diag(parti.dist))
colnames(parti.dist) <- rownames(parti.dist) <- partier

parti.dist <- 100 - 100 * parti.dist / 180
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
