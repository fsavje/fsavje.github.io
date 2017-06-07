rm(list = ls())

mandat <- read.table("mandat.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
mandat <- colSums(mandat[-1])
mandat <- mandat[c("V", "S", "MP", "SD", "C", "FP", "KD", "M")]
politisk.skala <- c(V = 12, S = 33, MP = 39, SD = 74, C = 63, FP = 66, KD = 68, M = 83)
politisk.skala <- politisk.skala[order(politisk.skala)]
ordinal.ps <- 1:length(politisk.skala)
names(ordinal.ps) <- names(politisk.skala)

koalitioner <- do.call(c, lapply(1:8, function (p) { combn(mandat, p, simplify = FALSE) }))
mandat.koal <- unlist(lapply(koalitioner, sum))

koal.majoritet <- koalitioner[mandat.koal >= 175]

koal.minimala <- koal.majoritet[unlist(lapply(koal.majoritet, function (k) {
  if (sum(k) - min(k) >= 175)
    return(FALSE)
  return(TRUE)
}))]

koal.minsta <- koalitioner[mandat.koal == min(mandat.koal[mandat.koal >= 175])]

partier.koal <- unlist(lapply(koalitioner, length))
koal.min.parti <- koalitioner[partier.koal == min(partier.koal[mandat.koal >= 175]) & mandat.koal >= 175]

koal.samk.minimal <- koal.majoritet[unlist(lapply(koal.majoritet, function (k) {
  ps.i <- which(names(politisk.skala) %in% names(k))
  if (!all(min(ps.i):max(ps.i) %in% ps.i))
    return(FALSE)
  if (sum(k) - min(k[names(politisk.skala)[min(ps.i)]], k[names(politisk.skala)[max(ps.i)]]) >= 175)
    return(FALSE)
  return(TRUE)
}))]

koal.minsta.bredd <- koal.minimala[which.min(unlist(lapply(koal.minimala, function (k) {
  ps.i <- which(names(politisk.skala) %in% names(k))
  return(abs(politisk.skala[max(ps.i)]  - politisk.skala[min(ps.i)]))
})))]

load("likhet.RData")
diag(parti.dist) <- NA
koal.mest.lik <- koal.majoritet[which.max(unlist(lapply(koal.majoritet, function (k) {
  min(parti.dist[names(k), names(k)], na.rm = TRUE)
})))]



PrintaKoalitioner <- function (koalitioner) {
  cat(paste("<div class=\"kmangd\">", paste(lapply(koalitioner, function (k) {
    paste("<ul class=\"k", length(k), "\">", paste(lapply(1:length(k), function (p) {
      paste("<li><img src=\"", names(k)[p], ".jpg\" />", k[p], "</li>", sep = "")
    }), collapse = " "), "</ul>", sep = "")
  }), collapse = "\n"), "</div>", sep = "\n"))
}

PrintaKoalitioner(koalitioner)
PrintaKoalitioner(koal.majoritet)
PrintaKoalitioner(koal.minimala)
PrintaKoalitioner(koal.minsta)
PrintaKoalitioner(koal.min.parti)
PrintaKoalitioner(koal.samk.minimal)
PrintaKoalitioner(koal.minsta.bredd)
PrintaKoalitioner(koal.mest.lik)
