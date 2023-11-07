#Let's try Michael Kowalewski's code https://rdrr.io/github/MJKowalewski/PaleoFidelity/src/R/FidelityEst.R

FidelityEst <- function (live, dead, gp = NULL, cor.measure = "spearman",
                         sim.measure = "chao", n.filters = 0, t.filters = 1,
                         report = FALSE, iter = 10, iter2 = 10, min.sam = 30, CI = 0.95,
                         rm.zero = FALSE, tfsd = "none")
{
  out <- FidelitySummary(live, dead, gp, report = report, output = TRUE,
                         n.filters = n.filters, t.filters = t.filters)
  if (length(out) == 2) {
    live <- out$live
    dead <- out$dead
  }
  if (length(out) == 3) {
    live <- out$live
    dead <- out$dead
    gp <- out$gp
  }
  my.cor.F <- function(x, y) {
    if (rm.zero) {
      if (sum(colSums(rbind(x, y)) == 0) > 0) {
        good.taxa <- which(colSums(rbind(x, y)) > 0)
        x <- x[good.taxa]
        y <- y[good.taxa]
      }
    }
    stats::cor(x, y, method = cor.measure)
  }
  
  x1 <- as.data.frame(t(live))
  x2 <- as.data.frame(t(dead))
  cor.e <- mapply(function(x, y) my.cor.F(as.vector(x), as.vector(y)),
                  x1, x2)
  x3 <- x1
  x4 <- x2
  if (tfsd == "total") {
    x3 <- vegan::decostand(x1, "total")
    x4 <- vegan::decostand(x2, "total")
  }
  if (tfsd == "total4") {
    x3 <- vegan::decostand(x1, "total")^(1/4)
    x4 <- vegan::decostand(x2, "total")^(1/4)
  }
  if (tfsd == "wisconsin") {
    x3 <- vegan::wisconsin(x1)
    x4 <- vegan::wisconsin(x2)
  }
  if (tfsd == "log") {
    x3 <- vegan::decostand(x1, "log")
    x4 <- vegan::decostand(x2, "log")
  }
  if (tfsd == "r4") {
    x3 <- x1^0.25
    x4 <- x2^0.25
  }
  sim.e <- mapply(function(x, y) 1 - vegan::vegdist(rbind(x,
                                                          y), method = sim.measure), x3, x4)
  fid.sam <- cbind(cor.e, sim.e)
  colnames(fid.sam) <- c(cor.measure, sim.measure)
  rownames(fid.sam) <- rownames(live)
  raw.stats <- cbind(mean(cor.e), mean(sim.e))
  colnames(raw.stats) <- c(cor.measure, sim.measure)
  rownames(raw.stats) <- "all samples"
  if (length(gp) == 0)
    mean.gp <- NA
  if (length(gp) > 0 & nrow(live) > 1) {
    mean.gp <- cbind(tapply(cor.e, gp, mean), tapply(sim.e,
                                                     gp, mean))
    colnames(mean.gp) <- c(cor.measure, sim.measure)
    raw.stats <- rbind(raw.stats, mean.gp)
  }
  raw.x <- cbind(raw.stats[, 1])
  raw.y <- cbind(raw.stats[, 2])
  if (iter2 >= 10) {
    LCL <- (1 - CI)/2
    UCL <- 1 - LCL
    fid.subsam <- function(x, y, min) {
      a <- as.data.frame(t(vegan::rrarefy(x, sample = min)))
      b <- as.data.frame(t(vegan::rrarefy(y, sample = min)))
      cor.ab <- mapply(function(x, y) my.cor.F(as.vector(x),
                                               as.vector(y)), a, b)
      sim.ab <- mapply(function(x, y) 1 - vegan::vegdist(rbind(x,
                                                               y), method = sim.measure), a, b)
      cbind(cor.ab, sim.ab)
    }
    outSS <- array(NA, dim = c(nrow(live), 2, iter2))
    for (i in 1:iter2) outSS[, , i] <- fid.subsam(live, dead,
                                                  min = min.sam)
    SSCOR <- cbind(est = rowMeans(rbind(outSS[, 1, ])), t(apply(rbind(outSS[,
                                                                            1, ]), 1, stats::quantile, prob = c((1 - CI)/2, 1 -
                                                                                                                  (1 - CI)/2))), n.std = min.sam)
    SSSIM <- cbind(est = rowMeans(rbind(outSS[, 2, ])), t(apply(rbind(outSS[,
                                                                            2, ]), 1, stats::quantile, prob = c((1 - CI)/2, 1 -
                                                                                                                  (1 - CI)/2))), n.std = min.sam)
    colnames(SSCOR)[1:3] <- c(paste(cor.measure, ".SS",
                                    sep = ""), paste(LCL), paste(UCL))
    colnames(SSSIM)[1:3] <- c(paste(sim.measure, ".SS",
                                    sep = ""), paste(LCL), paste(UCL))
    rownames(SSCOR) <- rownames(live)
    rownames(SSSIM) <- rownames(live)
    ifelse(nrow(live) > 1, xss <- colMeans(outSS[, 1, ]),
           xss <- mean(outSS[, 1, ]))
    ifelse(nrow(live) > 1, yss <- colMeans(outSS[, 2, ]),
           yss <- mean(outSS[, 2, ]))
    xss2 <- stats::quantile(xss, prob = c(LCL, 0.5, UCL))
    yss2 <- stats::quantile(yss, prob = c(LCL, 0.5, UCL))
    xs.stat <- cbind(mean = mean(xss), LCL = xss2[1],
                     median = xss2[2], UCL = xss2[3])
    rownames(xs.stat) <- "all samples"
    ys.stat <- cbind(mean = mean(yss), LCL = yss2[1],
                     median = yss2[2], UCL = yss2[3])
    rownames(ys.stat) <- "all samples"
    if (length(gp) > 0 & nrow(live) > 1) {
      xsg <- apply(outSS[, 1, ], 2, function(x) tapply(x,
                                                       gp, mean))
      xsg2 <- t(rbind(apply(xsg, 1, mean), apply(xsg, 1,
                                                 stats::quantile, prob = c(LCL, 0.5, UCL))))
      colnames(xsg2) <- c("mean", "LCL", "median",
                          "UCL")
      xs.stat <- rbind(`all samples` = xs.stat, xsg2)
      ysg <- apply(outSS[, 2, ], 2, function(x) tapply(x,
                                                       gp, mean))
      ysg2 <- t(rbind(apply(ysg, 1, mean), apply(ysg, 1,
                                                 stats::quantile, prob = c(LCL, 0.5, UCL))))
      colnames(ysg2) <- c("mean", "LCL", "median",
                          "UCL")
      ys.stat <- rbind(`all samples` = ys.stat, ysg2)
    }
    colnames(xs.stat)[c(2, 4)] <- c(paste(LCL), paste(UCL))
    colnames(ys.stat)[c(2, 4)] <- c(paste(LCL), paste(UCL))
    ss.cor.rep <- t(outSS[, 1, ])
    ss.sim.rep <- t(outSS[, 2, ])
    colnames(ss.cor.rep) <- rownames(live)
    colnames(ss.sim.rep) <- rownames(live)
  }
  if (iter >= 10) {
    FidPerfModel <- function(live, dead) {
      pooled <- live + dead
      rlive <- as.vector(vegan::rrarefy(x = pooled, sum(live)))
      rdead <- as.vector(vegan::rrarefy(x = pooled, sum(dead)))
      r.cor.e = my.cor.F(rlive, rdead)
      rx3 <- rlive
      rx4 <- rdead
      if (tfsd == "total") {
        rx3 <- vegan::decostand(rlive, "total")
        rx4 <- vegan::decostand(rdead, "total")
      }
      if (tfsd == "total4") {
        rx3 <- vegan::decostand(rlive, "total")^(1/4)
        rx4 <- vegan::decostand(rdead, "total")^(1/4)
      }
      if (tfsd == "wisconsin") {
        rx3 <- vegan::wisconsin(rlive)
        rx4 <- vegan::wisconsin(rdead)
      }
      if (tfsd == "log") {
        rx3 <- vegan::decostand(rlive, "log")
        rx4 <- vegan::decostand(rdead, "log")
      }
      if (tfsd == "r4") {
        rx3 <- rlive^0.25
        rx4 <- rdead^0.25
      }
      r.sim.e = 1 - vegan::vegdist(t(cbind(rx3, rx4)),
                                   method = sim.measure)
      return(c(r.cor.e, r.sim.e))
    }
    pf.output <- array(0, dim = c(iter, nrow(live), 2), dimnames = list(1:iter,
                                                                        rownames(live), c(cor.measure, sim.measure)))
    for (i in 1:iter) {
      for (j in 1:nrow(live)) {
        pf.output[i, j, ] <- FidPerfModel(rbind(live[j,
        ]), rbind(dead[j, ]))
      }
    }
    perfidest <- apply(pf.output, c(2, 3), stats::median)
    cor.obs.rep <- matrix(fid.sam[, 1], iter, length(fid.sam[,
                                                             1]), byrow = T)
    sim.obs.rep <- matrix(fid.sam[, 2], iter, length(fid.sam[,
                                                             2]), byrow = T)
    cor.m.adj <- (1 - pf.output[, , 1]) + cor.obs.rep
    sim.m.adj <- (1 - pf.output[, , 2]) + sim.obs.rep
    cor.m.adj[cor.m.adj > 1] <- 1
    sim.m.adj[sim.m.adj > 1] <- 1
    cor.adj.sam <- apply(cor.m.adj, 2, mean)
    sim.adj.sam <- apply(sim.m.adj, 2, mean)
    cor.adj.sam.CI <- apply(cor.m.adj, 2, stats::quantile,
                            prob = c(LCL, UCL))
    sim.adj.sam.CI <- apply(sim.m.adj, 2, stats::quantile,
                            prob = c(LCL, UCL))
    corrected <- cbind(cor.adj.sam, sim.adj.sam)
    corrected.mean <- c(mean(cor.m.adj), mean(sim.m.adj))
    names(corrected.mean) <- c(cor.measure, sim.measure)
    xc.stat <- c(mean(corrected[, 1]), stats::quantile(corrected[,
                                                                 1], prob = c(LCL, 0.5, UCL)))
    yc.stat <- c(mean(corrected[, 2]), stats::quantile(corrected[,
                                                                 2], prob = c(LCL, 0.5, UCL)))
    if (length(gp) == 0) {
      xc.stat <- rbind(xc.stat)
      yc.stat <- rbind(yc.stat)
      colnames(xc.stat) <- c("mean", paste(LCL),
                             "median", paste(UCL))
      colnames(yc.stat) <- c("mean", paste(LCL),
                             "median", paste(UCL))
      rownames(xc.stat) <- "all samples"
      rownames(yc.stat) <- "all samples"
    }
    if (length(gp) > 0 & nrow(live) > 1) {
      statsgp.x1 <- tapply(corrected[, 1], gp, mean)
      statsgp.x2 <- tapply(corrected[, 1], gp, stats::quantile,
                           prob = c(LCL, 0.5, UCL))
      statsgp.x3 <- cbind(statsgp.x1, matrix(unlist(statsgp.x2),
                                             length(levels(gp)), 3, byrow = T))
      statsgp.y1 <- tapply(corrected[, 2], gp, mean)
      statsgp.y2 <- tapply(corrected[, 2], gp, stats::quantile,
                           prob = c(LCL, 0.5, UCL))
      statsgp.y3 <- cbind(statsgp.y1, matrix(unlist(statsgp.y2),
                                             length(levels(gp)), 3, byrow = T))
      xc.stat <- rbind(all.samples = xc.stat, statsgp.x3)
      yc.stat <- rbind(all.samples = yc.stat, statsgp.y3)
      colnames(xc.stat) <- c("mean", paste(LCL),
                             "median", paste(UCL))
      colnames(yc.stat) <- c("mean", paste(LCL),
                             "median", paste(UCL))
    }
    xc.stat <- as.matrix(xc.stat)
    yc.stat <- as.matrix(yc.stat)
    xc.sum <- cbind(corrected[, 1], t(cor.adj.sam.CI),
                    perfidest[, 1])
    colnames(xc.sum) <- c(paste(cor.measure, ".ADJ", sep = ""),
                          paste(LCL), paste(UCL), paste(cor.measure, ".PF",
                                                        sep = ""))
    yc.sum <- cbind(corrected[, 2], t(sim.adj.sam.CI),
                    perfidest[, 2])
    colnames(yc.sum) <- c(paste(sim.measure, ".ADJ", sep = ""),
                          paste(LCL), paste(UCL), paste(sim.measure, ".PF",
                                                        sep = ""))
    if (nrow(live) == 1) {
      raw.stats <- c(NA, NA)
      xc.stat <- NA
      yc.stat <- NA
      xs.stat <- NA
      ys.stat <- NA
    }
  }
  if (iter < 10) {
    xc.sum <- yc.sum <- xc.stat <- yc.stat <- cor.m.adj <- sim.m.adj <- "NA"
    pf.output <- array(dim = c(1, 1, 2))
  }
  if (iter2 < 10)
    SSCOR <- SSSIM <- xs.stat <- ys.stat <- ss.cor.rep <- ss.sim.rep <- "NA"
  out1 <- list(x = fid.sam[,1], y = fid.sam[,2], xc = xc.sum,
               yc = yc.sum, xs = SSCOR, ys = SSSIM, x.stats = raw.x,
               y.stats = raw.y, xc.stats = xc.stat, yc.stats = yc.stat,
               xs.stats = xs.stat, ys.stats = ys.stat, x.pf.dist = pf.output[, , 1],
               y.pf.dist = pf.output[, , 2], xc.dist = cor.m.adj,
               yc.dist = sim.m.adj, xs.dist = ss.cor.rep, ys.dist = ss.sim.rep,
               live = live, dead = dead, gp = gp, values = list(measures = c(cor.measure, sim.measure),
                                                                data.transf = tfsd, remove.double.zeros = rm.zero,
                                                                PFiter = iter, SSIter = iter2, min.sam = min.sam))
  class(out1) <- append(class(out1), "FidelityEst")
  return(out1)
}
