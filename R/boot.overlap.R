boot.overlap <- function (x, B = 1000, pairsOverlap = TRUE, ...)
{
  
  out <- overlap(x, ...)
  
  if (pairsOverlap) {
    out <- out$OVPairs
    outb <- t(sapply(1:B, function(b) {
      xb <- lapply(x, FUN = sample, replace = TRUE)
      out2 <- overlap(xb, ...)$OVPairs
    }))
  } else {
    out <- out$OV
    outb <- t(sapply(1:B, function(b) {
      xb <- lapply(x, FUN = sample, replace = TRUE)
      out2 <- overlap(xb, ...)$OV
    }))
  }
  
  if (nrow(outb) > 1) {
    bias <- apply(outb, 2, mean) - out
    se <- apply(outb, 2, sd)
  } else {
    bias <- mean(outb) - out
    se <- sd(outb)
  }
  OVboot <- data.frame(estOV = out, bias = bias, se = se)
  return(list(OVboot_stats = OVboot, OVboot_dist = outb))
}