hei <- function(tf){

  # sums and names#####
  sum.in.asa <- c("F_TOTAL",
                  "G_WHOLE",
                  "D_TOTAL")
  intakes <- c("SODI",
               "G_REFINED",
               "SFAT",
               "ADD_SUGARS")

  to.numeric <- c(sum.in.asa,
                  "KCAL",
                  intakes,
                  "F_CITMLB",
                  "F_OTHER",
                  "MFAT",
                  "PFAT",
                  "V_TOTAL",
                  "V_LEGUMES",
                  "V_DRKGR",
                  "PF_MPS_TOTAL",
                  "PF_EGGS",
                  "PF_NUTSDS",
                  "PF_SOY",
                  "PF_LEGUMES",
                  "PF_SEAFD_HI",
                  "PF_SEAFD_LOW")

  tf[, to.numeric] <- sapply(tf[, to.numeric],
                             as.numeric)

  tf$FWHOLEFRT <- with(tf, F_CITMLB+
                         F_OTHER)
  tf$MONOPOLY <- with(tf, MFAT+
                        PFAT)
  tf$VTOTALLEG <- with(tf, V_TOTAL+
                         V_LEGUMES)
  tf$VDRKGRLEG <- with(tf, V_DRKGR+
                         V_LEGUMES)
  tf$PFALLPROTLEG <- with(tf,
                          PF_MPS_TOTAL+
                            PF_EGGS+
                            PF_NUTSDS+
                            PF_SOY+
                            PF_LEGUMES)
  tf$PFSEAPLANTLEG <- with(tf,
                           PF_SEAFD_HI+
                             PF_SEAFD_LOW+
                             PF_NUTSDS+
                             PF_SOY+
                             PF_LEGUMES)

  # components 1 to 8####
  tnames <- c("VTOTALLEG",
              "VDRKGRLEG",
              "F_TOTAL",
              "FWHOLEFRT",
              "G_WHOLE",
              "D_TOTAL",
              "PFALLPROTLEG",
              "PFSEAPLANTLEG")
  dnames <- c("VEGDEN",
              "GRBNDEN",
              "FRTDEN",
              "WHFRDEN",
              "WGRNDEN",
              "DAIRYDEN",
              "PROTDEN",
              "SEAPLDEN")
  cnames1to8 <- c("C1_TOTALVEG",
                  "C2_GREEN_AND_BEAN",
                  "C3_TOTALFRUIT",
                  "C4_WHOLEFRUIT",
                  "C5_WHOLEGRAIN",
                  "C6_TOTALDAIRY",
                  "C7_TOTPROT",
                  "C8_SEAPLANT_PROT")

  # loop
  dfac <- c(1.1,
            0.2,
            0.8,
            0.4,
            1.5,
            1.3,
            2.5,
            0.8)

  for (i in seq(dnames)){
    kcal <- tf$KCAL
    den <- tf[, tnames[i]]/(kcal/1000)
    mfac <- ifelse(dnames[i] %in% c("WGRNDEN", "DAIRYDEN"),
                   10,
                   5)
    c <- mfac*(den/dfac[i])
    c <- ifelse(c > mfac,
                mfac,
                ifelse(den %in% 0,
                       0,
                       c))
    tf[, paste0("HEI2015", cnames1to8[i])] <- c
    tf[, dnames[i]] <- den
  }

  # component 9####
  FARATIO <-  tf$MONOPOLY/tf$SFAT
  FARMIN <- 1.2
  FARMAX <- 2.5
  c9 <- paste0("HEI2015",
               "C9_FATTYACID")
  tf[, c9] <- with(tf,
                   ifelse(SFAT == 0 & MONOPOLY == 0, 0,
                          ifelse(SFAT == 0 & MONOPOLY > 0, 10,
                                 ifelse(FARATIO >= FARMAX, 10,
                                        ifelse(FARATIO <= FARMIN, 0,
                                               10*((FARATIO - FARMIN)/(FARMAX - FARMIN)))))))

  # components 10 to 13####
  mins <- c(1.1,
            1.8,
            8,
            6.5)

  maxs <- c(2.0,
            4.3,
            16,
            26)

  cnames10to13 <- c("C10_SODIUM",
                    "C11_REFINEDGRAIN",
                    "C12_SFAT",
                    "C13_ADDSUG")

  for (i in seq(intakes)){
    intake <- intakes[i]
    percentify <- ifelse(intake %in% c("SODI", "G_REFINED"),
                         1,
                         100)
    k.units <- ifelse(intake %in% c("G_REFINED"),
                      1000,
                      1)
    mfac <- ifelse(intake %in% c("SFAT"), 9,
                   ifelse(intake %in% c("ADD_SUGARS"), 16,
                          1))

    den <- percentify*mfac*tf[, intakes[i]]/(kcal/k.units)
    tf[, paste0("HEI2015", cnames10to13[i])] <- ifelse(den <= mins[i], 10,
                                                       ifelse(den >= maxs[i], 0,
                                                              10 - (10*(den - mins[i])/(maxs[i] - mins[i]))))
  }

  components <- c(paste0("HEI2015",
                         c(cnames1to8,
                           "C9_FATTYACID",
                           cnames10to13)))
  tf[tf$KCAL %in% 0, components] <- 0
  tf$HEI2015_TOTAL_SCORE <- rowSums(tf[, components])

  tf
}

file.exists("~/.ssh/id_rsa.pub")
