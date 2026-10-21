if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(ggplot2)

out_dir <- "Path to File"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

V_work <- 5.0                                   # m3 working volume
fill   <- 0.75                                  # working / gross volume
V_tot  <- V_work / fill                         # m3 gross
HD     <- 2.5                                   # straight height / diameter
D      <- (4 * V_tot / (pi * HD))^(1/3)         # tank diameter, m (cylinder approximation)
H_str  <- HD * D                                # straight height, m
A_x    <- pi * D^2 / 4                          # cross-section, m2
H_liq  <- V_work / A_x                          # liquid height, m
Di     <- 0.40 * D                              # impeller diameter, m (Di/D = 0.40)
N_rpm  <- 120; N <- N_rpm / 60                  # agitation, rpm and rps
rho    <- 1030                                  # saline broth density, kg/m3
Po_R   <- 5.0; Po_P <- 1.5                      # power numbers: Rushton, pitched blade
P_ung  <- (Po_R + 2 * Po_P) * rho * N^3 * Di^5  # un-gassed power, W (1 Rushton + 2 PBT)
P_gas  <- 0.6 * P_ung                           # gassed power, W (Pg/P0 about 0.6)
tip    <- pi * Di * N                           # impeller tip speed, m/s
vvm    <- 0.5                                   # aeration, vol/vol/min
v_s    <- (vvm * V_work / 60) / A_x             # superficial gas velocity, m/s
kLa_h  <- 0.002 * (P_gas / V_work)^0.7 * v_s^0.2 * 3600       # 1/h, van't Riet (ionic medium)
p_abs  <- 1.013 + 0.5 + rho * 9.81 * (H_liq / 2) / 1e5       # mean absolute pressure, bar
Cstar  <- 7.0 * (p_abs / 1.013) * 0.8           # mean O2 saturation driving level, mg/L
DO_sp  <- 0.30 * 7.0                            # DO set-point (30 % of air saturation), mg/L
OTR    <- kLa_h * (Cstar - DO_sp) / 32          # max O2 transfer rate, mmol O2 / L / h
Q_met  <- OTR / 1000 * V_work * 1000 * 460 / 3600            # metabolic heat, kW (460 kJ per mol O2)
Q_tot  <- Q_met + P_gas / 1000                  # total peak cooling duty, kW
Q_jack <- pi * D * H_liq * 450 * 12 / 1000      # jacket capacity, kW (U 450 W/m2K, LMTD 12 K)
std    <- c(5.5, 7.5, 11, 15, 18.5, 22, 30)     # standard motor sizes, kW
motor  <- std[which(std >= P_ung / 1000 / 0.85 * 1.25)[1]]

# =============================================================================
# 2. HELPERS
# =============================================================================
fig_w <- 13.33                                  # slide width in inches (16:9 slide)
cw  <- function(sz, upi, bold = FALSE) sz * 0.0394 * 0.5 * upi * ifelse(bold, 1.12, 1)   # character width (units)
lh  <- function(sz, upi) sz * 0.0394 * 1.2 * upi                                         # line height (units)
wrapt  <- function(s, chars) paste(strwrap(s, max(6, floor(chars))), collapse = "\n")
nl     <- function(s) lengths(strsplit(s, "\n"))
tab <- function(cols, ...) {
  r <- list(...); bad <- which(lengths(r) != length(cols))
  if (length(bad)) stop("Row(s) ", paste(bad, collapse = ","), " have the wrong number of fields")
  df <- as.data.frame(do.call(rbind, r), stringsAsFactors = FALSE); names(df) <- cols; df
}
rrect <- function(x1, y1, x2, y2, r = 2.2) {
  th <- seq(0, pi / 2, length.out = 12)
  rbind(data.frame(x = x2 - r + r * cos(th),              y = y2 - r + r * sin(th)),
        data.frame(x = x1 + r + r * cos(th + pi / 2),     y = y2 - r + r * sin(th + pi / 2)),
        data.frame(x = x1 + r + r * cos(th + pi),         y = y1 + r + r * sin(th + pi)),
        data.frame(x = x2 - r + r * cos(th + 3 * pi / 2), y = y1 + r + r * sin(th + 3 * pi / 2)))
}
save_slide <- function(L, W, H, name) {
  p <- Reduce(`+`, L, ggplot()) + coord_fixed(xlim = c(0, W), ylim = c(0, H), expand = FALSE) + theme_void() +
    theme(plot.background = element_rect(fill = "white", colour = NA), plot.margin = margin(3, 3, 3, 3))
  ggsave(file.path(out_dir, paste0(name, ".png")), p, width = fig_w, height = fig_w * H / W, dpi = 200, bg = "white")
  print(p); message("Saved: ", name, ".png"); invisible(p)
}

design_rows <- data.frame(
  label = c("Volume", "Geometry", "Impellers", "Baffles and sparger", "Material", "Agitation and tip speed",
            "Power input", "Oxygen transfer (kLa)", "Oxygen transfer rate", "Cooling duty"),
  value = c(sprintf("%.1f m\u00B3 gross | %.1f m\u00B3 working", V_tot, V_work),
            sprintf("H/D = %.1f (D %.2f m, H %.2f m)", HD, D, H_str),
            sprintf("1 Rushton + 2 pitched-blade (Di %.2f m)", Di),
            "4 baffles + ring sparger",
            "SS316L, electropolished",
            sprintf("%d rpm; tip speed %.1f m/s", N_rpm, tip),
            sprintf("%.1f kW gassed (%.2f kW/m\u00B3); motor %.0f kW", P_gas / 1000, P_gas / 1000 / V_work, motor),
            sprintf("~%.0f 1/h at %.1f vvm", kLa_h, vvm),
            sprintf("~%.0f mmol O2/L/h (max)", OTR),
            sprintf("~%.0f kW peak; jacket ~%.0f kW", Q_tot, Q_jack)),
  why   = c("Fed-batch scale; 75 % fill leaves foam headspace",
            "Tall vessel: more hydrostatic head, better O2 transfer",
            "Rushton disperses gas; upper blades mix at lower shear",
            "Baffles stop vortexing; sparger aerates evenly",
            "Resists saline medium; supports SIP and CIP",
            "Tip speed kept below 4 m/s to protect mycelium",
            "Motor sized above un-gassed demand, with safety margin",
            "Ionic-media correlation; verify by gassing-out",
            "Meets dense-culture O2 demand at DO 30 %",
            "Jacket exceeds peak load; add internal coil if short"),
  stringsAsFactors = FALSE)

control_rows <- data.frame(
  label = c("Temperature", "pH", "Dissolved oxygen", "Headspace pressure", "Foam", "Off-gas analysis",
            "Aseptic safeguards", "Weight", "Key", "Drawing"),
  value = c("TIC-101 -> jacket valve TV-101",
            "AIC-102 doses acid (P-102) or base (P-103)",
            "AIC-103: agitation (SIC-106) -> airflow (FIC-101) -> O2",
            "PIC-104 -> back-pressure valve PV-104",
            "LIC-105 pulses antifoam pump P-106",
            "AT-107 measures O2 and CO2 in exhaust",
            "Sterile air filters, vent condenser + filter, sterile sampling port",
            "WIT-108 load cells",
            "OUR / CER = O2 uptake / CO2 evolution rate",
            "TBN-PID-101 (P&ID of PF-101)"),
  why   = c("Hold about 28 \u00B0C; valve switches cooling water / steam",
            "Hold near pH 7.0",
            "Keep DO at or above 30 %; last step is O2 enrichment",
            "Hold 0.5 barg",
            "Prevent foam-over",
            "Gives OUR, CER and RQ to guide feeding",
            "Prevent contamination and release of the strain",
            "Track volume and feed additions",
            "RQ = respiratory quotient",
            "Loop tags refer to this drawing"),
  stringsAsFactors = FALSE)

slide_design_control <- function() {
  W <- 200; upi <- W / fig_w
  sz <- list(title = 9.5, sub = 4.6, head = 4.7, lab = 4.3, val = 4.2, why = 3.4, foot = 3.4)
  px0 <- c(3, 102); pw <- 95; lab_w <- 29; txt_w <- pw - lab_w - 5
  cols <- c("#3B9C94", "#E39B4C"); navy <- "#1F3A5F"
  n <- nrow(design_rows)
  wv <- function(v) vapply(v, wrapt, "", chars = txt_w / cw(sz$val, upi, TRUE), USE.NAMES = FALSE)
  ww <- function(v) vapply(v, wrapt, "", chars = txt_w / cw(sz$why, upi), USE.NAMES = FALSE)
  wl <- function(v) vapply(v, wrapt, "", chars = (lab_w - 3) / cw(sz$lab, upi, TRUE), USE.NAMES = FALSE)
  vL <- wv(design_rows$value);  vR <- wv(control_rows$value)
  wL <- ww(design_rows$why);    wR <- ww(control_rows$why)
  lL <- wl(design_rows$label);  lR <- wl(control_rows$label)
  need <- pmax(nl(vL) * lh(sz$val, upi) + nl(wL) * lh(sz$why, upi),
               nl(vR) * lh(sz$val, upi) + nl(wR) * lh(sz$why, upi),
               nl(lL) * lh(sz$lab, upi), nl(lR) * lh(sz$lab, upi)) + 3.4
  row_h <- pmax(need, 7.5)
  H <- max(112.5, 17 + 9 + sum(row_h) + 9)
  y_head <- H - 17; y_rows_top <- y_head - 9
  ymax <- y_rows_top - c(0, cumsum(row_h))[seq_len(n)]; ymin <- ymax - row_h
  y_bot <- min(ymin)
  L <- list(); add <- function(z) L[[length(L) + 1]] <<- z
  add(annotate("text", x = 3, y = H - 6.5,  label = "PF-101 Production Fermenter: Design and Control", hjust = 0, size = sz$title, fontface = "bold", colour = navy))
  add(annotate("text", x = 3, y = H - 13.5, label = "5 m\u00B3 fed-batch | Drawing TBN-PID-101", hjust = 0, size = sz$sub, colour = "#6B7785"))
  heads <- list(c("Design", "Fermenter design: value and why"), c("Loop", "Control strategy: action and purpose"))
  tabs  <- list(list(l = lL, v = vL, w = wL), list(l = lR, v = vR, w = wR))
  for (k in 1:2) {
    x0 <- px0[k]; t <- tabs[[k]]
    add(annotate("text", x = x0 + 1, y = y_head - 4, label = heads[[k]][1], hjust = 0, size = sz$head, fontface = "bold", colour = navy))
    add(annotate("text", x = x0 + lab_w + 3, y = y_head - 4, label = heads[[k]][2], hjust = 0, size = sz$head, fontface = "bold", colour = navy))
    add(annotate("segment", x = x0, xend = x0 + pw, y = y_head - 8, yend = y_head - 8, linewidth = 1.5, colour = cols[k]))
    add(annotate("segment", x = x0, xend = x0, y = y_head - 8, yend = y_bot, linewidth = 1.5, colour = cols[k]))
    for (i in seq_len(n)) {
      if (i < n) add(annotate("segment", x = x0 + 1, xend = x0 + pw, y = ymin[i], yend = ymin[i], linewidth = 0.25, colour = "#D5DBE1"))
      add(annotate("text", x = x0 + 2.5, y = (ymax[i] + ymin[i]) / 2, label = t$l[i], hjust = 0, vjust = 0.5, size = sz$lab, fontface = "bold", colour = navy, lineheight = 0.95))
      add(annotate("text", x = x0 + lab_w + 3, y = ymax[i] - 2.0, label = t$v[i], hjust = 0, vjust = 1, size = sz$val, fontface = "bold", colour = "#1F2D3D", lineheight = 0.95))
      add(annotate("text", x = x0 + lab_w + 3, y = ymax[i] - 2.0 - nl(t$v[i]) * lh(sz$val, upi) - 0.3, label = t$w[i], hjust = 0, vjust = 1, size = sz$why, colour = "#6B7785", lineheight = 0.95))
    }
  }
  add(annotate("segment", x = 3, xend = W - 3, y = y_bot, yend = y_bot, linewidth = 0.8, colour = navy))
  add(annotate("text", x = 3, y = y_bot - 4, label = "Preliminary design calculation; to be confirmed by kLa mapping and scale-down runs.",
               hjust = 0, size = sz$foot, fontface = "italic", colour = "#6B7785"))
  save_slide(L, W, H, "Slide1_PF101_Design_and_Control")
}

deg <- "\u00B0C"; m3 <- "m\u00B3"; um <- "\u00B5m"
cmp <- tab(c("row", "group", "sf_val", "sf_txt", "pf_val", "pf_txt"),
           c("ROLE", "equip", "400 L inoculum",
             "Final seed stage: 40 L from SF-101 (10 % v/v) grown 48 h, then charged to PF-101",
             paste0("5 ", m3, " fed-batch"),
             "Controlled fed-batch turbinmicin production: batch growth, idiophase trigger, fed-batch, harvest"),
           c("VESSEL", "equip", "600 L | 400 L",
             paste0("Gross | working. SS316L, EP Ra <= 0.4 ", um, "; 3 barg + FV; 4 baffles; jacketed; SIP/CIP"),
             paste0("6.7 | 5.0 ", m3),
             paste0("Gross | working. SS316L, EP Ra <= 0.4 ", um, "; 3 barg + FV; 4 baffles; jacketed; SIP/CIP")),
           c("AGITATION", "equip", "2 x Rushton",
             "M-101 / VFD drive; speed set by SIC-106 in the DO cascade; rpm not specified",
             "80-150 rpm",
             "1 Rushton (RDT) + 2 PBT; M-101 / VFD drive; SIC-106 raises speed in the DO cascade (range = batch phase)"),
           c("AERATION", "equip", "Ring sparger",
             "Oil-free air 7 barg: F-101, PCV-101, FV-101 (MFC), F-102, NRV-101, XV-103; SIP steam via HV-101 / XV-102; rate not specified",
             "0.3-0.5 vvm",
             "Ring sparger on the same air train (F-101 ... XV-103); FIC-101 driven by the DO cascade; O2 enrichment; SIP steam via HV-101 / XV-102"),
           c("TEMPERATURE", "loop", paste0("28 ", deg),
             paste0("TIC-101 (TT-101) to TV-101: jacket split-range, CW supply 7 ", deg, " / plant steam"),
             paste0("28 -> 26-28 ", deg),
             paste0("28 ", deg, " batch, 26-28 ", deg, " production (OSMAC); same TIC-101 / TV-101 loop; cool to 10-15 ", deg, " at harvest")),
           c("pH", "loop", "7.0",
             "AIC-102 (AT-102) split-range to P-102 (1 M H2SO4, T-102) and P-103 (2 M NaOH, T-103)",
             "7.0 +/- 0.1",
             "AIC-102 (AT-102) split-range to P-102 (1 M H2SO4, T-102) and P-103 (2 M NaOH, T-103)"),
           c("DISSOLVED OXYGEN", "loop", ">= 30 %",
             "AIC-103 (AT-103) cascade: SIC-106 (agitation), FIC-101 (air), then O2 enrichment",
             ">= 30 %",
             "AIC-103 (AT-103) cascade: SIC-106, FIC-101, then O2 enrichment; DO-stat also guides feeding in production"),
           c("PRESSURE & OFF-GAS", "loop", "PIC-104",
             paste0("PT-104 to PV-104 back-pressure; PSV-110 relief; E-102 condenser; F-103 0.2 ", um, " filter; vent scrubber; set-point not specified"),
             "0.5 barg",
             paste0("PIC-104 to PV-104; PSV-110; E-102; F-103 (0.2 ", um, "); scrubber; AT-107 O2 / CO2 analyser feeds off-gas MS (OUR, CER, RQ)")),
           c("FOAM CONTROL", "loop", "Antifoam",
             "LIC-105 to P-106 dosing from T-106 (via XV-115); antifoam charged with the sterile medium",
             "Antifoam",
             "LIC-105 to P-106 dosing from T-106 (via XV-115)"),
           c("FEEDS", "proc", "None (batch)",
             "Sterile medium + antifoam only; no feed lines on the SF-102 drawing",
             "2 feeds",
             "Feed-1 carbon (T-104, P-104) and Feed-2 N + trace (T-105, P-105) on load cell WIT-108; Feed-1 starts at glucose < 2 g/L + OUR plateau; residual C 1-5 g/L; N-limitation; Co2+ / precursor pulses"),
           c("MONITORING & SAMPLING", "proc", "Pre-transfer",
             "SV-109 aseptic sample; microscopy + purity check before transfer; WIT-108 load cell",
             "Every 12 h",
             "SV-109 to HPLC / LC-MS, microscopy, PCV / DCW, glucose, NH4+, titre, bioactivity, contamination plating; capacitance probe; off-gas MS"),
           c("PHASES & TIMING", "proc", "48 h",
             paste0("Sterile medium + antifoam charged, inoculate 40 L from SF-101 (10 %), grow at the same set-points (28 ", deg, ", pH 7.0, DO >= 30 %)"),
             "192-216 h",
             paste0("SIP 121 ", deg, " >= 30 min, medium 121 ", deg, " 30 min, cool to 28 ", deg, "; inoculate 400 L (8 % v/v) into 4.6 ", m3,
                    "; batch 0-48 h; idiophase trigger 48-72 h; fed-batch 72-192 h; harvest decision 192-216 h")),
           c("TRANSFER / HARVEST", "proc", "<= 30 min",
             "400 L seed to PF-101 via steam-sterilisable line at 0.5 barg overpressure (8 % v/v of PF-101); P&ID outlet reads 'to PF-101'",
             "<= 3 h",
             "Harvest if titre gain < 5 % per 12 h or DO spike (lysis); P-107 lobe pump (HV-106, XV-117) to HT-201; residuals to kill tank KT-701; CIP skid CP-501")
)

slide_comparison <- function() {
  W <- 250; upi <- W / fig_w
  sz <- list(txt = 2.9, val = 3.2, lab = 2.9, ptitle = 5.0, psub = 2.9, head = 2.6, title = 8.0, sub = 3.6, foot = 2.5)
  grp_col <- c(equip = "#5B6770", loop = "#D98E2B", proc = "#2E8B57")
  lab_x0 <- 2; lab_w <- 24; sf0 <- 28; sf_w <- 110; pf0 <- 141; pf_w <- 107; val_w <- 30
  n <- nrow(cmp)
  wval <- function(v) vapply(v, wrapt, "", chars = (val_w - 3) / cw(sz$val, upi, TRUE), USE.NAMES = FALSE)
  wtxt <- function(v, w) vapply(v, wrapt, "", chars = (w - val_w - 4) / cw(sz$txt, upi), USE.NAMES = FALSE)
  wlab <- vapply(cmp$row, wrapt, "", chars = (lab_w - 4) / cw(sz$lab, upi, TRUE), USE.NAMES = FALSE)
  sfv <- wval(cmp$sf_val); pfv <- wval(cmp$pf_val)
  sft <- wtxt(cmp$sf_txt, sf_w); pft <- wtxt(cmp$pf_txt, pf_w)
  need <- pmax(nl(sfv) * lh(sz$val, upi), nl(pfv) * lh(sz$val, upi), nl(sft) * lh(sz$txt, upi), nl(pft) * lh(sz$txt, upi), nl(wlab) * lh(sz$lab, upi)) + 3.2
  row_h <- pmax(need, 8); gap <- 1.2
  R <- sum(row_h) + (n - 1) * gap
  H <- 15 + 4 + 14 + 6 + R + 14
  y_rows_top <- H - 39
  ymax <- y_rows_top - c(0, cumsum(row_h + gap))[seq_len(n)]; ymin <- ymax - row_h
  L <- list(); add <- function(z) L[[length(L) + 1]] <<- z
  # title bar
  add(annotate("rect", xmin = 0, xmax = W, ymin = H - 15, ymax = H, fill = "#0F3B5F"))
  add(annotate("rect", xmin = 0, xmax = W, ymin = H - 15.7, ymax = H - 15, fill = "#C98A2B"))
  add(annotate("text", x = 3, y = H - 6.2, label = "Seed vs Production Fermenter: Ancillaries, Processes & Conditions", hjust = 0, size = sz$title, fontface = "bold", colour = "white"))
  add(annotate("text", x = 3, y = H - 11.6, label = "Turbinmicin fed-batch process | Micromonospora sp. WMMC-415 | Drawings TBN-PID-102 (SF-102) and TBN-PID-101 (PF-101), Rev A",
               hjust = 0, size = sz$sub, colour = "#DCE6F0"))
  # panel headers
  hy0 <- H - 33; hy1 <- H - 19
  pill <- rrect(lab_x0, hy0, lab_x0 + lab_w, hy1, r = 2.5)
  add(annotate("polygon", x = pill$x, y = pill$y, fill = "#2B3A47", colour = NA))
  add(annotate("text", x = lab_x0 + lab_w / 2, y = (hy0 + hy1) / 2, label = "PARAMETER", size = 3.8, fontface = "bold", colour = "white"))
  hdr <- list(list(x0 = sf0, w = sf_w, col = "#2F8FD0", t = "SEED FERMENTER  SF-102", r = "400 L seed -> PF-101",
                   s = paste0("600 L gross | 400 L working | 48 h | 28 ", deg)),
              list(x0 = pf0, w = pf_w, col = "#0F3B5F", t = "PRODUCTION FERMENTER  PF-101", r = paste0("-> 5 ", m3, " broth to HT-201"),
                   s = paste0("6.7 ", m3, " gross | 5.0 ", m3, " working | 192-216 h | fed-batch")))
  for (h in hdr) {
    b <- rrect(h$x0, hy0, h$x0 + h$w, hy1, r = 2.5)
    add(annotate("polygon", x = b$x, y = b$y, fill = h$col, colour = NA))
    add(annotate("text", x = h$x0 + 3, y = hy1 - 4.3, label = h$t, hjust = 0, size = sz$ptitle, fontface = "bold", colour = "white"))
    add(annotate("text", x = h$x0 + h$w - 3, y = hy1 - 4.3, label = h$r, hjust = 1, size = 3.0, fontface = "bold", colour = "white"))
    add(annotate("text", x = h$x0 + 3, y = hy0 + 3.4, label = h$s, hjust = 0, size = sz$psub, colour = "#E6F0FA"))
    add(annotate("text", x = h$x0 + 2, y = hy0 - 2.2, label = "VALUE", hjust = 0, size = sz$head, fontface = "bold", colour = "#222222"))
    add(annotate("text", x = h$x0 + val_w + 2, y = hy0 - 2.2, label = "ANCILLARIES, LOOPS, CONDITIONS", hjust = 0, size = sz$head, fontface = "bold", colour = "#222222"))
  }
  # rows
  for (i in seq_len(n)) {
    yc <- (ymax[i] + ymin[i]) / 2
    add(annotate("rect", xmin = lab_x0, xmax = lab_x0 + 1.4, ymin = ymin[i], ymax = ymax[i], fill = grp_col[[cmp$group[i]]]))
    add(annotate("rect", xmin = lab_x0 + 1.4, xmax = lab_x0 + lab_w, ymin = ymin[i], ymax = ymax[i], fill = "#E6EBF0"))
    add(annotate("text", x = lab_x0 + 3.4, y = yc, label = wlab[i], hjust = 0, size = sz$lab, fontface = "bold", colour = "#2B3A47", lineheight = 0.95))
    for (k in 1:2) {
      x0 <- c(sf0, pf0)[k]; w <- c(sf_w, pf_w)[k]
      v <- list(sfv, pfv)[[k]][i]; tx <- list(sft, pft)[[k]][i]
      add(annotate("rect", xmin = x0, xmax = x0 + w, ymin = ymin[i], ymax = ymax[i], fill = "#EAF0F6"))
      add(annotate("text", x = x0 + 2, y = yc, label = v, hjust = 0, size = sz$val, fontface = "bold", colour = "#111111", lineheight = 0.95))
      add(annotate("text", x = x0 + val_w + 2, y = yc, label = tx, hjust = 0, size = sz$txt, colour = "#222222", lineheight = 0.95))
    }
  }
  y_bot <- min(ymin)
  add(annotate("text", x = 2, y = y_bot - 4.0, hjust = 0, size = sz$foot, colour = "#555555",
               label = "Row bars: grey = equipment, amber = control loops, green = process. n/s = not specified in the supplied drawings. PBT = pitched-blade turbine; RDT = Rushton disc turbine; FV = full vacuum; MFC = mass-flow controller;"))
  add(annotate("text", x = 2, y = y_bot - 7.2, hjust = 0, size = sz$foot, colour = "#555555",
               label = "OUR / CER = O2 uptake rate / CO2 evolution rate; RQ = respiratory quotient; CW = cooling water. The SF-102 P&ID (Slide 3) is drawn to its own specification: 400 L, 2 x Rushton, no feed lines, outlet to PF-101."))
  save_slide(L, W, H, "Slide2_Seed_vs_Production_Conditions")
}

arc_pts <- function(cx, cy, a, b, t1, t2, n = 80) {
  t <- seq(t1, t2, length.out = n); data.frame(x = cx + a * cos(t), y = cy + b * sin(t))
}
circle_df <- function(x, y, r, id) { d <- arc_pts(x, y, r, r, 0, 2 * pi, 40); d$id <- id; d }

build_pid <- function(tag = "PF-101", title = "PRODUCTION FERMENTER",
                      spec1 = "6.7 m3 gross | 5.0 m3 working",
                      spec2 = "SS316L, EP Ra <= 0.4 um | 3 barg + FV",
                      spec3 = "1 Rushton + 2 PBT | 4 baffles | jacketed | SIP/CIP",
                      drawing = "TBN-PID-101 Rev A",
                      vol_label = "5 m3 working volume",
                      n_imp = 3, imp_labels = c("RDT", "PBT", "PBT"),     # bottom to top
                      show_feeds = TRUE, show_pump = TRUE,
                      outlet_label = "to harvest tank HT-201") {
  
  # --- vessel geometry
  bottom <- arc_pts(50, 12, 12, 3,   pi, 2 * pi)
  top    <- arc_pts(50, 54, 12, 3.5, 0,  pi)
  vessel <- cbind(rbind(bottom, top), g = 1)
  liquid <- cbind(rbind(bottom, data.frame(x = c(62, 38), y = c(47, 47))), g = 1)
  jacket <- data.frame(x = c(34.5, 65.5, 65.5, 34.5), y = c(14, 14, 41, 41), g = 1)
  sparg  <- arc_pts(50, 14.5, 5, 0.7, 0, 2 * pi)
  struct_seg <- data.frame(x = c(50, 40.2, 59.8, 60), y = c(15, 13.5, 13.5, 10.1),
                           xend = c(50, 40.2, 59.8, 60), yend = c(62, 46, 46, 6.5))
  imp_y <- if (n_imp == 3) c(19, 29, 39) else c(23, 35)                  # impeller centres
  imp   <- data.frame(xmin = 45.5, xmax = 54.5, ymin = imp_y - 0.6, ymax = imp_y + 0.6)
  rects <- data.frame(
    xmin = c(45, 48.5, 6.5, 24.5, 68.4, 78.4, 1, 1, 1, 1, 91, 58.5),
    xmax = c(55, 51.5, 9.5, 27.5, 71.6, 81.6, 9, 9, 9, 9, 99, 61.5),
    ymin = c(62, 57.5, 6.5, 6.4, 62.4, 62.4, 51.8, 48.8, 45.8, 42.8, 47.8, 6.0),
    ymax = c(67, 62, 9.5, 9.6, 65.6, 65.6, 54.2, 51.2, 48.2, 45.2, 50.2, 6.6),
    fill = c("#CFD8DC", "#B0BEC5", "#E8F5E9", "#E8F5E9", "#E3F2FD", "#FFF3E0",
             "#F5F5F5", "#F5F5F5", "#F5F5F5", "#F5F5F5", "#F5F5F5", "#455A64"))
  
  # --- lines
  P <- function(x, y, xend, yend, service, arrow = FALSE)
    data.frame(x = x, y = y, xend = xend, yend = yend, service = service, arrow = arrow, stringsAsFactors = FALSE)
  pipes <- rbind(
    P(3, 8, 46, 8, "Sterile air"),            P(46, 8, 46, 14, "Sterile air", TRUE),
    P(3, 4, 36, 4, "Steam (SIP)"),            P(36, 4, 36, 8, "Steam (SIP)", TRUE),
    P(54, 9.2, 54, 2, "Process liquid"),      P(54, 2, 96, 2, "Process liquid", TRUE),
    P(96, 16, 65.5, 16, "Cooling water", TRUE), P(65.5, 39, 96, 39, "Cooling water", TRUE),
    P(9, 53, 38, 53, "Process liquid", TRUE), P(9, 50, 38, 50, "Process liquid", TRUE),
    P(9, 47, 38, 47, "Process liquid", TRUE), P(9, 44, 38, 44, "Process liquid", TRUE),
    P(91, 49, 62, 49, "Process liquid", TRUE),
    P(57, 56.9, 57, 64, "Off-gas"),           P(57, 64, 96, 64, "Off-gas", TRUE),
    P(74, 64, 74, 58, "Off-gas", TRUE),
    P(62, 22, 68, 22, "Process liquid"),
    P(38, 37, 29.5, 37, "Impulse line"), P(38, 33, 29.5, 33, "Impulse line"),
    P(62, 30, 70.5, 30, "Impulse line"), P(62, 53, 71.5, 53, "Impulse line"),
    P(60, 56, 60, 60, "Impulse line"),   P(60, 60, 61, 60, "Impulse line"),
    P(60, 8, 67, 8, "Impulse line"),
    P(73.5, 30, 84, 30, "Instrument signal"), P(84, 30, 84, 27.5, "Instrument signal"),
    P(84, 24.5, 84, 18.6, "Instrument signal"),
    P(26.5, 37, 20.5, 37, "Instrument signal"),
    P(19, 38.5, 19, 55, "Instrument signal"), P(19, 55, 13, 55, "Instrument signal"), P(13, 55, 13, 54.2, "Instrument signal"),
    P(19, 51.8, 13, 51.8, "Instrument signal"), P(13, 51.8, 13, 51.2, "Instrument signal"),
    P(26.5, 33, 11.5, 33, "Instrument signal"),
    P(10, 31.5, 10, 14, "Instrument signal"), P(10, 14, 18.5, 14, "Instrument signal"),
    P(20, 12.5, 20, 10.6, "Instrument signal"),
    P(10, 34.5, 10, 61.5, "Instrument signal"), P(10, 61.5, 29.5, 61.5, "Instrument signal"),
    P(32.5, 61.5, 40, 61.5, "Instrument signal"), P(40, 61.5, 40, 64.5, "Instrument signal"),
    P(40, 64.5, 45, 64.5, "Instrument signal"),
    P(62.5, 61.5, 62.5, 70, "Instrument signal"), P(62.5, 70, 86.5, 70, "Instrument signal"),
    P(88, 68.5, 88, 66.6, "Instrument signal"),
    P(74.5, 53, 86, 53, "Instrument signal"), P(86, 53, 86, 50.3, "Instrument signal"))
  lv <- c("Sterile air", "Steam (SIP)", "Cooling water", "Process liquid", "Off-gas", "Impulse line", "Instrument signal")
  
  # --- valves
  V <- function(x, y, dir, ctrl = FALSE) data.frame(x = x, y = y, dir = dir, ctrl = ctrl, stringsAsFactors = FALSE)
  valves <- rbind(
    V(14, 4, "h"), V(36, 6, "v"), V(14, 8, "h"), V(20, 8, "h", TRUE), V(31, 8, "h"), V(41, 8, "h"),
    V(22, 53, "h"), V(22, 50, "h"), V(22, 47, "h"), V(22, 44, "h"),
    V(78, 49, "h"), V(84, 16, "h", TRUE), V(88, 64, "h", TRUE), V(74, 61, "v"),
    V(54, 5, "v"), V(82, 2, "h"), V(66, 22, "h"), V(42, 58.5, "v"))
  
  # --- pumps
  pumps <- rbind(data.frame(x = 13, y = 53, id = "p1"), data.frame(x = 13, y = 50, id = "p2"),
                 data.frame(x = 13, y = 47, id = "p3"), data.frame(x = 13, y = 44, id = "p4"),
                 data.frame(x = 86, y = 49, id = "p5"), data.frame(x = 72, y = 2,  id = "p6"))
  
  # --- text labels
  Lb <- function(x, y, label, hjust = 0.5, angle = 0)
    data.frame(x = x, y = y, label = label, hjust = hjust, angle = angle, stringsAsFactors = FALSE)
  lab <- rbind(
    Lb(1.5, 12.2, "Compressed air (oil-free, 7 barg)", 0), Lb(1.5, 5.4, "Plant steam (SIP, 3 barg)", 0),
    Lb(8, 10.7, "F-101"), Lb(14, 10.6, "PCV-101"), Lb(20, 6.0, "FV-101 (MFC)"), Lb(26, 10.7, "F-102"),
    Lb(31, 10.6, "NRV-101"), Lb(41, 10.6, "XV-103"), Lb(14, 2.7, "HV-101"), Lb(37.3, 6, "XV-102", 0),
    Lb(50, 44.5, vol_label), Lb(55.6, imp_y, imp_labels, 0),
    Lb(56, 15.6, "Ring sparger", 0), Lb(36.2, 27.5, "JACKET", 0.5, 90),
    Lb(5, 53, "T-102 Acid"), Lb(5, 50, "T-103 Base"), Lb(5, 47, "T-104 Feed-1"), Lb(5, 44, "T-105 Feed-2"),
    Lb(24, 54.25, "P-102  1 M H2SO4 (pH down)", 0), Lb(24, 51.25, "P-103  2 M NaOH (pH up)", 0),
    Lb(24, 48.25, "P-104  Feed-1 (carbon)", 0),     Lb(24, 45.25, "P-105  Feed-2 (N + trace)", 0),
    Lb(95, 49, "T-106 Antifoam"), Lb(86, 47.2, "P-106"), Lb(78, 47.4, "XV-115"),
    Lb(84, 13.9, "TV-101"), Lb(96, 17.3, "CW supply 7 C", 1), Lb(96, 40.3, "CW return", 1),
    Lb(69, 22, "SV-109 aseptic sample", 0), Lb(40.8, 58.5, "PSV-110", 1),
    Lb(70, 66.4, "E-102"), Lb(80, 66.4, "F-103"), Lb(91.2, 66, "PV-104", 0), Lb(96, 62.6, "to vent scrubber", 1),
    Lb(75.2, 61, "XV-116", 0), Lb(76.5, 56.5, "O2 / CO2 off-gas", 0),
    Lb(52.6, 5, "HV-106", 1), Lb(72, 0.1, "P-107"), Lb(82, 0.4, "XV-117"), Lb(96, 3.4, outlet_label, 1),
    Lb(50, 64.5, "M-101 / VFD"), Lb(64.5, 6.4, "load cell", 0))
  
  # --- optional items for the seed fermenter drawing
  if (!show_feeds) {                                   # SF-102 is a batch vessel: no Feed-1 / Feed-2 lines
    pipes  <- pipes[!(pipes$x == 9 & pipes$y %in% c(47, 44)), ]
    valves <- valves[!(valves$x == 22 & valves$y %in% c(47, 44)), ]
    rects  <- rects[-c(9, 10), ]
    pumps  <- pumps[!pumps$id %in% c("p3", "p4"), ]
    lab    <- lab[!lab$label %in% c("T-104 Feed-1", "T-105 Feed-2", "P-104  Feed-1 (carbon)", "P-105  Feed-2 (N + trace)"), ]
  }
  if (!show_pump) {                                    # seed transfer is by overpressure, not by pump
    pumps <- pumps[pumps$id != "p6", ]
    lab   <- lab[lab$label != "P-107", ]
  }
  pipes$service <- factor(pipes$service, levels = lv)
  
  # --- valve symbols and actuators
  vp <- do.call(rbind, lapply(seq_len(nrow(valves)), function(i) {
    x <- valves$x[i]; y <- valves$y[i]; s <- 0.9; a <- 0.6
    if (valves$dir[i] == "h") {
      t1 <- data.frame(x = c(x - s, x - s, x), y = c(y + a, y - a, y)); t2 <- data.frame(x = c(x + s, x + s, x), y = c(y + a, y - a, y))
    } else {
      t1 <- data.frame(x = c(x - a, x + a, x), y = c(y - s, y - s, y)); t2 <- data.frame(x = c(x - a, x + a, x), y = c(y + s, y + s, y))
    }
    t1$id <- paste0(i, "a"); t2$id <- paste0(i, "b"); rbind(t1, t2)
  }))
  cv <- valves[valves$ctrl, ]
  act <- do.call(rbind, lapply(seq_len(nrow(cv)), function(i) {
    th <- seq(0, pi, length.out = 30)
    data.frame(x = cv$x[i] + 0.9 * cos(th), y = cv$y[i] + 1.6 + 0.9 * sin(th), id = paste0("act", i))
  }))
  stems <- data.frame(x = cv$x, y = cv$y, xend = cv$x, yend = cv$y + 1.6)
  pump_poly <- do.call(rbind, lapply(seq_len(nrow(pumps)), function(i) circle_df(pumps$x[i], pumps$y[i], 1.25, pumps$id[i])))
  
  # --- instrument bubbles
  B <- function(x, y, top, bot) data.frame(x = x, y = y, top = top, bot = bot, stringsAsFactors = FALSE)
  bub <- rbind(B(72, 30, "TT", "101"), B(84, 26, "TIC", "101"), B(28, 37, "AT", "102"), B(19, 37, "AIC", "102"),
               B(28, 33, "AT", "103"), B(10, 33, "AIC", "103"), B(20, 14, "FIC", "101"), B(62.5, 60, "PT", "104"),
               B(88, 70, "PIC", "104"), B(73, 53, "LIC", "105"), B(31, 61.5, "SIC", "106"), B(74, 56.5, "AT", "107"),
               B(68.5, 8, "WIT", "108"))
  bub_poly <- do.call(rbind, lapply(seq_len(nrow(bub)), function(i) circle_df(bub$x[i], bub$y[i], 1.5, paste0("b", i))))
  
  cols <- c("Sterile air" = "#2E7D32", "Steam (SIP)" = "#C62828", "Cooling water" = "#1565C0", "Process liquid" = "#212121",
            "Off-gas" = "#EF6C00", "Impulse line" = "#757575", "Instrument signal" = "#7B1FA2")
  lt   <- c("Sterile air" = "solid", "Steam (SIP)" = "solid", "Cooling water" = "solid", "Process liquid" = "solid",
            "Off-gas" = "solid", "Impulse line" = "solid", "Instrument signal" = "dashed")
  
  ggplot() +
    geom_polygon(data = jacket, aes(x, y, group = g), fill = "#CFE8FB", colour = "#1565C0", linewidth = 0.5) +
    geom_polygon(data = vessel, aes(x, y, group = g), fill = "#F7F9FA", colour = "black", linewidth = 1.0) +
    geom_polygon(data = liquid, aes(x, y, group = g), fill = "#FBE7B5", alpha = 0.85, colour = NA) +
    geom_segment(data = struct_seg, aes(x = x, y = y, xend = xend, yend = yend), colour = "grey35", linewidth = 0.7) +
    geom_rect(data = imp, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "grey25", colour = "black") +
    geom_rect(data = rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), colour = "black", linewidth = 0.4, show.legend = FALSE) +
    scale_fill_identity() +
    geom_path(data = sparg, aes(x, y), colour = "black", linewidth = 0.8) +
    geom_segment(data = pipes, aes(x = x, y = y, xend = xend, yend = yend, colour = service, linetype = service), linewidth = 0.8) +
    geom_segment(data = pipes[pipes$arrow, ], aes(x = x, y = y, xend = xend, yend = yend, colour = service),
                 linewidth = 0.8, arrow = arrow(length = unit(2.2, "mm"), type = "closed"), show.legend = FALSE) +
    geom_segment(data = stems, aes(x = x, y = y, xend = xend, yend = yend), linewidth = 0.5) +
    geom_polygon(data = act,       aes(x, y, group = id), fill = "white",   colour = "black", linewidth = 0.4) +
    geom_polygon(data = vp,        aes(x, y, group = id), fill = "white",   colour = "black", linewidth = 0.4) +
    geom_polygon(data = pump_poly, aes(x, y, group = id), fill = "#FFF9C4", colour = "black", linewidth = 0.4) +
    geom_polygon(data = bub_poly,  aes(x, y, group = id), fill = "white",   colour = "black", linewidth = 0.5) +
    geom_segment(data = bub, aes(x = x - 1.5, xend = x + 1.5, y = y, yend = y), linewidth = 0.3) +
    geom_text(data = bub, aes(x, y + 0.5, label = top), size = 2.1, fontface = "bold") +
    geom_text(data = bub, aes(x, y - 0.6, label = bot), size = 2.1) +
    geom_text(data = pumps, aes(x, y, label = "P"), size = 2.0, fontface = "bold") +
    geom_text(data = lab, aes(x, y, label = label, hjust = hjust, angle = angle), size = 2.3) +
    annotate("text", x = 80, y = 46.3, label = paste(tag, title), fontface = "bold", size = 3.6) +
    annotate("text", x = 80, y = 44.7, label = spec1, size = 2.5) +
    annotate("text", x = 80, y = 43.3, label = spec2, size = 2.5) +
    annotate("text", x = 80, y = 41.9, label = spec3, size = 2.5) +
    scale_colour_manual(name = "Line service", values = cols, breaks = lv) +
    scale_linetype_manual(name = "Line service", values = lt, breaks = lv) +
    coord_fixed(ratio = 1, xlim = c(0, 100), ylim = c(-2, 76), expand = FALSE) +
    labs(title = paste0("P&ID  |  ", tag, "  ", title),
         subtitle = paste0("Turbinmicin fed-batch process, Micromonospora sp. WMMC-415   |   Drawing ", drawing),
         caption = paste0(
           "TT/PT/WIT = transmitters; AT = analytical transmitter; xIC = indicating controller (circle + line = DCS-shared display); XV = on/off valve; NRV = check valve; PSV = relief valve; MFC = mass-flow controller.\n",
           "Control loops: TIC-101 -> TV-101 (CW/steam split-range) | AIC-102 -> P-102/P-103 (pH split-range) | AIC-103 DO cascade -> SIC-106 (agitation) -> FIC-101 (air) -> O2 enrichment | ",
           "PIC-104 -> PV-104 (back-pressure) | LIC-105 -> P-106 (antifoam).\n",
           "All product-contact lines SS316L, orbital-welded, self-draining, steam-sterilisable; sterile boundaries on feed, inoculum and sample ports with double-block-and-bleed. Gas exit via condenser + hydrophobic 0.2 um filter.")) +
    theme_void(base_size = 11) +
    theme(legend.position = "bottom", legend.title = element_text(face = "bold"),
          plot.title = element_text(face = "bold", size = 17, hjust = 0.02),
          plot.subtitle = element_text(size = 10, colour = "grey30", hjust = 0.02),
          plot.caption = element_text(size = 7.5, hjust = 0, colour = "grey25", margin = margin(t = 8)),
          plot.margin = margin(10, 12, 10, 12), plot.background = element_rect(fill = "white", colour = NA))
}

slide_design_control()
slide_comparison()

pid_pf <- build_pid()                                   # production fermenter, as specified
print(pid_pf)
ggsave(file.path(out_dir, "Slide3_PID_PF-101_Production_Fermenter.png"), pid_pf, width = 18, height = 14.5, dpi = 200, bg = "white")

pid_sf <- build_pid(tag = "SF-102", title = "SEED FERMENTER",                      # seed fermenter, corrected to its specification
                    spec1 = "600 L gross | 400 L working",
                    spec2 = "SS316L, EP Ra <= 0.4 um | 3 barg + FV",
                    spec3 = "2 x Rushton | 4 baffles | jacketed | SIP/CIP",
                    drawing = "TBN-PID-102 Rev A",
                    vol_label = "400 L working volume",
                    n_imp = 2, imp_labels = c("RDT", "RDT"),
                    show_feeds = FALSE, show_pump = FALSE,
                    outlet_label = "to PF-101 (steam-sterilisable transfer line)")
print(pid_sf)
ggsave(file.path(out_dir, "Slide3_PID_SF-102_Seed_Fermenter.png"), pid_sf, width = 18, height = 14.5, dpi = 200, bg = "white")

cat("\nAll slides saved in:", out_dir, "\n")