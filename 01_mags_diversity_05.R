## =========================================================
## 01_mags_diversity.R (GitHub-ready version; workflow-aware colors)
## - Create results_mags_diversity/ALL and /PAIRWISE
## - ALL: alpha/beta + global + pairwise summaries -> Stats_ALL.xlsx
## - PAIRWISE: each pair gets a folder with alpha/beta plots + stats + TSV
## - NMDS ellipse only for groups with n>=3 (correct behavior)
## - Small fonts for many groups
## - NEW: workflow-aware ordering and color differentiation for S / M
## =========================================================

suppressPackageStartupMessages({
  library(vegan)
  library(ggplot2)
  library(patchwork)
  library(readxl)
  library(rstatix)
  library(dplyr)
  library(stringr)
  library(openxlsx)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b

## -------------------- USER SETTINGS --------------------
MAGS_FILE  <- "mags_95_75_10.tsv"
GROUP_XLSX <- "Group.xlsx"
OUT_ROOT   <- "results_mags_diversity"

GROUP_COL  <- "Group"
ID_COL     <- "ID"

RUN_ALL      <- TRUE
RUN_PAIRWISE <- TRUE
PERMUTATIONS <- 999
set.seed(123)

## Fonts
BASE_FONT_SIZE   <- 9
LEGEND_TEXT_SIZE <- 7
AXIS_TEXT_SIZE   <- 8

## -------------------- COLORS --------------------
## Base stage colors: keep same global hue identity
BASE_COLORS <- c(
  F = "#1b9e77",   # Farm
  S = "#377eb8",   # Slaughter
  M = "#ff7f00",   # Market
  C = "#984ea3"    # Control
)

## Day tint: temporal logic retained
DAY_TINT <- c(
  D1  = 0.55,
  D31 = 0.75,
  D57 = 0.95,
  D59 = 0.95,
  D60 = 0.95
)

DEFAULT_TINT  <- 0.85
UNKNOWN_COLOR <- "#7f7f7f"

## Compartment order used in the workflow parser
COMP_ORDER <- c("Chicken","Meat","Carcass","Worker","Human","Environment","Water","Other")
COMP_SHAPES <- c(
  Chicken=16, Meat=15, Carcass=17, Worker=19,
  Human=18, Environment=3, Water=4, Other=1
)

## Process order within each production stage
PROCESS_ORDER <- list(
  F = c(
    ChickenFeces = 1,
    Feed         = 2,
    WorkerFeces  = 3,
    WorkerHands  = 4
  ),
  S = c(
    Cutting      = 1,
    Evisceration = 2,
    PostChill    = 3
  ),
  M = c(
    MarketEnv    = 1,
    CutBreast    = 2,
    CutLeg       = 3,
    WholeCarcass = 4,
    MarketWorker = 5
  ),
  C = c(
    HumanFeces   = 1,
    HumanHands   = 2
  )
)

## Within-stage color tint by process
## Larger values produce darker colors closer to the base stage color
PROCESS_TINT <- list(
  F = c(
    ChickenFeces = 0.95,
    Feed         = 0.80,
    WorkerFeces  = 0.68,
    WorkerHands  = 0.55
  ),
  S = c(
    Cutting      = 0.98,
    Evisceration = 0.82,
    PostChill    = 0.66
  ),
  M = c(
    MarketEnv    = 0.98,
    CutBreast    = 0.86,
    CutLeg       = 0.75,
    WholeCarcass = 0.64,
    MarketWorker = 0.53
  ),
  C = c(
    HumanFeces   = 0.92,
    HumanHands   = 0.72
  )
)

## Alpha style
ALPHA_JITTER <- 0.28
SIZE_JITTER  <- 2.2
WIDTH_JITTER <- 0.15
SIZE_MEAN    <- 3.6
STROKE_MEAN  <- 0.6
ALPHA_ERRBAR <- 0.75
LINE_ERRBAR  <- 0.55

AUTO_ROTATE_X <- TRUE
ROTATE_N_GROUPS <- 8
ROTATE_MAX_CHARS <- 10
ROTATE_ANGLE <- 45
ROTATE_HJUST <- 1
ROTATE_VJUST <- 1

## -------------------- Helpers --------------------
safe_try <- function(expr){
  suppressWarnings(
    tryCatch(expr, error=function(e) NULL)
  )
}

clean_str <- function(x){
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  x
}

read_mags <- function(path){
  df <- read.delim(path, header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
  feature_col <- colnames(df)[1]
  rownames(df) <- df[[feature_col]]
  df[[feature_col]] <- NULL
  for (j in seq_len(ncol(df))) df[[j]] <- suppressWarnings(as.numeric(df[[j]]))
  df[is.na(df)] <- 0
  mat <- t(as.matrix(df))  # rows=samples, cols=features
  rownames(mat) <- clean_str(rownames(mat))
  colnames(mat) <- clean_str(colnames(mat))
  mat
}

read_group_xlsx <- function(path){
  g <- readxl::read_xlsx(path) |> as.data.frame()
  colnames(g) <- clean_str(colnames(g))
  if (!(ID_COL %in% colnames(g))) stop(paste0("Group.xlsx must contain column: ", ID_COL))
  if (!(GROUP_COL %in% colnames(g))) stop(paste0("Group.xlsx must contain column: ", GROUP_COL))
  g[[ID_COL]] <- clean_str(g[[ID_COL]])
  g[[GROUP_COL]] <- clean_str(g[[GROUP_COL]])
  g <- g[!is.na(g[[ID_COL]]) & g[[ID_COL]]!="" & !is.na(g[[GROUP_COL]]) & g[[GROUP_COL]]!="", , drop=FALSE]
  g
}

alpha_metrics <- function(mags){
  Richness <- vegan::specnumber(mags)
  Shannon  <- vegan::diversity(mags, index="shannon", base=exp(1))
  Simpson  <- vegan::diversity(mags, index="simpson")
  Pielou   <- ifelse(Richness > 1, Shannon / log(Richness, base=exp(1)), NA_real_)
  data.frame(Richness, Shannon, Simpson, Pielou, check.names=FALSE)
}

## -------------------- Parse Group --------------------
detect_stage <- function(g){
  g0 <- toupper(g)
  if (str_detect(g0, "^[FSMC]")) return(substr(g0, 1, 1))
  if (str_detect(g0, "FARM")) return("F")
  if (str_detect(g0, "SLAUGHTER|EVISCER|CUTTING")) return("S")
  if (str_detect(g0, "MARKET")) return("M")
  if (str_detect(g0, "CONTROL")) return("C")
  NA_character_
}

detect_day <- function(g){
  g0 <- toupper(g)
  m <- str_match(g0, "(D\\d+)")
  if (!is.na(m[,2])) return(m[,2])
  m2 <- str_match(g0, "DAY(\\d+)")
  if (!is.na(m2[,2])) return(paste0("D", m2[,2]))
  NA_character_
}

detect_compartment <- function(g){
  g0 <- toupper(g)
  
  ## Apply more specific rules first
  if (str_detect(g0, "HANDS|HAND")) return("Worker")
  if (str_detect(g0, "WORKER")) return("Human")
  if (str_detect(g0, "CARCASS")) return("Carcass")
  if (str_detect(g0, "CHICKEN|LEG|BREAST|WHOLE|FECES|GUT|INTESTINE")) return("Chicken")
  if (str_detect(g0, "MEAT")) return("Meat")
  if (str_detect(g0, "ENV|ENVIRONMENT|KNIFE|BOARD|SURFACE|AIR|DUST|DOOR|EXIT")) return("Environment")
  if (str_detect(g0, "WATER|POOL|CHILL")) return("Water")
  if (str_detect(g0, "HUMAN")) return("Human")
  
  "Other"
}

detect_process <- function(g){
  g0 <- toupper(g)
  st <- detect_stage(g0)
  
  ## Farm
  if (st == "F") {
    if (str_detect(g0, "^F_W_H_")) return("WorkerHands")
    if (str_detect(g0, "^F_W_"))   return("WorkerFeces")
    if (str_detect(g0, "^F_F_"))   return("Feed")
    if (str_detect(g0, "^F_D"))    return("ChickenFeces")
  }
  
  ## Slaughter
  if (st == "S") {
    if (str_detect(g0, "^S_C_")) return("Cutting")
    if (str_detect(g0, "^S_E_")) return("Evisceration")
    if (str_detect(g0, "^S_P_")) return("PostChill")
  }
  
  ## Market
  if (st == "M") {
    if (str_detect(g0, "^M_E_"))     return("MarketEnv")
    if (str_detect(g0, "^M_SMCB_"))  return("CutBreast")
    if (str_detect(g0, "^M_SMCL_"))  return("CutLeg")
    if (str_detect(g0, "^M_SMWC_"))  return("WholeCarcass")
    if (str_detect(g0, "^M_W_"))     return("MarketWorker")
  }
  
  ## Control
  if (st == "C") {
    if (str_detect(g0, "^C_H_H_")) return("HumanHands")
    if (str_detect(g0, "^C_H_"))   return("HumanFeces")
  }
  
  "Other"
}

parse_group_fields <- function(group_vec){
  g <- clean_str(group_vec)
  data.frame(
    Group       = g,
    Stage       = vapply(g, detect_stage, character(1)),
    Day         = vapply(g, detect_day, character(1)),
    Process     = vapply(g, detect_process, character(1)),
    Compartment = vapply(g, detect_compartment, character(1)),
    stringsAsFactors = FALSE
  )
}

order_groups <- function(groups){
  stage_order <- c(F=1, S=2, M=3, C=4)
  comp_order  <- setNames(seq_along(COMP_ORDER), COMP_ORDER)
  
  df <- parse_group_fields(groups)
  
  df$StageRank <- ifelse(df$Stage %in% names(stage_order), stage_order[df$Stage], 99)
  
  day_num <- suppressWarnings(as.numeric(gsub("^D", "", df$Day)))
  df$DayRank <- ifelse(!is.na(day_num), day_num, 999)
  
  df$ProcessRank <- mapply(function(st, pr){
    if (!is.na(st) && st %in% names(PROCESS_ORDER) && pr %in% names(PROCESS_ORDER[[st]])) {
      PROCESS_ORDER[[st]][[pr]]
    } else {
      999
    }
  }, df$Stage, df$Process)
  
  df$CompRank <- ifelse(df$Compartment %in% names(comp_order), comp_order[df$Compartment], 999)
  
  ## For S and M: workflow/process first
  ## For F and C: day first
  df$PrimaryRank <- ifelse(df$Stage %in% c("S", "M"), df$ProcessRank, df$DayRank)
  df$SecondRank  <- ifelse(df$Stage %in% c("S", "M"), df$CompRank,  df$ProcessRank)
  df$ThirdRank   <- ifelse(df$Stage %in% c("S", "M"), df$DayRank,   df$CompRank)
  
  df$Group[order(df$StageRank, df$PrimaryRank, df$SecondRank, df$ThirdRank, df$Group)]
}

tint_to_white <- function(hex, tint){
  tint <- max(0, min(1, tint))
  rgb0 <- grDevices::col2rgb(hex) / 255
  rgb1 <- (1 - tint) * 1 + tint * rgb0
  grDevices::rgb(rgb1[1,1], rgb1[2,1], rgb1[3,1])
}

get_process_tint <- function(stage, process){
  if (!is.na(stage) && stage %in% names(PROCESS_TINT)) {
    x <- PROCESS_TINT[[stage]]
    if (!is.na(process) && process %in% names(x)) return(x[[process]])
  }
  DEFAULT_TINT
}

make_group_palette <- function(groups){
  groups <- order_groups(unique(groups))
  df <- parse_group_fields(groups)
  cols <- setNames(rep(UNKNOWN_COLOR, length(groups)), groups)
  
  for (i in seq_len(nrow(df))) {
    g  <- df$Group[i]
    st <- df$Stage[i]
    dy <- df$Day[i]
    pr <- df$Process[i]
    
    if (is.na(st) || !(st %in% names(BASE_COLORS))) {
      cols[g] <- UNKNOWN_COLOR
      next
    }
    
    day_tint <- DEFAULT_TINT
    if (!is.na(dy) && dy %in% names(DAY_TINT)) {
      day_tint <- DAY_TINT[[dy]]
    }
    
    proc_tint <- get_process_tint(st, pr)
    
    ## S / M: process dominates
    ## F / C: day dominates
    final_tint <- if (st %in% c("S", "M")) {
      0.25 * day_tint + 0.75 * proc_tint
    } else {
      0.75 * day_tint + 0.25 * proc_tint
    }
    
    final_tint <- max(0.45, min(0.98, final_tint))
    cols[g] <- tint_to_white(BASE_COLORS[[st]], final_tint)
  }
  
  cols
}

theme_nc_small <- function(){
  theme_bw(base_size = BASE_FONT_SIZE) +
    theme(
      axis.text = element_text(color="black", size=AXIS_TEXT_SIZE),
      axis.title = element_text(color="black", face="bold", size=BASE_FONT_SIZE+1),
      legend.text = element_text(size=LEGEND_TEXT_SIZE),
      legend.title = element_blank(),
      legend.position = "left",
      panel.grid = element_blank()
    )
}

plot_alpha_nc <- function(df, metric, ylab, pal_named){
  df$Group <- factor(df$Group, levels = names(pal_named))
  n_groups <- length(unique(df$Group))
  max_chars <- max(nchar(as.character(unique(df$Group))), na.rm=TRUE)
  do_rotate <- isTRUE(AUTO_ROTATE_X) && (n_groups >= ROTATE_N_GROUPS || max_chars >= ROTATE_MAX_CHARS)
  
  p <- ggplot(df, aes(x=Group, y=.data[[metric]])) +
    geom_jitter(aes(color=Group), alpha=ALPHA_JITTER, size=SIZE_JITTER, width=WIDTH_JITTER) +
    stat_summary(fun.data="mean_cl_normal", geom="errorbar", width=0.20, alpha=ALPHA_ERRBAR, linewidth=LINE_ERRBAR) +
    stat_summary(aes(fill=Group), fun="mean", geom="point", size=SIZE_MEAN, shape=21, color="black", stroke=STROKE_MEAN) +
    scale_color_manual(values=pal_named, drop=FALSE) +
    scale_fill_manual(values=pal_named, drop=FALSE) +
    labs(x=NULL, y=ylab) +
    theme_nc_small() +
    theme(legend.position="none", axis.ticks.x=element_blank())
  
  if (do_rotate) p <- p + theme(axis.text.x = element_text(angle=ROTATE_ANGLE, hjust=ROTATE_HJUST, vjust=ROTATE_VJUST))
  p
}

## Beta diversity plot with ellipses (n>=3 only)
plot_beta_nmds_group <- function(plot_df, pal_group, stress, anosim_res){
  plot_df$Group <- factor(plot_df$Group, levels = names(pal_group))
  
  gs <- table(plot_df$Group)
  eligible <- names(gs[gs >= 3])
  
  p <- ggplot(plot_df, aes(x=NMDS1, y=NMDS2)) +
    geom_vline(xintercept=0, linetype="dotted", color="gray50") +
    geom_hline(yintercept=0, linetype="dotted", color="gray50") +
    geom_point(aes(color=Group), size=2.8, alpha=0.9) +
    scale_color_manual(values=pal_group, drop=FALSE) +
    theme_nc_small() +
    labs(x="NMDS1", y="NMDS2")
  
  if (length(eligible) > 0) {
    p <- p + stat_ellipse(
      data = subset(plot_df, Group %in% eligible),
      aes(group=Group, color=Group),
      type="t", level=0.68, linewidth=0.6, show.legend=FALSE
    )
  }
  
  r_val <- if (!is.null(anosim_res)) round(as.numeric(anosim_res$statistic), 3) else NA
  p_val <- if (!is.null(anosim_res)) signif(as.numeric(anosim_res$signif), 3) else NA
  stress_show <- ifelse(is.na(stress), NA, round(stress, 3))
  
  p_txt <- ggplot() +
    annotate("text", x=0, y=0,
             label=paste0("Stress = ", stress_show,
                          "\nANOSIM\nR = ", r_val,
                          "\np = ", p_val),
             size=3.2, fontface="bold") +
    theme_void(base_size = BASE_FONT_SIZE) +
    theme(panel.border = element_rect(fill=NA, color="black", linewidth=0.8))
  
  p + p_txt + plot_layout(widths=c(4.3,1))
}

subset_dist <- function(d, idx){
  m <- as.matrix(d)
  as.dist(m[idx, idx, drop=FALSE])
}

## -------------------- XLSX writers --------------------
write_stats_xlsx_all <- function(path_xlsx, alpha_kw, alpha_pw_list, beta_global, beta_perm_tbl, beta_pairwise){
  wb <- createWorkbook()
  
  addWorksheet(wb, "Alpha_KW")
  writeDataTable(wb, "Alpha_KW", alpha_kw)
  
  addWorksheet(wb, "Alpha_Pairwise_BH")
  alpha_pw <- bind_rows(lapply(names(alpha_pw_list), function(nm){
    df <- alpha_pw_list[[nm]]
    if (is.null(df) || nrow(df)==0) return(NULL)
    df$Metric <- nm
    df
  }))
  if (is.null(alpha_pw) || nrow(alpha_pw)==0) alpha_pw <- data.frame(Note="No alpha pairwise results (maybe only 1 group or errors).")
  writeDataTable(wb, "Alpha_Pairwise_BH", alpha_pw)
  
  addWorksheet(wb, "Beta_Global")
  writeDataTable(wb, "Beta_Global", beta_global)
  
  addWorksheet(wb, "Beta_PERMANOVA_Table")
  if (is.null(beta_perm_tbl) || nrow(beta_perm_tbl)==0) beta_perm_tbl <- data.frame(Note="PERMANOVA table empty (adonis2 failed).")
  writeDataTable(wb, "Beta_PERMANOVA_Table", beta_perm_tbl)
  
  addWorksheet(wb, "Beta_Pairwise")
  if (is.null(beta_pairwise) || nrow(beta_pairwise)==0) beta_pairwise <- data.frame(Note="No beta pairwise results (pairs skipped/failed or n too small).")
  writeDataTable(wb, "Beta_Pairwise", beta_pairwise)
  
  saveWorkbook(wb, path_xlsx, overwrite=TRUE)
}

write_pairwise_xlsx <- function(path_xlsx, alpha_tbl, beta_tbl){
  wb <- createWorkbook()
  addWorksheet(wb, "Alpha")
  writeDataTable(wb, "Alpha", alpha_tbl)
  addWorksheet(wb, "Beta")
  writeDataTable(wb, "Beta", beta_tbl)
  saveWorkbook(wb, path_xlsx, overwrite=TRUE)
}

sanitize_name <- function(x){
  x <- gsub("[^A-Za-z0-9_\\-\\.]", "_", x)
  x
}

## -------------------- MAIN --------------------
run_all_and_pairwise <- function(meta, mags_mat){
  dir.create(OUT_ROOT, showWarnings=FALSE, recursive=TRUE)
  out_all <- file.path(OUT_ROOT, "ALL")
  out_pw  <- file.path(OUT_ROOT, "PAIRWISE")
  dir.create(out_all, showWarnings=FALSE, recursive=TRUE)
  dir.create(out_pw,  showWarnings=FALSE, recursive=TRUE)
  
  keep <- intersect(meta[[ID_COL]], rownames(mags_mat))
  if (length(keep) < 3) stop("Fewer than 3 matched samples were found. Please check that IDs in Group.xlsx match the sample names in the abundance table.")
  
  meta <- meta[meta[[ID_COL]] %in% keep, , drop=FALSE]
  mags <- mags_mat[keep, , drop=FALSE]
  meta <- meta[match(keep, meta[[ID_COL]]), , drop=FALSE]
  
  group_levels <- order_groups(unique(meta[[GROUP_COL]]))
  meta$Group <- factor(meta[[GROUP_COL]], levels=group_levels)
  pal_group <- make_group_palette(levels(meta$Group))
  
  ## ==================== ALL ====================
  if (RUN_ALL) {
    a <- alpha_metrics(mags)
    alpha_df <- data.frame(samples=meta[[ID_COL]], Group=meta$Group, a, check.names=FALSE)
    write.table(alpha_df, file=file.path(out_all, "Alpha_diversity_metrics.tsv"), sep="\t", row.names=FALSE, quote=FALSE)
    
    ## Alpha stats
    alpha_kw <- data.frame(Metric=c("Richness","Shannon","Simpson","Pielou"), p_value=NA_real_)
    alpha_pw_list <- list()
    
    sink(file.path(out_all, "Stats_ALPHA.txt"))
    cat("=== ALPHA: Kruskal-Wallis (all groups) ===\n")
    for (m in alpha_kw$Metric) {
      p <- safe_try(kruskal.test(alpha_df[[m]] ~ alpha_df$Group)$p.value)
      pv <- ifelse(is.null(p), NA, p)
      alpha_kw$p_value[alpha_kw$Metric==m] <- pv
      cat(m, "\tp=", pv, "\n", sep="")
    }
    cat("\n=== ALPHA: Pairwise Wilcoxon (BH, exact=FALSE) ===\n")
    for (m in alpha_kw$Metric) {
      pw <- safe_try(rstatix::pairwise_wilcox_test(
        alpha_df, formula=as.formula(paste0(m, " ~ Group")),
        p.adjust.method="BH", exact=FALSE
      ))
      alpha_pw_list[[m]] <- if (is.null(pw)) NULL else as.data.frame(pw)
    }
    sink()
    
    ## Alpha plots
    p1 <- plot_alpha_nc(alpha_df, "Richness", "Richness", pal_group)
    p2 <- plot_alpha_nc(alpha_df, "Shannon",  "Shannon",  pal_group)
    p3 <- plot_alpha_nc(alpha_df, "Simpson",  "Simpson",  pal_group)
    p4 <- plot_alpha_nc(alpha_df, "Pielou",   "Pielou",   pal_group)
    ggsave(file.path(out_all, "Fig_Alpha_diversity_4metrics.pdf"),
           plot=(p1+p2+p3+p4)+plot_layout(ncol=2), width=9.5, height=6.8, units="in")
    
    ## Beta
    dist_bray <- safe_try(vegan::vegdist(mags, method="bray"))
    nmds <- suppressMessages(safe_try(vegan::metaMDS(dist_bray, k=2, trymax=100, autotransform=FALSE, trace=FALSE)))
    stress <- ifelse(is.null(nmds), NA_real_, nmds$stress)
    
    ad_all <- suppressMessages(safe_try(vegan::adonis2(dist_bray ~ Group, data=meta, permutations=PERMUTATIONS)))
    an_all <- suppressMessages(safe_try(vegan::anosim(dist_bray, meta$Group, permutations=PERMUTATIONS)))
    
    beta_global <- data.frame(
      NMDS_stress = stress,
      PERMANOVA_R2 = if (!is.null(ad_all)) as.numeric(ad_all$R2[1]) else NA_real_,
      PERMANOVA_p  = if (!is.null(ad_all)) as.numeric(ad_all$`Pr(>F)`[1]) else NA_real_,
      ANOSIM_R     = if (!is.null(an_all)) as.numeric(an_all$statistic) else NA_real_,
      ANOSIM_p     = if (!is.null(an_all)) as.numeric(an_all$signif) else NA_real_
    )
    
    beta_perm_tbl <- if (!is.null(ad_all)) {
      tb <- as.data.frame(ad_all)
      tb$Term <- rownames(tb)
      rownames(tb) <- NULL
      tb
    } else data.frame()
    
    ## Beta pairwise summary for ALL.xlsx
    beta_pairwise <- data.frame(
      Group1=character(), Group2=character(),
      PERMANOVA_R2=numeric(), PERMANOVA_p=numeric(),
      ANOSIM_R=numeric(), ANOSIM_p=numeric(),
      n_total=integer(),
      stringsAsFactors=FALSE
    )
    
    group_list <- levels(meta$Group)
    pairs <- combn(group_list, 2, simplify=FALSE)
    for (pair in pairs) {
      g1 <- pair[1]
      g2 <- pair[2]
      idx <- which(meta$Group %in% c(g1, g2))
      if (length(idx) < 3) next
      dist_sub <- subset_dist(dist_bray, idx)
      meta_sub <- meta[idx, , drop=FALSE]
      meta_sub$Group <- droplevels(meta_sub$Group)
      
      ad_p <- suppressMessages(safe_try(vegan::adonis2(dist_sub ~ Group, data=meta_sub, permutations=PERMUTATIONS)))
      an_p <- suppressMessages(safe_try(vegan::anosim(dist_sub, meta_sub$Group, permutations=PERMUTATIONS)))
      
      if (is.null(ad_p) && is.null(an_p)) next
      beta_pairwise <- rbind(beta_pairwise, data.frame(
        Group1=g1, Group2=g2,
        PERMANOVA_R2 = if (!is.null(ad_p)) as.numeric(ad_p$R2[1]) else NA_real_,
        PERMANOVA_p  = if (!is.null(ad_p)) as.numeric(ad_p$`Pr(>F)`[1]) else NA_real_,
        ANOSIM_R     = if (!is.null(an_p)) as.numeric(an_p$statistic) else NA_real_,
        ANOSIM_p     = if (!is.null(an_p)) as.numeric(an_p$signif) else NA_real_,
        n_total = length(idx),
        stringsAsFactors=FALSE
      ))
    }
    
    sink(file.path(out_all, "Stats_BETA.txt"))
    cat("=== BETA: Global PERMANOVA/ANOSIM ===\n")
    print(beta_global)
    cat("\nPERMANOVA full table:\n")
    if (nrow(beta_perm_tbl) > 0) print(beta_perm_tbl)
    cat("\nPairwise summary rows: ", nrow(beta_pairwise), "\n", sep="")
    sink()
    
    ## NMDS plot with ellipses where possible
    if (!is.null(nmds)) {
      pts <- as.data.frame(nmds$points)
      pts$samples <- rownames(pts)
      colnames(pts)[1:2] <- c("NMDS1", "NMDS2")
      plot_df <- merge(pts, meta[, c(ID_COL, "Group"), drop=FALSE], by.x="samples", by.y=ID_COL)
      plot_df$Group <- factor(plot_df$Group, levels=levels(meta$Group))
      
      ggsave(file.path(out_all, "Fig_Beta_NMDS_BrayCurtis_GroupColor.pdf"),
             plot=plot_beta_nmds_group(plot_df, pal_group, nmds$stress, an_all),
             width=11, height=6, units="in")
    }
    
    ## ALL stats -> XLSX
    write_stats_xlsx_all(
      path_xlsx=file.path(out_all, "Stats_ALL.xlsx"),
      alpha_kw=alpha_kw,
      alpha_pw_list=alpha_pw_list,
      beta_global=beta_global,
      beta_perm_tbl=beta_perm_tbl,
      beta_pairwise=beta_pairwise
    )
  }
  
  ## ==================== PAIRWISE (folders) ====================
  if (RUN_PAIRWISE) {
    group_list <- levels(meta$Group)
    if (length(group_list) >= 2) {
      pairs <- combn(group_list, 2, simplify=FALSE)
      
      for (pair in pairs) {
        g1 <- pair[1]
        g2 <- pair[2]
        idx <- which(meta$Group %in% c(g1, g2))
        if (length(idx) < 3) next
        
        pair_name <- sanitize_name(paste0(g1, "_vs_", g2))
        subdir <- file.path(out_pw, pair_name)
        dir.create(subdir, showWarnings=FALSE, recursive=TRUE)
        
        meta_sub <- meta[idx, , drop=FALSE]
        mags_sub <- mags[meta_sub[[ID_COL]], , drop=FALSE]
        meta_sub$Group <- droplevels(meta_sub$Group)
        
        ## Alpha diversity
        a <- alpha_metrics(mags_sub)
        alpha_sub <- data.frame(samples=meta_sub[[ID_COL]], Group=meta_sub$Group, a, check.names=FALSE)
        write.table(alpha_sub, file=file.path(subdir, "Alpha_diversity_metrics.tsv"),
                    sep="\t", row.names=FALSE, quote=FALSE)
        
        alpha_p <- data.frame(Metric=c("Richness","Shannon","Simpson","Pielou"), p_value=NA_real_)
        for (m in alpha_p$Metric) {
          p <- safe_try(wilcox.test(alpha_sub[[m]] ~ alpha_sub$Group, exact=FALSE)$p.value)
          alpha_p$p_value[alpha_p$Metric==m] <- ifelse(is.null(p), NA, p)
        }
        
        ## Alpha diversity plots
        pal_pair <- pal_group[c(as.character(g1), as.character(g2))]
        p1 <- plot_alpha_nc(alpha_sub, "Richness", "Richness", pal_pair)
        p2 <- plot_alpha_nc(alpha_sub, "Shannon",  "Shannon",  pal_pair)
        p3 <- plot_alpha_nc(alpha_sub, "Simpson",  "Simpson",  pal_pair)
        p4 <- plot_alpha_nc(alpha_sub, "Pielou",   "Pielou",   pal_pair)
        ggsave(file.path(subdir, "Fig_Alpha_diversity_4metrics.pdf"),
               plot=(p1+p2+p3+p4)+plot_layout(ncol=2), width=9, height=6.5, units="in")
        
        ## Beta diversity
        dist_sub <- safe_try(vegan::vegdist(mags_sub, method="bray"))
        nmds_sub <- suppressMessages(safe_try(vegan::metaMDS(dist_sub, k=2, trymax=100, autotransform=FALSE, trace=FALSE)))
        ad_sub <- suppressMessages(safe_try(vegan::adonis2(dist_sub ~ Group, data=meta_sub, permutations=PERMUTATIONS)))
        an_sub <- suppressMessages(safe_try(vegan::anosim(dist_sub, meta_sub$Group, permutations=PERMUTATIONS)))
        
        beta_tbl <- data.frame(
          NMDS_stress = if (!is.null(nmds_sub)) nmds_sub$stress else NA_real_,
          PERMANOVA_R2 = if (!is.null(ad_sub)) as.numeric(ad_sub$R2[1]) else NA_real_,
          PERMANOVA_p  = if (!is.null(ad_sub)) as.numeric(ad_sub$`Pr(>F)`[1]) else NA_real_,
          ANOSIM_R     = if (!is.null(an_sub)) as.numeric(an_sub$statistic) else NA_real_,
          ANOSIM_p     = if (!is.null(an_sub)) as.numeric(an_sub$signif) else NA_real_
        )
        
        ## Beta diversity plot
        if (!is.null(nmds_sub)) {
          pts <- as.data.frame(nmds_sub$points)
          pts$samples <- rownames(pts)
          colnames(pts)[1:2] <- c("NMDS1", "NMDS2")
          dfp <- merge(pts, meta_sub[, c(ID_COL, "Group"), drop=FALSE], by.x="samples", by.y=ID_COL)
          dfp$Group <- factor(dfp$Group, levels=levels(meta_sub$Group))
          
          pal2 <- make_group_palette(levels(meta_sub$Group))
          ggsave(file.path(subdir, "Fig_Beta_NMDS_BrayCurtis.pdf"),
                 plot=plot_beta_nmds_group(dfp, pal2, nmds_sub$stress, an_sub),
                 width=9, height=5.5, units="in")
        }
        
        ## Write TXT and XLSX outputs
        sink(file.path(subdir, "Stats.txt"))
        cat("Pair: ", g1, " vs ", g2, "\n\n", sep="")
        cat("Alpha Wilcoxon:\n")
        print(alpha_p)
        cat("\nBeta:\n")
        print(beta_tbl)
        sink()
        
        write_pairwise_xlsx(file.path(subdir, "Stats.xlsx"), alpha_p, beta_tbl)
      }
    }
  }
  
  message("[DONE] Outputs saved to: ", OUT_ROOT)
}

## -------------------- RUN --------------------
mags_mat <- read_mags(MAGS_FILE)
meta_all <- read_group_xlsx(GROUP_XLSX)
meta_all <- meta_all[, c(ID_COL, GROUP_COL), drop=FALSE]

run_all_and_pairwise(meta_all, mags_mat)
