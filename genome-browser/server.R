library(shiny)
library(ggvis)
library(dplyr)

source("ggcircos_helpers.R")

use_donors <- c("DO33200", "DO32887", "DO49184")

donors <- read.table("data/donor_paca_au.csv", header = TRUE, sep = "\t") %>% filter(icgc_donor_id %in% use_donors)
samples <- read.table("data/sample_paca_au.csv", header = TRUE, sep = "\t") %>% filter(icgc_donor_id %in% use_donors)
specimens <- read.table("data/specimen_paca_au.csv", header = TRUE, sep = "\t") %>% filter(icgc_donor_id %in% use_donors)
snp <- read.table("data/filt_simple_somatic_mutation_paca_au_2.csv", header = TRUE, sep = "\t") %>% filter(icgc_donor_id %in% use_donors, chromosome != "MT")
struct <- read.table("data/filt_structural_somatic_mutation_paca_au.csv", header = TRUE, sep = "\t") %>% filter(icgc_donor_id %in% use_donors)
cnv <- read.table("data/filt_copy_number_somatic_mutation_paca_au.csv", header = TRUE, sep = "\t") %>% filter(icgc_donor_id %in% use_donors)
genes <- read.csv("data/genes.csv")
# top_genes <- xap.read_table("icgc_top_genes")
clinvar <- read.csv("data/filt_clinvar_variant_summary.csv", header = TRUE, sep = "\t")

snp_genes <- snp %>%
  inner_join(genes, by = c("gene_affected" = "ensembl_gene_id")) %>%
  inner_join(clinvar, by = c("hgnc_symbol" = "gene_symbol", "chromosome" = "chromosome", "chromosome_start" = "start", "chromosome_end" = "stop", "mutated_from_allele" = "reference_allele", "mutated_to_allele" = "alternate_allele"))

# comment this bit out in the platform and use the view instead above
top_genes <- snp_genes %>%
  group_by(hgnc_symbol, gene_affected, mutated_from_allele, mutated_to_allele, mutation_type, consequence_type, chromosome, chromosome_start, chromosome_end) %>%
  summarise(count = n())

snp_transcripts <- snp_genes[, c("icgc_donor_id", "hgnc_symbol", "transcript_affected", "gene_affected", "chromosome", "chromosome_start", "chromosome_end", "mutated_from_allele", "mutated_to_allele", "consequence_type", "mutation_type")]

snp_transcripts <- snp_transcripts[!duplicated(snp_transcripts), ]

snp_genes <- snp_genes[, c("icgc_donor_id", "hgnc_symbol", "gene_affected", "chromosome", "chromosome_start", "chromosome_end", "mutated_from_allele", "mutated_to_allele", "consequence_type", "mutation_type", "clinical_significance", "dbsnp_rs_no", "review_status", "variant_id", "assembly")]

snp_genes <- snp_genes[!duplicated(snp_genes), ]

snp_clinvar <- snp %>% left_join(snp_transcripts,
  by = c(c(
    "icgc_donor_id", "transcript_affected", "gene_affected",
    "chromosome", "chromosome_start", "chromosome_end",
    "mutated_from_allele", "mutated_to_allele", "consequence_type", "mutation_type"
  ))
)

snp$is_clinvar <- ifelse(is.na(snp_clinvar$hgnc_symbol), "No", "Yes")


## snp and struct are from the same specimens. cnv has more than one specimen per donor
specimens <- unique(snp$icgc_specimen_id)

cnv <- cnv %>% filter(icgc_specimen_id %in% specimens)

chroms <- c(1:22, "X", "Y")
lengths <- c(
  249250621, 243199373, 198022430, 191154276, 180915260, 171115067,
  159138663, 146364022, 141213431, 135534747, 135006516, 133851895,
  115169878, 107349540, 102531392, 90354753, 81195210, 78077248,
  59128983, 63025520, 48129895, 51304566, 155270560, 59373566
)

radians_f <- create_radians(chroms[1:23], lengths[1:23])
radians_m <- create_radians(chroms, lengths)


server <- function(input, output, session) {
  re_values <- reactiveValues(
    scaling_factors = rep(1, 24), previous_radians = radians_f,
    chrom_clicked = "1", chroms_selected = NULL
  )

  radians <- reactive({
    donors_filt <- donors %>% filter(icgc_donor_id == input$donor)

    gender <- unique(donors_filt$donor_sex)

    rads <- create_radians(chroms, lengths * re_values$scaling_factors)
    #       if(gender == "female") {
    #
    #         create_radians(chroms[1:23], lengths[1:23] * re_values$scaling_factors[1:23])
    #
    #       } else {
    #
    #         create_radians(chroms, lengths * re_values$scaling_factors)
    #
    #       }

    isolate(mid <- mean(rads[names(rads) == re_values$chrom_clicked]))

    isolate(prev_mid <- mean(re_values$previous_radians[names(rads) == re_values$chrom_clicked]))

    offset <- mid - prev_mid

    rads - offset
  })


  track_radians <- reactive({
    create_track_radians(radians(), points_per_track = rep(20, length(radians())))
  })


  output$top_clinvar_genes <- renderTable(
    {
      top_genes <- top_genes[, c("hgnc_symbol", "chromosome", "chromosome_start", "mutated_from_allele", "mutated_to_allele", "consequence_type", "count")] %>%
        mutate(count = as.integer(count), chromosome_start = as.integer(chromosome_start)) %>%
        arrange(desc(count)) %>%
        top_n(n = 10)
      setNames(top_genes, c("HGNC", "Chr", "Start", "From", "To", "Consequence", "Count"))
    },
    include.rownames = FALSE
  )

  seq_df <- reactive({
    donors_filt <- donors %>% filter(icgc_donor_id == input$donor)

    #    gender <- unique(donors_filt$donor_sex)

    #     if (gender == "female") {
    #       scale <- re_values$scaling_factors[1:23]
    #     } else {
    scale <- re_values$scaling_factors
    #    }

    create_seq_df(radians(), scale = scale)
  })

  snp_plot_data <- reactive({
    snp_filt <- snp %>% filter(icgc_donor_id == input$donor, consequence_type %in% input$consequence_type)

    snp_data <- snp_filt %>%
      group_by(
        chromosome, chromosome_start, chromosome_end, gene_affected,
        mutation_type, mutated_from_allele, mutated_to_allele, is_clinvar
      ) %>%
      summarise(transcripts = n())

    points <- fit_points(snp_data$chromosome, snp_data$chromosome_start, snp_data$transcripts,
      0.8, 0.6, seq_df(),
      metadata = snp_data[, c(
        "transcripts", "mutation_type", "gene_affected", "chromosome",
        "chromosome_start", "chromosome_end",
        "mutated_from_allele", "mutated_to_allele", "is_clinvar"
      )],
      min_value = 0, max_value = max(snp_data$transcripts) + 1
    )

    points$id <- paste0("snp", 1:nrow(points))

    points
  })

  clinvar_data <- reactive({
    clinvar_gene_data <- snp_genes %>% filter(icgc_donor_id == input$donor)
    clinvar_gene_data
  })

  snp_transcript_data <- reactive({
    snp_data <- snp %>% filter(icgc_donor_id == input$donor)

    snp_data
  })

  struct_plot_data <- reactive({
    struct_filt <- struct %>% filter(
      icgc_donor_id == input$donor,
      chr_from != chr_to | abs(chr_from_bkpt - chr_to_bkpt) > 10^6
    )

    links <- fit_links(
      struct_filt$chr_from, struct_filt$chr_to, struct_filt$chr_from_bkpt, struct_filt$chr_to_bkpt,
      seq_df(), 0.6, 0.6, 0
    )

    links <- links %>%
      ungroup() %>%
      mutate(link = as.numeric(link)) %>%
      arrange(link) %>%
      group_by(link)

    links$annotation <- rep(struct_filt$annotation, each = 3)
    links$pos_from <- rep(struct_filt$chr_from_bkpt, each = 3)
    links$pos_to <- rep(struct_filt$chr_to_bkpt, each = 3)

    #     if (length(re_values$chroms_selected) > 0) {
    #
    #       links <- links %>% filter(name_from %in% re_values$chroms_selected | name_to %in% re_values$chroms_selected)
    #
    #     }

    if (length(re_values$chroms_selected) > 0) {
      links <- isolate(mutate(links, opac = ifelse(name_from %in% re_values$chroms_selected | name_to %in% re_values$chroms_selected,
        1, 0.001
      )))
    } else {
      links$opac <- 1
    }

    links
  })

  cnv_filt <- reactive({
    cnv %>%
      filter(icgc_donor_id == input$donor) %>%
      mutate(pos = (chromosome_end + chromosome_start) / 2)
  })

  cnv_plot_data <- reactive({
    cnv_filt <- cnv_filt()

    cnv_plot_data <- fit_to_seq(cnv_filt$chromosome, cnv_filt$pos, seq_df(),
      metadata = cnv_filt[, c("copy_number", "mutation_type", "chromosome_start", "chromosome_end", "segment_median")]
    )

    cnv_inner <- rep(0.8 + 2 / 90, nrow(cnv_plot_data))
    cnv_outer <- 0.8 + as.numeric(cnv_plot_data$copy_number) / 90

    copy_neutral <- cnv_inner == cnv_outer
    cnv_inner[copy_neutral] <- 0.8 + 1.5 / 90
    cnv_outer[copy_neutral] <- 0.8 + 2.5 / 90

    cnv_plot_data$copy_number <- paste0("cnv", cnv_plot_data$copy_number)

    cnv_plot_data <- data.frame(rbind(cnv_plot_data, cnv_plot_data),
      # r = c(rep(cnv_inner, nrow(cnv_plot_data)), rep(cnv_outer, nrow(cnv_plot_data))))
      r = c(cnv_inner, cnv_outer)
    )

    cnv_plot_data$id <- paste0("cnv", 1:(nrow(cnv_plot_data) / 2))

    cnv_plot_data %>% filter(seq %in% re_values$chroms_selected)
  })

  cnv_line_data <- reactive({
    donors_filt <- donors %>% filter(icgc_donor_id == input$donor)

    gender <- unique(donors_filt$donor_sex)

    cnv_filt <- cnv_filt() %>%
      group_by(chromosome) %>%
      arrange(pos)

    #     if (gender == "female") {
    #
    #       cnv_filt <- cnv_filt %>%
    #         filter(chromosome != "Y")
    #
    #     }

    cnv_line_data <- fit_points(cnv_filt$chromosome, cnv_filt$pos, cnv_filt$copy_number, 0.9, 0.8, seq_df(), max_value = 8, min_value = 0)

    cnv_line_data$opac <- ifelse(cnv_line_data$seq %in% re_values$chroms_selected, 0, 1)

    cnv_line_data
  })

  text_df <- reactive({
    seq_df() %>% mutate(theta = (seq_start + seq_end) / 2, r = 1.05)
  })

  tooltip_fun <- function(data, session) {
    if ("mutation_type" %in% names(data)) {
      tt_data <- snp_plot_data()

      row <- tt_data[tt_data$id == data$id, ]

      transcripts <- snp_transcript_data() %>% filter(gene_affected == row$gene_affected)

      gene_name <- genes %>% filter(ensembl_gene_id %in% row$gene_affected)

      clin_data <- clinvar_data() %>% filter(gene_affected %in% row$gene_affected)

      dbsnp_url <- "http://www.ncbi.nlm.nih.gov/projects/SNP/snp_ref.cgi?rs="
      clinvar_url <- "http://www.ncbi.nlm.nih.gov/clinvar/variation/"

      clin_text <- ""

      if (nrow(clin_data) > 0) {
        for (i in 1:nrow(clin_data)) {
          clin_text <- paste0(clin_text, clin_data[i, c("hgnc_symbol")], " | ", clin_data[i, c("consequence_type")], " | ", clin_data[i, c("clinical_significance")], " | ", "<a href=", dbsnp_url, clin_data[i, c("dbsnp_rs_no")], ">", clin_data[i, c("dbsnp_rs_no")], "</a>", " | ", "<a href=", clinvar_url, clin_data[i, c("variant_id")], " target=_blank>", clin_data[i, c("variant_id")], "</a><br>")
        }
      } else {
        clin_text <- paste0(row$gene_affected, " | chr", row$chromosome, ": No associated ClinVar records found.")
      }

      transcript_text <- ""

      ensembl_gene_url <- "http://www.ensembl.org/Homo_sapiens/Gene/Summary?db=core;g="
      ensembl_transcript_url <- "http://www.ensembl.org/Homo_sapiens/Transcript/Summary?db=core;t="

      for (i in 1:nrow(transcripts)) {
        transcript_text <- paste0(transcript_text, " <a href=", ensembl_transcript_url, transcripts[i, c("transcript_affected")], " target=_blank>", transcripts[i, c("transcript_affected")], "</a>")
      }

      record_colour <- paste0("rgb(", sample(170:255, 1), ",", sample(150:255, 1), ",", sample(170:255, 1), ")")

      paste0(
        "Start: ", row$chromosome_start, "<br>",
        "End: ", row$chromosome_end, "<br>",
        "Base Change: ", row$mutated_from_allele, ">", row$mutated_to_allele, "<br>",
        "Mutation Type: ", row$mutation_type, "<br>",
        "Gene Affected: ", paste0("<a href=", ensembl_gene_url, row$gene_affected, " target=_blank>", gene_name$hgnc_symbol, "</a>"), "<br>",
        'Ensembl ID: <a href="', "javascript:pick_gene('", row$gene_affected, "','", row$mutation_type, "','", row$mutated_from_allele, " > ", row$mutated_to_allele, "','", row$chromosome, "','", row$chromosome_start, "','", row$chromosome_end, "','", record_colour, "')", '">', row$gene_affected, "</a>", "<br>",
        "Total Transcripts Affected: ", '<a href="', "javascript:show_transcripts('", transcript_text, "','", row$gene_affected, "','", gene_name$hgnc_symbol, "','", runif(1), "','", record_colour, "')", '">', row$transcripts, "</a><br>",
        "ClinVar Records: ", '<a href="', "javascript:show_clinvar('", clin_text, "','", runif(1), "','", record_colour, "')", '">', nrow(clin_data), "</a><br>"
      )
    } else if ("copy_number" %in% names(data)) {
      tt_data <- cnv_plot_data()

      rows <- tt_data[tt_data$theta > data$theta - 0.0001 & tt_data$theta < data$theta + 0.0001, ]

      paste0(
        "Start: ", unique(rows$chromosome_start), "<br>",
        "End: ", unique(rows$chromosome_end), "<br>",
        "Segment Median ", unique(rows$segment_median), "<br>",
        "Copy Number: ", sub("cnv", "", unique(rows$copy_number)), "<br>",
        "Mutation Type: ", unique(rows$mutation_type)
      )
    } else if ("link" %in% names(data)) {
      tt_data <- struct_plot_data()

      row <- tt_data[tt_data$link == data$link, ][1, ]

      paste0(
        "<center>",
        row$name_from, ":", row$pos_from, "<br>",
        "&rarr;", "<br>",
        row$name_to, ":", row$pos_to,
        "<center/>",
        "Annotation: ", row$annotation
      )
    }
  }


  tooltip_click_fun <- function(data) {
    str(data)
  }

  click_handle <- function(data, location, session) {
    if (is.null(data)) {
      return(NULL)
    }

    isolate(re_values$chrom_clicked <- data$group)

    isolate(re_values$previous_radians <- radians())

    isolate(re_values$scaling_factors[which(chroms == data$group)] <- ifelse(re_values$scaling_factors[which(chroms == data$group)] == 1,
      input$integer, 1
    ))

    isolate(re_values$chroms_selected <- chroms[which(re_values$scaling_factors > 1)])

    print(data)
  }

  fill_domain <- c(
    c(1:22, "X", "Y"), # chromosomes
    "single base substitution", "insertion of <=200bp", "deletion of <=200bp"
  ) # snp types

  fill_range <- c(
    # chromosome colours from Circos
    "#996600", "#666600", "#99991E", "#CC0000", "#FF0000", "#FF00CC", "#FFCCCC", "#FF9900", "#FFCC00",
    "#FFFF00", "#CCFF00", "#00FF00", "#358000", "#0000CC", "#6699FF", "#99CCFF", "#00FFFF", "#CCFFFF",
    "#9900CC", "#CC33FF", "#CC99FF", "#666666", "#999999", "#CCCCCC",

    # colours for Snps
    "red", "blue", "green"
  )


  stroke_domain <- c(
    c(1:22, "X", "Y"), # chromosomes
    paste0("cnv", 0:8), # copy numbers
    "Yes", "No"
  ) # interchromosomal or not

  stroke_range <- c(
    # chromosome colours from Circos
    "#996600", "#666600", "#99991E", "#CC0000", "#FF0000", "#FF00CC", "#FFCCCC", "#FF9900", "#FFCC00",
    "#FFFF00", "#CCFF00", "#00FF00", "#358000", "#0000CC", "#6699FF", "#99CCFF", "#00FFFF", "#CCFFFF",
    "#9900CC", "#CC33FF", "#CC99FF", "#666666", "#999999", "#CCCCCC",

    # colours for copy numbers
    "#0C2F60", "#0C2F60", "green", rep("#C11F29", 6),

    # colours for links
    "blue", "grey"
  )


  add_tooltip <- function(vis, html, on = c("hover", "click")) {
    on <- match.arg(on)

    show_tooltip2 <- function(data, location, session, ...) {
      if (is.null(data)) {
        hide_tooltip(session)
        return()
      }

      html <- html(data)
      if (is.null(html)) {
        hide_tooltip(session)
      } else {
        show_tooltip(session, location$x + 5, location$y + 5, html)
      }
    }
    hide_tooltip2 <- function(session) {
      hide_tooltip(session)
    }

    switch(on,
      click = handle_click(vis, show_tooltip2),
      hover = handle_hover(vis, show_tooltip2)
    )
  }

  ggvis() %>%
    # layer_rects(data = data.frame(x=-1.5,y=-1.5, x2 = 1.5, y2 = 1.5), ~x, ~y, x2 = ~x2, y2 = ~ y2, fill:="#000000") %>%
    add_track(track_radians, 1, 0.9, fill = ~group, stroke = ~group, fillOpacity := 0.7, fillOpacity.hover := 1) %>%
    layer_paths(
      data = cnv_line_data %>% group_by(seq), ~ sin(theta) * r, ~ cos(theta) * r, interpolate := "basis",
      strokeWidth := 0.8, opacity := ~opac
    ) %>%
    layer_paths(
      data = cnv_plot_data %>% group_by(theta), ~ sin(theta) * r, ~ cos(theta) * r, stroke = ~copy_number,
      strokeWidth := 2, strokeWidth.hover := 3
    ) %>%
    add_circles(track_radians, r = 0.8 + 2 / 90, opacity := 0.2) %>%
    add_track(track_radians, 0.8, 0.6, strokeOpacity := 0.5, stroke := "black", strokeWidth := 0.5) %>%
    add_circles(track_radians, seq(0.6, 0.8, length.out = 7)[-c(1, 7)], strokeOpacity := 0.3, strokeWidth := 0.5) %>%
    layer_points(
      data = snp_plot_data, ~ sin(theta) * r, ~ cos(theta) * r, fill = ~mutation_type, shape = ~is_clinvar,
      key := ~id, size = ~is_clinvar, size.hover := 30, strokeOpacity = ~is_clinvar, strokeOpacity.hover := 1,
      stroke := "black", strokeWidth := 0.5
    ) %>%
    #     add_links(seq_df = seq_df, data = struct_plot_data, name_from = chr_from, name_to = chr_to,
    #               pos_from = chr_from_bkpt, pos_to = chr_to_bkpt, 0.6, 0.6, 0, stroke = ~inter,
    #               strokeWidth := 1, strokeWidth.hover := 2) %>%
    layer_paths(
      data = struct_plot_data, ~ sin(theta) * r, ~ cos(theta) * r, stroke = ~name_from,
      strokeWidth := ~opac, strokeWidth.hover := 2, strokeOpacity := 1, interpolate := "basis"
    ) %>%
    layer_text(
      data = text_df, ~ sin(theta) * r, ~ cos(theta) * r, text := ~seq, align := "center", baseline := "middle",
      angle := ~ 180 * (theta - pi * (cos(theta) < 0)) / pi
    ) %>%
    add_tooltip(tooltip_fun, "hover") %>%
    add_tooltip(tooltip_click_fun, "click") %>%
    handle_click(click_handle) %>%
    #     scale_numeric("x", domain = c(-1, 1), nice = FALSE, clamp = TRUE) %>%
    #     scale_numeric("y", domain = c(-1, 1), nice = FALSE, clamp = TRUE) %>%
    scale_ordinal("fill", domain = fill_domain, range = fill_range) %>%
    scale_ordinal("stroke", domain = stroke_domain, range = stroke_range) %>%
    scale_nominal("shape", domain = c("Yes", "No"), range = c("cross", "circle")) %>%
    scale_nominal("size", domain = c("Yes", "No"), range = c(20, 10)) %>%
    scale_nominal("opacity", domain = c("Yes", "No"), range = c(1, 0)) %>%
    hide_axis("x") %>%
    hide_axis("y") %>%
    hide_legend(c("fill", "stroke", "shape", "size")) %>%
    set_options(hover_duration = 0, width = 775, height = 775, keep_aspect = TRUE, duration = 1000) %>%
    bind_shiny("plot")
}
