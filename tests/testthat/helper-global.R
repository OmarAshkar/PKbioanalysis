

# PKbioanalysis:::.reset_samples_db()
# .generate_testing_injeseq()
# path <- system.file("extdata", "waters_MZML_ex", package="PKChromaMetrics")
# main <- read_chrom(path, format = "mzML", peaks = imported_peaks)


# path <- system.file("extdata", "waters_raw_ex", package="PKChromaMetrics")
# imported_peaks_path <- system.file("sample_peaktab.csv", package="PKChromaMetrics")
# imported_peaks <- read.csv(imported_peaks_path)
# main <- read_chrom(path, method = "3")

.reset_samples_db()
x <- system.file("cmpds.yaml", package = "PKbioanalysis")  |> 
    .parse_cmpds()  |> suppressWarnings()
.save_cmpd_db(x)

path <- system.file("extdata", "waters_raw_ex", package="PKbioanalysis")
# imported_peaks <- read_experiment_results(system.file("extdata", "waters_NEU_PK/quandata.xml", package="PKChromaMetrics"), vendor = "targetlynx")
# imported_peaks <- .peakresToDF(imported_peaks)
main <- read_chrom(path, method = 1)



## quant obj 

dat <- system.file("extdata", "08122019_MTG.txt", package = "PKbioanalysis")
suppressWarnings(
    dat <- .parse_tlynx_csv(dat)
)
quantobj <- lapply(names(dat$res), function(y){
    dat$res[[y]]$compound <- y
    dat$res[[y]]
})
quantobj <- do.call("rbind", quantobj)
quantobj <- quantobj |> rename(filename = "Name") |>
    rename(vial = "Vial") |>
    rename(type = "Type") |>
    # rename(height = "PEAK_height") |>
    # rename(peak_start = "PEAK_startrt") |>
    # rename(peak_end = "PEAK_endrt") |>
    rename(SN = "S/N") |> 
    mutate(height = NA) |>
    mutate(peak_start = NA) |>
    mutate(peak_end = NA) |>
    mutate(IS_name = NA) |>
    dplyr::select("filename", "vial", "type", "stdconc", "compound", "area", "height", "peak_start", "peak_end", "SN", "IS_name", "RT") |>
    mutate(across(c("stdconc", "area", "height", "peak_start", "peak_end", "SN", "RT"), as.numeric)) 


quantobj <- create_quant_object(quantobj)
cmpyml <- system.file("cmpds_MTG.yaml", package = "PKbioanalysis")
cmpyml <- .parse_cmpds(cmpyml)  |> suppressWarnings()
.save_cmpd_db(cmpyml)
