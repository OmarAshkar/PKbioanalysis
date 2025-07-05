

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


