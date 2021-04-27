ks.data = system.file("data-raw/rrds-slide-gate-loss-table.csv",
  package = "culvrt")
rrds.ks.table = read.csv(ks.data)
rrds.ks.table = rrds.ks.table[c("open_fraction", "coefficient")]
usethis::use_data(rrds.ks.table, internal = TRUE, overwrite = TRUE)
