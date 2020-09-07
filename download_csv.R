library(robis)

df <- occurrence("Porifera", dropped = "include", absence = FALSE, qcfields = TRUE)
write.csv(df, paste0("porifera_", format(Sys.time(), "%Y%m%d"), ".csv"), row.names = FALSE, na = "")
