library(rxaf)

# Replace 'example.xaf' with the path to the .xaf file you want to read
dataset <- read.xaf('example.xaf')

# The imported XAF file can be exported to CSV
write.csv(dataset, "example.csv", row.names = FALSE)
