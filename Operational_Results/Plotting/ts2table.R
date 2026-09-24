# Function to convert time series data (indexed) to table format.
ts2table <- function(x){
  months <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
  years <- unique(substr(index(x), 1, 4))
  table <- data.frame(matrix(rep(NA, length(years) * 12), nrow=length(years), ncol=12), row.names=years)
  names(table) <- months
  for (d in 1:length(x)){
    i <- which(rownames(table)==substr(index(x)[d], 1, 4))
    j <- which(names(table)==substr(index(x)[d], 6, 7))
    table[i, j] <- x[d]
  }
  table
}