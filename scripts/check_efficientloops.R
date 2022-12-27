Counter <- 0
Result <- list(NULL)
Size <- 1
df = data.frame(col1 = c(1:3), col2 = c(1:3))
df = st_as_sf(df, coords = c("col1", "col2"))

AddItemDoubling <- function(item)
{
  if( .GlobalEnv$Counter == .GlobalEnv$Size )
  {
    length(.GlobalEnv$Result) <- .GlobalEnv$Size <- .GlobalEnv$Size * 2
  }
  
  .GlobalEnv$Counter <- .GlobalEnv$Counter + 1
  
  .GlobalEnv$Result[[.GlobalEnv$Counter]] <- df
}

system.time(for(i in seq_len(100)) AddItemDoubling(i))
