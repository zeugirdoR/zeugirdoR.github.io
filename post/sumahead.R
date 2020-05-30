sum_ahead <- function(v,lead=0) {
    if (lead > 0) {
        df <- tibble(V1=v)
        df$V2 <- rollsum(lead(df$V1), lead, align="left", fill = NA) + df$V1
        V3 <- df[['V2']]
        df1 <- df %>% filter(is.na(V2)) %>% mutate(V2=rev(cumsum(rev(V1))))
        V3[is.na(V3)] <- df1$V2
    }
    else V3 <- v
    V3
}
