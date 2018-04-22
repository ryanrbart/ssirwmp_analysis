# Stream Temperature Analysis
# 

source("R/0_utilities.R")

ls(ss_stream_lines)

# ---------------------------------------------------------------------
# Processing

ss_stream_lines <- ss_stream_lines %>% 
  mutate(elev_category=cut(ELEV, breaks=c(-Inf,1200,2400,Inf), labels=c("low","middle","high")))

ss_stream_lines <- ss_stream_lines %>% 
  mutate(temp_diff_2080 = S31_2080 - S1_93_11)

ss_stream_lines <- ss_stream_lines %>% 
  mutate(temp_diff_2080D = S32_2080D - S1_93_11)

summary(ss_stream_lines$temp_diff_2080)
summary(ss_stream_lines$temp_diff_2080D)


# ---------------------------------------------------------------------
# Summarizing

# Average historical temperature over SS
ss_stream_temp_avg <- ss_stream_lines %>% 
  summarize(average_temp = mean(S1_93_11))

# Average historical temperature by elevation
ss_stream_temp_elev_avg <- ss_stream_lines %>% 
  group_by(elev_category) %>% 
  summarize(average_temp = mean(S1_93_11))



# ---------------------------------------------------------------------
# Plotting

ggplot() +
  geom_sf(data=ss_border, fill=NA, col="black") +
  geom_sf(data=ss_stream_lines, fill="red", col="black")


ggplot() +
  geom_sf(data=ss_border, fill=NA, col="black") +
  geom_sf(data=ss_stream_lines, aes(col=S41_2080DM))



