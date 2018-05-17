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
  mutate(temp_diff_2080D = S32_2080D - S1_93_11) #D means that warmer streams respond more to temp changes than cold streams.

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


# Change in streamflow temp
ggplot() +
  theme_bw(base_size =12) +
  geom_sf(data=ss_border, fill=NA, col="black") +
  geom_sf(data=ss_stream_lines, aes(col=temp_diff_2080D)) +
  scale_color_continuous(low="blue", high="red",  name="Temperature\nChange (C)") +
  labs(title="Change in stream temperatures (2070-2099)", x="Longitude",y="Latitude", size=0.5) +
  theme_classic(base_size =12)
  
ggsave("output/stream_temp_change.jpg", width = 5, height = 5)


