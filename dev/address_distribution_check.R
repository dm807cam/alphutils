pkgload::load_all()
library(ggplot2)
library(ggpubr)

files <- list.files(path = "../../../Downloads/alephium_richlist/", full.names = TRUE)

cols <- c("address", "balance", "datetime")
dt <- lapply(files, \(file) {
  data.table::fread(file)[, ..cols]
}) |> data.table::rbindlist()

# Remove CEX + DEX + Bridge
dt <- dt[!address %in% known_addresses()$address, ]

# Remove genesis
dt <- dt[!address %in% genesis_addresses()$address, ]

balance_seq <- seq_len(max(dt$balance))
log_indices <- round(exp(seq(log(1), log(length(balance_seq)), length.out = 20)))
balance_seq_breaks <- balance_seq[log_indices]

breaks <- c(balance_seq_breaks, Inf)
dt[, balance_range := cut(balance, breaks = breaks, include.lowest = TRUE, ordered_result = TRUE)]

dt_summary <- lapply(unique(dt$datetime), \(time) {
  total_supply <- sum(dt[datetime == time, balance])

  summary_data <- dt[datetime == time, .(
    n_wallets = .N,
    percent_of_supply = sum(balance) / total_supply * 100
  ), by = c("balance_range", "datetime")]

  summary_data[, cumulative_supply := cumsum(percent_of_supply)]
  return(summary_data)
}) |> data.table::rbindlist()

p1 <- ggplot(dt_summary[datetime == max(datetime)], aes(x = balance_range, y = percent_of_supply, fill = balance_range)) +
  geom_col() +
  #geom_text(aes(label = paste0(round(percent_of_supply, 2), "%")), vjust = -0.5) +
  labs(
    title = "Distribution of Wallets by Balance",
    x = "Balance Range",
    y = "Percent of Total Supply"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  scale_fill_viridis_d()

p2 <- ggplot(dt_summary, aes(x = n_wallets, y = cumulative_supply, colour = as.factor(datetime))) +
  geom_line() +
  #geom_smooth(span = 1, size = 1, se = F) +
  scale_x_continuous(transform = "log10") +
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
  labs(
    title = "Cumulative Distribution of Supply",
    x = "Number of Wallets (log10)",
    y = "Cumulative Percentage of Supply"
  ) +
  theme_minimal() + theme(legend.position = "none")

p3 <- ggplot(dt_summary,
             aes(x = datetime, y = cumulative_supply, fill = balance_range)) +
  geom_area(position = "identity") +
  theme(legend.position = "none") +
  theme_minimal() +
  scale_fill_viridis_d()

g1 <- ggarrange(p1, p2, ncol = 2, align = "hv")
ggarrange(p3, g1, ncol = 1, align = "hv", legend = "none")



