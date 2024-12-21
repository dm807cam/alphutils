pkgload::load_all()

utc_time <- utc_time()
dt <- api_address_richlist(min_balance = 1L)
dt[, datetime := utc_time]

path <- fs::path(fs::path_home(),
                 "alephium_richlist")

if(!fs::dir_exists(path)) {
  fs::dir_create(path)
}

file_path = fs::path(path,
                     as.numeric(utc_time),
                     ext = "csv")

write.csv(dt, file = file_path)
