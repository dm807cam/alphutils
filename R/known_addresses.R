known_addresses <- function() {
  dt <- data.table::as.data.table(rbind(
    c(
      "MEXC",
      "CEX",
      "1Dr9dDi2ydCvaJ5fBf9TsPSNouuZFdQXa6yrMBZW26cro"
    ),
    c(
      "Bridge",
      "DEX",
      "226T1XspViny5o6Ce1jQR6UCGrDXuq5NBVoCFNufMEWBZ"
    ),
    c(
      "Gate.io",
      "CEX",
      "17R6Ptkz9i1LhiKyMhnitUMkgFygGeeQUFZvRx6GgV8Fc"
    ),
    c(
      "ayin.app",
      "DEX",
      "25ywM8iGxKpZWuGA5z6DXKGcZCXtPBmnbQyJEsjvjjWTy"
    ),
    c(
      "Coinex",
      "CEX",
      "14pnhqYUaGKcJq5kra3ARogumSFFsRoPaHSAkUB2KqoQY"
    ),
    c(
      "Elexium",
      "DEX",
      "27Ub32AhfC9ULKGKGUTdDGU2ehvUN55aLS4oU8nmW3x9M"
    ),
    c(
      "Tradeogre",
      "CEX",
      "1FGF4qXtTWLsZ8ANigFPkpec9g9n5gEvXfiHBRkjKqw9v"
    ),
    c(
      "Alephium Pool",
      "Mining Pool",
      "2A5R8KZQ3rhKYrW7bAS4JTjY9FCFLJg6HjQpqSFZBqACX"
    ),
    c(
      "Coinmetro",
      "CEX",
      "19EeatT7mdZAK36SFRj4zf6Wv98fESGv8CN42YXnQTcFQ"
    )
  ))

  data.table::setnames(dt,
    old = names(dt),
    new = c("name", "type", "address")
  )

  return(dt)
}

genesis_addresses <- function(endpoint = "https://wallet.mainnet.alephium.org", req_timeout = 300) {
  url <- file.path(
    endpoint,
    "blockflow/blocks?fromTs=1231006505000&toTs=1231006506000"
  )

  resp <- api_GET_with_retry(url = url, req_timeout = req_timeout)

  dt <- lapply(resp$blocks, \(block) {
    if (length(block[[1]][["transactions"]])) {
      fixed_outputs <- block[[1]][["transactions"]][[1]]$unsigned$fixedOutputs

      dt <- lapply(fixed_outputs, \(out) {
        data.table::data.table(
          "address" = out$address,
          "amount" = convert_pi_to_alph(out$attoAlphAmount)
        )
      }) |> data.table::rbindlist()

      return(dt)
    }
  }) |> data.table::rbindlist()

  dt[, amount := sum(amount), by = address]

  return(unique(dt))
}
