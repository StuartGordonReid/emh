

RandomnessResult <- function(name, type, statistic, p.value, z.score, non.random, frequency = 1) {
  result <- list(
    frequency = frequency,
    type = type,
    name = name,
    statistic = statistic,
    p.value = p.value,
    z.score = z.score,
    non.random = non.random
  )
  class(result) <- append(class(result),"RandomnessResult")
  return(result)
}
