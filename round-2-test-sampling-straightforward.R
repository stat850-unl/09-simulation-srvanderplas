# Alternately you could type the function steps out for each possible suit
# diamonds
replicate(50, pick_card(hands$hand[[3]], "diamonds"), simplify = F) %>%
  bind_rows() %>%
  group_by(suit, value) %>%
  count()

replicate(50, pick_card(hands$hand[[4]], "diamonds"), simplify = F) %>%
  bind_rows() %>%
  group_by(suit, value) %>%
  count()

replicate(50, pick_card(hands$hand[[1]], "diamonds"), simplify = F) %>%
  bind_rows() %>%
  group_by(suit, value) %>%
  count()

# clubs
replicate(50, pick_card(hands$hand[[3]], "clubs"), simplify = F) %>%
  bind_rows() %>%
  group_by(suit, value) %>%
  count()

replicate(50, pick_card(hands$hand[[4]], "clubs"), simplify = F) %>%
  bind_rows() %>%
  group_by(suit, value) %>%
  count()

replicate(50, pick_card(hands$hand[[1]], "clubs"), simplify = F) %>%
  bind_rows() %>%
  group_by(suit, value) %>%
  count()

# spades
replicate(50, pick_card(hands$hand[[3]], "spades"), simplify = F) %>%
  bind_rows() %>%
  group_by(suit, value) %>%
  count()

replicate(50, pick_card(hands$hand[[4]], "spades"), simplify = F) %>%
  bind_rows() %>%
  group_by(suit, value) %>%
  count()

replicate(50, pick_card(hands$hand[[1]], "spades"), simplify = F) %>%
  bind_rows() %>%
  group_by(suit, value) %>%
  count()
