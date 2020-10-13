deal_hands <- function(deck, players = 1:4) {
  # Must have between 3 and 5 players"
  stopifnot(length(players) >= 3, length(players) <= 5)

  # Rules to ensure that deck is evenly divisible
  # I'm deviating slightly from the official Bicycle rules to make things a bit easier
  if (length(players) == 3) {
    deck <- deck %>%
      filter(!(suit == "diamonds" & value == "2"))
  } else if (length(players) == 5) {
    deck <- deck %>%
      filter(!(suit %in% c("diamonds", "spades") & value == "2"))
  }

  # Now, assign players to cards (or cards to players) and sort hands based
  # on value and suit
  deck %>%
    # shuffle the rows
    slice_sample(n = nrow(deck)) %>%
    mutate(player = rep(players, length.out = n())) %>%
    arrange(player, suit, value)
}


# Check whether the 2 of clubs is available
pick_first <- function(cards) {
  # Check for 2 of clubs
  # 2 of clubs is always selected first; if that's an option it is the first trick
  inner_join(cards,
             tibble(suit = "clubs", value = "2"),
             by = c("suit", "value"))
}

# Check whether there's a card in the selected suit
pick_card_in_suit <- function(cards, sel_suit = NA) {
  # if sel_suit is NA, this will return nothing
  filter(cards, suit == sel_suit) %>%
    slice_sample(n = 1)
}

# Check whether there's a card in another suit
pick_card_out_suit <- function(cards, hearts_avail = F) {
  card_options <- cards

  onlyhearts <- sum(cards$suit != "hearts") > 0

  # If hearts aren't available and there are other suits, filter out the hearts
  if (!onlyhearts | hearts_avail) {
    card_options <- cards %>% filter(suit != "hearts")
  }

  slice_sample(card_options, n = 1)
}

pick_card <- function(cards, sel_suit = NA, hearts_avail = F) {
  play <- pick_first(cards)

  # Try picking a card of the selected suit
  if (nrow(play) == 0) {
    play <- pick_card_in_suit(cards, sel_suit)
  }

  # Otherwise, try picking a card outside of the selected suit
  if (nrow(play) == 0) {
    play <- pick_card_out_suit(cards, hearts_avail)
  }

  return(play)
}

pick_indiv_cards <- function(hand, sel_suit = NA, hearts_avail = F) {
  res <- hand %>%
    nest(hand = -player)

  # I'm writing this without purrr because you don't know it yet
  for (i in 1:nrow(res)) {
    res$hand[[i]] <- pick_card(res$hand[[i]], sel_suit = sel_suit, hearts_avail = hearts_avail)
  }

  res %>%
    unnest(hand)
}


# for this function, we assume deck is going to have a player for each card in the deck
# and that cards which have been played already will have been removed
pick_trick <- function(deck, lead_player = NA, hearts_avail = F) {
  # Check that everyone has a card to play
  stopifnot(nrow(deck) >= length(deck$player))

  # lead player is not determined for first trick - it's person with 2 of clubs
  if (is.na(lead_player)) {
    lead_card <- pick_card(deck, sel_suit = "clubs")
    lead_player <- lead_card$player
  } else {
    lead_card <- deck %>% filter(lead_player == player) %>% pick_card()
  }

  # assess whether hearts can be played based on the lead card
  hearts_avail <- hearts_avail | (lead_card$points > 0)

  # collect the remaining cards
  # technically, we should do this player-by-player, but the only difference
  # is whether hearts can be played or not. Lets hand-wave and make use of
  # the fact that slice_sample() works with groups so that we don't have to
  # specify the order of play...
  trick <- deck %>%
    filter(player != lead_player) %>%
    group_by(player) %>%
    # use pick_indiv_cards here because there are multiple players
    pick_indiv_cards(sel_suit = lead_card$suit, hearts_avail = hearts_avail)

  # combine the cards together
  trick <- bind_rows(lead_card, trick) %>%
    mutate(in_suit = (suit == lead_card$suit)) %>%
    # order so that the first card in the trick is the winner of this hand
    arrange(desc(in_suit), desc(value)) %>%
    # keep track of who has which cards
    mutate(cards_to = player[1]) %>%
    # also keep track of whether hearts can be lead yet
    mutate(hearts_avail = hearts_avail | sum(points) > 0)

  return(trick)
}

play_trick <- function(unplayed, played, lead_player) {
  if (nrow(played) == 0) {
    trick_num <- 1
  } else {
    trick_num <- max(played$trick_num) + 1
  }

  hearts_avail <- sum(played$points) > 0
  current_trick <- pick_trick(unplayed,
                              lead_player = lead_player,
                              hearts_avail = hearts_avail) %>%
    mutate(trick_num = trick_num)

  unplayed <- anti_join(unplayed, current_trick, by = c("suit", "value"))
  played <- bind_rows(played, current_trick)
  lead = unique(current_trick$cards_to)

  return(list(unplayed = unplayed,
              played = played,
              lead_player = lead))
}


card_values <- factor(1:13, labels = c(2:10, "Jack", "Queen", "King", "Ace"))
suit <- c("hearts", "diamonds", "spades", "clubs")
# start with the full deck
deck_of_cards <- tidyr::crossing(suit, value = card_values) %>%
  mutate(points = (suit == "hearts") +
           13*(suit == "spades" & value == "Queen"))

game_of_hearts <- function(players = 1:4) {
  hands <- deal_hands(deck_of_cards, players = players)

  # get total number of tricks:
  ntricks <- hands %>% group_by(player) %>% count() %>% pluck("n") %>% unique()

  res <- as.list(1:(ntricks + 1))
  res[[1]] <- list(unplayed = hands, played = tibble(), lead_player = NA)
  for (trick in 1:ntricks) {
    res[[trick + 1]] <- play_trick(res[[trick]]$unplayed, res[[trick]]$played, res[[trick]]$lead_player)
  }

  return(res$played)
}
