library(tidyverse)
# Simulating Hearts

#' Function to create the deck
#'
#' @examples
#' hearts_deck <- create_deck()
create_deck <- function() {
  # Set up cards
  suits <- c("hearts", "diamonds", "spades", "clubs")
  values <- factor(1:13, labels = c(2:10, "Jack", "Queen", "King", "Ace"))
  deck <- crossing(suit = suits, value = values)

  # Set up points
  deck %>%
    mutate(points = (suit == "hearts") +
             13*(suit == "spades" & value == "Queen")) %>%
    # add a card ID
    mutate(id = 1:n())
}

deck_fix <- function(deck = create_deck(), n_players = 4) {

  # Remove cards from deck to make the game playable by 3 or 5 players
  if (n_players == 4) {
    # do nothing special
  } else if (n_players == 3) {
    deck <- deck %>%
      filter(!(suit == "diamonds" & value == "2"))
  } else if (n_players == 5) {
    deck <- deck %>%
      filter(!(suit %in% c("diamonds", "spades") & value == "2"))
  } else {
    stop("Hearts is playable by between 3 and 5 players")
  }

  deck
}

#' Deal cards to players
#'
#' @examples
#' create_deck() %>%
#' deal_cards(players = c("Amy", "Joe", "Sarah", "Bill"))
deal_cards <- function(deck, players = 1:4) {
  deck %>%
    # Fix the deck
    deck_fix(n_players = length(players)) %>%
    # shuffle the rows
    slice_sample(n = nrow(deck)) %>%
    # Assign cards to players one-by-one
    mutate(player = rep(players, length.out = n())) %>%
    # Sort deck by player, suit, and value
    arrange(player, suit, value)
}

# --- Helper functions - picking single cards ----
# Pick a card for each player given the suit and whether
# hearts have been broken yet
pick_card <- function(deck,
                      this_player = NA,
                      sel_suit = NA,
                      hearts_avail = F) {
  if (!is.na(this_player)) {
    options <- filter(deck, player %in% this_player)
  } else {
    options <- deck
  }

  play <- bind_rows(
    # First, try to pick something in suit
    pick_card_in_suit(options, sel_suit = sel_suit),
    # Then if that fails pick something out of suit
    pick_card_out_suit(options, hearts_avail = hearts_avail)
  )

  # Take the first row - in-suit if it's available, out-of-suit otherwise
  play[1,]
}

# Check whether there's a card in the selected suit
pick_card_in_suit <- function(cards, sel_suit = NA) {
  # if sel_suit is NA, this will return nothing
  filter(cards, suit == sel_suit) %>%
    slice_sample(n = 1)
}

# Check whether there's a card in another suit
pick_card_out_suit <- function(cards, hearts_avail = F) {
  if (is.na(hearts_avail)) stop("hearts_avail cannot be NA")
  card_options <- cards

  nonhearts <- any(cards$suit != "hearts")

  # If hearts aren't available and there are other suits,
  # filter out the hearts
  if (nonhearts & !hearts_avail) {
    card_options <- cards %>% filter(suit != "hearts")
  }

  # print(card_options)
  slice_sample(card_options, n = 1)
}
# -------------------------------------------------------------

#' Evaluate a played trick, adding columns for in_suit, player_win
#'
#' @examples
#' create_deck() %>%
#' deal_cards(1:4) %>%
#' group_by(player) %>%
#' slice_sample(n = 1) %>%
#' ungroup() %>%
#' eval_trick(spec_suit = .$suit[1])
eval_trick <- function(cards, spec_suit, trick_num = NA) {
  cards %>%
    mutate(trick_order = 1:n()) %>%
    mutate(in_suit = suit == spec_suit) %>%
    arrange(desc(in_suit), desc(value)) %>%
    mutate(player_win = .$player[1]) %>%
    arrange(trick_order) %>%
    mutate(trick = trick_num)
}

#' Remove cards from deck that have been played and bind on as a played trick
fix_deck(dealt, trick) {
  dealt %>%
    anti_join(trick, by = c("suit", "value", "player", "id")) %>%
    bind_rows(trick)
}


#' Handle the first hand
#'
#' @examples
#' create_deck() %>%
#' deal_cards(players = c("Amy", "Joe", "Sarah", "Bill")) %>%
#' first_hand()
first_hand <- function(dealt) {
  # Which player has 2 of clubs?
  first_card <- dealt %>%
    filter(suit == "clubs" & value == "2")

  remaining_players <- unique(dealt$player) %>%
    setdiff(first_card$player)

  play <- first_card
  for (i in remaining_players) {
    play <- dealt %>%
      filter(player == i) %>%
      pick_card(sel_suit = "clubs") %>%
      bind_rows(play)
  }
  play <- play %>%
    eval_trick(spec_suit = "clubs",
               trick_num = 1)

  if (!all(dealt$player %in% play$player)) {
    stop("Not all players picked cards?")
  }

  fix_deck(dealt, play)
}

hearts_playable <- function(df) {
  sum(df$points, na.rm = T) > 0
}

#' Play a trick (that isn't the first trick)
#'
#' @examples
#' create_deck() %>%
#' deal_cards(players = c("Amy", "Joe", "Sarah", "Bill")) %>%
#' play_trick() %>%
#' play_trick() %>%
#' filter(!is.na(trick))
play_trick <- function(dealt) {
  if (!"trick" %in% names(dealt)) {
    return(first_hand(dealt))
  }

  # get empty df
  play <- filter(dealt, id < 0)

  # get trick number
  trick_num <- (1 + dealt$trick) %>% max(na.rm = T)

  # get player to lead
  avail <- filter(dealt, is.na(trick))
  last_player <- filter(dealt, trick == max(trick, na.rm = T)) %>%
    pluck("player_win") %>%
    unique()

  # get already played cards and see if hearts can be played
  already_played <- anti_join(dealt, avail, by = c("suit", "value"))
  can_play_hearts <- bind_rows(already_played, play) %>%
    hearts_playable()

  # pick first card
  play <- pick_card(avail,
                    this_player = last_player,
                    sel_suit = NA,
                    hearts_avail = can_play_hearts)

  # pick remaining cards
  remaining_players <- unique(dealt$player) %>%
    setdiff(last_player)
  for (i in remaining_players) {
    can_play_hearts <- bind_rows(already_played, play) %>%
      hearts_playable()
    play <- bind_rows(
      play,
      pick_card(avail, this_player = i,
                sel_suit = play$suit[1],
                hearts_avail = can_play_hearts)
    )
  }

  # evaluate the trick
  play <- play %>%
    eval_trick(spec_suit = play$suit[1], trick_num)

  # remove trick from deck and add it back in as a played trick
  fix_deck(dealt, play)
}


#' This plays a whole game
#' @examples
#' hearts(c("Amy", "Beth", "Cara", "Dawn"))
hearts <- function(players) {
  deck <- create_deck() %>%
    deal_cards(players)

  number_of_rounds <- nrow(deck)/length(players)

  for (i in 1:number_of_rounds) {
    # print(i)
    deck <- play_trick(deck)
    # print(filter(deck, trick == i))
  }

  return(deck)
}
