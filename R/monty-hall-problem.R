#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#' Player Selects Door
#' @description
#' `select_door()` randomly generates a number representing one of three doors.
#' @details
#' This function represents the part of the game where the contestant
#' blindly selects a door to be opened at the end of the game.
#' @param ... no arguments used by this function
#' @return The function returns a number (1, 2, or 3)
#' @examples
#' select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Open Door With Goat
#' @description
#' `open_goat_door` generates a door number that represents a goat
#' @details
#' This function represents the part of the game where the
#' game-show host opens a door with a goat behind it. This
#' door cannot contain a car and cannot be the contestant's pick.
#' @param
#' This function uses arguments game and a.pick in order to
#' open a door that has a goat and is not the result of the
#' select_door() function.
#' @return The function returns a number (1, 2, or 3)
#' @examples
#' open_goat_door(game, a.pick)
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' Stay or Switch doors
#' @description
#' `change_door` generates a door number that is not revealed by the host
#' @details
#' This function represents the part of the game where the host allows
#' the contestant to stay with their initial door pick or switch
#' for a chance at winning a car.
#' @param
#' This function uses arguments stay=T/F, opened.door, and a.pick
#' in order to prevent from selecting the goat door opened by the host
#' and to decide whether or not to use the contestant's initial
#' pick (T or F).
#' @return The function returns a number (1, 2, or 3)
#' @examples
#' change_door(stay=T, opened.door, a.pick) or
#' change_door(stay=F, opened.door, a.pick)
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
   return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine Game Winner
#' @description
#' `determine_winner` returns 'WIN' or 'LOSE' based on game outcome
#' @details
#' This function represents the reveal where the host opens the door
#' containing the contestant's final pick. The contestant WINS if
#' their door has a car and LOSES if it has a goat.
#' @param
#' This function uses the arguments final.pick and game to determine
#' if the contestant's door number represents a car position in the game.
#' @return The function returns "WIN" or "LOSE" 
#' @examples
#' determine_winner(final.pick, game)
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#' Play Whole Game
#' @description
#' `play_game` runs the entire simulation above
#' @details
#' @param ... no parameters
#' @return This function returns the game results (win or lose).
#' @examples
#' play_game()
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Play N Whole Games
#' @description
#' `play_n_games` allows you to run the game simulation for a selected
#' number of times
#' @details
#' Simply enter the amount of games to complete n simulations.
#' @param
#' This function uses the argument n= to determine how many games to run.
#' @return This function returns a datafram of results for n games.
#' @examples
#' play_n_games(n=100)
#' play_n_games(n=5000)
#' @export
play_n_games <- function( n=100 )
{
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1
  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  results.df <- dplyr::bind_rows( results.list )
  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  return( results.df )
}
