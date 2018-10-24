#bei MACuser=F: wenn man sich das gewinnerfeld anzeigen lässt und dann ins feld klickt, wird das spiel beendet.
#besse wäre auf die siegermeldung zurückzuschalten.

#' Main Function
#'
#' Function to play FourScores
#' @param rows integer: how many rows shall the playing field have?
#' @param columns integer: how many columns shall the playing field have?
#' @param AI boolean: play against AI?
#' @param AIstrength integer: strength of the AI - number of moves the AI will simulate?
#' @param AIplayernumber integer: 0 or 1: should the AI be player 1 or player 2?
#' @param MACuser boolean: on some non-mac computers this can be set to \code{FALSE} to have mouse-functionality in the graphics device.
#' @param PlayerNames array of characters: the players' names.
#' @param getnewnames boolean: should new names be asked for?
#' @param PlayerColors vector of characters: the players' colors.
#' @param getnewcolors boolean: should new colors be asked for?
#' @examples
#' \dontrun{
#' FourScores(AI = T, AIstrength = 10, MACuser = T, getnewnames = F, getnewcolors = F)
#' }
#' @export
FourScores <- function(rows = 6, columns = 7, AI = TRUE, AIstrength = rows * columns, AIplayernumber = 1, MACuser = TRUE,
                        PlayerNames = c("AI", "Human"), getnewnames = FALSE, PlayerColors = c("green", "blue"), getnewcolors = FALSE){


  if(AI & AIplayernumber == 2){
    PlayerNames <- c("Human", "AI")
  }

  # help-function which gives an optical help for the player to see which column is selected if he releases the mouse button
  # currently it cannot be defined outside, as it refers to field etc, which is not allowed as input in getgraphicsevent
  preview <- function(buttons, x, y) {

    #the x-axis-value of the mouse
    plx <- graphics::grconvertX(x, "ndc", "user")

    #rounds the x-axis-value to a natural number
    column <- round(plx)

    #getting the heigth of the field and add 0.5 Points, so that the preview is above and not over the actual field
    displayheigth <- nrow(field) + 0.5

    #variables that has to be used in every plotting, so that the old grey previews "disapear" (are overplotted by white previews)
    xvalues <- 1:ncol(field)
    yvalues <- rep(displayheigth, times = ncol(field))

    color <- rep("white", times = ncol(field))

    ##where the mouse is, the color shall be grey
    color[column] <- "grey68"

    #plot a grey preview
    graphics::points(x = xvalues, y = yvalues, cex = 1.5, pch = PlayerPch[player], lty = 3, col = color)
  }

  #Vector for the symbols which destinguishes the player's stones (in addition to the color - which is unwanted in the preview)
  PlayerPch <- c("O", "X")

  #entering the names
  if(getnewnames == TRUE){
    PlayerNames <- getPlayerNames(PlayerNames = PlayerNames, MACuser = MACuser)
  }

  #chosing a color
  if(getnewcolors == TRUE){
    PlayerColors <- getColors(PlayerNames = PlayerNames, PlayerColors = PlayerColors, MACuser = MACuser)
  }

  #generating the playing field
  field <- FieldGeneration(rows = rows, columns = columns)

  #initializing a boolean variable which indicates if a player has won
  won <- FALSE

  #initializing a boolean variable which indicates if it is a draw
  draw <- FALSE

  #initializing a variable to "keep in mind" which player has to move
  i <- 1

  #while nobody has won, the game goes on and a next round is started
  while(!won && !draw){

    #increasing the number of rounds
    i <- i + 1

    #Variable for remebering whether player1 oder player2 has to move
    player <- (i %% 2) + 1

    #initalizing a boolean variable which indicates wheter a valid input is given by the player
    correct <- FALSE

    #while no valid input is given, the player is said to choose a valid column
    while(!correct){

      #...if a correct input is given, the field is plotted
      FieldPlot(field = field, message = paste(PlayerNames[player], " (", PlayerPch[player], "): it's your turn", sep = ""),
                PlayerColors = PlayerColors)

      #if there is an AI, it is player1 and makes a move
      if(AI && player == AIplayernumber){
        column <- AImove(field = field, AIstrength = AIstrength, AIplayernumber = AIplayernumber)
        #get an input for the collum by using 'getGraphicsEvent()'
      }else{
        msg <- paste(PlayerNames[player], " (", PlayerPch[player], "): ", "select column: ", sep = "")
        if(MACuser){
          column <- as.integer(readline(msg))
        }else{
          column <- as.integer(grDevices::getGraphicsEvent(
            msg,    #appears in the console, can be removed when the text appears in the plot
            onKeybd = typing,
            onMouseUp = clicking,
            onMouseMove = preview))
        }
      }
      #checking by a help-function whether the chosen cullumn an the current field fits together
      correct <- FieldCorrect(column = column, field = field)
    }

    #generating a new field by adding to the old field the information about the selected column and which player has chosen this column

    field <- NewField(field = field, column = column, player = player)

    #checken by a help-function whether the current player has won by this move
    won <- FieldWinCheck(field, player)

    #checken if there is a draw
    draw <- !won && sum(is.na(field)) == 0
  }

  graphics::plot(1, axes = FALSE, ann = FALSE, col = "white")
  if(won){
    #giving a message to the winner
    msgLong <- paste(PlayerNames[player], " (", PlayerPch[player], ") has won!", sep = "")
    msgShort <- paste(PlayerNames[player], " has won!", sep = "")
    graphics::text(x = 1, y = 1, msgShort, cex = 3, col = "lightgoldenrod3")
  }else{
    msgShort <- "It's a draw"
    msgLong <- msgShort
    graphics::text(x = 1, y = 1, msgShort, cex = 3, col = "slategray4")
  }

  # Add buttions to the plot, and ask the player(s) to show the field, to play again, or to exit the game.
  fbuttons(field = field, justsub = FALSE, MACuser = MACuser,
           message = msgLong,
           rows = rows, columns = columns, AI = AI, AIstrength = AIstrength, AIplayernumber = AIplayernumber,
           PlayerNames = PlayerNames,
           PlayerColors = PlayerColors)
}

#' Field buttons
#'
#' A function to show buttons, letting the player(s) decide what to do: show the winning field, play again or exit.
#' @param field matrix: the field.
#' @param justsub boolean: should only be a subtitle plotted (below the winning field)?
#' @param message character: a message to be plotted.
#' @inheritParams FourScores
fbuttons <- function(field, justsub, message,
                     MACuser,
                     rows, columns,
                     AI, AIstrength, AIplayernumber,
                     PlayerNames,
                     PlayerColors){
  if(justsub){
    graphics::title(sub = "[a]gain | [f]ield | [e]xit")
  }else{
    graphics::rect(0.6, 0.6, 0.9, 0.8)
    graphics::text(x = 0.75, y = 0.7, "[a]gain", cex = 3, col = "green")

    graphics::rect(0.9, 0.6, 1.2, 0.8)
    graphics::text(x = 1.05, y = 0.7, "[f]ield", cex = 3, col = "blue")

    graphics::rect(1.2, 0.6, 1.4, 0.8)
    graphics::text(x = 1.3, y = 0.7, "[e]xit", cex = 3, col = "red")
  }


  x <- 0
  y <- 0
  whattodoMAC <- ""
  if(MACuser){
    whattodoMAC <- tolower(readline("[a]gain, show [f]ield, [e]xit? "))
  }else{
    whattodo <- grDevices::getGraphicsEvent(
      "[a]gain, show [f]ield, [e]xit? ",    #appears in the console, can be removed when the text appears in the plot
      onKeybd = typing,
      onMouseUp = clickingXY)

    if(is.character(whattodo)){
      whattodoMAC <- whattodo
    }else{
      x <- whattodo[1]
      y <- whattodo[2]
    }
  }
  #again
  if(((x >= 0.6 && x <= 0.9)&&(y >= 0.6 && y <= 0.8)) || whattodoMAC == "a"){
    FourScores(rows = rows, columns = columns, AI = AI, AIstrength = AIstrength, AIplayernumber = AIplayernumber,
               MACuser = MACuser, PlayerNames = PlayerNames,
               getnewnames = FALSE, PlayerColors = PlayerColors, getnewcolors = FALSE)
  }
  #field
  if(((x >= 0.9 && x <= 1.2)&&(y >= 0.6 && y <= 0.8)) || whattodoMAC == "f"){
    FieldPlot(field = field, message = message, PlayerColors = PlayerColors)

    #plot buttons
    fbuttons(field = field, justsub = TRUE,
             message = message,
             MACuser = MACuser,
             rows = rows, columns = columns, AI = AI, AIstrength = AIstrength, AIplayernumber = AIplayernumber,
             PlayerNames = PlayerNames,
             PlayerColors = PlayerColors)

  }
  #exit
  if(((x >= 1.2 && x <= 1.4)&&(y >= 0.6 && y <= 0.8)) || whattodoMAC == "e"){
    plotlogo()
    grDevices::dev.off()
  }
}


#' A function
#'
#' A function to get some colors
#' @inheritParams FourScores
#' @return a vector with the updated player colors
getColors <- function(PlayerNames, PlayerColors, MACuser){

  #defining the choosable colors
  MYcolors <- c("forestgreen","aquamarine", "blue ", "black",
                "chartreuse", "lightcyan4", "darkorchid1","royalblue4",
                "bisque2", "lightgoldenrod1","deeppink", "red4",
                "yellow2","orange1", "red", "saddlebrown")

  MYcolors9 <- c("darkorchid1", "royalblue4", "blue",
                 "deeppink", "lightgoldenrod1", "aquamarine",
                 "tomato3", "saddlebrown", "chartreuse")

  #get the colors for both players
  for(k in 1:length(PlayerNames)){
    #define a promt for the players to choose a color
    msg <- paste(PlayerNames[k], ", please choose a color: ", sep = "")

    #Plotting the 9 colors in a 3x3 mosaic
    graphics::plot(1:4, axes = FALSE, ann = FALSE, col = "white")
    graphics::title("choose a color")
    graphics:: mtext(msg)
    for(i in 1:3){
      for(j in 1:3){
        graphics::rect(0 + i, 0 + j, 1 + i, 1 + j, col = MYcolors9[(i - 1)*3 + j], lwd = 2)
        graphics::text(x = 0.5 + i, y = 0.5 + j, (i - 1)*3 + j, col = "white", cex = 1.5)
      }
    }

    #Extract a choice
    PlayerColors[k] <- "white"
    while(PlayerColors[k] == "white"){
      if(MACuser){
        ColorNumber <- as.integer(readline(msg))

      }else{
        xy <- as.integer(grDevices::getGraphicsEvent(
          msg,    #appears in the console, can be removed when the text appears in the plot
          onMouseUp = clickingXY,
          onKeybd = typing))

        if(length(xy) == 1 && xy %in% 1:9){
          ColorNumber <- (xy)

        }else{
          iwouldbe <- xy[1]
          jwouldbe <- xy[2]
          ColorNumber <- (iwouldbe - 1)*3 + jwouldbe

        }
      }

      PlayerColors[k] <- MYcolors9[ColorNumber]

      ###not done jet: prevent a color to be chosen twice.
      # Currently only white is shown, with the possibility for the next player to choose white...

    }
    MYcolors9[ColorNumber] <- "white"
  }

  #returning the Colors
  return(PlayerColors)
}

#' Get player names
#'
#' help-function which gets and returns the players' names
#' @inheritParams FourScores
#' @return a vector with the player names
getPlayerNames <- function(PlayerNames, MACuser){
  for(i in 1:length(PlayerNames)){
    msg <- paste(PlayerNames[i], ", please enter your name: ", sep = "")
    if(MACuser){
      SingleLetters <- readline(msg)
    }else{


      graphics::plot(1, axes = FALSE, ann = FALSE, col = "white")
      graphics::mtext(msg)

      #initializing a variable needed for the generation of the names
      SingleLetters <- ""

      #boolean variable for checking if enter was given
      enter <- FALSE
      while(!enter){

        #getting a single letter, bye hitting a key on the keyboard
        tmp <- grDevices::getGraphicsEvent(onKeybd = typing)

        #if the player presses [Enter] (encoded by "ctrl-J")...
        if(tmp == "ctrl-J"){

          #... the boolean gets true
          enter = TRUE

          #im the case, the player presses [Backspace] (encoded by "ctrl-H")...
        }else if(tmp == "ctrl-H"){

          #... the last saved single letter, is removed from the singleletters
          SingleLetters <- substr(SingleLetters, 1, nchar(SingleLetters) -1)

          #in all other cases... ###INCLUDING [PageUp], [Home] etc. SO SHOULD USE tmp %in% letters()
        }else{

          #... the temporary variable is pasted onto the existing singleletters
          SingleLetters <- paste(SingleLetters, tmp, sep = "")
        }

        #"empty" plot (white dot on white background)
        graphics::plot(1, axes = FALSE, ann = FALSE, col = "white")

        #Instruction to the player
        graphics::mtext(paste(PlayerNames[i], ", please enter your name:", sep = ""))

        #displaying the progress (that means the current -unconfirmed- singleletters)
        graphics::text(x = 1, y = 1, SingleLetters)
      }
    }

    #overwriting the default names, by the input of the players
    PlayerNames[i] <- SingleLetters
  }

  #returning the names
  return(PlayerNames)
}

#' field generation
#'
#' help-function which generates the playing-field
#' @inheritParams FourScores
#' @return an empty matrix with rows and columns
FieldGeneration <- function(rows, columns){

  #the (at the generation empty) matrix will contain e.g. on line 3 column 5 the number 2,
  #if player 2 was the one how placed the third time a stone in column 3
  field <- matrix(nrow = rows, ncol = columns)
  return(field)
}

#' Generate a new field
#'
#' help-function which "throws" the stone into the field and returns the new field
#' @param field matrix: the playing field.
#' @param column integer: the column chosen by the current player.
#' @param player integer: the current player.
#' @return The updated field matrix.
NewField <- function(field, column, player){

  #the number of the row, where the stone "lands" in the selected column,
  #is equal to the number of free rows in that coullum (when the highest row ist labeled with "1", the second highest with "2" and so on
  roww <- sum(is.na(field[, column]))

  #if there is at least one free row
  if(roww >= 1){

    #the stone with the number of the player on it,
    #"falls" in the selected column on the calculated row
    field[roww, column] <- player
  }

  return(field)
}


#' Return a key
#'
#' help-function which returns, the key on the keyboard which is being typed
#' @param key a keyboard input.
#' @return the key pressed.
typing <- function(key){
  return(key)
}

#' check input
#'
#' a function to check the mouse click input by the user
#' @param buttons the mouse buttons input.
#' @param x the x-value of the mouse button.
#' @param y the y-value of the mouse button.
#' @return a Vector of the x and y coordinates of the mouse click
clickingXY <- function(buttons, x, y) {

  #the x-axis-value of the Mouse
  plx <- graphics::grconvertX(x, from = "ndc", to = "user")

  #the y-axis-value of the Mouse
  ply <- graphics::grconvertY(y, from = "ndc", to = "user")

  #rounds the x-axis-value to a natural number and returns it
  return(c(plx, ply))
}

#' a function
#'
#' help-function which return the x-axis-value of the mouse when releasing the mouse button.
#' @inheritParams clickingXY
#' @return a rounded value for the x-coordinate
clicking <- function(buttons, x, y) {

  #the x-axis-value of the Mouse
  plx <- graphics::grconvertX(x, from = "ndc", to = "user")

  #rounds the x-axis-value to a natural number and returns it
  return(round(plx))
}

#' resample
#'
#' resampling function
#' @param x a vector
#' @param ... other parameters
#' @references Help function from \code{?sample} to overcome the "sample(ret, size = 1)" problem for length(ret) == 1
#' @return a vector
resample <- function(x, ...) x[sample.int(length(x), ...)]

#' Move of AI
#'
#' Help-Function for an AI
#' @param field matrix: the playing field
#' @inheritParams FourScores
#' @return the selected row
AImove <- function(field, AIstrength, AIplayernumber){

  field0 <- field

  freecolumns0 <- which(apply(apply(field0, 2, is.na), 2, any))

  #matrix with 4 columns and so many rows as there are free columns on the field.
  # This matrix will be used to evaluate each possible column as a move.
  allresults <- matrix(0, nrow = length(freecolumns0), ncol = 4)
  #background: I want to have evaluated, how each column performs if it is chosed

  #the AI goes through each free colulmn and "tries to win"
  for(j in 1:length(freecolumns0)){

    #a vector for the results. That means: the number of...
    #    ...     winnning, losing, draw, moves
    results <- c(       0,      0,    0,    0)


    playertmp <- AIplayernumber

    columntmp <- freecolumns0[j]

    field1 <- NewField(field = field0, column = columntmp, player = playertmp)
    #check, if the AI has won by this move
    wonAI <- FieldWinCheck(field1, playertmp)
    drawAI <- !wonAI && sum(is.na(field1)) == 0

    freecolumns1 <- which(apply(apply(field1, 2, is.na), 2, any))

    wonHuman <- FALSE
    drawHuman <- FALSE
    #check if human can win the move after the AI
    playertmp <- (AIplayernumber %% 2 ) + 1
      #if AI is player 1, Human is 2; if AI is player2, Human is player1.
    for(n in 1:length(freecolumns1)){
      columntmp <- freecolumns1[n]
      field2 <- NewField(field = field1, column = columntmp, player = playertmp)
      wonHuman <- FieldWinCheck(field2, playertmp) | wonHuman # logical OR needed to detect any winning column.
      drawHuman <- !wonHuman && sum(is.na(field2)) == 0 | drawHuman
    }


    #now, if after the first AI move and the following (hypothetical) Human move, no one has won,
    #make AIstrength number of games (by default 42 (7*6)) for each free column, where the Human player has to move next.
    for(dummy in 1:AIstrength){

      fieldtmp <- field1

      wontmp  <- wonAI  | wonHuman
      drawtmp <- drawAI | drawHuman

      #getting the choosable columns (so often they can be chosen)
      tmp <- rep(1:ncol(field), times = apply(apply(fieldtmp, 2, is.na), 2, sum))

      #permutate these columns and receive a random game (which might end earlier because of a player winning)
      samp <- sample(tmp, size = length(tmp), replace = FALSE)

      #Counter for the number of rounds.
      m <- 0

      while(!(wontmp | drawtmp)){
        #increasing the number of rounds
        m <- m + 1

        #Variable indicating whether player1 oder player2 has to move
        # In the first cycle, playertmp has be be the number of the human
        # (which is 2 if the AI is player1; and is 1 if the AI is player2)
        # (which is achieved if the left part of the modulo is odd; resp. if it is even)
        playertmp <- ((m + AIplayernumber + 1) %% 2) + 1
        #get a column from the sample sequence of moves.
        columntmp <- samp[m]

        fieldtmp <- NewField(field = fieldtmp, column = columntmp, player = playertmp)

        #checking by a help-function whether the current player has won by this move
        wontmp <- FieldWinCheck(fieldtmp, playertmp)

        #checking if there is a draw
        drawtmp <- !wontmp && sum(is.na(fieldtmp)) == 0
      }

      #after having a winner (or a draw) store the results

      #the number of moves
      results[4] <- results[4] + m #average number of moves
      if(drawtmp){
        results[3] <- results[3] + 1
      }else{
        #if the AI has won, increase the number of winnings (first element);
        #if the human has won, increase the number of losings (second element);
        # four possibilities:
        # AI won, AI = player1, in this case playertmp = 1 (increase element 1)
        # AI won, AI = player2;              playertmp = 2 (increase element 1)
        # AI lost; AI = player1;             playertmp = 2 (increase element 2)
        # AI lost; AI = player2;             playertmp = 1 (increase element 2)
        results[2 - as.numeric(playertmp == AIplayernumber)] <- results[2 - as.numeric(playertmp == AIplayernumber)] + 1
      }
    }

    #if AI can win without sampling, store it
    if(wonAI){
      allresults[j, ] <- c(AIstrength + 1, 0, 0, 1)
    }else{
      #if Human can win without sampling, store it
      if(wonHuman){
        allresults[j, ] <- c(0, AIstrength + 1, 0, 2)
        #in all other cases, save the sampling results
        #(how often has the AI won, how often Human, or has it been a draw, and how many steps did it take
      }else{
        allresults[j, ] <- results #absolute frequencies
      }
    }
  }

  #maximin-rule

  #all with the minimal lost frequencie
  ret <- which(allresults[, 2] == min(allresults[, 2]))

  #the remaining with the maximal win frequencie
  ret <- ret[which(allresults[ret, 1] == max(allresults[ret, 1]))]

  #the remaining with the less average moves
  ret <- ret[which(allresults[ret, 4] == min(allresults[ret, 4]))]

  #make sure, that only one collum ist returned

  ret <- resample(ret, size = 1)

  return(freecolumns0[ret])
}


#' plot the field
#'
#' a major-function which plots the current field, and if given a hint, which player has won
#' @param field matrix: the playing field
#' @param message character: a message to be plotted.
#' @inheritParams FourScores
FieldPlot <- function(field, message,
                      PlayerColors){

  #vector of the colors
  color <- PlayerColors

  #vector of the symbols (seen in the title obove the field)
  PlayerPch <- c("O", "X")

  graphics::par(las = 1)

  #plotting the field, dependent on the number of rows and columns
  rows <- nrow(field)
  columns <- ncol(field)
  graphics::plot(c(1 - 0.2, ncol(field) + 0.2), c(1 - 0.2, nrow(field) + 0.5), type = "n", xlab = "", ylab = "",
                 xaxt = "n",
                 main = message)

  graphics::axis(1, at = 1:columns, labels = 1:columns)
  graphics::abline(h = 1:rows + 0.5, v = 1:columns + 0.5)

  #plotting the stones of each row into the field
  for(i in 1:nrow(field)){
    #PlayerColors
    #on the positions x=1,2,..7 and y=7,7,...7, than 6,6,...6
    graphics::points(x = 1:ncol(field), y = rep(nrow(field) - i + 1, times = ncol(field)),
           col = color[field[i, ]],   #dependent on which player has placed which stone, their color is picked from the color-vector
           cex = 2, pch = PlayerPch[field[i, ]], lwd = 2)  #and which symbol they refer to
  }

}

#' Is the field correct?
#'
#' help-function that checks whether the field is correct
#' @param field matrix: the playing field.
#' @param column integer: the column chosen by the current player
#' @return a boolean (\code{TRUE} if the given column would be a valid move for the field given).
FieldCorrect <- function(column, field){

  #turning warning messages off
  options(warn = -1)


  #transforming a typed "7" into a 7
  column <- as.integer(column)
  roww <- 0
  RangeCorrect <- (column >= 1 && column <= ncol(field))
  if(!is.na(RangeCorrect) && RangeCorrect){
    roww <- sum(is.na(field[, column]))
  }

  #turning warning messages on again
  options(warn = 0)
  return(roww >= 1 && RangeCorrect)
}


#' check for a winner
#'
#' help-function that checks whether (at least) one of the four possibilities of winning is given
#' @param field matrix: the playing field.
#' @param player integer: the current player.
#' @return a boolean whether the player has won the match or not
FieldWinCheck <- function(field, player){

  #initializing boolean variable that will get TRUE if (at least) one of the four possibilities of winning is given
  won <- FALSE

  #check on four in a vertical row
  for(j in 1:ncol(field)){
    for(i in 1:(nrow(field) - 3)){
      if(sum(field[i:(i+3), j] == player, na.rm = TRUE) >= 4){
        won <- TRUE
      }
    }
  }

  #check on four in a horizontal row
  for(i in 1:nrow(field)){
    for(j in 1:(ncol(field) - 3)){
      if(sum(field[i, j:(j+3)] == player, na.rm = TRUE) >= 4){
        won <- TRUE
      }
    }
  }

  #check on four in a diagonal row (slope = +1)
  for(i in 1:(nrow(field) - 3)){
    for(j in 1:(ncol(field) - 3)){
      if(sum(field[i:(i+3), j:(j+3)][c(4, 7, 10, 13)] == player, na.rm = TRUE) >= 4){
        won <- TRUE
      }
    }
  }

  #check on four in a diagonal row (slope = -1)
  for(i in 1:(nrow(field)-3)){
    for(j in 1:(ncol(field) - 3)){
      if(sum(diag(field[i:(i+3), j:(j+3)]) == player, na.rm = TRUE) >= 4){
        won <- TRUE
      }
    }
  }

  return(won)
}

#' logo painter
#'
#' a general help function to plot
#' @param numberMatrix a matrix with different integers showing which color to pick from the \code{colorArray}.
#' @param colorArray a character array with different names of colors to be used by the painter.
painter <- function(numberMatrix, colorArray){
  numberMatrix <- numberMatrix + 1#so, that matrix_ij = 0 does not throw an error for colorArray[0]

  graphics::plot(1, axes = FALSE, ann = FALSE, col = "white",
       xlim = c(0, ncol(numberMatrix)),
       ylim = c(0, nrow(numberMatrix)))

  for(i in 1:nrow(numberMatrix)){
    for(j in 1:ncol(numberMatrix))
      graphics::rect(j - 1, nrow(numberMatrix) - i,
           j   , nrow(numberMatrix) - i + 1,
           col = colorArray[numberMatrix[i, j]],
           lwd = 0, border = NA)
  }

}


#' plot logo
#'
#' plot the "different purpose" logo
plotlogo <- function(){
  myd <- matrix(c(0,0,1,
                  0,0,1,
                  0,1,1,
                  1,0,1,
                  0,1,1,
                  0,0,0,
                  0,0,0), ncol = 3, byrow = T)

  mye <- matrix(c(0,0,0,
                  0,1,0,
                  1,0,1,
                  1,1,0,
                  0,1,1,
                  0,0,0,
                  0,0,0), ncol = 3, byrow = T)



  myf <- matrix(c(0,0,1,
                  0,1,0,
                  0,1,1,
                  0,1,0,
                  0,1,0,
                  0,1,0,
                  0,0,0), ncol = 3, byrow = T)

  myi <- matrix(c(0,1,0,
                  0,0,0,
                  0,1,0,
                  0,1,0,
                  0,1,0,
                  0,0,0,
                  0,0,0), ncol = 3, byrow = T)



  myn <- matrix(c(0,0,0,
                  1,1,0,
                  1,0,1,
                  1,0,1,
                  1,0,1,
                  0,0,0,
                  0,0,0), ncol = 3, byrow = T)

  myo <- matrix(c(0,0,0,
                  0,0,0,
                  0,1,0,
                  1,0,1,
                  0,1,0,
                  0,0,0,
                  0,0,0), ncol = 3, byrow = T)

  myp <- matrix(c(0,0,0,
                  0,0,0,
                  1,1,0,
                  1,0,1,
                  1,1,0,
                  1,0,0,
                  1,0,0), ncol = 3, byrow = T)

  myr <- matrix(c(0,0,0,
                  0,1,0,
                  1,0,1,
                  1,0,0,
                  1,0,0,
                  0,0,0,
                  0,0,0), ncol = 3, byrow = T)


  mys <- matrix(c(0,1,1,
                  1,0,0,
                  0,1,0,
                  0,0,1,
                  1,1,0,
                  0,0,0,
                  0,0,0), ncol = 3, byrow = T)

  myt <- matrix(c(0,1,0,
                  1,1,1,
                  0,1,0,
                  0,1,0,
                  0,0,1,
                  0,0,0,
                  0,0,0), ncol = 3, byrow = T)



  myu <- matrix(c(0,0,0,
                  1,0,1,
                  1,0,1,
                  1,0,1,
                  0,1,0,
                  0,0,0,
                  0,0,0), ncol = 3, byrow = T)


  myspace <- matrix(c(0,0,0,
                      0,0,0,
                      0,0,0,
                      0,0,0,
                      0,0,0,
                      0,0,0,
                      0,0,0), ncol = 3, byrow = T)


  differentpurpose <- cbind(myd*1, myi*3, myf*4, myf*5, mye*4, myr*3,
                            mye*1, myn*3, myt*4,
                            myspace,
                            myp*5, myu*4, myr*3, myp*1, myo*3, mys*4, mye*5)


  teil1 <- matrix(c(
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,2,2,2,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,2,2,2,2,2,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,2,2,2,2,2,2,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,2,2,2,0,0,0,2,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,2,2,2,0,0,0,2,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,2,2,2,0,0,0,2,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,2,2,2,2,2,2,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,2,2,2,2,2,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,2,2,2,2,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    ncol = ncol(differentpurpose), byrow = TRUE)

  LOGO <- matrix(c(t(teil1), t(differentpurpose)),
                 ncol = ncol(differentpurpose), byrow = TRUE)

  LOGO1 <- LOGO[nrow(LOGO):1,]
  LOGO2 <- matrix(rev(LOGO), ncol = ncol(LOGO))
  LOGO3 <- LOGO[,ncol(LOGO):1]
  LOGO4 <- LOGO


  ###Here starts the main part of the game
  colorArray <- c("white", "red", "black", "green", "blue", "deeppink")
  painter(LOGO1, colorArray)
  Sys.sleep(0.4)
  painter(LOGO2, colorArray)
  Sys.sleep(0.4)
  painter(LOGO3, colorArray)
  Sys.sleep(0.4)
  painter(LOGO4, colorArray)
  Sys.sleep(3)
}
