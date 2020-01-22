# Ocaml.io
A simplified version of the famous game Agar.io: a hero evolves
in a zombie world; to survive, he must eat the zombies smaller than him; but it can happen
make a zombie bigger than him eat. Zombies move randomly. The hero is
directed by the player using the mouse.
The hero and the zombies are cells; one cell can eat another when they are in contact.
Cells are represented by circles. The hero is in red, the zombies in blue.
To compile and execute the .ml file you need to install Utop.

To test the functions you will use the graphics.cma library. To have it in your environment, type the command

    $ utop graphics.cma
    
You must then load the agar.ml file

    utop # #use "agar.ml" ;;
    
You can launch the game by invoking the main function:

    utop # main () ;;
