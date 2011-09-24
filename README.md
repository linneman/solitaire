# Solver for the Game of Solitaire in Clojure

> "Peg solitaire is  a board game for one player  involving movement of pegs
on a board with holes. Some sets  use marbles in a board with indentations.
The game is known simply as Solitaire  in the United Kingdom where the card
games are called Patience. It is  also referred to as Brainvita (especially
in India) ...", quoted from [Wikipedia article Peg-Solitaire](http://en.wikipedia.org/wiki/Peg_solitaire).

This clojure application finds solutions for the game of peg solitaire. For
a small number  of moves all constellations are  determined iteratively and
stored in  an internal hash-map. Hashing  ensures that each board  setup is
evaluated only  once. After an iteration  step the given solution  space is
pruned by means  of a scoring function. The next  iteration step is applied
only for those  constellations which seem to be  most promising afterwards.
This approach  shrinks the number of  analyzed constellations dramatically.
On a 2  GHz Intel Dual Core  machine the solution is determined  in only 20
seconds.

### Build

You need  the clojure build  tool leinignen for  compilation. Download
the lein script file from Github

    $ cd ~/bin
    $ wget http://github.com/technomancy/leiningen/raw/stable/bin/lein
    $ chmod +x lein

and type

    $ lein self-install

The following commands will generate and stand-alone jar file:

    $ lein compile
    $ lein uberjar

Refer also to [Zef's Leiningen page](http://zef.me/2470/building-clojure-projects-with-leiningen) for more specific information about build options.


### Invocation
The following  command will start the simulation

    $ java -jar solitaire-standalone.jar 

Two  optional parameters  allow to  change the  complexity of  the solution
space. The first  determines the number of iterations  before pruning takes
place. The second  one determines the number of constellations  to be taken
into account for following iterations

## Licence
This clojure solitaire solver implementation stands under the terms of the
[GNU General Public Licence](http://www.gnu.org/licenses/gpl.html).

September 2011, Otto Linnemann

## Resources and links
Thanks to all the giants whose shoulders we stand on. And the giants theses giants stand on...
And special thanks to Rich Hickey (and the team) for Clojure. Really, thanks!

* Clojure: http://clojure.org
* Leiningen: https://github.com/technomancy/leiningen

