main = [markdown|

#    VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV

# >>>>> [Press here to launch the Graphical Elm IDE](GraphicalElm.html) <<<<<<<<

#    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Graphical elm is an interface for editing a subset of the Elm programming language.  It displays Elm code as a levelized Elm signal graph.

When I think about signal graphs, the first thing that I think of, are modular synths, like this one:

![Screenshot of alsa modular synth](http://www.passback.org.uk/music/fedora-music-sounds/images/ams.png)

These are great examples of visual programming, but they are hard to edit for three reasons:

 - You have to switch between the keyboard and the mouse

 - You have to avoid cable overlap

 - Performance

So graphical Elm does things differently.

 - It's keyboard only

 - There are no cables

 - Well, it's not there yet

## How does it work?

The signal graph is transformed into a layered graph.  A layered graph is very simple.  We put nodes with no dependencies into the first layer, the next layer contains nodes who's dependencies were fulfilled by the first.

We then can move around this table with the Ctrl+arrow keys.

If you imagine a signal graph like this:

![sample graph](img/navigation.png)

You can see that we show the slected node in green, the dependencies of that node in red, and the dependents of that node in blueYou can see that we show the slected node in green, the dependencies of that node in red, and the dependents of that node in blue.

![color coding](img/colorcode.png)

As you can see, with this color coding, we no longer need the cable connections, because we can see the connections with color.  This allows us to prevent visual clutter, cable overlap, and also should make things more performant in the IDE.

In order to add nodes, we can type in a nodes name into the add node box, and then press F9.  Then we need to hook this node up to other nodes and put code in it.

There are several modes for editing individual nodes:

![edit-modes](img/edit-modes.png)

To hook a node up, we change to the edit mode 'Parents' we then type in a comma separated list of nodes which this node depends on.  To pipe this node into another node, we navigate to that node and add this node as a parent.

We can now build our signal graph.

## Writing programs in graphical-elm

Graphical elm programming should be thought of as a two step process.  First, we build our signal graph.  Seccond, we fill in our nodes with algorithms.

You now know how to build a signal graph in graphical elm, but how do we fill in each node?  Tell it what to do?

We actually have a choice.  Since Elm's singal graphs are filled by pure functions we can really fill these nodes with ANY kind of functions.  They don't have to be Elm functions per say.

Graphical elm provides two languages for filling in these nodes.  But many more could be written.

### Filling nodes with normal Elm code

I can fill nodes with normal elm code.  The compiler does a very simple transformation to your code.  It inserts it as is, and then it adds the list of parents on the end, interspersed with the `~` character.

So if `foo` has the parents `baz` and `bar`, we can fill `foo` with

(\bar baz -> bar + baz) <~

And we will get

(\bar baz -> bar + baz) <~ bar ~ baz

We can also do foldp's as well with this syntax:

If `tig` has the parent `tag`, we can fill `tig` with

foldp (\a b-> a+b) 0

and it will be tranformed to

foldp (\a b-> a+b) 0 tag

## Ikcilpazc

If we are doing a lot of arithmetic heavy computations, coding in Elm can get a bit tiresome.  Both due to parethesis, and the need to convert between Int and Float.  For this purpose, I include a reverse polish style notation:

![1 2 .. +](img/ikcilpazc1+2.png)

It differs from normal reverse polish notation in one way.  That is the dots.  Rather than having each function with an explicit arity, we declare function applications and arity explicitly.  One dot, '.', applies the last item on the stack to the function that follows.  Two dots '..' applies the last two items on the stack to the function that follows.

![3 4 .. + 2 .. *](img/ikcilpazc3+4.png)

![3 . ~](img/ikcilpazc-toFloat.png)

We also provide several aliases for converting between `Int` and `Float`:

_  is an aliase for `floor`
-- is an aliase for `round`
^^ is an aliase for `ceiling`
~  is an aliase for `toFloat`

foo . ~ bar . ~ .. / 4 .. * . --

becomes:

round <| (toFloat foo / toFloat bar) * 4

## Compiling and saving your work, and opening it again

### The quick and easy way

First, you need to make something in graphical-elm:

![Some graphical elm graph](img/somegraph.png)

Then press F4 till you get to the "SaveAndCompile" mode.

![SaveAndCompile mode](img/saveAndCompile.png)

Then copy the generated code out, and paste it into [the online elm editor](http://elm-lang.org/try)

![the elm editor](img/tryelm.png)

You can also save this code to the disk and open it later.  To do this, paste your code into the "paste code to load" box, then press the "Home"  key and then F2.

![open the graph](img/opengraph.png)

Unfortunately this isn't hooked up to any kind of web service, so you have to manage your graphical elm files by your self.  But at least you have control over your own data, if you're that kind of person.

### The profi high professional way

First, install the Elm compiler: [Install instructions](https://github.com/evancz/Elm/blob/master/README.md#install)

Graphical elm generates valid Elm code which you can now compile.  It also knows how to parse and reload it's own autogenerated code.

Since copying and pasting, then saving, then compiling code can be tedius, I provide a quick build script here:

[build script](builder.sh)

Don't forget to make it executable with:

chmod 755 builder.sh

You use it by typing:

./builder.sh FileName.elm

It will cat the given file to the screen.  You can then paste code in here and press Ctrl-D twice to compile and run the code.  So this allows you to have a normalish workflow.

## Examples of graphical elm code:

See the following files in the git repository:

ClockExample.elm

## The source code to graphical elm

[Source](https://github.com/timthelion/graphical-elm)

|]
