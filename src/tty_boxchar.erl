%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Test unicode box drawing characters
%%%%   erl -noshell -s tty_boxchar demo 
%%% @end
%%% Created :  8 Dec 2021 by Tony Rogvall <tony@rogvall.se>

-module(tty_boxchar).

-include("../include/tty_boxchar.hrl").
-compile(export_all).

demo() ->
    TTY = tty:open(),
    draw_box(TTY, 5, 8, 20, light),
    draw_box(TTY, 5, 8, 20, heavy),
    draw_box(TTY, 5, 8, 20, double),
    tty:close(TTY),
    halt(0).

draw_box(TTY, Offset, Rows, Columns, light) ->
    draw_box_(TTY,Offset, Rows, Columns,
	      ?BOX_DRAWINGS_LIGHT_DOWN_AND_RIGHT,
	      ?BOX_DRAWINGS_LIGHT_DOWN_AND_LEFT,
	      ?BOX_DRAWINGS_LIGHT_UP_AND_RIGHT,
	      ?BOX_DRAWINGS_LIGHT_UP_AND_LEFT,
	      ?BOX_DRAWINGS_LIGHT_HORIZONTAL,
	      ?BOX_DRAWINGS_LIGHT_VERTICAL);
draw_box(TTY,Offset, Rows, Columns, heavy) ->
    draw_box_(TTY,Offset, Rows, Columns,
	      ?BOX_DRAWINGS_HEAVY_DOWN_AND_RIGHT,
	      ?BOX_DRAWINGS_HEAVY_DOWN_AND_LEFT,
	      ?BOX_DRAWINGS_HEAVY_UP_AND_RIGHT,
	      ?BOX_DRAWINGS_HEAVY_UP_AND_LEFT,
	      ?BOX_DRAWINGS_HEAVY_HORIZONTAL,
	      ?BOX_DRAWINGS_HEAVY_VERTICAL);
draw_box(TTY,Offset, Rows, Columns, double) ->
    draw_box_(TTY,Offset, Rows, Columns,
	      ?BOX_DRAWINGS_DOUBLE_DOWN_AND_RIGHT,
	      ?BOX_DRAWINGS_DOUBLE_DOWN_AND_LEFT,
	      ?BOX_DRAWINGS_DOUBLE_UP_AND_RIGHT,
	      ?BOX_DRAWINGS_DOUBLE_UP_AND_LEFT,
	      ?BOX_DRAWINGS_DOUBLE_HORIZONTAL,
	      ?BOX_DRAWINGS_DOUBLE_VERTICAL).

draw_box_(TTY,Offset, Rows, Columns,
	  DOWN_AND_RIGHT,
	  DOWN_AND_LEFT,
	  UP_AND_RIGHT,
	  UP_AND_LEFT,
	  HORIZONTAL,
	  VERTICAL) ->

    tty:output(TTY,[lists:duplicate(Offset,$\s),DOWN_AND_RIGHT, 
		    lists:duplicate(Columns,HORIZONTAL),
		    DOWN_AND_LEFT, $\n]),
    lists:foreach(
      fun(_I) ->
	      tty:output(TTY,[lists:duplicate(Offset,$\s),
			      VERTICAL,
			      lists:duplicate(Columns,$\s),
			      VERTICAL, $\n])
      end, lists:seq(1, Rows)),
    
    tty:output(TTY, [lists:duplicate(Offset,$\s),UP_AND_RIGHT, 
		     lists:duplicate(Columns,HORIZONTAL),
		     UP_AND_LEFT, $\n]),	  
    ok.
