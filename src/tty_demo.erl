%% Open tty and draw various stuff on screen

-module(tty_demo).

-compile(export_all).

start() ->
    demo(),
    halt(0).

demo() ->
    TTY = tty:open(),
    Geom = {W,H} = init(TTY),

    %% top-line 
    lists:foreach(
      fun(X) ->
	      goto(TTY, X, 1, Geom),
	      tty:output(TTY, [(X rem 10)+$0])
      end, lists:seq(1, W-1)),

    %% near end
    lists:foreach(
      fun(X) ->
	      goto(TTY, X, 11, Geom),
	      tty:output(TTY, [(X rem 10)+$0])
      end, lists:seq(1, W-1)),
    
    %% LIGHT COLORS
    goto(TTY, 1, H-3, Geom),
    %% tty:output(TTY, "5.1 "),    
    tty:csi(TTY, {fg,light_red}),
    tty:output(TTY, "RED "),
    tty:csi(TTY, {fg,light_green}),
    tty:output(TTY, "GREEN "),
    tty:csi(TTY, {fg,light_blue}),
    tty:output(TTY, "BLUE "),
    tty:csi(TTY, {fg,light_magenta}),
    tty:output(TTY, " MAGENTA "),
    tty:csi(TTY, {fg,light_cyan}),
    tty:output(TTY, "CYAN "),

    %% NORMAL COLORS
    goto(TTY, 1, H-4, Geom),
    %% tty:output(TTY, "5.2 "),
    tty:csi(TTY, {fg,red}),
    tty:output(TTY, "RED "),
    tty:csi(TTY, {fg,green}),
    tty:output(TTY, "GREEN "),
    tty:csi(TTY, {fg,blue}),
    tty:output(TTY, "BLUE "),
    tty:csi(TTY, {fg,magenta}),
    tty:output(TTY, " MAGENTA "),
    tty:csi(TTY, {fg,cyan}),
    tty:output(TTY, "CYAN "),

    %% BOLD LIGHT COLORS
    goto(TTY, 1, H-5, Geom),
    %% tty:output(TTY, "5.3 "),
    tty:csi(TTY, bold),
    tty:csi(TTY, {fg,light_red}),
    tty:output(TTY, "RED "),
    tty:csi(TTY, {fg,light_green}),
    tty:output(TTY, "GREEN "),
    tty:csi(TTY, {fg,light_blue}),
    tty:output(TTY, "BLUE "),
    tty:csi(TTY, {fg,light_magenta}),
    tty:output(TTY, " MAGENTA "),
    tty:csi(TTY, {fg,light_cyan}),
    tty:output(TTY, "CYAN "),
    tty:csi(TTY, no_bold),

    %% Draw hello world
    goto(TTY, (W div 2)-4, H div 2, Geom),
    tty:csi(TTY, bold),
    tty:outputf(TTY, "Hello World [~wx~w]", [W,H]),
    tty:csi(TTY, no_bold),
    tty:csi(TTY, off),

    tty:input(TTY),
    tty:close(TTY).

%% move to start of screen (after init)
origin(TTY, {W,H}) ->
    tty:move(TTY, -(W*H)).

%% move to (X,Y) relative from (0,0)
goto(TTY, X, Y, Geom={W,_H}) ->
    origin(TTY, Geom),
    Pos = (X-1)+(Y-1)*W,
    tty:move(TTY, Pos).

%% fill all screen with blanks (but not the last char!)
%% then try not to scroll, maybe we could remove newline from
%% all output in this mode?
init(TTY) ->
    Geom={W,H}=tty:get_tty_geometry(TTY),
    tty:csi(TTY, off),
    tty:csi(TTY, {erase,scrn}),
    %% make sure cursor i in the left lower corner
    %% tty:output(TTY, "\r"),
    %% tty:output(TTY, lists:duplicate(H,$\n)),
    %% now fill with blanks
    tty:output(TTY, lists:duplicate(W*H-1,$\s)),
    %% and move to (0,0)
    origin(TTY, Geom),
    Geom.
