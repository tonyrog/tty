%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Handle tty io
%%% @end
%%% Created : 22 Mar 2021 by Tony Rogvall <tony@rogvall.se>

-module(tty).

-export([open/0]).
-export([close/1]).
-export([input/1]).
-export([input_ready/1]).
-export([output/2]).
-export([output_ready/1]).
-export([output_sync/2]).
-export([get_line/1]).
-export([move/2]).
-export([insert/2]).
-export([delete/2]).
-export([beep/1]).
-export([get_tty_geometry/1]).
-export([get_unicode_state/1]).
-export([set_unicode_state/2]).
%% util
-export([outputf/2, outputf/3]).
-export([csi/2]).
-export([sgr/1]).

-define(OP_PUTC,0).
-define(OP_MOVE,1).
-define(OP_INSC,2).
-define(OP_DELC,3).
-define(OP_BEEP,4).
-define(OP_PUTC_SYNC,5).
% Control op
-define(ERTS_TTYSL_DRV_CONTROL_MAGIC_NUMBER, 16#018b0900).
-define(CTRL_OP_GET_WINSIZE, (100 + ?ERTS_TTYSL_DRV_CONTROL_MAGIC_NUMBER)).
-define(CTRL_OP_GET_UNICODE_STATE, (101 + ?ERTS_TTYSL_DRV_CONTROL_MAGIC_NUMBER)).
-define(CTRL_OP_SET_UNICODE_STATE, (102 + ?ERTS_TTYSL_DRV_CONTROL_MAGIC_NUMBER)).
-define(ESC,   27).
-define(UP,    $A).
-define(DOWN,  $B).
-define(RIGHT, $C).
-define(LEFT,  $D).

-define(DEL,   $3).
-define(CSI,   ?ESC,$[).

-define(TTY_INPUT, tty_input).
-define(KILL_BUFFER, kill_buffer).
-define(HISTORY, history).

%% FIXME: how do we steel tty_sl without fuzz?
open() ->
    put(?TTY_INPUT, []),
    open_port({spawn,'tty_sl -c -e'}, [eof]).

close(Port) ->
    port_close(Port).

input(Port) ->
    case get(?TTY_INPUT) of
	[Key|Ks] ->
	    put(?TTY_INPUT, Ks),
	    Key;
	[] ->
	    receive
		{Port,{data,Cs}} ->
		    [Key|Ks] = translate_keys(Cs),
		    put(?TTY_INPUT, Ks),
		    Key;
		{Port,eof} ->
		    eof
	    end
    end.

input_ready(Port) ->
    case get(?TTY_INPUT) of
	[_|_] ->
	    true;
	[] ->
	    receive
		{Port,{data,Cs=[_|_]}} ->
		    Ks = translate_keys(Cs),
		    put(?TTY_INPUT, Ks),
		    true
	    after 0 ->
		    false
	    end
    end.

output_ready(undefined) ->
    false;
output_ready(_) ->
    true.

outputf(TTY, Fmt) ->
    output(TTY, io_lib:format(Fmt, [])).
outputf(TTY, Fmt, Args) ->
    output(TTY, io_lib:format(Fmt, Args)).

output(Port, Cs) ->
    port_command(Port, [?OP_PUTC|unicode:characters_to_binary(Cs,utf8)]).

output_sync(Port, Cs) ->
    port_command(Port,[?OP_PUTC_SYNC|unicode:characters_to_binary(Cs,utf8)]).

move(Port, N) ->
    port_command(Port, [?OP_MOVE|<<N:16>>]).

insert(Port,Cs) ->
    port_command(Port, [?OP_INSC|unicode:characters_to_binary(Cs,utf8)]).

delete(Port, N) ->
    port_command(Port, [?OP_DELC|<<N:16>>]).

beep(Port) ->
    port_command(Port, [?OP_BEEP]).

% Let driver report window geometry,
% definitely outside of the common interface
get_tty_geometry(Port) ->
    case (catch port_control(Port,?CTRL_OP_GET_WINSIZE,[])) of
	List when length(List) =:= 8 -> 
	    <<W:32/native,H:32/native>> = list_to_binary(List),
	    {W,H};
	_ ->
	    error
    end.
get_unicode_state(Port) ->
    case (catch port_control(Port,?CTRL_OP_GET_UNICODE_STATE,[])) of
	[Int] when Int > 0 -> 
	    true;
	[Int] when Int =:= 0 ->
	    false;
	_ ->
	    error
    end.

set_unicode_state(Port, Bool) ->
    Data = case Bool of
	       true -> [1];
	       false -> [0]
	   end,
    case (catch port_control(Port,?CTRL_OP_SET_UNICODE_STATE,Data)) of
	[Int] when Int > 0 -> 
	    {unicode, utf8};
	[Int] when Int =:= 0 ->
	    {unicode, false};
	_ ->
	    error
    end.

%% probably not useful -
%% to use multiline movement then use
%% get_tty_gemometry and move
csi(Port, {up,A}) ->
    output(Port, [?CSI,integer_to_list(A),$A]);
csi(Port, {down,B}) ->
    output(Port, [?CSI,integer_to_list(B),$B]);
csi(Port, {forword,C}) ->
    output(Port, [?CSI,integer_to_list(C),$C]);
csi(Port, {back,D}) ->
    output(Port, [?CSI,integer_to_list(D),$D]);
csi(Port, {next_line,E}) ->
    output(Port, [?CSI,integer_to_list(E),$E]);
csi(Port, show_cursor) ->
    output(Port, [?CSI,$?,$2,$5,$h]);
csi(Port, hide_cursor) ->
    output(Port, [?CSI,$?,$2,$5,$l]);
csi(Port, {erase,eos}) -> output(Port, [?CSI,$0,$J]);
csi(Port, {erase,bos}) -> output(Port, [?CSI,$1,$J]);
csi(Port, {erase,scrn}) -> output(Port, [?CSI,$2,$J]);
csi(Port, {erase,eol}) -> output(Port, [?CSI,$0,$K]);
csi(Port, {erase,bol}) -> output(Port, [?CSI,$1,$K]);
csi(Port, {erase,line}) -> output(Port, [?CSI,$2,$K]);
csi(Port, {scroll,N}) when N>0 -> 
    output(Port,[?CSI,integer_to_list(N),$S]);
csi(Port, {scroll,N}) when N<0 ->
    output(Port,[?CSI,integer_to_list(-N),$T]);
csi(Port, SGR) ->
    output(Port, [?CSI,sgr(SGR),$m]).

sgr(L) when is_list(L) -> sgrl(L);
sgr(V) when is_integer(V) -> integer_to_list(V);
sgr(C) -> sgrc(C).
    
sgrl([H]) -> [sgr(H)];
sgrl([H|T]) -> [sgr(H),$;|sgrl(T)];
sgrl([]) -> [].

sgrc(Code) ->
    case Code of
	off -> $0;
	bold -> $1;
	dim -> $2;
	italic -> $3;
	underline -> $4;
	slow_blink -> $5;
	fast_blink -> $6;
	invert -> $7;
	hide -> $8;
	strike -> $9;
	primary -> "10";
	{alternative,9} -> "20";
	{alternative,F} -> [$1,$1+(F rem 9)]; %% 0..8
	no_bold -> "21";
	no_dim -> "22";
	no_italic -> "23";
	no_underline -> "24";
	no_blink -> "25";
	proportional -> "26";
	no_invert -> "27";
	no_hide -> "28";
	no_strike -> "29";
	%% 38 for r;g;b
	{fg,default} -> "39";
	{fg,C} -> integer_to_list(30+color(C));
	{bg,default} -> "49";
	{bg,C} -> integer_to_list(40+color(C));
	not_proportional -> "50"
    end.

color(black) -> 0;
color(red) -> 1;
color(green) -> 2;
color(yellow) -> 3;
color(blue) -> 4;
color(magenta) -> 5;
color(cyan) -> 6;
color(white) -> 7;
color(gray) -> 60;
color(light_red) -> 61;
color(light_green) -> 62;
color(light_yellow) -> 63;
color(light_blue) -> 64;
color(light_magenta) -> 65;
color(light_cyan) -> 66;
color(light_white) -> 67;
color(C) when C >= 0, C =< 7 -> C;
color(C) when C >= 8, C =< 15 -> 30+(C-8).

-define(BACKSPACE, 127).

%% translate multiple keystrokes to meta keys
translate_keys([?ESC,$[,$A | Cs]) ->
    [up | translate_keys(Cs)];
translate_keys([?ESC,$[,$B | Cs]) ->
    [down | translate_keys(Cs)];
translate_keys([?ESC,$[,$C | Cs]) ->
    [right | translate_keys(Cs)];
translate_keys([?ESC,$[,$D | Cs]) ->
    [left | translate_keys(Cs)];
translate_keys([?ESC,$[,$H | Cs]) ->
    [home | translate_keys(Cs)];
translate_keys([?ESC,$[,$F | Cs]) ->
    ['end' | translate_keys(Cs)];
translate_keys([?ESC,$[,$2,$~ | Cs]) ->
    [insert | translate_keys(Cs)];
translate_keys([?ESC,$[,$3,$~ | Cs]) ->
    [delete | translate_keys(Cs)];
translate_keys([?ESC,$[,$5,$~ | Cs]) ->
    [page_up | translate_keys(Cs)];
translate_keys([?ESC,$[,$6,$~ | Cs]) ->
    [page_down | translate_keys(Cs)];
translate_keys([?BACKSPACE | Cs]) ->
    [backspace | translate_keys(Cs)];
translate_keys([?ESC,C | Cs]) ->
    [{esc,C}|translate_keys(Cs)];
translate_keys([C|Cs]) ->
    [C|translate_keys(Cs)];
translate_keys([]) ->
    [].

%% {esc,$b} - backward word
%% {esc,$f} - forward word

-spec get_line(Port::port()) -> binary().
get_line(Port) ->
    get_line(Port, [], []).

get_line(Port, After, Before) ->
    case input(Port) of
	eof -> eof;
	$\r ->
	    output(Port, [$\s]),
	    Line = list_to_binary(lists:reverse(Before)++After),
	    case is_blank_line(Line) of
		true ->
		    Line;
		false ->
		    case get(?HISTORY) of
			undefined ->
			    put(?HISTORY, {[{After,Before}],[]});
			{Above,Beneath} ->
			    Hist=[{After,Before}|lists:reverse(Beneath,Above)],
			    put(?HISTORY,{Hist,[]})
		    end
	    end,
	    Line;
	$\t ->
	    %% FIXME: expand_fun!
	    {Silent,Insert,Expand} = ffe:expand(Before),
	    if Silent =:= yes -> ok;
	       Silent =:= no -> beep(Port)
	    end,
	    %% FIXME: build into expand fun
	    ffe:format_word_list(0, Expand),
	    Before1 = lists:reverse(binary_to_list(Insert),Before),
	    if Expand =:= [], Insert =/= [] ->
		    insert(Port, Insert);
	       Expand =/= [] ->
		    insert(Port, lists:reverse(Before1));
	       true ->
		    ok
	    end,
	    get_line(Port, After, Before1);
	$\b ->
	    get_line_bs(Port, After, Before);
	backspace ->
	    get_line_bs(Port, After, Before);
	$\^a ->
	    get_line_beginning_of_line(Port, After, Before);
	$\^b ->
	    get_line_backward_char(Port, After, Before);
	$\^d ->
	    get_line_delete_char(Port, After, Before);
	$\^e ->
	    get_line_end_of_line(Port, After, Before);
	$\^f ->
	    get_line_forward_char(Port, After, Before);
	$\^k ->
	    get_line_kill_to_end_of_line(Port, After, Before);
	$\^y ->
	    get_line_insert_from_kill_buffer(Port, After, Before);
	$\^p -> 
	    get_line_previous_line(Port, After, Before);
	$\^n -> 
	    get_line_next_line(Port, After, Before);
	up -> 
	    get_line_previous_line(Port, After, Before);
	down ->
	    get_line_next_line(Port, After, Before);
	left ->
	    get_line_backward_char(Port, After, Before);
	right ->
	    get_line_forward_char(Port, After, Before);
	Key when Key >= $\s, Key =< $~ ->
	    insert(Port, [Key]),
	    get_line(Port, After, [Key|Before]);
	_Key ->
	    beep(Port),
	    get_line(Port, After, Before)
    end.

is_blank_line(<<$\s,Cs/binary>>) -> is_blank_line(Cs);
is_blank_line(<<$\t,Cs/binary>>) -> is_blank_line(Cs);
is_blank_line(<<>>) -> true;
is_blank_line(_) -> false.
    
get_line_delete_char(Port, After, Before) ->
    case After of
	[] ->
	    beep(Port), %% option?
	    get_line(Port, After, Before);
	[_|After1] ->
	    delete(Port, 1),
	    get_line(Port, After1, Before)
    end.

get_line_backward_char(Port, After, Before) ->
    case Before of
	[] ->
	    beep(Port), %% option?
	    get_line(Port, After, Before);
	[Char|Before1] ->
	    move(Port, -1),
	    get_line(Port, [Char|After], Before1)
    end.

get_line_forward_char(Port, After, Before) ->
    case After of
	[Char|After1] ->
	    move(Port, 1),
	    get_line(Port, After1, [Char|Before]);
	[] ->
	    beep(Port), %% option?
	    get_line(Port, After, Before)
    end.

get_line_end_of_line(Port, After, Before) ->
    case After of
	[] ->
	    beep(Port),
	    get_line(Port, After, Before);
	_ ->
	    move(Port, length(After)),
	    get_line(Port, [], lists:reverse(After,Before))
    end.
    
get_line_beginning_of_line(Port, After, Before) ->
    case Before of
	[] ->
	    beep(Port),
	    get_line(Port, After, Before);
	_ ->
	    move(Port, -length(Before)),
	    get_line(Port, lists:reverse(Before,After), [])
    end.
    
get_line_bs(Port, After, Before) ->
    case Before of
	[_|Before1] ->
	    delete(Port, -1),
	    get_line(Port, After, Before1);
	[] ->
	    beep(Port),
	    get_line(Port, After, Before)
    end.

get_line_kill_to_end_of_line(Port, After, Before) ->
    case After of
	[] ->
	    put(?KILL_BUFFER, []),
	    get_line(Port, After, Before);
	_ ->
	    delete(Port, length(After)),
	    put(?KILL_BUFFER, After),
	    get_line(Port, [], Before)
    end.

get_line_insert_from_kill_buffer(Port, After, Before) ->
    case get(?KILL_BUFFER) of
	[] ->
	    get_line(Port, After, Before);
	Yank ->
	    insert(Port, Yank),
	    get_line(Port, After, lists:reverse(Yank, Before))
    end.

get_line_previous_line(Port, After, Before) ->
    case get(?HISTORY) of
	undefined ->
	    beep(Port),
	    get_line(Port, After, Before);
	{[],_} ->
	    beep(Port),
	    get_line(Port, After, Before);
	{[{A,B}|Above],Beneath} ->
	    move(Port, -length(Before)),
	    delete(Port, length(After)+length(Before)),
	    put(?HISTORY, {Above, [{A,B}|Beneath]}),
	    insert(Port, lists:reverse(B)),
	    insert(Port, A),
	    move(Port, -length(A)),
	    get_line(Port, A, B)
    end.
    

get_line_next_line(Port, After, Before) ->
    case get(?HISTORY) of
	undefined ->
	    beep(Port),
	    get_line(Port, After, Before);
	{_,[]} ->
	    beep(Port),
	    get_line(Port, After, Before);
	{Above,[{A,B}|Beneath]} ->
	    move(Port, -length(Before)),
	    delete(Port, length(After)+length(Before)),
	    put(?HISTORY, {[{A,B}|Above], Beneath}),
	    insert(Port, lists:reverse(B)),
	    insert(Port, A),
	    move(Port, -length(A)),
	    get_line(Port, A, B)
    end.
