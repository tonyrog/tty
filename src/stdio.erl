%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    erlang stdio wrapper
%%% @end
%%% Created :  3 Jan 2022 by Tony Rogvall <tony@rogvall.se>

-module(stdio).

-export([putchar/1, fputc/2]).
-export([puts/1, fputs/2]).
-export([printf/1,printf/2,printf/3,printf/4]).
-export([fprintf/2,fprintf/3,fprintf/4,fprintf/5]).
-export(['$handle_undefined_function'/2]).
%% test
-export([format_float/1, format_float/2]).
-export([format_mantissa/2, format_mantissa/3]).

putchar(C) when is_integer(C) ->
    tty:output(standard_io, [C]).

fputc(C, TTY) ->
    tty:output(TTY, [C]).

puts(S) ->
    tty:output(standard_io, S).

fputs(S, TTY) ->
    tty:output(TTY, S).

printf(F) -> fprintf_(standard_io,F, []).
printf(F,A1) -> fprintf_(standard_io,F, [A1]).
printf(F,A1,A2) -> fprintf_(standard_io,F, [A1,A2]).
printf(F,A1,A2,A3) -> fprintf_(standard_io,F, [A1,A2,A3]).

fprintf(Fd,F) -> fprintf_(Fd,F,[]).
fprintf(Fd,F,A1) -> fprintf_(Fd,F,[A1]).
fprintf(Fd,F,A1,A2) -> fprintf_(Fd,F,[A1,A2]).
fprintf(Fd,F,A1,A2,A3) -> fprintf_(Fd,F,[A1,A2,A3]).

'$handle_undefined_function'(printf, [F|As]) ->
    fprintf_(standard_io,F,As);
'$handle_undefined_function'(fprintf, [Fd,F|As]) ->
    fprintf_(Fd,F,As).

fprintf_(Stream,Format,Args) ->
    format(fun(Cs,N) -> fputs(Cs, Stream), N+length(Cs) end, 0, Format, Args).

-define(IS_SET(F,X), (((F) band (X)) =:= (X))).

-define(FMT_SPACE,     16#0001).
-define(FMT_PLUS,      16#0002).
-define(FMT_MINUS,     16#0004).
-define(FMT_NUMBER,    16#0008).
-define(FMT_ZERO,      16#0010).

-define(QUAL_LONG,      16#0001).
-define(QUAL_DBL_LONG,  16#0002).
-define(QUAL_SHORT,     16#0004).

format(Fun, Acc, Fmt, As) ->
    case collect(Fmt, []) of
	{[],[]} -> Acc;
	{[],Cs} -> Fun(Cs,Acc);
	{Fmt1,[]} ->
	    lflags(Fun,Acc,Fmt1,0,As);
	{Fmt1,Cs} ->
	    Acc1 = Fun(Cs,Acc),
	    lflags(Fun,Acc1,Fmt1,0,As)
    end.

collect([$%|Fmt], Acc) ->
    {Fmt, lists:reverse(Acc)};
collect([C|Fmt], Acc) ->
    collect(Fmt, [C|Acc]);
collect([], Acc) ->
    {[], lists:reverse(Acc)}.


lflags(Fun,Acc,Fmt0=[C|Fmt],Flag,As) ->
    case C of
	$\s ->
	    lflags(Fun,Acc,Fmt,?FMT_SPACE bor Flag,As);
	$0 ->
	    lflags(Fun,Acc,Fmt,?FMT_ZERO bor Flag,As);
	$- ->
	    lflags(Fun,Acc,Fmt,?FMT_MINUS bor Flag,As);
	$+ ->
	    lflags(Fun,Acc,Fmt,?FMT_PLUS bor Flag,As);
	$# ->
	    lflags(Fun,Acc,Fmt,?FMT_NUMBER bor Flag,As);
	_ ->
	    width(Fun,Acc,Fmt0,Flag,As)
    end.

width(Fun,Acc,[$*|Fmt],Flag,[Width|As]) when Width < 0 ->
    prec(Fun,Acc, Fmt, Flag bor ?FMT_MINUS, -Width, As);
width(Fun,Acc,[$*|Fmt],Flag,[Width|As]) ->
    prec(Fun,Acc, Fmt, Flag, Width, As);
width(Fun,Acc,[C|Fmt],Flag,As) when C >= $0, C =< $9 ->
    width_(Fun,Acc,Fmt,Flag,C-$0,As);
width(Fun,Acc,Fmt,Flag,As) ->
    prec(Fun,Acc, Fmt, Flag, 0, As).

width_(Fun,Acc,[C|Fmt],Flag,Width,As) when C >= $0, C =< $9 ->
    width_(Fun,Acc,[C|Fmt],Flag,10*Width+(C-$0),As);
width_(Fun,Acc,Fmt,Flag,Width,As) ->
    prec(Fun,Acc,Fmt,Flag,Width,As).

prec(Fun,Acc, [$.,$*|Fmt], Flag, Width, [Prec|As]) ->
    qual(Fun,Acc, Fmt, Flag, Width, Prec, As);
prec(Fun,Acc, [$.|Fmt], Flag, Width, As) ->
    prec_(Fun,Acc, Fmt, Flag, Width, 0, As);
prec(Fun,Acc, Fmt, Flag, Width, As) ->
    qual(Fun,Acc, Fmt, Flag, Width, -1, As).
    
prec_(Fun,Acc, [C|Fmt], Flag, Width, Prec, As) when C >= $0, C =< $9 ->
    prec_(Fun,Acc, Fmt, Flag, Width, Prec*10 + (C-$0), As);
prec_(Fun,Acc, Fmt, Flag, Width, Prec, As) ->
    qual(Fun,Acc, Fmt, Flag, Width, Prec, As).

qual(Fun,Acc, [$h|Fmt], Flag, Width, Prec, As) ->
    conv(Fun,Acc, Fmt, Flag, Width, Prec, ?QUAL_SHORT, As);
qual(Fun,Acc, [$L|Fmt], Flag, Width, Prec, As) ->
    conv(Fun,Acc, Fmt, Flag, Width, Prec, ?QUAL_DBL_LONG, As);
qual(Fun,Acc, [$l|Fmt], Flag, Width, Prec, As) ->
    conv(Fun,Acc, Fmt, Flag, Width, Prec, ?QUAL_LONG, As);
qual(Fun,Acc, Fmt, Flag, Width, Prec, As) ->
    conv(Fun,Acc, Fmt, Flag, Width, Prec, 0, As).

conv(Fun,Acc, Fmt0=[C|Fmt], Flag, Width, Prec, Qual, As) ->
    case C of
	$c -> conv_chr(Fun,Acc, Fmt0, Flag, Width, Prec, Qual, As);
	$d -> conv_int(Fun,Acc, Fmt0, Flag, Width, Prec, Qual, As);
	$i -> conv_int(Fun,Acc, Fmt0, Flag, Width, Prec, Qual, As);
	$b -> conv_int(Fun,Acc, Fmt0, Flag, Width, Prec, Qual, As);
	$o -> conv_int(Fun,Acc, Fmt0, Flag, Width, Prec, Qual, As);
	$u -> conv_int(Fun,Acc, Fmt0, Flag, Width, Prec, Qual, As);
	$x -> conv_int(Fun,Acc, Fmt0, Flag, Width, Prec, Qual, As);
	$X -> conv_int(Fun,Acc, Fmt0, Flag, Width, Prec, Qual, As);
	$e -> conv_flt(Fun,Acc, Fmt0, Flag, Width, Prec, Qual, As);
	$E -> conv_flt(Fun,Acc, Fmt0, Flag, Width, Prec, Qual, As);
	$f -> conv_flt(Fun,Acc, Fmt0, Flag, Width, Prec, Qual, As);
	$g -> conv_flt(Fun,Acc, Fmt0, Flag, Width, Prec, Qual, As);
	$G -> conv_flt(Fun,Acc, Fmt0, Flag, Width, Prec, Qual, As);
	$p -> conv_int(Fun,Acc, Fmt0, Flag, Width, Prec, Qual, As);
	$s -> conv_str(Fun,Acc, Fmt0, Flag, Width, Prec, Qual, As);
	$T -> conv_str(Fun,Acc, Fmt0, Flag, Width, Prec, Qual, As);
	_ -> 
	    Acc1 = Fun([C],Acc),
	    format(Fun, Acc1, Fmt, As)
    end.

conv_chr(Fun, Acc, [_|Fmt], _Flag, _Width, _Prec, _Qual, [X|As]) ->
    Acc1 = Fun([X], Acc),
    format(Fun, Acc1, Fmt, As).


conv_str(Fun, Acc, [$s|Fmt], Flag, Width, _Prec, _Qual, [X|As]) ->
    Len = if is_list(X) -> length(X);
	     is_binary(X) -> byte_size(X) %% FIXME: unicode
	  end,
    Width1 = if Width =:= 0, is_list(X) -> Len;
		Width =:= 0, is_binary(X) -> Len;
		true -> Width
	     end,
    AccN = 
	if Len >= Width1 ->
		Str = string:sub_string(X, 1, Len),
		Fun(Str, Acc);
	   ?IS_SET(Flag, ?FMT_MINUS) ->
		Acc1 = Fun(string:sub_string(X, 1, Len), Acc),
		Fun(lists:duplicate(Width1-Len, $\s), Acc1);
	   true ->
		Acc1 = Fun(lists:duplicate(Width1-Len, $\s),Acc),
		Fun(string:sub_string(X, 1, Len), Acc1)
	end,
    format(Fun, AccN, Fmt, As);
conv_str(Fun, Acc, [$T|Fmt], Flag, Width, _Prec, _Qual, [X|As]) ->
    %% fixme!
    X1 = io_lib:format("~p", [X]),
    conv_str(Fun, Acc, [$s|Fmt], Flag, Width, _Prec, _Qual, [X1|As]).


conv_int(Fun, Acc, [C|Fmt], Flag, Width, Prec, Qual, [X|As]) ->
    Prefix = prefix(C, Flag, X),
    X1 = coerce(X, Qual),
    Ds = digits(C, X1),
    Plen = length(Prefix),
    Len = length(Ds),
    DWidth = max(Prec, Len) + Plen,
    Width1 = max(Width,DWidth),
    AccN =
	if Width1 =:= DWidth ->
		Acc1 = Fun(Prefix, Acc),
		Acc2 = Fun(lists:duplicate(max(0, Prec - Len), $0), Acc1),
		Fun(Ds, Acc2);
	   ?IS_SET(Flag, ?FMT_MINUS) ->
		Acc1 = Fun(Prefix, Acc),
		Acc2 = Fun(lists:duplicate(max(0, Prec - Len), $0), Acc1),
		Acc3 = Fun(Ds, Acc2),
		Fun(lists:duplicate(max(0, Width1 - DWidth), $\s), Acc3);
	   ?IS_SET(Flag, ?FMT_ZERO) ->
		Acc1 = Fun(Prefix, Acc),
		Acc2 = Fun(lists:duplicate(max(0, Width1 - DWidth), $0),Acc1),
		Acc3 = Fun(lists:duplicate(max(0, Prec - Len), $0), Acc2),
		Fun(Ds, Acc3);
	   true ->
		Acc1 = Fun(lists:duplicate(max(0, Width1 - DWidth), $\s), Acc),
		Acc2 = Fun(Prefix, Acc1),
		Acc3 = Fun(lists:duplicate(max(0, Prec - Len), $0), Acc2),
		Fun(Ds, Acc3)
	end,
    format(Fun, AccN, Fmt, As).


conv_flt(Fun, Acc, [_C|Fmt], _Flag, Width, Prec, _Qual, [X|As]) ->
    %% Prefix = prexif(C, Flag, X),
    Prec1 = if Prec < 0 -> 6; true -> Prec end,
    %% {S,M,Exp} = sign_mantissa_exponent(X),
    Format = if Width =:= 0, Prec1 =:= 0 ->
		     "~f";
		Width =:= 0 ->
		     "~."++integer_to_list(Prec1)++"f";
		true ->
		     "~"++integer_to_list(Width)++"."++
			 integer_to_list(Prec1)++"f"
	     end,
    Acc1 = Fun(io_lib:format(Format, [float(X)]), Acc),
    format(Fun, Acc1, Fmt, As).

%% i2b(I) ->    
%%    "2#"++integer_to_list(I, 2).

%% F = (-1)^S 1.b52...b0 * 2^(E-1023)
format_float(F) ->
    format_float(F, 6).
format_float(F, Prec) ->
    <<S:1, E:11, M:52>> = <<F:64/float>>,
    case <<F:64/float>> of
	<<S:1, 0:11, 0:52>> ->
	    sign(S, "0");
	<<S:1, E:11, M:52>> ->
	    M2 = (1 bsl 52) bor M,
	    E2 = E - 1023,
	    if
		E2 >= 0, E2 =< 52 ->
		    IBits = E2+1,     %% number of bits in integer part
		    FBits = 53-IBits, %% number of bits in fraction part
		    FMask = ((1 bsl FBits)-1),
		    Int = (M2 bsr FBits),
		    Frac = (M2 band FMask),
		    integer_to_list(Int)++"."++
			format_mantissa(Frac, FBits, Prec);
		true ->
		    Ds0 = format_mantissa(M2, Prec),
		    Ds1 = Ds0 ++ "E"++integer_to_list(E2),
		    sign(S, Ds1)
	    end
    end.

sign(1, Ds) -> [$-|Ds];
sign(_, Ds) -> Ds.
    

%% 10^Prec * (b51/2^1 + b50/2^2 + b52-r/2^r... b1/2^52)
format_mantissa(M, Prec) ->
    format_mantissa(M, 52, Prec).

format_mantissa(M, Size, Prec) ->
    N10 = pow(10,Prec),
    {N,D} = mantissa_(M,(1 bsl (Size-1)), 1, 0, 1),
    tl(integer_to_list(N10 + ((N*N10) div D))).

mantissa_(_M, 0, _R, N, D) ->
    {N,D};
mantissa_(M, Bit, R, N, D) ->
    if M band Bit =/= 0 ->
	    mantissa_(M, Bit bsr 1, R+1, (N bsl R) + D, D bsl R);
       Bit > 1, M band (Bit-1) =:= 0 ->
	    {N,D};
       true ->
	    mantissa_(M, Bit bsr 1, R+1, N, D)
    end.
    

pow(_A, 0) -> 1;
pow(A, B) when B>=0 -> pow_(A, B, 1).

pow_(A,1,P) -> A*P;
pow_(A,B,P)  ->
    B1 = B bsr 1,
    A1 = A*A,
    if B - B1 =:= B1 ->
	    pow_(A1, B1, P);
       true ->
	    pow_(A1, B1, A*P)
    end.

coerce(X, 0) -> 
    abs(X);
coerce(X, Qual) ->
    case Qual of
	?QUAL_SHORT -> X band 16#ffff;
	?QUAL_LONG  ->
	    case erlang:system_info(wordsize) of
		4 -> X band 16#ffffffff;
		8 -> X band 16#ffffffffffffffff
	    end;
	?QUAL_DBL_LONG ->
	    case erlang:system_info(wordsize) of
		4 -> X band 16#ffffffffffffffff;
		8 -> X band 16#ffffffffffffffff
	    end
    end.

prefix($d, Flag, X) -> iprefix(10,Flag,X);
prefix($b, Flag, X) -> iprefix(2,Flag,X);
prefix($o, Flag, X) -> iprefix(8,Flag,X);
prefix($x, Flag, X) -> iprefix(16,Flag,X);
prefix($X, Flag, X) -> iprefix(16,Flag,X);
prefix($f, Flag, X) -> fprefix(Flag,X);
prefix($e, Flag, X) -> fprefix(Flag,X);
prefix($E, Flag, X) -> fprefix(Flag,X);
prefix($g, Flag, X) -> fprefix(Flag,X);
prefix($G, Flag, X) -> fprefix(Flag,X).

iprefix(Base,Flag,X) when X < 0, ?IS_SET(Flag,?FMT_NUMBER) ->
    [$-|integer_to_list(Base)]++"#";
iprefix(Base,Flag,_X) when ?IS_SET(Flag,?FMT_NUMBER) ->
    integer_to_list(Base)++"#";
iprefix(_Base,_Flag,X) when X < 0 -> "-";
iprefix(_Base,Flag,_X) when ?IS_SET(Flag,?FMT_PLUS) ->  "+";
iprefix(_Base,Flag,_X) when ?IS_SET(Flag,?FMT_SPACE) -> " ";
iprefix(_Base,_Flag,_X) -> "".

fprefix(_Flag, X) when X < 0 -> "-";
fprefix(Flag, X) when X > 0, ?IS_SET(Flag, ?FMT_PLUS) -> "+";
fprefix(Flag, X) when X >= 0, ?IS_SET(Flag, ?FMT_SPACE) -> "\s";
fprefix(_Flag,_X) -> "".

digits($d, X) -> integer_to_list(X,10);
digits($u, X) -> integer_to_list(X,10);
digits($i, X) -> integer_to_list(X,10);
digits($b, X) -> integer_to_list(X, 2);
digits($o, X) -> integer_to_list(X, 8);
digits($x, X) -> string:to_lower(integer_to_list(X,16));
digits($X, X) -> integer_to_list(X,16);
digits($p, X) -> integer_to_list(X,16);
digits(_, _X) ->  "".
