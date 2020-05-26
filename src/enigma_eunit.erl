-module(enigma_eunit).

-include_lib("eunit/include/eunit.hrl").

% f_rotor_inverse_test() ->
%     P = $X,
%     X = $B,
%     Rotor = enigma:rotorBeta(),
%     Frotor = enigma:f_rotor(Rotor, P, X),
%     FrotorBar = enigma:inverse_f_rotor(Rotor, P, Frotor),
%     ?assertEqual(X, FrotorBar).
    % Enigma = enigma:setup("B",["II","I","III"],[26,23,4],[{$E,$Z}, {$B,$L}, {$X,$P}, {$W,$R}, {$I,$U}, {$V,$M}, {$J,$O}]),
    % Res = enigma:crypt([$A,$G,$I],Enigma,""),

%% ========================
%% Existing Tests
%% ========================

%% A simple test from
%% http://gcc.eisbehr.de/manual/en/enigma.html
simple_test() ->
    Enigma = enigma:setup("B",{"II","I","III"},{26,23,4},[{$E,$Z}, {$B,$L}, {$X,$P}, {$W,$R}, {$I,$U}, {$V,$M}, {$J,$O}], {$A,$G,$I}),
    Res = enigma:crypt(Enigma,"WBCGQLUWJFCKLWMQIXWPDYVIEIRLYSDQRIANEQQQIZRWMIKFWNKZNGSVKZVVWXNBFNQDO"),
    ?assertEqual("THISISANEXAMPLEMESSAGEXBROUGHTTOYOUBYGCCXQEERSANDHAVEFUNWITHTHEENIGMA",Res).

%% This is a simple test from the Enigma instruction manual
inst_man_test() ->
    Enigma = enigma:setup("A",{"II","I","III"},{24,13,22},[{$A,$M}, {$F,$I}, {$N,$V}, {$P,$S}, {$T,$U}, {$W,$Z}], {$A,$B,$L}),
    Res = enigma:crypt(Enigma,"GCDSEAHUGWTQGRKVLFGXUCALXVYMIGMMNMFDXTGNVHVRMMEVOUYFZSLRHDRRXFJWCFHUHMUNZEFRDISIKBGPMYVXUZ"),
    ?assertEqual("FEINDLIQEINFANTERIEKOLONNEBEOBAQTETXANFANGSUEDAUSGANGBAERWALDEXENDEDREIKMOSTWAERTSNEUSTADT",Res).

%% ========================
%% Not implemented
%% ========================

%% This test uses only standard rotors, but requires your machine to be re-set.
%% My implementation allows you to specify the rotor settings as a parameter to crypt/3,
%% but you may prefer a reset function, or to completely reset the machine.
barbarossa() ->
    Enigma = enigma:setup("B",{"II","IV","V"},{2,21,12},[{$A,$V}, {$B,$S}, {$C,$G}, {$D,$L}, {$F,$U}, {$H,$Z}, {$I,$N}, {$K,$M}, {$O,$W}, {$R,$X}], {$B,$L,$A}),
    Res = enigma:crypt(Enigma,"EDPUDNRGYSZRCXNUYTPOMRMBOFKTBZREZKMLXLVEFGUEYSIOZVEQMIKUBPMMYLKLTTDEISMDICAGYKUACTCDOMOHWXMUUIAUBSTSLRNBZSZWNRFXWFYSSXJZVIJHIDISHPRKLKAYUPADTXQSPINQMATLPIFSVKDASCTACDPBOPVHJK"),
    ?assertEqual("AUFKLXABTEILUNGXVONXKURTINOWAXKURTINOWAXNORDWESTLXSEBEZXSEBEZXUAFFLIEGERSTRASZERIQTUNGXDUBROWKIXDUBROWKIXOPOTSCHKAXOPOTSCHKAXUMXEINSAQTDREINULLXUHRANGETRETENXANGRIFFXINFXRGTX",Res,{$L,$S,$D}),
    Res2 = enigma:crypt(Enigma,"SFBWDNJUSEGQOBHKRTAREEZMWKPPRBXOHDROEQGBBGTQVPGVKBVVGBIMHUSZYDAJQIROAXSSSNREHYGGRPISEZBOVMQIEMMZCYSGQDGRERVBILEKXYQIRGIRQNRDNVRXCYYTNJR"),
    ?assertEqual("DREIGEHTLANGSAMABERSIQERVORWAERTSXEINSSIEBENNULLSEQSXUHRXROEMXEINSXINFRGTXDREIXAUFFLIEGERSTRASZEMITANFANGXEINSSEQSXKMXKMXOSTWXKAMENECXK",Res2).
    
%% This requires support for 4 rotors.
u264() ->
    Enigma = enigma:setup("ThinB",{"Beta","II","IV","I"},{1,1,1,22},[{$A,$T}, {$B,$L}, {$D,$F}, {$G,$J}, {$H,$M}, {$N,$W}, {$O,$P}, {$Q,$Y}, {$R,$Z}, {$V,$X}],{$V,$J,$N,$A}),
    Res = enigma:crypt(Enigma,"NCZWVUSXPNYMINHZXMQXSFWXWLKJAHSHNMCOCCAKUQPMKCSMHKSEINJUSBLKIOSXCKUBHMLLXCSJUSRRDVKOHULXWCCBGVLIYXEOAHXRHKKFVDREWEZLXOBAFGYUJQUKGRTVUKAMEURBVEKSUHHVOYHABCJWMAKLFKLMYFVNRIZRVVRTKOFDANJMOLBGFFLEOPRGTFLVRHOWOPBEKVWMUQFMPWPARMFHAGKXIIBG"),
    ?assertEqual("VONVONJLOOKSJHFFTTTEINSEINSDREIZWOYYQNNSNEUNINHALTXXBEIANGRIFFUNTERWASSERGEDRUECKTYWABOSXLETZTERGEGNERSTANDNULACHTDREINULUHRMARQUANTONJOTANEUNACHTSEYHSDREIYZWOZWONULGRADYACHTSMYSTOSSENACHXEKNSVIERMBFAELLTYNNNNNNOOOVIERYSICHTEINSNULL",Res).

%% This test requires correct implementation of the "double stepping" of the middle rotor, as described in
%% https://home.comcast.net/~dhhamer/downloads/rotors1.pdf
%% as well as implementation of the higher numbered rotors that increment twice per revolution
scharnhorst() ->
    Enigma = enigma:setup("B",{"III","VI","VIII"},{1,8,13},[{$A,$N}, {$E,$Z}, {$H,$K}, {$I,$J}, {$L,$R}, {$M,$Q}, {$O,$T}, {$P,$V}, {$S,$W}, {$U,$X}], {$U,$Z,$V}),
    Res = enigma:crypt(Enigma,"YKAENZAPMSCHZBFOCUVMRMDPYCOFHADZIZMEFXTHFLOLPZLFGGBOTGOXGRETDWTJIQHLMXVJWKZUASTR"),
    ?assertEqual("STEUEREJTANAFJORDJANSTANDORTQUAAACCCVIERNEUNNEUNZWOFAHRTZWONULSMXXSCHARNHORSTHCO",Res).

%% =================================
%% Additional Tests
%% =================================