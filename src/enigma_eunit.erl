-module(enigma_eunit).

-include_lib("eunit/include/eunit.hrl").

%% A simple test from
%% http://gcc.eisbehr.de/manual/en/enigma.html
simple_test() ->
    Enigma = enigma:setup("B",["II","I","III"],[26,23,4],[{$E,$Z}, {$B,$L}, {$X,$P}, {$W,$R}, {$I,$U}, {$V,$M}, {$J,$O}]),
    Res = enigma:crypt([$A,$G,$I],Enigma,"WBCG QLUWJ FCKLW MQIXW PDYVI EIRLY SDQRI ANEQQ QIZRW MIKFW NKZNG SVKZV VWXNB FNQDO"),
    ?assertEqual("THIS ISANE XAMPL EMESS AGEXB ROUGH TTOYO UBYGC CXQEE RSAND HAVEF UNWIT HTHEE NIGMA",Res).

%% This is a simple test from the Enigma instruction manual
inst_man_test() ->
    Enigma = enigma:setup("A",["II","I","III"],[24,13,22],[{$A,$M}, {$F,$I}, {$N,$V}, {$P,$S}, {$T,$U}, {$W,$Z}]),
    Res = enigma:crypt([$A,$B,$L],Enigma,"GCDSE AHUGW TQGRK VLFGX UCALX VYMIG MMNMF DXTGN VHVRM MEVOU YFZSL RHDRR XFJWC FHUHM UNZEF RDISI KBGPM YVXUZ"),
    ?assertEqual("FEIND LIQEI NFANT ERIEK OLONN EBEOB AQTET XANFA NGSUE DAUSG ANGBA ERWAL DEXEN DEDRE IKMOS TWAER TSNEU STADT",Res).

%% This test uses only standard rotors, but requires your machine to be re-set.
%% My implementation allows you to specify the rotor settings as a parameter to crypt/3,
%% but you may prefer a reset function, or to completely reset the machine.
barbarossa_test() ->
    Enigma = enigma:setup("B",["II","IV","V"],[2,21,12],[{$A,$V}, {$B,$S}, {$C,$G}, {$D,$L}, {$F,$U}, {$H,$Z}, {$I,$N}, {$K,$M}, {$O,$W}, {$R,$X}]),
    Res = enigma:crypt([$B,$L,$A],Enigma,"EDPUD NRGYS ZRCXN UYTPO MRMBO FKTBZ REZKM LXLVE FGUEY SIOZV EQMIK UBPMM YLKLT TDEIS MDICA GYKUA CTCDO MOHWX MUUIA UBSTS LRNBZ SZWNR FXWFY SSXJZ VIJHI DISHP RKLKA YUPAD TXQSP INQMA TLPIF SVKDA SCTAC DPBOP VHJK"),
    ?assertEqual("AUFKL XABTE ILUNG XVONX KURTI NOWAX KURTI NOWAX NORDW ESTLX SEBEZ XSEBE ZXUAF FLIEG ERSTR ASZER IQTUN GXDUB ROWKI XDUBR OWKIX OPOTS CHKAX OPOTS CHKAX UMXEI NSAQT DREIN ULLXU HRANG ETRET ENXAN GRIFF XINFX RGTX",Res),
    Res2 = enigma:crypt([$L,$S,$D],Enigma,"SFBWD NJUSE GQOBH KRTAR EEZMW KPPRB XOHDR OEQGB BGTQV PGVKB VVGBI MHUSZ YDAJQ IROAX SSSNR EHYGG RPISE ZBOVM QIEMM ZCYSG QDGRE RVBIL EKXYQ IRGIR QNRDN VRXCY YTNJR"),
    ?assertEqual("DREIG EHTLA NGSAM ABERS IQERV ORWAE RTSXE INSSI EBENN ULLSE QSXUH RXROE MXEIN SXINF RGTXD REIXA UFFLI EGERS TRASZ EMITA NFANG XEINS SEQSX KMXKM XOSTW XKAME NECXK",Res2).
    
%% This requires support for 4 rotors.
u264_test() ->
    Enigma = enigma:setup("ThinB",["Beta","II","IV","I"],[1,1,1,22],[{$A,$T}, {$B,$L}, {$D,$F}, {$G,$J}, {$H,$M}, {$N,$W}, {$O,$P}, {$Q,$Y}, {$R,$Z}, {$V,$X}]),
    Res = enigma:crypt([$V,$J,$N,$A],Enigma,"NCZW VUSX PNYM INHZ XMQX SFWX WLKJ AHSH NMCO CCAK UQPM KCSM HKSE INJU SBLK IOSX CKUB HMLL XCSJ USRR DVKO HULX WCCB GVLI YXEO AHXR HKKF VDRE WEZL XOBA FGYU JQUK GRTV UKAM EURB VEKS UHHV OYHA BCJW MAKL FKLM YFVN RIZR VVRT KOFD ANJM OLBG FFLE OPRG TFLV RHOW OPBE KVWM UQFM PWPA RMFH AGKX IIBG"),
    ?assertEqual("VONV ONJL OOKS JHFF TTTE INSE INSD REIZ WOYY QNNS NEUN INHA LTXX BEIA NGRI FFUN TERW ASSE RGED RUEC KTYW ABOS XLET ZTER GEGN ERST ANDN ULAC HTDR EINU LUHR MARQ UANT ONJO TANE UNAC HTSE YHSD REIY ZWOZ WONU LGRA DYAC HTSM YSTO SSEN ACHX EKNS VIER MBFA ELLT YNNN NNNO OOVI ERYS ICHT EINS NULL",Res).

%% This test requires correct implementation of the "double stepping" of the middle rotor, as described in
%% https://home.comcast.net/~dhhamer/downloads/rotors1.pdf
%% as well as implementation of the higher numbered rotors that increment twice per revolution
scharnhorst_test() ->
    Enigma = enigma:setup("B",["III","VI","VIII"],[1,8,13],[{$A,$N}, {$E,$Z}, {$H,$K}, {$I,$J}, {$L,$R}, {$M,$Q}, {$O,$T}, {$P,$V}, {$S,$W}, {$U,$X}]),
    Res = enigma:crypt([$U,$Z,$V],Enigma,"YKAE NZAP MSCH ZBFO CUVM RMDP YCOF HADZ IZME FXTH FLOL PZLF GGBO TGOX GRET DWTJ IQHL MXVJ WKZU ASTR"),
    ?assertEqual("STEU EREJ TANA FJOR DJAN STAN DORT QUAA ACCC VIER NEUN NEUN ZWOF AHRT ZWON ULSM XXSC HARN HORS THCO",Res).

