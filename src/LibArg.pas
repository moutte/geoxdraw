UNIT LibArg; {normative calculation to clay-mineral components}

INTERFACE

USES libio;

PROCEDURE Init_LibArg(FrmIn:Forme; VAR FrmArg,FrmAtm:Forme; VAR RecAtom:Rec_Atom);
PROCEDURE Traite_Arg(FrmIn,FrmArg,FrmAtm:Forme; R:Rec_Ana; RecAtom:Rec_Atom);
PROCEDURE Close_LibArg;

IMPLEMENTATION

CONST
 lis_min = 	'QZCCABANKFGIKAMUCHILBITT';
 Lis_Atm = 	'SITIALFEMNMGCANAK.P.';
 Debug = 	False;

VAR
 Trc, F_arg: text;
 L: String;
 prm: ArrEle_Byte;
 Max_Anort: 		real; {maxi anorthite in plagio}

PROCEDURE Init_LibArg(FrmIn:Forme; VAR FrmArg,FrmAtm:Forme; VAR RecAtom:Rec_Atom);
VAR i: Byte;
BEGIN
 Calc_Recatom(FrmIn,RecAtom);
 L:= Lis_Atm;
 Ent_Prm(frmIn,FrmAtm,L,prm);
 WITH FrmAtm DO BEGIN
  FOR i:= 1 TO nb_ele DO tit_oxy[i]:= copy(Lis_Atm,2*i-1,2)
 END;

 Max_Anort:=35;

 FrmArg:= FrmIn;
 WITH FrmArg DO
  BEGIN
 {lis_min = 'QZCCABANKFGIKAMUCHILBI'}
   nb_ele:= Length(Lis_Min) DIV 2;
   FOR i:= 1 TO nb_ele DO tit_oxy[i]:= copy(lis_min,2*i-1,2)
  END;

 Assign(Trc,Dir_Dat+'TRACE_CLAY.TXT');Rewrite(Trc);

 {$I-}Assign(F_Arg, Dir_Dat+'ARGILE.TXT'); Rewrite(F_Arg);{$I+}
 TitChamp_TO_text(F_arg, frmIn);
 WITH FrmArg DO
   FOR i:= 1 TO nb_ele DO write(F_Arg,tit_oxy[i],#9);
 write(F_Arg,'AN100',#9);
 WITH FrmAtm DO
  FOR i:= 1 TO nb_ele DO write(F_Arg,tit_oxy[i],#9);
 writeln(F_Arg);
END;{Init_LibArg}

FUNCTION Sum_Vec(v: ArrEle_Sing; i1, i2: Byte): Single;
VAR S: Single; i: Byte;
BEGIN
 S:= 0.0; FOR i:= i1 TO i2 DO S:= S + v[i]; Sum_Vec:= S
END;{Sum_Vec}

PROCEDURE Traite_Arg(FrmIn,FrmArg,FrmAtm:Forme; R:Rec_Ana; RecAtom:Rec_Atom);
VAR
 j: byte;
 Az_, Alz_, Sz_, K_, FM_, Anort, PF_: real;
 T, v, w: arrele_sing;

PROCEDURE sortie;
BEGIN
 w[1]{QZ}:= w[1]{QZ} * 0.6;
 w[3]{AB}:= w[3]{AB} * 2.62;
 w[4]{AN}:= w[4]{AN} * 2.78;
 w[5]{KF}:= w[5]{KF} * 2.78;
 w[6]{GI}:= w[6]{GI} * 0.78;
 w[7]{KA}:= w[7]{KA} * 2.94;
 w[8]{MU}:= w[8]{MU} * 3.98;
 w[9]:= w[9]{CH} * (7.14*PF_ + (1-PF_)*5.54);
 w[10]:=w[10]{IL} * (4.13*PF_ + (1-PF_)*3.97);
 w[11]:=w[11]{BI} * (5.63*PF_ + (1-PF_)*4.67);
 w[12]:= Sum_Vec(w,1,11);

 Champ_TO_Text(F_arg, FrmArg, r.champ);
 Don_TO_Text(F_arg, FrmArg,Arr_Vrai,w);
 write(F_Arg,Real_Str(100*Anort),#9);
 Don_TO_Text(F_Arg,FrmAtm,Arr_Vrai,T);
 writeln(F_arg)
END;{sortie}

PROCEDURE KAMUCHQZ;{Az_>((15*K_ +2*FM_)/5)} {KA-MU-CH-QZ}
BEGIN
 w[8]{MU}:= K_;
 w[9]{CH}:= FM_/5;
 Az_:= Az_ - 2*w[9]{CH} - 3*w[8]{MU};
 Sz_:= Sz_ - 3*w[9]{CH} - 3*w[8]{MU};
 IF Az_<Sz_ THEN BEGIN
  {KA-MU-CH-QZ} w[7]{KA}:= Az_/4; w[1]{QZ}:= Sz_ - w[7]{KA}
 END
 ELSE BEGIN
  {KA-MU-CH-GI} w[1]{QZ}:= 0; w[7]{KA}:= Sz_/4; w[6]{GI}:= Az_ - Sz_
 END
END;{KA-MU-CH-QZ}

PROCEDURE ILMUKFQZ;{IL-MU-KF-QZ}
BEGIN
 w[10]{IL}:= 2*FM_;
 K_:= K_ - w[10]{IL}; Az_:= Az_ - 2*w[10]{IL};
 w[8]{MU}:= (Az_ - K_)/2; w[5]{KF}:= (3*K_ - Az_)/2;
 w[1]{QZ}:= Sz_ - 3*w[5]{KF} - 3*w[8]{MU} - 7*w[10]{IL}/2;
END;{IL-MU-KF-QZ}

PROCEDURE ILMUCHQZ;{Az_>((9*K_ +2*FM_)/5)}
BEGIN
 IF Az_<(3*K_ - 2*FM_) THEN ILMUKFQZ
  ELSE
  BEGIN {IL-MU-CH-QZ}
   w[9]{CH}:= (Az_ +2*FM_ - 3*K_)/12;
   w[10]{IL}:= (15*K_ + 2*FM_ - 5*Az_)/6;
   w[8]{MU}:= (5*Az_ - 9*K_ -2*FM_)/6;
   w[1]{QZ}:= Sz_ - 3*w[9]{CH} - 7*w[10]{IL}/2 - 3*w[8]{MU};
  END
END;{ILMUCHQZ}

PROCEDURE ILKFCHQZ;{Az_>((5*K_ +2*FM_)/5)}
BEGIN
 IF Az_>(K_+2*FM_) THEN ILMUKFQZ
  ELSE
  BEGIN {IL-KF-CH-QZ}
   w[9]{CH}:= (K_ + 2*FM_ - Az_)/8;
   w[10]{IL}:= (-5*K_ - 2*FM_ + 5*Az_)/4;
   w[5]{KF}:= (9*K_ +2*FM_ -5*Az_)/4;
   w[1]{QZ}:= Sz_ -3*w[9]{CH} -7*w[10]{IL}/2 -3*w[5]{KF};
  END
END;{ILKFCHQZ}

PROCEDURE BIKFCHQZ; {BI-KF-CH-QZ}
BEGIN
 w[9]{CH}:= (Az_ - K_)/2;
 w[11]{BI}:= (5*K_ +2*FM_ -5*Az_)/6;
 w[5]{KF}:= (5*Az_ +K_ -2*FM_)/6;
 w[1]{QZ}:= Sz_ -3*(w[9]{CH} +w[11]{BI} +w[5]{KF});
END;{BIKFCHQZ}

PROCEDURE carbonate;
VAR x, x_def: byte;
BEGIN
 x:= round(100*Anort);
 writeln(Trc,'SOUS-SATYRATION EN ALUMINE !!  AN% = '+ Real_Str(x));
 IF x>1 THEN x_def:= x-1 ELSE x_def:= 0;
 x:= x_def; {Fun_Byte(1,WYMax-1,'NOUVELLE VALEUR DE AN% ',x_def,0,x);}
 Anort:= x/100;
 w[4]{AN}:= w[3]{AB} * Anort / (1-Anort);
 w[2]{CC}:= v[7] - w[4]{AN};
 Alz_:= Az_ -2*w[4]{AN}
END;{carbonate}

BEGIN{traiter}
 v:= R.Don; v[0]:= 0;
 FOR j:= 1 TO 11 DO w[j]:= 0;
 WITH RecAtom DO
 FOR j:= 1 TO frmIn.Nb_Ele DO
  v[j]:= 100*v[j]/(Atomic_Weit[j] * Stoch[j] + 16.00 * Nb_Oxy[j])*Stoch[j];

 T:= v;
 prm_vec(prm,T);{IF F32 THEN T[4]:= T[4] + v[prm[4]+1];}
 v:= T;

 {SITIALFEMNMGCANAK.P.TT}
 {1 2 3 4 5 6 7 8 9 1011}
 {Qtz,Clc,Alb,Ant,Kfs,Gib,Kao,Mus,Chl,Ill,Bio}
 {QZ CC AB AN KF GI KA MU CH IL BI TT}
 {1  2  3  4  5  6  7  8  9  10 11 12}

 K_:= v[9];                        {K_.}
 FM_:= Sum_Vec(v,4,6);         {Fe+Mn+Mg}
 IF FM_>Epsilon THEN PF_:= Sum_Vec(v,4,5)/FM_ ELSE PF_:= 0.0;{Fe+Mn/Fe+Mn+Mg}
 v[7]:=v[7] - 5/3 * v[10]; IF v[7]<0 THEN v[7]:=0;
 w[4]{AN}:= v[7];{Ca}
 w[3]{AB}:= v[8];{Na}
 IF Sum_Vec(v,7,8)>Epsilon {Na+Ca}
 THEN Anort:= v[7]/(Sum_Vec(v,7,8))
 ELSE Anort:= 0;{An%/100}

 Writeln(Trc,'AN%= '+Real_Str(100*Anort), '_____', 'FM%= '+Real_Str(100*PF_));

 IF Anort>Max_Anort/100 THEN BEGIN
  Anort:= Max_Anort/100;
  w[4]{AN}:= w[3]{AB} * Anort / (1-Anort);
  w[2]{CC}:= v[7] - w[4]{AN};
 END;

 Sz_:= v[1]{Si}-3*w[3]{AB=NaAlSi3};           {Si residuel}
 Az_:= v[3]{Al}-w[3]  {AB=NaAlSi3};           {Al residuel}

 Writeln(Trc,'Al Left= '+Real_Str(Az_));

 IF Az_<2*w[4]{AN=CaAl2Si2}
 THEN REPEAT carbonate UNTIL (Alz_>=0) OR (Anort=0)
 ELSE Alz_:= Az_ -2*w[4]{AN=CaAl2Si2};

 IF Alz_<K_ THEN REPEAT carbonate UNTIL (Alz_>=K_) OR (Anort=0);

 IF Alz_<(2*FM_-K_)/5 THEN REPEAT carbonate UNTIL (Alz_>=(2*FM_-K_)/5) OR (Anort=0);
 Az_:= Alz_;
 Sz_:= Sz_ -2*w[4]{AN};
 IF Az_>((15*K_ +2*FM_)/5)
  THEN KAMUCHQZ
   ELSE
    IF Az_>((9*K_ +2*FM_)/5)
     THEN ILMUCHQZ
      ELSE
      IF Az_>((5*K_ +2*FM_)/5)
       THEN ILKFCHQZ
        ELSE BIKFCHQZ;

 Sortie
END;{traiter}

PROCEDURE Close_LibArg;
BEGIN WriteLn(F_Arg, '§'); CloseFile(F_Arg) END;

END.


