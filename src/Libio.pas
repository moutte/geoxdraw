UNIT LibIo;

INTERFACE

USES StdCtrls,Grids;

CONST
 Epsilon=1.0E-5;
 Fin_Car = ['!','£','õ','œ','§'];
 SepCar = #9;
 _t = #9;
 DirCar = '\';

 Main_Ok: Boolean = False;
 Trc_Open: Boolean = False;
//Zero_Is_Ok : Boolean = TRUE;
 Nega_Is_Ok : Boolean = TRUE;
 Signif = 7;
 Pad_Car = '.';
 Delim_Car='.';
 Max_Ele = 80;
 Max_Champ = 10;
 Max_Trm = 6;
 Lis_52  = 'SITIALFEMNMGCANAK.P.HULOC.F.CLB.LIRBCSBESRBASCV.CRCONICUZNGASNPBZRHFNBTAW.MOLACENDSMEUGDTBDYY.ERYBLUTHU.';

TYPE
 ModCnv = (Oxy,Met,Atm,Prm,Rha);
 Set_Car = Set Of Char;
 Str2 = String[2];
 Str_Signif = String[signif];

 Arrele_Real    = Array[0..Max_Ele] Of Real;
 Arrele_Sing 	= Array[0..Max_Ele] Of Single;
 Arrele_Byte 	= Array[0..Max_Ele] Of Byte;
 ArrEle_Word	= Array[0..Max_Ele] Of Word;
 Arrele_Bool 	= Array[0..Max_Ele] Of Boolean;
 Arrele_Str  	= Array[0..Max_Ele] Of Str_Signif;
 Arrele_Str2  	= Array[0..Max_Ele] Of Str2;

 Arrchamp_Str 	= Array[0..Max_Champ] Of Str_Signif;
 Arrchamp_Byte 	= Array[0..Max_Champ] Of Byte;

 Rec_Ana = RECORD
  Champ: Arrchamp_Str;
  Don: Arrele_Sing
 END;

 Rec_Str = RECORD
  Champ: Arrchamp_Str;
  DonStr: Arrele_Str
 END;

 Forme = RECORD {Describe The Format Of a Data File}
  Nom_Fil: String;
  No_Num: boolean;
  Nb_Champ, Nb_Ele: Byte;
  Lis_Ele: String;
  Tit_Champ: Arrchamp_Str;
  Tit_Oxy: Arrele_Str
 END;

 Rec_Atom = RECORD
  Nb_Ele: Byte;
  Lis_Ele: String;
  Tit_Ele, Tit_Suf: Arrele_Str2;
  Tit_Ox: ArrEle_Str;
  Atomic_Weit, Molar_Weit, TO_Met, TO_Oxy: Arrele_Sing;
  Valence, Nb_Oxy, Stoch: Arrele_Byte
 END;

 Expression = RECORD
  Atomic: Boolean;
  Factor: integer;
  Num_Tit, Den_Tit, Titre: String;
  Num_Ind, Den_Ind: Array[0..Max_Trm] Of Byte;
  Num_Kof, Den_Kof: Array[1..Max_Trm] Of Single;
  Num_Trm, Den_Trm: Byte
 END;

TYPE
 CoorXYZ     = (CorX, CorY, CorZ);
VAR
 ArAtm:             array[CoorXYZ] of Rec_Atom;
 ArExp:             array[CoorXYZ] of expression;

VAR
 Dir_Dat,Dir_Tmp,Dir_Stc,Dir_Ps:string;
 Log_File,Trc_File:string;

VAR
 Def_Prm:            Arrele_Byte;
 Arr_Vrai,Arr_Faux:  ArrEle_Bool;
 Vec_0: 	     ArrEle_Sing;
 Def_Exp:	     Expression;
 Def_RecAtom:        Rec_Atom;
 ArLisChamp:         Array[1..Max_Champ] Of String;
{for Spidergrams}
VAR
 PrmSpd:         ArrEle_Byte;
 FrmSpd:         Forme;
 SufSpd:         Str_Signif;
 SpdFile:        STRING;
 VecSpd:         ArrEle_Sing;

CONST
 Max_Spd = 12;
VAR
 ArLabSpd:              array[1..Max_Spd] of Str_Signif;

function R_Str(R: Real): String;
function R_StrI(I:byte;R: Real): String;
FUNCTION Fun_Intg(e:TEdit; Ch:Char; l:TLabel; zdef,zmin,zmax:integer): integer;
FUNCTION Fun_Sing(e:TEdit; Ch:Char; l:TLabel; zdef,zmin,zmax:single): single;
FUNCTION Fun_Intg2(e:TEdit; Ch:Char; zdef,zmin,zmax:integer): integer;
FUNCTION Fun_Sing2(e:TEdit; Ch:Char; zdef,zmin,zmax:single): single;
FUNCTION Test_Dir(s:string): BOOLEAN;
FUNCTION Position(N: Byte; T, S: String): Byte;

PROCEDURE Text_TO_Recana(Skip:boolean;VAR F:TextFile;Frm:Forme;VAR R:Rec_Ana;VAR N: word; VAR b:Boolean);
PROCEDURE Text_TO_RecStr(VAR F:TextFile;Frm:Forme;VAR R:Rec_Str;VAR b:Boolean);
PROCEDURE Text_TO_Forme(VAR F:TextFile;VAR Frm:Forme;LirLis:BOOLEAN;VAR OK:BOOLEAN);

PROCEDURE TitChamp_TO_Text(VAR OutF:TextFile;Frm:Forme);
PROCEDURE Forme_TO_Text(VAR OutF:TextFile;Frm:Forme);
PROCEDURE Champ_TO_Text(VAR OutF:TextFile;Frm:Forme;Arr_C:ArrChamp_Str);

PROCEDURE Don_TO_TextOld(VAR OutF:TextFile;Frm:Forme;Arr_B:ArrEle_Bool;Arr_S:ArrEle_Sing);
PROCEDURE Don_TO_Text(VAR OutF:TextFile;Frm:Forme;Arr_B:ArrEle_Bool;Arr_S:ArrEle_Sing);
PROCEDURE Don_TO_Text2(VAR OutF:TextFile;Old:Boolean;Frm:Forme;Arr_B:ArrEle_Bool;Arr_S:ArrEle_Sing);

PROCEDURE RecAna_TO_Text(VAR OutF:TextFile;Frm:Forme;Arr_B:ArrEle_Bool;R: Rec_Ana);

PROCEDURE Concat_Lis(L: Byte; T: String; VAR S: String);

procedure Forme_To_Grid(Frm:Forme;G:TStringGrid;C:Byte);
procedure Don_To_Grid(Frm:Forme;G:TStringGrid;S:String;A:ArrEle_Sing;C:Byte);
procedure Champ_To_Grid(Frm:Forme;G:TStringGrid;A:ArrChamp_Str;C:Byte);

PROCEDURE Calc_Recatom(Frm: Forme; VAR R: Rec_Atom);

PROCEDURE Skip_Rite(c: Set_Car; VAR S: String);
PROCEDURE Skip_Left(c: Set_Car; VAR S: String);
FUNCTION Real_Str(R: Real): Str_Signif;
FUNCTION Real_StrOld(R: Real): Str_Signif;
FUNCTION Log10(R: real): real;

PROCEDURE Ent_Prm(F0: Forme; VAR F1: Forme; VAR S1: String; VAR Prm: Arrele_Byte);
PROCEDURE Ent_Prm2(F0: Forme; VAR F1: Forme; VAR S1: String; VAR Prm: Arrele_Byte);
PROCEDURE Prm_Vec(Prm: Arrele_Byte; VAR v: Arrele_Sing);
PROCEDURE Prm_Vec2(Prm:Arrele_Byte;Dim:Byte;VAR v: Arrele_Sing);
//PROCEDURE Ent_SpdCustom(F0:Forme;VAR F1:Forme;VAR Prm:ArrEle_Byte;VAR v25:ArrSpd_Real);
//PROCEDURE Ent_Spd(M:ModSpd;F0:Forme;VAR F1:Forme;VAR Prm:ArrEle_Byte;VAR v25:ArrSpd_Real);
PROCEDURE Calc_GdSm(Prm: ArrEle_Byte;Dim: Byte;V_Spd:ArrEle_Sing;VAR w: ArrEle_Sing);
PROCEDURE Stc_Vec(S: Str_Signif; D: Byte; R: Rec_Atom; VAR v: Arrele_Sing);

FUNCTION Calc_Exp(v: Arrele_Sing; Atm: Rec_Atom; Exp: Expression; VAR Ok: Boolean): Single;
PROCEDURE Titr_Exp(VAR Exp: Expression; Frm: Forme; Atm: Rec_Atom);
//PROCEDURE Ent_Exp(b: Arrele_Bool; Frm: Forme; VAR Atm: Rec_Atom; VAR Exp: Expression);

PROCEDURE Cnv_Vec(Mode: ModCnv; R:Rec_Atom; Dim:Byte; VAR V:ArrEle_Sing);
PROCEDURE Nettoie(VAR S: Str_Signif);

IMPLEMENTATION

USES Sysutils;

CONST
 Def_Str ='z';
 Def_Oxy:Byte= 24;

 Lis_Tou1 = 'H.LIBEB.C.N.O.F.NAMGALSIP.S.CLK.CASCTIV.CRMNFECONICUZNGAGEASSERBSR';
 Lis_Tou2 = 'Y.ZRNBMOAGCDINSNSBCSBALACENDSMEUGDTBDYHOERYBLUHFTAW.PBBITHU.';
{
H.LIBEB.C.N.O.F.NAMGALSIP.S.CLK.CASCTIV.CRMNFECONICUZNGAGEASSERBSRY.ZRNBMOAGCDINSNSBCSBALACENDSMEUGDTBDYHOERYBLUHFTAW.PBBITHU.
}
 poid_atom: array[0..63] of single  =
(1,1,6.94, 9.012,10.81,12.01,14.01,16,   19,   22.99,24.31,26.98,28.09,30.97,32.06,
     35.45,39.10,40.08,44.96,47.90,50.94,52.0, 54.94,55.85,58.93,
     58.71,63.54,65.37,69.72,72.59,74.92,78.96,85.47,87.62,88.9,
     91.22,92.91,95.94,107.9,112.4,114.8,118.7,121.8,132.9,137.3,
     138.9,140.1,144.2,150.3,152,  157.2,158.9,162.5,164.9,167.3,173,  175,
     178.5,180.9,183.8,207.2,209,  232,  238);
  {H LI    BE    B.    C.    N.    O.    F.    NA    MG    AL    SI    P.    S.}
    {CL    K.    CA    SC    TI    V.    CR    MN    FE    CO}
    {NI    CU    ZN    GA    GE    AS    SE    RB    SR    Y.}
    {ZR    NB    MO    AG    CD    IN    SN    SB    CS    BA}
    {LA    CE    ND    SM    EU    GD    TB    DY    HO    ER    YB    LU}
    {HF    TA    W.    PB    BI    TH    U.}

 Nb_Cation: array[0..63] of byte    =
(1,1, 1,    2,    3,    4,    4,    0,    0,    1,    2,    3,    4,    5,    4,
  {H LI    BE    B.    C.    N.    O.    F.    NA    MG    AL    SI    P.    S.}
      0,    1,    2,    3,    4,    3,    3,    2,    3,    2,
    {CL    K.    CA    SC    TI    V.    CR    MN    FE    CO}
      2,    2,    2,    3,    3,    3,    3,    1,    2,    3,
    {NI    CU    ZN    GA    GE    AS    SE    RB    SR    Y.}
      4,    5,    5,    1,    1,    1,    4,    3,    1,    2,
    {ZR    NB    MO    AG    CD    IN    SN    SB    CS    BA}
      3,    3,    3,    3,    3,    3,    3,    3,    3,    3,    3,    3,
    {LA    CE    ND    SM    EU    GD    TB    DY    HO    ER    YB    LU}
      4,    5,    6,    2,    2,    4,    4);
    {HF    TA    W.    PB    BI    TH    U.}

VAR
 Ana_0: Rec_Ana;
 Def_Champ: ArrChamp_Str;

PROCEDURE Nettoie(VAR S: Str_Signif);
BEGIN
 WHILE s[1] IN ['_','0'] DO Delete(S,1,1);
 WHILE s[length(s)]='.' DO Delete(S,length(s),1);
 IF S='' THEN S:='_'
END;

function R_StrI(I:byte;R: Real): String;
var S: String;
begin
{s1:=FloatToStrF(R,ffFixed,6,3);s2:=FloatToStrF(R,ffGeneral,5,6);}
 s:=FloatToStrF(R,ffGeneral,i,6);
 R_StrI:=s
end;

function R_Str(R: Real): String;
var S: String;
begin
{s1:=FloatToStrF(R,ffFixed,6,3);s2:=FloatToStrF(R,ffGeneral,5,6);}
 s:=FloatToStrF(R,ffGeneral,3,6);
 R_Str:=s
end;

function R_StrMin(R, Min: Real): String;
var S: String;
begin
{s1:=FloatToStrF(R,ffFixed,6,3);s2:=FloatToStrF(R,ffGeneral,5,6);}
 if ABS(R)<Min then s:='_' else s:=FloatToStrF(R,ffGeneral,5,6);
 R_StrMin:=s
end;

function R_Str_(R: Real; l: byte): String;
var S: String;
begin
{s1:=FloatToStrF(R,ffFixed,6,3);s2:=FloatToStrF(R,ffGeneral,5,6);}
 s:=FloatToStrF(R,ffGeneral,5,6);
{if l>0 then while length(s)<l do s:=s+' ';}
 if (length(s)>l) and (abs(R)<1) then s:=copy(s,1,l);
 R_Str_:=s
end;

procedure Forme_To_Grid(Frm:Forme;G:TStringGrid;C:Byte);
var CurCol,CurRow: integer;
begin
 with G do begin
  ColCount:=C+1;{C=Nbre de Colonnes Utiles};
  RowCount:=Frm.Nb_Champ+Frm.Nb_Ele;
  for CurCol:= 0 to ColCount - 1 do
   for CurRow:= 0 to RowCount - 1 do Cells[CurCol,CurRow]:='';
  for CurRow:= 1 to Frm.Nb_Champ do Cells[0,CurRow]:=Frm.Tit_Champ[CurRow];
  with Frm do for CurRow:= 1 to Nb_Ele do Cells[0,CurRow+Nb_Champ]:=Tit_Oxy[CurRow];
 end;
end;{Forme_To_Grid}

procedure Champ_To_Grid(Frm:Forme;G:TStringGrid;A:ArrChamp_Str;C:Byte);
var CurRow: integer;
begin
 with G do
  for CurRow:= 1 to Frm.Nb_Champ do Cells[C,CurRow]:=A[CurRow]
end;{Champ_To_Grid}

procedure Don_To_Grid(Frm:Forme;G:TStringGrid;S:String;A:ArrEle_Sing;C:Byte);
var CurRow: integer;
begin
 if C<G.ColCount then
 with G do begin
  Cells[C,0]:=S;
  for CurRow:= 1 to Frm.Nb_Ele do
   Cells[C,Frm.Nb_Champ+CurRow]:=R_Str(A[CurRow])
 end
end;{Don_To_Grid}

PROCEDURE Make_TitOxy(Tit_Ele: ArrEle_Str2; Dim: Byte; v: ArrEle_Byte; VAR Tit_Oxy: ArrEle_Str);
VAR s: String; i: Byte;
BEGIN
 FOR i:= 1 TO Dim DO BEGIN
  s:= Tit_Ele[i];
  IF s[2]='.' THEN SetLength(s,1);
  if s[2] in ['A'..'Z'] then s[2]:=chr(ord(s[2])+32);
  CASE V[i] OF
   0: Tit_Oxy[i]:= s;
   1: Tit_Oxy[i]:= s+'2O';
   2: Tit_Oxy[i]:= s+'O';
   3: Tit_Oxy[i]:= s+'2O3';
   4: Tit_Oxy[i]:= s+'O2';
   5: Tit_Oxy[i]:= s+'2O5';
   6: Tit_Oxy[i]:= s+'O3';
  END
 END
END;{Make_TitOxy}

function Fun_Intg(e:TEdit; Ch:Char; l:TLabel; zdef,zmin,zmax:integer): integer;
var T:string; z:integer; b:integer;
begin
 l.Caption:=ch+' in '+IntToStr(zmin)+'..'+IntToStr(zmax);
 if zdef<zmin then zdef:=zmin; if zdef>zmax then zdef:=zmax;
 T:=e.Text;
 if pos(',',T)>0 then T[pos(',',T)]:='.';
 val(T, z, b);
 if pos(',',T)>0 then T[pos(',',T)]:='.';
 IF b<>0 THEN val('0'+T,z,b);
 if (b=0) and (z>=zmin) and (z<=zmax) then Fun_Intg:=z
 else Fun_Intg:=zdef
end;{Fun_Intg}

function Fun_Intg2(e:TEdit; Ch:Char; zdef,zmin,zmax:integer): integer;
var T:string; z:integer; b:integer;
begin
 e.Hint:=ch+' in '+IntToStr(zmin)+'..'+IntToStr(zmax);
 if zdef<zmin then zdef:=zmin; if zdef>zmax then zdef:=zmax;
 T:=e.Text;
 if pos(',',T)>0 then T[pos(',',T)]:='.';
 val(T, z, b); IF b<>0 THEN val('0'+T,z,b);
 if (b=0) and (z>=zmin) and (z<=zmax) then Fun_Intg2:=z
 else Fun_Intg2:=zdef
end;{Fun_Intg}

function Fun_Sing(e:TEdit; Ch:Char; l:TLabel; zdef,zmin,zmax:single): single;
var T:string; z:single; b:integer;
begin
 l.Caption:=ch+' in '+R_Str(zmin)+'..'+R_Str(zmax);
 if zdef<zmin then zdef:=zmin; if zdef>zmax then zdef:=zmax;
 T:=e.Text;
 if pos(',',T)>0 then T[pos(',',T)]:='.';
 val(T, z, b);
 IF b<>0 THEN begin T:='0'+T; val('0'+T,z,b); end;
 if (b=0) and (z>=zmin) and (z<=zmax) then Fun_Sing:=StrToInt(T)
 else Fun_Sing:=zdef
end;{Fun_Sing}

function Fun_Sing2(e:TEdit; Ch:Char; zdef,zmin,zmax:single): single;
var T:string; z:single; b:integer;
begin
 e.Hint:=ch+' in '+R_Str(zmin)+'..'+R_Str(zmax);
 if zdef<zmin then zdef:=zmin; if zdef>zmax then zdef:=zmax;
 T:=e.Text;
 if pos(',',T)>0 then T[pos(',',T)]:='.';
 val(T, z, b); IF b<>0 THEN val('0'+T,z,b);
 if (b=0) and (z>=zmin) and (z<=zmax) then Fun_Sing2:=z
 else Fun_Sing2:=zdef
end;{Fun_Sing}

FUNCTION Test_Dir(s:string): BOOLEAN;
BEGIN
 Test_Dir:=TRUE;
 if not DirectoryExists(s) then
 if ForceDirectories(s) then Test_Dir:=TRUE
 else Test_Dir:=FALSE
END;{Test_Dir}

FUNCTION Pad_Left(c: Char; S: String; Len: Word): String;
BEGIN
 WHILE Length(S)<Len DO s:=c+s;
 SetLength(S,Len); Pad_Left:= S
END; {Pad}

FUNCTION Pad_Rite(c: Char; S: String; Len: Word): String;
BEGIN
 IF Length(S)<Len THEN Fillchar(S[Succ(Length(S))], Len - Length(S), c);
 SetLength(S,Len); Pad_Rite:= S
END; {Pad}

PROCEDURE Skip_Rite(c: Set_Car; VAR S: String);
BEGIN
 IF (Length(S)>1) THEN
 WHILE (Length(S)>1) AND (S[Length(S)]IN C) DO DELETE(S,Length(S),1);
END; {Skip}

PROCEDURE Skip_Left(c: Set_Car; VAR S: String);
BEGIN
 WHILE s[1] IN C DO Delete(S,1,1);
 IF s='' THEN S:='_'
END; {Skip}

PROCEDURE Text_TO_Recana(skip:Boolean;VAR F:TextFile;Frm: Forme;VAR R:Rec_Ana;VAR N: word; VAR b:Boolean);
//skip: skip fill characters on rite side of Champ
VAR
 s, t: String;
 i, j, k: word; 
 x: Single; 
 m: Integer;
BEGIN
 R:=Ana_0;
 if not Eof(F) then repeat
  readln(F,s)
 until not (s[1] in ['*','.','$']);
//skip lines that begin with '*','.','$'
//while (not Eof(f)) and (s[1] in ['*','.','$']) do readln(F,s);
//if (s[1] in ['*','.','$']) then readln(F,s);
 b:=Eof(F) or (s='') or (s[1] in Fin_Car);

 IF NOT B THEN BEGIN
  inc(N);
  i:=0; j:=0; k:=0; SetLength(t,1); t:='_';
  if Frm.No_Num then begin
   k:=1;
   R.Champ[k]:=Pad_Left('_',IntToStr(N),signif);//'x......';
  end;

  REPEAT
   inc(i); inc(j);
   if (i>length(s)) or (s[i]=#9) then begin
    inc(k); Trim(t);
//if i<Frm.Nb_Champ+1 then nm:=signif else n:=16;{added for Diaphore result files}
//t:= StringOfChar('.',n);
    IF k<=Frm.Nb_Champ THEN BEGIN
     WHILE length(t)<signif do t:=t+'.';
     R.Champ[k]:=copy(t,1,signif);
     IF Skip THEN Skip_Rite(['.'],t)
    END
    ELSE IF (k<=Frm.Nb_Champ+Frm.Nb_Ele) THEN BEGIN
     if pos(',',T)>0 then T[pos(',',T)]:='.';
     IF t[1]='.' THEN t:='0'+t;
     Val(t, x, m);
     IF m=0 THEN x:=StrToFloat(t) ELSE x:=0.0;
     //IF ((x<0) AND NOT Nega_Is_Ok) OR (m>0) THEN x:=-1;//modif200603
     R.Don[k-Frm.Nb_Champ]:=x
    END;

    SetLength(t,1); t:='_'; j:=0;//reset the string t
   END
   ELSE BEGIN
    SetLength(t,j); t[j]:=s[i]
   END
  UNTIL (k=Frm.Nb_Champ+Frm.Nb_Ele) or (i>length(s));
 END;//if not b
 R.Don[0]:= 0.0
END;//Text_TO_Recana

PROCEDURE Text_TO_RecStr(VAR F:TextFile;Frm:Forme;VAR R:Rec_Str;VAR b:Boolean);
VAR
 s, t: String;
 i, j, k: word;
BEGIN
 for i:=1 to frm.Nb_Ele do R.DonStr[i]:='_';
 if not Eof(F) then readln(F,s);
//skip lines that begin with '*','.','$'
 while (not Eof(f)) and (s[1] in ['*','.','$']) do readln(F,s);
 b:=Eof(F) or (s='') or (s[1] in Fin_Car);
 i:=0;
 j:=0; k:=0; SetLength(t,1); t:='_';
 if not b then repeat
  inc(i); inc(j);
  if (i>length(s)) or (s[i]=#9) then begin
   inc(k);
//if i<Frm.Nb_Champ+1 then m:=signif else m:=16;{added for Diaphore result files}
//t:= StringOfChar('.',m);
   IF k<Frm.Nb_Champ+1 THEN R.Champ[k]:=t
   ELSE IF (k<=Frm.Nb_Champ+Frm.Nb_Ele) THEN BEGIN
    R.DonStr[k-Frm.Nb_Champ]:=t
   END;
   SetLength(t,1); t:='_'; j:=0;//reset the string t
  end
  else begin
   SetLength(t,j); t[j]:=s[i]
  end
 until (k=Frm.Nb_Champ+Frm.Nb_Ele) or (i>length(s));
 R.DonStr[0]:='__'
END;//Text_TO_RecStr

PROCEDURE TitChamp_TO_Text(VAR OutF:TextFile;Frm:Forme);
VAR i:Byte; s:string;
BEGIN
 WITH Frm DO FOR i:= 1 TO Nb_Champ DO BEGIN
  s:=copy(Tit_Champ[i],1,signif-1); Skip_Rite(['.'],s);
  Write(OutF, Delim_Car, s, SepCar)
 END
END;

PROCEDURE Forme_TO_Text(VAR OutF:TextFile;Frm:Forme);
VAR i:Byte;
BEGIN
 IF Frm.Nom_Fil<>'' THEN Writeln(OutF,Frm.Nom_Fil);
{Champ}
 TitChamp_TO_Text(OutF,Frm);
{Don}
 WITH Frm DO
  FOR i:= 1 TO Nb_Ele DO Write(OutF, copy(Tit_Oxy[i],1,signif),SepCar);
 Writeln(OutF)
END;{Forme_TO_Text}

PROCEDURE Champ_TO_Text(VAR OutF:TextFile;Frm:Forme;Arr_C:ArrChamp_Str);
VAR i: Byte; s:string;
BEGIN
 FOR i:= 1 TO Frm.Nb_Champ DO BEGIN
  s:=Arr_C[i]; Skip_Rite(['.'],s); Write(OutF,s,SepCar)
 END
 {FOR i:= 1 TO Frm.Nb_Champ DO Write(OutF,Arr_C[i],SepCar)}
END;{Champ_TO_Text}

FUNCTION Real_StrOld(R: Real): Str_Signif;
VAR S: String;
BEGIN
 IF Abs(R/1.0E3)>32E+03 THEN R:= 0.0;
 IF ABS(R)<0.0001 THEN S:= '0.0'
  ELSE IF Abs(R)<0.001 THEN BEGIN
   Str(R:7:4,S);
   IF R>0 THEN WHILE s[1]<>'.' DO Delete(S,1,1)
  END
  ELSE IF Abs(R)<1 THEN BEGIN Str(R:5:3,S); IF R>0 THEN Delete(S,1,1) END
   ELSE IF Abs(R)<10 THEN Str(R:5:2,S)
    ELSE IF (Abs(R)<100) THEN Str(R:5:1,S)
     ELSE IF (Abs(R)<1.0E5) THEN Str(Trunc(R):5,S)
      ELSE BEGIN R:= R/1.0E3; Str(Trunc(R):3,S); S:= S + 'E3' END;
 Trim(s);
 IF s='' THEN s:='0';
 WHILE (Pos('.',S)<>0) And (S[Length(S)]='0') DO Delete(S,Length(S),1);
 IF s='.' THEN s:='0';
 {WHILE S[Length(S)]='.' DO Delete(S,Length(S),1);}
 Real_StrOld:= S;
 //IF R=1 THEN Real_StrOld:='1';
END; {Real_StrOld}

function Real_Str(R: Real): Str_Signif;
var S: String;
begin
{s1:=FloatToStrF(R,ffFixed,6,3);s2:=FloatToStrF(R,ffGeneral,5,6);}
 s:=FloatToStrF(R,ffGeneral,5,6);
{if l>0 then while length(s)<l do s:=s+' ';}
 WHILE (Pos('.',S)<>0) And (S[Length(S)]='0') DO Delete(S,Length(S),1);
 IF (Pos('.',S)=2) And (S[1]='0') THEN Delete(S,1,1);
 IF s='.' THEN s:='0';
 if (length(s)>signif) and (abs(R)<1) and (pos('E',s)=0) then s:=copy(s,1,signif);
 Real_Str:=s
end;

PROCEDURE Don_TO_Text(VAR OutF:TextFile;Frm:Forme;Arr_B:ArrEle_Bool;Arr_S:ArrEle_Sing);
VAR i: Byte;
BEGIN
 FOR i:= 1 TO Frm.Nb_Ele DO IF Arr_B[i] THEN
 {IF Arr_S[i]>0 THEN Write(OutF, Real_Str(Arr_S[i]), SepCar) ELSE Write(OutF, '0.0', SepCar)}
 Write(OutF, R_Str(Arr_S[i]), SepCar)
 ELSE Write(OutF, '_', SepCar)
END;{Don_TO_Text}

PROCEDURE Don_TO_TextOld(VAR OutF:TextFile;Frm:Forme;Arr_B:ArrEle_Bool;Arr_S:ArrEle_Sing);
VAR i: Byte;
BEGIN
 FOR i:= 1 TO Frm.Nb_Ele DO IF Arr_B[i] THEN
 {IF Arr_S[i]>0 THEN Write(OutF, Real_Str(Arr_S[i]), SepCar) ELSE Write(OutF, '0.0', SepCar)}
 Write(OutF, Real_StrOld(Arr_S[i]), SepCar)
 ELSE Write(OutF, '_', SepCar)
END;{Don_TO_Text}

PROCEDURE Don_TO_Text2(VAR OutF:TextFile;Old:Boolean;Frm:Forme;Arr_B:ArrEle_Bool;Arr_S:ArrEle_Sing);
VAR i: Byte;
BEGIN
 FOR i:= 1 TO Frm.Nb_Ele DO IF Arr_B[i] THEN
 IF Old THEN Write(OutF, Real_StrOld(Arr_S[i]), SepCar)
 ELSE Write(OutF, Real_Str(Arr_S[i]), SepCar)
END;{Don_TO_Text}

PROCEDURE RecAna_TO_Text(VAR OutF:TextFile;Frm:Forme;Arr_B:ArrEle_Bool;R: Rec_Ana);
BEGIN
 WITH R DO BEGIN
  Champ_TO_Text(OutF,Frm,Champ);
  IF Frm.Nb_Ele>0 THEN Don_TO_Text(OutF,Frm,Arr_B,Don)
 END;
 WriteLn(OutF)
END;{RecAna_TO_Text}

FUNCTION Position(N: Byte; T, S: String): Byte;
VAR p: Byte;
BEGIN
 p:= 1;
 if T[2]=' ' then T[2]:='.';
 T:= UpperCase(Pad_Rite('.',T ,N));
 WHILE (UpperCase(Copy(S,1,N))<>T) And (S<>'') DO BEGIN Delete(S, 1, N); Inc(p) END;
 IF S='' THEN p:= 0; Position:= p
END; {Position}

FUNCTION Fun_Poid_Atom(T: Str2): Single;
BEGIN Fun_Poid_Atom:= Poid_Atom[Position(2,T,Lis_Tou1+Lis_Tou2)] END;

PROCEDURE Traiter_Titoxy(M: Str_Signif; VAR T, F: Str2; VAR Ox, Sh: Byte);
VAR j: Byte;
BEGIN
 F:='';
 IF Length(M)=1 THEN M:= M + '.';
 IF (M='LOI') THEN M:= 'H2O';
 IF (M='CO2') THEN M:= 'C.O2';
 IF (M[2]='O') And (M[1] IN ['U','P','S','W']) THEN Insert('.',M,2);
 T:= '..'; Ox:= 0; Sh:= 1; j:= 0;
 WHILE (M<>'') And (M[1] IN ['a'..'z', 'A'..'Z', '.']) And (j<2) DO BEGIN
  j:= j+1; T[j]:= Upcase(M[1]); Delete(M,1,1)
 END;
 IF (M<>'') THEN BEGIN
  IF M[1] IN ['0'..'9'] THEN BEGIN Sh:=ORD(M[1])-ORD('0'); Delete(M,1,1) END;
  IF M[1] IN ['o','O'] THEN BEGIN Ox:= 1; Delete(M,1,1) END;
  IF M<>'' THEN IF M[1] IN ['0'..'9'] THEN Ox:=ORD(M[1])-ORD('0');
  IF M[1]='_' THEN BEGIN Delete(M,1,1); F:=M END
 END;
 if T[2] in ['A'..'Z'] then T[2]:=chr(ord(T[2])+32);
END; {Traiter_Titoxy}

PROCEDURE Calc_Recatom(Frm: Forme; VAR R: Rec_Atom);
VAR
 i, j: Byte; x: Real;
BEGIN
 R.Tit_Ele[0]:= '';
 R.Lis_Ele:= '';
 R.Nb_Ele:= Frm.Nb_Ele;
 WITH R DO
 FOR i:= 1 TO Nb_Ele DO BEGIN
  Traiter_Titoxy(Frm.Tit_Oxy[i], Tit_Ele[i], Tit_Suf[i], Nb_Oxy[i], Stoch[i]);
  Lis_Ele:= Lis_Ele + Tit_Ele[i];
  IF (Nb_Oxy[i]=0) AND (Stoch[i]=1) THEN x:=1.0E4 ELSE x:=1.00;
  IF (Tit_Ele[i]='F.') OR (Tit_Ele[i]='CL') THEN x:= 1;
  Atomic_Weit[i]:= x*Fun_Poid_Atom(Tit_Ele[i]);
  Molar_Weit[i]:= Atomic_Weit[i] * Stoch[i] + 16.00 * Nb_Oxy[i];
 END; {FOR}

{Oxyde_TO_Metal, From Percent Oxyde TO Ppm Metal}
 WITH R DO FOR i:= 1 TO Nb_Ele DO
 IF (Nb_Oxy[i]<>0) AND (Stoch[i]<>0) THEN
 To_Met[i]:= 1.0E4* Stoch[i]* Atomic_Weit[i]/ (Atomic_Weit[i] * Stoch[i] + 16.00 * Nb_Oxy[i])
 ELSE
 To_Met[i]:= 1.0;
{Oxyde_TO_Metal}

{Metal_TO_Oxyde, From Ppm Metal TO Percent Oxyde}
 WITH R DO FOR i:= 1 TO Nb_Ele DO BEGIN
  j:= Position(2,Tit_Ele[i],Lis_Tou1+Lis_Tou2);
  IF j>0 THEN BEGIN
   Valence[i]:= Nb_Cation[j]; {Valence}
   x:= Poid_Atom[j];
   To_Oxy[i]:=((Nb_Cation[j]*8.0+x)/x)*1.0E-4
  END
  ELSE To_Oxy[i]:= 1.0
 END;
{Metal_TO_Oxyde}

 Make_TitOxy(R.Tit_Ele,R.Nb_Ele,R.Valence,R.Tit_Ox);
END; {Calc_Recatom}

PROCEDURE Concat_Lis(L:Byte; T:String; VAR S:String);
VAR b: Boolean;
BEGIN
 b:= (Length(S)<(245-L)) And (Position(L,T,S)=0);
 IF b THEN S:= S + UpperCase(Pad_Rite('.',T,L))     
END; {Concat_Lis}

FUNCTION Log10(R: real): real; BEGIN Log10:= ln(R)/ln(10) END;

PROCEDURE Prm_Vec(Prm: Arrele_Byte; VAR v: Arrele_Sing);
VAR i: Byte; w: Arrele_Sing;
BEGIN
 w[0]:=0.0;
 FOR i:= 1 TO Max_Ele DO IF Prm[i]<>0 THEN w[i]:= v[Prm[i]] ELSE w[i]:= 0.0;
 v:= w
END;

PROCEDURE Prm_Vec2(Prm:Arrele_Byte;Dim:Byte;VAR v: Arrele_Sing);
VAR i: Byte; w: Arrele_Sing;
BEGIN
 FOR i:= 1 TO Dim DO IF Prm[i]<>0 THEN w[i]:= v[Prm[i]] ELSE w[i]:= 0.0;
 v:= w
END;{Prm_Vec2}

PROCEDURE Ent_Prm(F0: Forme; VAR F1: Forme; VAR S1: String; VAR Prm: Arrele_Byte);
VAR S0: String; i: Byte;
BEGIN
 F1:= F0;{only alfa fields will be affected..}
 FOR i:= 0 TO Max_Ele DO Prm[i]:= i;
 Calc_Recatom(F0, Def_Recatom);
 S0:='';
 WITH Def_Recatom DO FOR i:= 1 TO Nb_Ele DO S0:= S0 + Tit_Ele[i];
 S0:=Def_Recatom.Lis_Ele;

 S0:=F0.Lis_Ele;
 IF S1<>'' THEN BEGIN
  F1.Nb_Ele:= Length(S1) Div 2;
  FOR i:= 1 TO F1.Nb_Ele DO BEGIN
   Prm[i]:= Position(2,Copy(S1, 2*i-1, 2), S0);
   IF Prm[i]<>0 THEN F1.Tit_Oxy[i]:={Def_Recatom.Tit_Ele[Prm[i]]} F0.Tit_Oxy[Prm[i]]
    ELSE F1.Tit_Oxy[i]:= Copy(S1, 2*i-1, 2)
  END {WITH}
 END ELSE S1:= S0
END;{Ent_Prm}

PROCEDURE Ent_Prm2(F0: Forme; VAR F1: Forme; VAR S1: String; VAR Prm: Arrele_Byte);
VAR S0,S2: String; i: Byte;
BEGIN
 F1:= F0; FOR i:= 0 TO Max_Ele DO Prm[i]:= i;
 Calc_Recatom(F0, Def_Recatom);
 S0:='';
 WITH Def_Recatom DO FOR i:= 1 TO Nb_Ele DO S0:= S0 + Tit_Ele[i];
{first take out of S1, the new Lis_Ele,
 the elements that are not in the original file}
 S2:='';
 FOR i:=1 TO Length(S1) Div 2 DO
  IF Position(2,copy(S1,2*i-1,2),S0)<>0 THEN S2:=S2+copy(S1,2*i-1,2);
 S1:=S2;
{}
 IF S1<>'' THEN BEGIN
  F1.Nb_Ele:= Length(S1) Div 2;
  WITH F1 DO FOR i:= 1 TO Nb_Ele DO BEGIN
   Prm[i]:= Position(2,Copy(S1, 2*i-1, 2), S0);
   IF Prm[i]<>0 THEN Tit_Oxy[i]:= F0.Tit_Oxy[Prm[i]]
    ELSE Tit_Oxy[i]:= Copy(S1, 2*i-1, 2)
  END {WITH}
 END ELSE S1:= S0
END;{Ent_Prm2}

PROCEDURE Calc_GdSm(Prm:ArrEle_Byte;Dim:Byte;V_Spd:ArrEle_Sing;VAR w:ArrEle_Sing);
VAR
 v: ArrEle_Sing;
 i: Byte;
BEGIN
 FOR i:= 1 TO Dim DO
 IF w[i]>0.0 THEN v[i]:= Ln(w[i]/V_Spd[i])
 ELSE v[i]:= 0;
(*Interpolation for Gd if Sm<>0 and/or Dy<>0***********************************)
 IF v[8]=0 THEN BEGIN
  IF (v[6]<>0){Sm} AND (v[10]<>0){Dy} THEN v[8]:= v[6]+(v[10]-v[6])/2;
  IF (v[6]=0){Sm} AND (v[10]<>0){Dy} THEN BEGIN
   IF (v[4]<>0){ND} THEN BEGIN
    v[6]:= v[4]+(v[10]-v[4])*1/3;
    v[8]:= v[4]+(v[10]-v[4])*2/3;
   END
   ELSE IF (v[2]<>0){CE} THEN BEGIN
    v[6]:= v[2]+(v[10]-v[2])*1/2;
    v[8]:= v[2]+(v[10]-v[2])*3/4;
   END
  END;
 END;
(***)
 FOR i:= 1 TO Dim DO
 IF V[i]>0.0 THEN W[i]:= V_Spd[i]*Exp(V[i])
 ELSE W[i]:= 0.0;
END;{Calc_Spd}

PROCEDURE Stc_Vec(S: Str_Signif; D: Byte; R: Rec_Atom; VAR v: Arrele_Sing);
CONST
 Lis_Min = 'Bimuchcdfpplkforpxamhbwrrt';
{           1 3 5 7 9 111315171921232527}
VAR
 i, Oxy: Byte;
 Tot: Single;
BEGIN
 Tot:= 0.0; Oxy:= Def_Oxy;
 FOR i:= 1 TO D DO BEGIN
  v[i]:= v[i] / (R.Atomic_Weit[i] * R.Stoch[i] + 16.00 * R.Nb_Oxy[i]);
  Tot:= Tot + v[i] * R.Nb_Oxy[i];
 END;
 {}
 CASE Pos(Copy(S,1,2), Lis_Min) Of
  1, 3, 5:       Oxy:= 22;
  7:             Oxy:= 18;
  9, 11, 13, 15: Oxy:= 24;
  19, 21:        Oxy:= 23;
  23, 25:        Oxy:= 0;  {Analyse Roche Totale}
  0:             Oxy:= Def_Oxy
 END;
 {}
 FOR i:= 1 TO D DO
 IF Tot<>0 THEN BEGIN
  IF Oxy>0 THEN v[i]:=v[i]*(Oxy/Tot)*R.Stoch[i]
  ELSE BEGIN
   IF R.Nb_Oxy[i]>0 THEN v[i]:= 100*v[i]*R.Stoch[i] {from PERCENT to PERMIL}
   ELSE v[i]:= v[i]*R.Stoch[i]/100 {from PPM to PERMIL}
  END
 END
 ELSE v[i]:= 0.0
END; {Stc_Vec}

FUNCTION Calc_Exp(v: Arrele_Sing; Atm: Rec_Atom; Exp: Expression; VAR Ok: Boolean): Single;
VAR x, Num, Den: Single; i: Byte;
BEGIN
 Ok:= True;
 Calc_Exp:= 0.0;
 Num:= 0;
 WITH Exp DO BEGIN
  FOR i:= 1 TO Num_Trm DO BEGIN
   x:= v[Num_Ind[i]];
   IF Atomic THEN x:= 10*x/Atm.Molar_Weit[Num_Ind[i]]*Atm.Stoch[Num_Ind[i]];
   IF (x>0) OR Nega_Is_Ok (**OR ((X=0)AND Zero_Is_Ok)**)
    THEN Num:= Num + x * Num_Kof[i]
     ELSE Ok:= False
  END; {FOR}
  IF Ok THEN BEGIN
   IF Den_Trm=0 THEN Calc_Exp:= Num
   ELSE BEGIN
    Den:= 0;
    FOR i:= 1 TO Den_Trm DO BEGIN
     x:= v[Den_Ind[i]];
     IF Atomic THEN x:= 10*x/Atm.Molar_Weit[Den_Ind[i]]*Atm.Stoch[Den_Ind[i]];
     IF (x>0) OR Nega_Is_Ok {((X=0) AND Zero_Is_Ok)} THEN Den:= Den + x * Den_Kof[i]
      ELSE Ok:= False
    END;
    IF Abs(Den)>Epsilon THEN Calc_Exp:= Num/Den
    ELSE BEGIN Ok:= False; Calc_Exp:= 0.0 END
   END {ELSE}
  END
 END {WITH}
END; {Calc_Exp}

PROCEDURE Titr_Exp(VAR Exp: Expression; Frm: Forme; Atm: Rec_Atom);
VAR s, t: string; i: byte;
{ Atomic: Boolean;
  Num_Tit, Den_Tit, Titre: String;
  Num_Ind, Den_Ind: Array[0..] Of Byte;
  Num_Kof, Den_Kof: Array[1..] Of Single;
  Num_Trm, Den_Trm: Byte                  }
BEGIN
 WITH Exp DO BEGIN
  s:=''; Den_Tit:='';
  FOR i:=1 TO Num_Trm DO BEGIN
   IF abs(Num_Kof[i])<>1 THEN t:=R_Str(abs(Num_Kof[i]))+' ' ELSE t:='';
   IF Num_Kof[i]<0 THEN t:='-'+t ELSE IF i>1 THEN t:='+'+t;
   IF Atomic THEN s:= s + t + Atm.Tit_Ele[Num_Ind[i]]
   ELSE  s:= s + t + Frm.Tit_Oxy[Num_Ind[i]]
  END;
  Num_Tit:=s;
  s:='';
  IF Den_Trm>0 THEN
  FOR i:=1 TO Den_Trm DO BEGIN
   IF abs(Den_Kof[i])<>1 THEN t:=R_Str(abs(Den_Kof[i]))+' ' ELSE t:='';
   IF Den_Kof[i]<0 THEN t:='-'+t ELSE IF i>1 THEN t:='+'+t;
   IF Atomic THEN s:= s + t + Atm.Tit_Ele[Den_Ind[i]]
   ELSE  s:= s + t + Frm.Tit_Oxy[Den_Ind[i]]
  END;
  Den_Tit:=s;
  IF Den_Tit<>'' THEN Titre:=Num_Tit +' / '+Den_Tit
  ELSE Titre:=Num_Tit;
//  IF Atomic THEN Titre:=Titre + ' atm'
 END;
END;{Titr_Exp}

PROCEDURE Init_LibIo;
VAR i: byte;
BEGIN
 SpdFile:='';
 SufSpd:='';
 FOR i:=1 TO Max_Champ DO ArLisChamp[i]:='';
 WITH Def_Exp DO BEGIN
  Atomic:= False;
  Num_Tit:= ''; Den_Tit:= ''; Titre:= '';
  Num_Trm:= 1; Den_Trm:= 0;
  FOR i:= 0 TO Max_Trm DO BEGIN Num_Ind[i]:=0;Den_Ind[i]:=0 END;
  FOR i:= 1 TO Max_Trm DO BEGIN Num_Kof[i]:=1;Den_Kof[i]:=1 END;
  Num_Ind[1]:=1; Den_Ind[1]:=1;
 END; {WITH}
 FOR i:= 1 TO Max_Ele DO Arr_Vrai[i]:= true;
 FOR i:= 1 TO Max_Ele DO Arr_Faux[i]:= false;
 FOR i:= 0 TO Max_Ele DO Vec_0[i]:= 0.0;
 FOR i:= 1 TO Max_Ele DO Def_Prm[i]:= i;
 FOR i:= 1 TO Max_Champ DO Def_Champ[i]:= Def_Str;
 WITH Ana_0 DO BEGIN Champ:= Def_Champ; Don:= Vec_0 END
END;{Init_LibIo}

PROCEDURE Cnv_Vec(Mode: ModCnv; R:Rec_Atom; Dim:Byte; VAR V:ArrEle_Sing);
VAR i: byte;
BEGIN
 FOR i:= 1 TO Dim DO
 IF (v[i]>0.0) THEN
 CASE Mode OF
  Oxy: IF (R.Valence[i]>0) AND (V[i]<>-9.99) THEN BEGIN
    v[i]:= v[i]*R.TO_Met[i];
   {V[i]:= 10*V[i]*TO_Oxy[i];IN PERMIL}
    V[i]:= V[i]*R.TO_Oxy[i];{IN PERCENT}
  END;
  Met: IF (V[i]>0) THEN v[i]:= v[i]*R.TO_Met[i];
  Atm, Rha: v[i]:=10*R.Stoch[i]*v[i]/(R.Atomic_Weit[i]*R.Stoch[i]+16.00*R.Nb_Oxy[i])
 END
 ELSE v[i]:=0.0
END;{Cnv_Vec}

PROCEDURE Text_TO_Forme(VAR F:TextFile; VAR Frm:Forme; LirLis:BOOLEAN; VAR OK:BOOLEAN);
VAR
 s, t: String;
 i, j, k: word;
 b: boolean;
procedure Traiter_Mot(M:string);
begin
 IF M[1]='.' THEN BEGIN//IF M[1] IN ['.','_'] THEN BEGIN
  Delete(M,1,1);
  IF Frm.Nb_Champ<Max_Champ THEN BEGIN
   Inc(Frm.Nb_Champ); Frm.Tit_Champ[k]:=M
  END
  ELSE IF NOT LirLis THEN OK:=FALSE {too many alfanum fields}
 END
 ELSE BEGIN
  Skip_Rite(['.'],M);
  IF Length(M)=1 THEN M:=M+Pad_Car;
  IF (M='PF') OR (M='H2O+') THEN M:='LOI';
  IF (M='H2O-') THEN M:= 'Hum';
  IF (t<>'_') AND (NOT LirLis) AND(k-Frm.Nb_Champ<=Max_Ele) THEN BEGIN
   Inc(Frm.Nb_Ele);
   if M[2] in ['A'..'Z'] then M[2]:=chr(ord(M[2])+32);
   Frm.Tit_Oxy[k-Frm.Nb_Champ]:=M;
   IF Length(M)=1 THEN M:= M + '.';
   IF (M='LOI') THEN M:= 'H.2O';
   IF (M='CO2') THEN M:= 'C.O2';
   IF M[2] IN ['0'..'9'] THEN Insert('.',M,2);
   IF (M[2]='O') And (M[1] IN ['K','U','P','S','W']) THEN Insert('.',M,2);
   Frm.Lis_Ele:=Frm.Lis_Ele + copy(M,1,2)
  END
  {ELSE OK:=FALSE}
 END
end;//Traiter_Mot

BEGIN //Text_TO_Forme
//LirLis:reading a list file (descriptive fields only)
 OK:=TRUE;
 WITH Frm DO BEGIN
  Nb_Champ:=0;Nb_Ele:=0;
  No_Num:=FALSE;
  Tit_Champ:=Def_Champ;
  Nom_Fil:='';Lis_Ele:='';
 END;
 ReadLn(F,s);
//title
(***
 IF NOT (s[1] IN ['_','.']) THEN BEGIN
  WHILE POS(#9,s)>0 DO delete(s,Length(s),1);
  Frm.Nom_Fil:=s;
  ReadLn(F,s)
 END;
***)
//title, New Version
 IF (s[1]='_') THEN BEGIN
  WHILE POS(#9,s)>0 DO delete(s,Length(s),1);
  Frm.Nom_Fil:=s;
  ReadLn(F,s)
 END;
 b:=Eof(F) or (s='') or (s[1] in Fin_Car);

 i:=0;
 j:=0;k:=0;SetLength(t,1);t:='_';
 if not b then repeat
  inc(i); inc(j);
  if (i>length(s)) or (s[i]=#9) then begin
   inc(k);
   Traiter_Mot(t);
   SetLength(t,1); t:='_'; j:=0;//reset the string t
  end
  else begin
   SetLength(t,j); t[j]:=s[i]
  end
 until (i>length(s));
 with Frm do if Nb_Champ=0 then begin
  No_Num:=TRUE; Nb_Champ:=1; Tit_Champ[1]:='NUM'
 end
 else No_Num:=FALSE
END;//Text_TO_Forme

BEGIN
 Init_LibIo
END.





