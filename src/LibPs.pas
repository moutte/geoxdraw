UNIT LibPs;
{Outils pour dessin PostScript}

INTERFACE

USES LibIo, libgrf;

VAR
 Ps_Open: boolean;
 N_Ps: byte;
 Ps_Scale: byte;

FUNCTION Plot_Open
(S_Header,s_dir,s_nam:string;
x0,y0:integer;{mm} ss,sx,sy,sl: string): BOOLEAN;
PROCEDURE Plot_End;
PROCEDURE Plot_Close;
PROCEDURE Plot_Test(Dim:WORD);
PROCEDURE Plot_String(x,y: Word; t:byte; s:string);{x,y in mm}
PROCEDURE Plot_SymCol(x0,y0:integer;imod:byte;BA,BT,BS,BC:BOOLEAN);{x0,y0: positions in mm}
PROCEDURE Plot_ZeroCadre(x0,y0,xmm,ymm:integer);
PROCEDURE Plot_TolCadre(b_spd,b_clos:boolean;x0,y0,xmm,ymm:integer;NPaX,NPaY:byte; Tit_X,Tit_Y: string;Tol_X,Tol_Y:single);
PROCEDURE Plot_TrgCadre(d:byte;x0,y0,TBase:integer{mm};Tit_X,Tit_Y,Tit_Z:string);
PROCEDURE Plot_Profil(x0,y0,xmm,ymm:integer;Tol_X, Tol_Y: single; Dim:Word);
PROCEDURE Plot_Dessin(B_Trg: Boolean; Tol_X,Tol_Y:single; Dim:Word);
PROCEDURE Plot_Labels(b_corners:boolean;x0,y0,xmm,ymm:integer; B_Trg: Boolean; Tol_X, Tol_Y: single; Dim:WORD);
PROCEDURE Plot_LabelsPro(x0,y0,DY:integer; Frm: Forme; Yes_Pro: ArrEle_Bool);
PROCEDURE Plot_TitSpd(Frm:Forme;Prm:ArrEle_Byte);
PROCEDURE Plot_LabSpd(B:Boolean;X0,Y0:integer;N:byte);

IMPLEMENTATION

USES SysUtils;

VAR
 Ps: TextFile;
 Nam_Ps: String;
 Arr_Ps: array[0..Max_Symb] of string[3];
{
 XPoints: word = 10000;
 YPoints: word = 10000;
}
 CurCol,CurSym: byte;
 Scale: P_Intg;{contains XPoints,YPoints}

procedure Plot_Scale;
begin
 writeln(Ps,Ps_Scale/10:3:1,' ',Ps_Scale/10:3:1,' scale');
end;

FUNCTION Plot_Open
(S_Header,s_dir,s_nam:string;
x0,y0:integer;{mm} ss,sx,sy,sl: string): BOOLEAN;
VAR F: TextFile; t: string;
BEGIN
 S_Dir:=IncludeTrailingPathDelimiter(Dir_Ps+s_dir);
 IF Test_Dir(s_dir) then begin
 Plot_Open:=TRUE;
 if not Ps_Open then begin
  assign(Ps, s_dir+s_nam+'.ps'); Rewrite(Ps);
  AssignFile(F,S_Header);Reset(F);
  repeat readln(F,t); writeln(Ps,t) until pos('%END',t)=1;
  CloseFile(F);
  Ps_Open:=TRUE
 end;
(**
gsave
190 mm 10 mm moveto (_____) rsho %write here the file name, comment, etc ...
grestore
**)
 WriteLn(Ps,'%%%% %%%% %%%% %%%% %%%% %%%% %%%% %%%% %%%% %%%% %%%%');
 WriteLn(Ps,'%%%%Sample File= ',ss);
 WriteLn(Ps,'%%%%X-Data File= ',sx);
 WriteLn(Ps,'%%%%Y-Data File= ',sy);
 WriteLn(Ps,'%%%%Selection  = ',sl);
 WriteLn(Ps,'%%%%BEGIN PLOT ',S_Nam);
 writeln(Ps,'gsave');
 writeln(Ps,x0,' mm ',y0,' mm translate %absolute coordinates of plot (mm)');
 Nam_Ps:=S_Nam
 END
 ELSE BEGIN Ps_Open:=FALSE; Plot_Open:=FALSE END
END;{Plot_Open}

PROCEDURE Plot_Test(Dim:WORD);
BEGIN
// Plot_Open(Ps_Header,'','design4',0,170,'-','-','-','-');
 Plot_ZeroCadre(0,0,120,120);
 Plot_Dessin(FALSE{Trg},1,1{Tol_X,Tol_Y},Dim);
 Plot_Labels(true,0,0,100,100,FALSE{Trg},1,1{Tol_X,Tol_Y},Dim);
 Plot_Close
END;{Test_Plot}

PROCEDURE Plot_End;
BEGIN
 writeln(Ps,'grestore');
 WriteLn(Ps,'%%%%END PLOT ',Nam_Ps);
 WriteLn(Ps,'%%%% %%%% %%%% %%%% %%%% %%%% %%%% %%%% %%%% %%%% %%%%')
END;

PROCEDURE Plot_Close;
BEGIN
 writeln(Ps,'showpage');
 CloseFile(Ps); Ps_Open:=FALSE
END;{Plot_Close}

PROCEDURE Plot_TitSpd(Frm:Forme;Prm:ArrEle_Byte);
VAR
 i:Byte; j:integer;
BEGIN
 WriteLn(Ps,'%%%%elements names spd ',Nam_Ps);
 writeln(Ps,'gsave');
 writeln(Ps,'10 mm 10 mm translate');
 writeln(Ps,'0.5 sl 1 sg _colB');
 WriteLn(Ps,'/Times-Roman findfont 6 scalefont setfont');
 Plot_Scale;
 WITH Frm DO FOR i:= 1 TO Nb_Ele DO BEGIN
  j:=round((i-1)*Scale[Cor_X]/(Nb_Ele-1));
  Writeln(Ps,j,' ml 0 _barb_xlo');
  IF Prm[i]<>0 THEN BEGIN
   Writeln(Ps,j-200,' ml -10 moveto (',copy(Tit_Oxy[i],1,2),') show');
   IF ODD(i) THEN
    Writeln(Ps,j-200,' ml -16 moveto (',Real_Str(VecSpd[i]),') show')
   ELSE Writeln(Ps,j-200,' ml -22 moveto (',Real_Str(VecSpd[i]),') show')
  END
 END;
 writeln(Ps,'grestore');
 WriteLn(Ps,'%%%%end element names ',Nam_Ps)
END;{Plot_TitSpd}

PROCEDURE Plot_ZeroCadre(x0,y0,xmm,ymm:integer);
BEGIN {Plot_ZeroCadre}
 WriteLn(Ps,'%%%%REPERES ',Nam_Ps);
 WriteLn(Ps,'gsave 0.5 sl _colL');
 write(Ps,'newpath ');
 write(Ps,x0,' mm ',y0,' mm moveto ');
 write(Ps,0,' mm ',ymm,' mm rlineto ',xmm,' mm ',0,' mm rlineto ');
 write(Ps,0,' mm ',-ymm,' mm rlineto closepath stroke');
 writeln(Ps);
 WriteLn(Ps,'grestore');
 WriteLn(Ps,'%%%%END REPERES ',Nam_Ps)
END;{Plot_ZeroCadre}

PROCEDURE Plot_String(x,y: Word; t:byte; s: string);{x,y in mm}
BEGIN
 writeln(Ps,'%%%%Plot Comment ');
 writeln(Ps,'gsave');
 WriteLn(Ps,'/Times-Roman findfont ',t,' scalefont setfont');
 writeln(Ps, x,' mm ',y,' mm moveto (',s,') show');
 writeln(Ps,'grestore');
 writeln(Ps,'%%%%End Plot Comment ',Nam_Ps);
END;{Plot_String}

PROCEDURE Plot_LabelsPro(x0,y0,DY:integer; Frm: Forme; Yes_Pro: ArrEle_Bool);
VAR i, j: byte;
BEGIN
 j:=0;
 FOR i:=1 TO Frm.Nb_Ele DO IF Yes_Pro[i] THEN BEGIN
  inc(j);
  write(Ps,'_col',CodColor[(i-1)mod Max_Coul + 1],' ');
  writeln(Ps,x0,' mm ',y0 - 6*j,' mm ',Arr_Ps[(i-1) mod Max_Symb + 1]);
  writeln(Ps,'_colB ',x0,' mm ',y0 - 6*j,' mm moveto (',Frm.Tit_Oxy[i],') show')
 END
END;{Plot_LabelsPro}

PROCEDURE Plot_LabSpd(B:Boolean;X0,Y0:integer;N:byte);
VAR i:byte; s:String;
BEGIN
 writeln(Ps,'%%%%SAMPLE NAMES ',Nam_Ps);
 writeln(Ps,'gsave');
 writeln(Ps,x0,' mm ',y0,' mm translate');
 WriteLn(Ps,'/Times-Roman findfont 8 scalefont setfont');
 writeln(Ps,'0.6 sl _colB');
 for i:=1 to N do begin
  write(Ps,'_col',CodColor[(i-1)mod Max_Coul + 1]);
  IF B THEN writeln(Ps,' ',5,' mm ',5*i,' mm ',Arr_Ps[Sym_SF]) {His: only squares}
  ELSE writeln(Ps,' ',5,' mm ',-5*i,' mm ',Arr_Ps[ArrPrm_SymSpd[i]]); {Spd: only filled symbols}
 end;

 writeln(Ps,' _colB');
 for i:=1 to N do begin
  s:=ArLabSpd[i];
  Skip_Left(['_','0'],s);Skip_Rite([' ','.'],s);
  IF s='' THEN S:='.';
  if B then writeln(Ps,8,' mm ',5*i-1,' mm moveto (',s,') show')
  else writeln(Ps,8,' mm ',-5*i-1,' mm moveto (',s,') show')
 end;
 writeln(Ps,'grestore');
 writeln(Ps,'%%%%END NAMES ',Nam_Ps);
END;{Plot_LabSpd}

PROCEDURE Plot_SymCol(x0,y0:integer;imod:byte;BA,BT,BS,BC:BOOLEAN);
VAR
 i, j: byte;
 DX, DY: integer;
 N_Sym, N_Col, rcol, qsym: Byte;
 s, t: string;
BEGIN
 WriteLn(Ps,'%%%%CAPTIONS ',Nam_Ps);
 Writeln(Ps,'gsave');
 Writeln(Ps,x0,' mm ',y0,' mm translate');
 WriteLn(Ps,'/Times-Roman findfont 8 scalefont setfont _colB 0.5 sl');
 CurCol:=0;
 N_Sym:=0; N_Col:=0;
 DY:=4; DX:=2; {mm}
 IF BA OR BT THEN BEGIN {Automatic sym+col}
  j:=0;
  if BT then begin
   LisAutoSymCol:=LisAutomatic;
   LisAutoLegend:=LisAutomatic
  end;
  N_Sym:=length(LisAutoSymCol) DIV Signif;
  FOR i:= 1 TO N_Sym DO BEGIN
   if BA then begin
     s:=copy(LisAutoSymCol,Signif*(i-1)+1,Signif);
     rcol:=pos(UpCase(s[1]),CodColor);
     if length(s)>2 then qsym:=position(3,'_'+copy(s,2,2),CodSymbol)
     else qsym:=0
   end else begin //BT
    if i>0 then rcol:=(i mod Max_Coul) + 1 else rcol:=0;
    if i>0 then qsym:=4*(i div Max_Coul) + 1
    else qsym:=0; //=1,5,9,...
   end;
   if (qsym>0)and(rcol<=Max_Coul)and(rcol>0) then begin
    inc(j);
    write(Ps,'_col',CodColor[rcol],' ');
    write(Ps,5,' mm ',-DY*((j-1) mod imod),' mm ', Arr_Ps[qsym]);
    s:=copy(LisAutoLegend,Signif*(i-1)+1,Signif);
    WHILE s[Length(s)]='.' DO Delete(S,Length(s),1);
    writeln(Ps,' _colB ',5+DX,' mm ',-DY*((j-1) mod imod)-1,' mm moveto (',s,') show')
   end;
   if (j mod imod=0) then  Writeln(Ps,'20 mm 0 mm translate');
  END;
 END;
 IF BS THEN BEGIN {Symbols}
  N_Sym:=length(Lis_Symb) DIV Signif;
  {y0:=y0+DY*N_Sym;}
  FOR i:= 1 TO N_Sym DO BEGIN
   t:=Copy(Lis_Symb,Signif*(i-1)+1,Signif);
   qsym:= position(Length_Codsym,'_'+t,CodSymbol);
   if (qsym>0) and (qsym<=Max_Symb) then begin
    writeln(Ps,5,' mm ',-DY*i,' mm ',Arr_Ps[qsym],' ');
    t:= copy(Lis_SymbLeg,(i-1)*signif+1,signif);
    writeln(Ps,5+DX,' mm ',-DY*i-1,' mm moveto (',t,') show')
   end
  END;
  Writeln(Ps,'20 mm 0 mm translate');
 END;
 IF BC THEN BEGIN {Colors}
  N_Col:=length(Lis_Coul) DIV Signif;
  {y0:=y0+DY*N_Col;}
  FOR i:= 1 TO N_Col DO BEGIN
   t:= Copy(Lis_Coul,Signif*(i-1)+1,Signif);
   rcol:= position(1,t,CodColor);
   write(Ps,'_col',CodColor[rcol],' ');
   writeln(Ps,5,' mm ',-DY*i,' mm ',Arr_Ps[Sym_SF]);
   t:= copy(Lis_CoulLeg,(i-1)*signif+1,signif);
   writeln(Ps,5+DX,' mm ',-DY*i-1,' mm moveto (',t,') show');
  END
 END;
 Writeln(Ps,'grestore');
 WriteLn(Ps,'%%%%end captions ',Nam_Ps)
END;{Plot_SymCol}

PROCEDURE Rel_Str(r: real; VAR t: Str_Signif);
VAR s: string;
BEGIN
 str(r:8:3, s); WHILE s[1]=' 'DO delete(s,1,1);
 WHILE s[length(s)]='0' DO delete(s,length(s),1);
 WHILE s[length(s)]='.' DO delete(s,length(s),1);
 t:= s
END;{Rel_Str}

PROCEDURE Plot_TolCadre
(b_spd,b_clos:boolean;
 x0,y0,xmm,ymm:integer;NPaX,NPaY:byte;
 Tit_X,Tit_Y:string;
 Tol_X,Tol_Y:single);
{b_closed=true : axes drawn on both sides}
VAR
 r:               real;
 c:               coor_xy;
 i, j, t:         byte;
 tx0,tx1,ty0,ty1: Str_Signif;
 Log_Min:         P_Real;
BEGIN {Plot_TolCadre}
 Scale[Cor_X]:=100*xmm; Scale[Cor_Y]:=100*ymm;

 WriteLn(Ps,'%%%%FRAME ',Nam_Ps);
 writeln(Ps,'gsave');
 writeln(Ps,x0,' mm ',y0,' mm translate');

 writeln(Ps,'1 sl 1 sg');
 Plot_Scale;

 FOR c:= Cor_X TO Cor_Y DO IF Log[c] THEN BEGIN
  Log_Min[c]:= Log_Max[c];
  FOR i:= 1 TO Log_Mod[c] DO Log_Min[c]:= Log_Min[c]/10
 END;

{conversion of min AND max TO character strings}
 IF Log[Cor_X] THEN BEGIN
   Rel_Str(Log_Min[Cor_X],tx0); Rel_Str(Log_Max[Cor_X],tx1)
 END
 ELSE BEGIN Rel_Str(P_Low[Cor_X],tx0); Rel_Str(P_Hih[Cor_X],tx1) END;
 IF Log[Cor_Y] THEN BEGIN
  Rel_Str(Log_Min[Cor_Y],ty0); Rel_Str(Log_Max[Cor_Y],ty1)
 END
 ELSE BEGIN Rel_Str(P_Low[Cor_Y],ty0); Rel_Str(P_Hih[Cor_Y],ty1) END;

 writeln(Ps,'_colB');
{cadre}
 write(Ps,'newpath 0 ',ymm,' mm moveto 0 0 lineto ',xmm,' mm 0 lineto ');
 if B_Clos then
 writeln(Ps,xmm,' mm ',ymm,' mm lineto closepath stroke')
 else writeln(Ps,' stroke');

{titre X,Y}
 writeln(Ps,xmm div 2,' mm -6 mm moveto (',tit_x,') show');
 writeln(Ps,'-4 mm ',ymm+4,' mm moveto (',tit_y,') show');
{max,min X,Y}
 if not B_Spd then begin
  writeln(Ps,'0 -4 mm moveto (',tx0,') show');
  writeln(Ps,xmm - 4,' mm -4 mm moveto (',tx1,') show')
 end;
 writeln(Ps,xmm+1,' mm 1 mm moveto (',ty0,') show');
 writeln(Ps,xmm+1,' mm ',ymm+1,' mm moveto (',ty1,') show');

 WriteLn(Ps,'%%%%BARBULES ',Nam_Ps);
{barbules}
 writeln(Ps,'0.5 sl 0.2 sg');
 FOR c:= Cor_X TO Cor_Y DO BEGIN
(**LOG**)
  IF Log[c] THEN BEGIN
   FOR I:=0 TO Log_Mod[c] DO FOR J:= 1 TO 9 DO BEGIN
    r:=(I+Log10(J)-Pivot(c))*Scale[c]/Log_Mod[c];
    IF J=1 THEN t:=16 ELSE t:=8;
    IF (R>=0) AND (R<=Scale[c]*Tol_X) THEN
    IF c=Cor_X THEN BEGIN
     if J=1 then begin
      writeln(Ps,Round(r),' ml 0 _barb_xlo_');
      writeln(Ps,Round(r),' ml ',ymm,' mm _barb_xhi_')
     end
     else begin
      writeln(Ps,Round(r),' ml 0 _barb_xlo');
      writeln(Ps,Round(r),' ml ',ymm,' mm _barb_xhi')
     end
    END
    ELSE BEGIN
     write(Ps,'0 ',Round(r),' ml _barb_ylef');
     if j=1 then writeln(Ps,'_') else writeln(Ps);
     write(Ps,xmm,' mm ',Round(r),' ml _barb_yrit');
     if j=1 then writeln(Ps,'_') else writeln(Ps);
    END
   END {FOR I}
  END {IF Log[c]}

(**NOT LOG**)
  ELSE {NOT Log[c]} BEGIN
   IF (c=Cor_X) AND (NPaX>0) THEN BEGIN
    FOR i:= 0 TO NPaX DO BEGIN
     if B_Clos then writeln(Ps,Round(Scale[c]*i/NPaX),' ml 0 _barb_xhi')
     else writeln(Ps,Round(Scale[c]*i/NPaX),' ml 0 _barb_xlo');
     if B_Clos then writeln(Ps,Round(Scale[c]*i/NPaX),' ml ',ymm,' mm _barb_xlo')
     else writeln(Ps,Round(Scale[c]*i/NPaX),' ml ',ymm,' mm _barb_xhi')
    END
   END
   ELSE IF (NPaY>0)THEN FOR i:= 0 TO NPaY DO begin
    if B_Clos then writeln(Ps,'0 ',Round(Scale[c]*i/NPaY),' ml _barb_yrit')
    else writeln(Ps,'0 ',Round(Scale[c]*i/NPaY),' ml _barb_ylef');
    if B_Clos then writeln(Ps,xmm,' mm ',Round(Scale[c]*i/NPaY),' ml _barb_ylef')
    else writeln(Ps,xmm,' mm ',Round(Scale[c]*i/NPaY),' ml _barb_yrit')
   END
  END {NOT Log[c]}
 END;{FOR c}
 writeln(Ps,'grestore');
 WriteLn(Ps,'%%%%end barbules ',Nam_Ps)
END; {Plot_TolCadre}

PROCEDURE Plot_TrgCadre(d:byte;x0,y0,TBase:integer;Tit_X,Tit_Y,Tit_Z:string);
VAR
 i, n_barb, Z0Trg: byte;
 Xx, Xy, Yx, Yy, Zx, Zy,{apex coordinates in 1/100 mm}
 delta,deltaY: integer;
 s: string;
BEGIN {Plot_TrgCadre}
 Scale[Cor_X]:=d*TBase; Scale[Cor_Y]:=d*TBase;
{cadre}
 writeLn(Ps,'%%%%FRAME ',Nam_Ps);
 writeln(Ps,'gsave ',x0,' mm ',y0,' mm translate _colB ');

 Plot_Scale;

 if B_TrgDown then begin
  Xx:=d*TBase; Xy:=round(d*TBase*0.866);
  Yx:=0; Yy:=round(d*TBase*0.866);
  Zx:=round(d*TBase/2); Zy:=0
 end
 else begin
  Xx:=d*TBase; Xy:=0;
  Yy:=round(d*TBase*0.866); Yx:=round(d*TBase/2);
  Zx:=0; Zy:=0
 end;
 writeln(Ps,'newpath ',Zx,' ml ',Zy,' ml moveto ',Xx,' ml ',Xy,' ml lineto');
 writeln(Ps,Yx,' ml ',Yy,' ml lineto ',Zx,' ml ',Zy,' ml lineto stroke');

{titres}
 Z0Trg:= 100-X0Trg-Y0Trg;
 IF Z0Trg<100 THEN BEGIN Str(Z0Trg,S); S:= Tit_Z+'['+S+']' END ELSE S:= Tit_Z;
 delta:=-500;{5 mm below axis}
 writeln(Ps,Zx,' ml ',Zy+delta,' ml moveto (',s,') show');

 IF X1Trg<100 THEN BEGIN Str(X1Trg,S); S:= Tit_X+'['+S+']' END ELSE S:= Tit_X;
 if B_TrgDown then delta:=500{5 mm} else delta:=-500;
 writeln(Ps,Xx-1000,' ml ',Xy+delta,' ml moveto (',s,') show');

 IF Y1Trg<100 THEN BEGIN Str(Y1Trg,S); S:= Tit_Y+'['+S+']' END ELSE S:= Tit_Y;
 if B_TrgDown then delta:=500 else delta:=0;{5 mm}
 writeln(Ps,Yx,' ml ',Yy+delta,' ml moveto (',s,') show');

 writeln(Ps,'grestore');
 writeLn(Ps,'%%%%END FRAME ',Nam_Ps);
(**)
 WriteLn(Ps,'%%%%barbules ',Nam_Ps);
 writeln(Ps,'gsave 10 mm 10 mm translate 0.5 sl 0.2 sg _colL');

 Plot_Scale;

 n_barb:= (X1Trg-X0Trg) DIV 10 - 1;
 delta:=round(TBase*d/(N_Barb+1)/2);
 deltaY:=round(TBase*d*1.732/(N_Barb+1)/2);
 FOR i:= 1 TO N_Barb DO BEGIN {barbules on base}
  IF B_TrgDown THEN BEGIN
   writeln(Ps,'newpath ',2*Delta*i,' ml ',Xy,' ml moveto 5 -8.7 rlineto ');
   writeln(Ps,2*Delta*i,' ml ',Xy,' ml moveto -5 -8.7 rlineto stroke')
  END
  ELSE BEGIN
   writeln(Ps,'newpath ',2*Delta*i,' ml ',Xy,' ml moveto 5 8.7 rlineto ');
   writeln(Ps,2*Delta*i,' ml ',Xy,' ml moveto -5 8.7 rlineto stroke')
  END
 END;

 FOR i:= 1 TO N_Barb DO BEGIN
  IF B_TrgDown THEN BEGIN
    writeln(Ps,
   'newpath ',Zx-Delta*i,' ml ',DeltaY*i,' ml moveto 10 0 rlineto stroke');
    writeln(Ps,
   'newpath ',Zx-Delta*i,' ml ',DeltaY*i,' ml moveto 5 8.7 rlineto stroke');
  END
  ELSE BEGIN
    writeln(Ps,
   'newpath ',Delta*i,' ml ',DeltaY*i,' ml moveto 10 0 rlineto stroke');
    writeln(Ps,
   'newpath ',Delta*i,' ml ',DeltaY*i,' ml moveto 5 -8.7 rlineto stroke');
  END;
 END;

 FOR i:= 1 TO N_Barb DO BEGIN
  IF B_TrgDown THEN BEGIN
    writeln(Ps,
   'newpath ',Zx+Delta*i,' ml ',DeltaY*i,' ml moveto -10 0 rlineto stroke');
    writeln(Ps,
   'newpath ',Zx+Delta*i,' ml ',DeltaY*i,' ml moveto -5 8.7 rlineto stroke')
  END
  ELSE BEGIN
    writeln(Ps,
   'newpath ',Xx-Delta*i,' ml ',DeltaY*i,' ml moveto -10 0 rlineto stroke');
    writeln(Ps,
   'newpath ',Xx-Delta*i,' ml ',DeltaY*i,' ml moveto -5 -8.7 rlineto stroke')
  END;
 END;

 writeln(Ps,'grestore');
 WriteLn(Ps,'%%%%END BARBLES ',Nam_Ps);
END;{Plot_TrgCadre}

PROCEDURE Calc_Ps(p: p_real; VAR cx, cy: integer; VAR ok: boolean);
VAR c: coor_xy;
BEGIN
 ok:= true;
 FOR c:= Cor_X TO Cor_Y DO BEGIN
  IF Log[c] THEN BEGIN
   IF p[c]>0 THEN p[c]:= (Log10(p[c]/Log_Max[c])+Log_Mod[c])/Log_Mod[c]
    ELSE ok:= false
  END
  ELSE {IF NOT log} p[c]:= (p[c]-P_Low[c])/(P_Hih[c]-P_Low[c]);
 ok:= ok AND {(p[c]>=0) AND} (p[c]<=3.2)
 END;
 IF ok THEN BEGIN
  cx:= round(Scale[Cor_X] * p[Cor_X]);
  cy:= round(Scale[Cor_Y] * p[Cor_Y])
 END
END; {Calc_Ps}

PROCEDURE TrgCalc_Ps(x,y:real; VAR cx, cy:integer; VAR ok:boolean);
var z: real;
BEGIN
 ok:= true;
 x:= (x-X0Trg)/(X1Trg-X0Trg);
 y:= (y-Y0Trg)/(Y1Trg-Y0Trg);
 z:= 1 - x - y;

 if B_TrgDown then begin
(**
  IF (x+z/2)*Scale[Cor_X]<32000 THEN cx:= round((x+z/2)*Scale[Cor_X]) ELSE ok:= false;
  IF ok and ((x+y)*0.866*Scale[Cor_Y]<32000) THEN cy:= round((x+y)*0.866*Scale[Cor_Y]) ELSE ok:= false
**)
  cx:= round((x+z/2)*Scale[Cor_X]);
  cy:= round((x+y)*0.866*Scale[Cor_Y])
 end
 else begin
(**
  IF (x+y/2)*XPoints<32000 THEN cx:= round((x+y/2)*Xpoints) ELSE ok:= false;
  IF ok and (y*0.866*YPoints<32000) THEN cy:= round(y*0.866*YPoints) ELSE ok:= false
**)
  cx:= round((x+y/2)*Scale[Cor_X]);
  cy:= round(y*0.866*Scale[Cor_Y])
 end
END;{CalcTrg_Ps}

PROCEDURE Plot_Point
(x,y:integer; Tol_X,Tol_Y:single; s,c:byte; n:Str_Signif);
BEGIN
 IF c>0 THEN c:=(c-1)MOD Max_Coul +1 ELSE C:=0;
 IF (x>=0) AND (y>=0)
 AND (x<=Tol_X*Scale[Cor_X]) AND (y<=Tol_Y*Scale[Cor_Y])
 AND (c>0) THEN BEGIN
  IF s>0 THEN writeln(Ps,x,' ml ',y,' ml ',Arr_Ps[s],' %',n)
  ELSE writeln(Ps,x,' ml ',y,' ml __X');
 END
END; {Plot_Point}

PROCEDURE Plot_Label(b:boolean;x,y: integer; Tol_X,Tol_Y:single; n:Str_Signif);
BEGIN
 IF (x>=0) AND (y>=0)
 AND (x<=Tol_X*Scale[Cor_X]) AND (y<=Tol_Y*Scale[Cor_Y]) THEN BEGIN
  WHILE n[length(n)]='.' DO delete(n,length(n),1);
  if n<>'' then
  if b then
   writeln(Ps,x,' ml ',y,' ml ',Arr_Ps[Sym_P],' ',x,' ml ',y,' ml moveto (',n,')show')
  else
   writeln(Ps,x+2,' ml ',y-2,' ml moveto (',n,')show');
 END
END; {Plot_Label}

PROCEDURE Plot_Dessin
(B_Trg: Boolean; Tol_X, Tol_Y: single; Dim: Word);
VAR
 i:    word;
 b:    boolean;
 x, y, x0, y0: integer;
BEGIN
 CurCol:= 0; CurSym:= 0;
 x0:= -1; y0:= -1;
 WriteLn(Ps,'%%%%SYMBOLS ',Nam_Ps);
 writeln(Ps,'gsave');
 writeln(Ps,'10 mm 10 mm translate');
 writeln(Ps,'0.5 sl 0.2 sg');

 Plot_Scale;

 WriteLn(Ps,'%%%%Write Symbols ',Nam_Ps);
 FOR i:= 1 TO Dim DO BEGIN
  IF B_Trg THEN TrgCalc_Ps(arP[i][Cor_X],arP[i][Cor_Y],x, y, b)
  ELSE Calc_Ps(arp[i],x,y,b);
  IF b THEN begin
   if (arc[i]<>CurCol)and(arc[i]<=Max_Coul)and(arc[i]>0)
   then writeln(Ps,'_col',CodColor[arc[i]]);
   Plot_Point(x,y,Tol_X,Tol_Y,ars[i],arc[i],arn[i]);
   IF (arC[i]>1) AND (Arr_Ps[ars[i]][3]='F') THEN BEGIN
    write(Ps,'_colB ');Plot_Point(x,y,Tol_X,Tol_Y,ars[i]+1,1,arn[i]);
    CurCol:= 1
   END
   ELSE CurCol:=arC[i];
   CurSym:=arS[i]
  END
 END;
 WriteLn(Ps,'%%%%End Symbols ',Nam_Ps);

(**
 WriteLn(Ps,'%%%%Write Black Borders ',Nam_Ps);
 writeln(Ps,'_colB');
 FOR i:= 1 TO Dim DO
 IF (arC[i]>1) AND (Arr_Ps[ars[i]][3]='F') THEN BEGIN
  IF B_Trg THEN TrgCalc_Ps(arP[i][Cor_X],arP[i][Cor_Y],x, y, b)
  ELSE Calc_Ps(arp[i],x,y,b);
  IF b THEN Plot_Point(x,y,Tol_X,Tol_Y,ars[i]+1,1,arn[i])
 END;
 WriteLn(Ps,'%%%%End Black Borders ',Nam_Ps);
**)

 writeln(Ps,'grestore');
 WriteLn(Ps,'%%%%END SYMBOLS ',Nam_Ps)
END; {Plot_Dessin}

PROCEDURE Plot_Profil(x0,y0,xmm,ymm:integer;Tol_X,Tol_Y: single; Dim:Word);
VAR
 i:    word;
 b:    boolean;
 x1, y1, x, y: integer;
BEGIN
 x1:= -1; y1:= -1;
 CurCol:= 0; CurSym:= 0;
 WriteLn(Ps,'%%%%SPIDERS ',Nam_Ps);
 writeln(Ps,'gsave');
 writeln(Ps,x0,' mm ',y0,' mm translate');

 Plot_Scale;

 writeln(Ps,
'newpath -5 mm 0 moveto ',xmm+5,' mm 0 lineto ',xmm+5,' mm ',ymm+5,' mm lineto -5 mm ',ymm+5,' mm lineto closepath');
 writeln(Ps,' clip');
 writeln(Ps,'0.6 sl');

 write(Ps,'newpath');
 FOR i:= 1 TO Dim DO BEGIN
  Calc_Ps(arp[i],x,y,b);
  if (arC[i]<>CurCol) or (arS[i]<>CurSym)
  or (x<x1)//prevent to go back to begin next spider ...
  then begin
   writeln(Ps,' stroke');
   if (arC[i]<>CurCol) then writeln(Ps,'_col',CodColor[arC[i]]);
   writeln(Ps,'newpath ',x,' ml ',y,' ml moveto ')
  end
  else writeln(Ps,x,' ml ',y,' ml lineto');
  CurSym:=arS[i]; CurCol:=arC[i];
  x1:= x; y1:= y;
 END;
 writeln(Ps,' stroke grestore');
 WriteLn(Ps,'%%%%END SPIDERS ',Nam_Ps)
END; {Plot_Profil}

PROCEDURE Plot_Labels(b_corners:boolean;x0,y0,xmm,ymm:integer; B_Trg: Boolean; Tol_X, Tol_Y: single; Dim: word);
VAR
 i:    word;
 b:    boolean;
 x, y: integer;
BEGIN
 Scale[Cor_X]:=100*xmm; Scale[Cor_Y]:=100*ymm;

 WriteLn(Ps,'%%%%LABELS ONLY ',Nam_Ps);
 writeln(Ps,'gsave');
 writeln(Ps,x0,' mm ',y0,' mm translate 0.5 sl 0.2 sg _colB');

 Plot_Scale;

{lowlef _ uprite corners}
 if B_Corners then begin
  WriteLn(Ps,'newpath 10 mm 0 moveto 0 0 lineto 0 10 mm lineto stroke');
  Write(Ps,'newpath ',xmm-10,' mm ',ymm,' mm moveto ',xmm,' mm ',ymm,' mm lineto ');
  WriteLn(Ps,xmm,' mm ',ymm-10,' mm lineto stroke')
 end;

 WriteLn(Ps,'/Times-Roman findfont 6 scalefont setfont');
 FOR i:= 1 TO Dim DO BEGIN
  IF B_Trg THEN TrgCalc_Ps(arP[i][Cor_X],arP[i][Cor_Y],x, y, b)
  ELSE Calc_Ps(arp[i],x,y,b);
  IF b THEN Plot_Label(b_corners,x,y,Tol_X,Tol_Y,arn[i])
 END;
 writeln(Ps,'grestore');
 WriteLn(Ps,'%%%%END LABELS ',Nam_Ps)
END; {Plot_Labels}

PROCEDURE Ini_Sym;
VAR i:byte;
BEGIN
{BRGYAVTO = Black Red Greeen Yellow Aoi Violet Turquoise Orange}
{CodSymbol='SPSESFSCSXCPCECFCCCXTPTETFTCTXDPDEDFDCDXHPHEHFHCHXXPXEXFXCXX'}
 FOR i:=1 TO Max_Symb DO Arr_Ps[i]:=copy(CodSymbol,3*i-2,3);
END; {Ini_Sym}

BEGIN
 CurCol:= 0; CurSym:= 0;
 Ps_Scale:= 10;
 Scale[Cor_X]:=10000; Scale[Cor_Y]:=10000;
 Ini_Sym
END.


arr_sym[1]:={SPlus}
'SPC;PT4;PR-150,-150;ER300,300;PR150,150;PR-150,-20;RR300,40;PR150,20;PR-20,-150;RR40,300;PR20,150;';
arr_sym[2]:={SEmpty}
'SPC;PT4;PR-150,-150;ER300,300;PR150,150;';
arr_sym[3]:={SFilled}
'SP1;PR-150,-150;RR300,300;PR150,150;SPC;PR-120,-120;RR240,240;PR120,120;';
arr_sym[4]:={SCircle}
'SP1;PR-150,-150;RR300,300;PR150,150;SPC;PR-120,-120;RR240,240;PR120,120;SP1;WG100;';
arr_sym[5]:={SXross}
'SP1;PR-150,-150;RR300,300;PR150,150;SPC;PR-120,-120;RR240,240;PR120,120;SP1;PR-120,-20;RR240,40;PR120,20;PR-20,-120;RR40,240;PR20,120;';
arr_sym[6]:={CP}
'SPC;PT4;CI150,5;PR-150,-20;RR300,40;PR150,20;PR-20,-150;RR40,300;PR20,150;';
arr_sym[7]:={CE}
'SPC;PT4;CI150,5;';
arr_sym[8]:={CF}
'SP1;WG150;SPC;WG120;';
arr_sym[9]:={CC}
'SP1;WG150;SPC;WG120;SP1;WG60;';
arr_sym[10]:={CX}
'SP1;WG150;SPC;WG120;SP1;PR-150,-20;RR300,40;PR150,20;PR-20,-150;RR40,300;PR20,150;';
arr_sym[11]:={TP}
'SPC;PT2;CI150,120;PR-80,-20;RR200,40;PR80,20;';
arr_sym[12]:={TE}
'SPC;PT2;CI150,120;';
arr_sym[13]:={TF}
'SP1;PT2;CI150,120;SPC;PT6;CI120,120;CI80,120;CI40,120;';
arr_sym[14]:={TC}
'SP1;PT2;CI150,120;SPC;PT6;CI120,120;CI80,120;CI40,120;SP1;WG80;';
arr_sym[15]:={TX}
'SP1;PT2;CI150,120;SPC;PT6;CI120,120;CI80,120;CI40,120;SP1;PR-80,-20;RR200,40;PR80,20;';
arr_sym[16]:={DP}
'SPC;PT4;CI150,90;PR-140,-20;RR280,40;PR140,20;PR-20,-140;RR40,280;PR20,140;';
arr_sym[17]:={DE}
'SPC;PT4;CI150,90;';
arr_sym[18]:={DF}
'SP1;PT2;CI170,90;SPC;PT6;CI100,90;PR-60,-60;RR120,120;PR60,60;';
arr_sym[19]:={DC}
'SP1;PT2;CI170,90;SPC;PT6;CI100,90;SP1;WG80;';
arr_sym[20]:={DX}
'SP1;PT2;CI170,90;SPC;PT6;CI100,90;PR-55,-55;RR110,110;PR55,55;SP1;PR-150,-20;RR300,40;PR150,20;PR-20,-150;RR40,300;PR20,150;';
arr_sym[21]:={HP}
'SPC;PT4;CI150,45;PR-90,-10;RR180,20;PR90,10;PR-10,-90;RR20,180;PR10,90;';
arr_sym[22]:={HE}
'SPC;PT4;CI150,45;';
arr_sym[23]:={HF}
'SP1;PT2;CI150,45;SPC;PT6;CI80,45;CI60,45;CI40,45;CI20,45;';
arr_sym[24]:={HC}
'SP1;PT2;CI150,45;SPC;PT6;CI80,45;CI60,45;CI40,45;CI20,45;SP1;WG60;';
arr_sym[25]:={HX}
'SP1;PT2;CI150,45;SPC;PT6;CI80,45;CI60,45;CI40,45;CI20,45;SP1;PR-100,-10;RR200,20;PR100,10;PR-10,-100;RR20,200;PR10,100;';
arr_sym[26]:={XP}
'SPC;PT4;PR-100,-20;ER200,40;PR100,20;PR-20,-100;ER40,200;PR20,100;CI80,90;';
arr_sym[27]:={XE}
'SPC;PT4;PR-100,-20;RR200,40;PR100,20;PR-20,-100;RR40,200;PR20,100;';
arr_sym[28]:={XF}
'SP1;PT2;PR-100,-20;ER200,40;PR100,20;PR-20,-100;ER40,200;PR20,100;SPC;PR-90,-15;RR180,30;PR90,15;PR-15,-90;RR30,180;PR15,90;';
arr_sym[29]:={XC}
'SP1;PT2;PR-100,-40;ER200,80;PR100,40;PR-40,-100;ER80,200;PR40,100;SPC;PR-90,-35;RR180,70;PR90,35;PR-35,-90;RR70,180;PR35,90;';
arr_sym[30]:={XX}
'SP1;PT2;PR-40,-80;PD0,160;PU;PR-40,-40;PD160,0;PU;PR-40,40;PD0,-160;PU;PR40,40;PD-160,0;PU;PR40,-40;PR40,80;PR-35,-35;SPC;RR70,70;PR35,35;';

PROCEDURE Ini_Sym;
BEGIN
{BRGYAVTO = Black Red Greeen Yellow Aoi Violet Turquoise Orange}
s:='SPSESFSCSXCPCECFCCCXTPTETFTCTXDPDEDFDCDXHPHEHFHCHXXPXEXFXCXX';
arr_sym[1]:={SPlus}
'SPC;PT4;PR-15,-15;ER30,30;PR15,15;PR-15,-2;RR30,4;PR15,2;PR-2,-15;RR4,30;PR2,15;';
arr_sym[2]:={SEmpty}
'SPC;PT4;PR-15,-15;ER30,30;PR15,15;';
arr_sym[3]:={SFilled}
'SP1;PR-15,-15;RR30,30;PR15,15;SPC;PR-12,-12;RR24,24;PR12,12;';
arr_sym[4]:={SCircle}
'SP1;PR-15,-15;RR30,30;PR15,15;SPC;PR-12,-12;RR24,24;PR12,12;SP1;WG10;';
arr_sym[5]:={SXross}
'SP1;PR-15,-15;RR30,30;PR15,15;SPC;PR-12,-12;RR24,24;PR12,12;SP1;PR-12,-2;RR24,4;PR12,2;PR-2,-12;RR4,24;PR2,12;';
arr_sym[6]:={CP,CirclePlus}
'SPC;PT4;CI15,5;PR-15,-2;RR30,4;PR15,2;PR-2,-15;RR4,30;PR2,15;';
arr_sym[7]:={CE,CircleEmpty}
'SPC;PT4;CI15,5;';
arr_sym[8]:={CF,CircleFilled}
'SP1;WG15;SPC;WG12;';
arr_sym[9]:={CC}
'SP1;WG15;SPC;WG12;SP1;WG6;';
arr_sym[10]:={CX}
'SP1;WG15;SPC;WG12;SP1;PR-15,-2;RR30,4;PR15,2;PR-2,-15;RR4,30;PR2,15;';
arr_sym[11]:={TP}
'SPC;PT2;CI15,120;PR-8,-2;RR20,4;PR8,2;';
arr_sym[12]:={TE}
'SPC;PT2;CI15,120;';
arr_sym[13]:={TF}
'SP1;PT2;CI15,120;SPC;PT6;CI12,120;CI8,120;CI4,120;';
arr_sym[14]:={TC}
'SP1;PT2;CI15,120;SPC;PT6;CI12,120;CI8,120;CI4,120;SP1;WG8;';
arr_sym[15]:={TX}
'SP1;PT2;CI15,120;SPC;PT6;CI12,120;CI8,120;CI4,120;SP1;PR-8,-2;RR20,4;PR8,2;';
arr_sym[16]:={DP}
'SPC;PT4;CI15,90;PR-14,-2;RR28,4;PR14,2;PR-2,-14;RR4,28;PR2,14;';
arr_sym[17]:={DE}
'SPC;PT4;CI15,90;';
arr_sym[18]:={DF}
'SP1;PT2;CI15,90;SPC;PT8;CI12,90;PR-8,-8;RR10,10;PR8,8;';
arr_sym[19]:={DC}
'SP1;PT2;CI15,90;SPC;PT8;CI12,90;PR-8,-8;RR10,10;PR8,8;SP1;WG8;';
arr_sym[20]:={DX}
'SP1;PT2;CI11,90;SPC;PT8;CI12,90;PR-8,-8;RR10,10;PR8,8;SP1;PR-14,-2;RR28,4;PR14,2;PR-2,-14;RR4,28;PR2,14;';
arr_sym[21]:={HP}
'SPC;PT4;CI15,45;PR-9,-1;RR18,2;PR9,1;PR-1,-9;RR2,18;PR1,9;';
arr_sym[22]:={HE}
'SPC;PT4;CI15,45;';
arr_sym[23]:={HF}
'SP1;PT2;CI15,45;SPC;PT6;CI8,45;CI6,45;CI4,45;CI2,45;';
arr_sym[24]:={HC}
'SP1;PT2;CI15,45;SPC;PT6;CI8,45;CI6,45;CI4,45;CI2,45;SP1;WG6;';
arr_sym[25]:={HX}
'SP1;PT2;CI15,45;SPC;PT6;CI8,45;CI6,45;CI4,45;CI2,45;SP1;PR-10,-1;RR20,2;PR10,1;PR-1,-10;RR2,20;PR1,10;';
arr_sym[26]:={XP}
'SPC;PT4;PR-8,-2;ER16,4;PR8,2;PR-2,-8;ER4,16;PR2,8;CI7,90;';
arr_sym[27]:={XE}
'SPC;PT4;PR-8,-2;RR16,4;PR8,2;PR-2,-8;RR4,16;PR2,8;';
arr_sym[28]:={XF}
'SP1;PT2;PR-9,-2;ER18,4;PR9,2;PR-2,-9;ER4,18;PR2,9;SPC;PR-8.5,-1.5;RR17,3;PR8.5,1.5;PR-1.5,-8.5;RR3,17;PR1.5,8.5;';
arr_sym[29]:={XC}
'SP1;PT2;PR-8,-4;ER16,8;PR8,4;PR-4,-8;ER8,16;PR4,8;SPC;PR-7.5,-3.5;RR15,7;PR7.5,3.5;PR-3.5,-7.5;RR7,15;PR3.5,7.5;';
arr_sym[30]:={XX}
'SP1;PT2;PR-4,-8;PD0,16;PU;PR-4,-4;PD16,0;PU;PR-4,4;PD0,-16;PU;PR4,4;PD-16,0;PU;PR4,-4;PR4,8;PR-3.5,-3.5;SPC;RR7,7;PR3.5,3.5;';
END; {Ini_Sym}

{smal.cercle} Arr_Sym[0]:='CI4,15;';
{BigPlus}     Arr_Sym[1]:=Thik[1]+
              'PR-2,7;PD;PR4,0,0,-5,5,0,0,-4,-5,0,0,-5,-4,0,0,5,-5,0,0,4,5,0,0,5;PU;';
{BigPlus2}    Arr_Sym[2]:=Arr_Sym[1]+Thik[3]+
              'PR1,-1;PD;PR2,0,0,-5,5,0,0,-2,-5,0,0,-5,-2,0,0,5,-5,0,0,2,5,0,0,5;PU;';
{BigCroi}     Arr_Sym[3]:=Thik[1]+
              'PR-4,6;PD;PR4,-4,4,4,2,-2,-4,-4,4,-4,-2,-2,-4,4,-4,-4,-2,2,4,4,-4,4,2,2;PU;';
{BigCroi2}    Arr_Sym[4]:=Arr_Sym[3]+Thik[3]+
              'PR0,-1;PD;PR4,-4,4,4,1,-1,-4,-4,4,-4,-1,-1,-4,4,-4,-4,-1,1,4,4,-4,4,1,1;PU;';
{PlusDiam}    Arr_Sym[5]:=Thik[1]+
              'PR0,9;PD;PR3,-6,6,-3,-6,-3,-3,-6,-3,6,-6,3,6,3,3,6;PU;';
{PlusDiam2}   Arr_Sym[6]:=Arr_Sym[5]+Thik[3]+
              'PR0,-3;PD;PR2,-4,4,-2,-4,-2,-2,-4,-2,4,-4,2,4,2,2,4;PU;';
{CroiDiam}    Arr_Sym[7]:=Thik[1]+
              'PR0,4;PD;PR8,4,-4,-8,4,-8,-8,4,-8,-4,4,8,-4,8,8,-4;PU;';
{CroiDiam2}   Arr_Sym[8]:=Arr_Sym[7]+Thik[3]+
              'PR0,-2;PD;PR4,2,-2,-4,2,-4,-4,2,-4,-2,2,4,-2,4,4,-2;PU;';
{trian.up}    Arr_Sym[9]:=Thik[1]+'PR0,8;PD10,-14,-20,0,10,14;PU;';
              {Arr_Sym[9]:=Thik[1]+'PR0,6;PD7,-10,-14,0,7,10;PU;';}
{trian.up2}   Arr_Sym[10]:=Arr_Sym[9]
              +Thik[3]+'PR0,-5;PD3,-5,-6,0,3,5;PU;'
              +'PR0,1;PD5,-7,-10,0,5,7;PU;';

{trian.down}  Arr_Sym[11]:=Thik[1]+'PR0,-8;PD;PR-10,14,20,0,-10,-14;PU;';
              {Arr_Sym[11]:=Thik[1]+'PR0,-6;PD;PR-7,10,14,0,-7,-10;PU;';}
{trian.down2} Arr_Sym[12]:=Arr_Sym[11]
              +Thik[3]+'PR0,5;PD;PR-3,5,6,0,-3,-5;PU;'
              +'PR0,-1;PD-5,7,10,0,-5,-7;PU;';;

{YHauDiam}    Arr_Sym[13]:=Thik[1]+
              'PR0,2;PD;PR7,2,-5,-5,-2,-7,-2,7,-5,5,7,-2;PU;';
{YHauDiam2}   Arr_Sym[14]:=Arr_Sym[13]+Thik[3]+
              'PR0,-1;PD;PR4,1,-3,-3,-1,-4,-1,4,-3,3,4,-1;PU;';

{YBasDiam}    Arr_Sym[15]:=Thik[1]+
              'PR0,8;PD;PR2,-7,5,-5,-7,2,-7,-2,5,5,2,7;PU;';
{YBasDiam2}   Arr_Sym[16]:=Arr_Sym[15]+Thik[3]+
              'PR0,-4;PD;PR1,-3,3,-3,-4,1,-4,-1,3,3,1,3;PU;';

{YHauBig}     Arr_Sym[17]:=Thik[1]+
              'PR0,2;PD;PR6,4,2,-3,-6,-4,0,-7,-4,0,0,7,-6,4,2,3,6,-4;PU;';
{YHauBig2}    Arr_Sym[18]:=Arr_Sym[17]+Thik[3]+
              'PR0,-1;PD;PR5,3,1,-1,-5,-4,0,-6,-2,0,0,6,-5,4,1,1,5,-3;PU;';

{YBasBig}     Arr_Sym[19]:=Thik[1]+
              'PR2,8;PD;PR0,-7,6,-4,-2,-3,-6,4,-6,-4,-2,3,6,4,0,7,4,0;PU;';
{YBasBig2}    Arr_Sym[20]:=Arr_Sym[19]+Thik[3]+
              'PR-1,-1;PD0,-6,5,-4,-1,-1,-5,3,-5,-3,-1,1,5,4,0,6,2,0;PR;PU;';

{empt.diamon} Arr_Sym[21]:=Thik[1]+'PR0,-10;PD;PR-10,10,10,10,10,-10,-10,-10;PU;PR0,10;';
{fill.diamon} Arr_Sym[22]:=Arr_Sym[21]+Thik[3]+
              'PR0,-6;PD;PR-6,6,6,6,6,-6,-6,-6;PU;PR0,6;'+
              'PR0,-3;PD;PR-3,3,3,3,3,-3,-3,-3;PU;PR0,3;';
{empt.square} Arr_Sym[23]:=Thik[1]+'PR-8,-8;ER16,16;PR8,8;';
{fill.square} Arr_Sym[24]:='PR-8,-8;FT2;RR16,16;PR8,8;';
{empt.circle} Arr_Sym[25]:=Thik[1]+'CI11,5;';
{fill.circle} Arr_Sym[26]:='WG11,0,360,15;CI11,15;';
{+}           Arr_Sym[27]:=Thik[1]+'PD;PR-5,0,10,0,-5,0,0,-5,0,10,0,-5;PU;';
{cross}       Arr_Sym[28]:=Thik[1]+'PD;PR-5,-5,10,10,-5,-5,5,-5,-10,10,5,-5;PU;';
{Y_up}        Arr_Sym[29]:=Thik[1]+'PD;PR-5,-3,5,3,0,6,0,-6,5,-3,-5,3;PU;';
{Y_down}      Arr_Sym[30]:=Thik[1]+'PD;PR-5,3,5,-3,0,-6,0,6,5,3,-5,-3;PU;';

PROCEDURE Plot_ZeroCadre(x0,y0,xmm,ymm,d:integer);
BEGIN {Plot_ZeroCadre}
 WriteLn(Ps,'%%%%REPERES ',Nam_Ps);
 WriteLn(Ps,'gsave 0.5 sl _colB 0.9 sg');
(**
 writeln(Ps,//lowlef
'newpath ',x0+d,' mm ',y0,' mm moveto ',x0,' mm ',y0,' mm lineto ',x0,' mm ',y0+d,' mm lineto stroke');
 x0:=x0+xmm;y0:=y0+ymm;
 writeln(Ps,//_uprite
'newpath ',x0,' mm ',y0-d,' mm moveto ',x0,' mm ',y0,' mm lineto ',x0-d,' mm ',y0,' mm lineto stroke');
**)
 WriteLn(Ps,'grestore');
 WriteLn(Ps,'%%%%END REPERES ',Nam_Ps)
END;{Plot_ZeroCadre}


