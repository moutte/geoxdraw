unit LibGrf;

INTERFACE

USES Libio, Graphics;

CONST
 Sym_CF   = 5;{CircleFilled}
 Sym_SF   = 1;{Square Filled}
 Sym_P    = 25;{simple plus, for Plot_LabelOnly}
 Max_Coul   = 8;
 Max_Symb   = 34;
 Max_Point  = 800;
 CodColor='BRGYLVTO';
{Black / Red / Green / Yellow / bLue / Violet / Turquoise / Orange}
 CodSymbol=
'_SF_SE_SP_SX_CF_CE_CP_CX_DF_DE_DP_DX_AF_AE_BF_BE_VF_VE_WF_WE_PF_PE_QF_QE__P_XF_XE_RF_RE__X_TF_TE_UF_UE';
{--1--2--3--4--5--6--7--8--9-10-11-12-13-14-15-16-17-18-19-20-21-22-23-24-25-26-27-28-29-30-31-32-33-34}
//CodAutic='BSFRSFGSFYSFLSFVSFTSFOSFBSFRSFGSFYSFLSFVSFTSFOSF';
 Length_CodSym = 3;
 ArrPrm_SymSpd : array[1..24] of byte
 =(1,5, 9,13,15,17,19,21,23,26,28,31,2,6,10,14,16,18,20,22,24,27,29,32);
TYPE
 Coor_XY     = (Cor_X, Cor_Y);
 P_Real      = array[Coor_XY] of single;
 P_Intg      = array[Coor_XY] of integer;

VAR
 B_TrgDown: boolean;
 BitW,BitH:Word;
 B_Complex, Num_Single, B_Muet:         boolean;
 Log:                                  	array[Coor_XY] of boolean;
 Log_Max, P_Min, P_Max, P_Hih, P_Low:  	P_Real;
//P_Min-P_Max= bornes Min-Max calculated
//P_Hih-P_Low= bornes du dessin
 Log_Mod:                              	P_Intg;
 I_Label, I_Coul, I_Symb, I_Tic, I_SymCol, I_Legend, I_SymbLeg, I_CoulLeg:  byte;
 Lis_SelX, Lis_SelY,
 Lis_Coul, Lis_Symb, Lis_CoulLeg, Lis_SymbLeg,
 LisAutomatic,LisAutoSymCol,LisAutoLegend: string;

 I_SampX, I_SampY, I_SelX, I_SelY: Byte;
 CurColor:                              TColor;
 SizBorn, SizTitr, SizLabl:             Byte;

 MaxX, MaxY,
 DesMaxX, DesMaxY, DesMinX, DesMinY: word;
 X0Trg, X1Trg, Y0Trg, Y1Trg: byte;

 arP: array[1..Max_Point] of P_Real;{coordinates}
 arN: array[1..Max_Point] of Str_Signif;{label}
 arS: array[1..Max_Point] of Byte;{symbol}
 arC: array[1..Max_Point] of Byte;{color}

PROCEDURE Init_Grf(X_Lef,X_Rit,Y_Hau,Y_Bas: Byte);
PROCEDURE Test_Grf(Cvs: TCanvas;VAR Dim:Word);
PROCEDURE Calc_Crt(p: p_real; VAR cx, cy: integer; VAR ok: boolean);
PROCEDURE Calc3D_Crt(x,y,z: single; VAR cx, cy, cz: integer; VAR ok: boolean);
PROCEDURE CalcInv_Crt(cx,cy:integer; VAR px,py:real; VAR ok:boolean);
PROCEDURE TrgCalc_Crt(x, y: real; VAR cx, cy: integer; VAR ok: boolean);
FUNCTION Pivot(c: coor_xy): real;
FUNCTION Cal_Barb(delta: real): byte;
PROCEDURE Def_Bornes(NegaOK,Trg:boolean; c:coor_xy);
PROCEDURE Init_LibGrf;

PROCEDURE Cad_Crt(Cvs: TCanvas; B_Closed: boolean; Tit_X, Tit_Y, TitDes: string; barbx, barby: byte);
PROCEDURE Cad_Spd(Cvs: TCanvas; Prm: ArrEle_Byte; Frm: Forme);
PROCEDURE Cross_XY(C: TCanvas);
PROCEDURE Label_Spd(Cvs:TCanvas; i:byte; s:Str_Signif);
PROCEDURE Label_Histo(Cvs:TCanvas; i:byte; s:Str_Signif);
PROCEDURE Label_Profil(Cvs:TCanvas; Frm: Forme; Yes_Pro: ArrEle_Bool);
PROCEDURE TrgCad_Crt(Cvs: TCanvas; Canevas: boolean; Tit_X, Tit_Y, Tit_Z, Tit_Des: string);
PROCEDURE Des_SymCol(Cvs:TCanvas;BA,BT,BS,BC:boolean);
PROCEDURE Des_SymCol_P38(Cvs:TCanvas;L_Sam:String);
PROCEDURE Des_Symb_Crt(Cvs: TCanvas; icol, isym, Tail: byte; x, y: integer);
PROCEDURE Des_Point_Crt(Cvs: TCanvas; c, i, T: byte; x, y: integer; N: Str_Signif);
PROCEDURE Des_Crt(Cvs:TCanvas;B_Prof,B_Mono,B_Trg,B_Clos:boolean;Dim:Word;Tail:byte);
PROCEDURE Des_Lin_Crt(Cvs:TCanvas;c:byte;b0,b1:single;Dim:Word);
//PROCEDURE Des_Spider(Cvs:TCanvas; Nb_Spd,Dim:Byte);

IMPLEMENTATION
USES Windows, Types;
VAR
 XMargin, YMargin, Base_Len: integer; {FOR Triangle}

FUNCTION Pivot(c: Coor_XY): real;
VAR p: real;
BEGIN
 p:= 1.0E6; REPEAT p:= p/10 UNTIL Log_Max[c]>=p;
 pivot:= LN(Log_Max[c]/p)/LN(10)
END;

FUNCTION Cal_Barb(delta: real): byte;
VAR M: byte;
BEGIN
 IF Delta<=0 THEN Delta:= 10;
 WHILE delta<10 DO delta:= 10* delta;
 M:= 17; REPEAT dec(M) UNTIL (round(delta) mod M = 0) OR (M=1);
 Cal_Barb:= M
END; {Cal_Barb}

{
*constantes qui renvoient aux couleurs du système les plus
approchantes =
 clAqua, clBlack, clBlue, clDkGray, clFuchsia, clGray, clGreen,
 clLime, clLtGray, clMaroon, clNavy, clOlive, clPurple, clRed,
 clSilver, clTeal, clWhite, et clYellow.
*constantes qui renvoient aux couleurs des éléments écran
 du système = clActiveBorder, clActiveCaption, clAppWorkSpace,
 clBackground, clBtnFace, clBtnHighlight, clBtnShadow, clBtnText,
 clCaptionText, clGrayText, clHighlight, clHighlightText,
 clInactiveBorder, clInactiveCaption, clInactiveCaptionText, clMenu,
 clMenuText, clScrollBar, clWindow, clWindowFrame et clWindowText
}
PROCEDURE Init_LibGrf;
VAR c: coor_xy;
BEGIN
 {Suf_X:= ''; Suf_Y:= '';}
 {Ini_NomSym; Ini_NomCol;}

 B_Muet:=false; Num_Single:= FALSE; B_Complex:= false;

 FOR c:= Cor_X TO Cor_Y DO BEGIN
  Log[c]:= false; //Arr_Atm[c]:= false;
  P_Min[c]:=0.0; P_Max[c]:= 1.0;//P_Min-P_Max= bornes Min-Max calculated
  P_Low[c]:=0.0; P_Hih[c]:= 1.0;//P_Hih-P_Low= bornes du dessin
  Log_Max[c]:= 1; Log_Mod[c]:= 2
 END;

 I_Coul:= 0; I_Symb:= 0; I_Tic:= 0; I_Label:= 1; I_SymCol:=2;
 SizTitr:=12; SizLabl:=SizTitr-2; SizBorn:=SizTitr-2;

 I_SampX:=1; I_SampY:=1; I_SelX:=0; I_SelY:=0;
 ArExp[CorX]:=Def_Exp; ArExp[CorY]:=Def_Exp; ArExp[CorZ]:=Def_Exp;
 Lis_SelX:=''; Lis_SelY:='';
 Lis_Coul:=''; Lis_Symb:='';
 Lis_CoulLeg:=''; Lis_SymbLeg:='';
 X0Trg:=0; X1Trg:=100; Y0Trg:=0; Y1Trg:=100;
END;{Ini_LibGrf}

PROCEDURE Init_Grf(X_Lef,X_Rit,Y_Hau,Y_Bas: Byte);//nbre de pixels libres sur les cotes
BEGIN
 MaxX:=BitW; MaxY:=BitH;//taille totale de la toile en pixels
//for Triangles
 XMargin:= 20;//left and right, for triangle
 YMargin:= 50;//distance from bottom, for triangle
 Base_Len:= MaxX - 2*XMargin;{length of the base}
{}
 DesMinX:= X_Lef; DesMaxX:= MaxX-X_Rit;
 DesMinY:= Y_Hau; DesMaxY:= MaxY-Y_Bas;
END; {Init_Grf}

PROCEDURE Line(C:TCanvas; X1,Y1,X2,Y2: integer);
BEGIN WITH C DO BEGIN MoveTo(X1,Y1); LineTo(X2,Y2) END END;

PROCEDURE Cross_XY(C: TCanvas);
var x,y: word;
BEGIN
 x:= DesMinX+(DesMaxX-DesMinX) div 2;
 y:= DesMinY+(DesMaxY-DesMinY) div 2;
 Line(C,x,DesMinY,x,DesMaxY);
 Line(C,DesMinX,y,DesMaxX,y);
END;//Cross_XY

procedure Axe_X(C: TCanvas; B_Closed: boolean; x0, x1: real; barb: byte; tt: string);
var x, dx: integer; i, j: byte;
begin
  C.Font.Name:='Georgia'; //'Bitstream Vera Serif';
  //
  //BARBULATION
  C.Pen.Width:=1;
  C.Pen.Color:=clBlack;
  if Log[Cor_X] then
  for i:= 0 to Log_Mod[Cor_X]+1 do for j:= 1 to 9 do begin
    x:= DesMinX + round((DesMaxX - DesMinX)*(i+LN(j)/LN(10) - Pivot(Cor_X))/Log_Mod[Cor_X]);
    if j=1 then begin
      C.Pen.Width:=2;
      line(C, x, DesMaxY, x, DesMaxY + 12);
      line(C, x, DesMinY, x, DesMinY - 12);
      C.Pen.Width:=1;
      C.Pen.Style:=psDot;
      C.Pen.Color:=clAqua; //clRed;
      line(C, x, DesMinY, x, DesMaxY);
      C.Pen.Style:=psSolid;
      C.Pen.Color:=clBlack;
    end
    else begin
      C.Pen.Width:=1;
      line(C, x, DesMaxY, x, DesMaxY + 8);
      line(C, x, DesMinY, x, DesMinY - 8)
    end
  end
  else 
  if barb>1 then begin
    i:= 0;
    repeat
      x:= DesMinX + round(i*(DesMaxX-DesMinX)/barb);
      line(c, x, DesMaxY, x, DesMaxY + 8);
      line(c, x, DesMinY, x, DesMinY + 8);
      //
      C.Pen.Style:=psDot;
      C.Pen.Color:=clAqua; //clRed;
      line(C, x, DesMinY, x, DesMaxY);
      C.Pen.Style:=psSolid;
      C.Pen.Color:=clBlack;
      //
      inc(i)
    until (B_Closed and (x>DesMaxX)) OR (x>MaxX)
  end;
  //
  //FRAME
  C.Pen.Width:=2; //C.Pen.Style:=?
  C.Pen.Style:=psSolid;
  C.Pen.Color:=clBlack;
  if B_Closed then begin
   Line(C,DesMinX,DesMaxY,DesMaxX,DesMaxY);
   Line(C,DesMinX,DesMinY,DesMaxX,DesMinY)
  end
  else begin
   if Log[Cor_X] then Line(C,0,DesMaxY,MaxX,DesMaxY)
   else Line(C,DesMinX,DesMaxY,MaxX,DesMaxY)
  end;
  //
  //write the values of Bornes_X
  if Log[Cor_X] then begin
   x1:= Log_Max[Cor_X]; x0:= x1;
   for i:= 1 to Log_Mod[Cor_X] do x0:= x0/10
  end;
  dx:= 0;
  if (X1-X0)<>0 then begin
   C.Font.Size:=SizBorn;
   Line(C,DesMaxX, DesMaxY, DesMaxX, DesMaxY + 6);
   dx:=C.TextWidth(Real_Str(x1))+2;
   C.TextOut(DesMaxX-dx, DesMaxY+8, Real_Str(x1));
   Line(C,DesMinX, DesMaxY, DesMinX, DesMaxY + 6);
   C.TextOut(DesMinX+2, DesMaxY+8, Real_Str(x0))
  end;
  //
  //Tit_X
  C.Font.Size:=SizTitr;
  dx:=dx+C.TextWidth(tt)+20;
  C.TextOut(DesMaxX-dx, DesMaxY+10, tt);
end;//Axe_X

procedure Axe_Y(C: TCanvas; B_Closed: boolean; y0, y1: real; barb: byte; tt: string);
var y, dx, dy: integer; i, j: byte;
begin
  //
  //BARBULATION
  C.Pen.Width:=1;
  C.Pen.Color:=clBlack;
  if Log[Cor_Y] then
  for i:= 0 to Log_Mod[Cor_Y]+1 do for j:= 1 to 9 do begin
   y:= DesMaxY - round((DesMaxY - DesMinY)*(i+LN(j)/LN(10)
       - Pivot(Cor_Y))/Log_Mod[Cor_Y]);
   if j=1 then begin
     C.Pen.Width:=2;
     line(c,DesMinX, y, DesMinX - 12, y);
     line(c,DesMaxX, y, DesMaxX + 12, y);
     C.Pen.Width:=1;
     C.Pen.Style:=psDot;
     C.Pen.Color:=clAqua; //clRed;
     line(C, DesMinX, y, DesMaxX, y);
     C.Pen.Style:=psSolid;
     C.Pen.Color:=clBlack;
   end
   else begin
     C.Pen.Width:=1;
     line(c,DesMinX, y, DesMinX - 8, y);
     line(c,DesMaxX, y, DesMaxX + 8, y);
   end
  end
  else 
  if barb>1 then begin
    i:=1;
    repeat
      y:= DesMinY + round(i*(DesMaxY-DesMinY)/barb);
      line(c,DesMinX, y, DesMinX - 8, y);
      line(c,DesMaxX, y, DesMaxX - 8, y);
      //
      C.Pen.Style:=psDot;
      C.Pen.Color:=clAqua; //clRed;
      line(C, DesMinX, y, DesMaxX, y);
      C.Pen.Style:=psSolid;
      C.Pen.Color:=clBlack;
      //
      inc(i)
    until  (B_Closed and (i>Barb)) OR (i=barb) //y<=0
  end;
  //
  //FRAME
  C.Pen.Width:=2; //C.Pen.Style:=psSolid
  C.Pen.Color:=clBlack;
  C.Pen.Style:=psSolid;
  if B_Closed then begin
    line(c,DesMinX,DesMinY,DesMinX,DesMaxY);
    line(c,DesMaxX,DesMinY,DesMaxX,DesMaxY)
  end
  else begin
    if Log[Cor_Y] then line(c,DesMinX,MaxY,DesMinX,0)
    else line(c,DesMinX,DesMaxY,DesMinX,0)
  end;
  //
  //Tit_Y
  //select Font, VertDir, and Right/Center justif°
  //C.TextOut(DesMinX-6, DesMinY+(DesMaxY-DesMinY)div 2, tt);
  C.Font.Name:= 'Georgia'; //'Times New Roman';
  C.Font.Size:=SizTitr;
  C.TextOut(6, 6, tt);//en haut a gauche
  //
  //Bornes_Y
  if Log[Cor_Y] then begin
    y1:= Log_Max[Cor_Y]; y0:= y1;
    for i:= 1 to Log_Mod[Cor_Y] do y0:= y0/10
  end;
  if (Y1-Y0)<>0 then begin
    C.Font.Size:=SizBorn;
    line(c,DesMinX, DesMinY,DesMinX-8, DesMinY);
    dx:=C.TextWidth(Real_Str(y1))+4;
    C.TextOut(DesMinX-dx, DesMinY+2, Real_Str(y1));
    line(c,DesMinX, DesMaxY,DesMinX-8, DesMaxY);
    dx:=C.TextWidth(Real_Str(y0))+4;
    dy:=C.TextHeight(Real_Str(y0))+2;
    C.TextOut(DesMinX-dx, DesMaxY-dy, Real_Str(y0))
  end;
end;//Axe_Y

//PROCEDURE Axe_X(C: TCanvas; B_Closed: boolean; x0, x1: real; barb: byte; tt: string);
//VAR x, dx: integer; i, j: byte;
//BEGIN
//{set line style etc...}
// IF Log[Cor_X] THEN BEGIN
//  x1:= Log_Max[Cor_X]; x0:= x1;
//  FOR i:= 1 TO Log_Mod[Cor_X] DO x0:= x0/10
// END;
// C.Pen.Width:=2; {C.Pen.Style:=?}
// C.Pen.Color:=clBlack;
// IF B_Closed THEN BEGIN
//  Line(C,DesMinX,DesMaxY,DesMaxX,DesMaxY);
//  Line(C,DesMinX,DesMinY,DesMaxX,DesMinY)
// END
// ELSE BEGIN
//  IF Log[Cor_X] THEN Line(C,0,DesMaxY,MaxX,DesMaxY)
//  ELSE Line(C,DesMinX,DesMaxY,MaxX,DesMaxY)
// END;
//{write the values of Bornes_X}
// dx:= 0;
// IF (X1-X0)<>0 THEN BEGIN
//  C.Font.Size:=SizBorn;
//  Line(C,DesMaxX, DesMaxY, DesMaxX, DesMaxY + 6);
//  dx:=C.TextWidth(Real_Str(x1))+2;
//  C.TextOut(DesMaxX-dx, DesMaxY+8, Real_Str(x1));
//  Line(C,DesMinX, DesMaxY, DesMinX, DesMaxY + 6);
//  C.TextOut(DesMinX+2, DesMaxY+8, Real_Str(x0))
// END;
//(**Tit_X**)
// C.Font.Size:=SizTitr;
// dx:=dx+C.TextWidth(tt)+20;
// C.TextOut(DesMaxX-dx, DesMaxY+10, tt);
//{BARBULATION}
// C.Pen.Width:=1;
// IF Log[Cor_X] THEN
// FOR i:= 0 TO Log_Mod[Cor_X]+1 DO FOR j:= 1 TO 9 DO BEGIN
//  x:= DesMinX + round((DesMaxX - DesMinX)*(i+LN(j)/LN(10)
//      - Pivot(Cor_X))/Log_Mod[Cor_X]);
//  if j=1 then C.Pen.Width:=2 else C.Pen.Width:=1;
//  line(C, x, DesMaxY, x, DesMaxY + 3);
//  line(C, x, DesMinY, x, DesMinY - 3);
// END
// ELSE BEGIN
//  i:= 0;
//  REPEAT
//   x:= DesMinX + round(i*(DesMaxX-DesMinX)/barb);
//   line(c, x, DesMaxY, x, DesMaxY + 3);
//   line(c, x, DesMinY, x, DesMinY + 3);
//   inc(i)
//  UNTIL (B_Closed AND (x>DesMaxX)) OR (x>MaxX)
// END;
//END; {Axe_X}
//
//PROCEDURE Axe_Y(C: TCanvas; B_Closed: boolean; y0, y1: real; barb: byte; tt: string);
//VAR y, dx, dy: integer; i, j: byte;
//BEGIN
// IF Log[Cor_Y] THEN BEGIN
//  y1:= Log_Max[Cor_Y]; y0:= y1;
//  FOR i:= 1 TO Log_Mod[Cor_Y] DO y0:= y0/10
// END;
//
// C.Pen.Width:=2; {C.Pen.Style:=?}
// C.Pen.Color:=clBlack;
// IF B_Closed THEN BEGIN
//  line(c,DesMinX,DesMinY,DesMinX,DesMaxY);
//  line(c,DesMaxX,DesMinY,DesMaxX,DesMaxY)
// END
// ELSE BEGIN
//  IF Log[Cor_Y] THEN line(c,DesMinX,MaxY,DesMinX,0)
//  ELSE line(c,DesMinX,DesMaxY,DesMinX,0)
// END;
//{Tit_Y}
//{select Font, VertDir, and Right/Center justif°}
//{C.TextOut(DesMinX-6, DesMinY+(DesMaxY-DesMinY)div 2, tt);}
// C.Font.Name := 'Times New Roman';
// C.Font.Size:=SizTitr;
// C.TextOut(6, 6, tt);{en haut a gauche}
//{Bornes_Y}
// IF (Y1-Y0)<>0 THEN BEGIN
//  C.Font.Size:=SizBorn;
//  line(c,DesMinX, DesMinY,DesMinX-8, DesMinY);
//  dx:=C.TextWidth(Real_Str(y1))+4;
//  C.TextOut(DesMinX-dx, DesMinY+2, Real_Str(y1));
//  line(c,DesMinX, DesMaxY,DesMinX-8, DesMaxY);
//  dx:=C.TextWidth(Real_Str(y0))+4;
//  dy:=C.TextHeight(Real_Str(y0))+2;
//  C.TextOut(DesMinX-dx, DesMaxY-dy, Real_Str(y0))
// END;
//{BARBULATION}
// C.Pen.Width:=1;
// IF Log[Cor_Y] THEN
// FOR i:= 0 TO Log_Mod[Cor_Y]+1 DO FOR j:= 1 TO 9 DO BEGIN
//  y:= DesMaxY - round((DesMaxY - DesMinY)*(i+LN(j)/LN(10)
//      - Pivot(Cor_Y))/Log_Mod[Cor_Y]);
//  if j=1 then C.Pen.Width:=2 else C.Pen.Width:=1;
//  line(c,DesMinX, y, DesMinX - 4, y);
//  line(c,DesMaxX, y, DesMaxX + 4, y);
// END
// ELSE BEGIN
//  i:= 0;
//  REPEAT
//   y:= DesMaxY - round(i*(DesMaxY-DesMinY)/barb);
//   line(c,DesMinX, y, DesMinX - 4, y);
//   line(c,DesMaxX, y, DesMaxX - 4, y);
//   inc(i)
//  UNTIL y<0
// END
//END; {Axe_Y}
//
PROCEDURE Calc3D_Crt(x,y,z: single; VAR cx, cy, cz: integer; VAR ok: boolean);
var x0, y0: integer; xp, yp: single;
//x0,y0: coordinates of center point in canvas
begin
 xp:= x0 + y - x/2;
 yp:= x0 - x*0.733 - z;
end;//Calc3D_Crt

PROCEDURE Calc_Crt(p:p_real; VAR cx,cy:integer; VAR ok:boolean);
VAR c: coor_xy;
BEGIN
 ok:= true;
 FOR c:= cor_x TO cor_y DO BEGIN
  IF Log[c] THEN BEGIN
   IF p[c]>0 THEN
    p[c]:= (LN(p[c]/Log_Max[c])/LN(10)+Log_Mod[c])/Log_Mod[c]
   ELSE ok:= FALSE
  END
  ELSE {IF not log} p[c]:= (p[c]-P_Low[c])/(P_Hih[c]-P_Low[c]);
  ok:= ok and (p[c]>=-2) and (p[c]<=2)
 END;{FOR}
 IF ok THEN BEGIN
  cx:= round((DesMaxX-DesMinX)* p[cor_x]) + DesMinX;
  cy:= DesMaxY - round((DesMaxY-DesMinY)* p[cor_y])
 END
END; {Calc_Crt}

PROCEDURE CalcInv_Crt(cx,cy:integer; VAR px,py:real; VAR ok:boolean);
BEGIN
 ok:= true;
 px:=(cx - DesMinX)/(DesMaxX-DesMinX);
 px:=px*(P_Hih[Cor_X]-P_Low[Cor_X])+P_Low[Cor_X];
 py:=(DesMaxY - cy)/(DesMaxY-DesMinY);
 py:=py*(P_Hih[Cor_Y]-P_Low[Cor_Y])+P_Low[Cor_Y];
END; {CalcInv_Crt}

PROCEDURE TrgCalc_Crt(x, y: real; VAR cx, cy: integer; VAR ok: boolean);
var z: real;
BEGIN
 ok:= true;
 if (X1Trg-X0Trg)>Epsilon then x:= (x-X0Trg)/(X1Trg-X0Trg)
 else ok:=false;
 if ok then begin
  if (Y1Trg-Y0Trg)>Epsilon then y:= (y-Y0Trg)/(Y1Trg-Y0Trg)
  else ok:=false;
  if ok then z:= 1 - x - y
 end;

 if ok then begin
  if B_TrgDown then begin
   IF (x+z/2)*Base_Len<32000
   THEN cx:= XMargin + round((x+z/2)*Base_Len)
   ELSE ok:= false;
   IF ok and ((x+y)*0.866*Base_Len<32000)
   THEN cy:= MaxY - YMargin - round((x+y)*0.866*Base_Len)
   ELSE ok:= false
  end
  else begin
   IF (x+y/2)*Base_Len<32000
   THEN cx:= XMargin + round((x+y/2)*Base_Len)
   ELSE ok:= false;
   IF ok and (y*0.866*Base_Len<32000)
   THEN cy:= MaxY - YMargin - round(y*0.866*Base_Len)
   ELSE ok:= false
  end
 end
END; {TrgCalc_Crt}

PROCEDURE Cad_Crt(Cvs: TCanvas; B_Closed: boolean; Tit_X, Tit_Y, TitDes: string; barbx, barby: byte);
BEGIN
 Cvs.Font.Name := 'Times New Roman';
 Cvs.Pen.Color:=clBlack;
 Axe_X(Cvs,B_Closed,P_Low[Cor_X],P_Hih[Cor_X],barbx,Tit_X);
 Axe_Y(Cvs,B_Closed,P_Low[Cor_Y],P_Hih[Cor_Y],barby,Tit_Y);
 Cvs.Font.Size:=SizBorn
END; {Cad_Crt}

PROCEDURE Lin_Trg(Cvs:TCanvas; xa,ya,xb,yb: real);
VAR Ok: boolean; cxa, cya, cxb, cyb: integer;
BEGIN
 TrgCalc_Crt(xa,ya,cxa,cya,ok);
 IF ok THEN TrgCalc_Crt(xb,yb,cxb,cyb,ok);
 IF ok THEN Line(Cvs, cxa, cya, cxb, cyb)
END;{Lin_Trg}

PROCEDURE TrgCad_Crt(Cvs: TCanvas; Canevas: boolean; Tit_X, Tit_Y, Tit_Z, Tit_Des: string);
VAR h, Xx, Xy, Yx, Yy, Zx, Zy: integer; i: byte; S: string;
BEGIN
{coord of Y-apex}
 if B_TrgDown then begin
  Zx:=XMargin + Base_Len div 2; Zy:=MaxY-YMargin;
  Xx:=MaxX-XMargin; Xy:=MaxY - YMargin - round(0.866*Base_Len);
  Yx:=XMargin; Yy:=Xy;
 end
 else begin
  Zx:=XMargin; Zy:=MaxY-YMargin;
  Xx:=MaxX-XMargin; Xy:=MaxY-YMargin;
  Yx:=XMargin + Base_Len div 2;
  Yy:=MaxY - YMargin - round(0.866*Base_Len)
 end;
 WITH Cvs DO BEGIN
  Font.Name:='Times New Roman';
  h:=TextHeight('A');
  Pen.Color:=clBlack;
  Pen.Width:=2;
 END;
 Line(cvs,Zx,Zy,Xx,Xy);
 Line(cvs,Xx,Xy,Yx,Yy);
 Line(cvs,Yx,Yy,Zx,Zy);
{Tit_Y}
 IF Y1Trg<100 THEN BEGIN Str(Y1Trg,S); S:=' ='+S END ELSE s:='';
 Cvs.TextOut(Yx,Yy-h,Tit_Y+S);
{Tit_X}
 IF X1Trg<100 THEN BEGIN Str(X1Trg,S); S:=' ='+S END ELSE s:='';
 if B_TrgDown then Cvs.TextOut(Xx-Cvs.TextWidth(Tit_X+s)+4,Xy-h,Tit_X+S)
 else Cvs.TextOut(Xx-Cvs.TextWidth(Tit_X+s)+4,Xy+4,Tit_X+S);
{Tit_Z}
 IF X0Trg+Y0Trg>0 THEN BEGIN Str(100-X0Trg-Y0Trg,S); S:=' ='+S END
 ELSE s:='';
 Cvs.TextOut(Zx-4,Zy+4,Tit_Z+S);
{}
 IF Canevas THEN BEGIN
  Cvs.Pen.Width:=1;
  FOR i:= 1 TO 9 DO Lin_Trg(cvs,i*10,0,0,i*10);
  FOR i:= 1 TO 9 DO Lin_Trg(cvs,i*10,0,i*10,(10-i)*10);
  FOR i:= 1 TO 9 DO Lin_Trg(cvs,0,i*10,(10-i)*10,i*10);
 END
END; {TrgCad_Crt}

PROCEDURE Des_Symb_Crt(Cvs: TCanvas; icol, isym, Tail: byte; x, y: integer);

 PROCEDURE SmallCross;
 BEGIN line(Cvs,x-1,y-1,x+1,y+1);line(Cvs,x+1,y-1,x-1,y+1) END;

 PROCEDURE Cross(jT: byte);
 BEGIN
  Cvs.Pen.Width:=2;
  if jT>2 then jT:=jT-2;
  line(Cvs,x-jT,y-jT,x+jT,y+jT);line(Cvs,x+jT,y-jT,x-jT,y+jT);
  Cvs.Pen.Width:=1
 END;

 PROCEDURE Plus(T,jT: byte);
 BEGIN
  Cvs.Pen.Width:=T;
  line(Cvs,x,y-jT,x,y+jT);line(Cvs,x-jT,y,x+jT,y);
  Cvs.Pen.Width:=1
 END;

 PROCEDURE Squar(j:byte);
 BEGIN
  Cvs.rectangle(x-j,y-j,x+j,y+j); Cvs.rectangle(x-j-1,y-j-1,x+j-1,y+j-1)
 END;

 PROCEDURE Circl(j:byte);
 BEGIN
  Cvs.ellipse(x-j,y-j,x+j,y+j); Cvs.ellipse(x-j-1,y-j-1,x+j+1,y+j+1)
 END;

 PROCEDURE Diamond(j:byte);
 BEGIN
  Cvs.Polygon([Point(x,y-j),Point(x+j,y),Point(x,y+j),Point(x-j,y)]);
  inc(j);
  Cvs.Polygon([Point(x,y-j),Point(x+j,y),Point(x,y+j),Point(x-j,y)]);
 END;{Diamond}

 PROCEDURE Triangl(j: byte);
 BEGIN
  Cvs.Polygon([Point(x+j,y),Point(x-j,y+j),Point(x-j,y-j)])
 END;

 PROCEDURE Tri_Haut(j: byte);
 BEGIN
  Cvs.Polygon([Point(x,y+j),Point(x-j,y-j),Point(x+j,y-j)])
 END;

 PROCEDURE Tri_Bas(j: byte);
 BEGIN
  Cvs.Polygon([Point(x,y-j),Point(x-j,y+j),Point(x+j,y+j)])
 END;

 PROCEDURE BigPlus(j:byte);
 BEGIN
  Cvs.Polygon(
  [Point(x+j,y+2*j),Point(x+j,y+j),Point(x+2*j,y+j),Point(x+2*j,y-j),
  Point(x+j,y-j),Point(x+j,y-2*j),Point(x-j,y-2*j),Point(x-j,y-j),
  Point(x-2*j,y-j),Point(x-2*j,y+j),Point(x-j,y+j),Point(x-j,y+2*j)])
 END;{BigPlus}

 PROCEDURE BigPlus1(j:byte);
 BEGIN
  Cvs.PolyGon([
  Point(x+3*j,y),Point(x+j,y+j),Point(x,y+3*j),Point(x-j,y+j),
  Point(x-3*j,y),Point(x-j,y-j),Point(x,y-3*j),Point(x+j,y-j)])
 END;{BigCroi}

 PROCEDURE BigCroi(j:byte);
 BEGIN
  Cvs.PolyGon([
  Point(x,y+j),Point(x+j,y+2*j),Point(x+2*j,y+j),Point(x,y+j),
  Point(x+2*j,y-j),Point(x+j,y-2*j),Point(x,y-j),Point(x-j,y-2*j),
  Point(x-2*j,y-j),Point(x-j,y),Point(x-2*j,y+j),Point(x-j,y+2*j)])
 END;{BigCroi}

 PROCEDURE BigCroi1(j:byte);
 BEGIN
  Cvs.PolyGon([
  Point(x+j,y),Point(x+3*j,y+3*j),Point(x,y+j),Point(x-3*j,y+3*j),
  Point(x-j,y),Point(x-3*j,y-3*j),Point(x,y-j),Point(x+3*j,y-3*j)])
 END;{BigCroi}

 PROCEDURE BigCroi2(j:byte);
 BEGIN
  Cvs.PolyGon([
  Point(x+2*j,y),Point(x+3*j,y+3*j),Point(x,y+2*j),Point(x-3*j,y+3*j),
  Point(x-2*j,y),Point(x-3*j,y-3*j),Point(x,y-2*j),Point(x+3*j,y-3*j)])
 END;{BigCroi}

 PROCEDURE YHauDiam(j:byte);{YV}
 BEGIN
  Cvs.PolyGon([
  Point(x,y+8),Point(x+2,y+1),Point(x+7,y-4),
  Point(x,y-2),Point(x-7,y-4),Point(x-2,y+1)])
 END;{YHauDiam}

 PROCEDURE YBasDiam(j:byte);
 BEGIN
  Cvs.PolyGon([
  Point(x,y+2),Point(x+7,y+4),Point(x+2,y-1),
  Point(x,y-8),Point(x-2,y-1),Point(x-7,y+4)])
 END;{YBasDiam}

 PROCEDURE YHauBig(j:byte);
 BEGIN
  Cvs.PolyGon([
  Point(x,y+1),Point(x+4,y+3),Point(x+5,y+2),
  Point(x+1,y-1),Point(x+1,y-5),Point(x-1,y-5),
  Point(x-1,y-1),Point(x-5,y+2),Point(x-4,y+3)])
 END;{YHauBig}

 PROCEDURE YBasBig(j:byte);
 BEGIN
  Cvs.PolyGon([
  Point(x+1,y+5),Point(x+1,y+1),Point(x+5,y-2),
  Point(x+4,y-3),Point(x,y-1),Point(x-4,y-3),
  Point(x-5,y-2),Point(x-1,y+1),Point(x-1,y+5)])
 END;{YBasBig}

BEGIN
(**
 clAqua, clBlack, clBlue, clDkGray, clFuchsia, clGray, clGreen,
 clLime, clLtGray, clMaroon, clNavy, clOlive, clPurple, clRed,
 clSilver, clTeal, clWhite, et clYellow.
**)
//CodColor='BRGYLVTO';
//Black / Red / Green / Yellow / bLue / Violet / Turquoise / Orange
 IF icol>0 THEN BEGIN
  ICol:=(icol-1) mod Max_Coul + 1;
  CASE ICol OF
   1:CurColor:=clBlack;
   2:Curcolor:=clRed;
   3:Curcolor:=clLime;
   4:Curcolor:=clYellow;
   5:Curcolor:=clBlue;
   6:Curcolor:=clFuchsia;
   7:Curcolor:=clAqua;
   8:Curcolor:=clPurple;
//Gray,DkGray,MedGray,LtGray,
//Cream,Green,Lime,Maroon,Navy,
//Olive,Purple,Silver,SkyBlue
  END;
  WITH Cvs DO BEGIN
   Pen.Color:=CurColor;
   Brush.Color:=CurColor;
   if CodSymbol[3*isym]='F' then  Brush.Style:=bsSolid
   else Brush.Style:=bsClear
  END
 END;

 IF Tail>8 THEN Tail:= 8; IF Tail<3 THEN Tail:= 3; {taille de symbole}
 if isym=0 then SmallCross else
 case CodSymbol[3*isym-1] of
  'C': Circl(Tail);
  'D': Diamond(Tail);
  'S': Squar(Tail);
  'V': Tri_Haut(Tail);
  'W': YHauDiam(Tail);
  'A': Tri_Bas(Tail);
  'B': YBasDiam(Tail);
  'P': BigPlus(Tail-2);
  'Q': BigPlus1(Tail-2);
  'X': BigCroi1(Tail-2);
  'R': BigCroi2(Tail-2);
  'T': YBasBig(Tail);
  'U': YHauBig(Tail)
 END;
(**
_SE_SP_SX      _CF_CE_CP_CX      _DF_DE_DP_DX   _AF_AE_BF_BE   _VF_VE_WF_WE
--1--2--3--4--------5--6--7--8--------9-10-11-12----13-14-15-16----17-18-19-20
_PF_PE_QF_QE__P   _XF_XE_RF_RE__X   _TF_TE_UF_UE
-21-22-23-24-25----26-27-28-29-30----31-32-33-34
**)
 case CodSymbol[3*isym] of
  'P': Plus(2,Tail);
  'X': Cross(Tail)
 end;
END; {Des_Symb_Crt}

PROCEDURE Des_Point_Crt(Cvs: TCanvas; c, i, T: byte; x, y: integer; N: Str_Signif);
BEGIN
 WHILE N[length(N)]='.' DO delete(N,length(N),1);
 Des_Symb_Crt(Cvs, c, i, T, x, y);
 IF not B_Muet THEN WITH Cvs DO BEGIN
  Brush.Style:=bsClear;
  TextOut(x+3,y-3,N)
 END
END;{Des_Point_Crt}

procedure Test_Grf(Cvs: TCanvas;VAR Dim:Word);
var i: byte;
begin
 Init_Grf(30,30,30,30);
 P_Hih[Cor_X]:=1000; P_Hih[Cor_Y]:=1000;
 P_Low[Cor_X]:=0;    P_Low[Cor_Y]:=0;
 Log[Cor_Y]:= False; Log_Max[Cor_Y]:= 1000;
 Cad_Crt(Cvs,True,'tikyu','kagaku','',10,10);
 for i:= 1 to Max_Symb do begin
  arP[i][Cor_X]:=80*((i-1)MOD 10 +1);
  arP[i][Cor_Y]:=1000 - 100*((i-1)DIV 10 + 1);
{
  arN[i]:=chr(i DIV 10 + Ord('0'))+chr(i MOD 10 + Ord('0'));
  IF i<27 THEN arN[i]:=chr(i + Ord('A') - 1)
  ELSE arN[i]:=chr((i-26) + Ord('0'));
}
  arN[i]:=copy(CodSymbol,3*i-1,2);
  arS[i]:= (i-1) mod Max_Symb + 1;
  arC[i]:= 3;
 end;
 for i:= 1 to Max_Coul do begin
  arP[Max_Symb+i][Cor_X]:=80*((i-1)MOD 10 +1);
  arP[Max_Symb+i][Cor_Y]:=500;
  arN[Max_Symb+i]:=chr(i DIV 10 + Ord('0'))+chr(i MOD 10 + Ord('0'));
  arN[Max_Symb+i]:=copy(CodColor,i,1);
  arS[Max_Symb+i]:=Sym_SF;
  arC[Max_Symb+i]:=(i-1) mod Max_Coul + 1;;
 end;
 Dim:=Max_Symb + Max_Coul;
 Des_Crt(Cvs,False,False,False,True,Dim,10);
end;{Test_Grf}

PROCEDURE Cad_Spd(Cvs: TCanvas; Prm: ArrEle_Byte; Frm: Forme);
VAR
 x, y: Word;
 i:    Byte;
BEGIN
 Log[Cor_Y]:= True;
 P_Hih[Cor_X]:=Frm.Nb_Ele-1;
 P_Max[Cor_X]:=Frm.Nb_Ele-1;

 Cvs.Pen.Width:=2;
 Cvs.Pen.Color:=clBlack;
 Line(Cvs,DesMinX,DesMaxY,DesMaxX,DesMaxY);
 Line(Cvs,DesMinX,DesMinY,DesMaxX,DesMinY);
 WITH Frm DO FOR i:= 1 TO Nb_Ele DO BEGIN
  x:= Round((i-1)*(DesMaxX-DesMinX)/(Nb_Ele-1)) + DesMinX;
  y:= DesMaxY;
  Line(Cvs,x,y,x,Y+4);
  Line(Cvs,x,DesMinY,x,DesMinY-4);
  IF Prm[i]<>0 THEN Cvs.TextOut(x-2,y+4,Tit_Oxy[i]);
  IF Prm[i]<>0 THEN Cvs.TextOut(x-2,y+16,Real_Str(VecSpd[i]));
 END;
 Axe_Y(Cvs,True,1,1000,10,'Sample/'+SufSpd)
END;{Cad_Spd}

PROCEDURE Label_Profil(Cvs:TCanvas; Frm: Forme; Yes_Pro: ArrEle_Bool);
VAR x0, y, y0: Word; i, j: byte; d:integer;
BEGIN
 Cvs.Font.Name:='Times New Roman';
 x0:=12; y0:=0; d:=Cvs.TextHeight('A');
 j:=0;
 FOR i:=1 TO Frm.Nb_Ele DO IF Yes_Pro[i] THEN BEGIN
  inc(j);
  y:=y0 + j*(d+4) - d div 2;
  Cvs.TextOut(x0+8,y,Frm.Tit_Oxy[i])
 END;
 j:=0;
 FOR i:=1 TO Frm.Nb_Ele DO IF Yes_Pro[i] THEN BEGIN
  inc(j);
  y:=y0+j*(d+4);
  Des_Symb_Crt(Cvs,(i-1) mod Max_Coul + 1,(i-1) mod Max_Symb + 1,4,x0,y);
 END;
END;{Plot_LabPro}

PROCEDURE Des_SymCol(Cvs:TCanvas;BA,BT,BS,BC:boolean);
VAR x, y, y0: Word; i, rcol, qsym: byte; t: string;
BEGIN
 Cvs.Font.Name:= 'Times New Roman';
 x:=12;y0:=0;
 IF BA THEN
 FOR i:= 1 TO length(LisAutoSymCol) DIV Signif DO BEGIN
  t:=copy(LisAutoSymCol,Signif*(i-1)+1,Signif);
  WHILE t[length(t)]='.' DO Delete(t,length(t),1);
  IF t='' THEN t:='_';

  rcol:=pos(UpCase(T[1]),CodColor);
  if length(T)>2 then begin
   qsym:=position(3,'_'+copy(T,2,2),CodSymbol);
   y:=y0+i*(Cvs.TextHeight('A')+3);
   Des_Symb_Crt(Cvs,rcol,qsym,4,x,y);
   Cvs.Brush.Style:=bsClear;
   t:=copy(LisAutoLegend,Signif*(i-1)+1,Signif);
   Cvs.TextOut(x+8,y-Cvs.TextHeight('A')div 2,t)
  end
  else qsym:=0;

 END;//BA

 IF BT THEN
 FOR i:= 1 TO length(LisAutomatic) DIV Signif DO BEGIN
  t:=copy(LisAutomatic,Signif*(i-1)+1,Signif);
  WHILE t[length(t)]='.' DO Delete(t,length(t),1);
  IF t='' THEN t:='_';
  if i>0 then rcol:=(i mod Max_Coul) + 1 else rcol:=0;
  if i>0 then qsym:=4*(i div Max_Coul) + 1 else qsym:=0;//=1,5,9,...
  y:=y0+i*(Cvs.TextHeight('A')+3);
  Des_Symb_Crt(Cvs,rcol,qsym,4,x,y);
  Cvs.Brush.Style:=bsClear;
  Cvs.TextOut(x+8,y-Cvs.TextHeight('A')div 2,t)
 END;//BT, Automatic

 IF BS THEN
 FOR i:= 1 TO Length(Lis_Symb) DIV Signif DO BEGIN
  y:=y0+i*(Cvs.TextHeight('A')+3);
  t:= '_'+copy(Lis_Symb,(i-1)*signif+1,signif);
  qsym:= position(Length_Codsym,t,CodSymbol);
  Des_Symb_Crt(Cvs,1,qsym,4,x,y);
  Cvs.Brush.Style:=bsClear;
  t:= copy(Lis_SymbLeg,(i-1)*signif+1,signif);
  Cvs.TextOut(x+8,y-Cvs.TextHeight('A')div 2,t)
 END;

 IF BS THEN y0:=y0 + (Cvs.TextHeight('A')+3) * ((length(Lis_Symb) DIV signif)+1);

 IF BC THEN
 FOR i:= 1 TO Length(Lis_Coul) DIV Signif DO BEGIN
  y:=y0+i*(Cvs.TextHeight('A')+4);
  t:= copy(Lis_Coul,(i-1)*signif+1,signif);
  rcol:= position(1,copy(t,1,1),CodColor);
  Des_Symb_Crt(Cvs,rcol,Sym_SF,4,x,y);
  Cvs.Brush.Style:=bsClear;
  t:= copy(Lis_CoulLeg,(i-1)*signif+1,signif);
  Cvs.TextOut(x+8,y-Cvs.TextHeight('A')div 2,t)
 END;
END;{Des_SymCol}

PROCEDURE Label_Spd(Cvs:TCanvas; i:byte; s:Str_Signif);
VAR x, y: Word;
BEGIN
 Cvs.Font.Name:='Times New Roman';
 x:=12; y:=8+i*(Cvs.TextHeight('A')+4);
 Des_Symb_Crt(Cvs,(i-1)mod Max_Coul + 1,ArrPrm_SymSpd[i],4,x,y);
 Cvs.Brush.Style:=bsClear;
 Cvs.TextOut(x+8,y-Cvs.TextHeight('A')div 2,s);
END;{Label_Spd}

PROCEDURE Label_Histo(Cvs:TCanvas; i:byte; s:Str_Signif);
VAR x, y: Word;
BEGIN
 Cvs.Font.Name:='Times New Roman';
 x:=12; y:=8+i*(Cvs.TextHeight('A')+4);
 Des_Symb_Crt(Cvs,(i-1)mod Max_Coul + 1,Sym_SF,4,x,y);
 Cvs.Brush.Style:=bsClear;
 Cvs.TextOut(x+8,y-Cvs.TextHeight('A')div 2,s);
END;{Label_Histo}

PROCEDURE Des_Crt(Cvs:TCanvas;B_Prof,B_Mono,B_Trg,B_Clos:boolean;Dim:Word;tail:byte);
VAR i: word; x, y, x0, y0: integer; ok: boolean;Cur_Col,Cur_Sym:byte;
BEGIN
 Cur_Col:= 0; Cur_Sym:= 0;
 x0:= -1; y0:= -1;
 Cvs.Font.Name := 'Times New Roman';
 Cvs.Font.Size := 8;
 Cvs.Brush.Style := bsSolid;
 Cvs.Brush.Color := clWhite;
 Cvs.Pen.Color   := clBlack;

 FOR i:= 1 TO Dim DO {IF (arS[i]>0) THEN} BEGIN
  IF B_Trg THEN TrgCalc_Crt(arP[i][Cor_X],arP[i][Cor_Y],x, y, ok)
  ELSE Calc_Crt(arP[i], x, y, ok);
  IF B_Clos THEN OK:= OK
  AND (X>=DesMinX) AND (X<=DesMaxX)
  AND (Y>=DesMinY) AND (Y<=DesMaxY);
  IF ok  THEN BEGIN
   IF B_Prof and (arC[i]=Cur_Col) and (arS[i]=Cur_Sym)
   and (x0>=0) and (y0>=0)
   and (x>=x0)//prevent to go back to begin next spider ...
   THEN line(Cvs,x0,y0,x,y);
   x0:= x; y0:= y;
   Des_Point_Crt(Cvs, arC[i], arS[i], tail, x, y, arN[i])
  END
  ELSE BEGIN
   x0:= -1; y0:= -1;
  END;
  Cur_Sym:= arS[i]; Cur_Col:= arC[i]
 END
END; {des_Crt}

PROCEDURE Des_Lin_Crt(Cvs:TCanvas;c:byte;b0,b1:single;Dim:Word);
VAR {i:byte;} x1, y1, x0, y0: integer; ok: boolean; P0, P1: P_Real;
BEGIN
 Cvs.Font.Name := 'Times New Roman';
 Cvs.Font.Size := 8;
 Cvs.Brush.Style := bsSolid;
 Cvs.Brush.Color := clWhite;
 case c of
  0: Cvs.Pen.Color   := clBlack;
  1: Cvs.Pen.Color   := clGreen;
  2: Cvs.Pen.Color   := clRed
 end;

 P0[Cor_X]:=P_Low[Cor_X];P1[Cor_X]:=P_Hih[Cor_X];
 P0[Cor_Y]:=b0 + b1 * P0[Cor_X];
 P1[Cor_Y]:=b0 + b1 * P1[Cor_X];
{
 FOR i:= 1 TO Dim-1 DO BEGIN
  P0[Cor_X]:=P_Min[Cor_X]+(P_Max[Cor_X]- P_Min[Cor_X])*i/Dim;
  P0[Cor_Y]:=b0 + b1 * P0[Cor_X];
  P1[Cor_X]:=P_Min[Cor_X]+(P_Max[Cor_X]- P_Min[Cor_X])*(i+1)/Dim;
  P1[Cor_Y]:=b0 + b1 * P1[Cor_X];
}
  Calc_Crt(P0, x0, y0, ok);
  Calc_Crt(P1, x1, y1, ok);
{
  OK:= OK
  AND (X0>=DesMinX) AND (X0<=DesMaxX)
  AND (Y0>=DesMinY) AND (Y0<=DesMaxY);
}
  IF ok  THEN line(Cvs,x0,y0,x1,y1);
 {END}
END; {des_Crt}

PROCEDURE Def_Bornes(NegaOK,Trg:boolean;c: coor_xy);
VAR p: real;
BEGIN
 IF Trg THEN BEGIN
  X0Trg:=0; X1Trg:=100; Y0Trg:=0; Y1Trg:=100;
  P_Hih[c]:=100;P_Low[c]:=0.0
 END
 ELSE IF NOT NegaOK THEN BEGIN
  IF abs(P_Min[c])<0.1 THEN P_Low[c]:=0
   ELSE IF abs(P_Min[c])<5 THEN P_Low[c]:= trunc(10* P_Min[c])/10-0.1
    ELSE IF abs(P_Min[c])<32E3 THEN P_Low[c]:= trunc(P_Min[c])
     ELSE P_Low[c]:= -1E4;
  IF abs(P_Max[c])<5 THEN P_Hih[c]:= trunc(10*P_Max[c])/10+0.1
   ELSE IF abs(P_Max[c])<80 THEN P_Hih[c]:=trunc(P_Max[c])+1
    ELSE IF abs(P_Max[c])<1000 THEN P_Hih[c]:=10*(trunc(P_Max[c]/10)+1)
     ELSE IF abs(P_Max[c])<32E3 THEN P_Hih[c]:=100*(trunc(P_Max[c]/100)+1)
      ELSE P_Hih[c]:= 1E4;
  IF P_Hih[c]<=P_Low[c] THEN P_Hih[c]:= P_Low[c] + 0.1;

  p:= 10.0E6; REPEAT p:= p/10 UNTIL P_Max[c]>=p;
  Log_Max[c]:= 10*p;
  IF Log_Max[c]>1E5*P_Min[c] THEN Log_Mod[c]:= 5
  ELSE IF Log_Max[c]>1E4*P_Min[c] THEN Log_Mod[c]:= 4
  ELSE IF Log_Max[c]>1E3*P_Min[c] THEN Log_Mod[c]:= 3
  ELSE IF Log_Max[c]>100*P_Min[c] THEN Log_Mod[c]:= 2
  ELSE Log_Mod[c]:= 1
 END
END;{Def_Bornes}

PROCEDURE Des_SymCol_P38(Cvs:TCanvas;L_Sam:String);
VAR x, y, y0: Word; i, rcol, qsym: byte; t: string;
BEGIN
 Cvs.Font.Name:= 'Times New Roman';
 x:=DesMaxX+12;
 y0:=DesMinY+8;
 FOR i:= 1 TO Length(L_Sam) DIV Signif DO BEGIN
  y:=y0+i*(Cvs.TextHeight('A')+3);
  t:= copy(L_Sam,(i-1)*signif+1,signif);
  qsym:= position(Signif,t,L_Sam);
  rcol:= position(Signif,t,L_Sam);
  Des_Symb_Crt(Cvs,rcol,qsym,4,x,y);
  Cvs.Brush.Style:=bsClear;
  Cvs.TextOut(x+8,y-Cvs.TextHeight('A')div 2,t)
 END
END;{Des_SymCol_P38}

BEGIN Init_LibGrf END.

 PROCEDURE CroiDiam(j:byte);
 VAR A: Array[1..8] OF TPoint;
 BEGIN
  Cvs.Polygon(
  [Point(x+2*j,y+2*j),Point(x+j,y),Point(x+2*j,y-2*j),Point(x,y-j),
  Point(x-2*j,y-2*j),Point(x-j,y),Point(x-2*j,y+2*j),Point(x,y+j))
 END;{CroiDiam}

 PROCEDURE Y_Hau(jT: byte);{YU}
 BEGIN
  line(Cvs,x-jT-1,y+jT+1,x-1,y+1);
  line(Cvs,x+jT+1,y+jT+1,x+1,y+1);
  line(Cvs,x,y,x,y-jT-1)
 END;

 PROCEDURE Y_Bas(jT: byte);{YD}
 BEGIN
  line(Cvs,x-jT-1,y-jT-1,x-1,y-1);
  line(Cvs,x+jT+1,y-jT-1,x+1,y-1);
  line(Cvs,x,y,x,y+jT+1)
 END;

 PROCEDURE Hexagon(j:byte);{PlusDiam}
 VAR A: Array[1..8] OF TPoint;
 BEGIN
  Cvs.PolyGon(
  [Point(x,y+3*j),Point(x+j,y+j),Point(x+3*j,y),Point(x+j,y-j),
  Point(x,y-3*j),Point(x-j,y-j),Point(x-3*j,y),Point(x-j,y+j)])
 END;{PlusDiam}

PROCEDURE Des_Spider(Cvs:TCanvas; Nb_Spd,Dim:Byte);
begin
 if Dim>2 then begin
  Init_Grf(30,30,30,30);
  Cad_Spd(Cvs,PrmSpd,FrmSpd);
  Des_Crt(Cvs,True,False,False,False,Dim,4);
 end
end;{Des_Spider}


