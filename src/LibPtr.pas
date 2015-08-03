unit LibPtr;

interface

USES LibIo, LibGrf;

TYPE
 Don_Ptr = ^ArrEle_Sing;
 Rec_Ptr = ^RecDbLink;
 RecDbLink = RECORD
  Rec: Rec_Ana;
  DonY: Don_Ptr;
  Prev,Next: Rec_Ptr
 END;

TYPE
 CoorXYZ     = (CorX, CorY, CorZ);
VAR
 ArAtm:             array[CoorXYZ] of Rec_Atom;
 ArExp:               array[CoorXYZ] of expression;

procedure Rha_Vec(var F:TextFile; Frm: Forme; DimRha: byte; R:Rec_Ana);
{OUTILS POUR MANIPULATION DE RECORD … CHAINAGE DOUBLE}
PROCEDURE Text_To_Ptr(LirLis,NegaOK:Boolean;
 VAR F: TextFile; VAR frm: Forme;
 VAR YesEle:ArrEle_Bool;
 VAR FirstRec, LastRec: Rec_Ptr);
PROCEDURE TextY_To_PtrXY(X_Y_,S_XY_: BOOLEAN;
 VAR F: TextFile; VAR frmX,frmY: Forme;
 VAR YesEle:ArrEle_Bool;
 VAR FirstRec, LastRec: Rec_Ptr);
PROCEDURE List_DbLinkList(Frm:Forme; Ch:Char; FirstRec,LastRec:Rec_Ptr);
PROCEDURE Free_DbLinkList(VAR FirstRec,LastRec:Rec_Ptr);
PROCEDURE Sort_DbLinkList(I_Champ,I_Don: byte; VAR FirstRec,LastRec: Rec_Ptr);

PROCEDURE Ptr_To_XY(FirstRec:Rec_Ptr;X_Y_,S_XY_,Auto,Tic,Trg:boolean;MaxChamp:Byte;VAR Dim:Word);
PROCEDURE Ptr_To_Pro(FirstRec:Rec_Ptr;N:Byte;X_Y_,S_XY_,Norm_Max:Boolean;Yes_Pro:ArrEle_Bool;VAR Dim:Word);
PROCEDURE Ptr_To_Spider(VAR CurRec:Rec_Ptr;Auto,Tic,X_Y_,S_XY_,Ree:Boolean;Frm:Forme;Yes:ArrEle_Bool;VAR Nb_Spd:Byte;VAR Dim:WORD);
PROCEDURE Ptr_To_Histo(FirstRec:Rec_Ptr;X_Y_,S_XY_,B_Log:BOOLEAN;VAR Nb_Histo:Byte;VAR Dim:WORD);

implementation

uses Math, LibMat;

procedure DQSortRha(X : PVector;  var a: Arrele_Str; Lbound, Ubound : Integer);
//Quick sort in descending order - Adapted from Borland's BP7 demo
procedure Sort(L, R : Integer);
var
  I, J : Integer; U, V : Float; S: str2;
begin
  I := L; J := R; U := X^[(L + R) div 2];
  repeat
    while X^[I] > U do I := I + 1;
    while U > X^[J] do J := J - 1;
    if I <= J then begin
      V:=X^[I]; X^[I]:=X^[J]; X^[J]:=V;
      S:=a[I+1]; a[I+1]:=a[J+1]; a[J+1]:= S;
      I := I + 1; J := J - 1;
    end;
  until I > J;
  if L < J then Sort(L, J);
  if I < R then Sort(I, R);
end;
begin
  Sort(Lbound, Ubound);
end;//DQSortRha

procedure Rha_Vec(var F:TextFile; Frm: Forme; DimRha: byte; R:Rec_Ana);
var X:PVector; i:byte; s:string; c: char; xH, xA, yA: double; m: byte;
begin
  DimVector(X,Frm.Nb_Ele-1);
  for i:=1 to Frm.Nb_Ele do X^[i-1]:=R.Don[i];
  DQSortRha(X,Frm.Tit_Oxy,0,Frm.Nb_Ele-1);
  //for i:=1 to Frm.Nb_Ele do write(F,Frm.Tit_Oxy[i],#9);writeln(F);
  //for i:=1 to Frm.Nb_Ele do write(F,R_Str(X^[i-1]),#9);writeln(F);
  s:='';
  for i:=1 to Frm.Nb_Ele do begin
    c:='>';
    if (i<Frm.Nb_Ele) and (X^[i]>0) then
    if (X^[i-1]/X^[i]<1.15) then c:='=';
    s:=s+copy(Frm.Tit_Oxy[i],1,2)+c
  end;
  m:=0;
  for i:=1 to Frm.Nb_Ele do if (X^[i-1]>0) then m:=m+1;
  if m>2 then begin
   if m>DimRha then m:=DimRha;
   xH:=0.0;//xH used for summation
   for i:=1 to m do xH:= xH + X^[i-1];
   for i:=1 to m do X^[i-1]:=X^[i-1]/xH;
   //(ln0.99955 + 9*ln0.00005)/10 – ln10
   xH:=0.0; xA:=0.0; yA:=0.0;
   yA:= -(ln(1 - (m-1)*0.00002) + (m-1)*ln(0.00001))/m - ln(m);
   for i:=1 to m do xH:= xH - X^[i-1]*ln(X^[i-1]);//Entropy
   for i:=1 to m do xA:= xA - ln(X^[i-1]);//AnEntropy –(1/n)Sum(ln(pi))-ln(n)
   xH:= xH/ln(m);
   xA:=(xA/m - ln(m))/yA;
   write(F,s,#9,R_Str(xH),#9,R_Str(xA),#9);
   Champ_TO_Text(F,Frm,R.Champ);writeln(F);
  end;
  DelVector(X,Frm.Nb_Ele-1)
end;//Rha_Vec

PROCEDURE Sort_DbLinkList(I_Champ, I_Don: byte; VAR FirstRec, LastRec: Rec_Ptr);
VAR
 PrevRec, CurRec: Rec_Ptr;
 NextRec, FarRec: Rec_Ptr;
 SortDone:        boolean;

 FUNCTION Sort_Condition: boolean;
 BEGIN
  IF I_Champ>0 THEN
   Sort_Condition:=(CurRec^.Rec.Champ[I_Champ])>(NextRec^.Rec.Champ[I_Champ])
{
 VAR S1,S2: Str_Signif;
  BEGIN
   S1:=CurRec^.Rec.Champ[I_Champ]; S2:=NextRec^.Rec.Champ[I_Champ];
   IF S1[2] IN ['0'..'9'] THEN REPEAT Insert('0',S1,2) UNTIL (Length(S1)>=5)OR(S1[6] IN ['A'..'Z']);
   IF S2[2] IN ['0'..'9'] THEN REPEAT Insert('0',S2,2) UNTIL (Length(S2)>=5)OR(S2[6] IN ['A'..'Z']);
   Sort_Condition:= S1>S2
  END
}
  ELSE Sort_Condition:= CurRec^.Rec.Don[I_Don]>NextRec^.Rec.Don[I_Don]
 END;
BEGIN{Sort_DbLinkList}
 IF (I_Champ>0)OR(I_Don>0) THEN BEGIN
  REPEAT
   CurRec:= FirstRec;
   PrevRec:= Nil;
   SortDone:= true;
   WHILE CurRec^.Next<>Nil DO BEGIN
    {Sho_ArrChamp(1, frm.Nb_Champ, CurRec^.champ);}
    NextRec:= CurRec^.Next;
(***
  IF S_XY_ THEN BEGIN
   OK:=(CurRec^.DonY<>NIL);
   IF OK THEN Rx.Don:=CurRec^.DonY^
  END;
***)
    IF Sort_Condition THEN BEGIN
     SortDone:= false;
     IF NextRec^.Next<>Nil THEN BEGIN
      FarRec:= NextRec^.Next;
      FarRec^.Prev:= CurRec
     END
     ELSE FarRec:= Nil;
     IF CurRec^.Prev=Nil THEN BEGIN
      FirstRec:= NextRec;
      PrevRec:= Nil
     END
     ELSE BEGIN
      PrevRec:= CurRec^.prev;
      PrevRec^.Next:= NextRec
     END;
     CurRec^.Next:= FarRec;
     CurRec^.Prev:= NextRec;
     NextRec^.Next:= CurRec;
     NextRec^.Prev:= PrevRec;
     CurRec:= FirstRec
    END
    ELSE CurRec:= CurRec^.Next
   END
  UNTIL SortDone;
  LastRec:= CurRec
 END
END;{Sort_DbLinkList}

PROCEDURE TextY_To_PtrXY(X_Y_,S_XY_: BOOLEAN;
 VAR F: TextFile; VAR frmX,frmY: Forme;
 VAR YesEle:ArrEle_Bool;
 VAR FirstRec, LastRec: Rec_Ptr);
VAR
 a:               Rec_Ana;
 END_Fi:          boolean;
{variables for F_Mrg, F_Cmp}
 F_Mrg,F_Cmp:     Textfile;
 RX,RY:           Rec_Atom;
 N,P,j:           BYTE;
 PrmX,PrmY:       ArrEle_Byte;
 z:               Real;
 W:               word;

PROCEDURE EnTete_Mrg;
VAR j:byte;
BEGIN
 AssignFile(F_Mrg, Dir_Dat+'MRG.TXT');Rewrite(F_Mrg);
 TitChamp_To_Text(F_Mrg,FrmX);
 FOR j:=1 TO N DO
  Write(F_Mrg,FrmX.Tit_Oxy[PrmX[j]],SepCar,FrmY.Tit_Oxy[PrmY[j]],SepCar);
 WriteLn(F_Mrg)
END;{EnTete_Mrg}

PROCEDURE EnTete_Cmp;
VAR j:byte;
BEGIN
 AssignFile(F_Cmp, Dir_Dat+'CMP.TXT');Rewrite(F_Cmp);
 Write(F_Cmp,'.XFIL',SepCar,'.SAMP',SepCar);
 FOR j:=1 TO N DO Write(F_Cmp,FrmX.Tit_Oxy[PrmX[j]],SepCar);
 WriteLn(F_Cmp);
 Write(F_Cmp,'.YFIL',SepCar,'.SAMP',SepCar);
 FOR j:=1 TO N DO Write(F_Cmp,FrmY.Tit_Oxy[PrmY[j]],SepCar);
 WriteLn(F_Cmp);
END;{EnTete_Cmp}

PROCEDURE Look_Rec;
VAR {PrevRec,} CurRec: Rec_Ptr; CurDon: Don_Ptr; i,j: byte;
BEGIN
 CurRec:= FirstRec;

 WHILE CurRec<>Nil DO BEGIN
  IF CurRec^.Rec.Champ[1]=a.Champ[1] THEN BEGIN
   FOR I:= 1 TO FrmY.Nb_Ele DO YesEle[i]:=YesEle[i] OR (a.Don[i]>0);
   New(CurDon);
   CurDon^:=a.Don;
   CurRec^.DonY:=CurDon;
   IF X_Y_ AND (N>0) THEN BEGIN
    Champ_To_Text(F_Mrg,FrmX,CurRec^.Rec.Champ);
    FOR j:=1 TO N DO BEGIN
     Write(F_Mrg,Real_Str(CurRec^.Rec.Don[PrmX[j]]),SepCar);
     Write(F_Mrg,Real_Str(CurRec^.DonY[PrmY[j]]),SepCar)
    END;
    WriteLn(F_Mrg);

    Write(F_Cmp,'X',SepCar,a.Champ[1],SepCar);
    FOR j:=1 TO N DO Write(F_Cmp,Real_Str(CurRec^.Rec.Don[PrmX[j]]),SepCar);
    WriteLn(F_Cmp);

    Write(F_Cmp,'Y',SepCar,a.Champ[1],SepCar);
    FOR j:=1 TO N DO Write(F_Cmp,Real_Str(CurRec^.DonY[PrmY[j]]),SepCar);
    WriteLn(F_Cmp);

    Write(F_Cmp,'Z',SepCar,a.Champ[1],SepCar);
    FOR j:=1 TO N DO BEGIN
     IF CurRec^.DonY[PrmY[j]]>Epsilon THEN
     Z:=100*CurRec^.Rec.Don[PrmX[j]] / CurRec^.DonY[PrmY[j]]
     ELSE z:=0.0;
     Write(F_Cmp,Real_Str(z),SepCar)
    END;
    WriteLn(F_Cmp);
   END;{IF X_Y_}
  END;{IF CurRec^.Rec.Champ[1]=a.Champ[1]}

  CurRec:= CurRec^.Next
 END
END;{Look_Rec}
BEGIN
 W:=0;
 Calc_RecAtom(FrmX,RX);  Calc_RecAtom(FrmY,RY);
 N:=0;{number of elements that are present in both files}
 IF X_Y_ THEN BEGIN
  FOR j:=1 TO RY.Nb_Ele DO BEGIN
   P:=POSITION(2,RY.Tit_Ele[j],RX.Lis_Ele);
   IF P>0 THEN BEGIN
    INC(N); PrmX[N]:=P; PrmY[N]:=j
   END
  END;
  IF (N>0) THEN BEGIN
   EnTete_Mrg; EnTete_Cmp
  END
 END;

 YesEle:=Arr_Faux;
 Text_TO_RecAna(False,F,FrmY,a,W,END_Fi);
 WHILE NOT END_Fi DO BEGIN
  Look_Rec; Text_TO_RecAna(False,F,FrmY,a,W,END_Fi)
 END;

 IF N>0 THEN CloseFile(F_Mrg);
 IF N>0 THEN CloseFile(F_Cmp);
END;{TextY_To_PtrXY}

PROCEDURE Text_To_Ptr(LirLis,NegaOK:Boolean;
 VAR F: TextFile; VAR frm: Forme;
 VAR YesEle:ArrEle_Bool;
 VAR FirstRec, LastRec: Rec_Ptr);
VAR
 PrevRec, CurRec: Rec_Ptr;
 a:               Rec_Ana;
 I:               byte;
 END_Fi:          boolean;
 W:               word;
 PROCEDURE EnterData;
 VAR I: Byte;
 BEGIN
  FOR i:= 1 TO Frm.Nb_Champ DO Concat_Lis(Signif,a.Champ[i],ArLisChamp[i]);
  IF (NOT LirLis) THEN 
    FOR I:= 1 TO Frm.Nb_Ele DO YesEle[i]:= NegaOK OR YesEle[i] OR (a.Don[i]>0);
  CurRec^.Rec:=a;
  CurRec^.DonY:=NIL
 END;
BEGIN
 IF NegaOK THEN YesEle:=Arr_Vrai ELSE YesEle:=Arr_Faux;
 W:=0;
 FOR i:= 1 TO Frm.Nb_Champ DO ArLisChamp[i]:='';

 FirstRec:= Nil;
 Text_TO_RecAna(LirLis,F,Frm,a,W,END_Fi);
{
 SufSpd:=a.Champ[1];
 FOR i:=1 TO FrmSpd.Nb_Ele DO Vec_Spd[i]:=1;
 FOR i:=1 TO FrmSpd.Nb_Ele DO
  IF a.Don[i]>0 THEN Vec_Spd[i]:=a.Don[i];
}
 WHILE NOT END_Fi DO BEGIN
  IF FirstRec=Nil THEN BEGIN
   New(CurRec);
   EnterData;
   CurRec^.Next:= Nil;
   CurRec^.Prev:= Nil;
   FirstRec:= CurRec;
   LastRec:= CurRec;
  END
  ELSE BEGIN
   PrevRec:=LastRec;
   New(CurRec);
   EnterData;
   PrevRec^.Next:= CurRec;
   CurRec^.Next:= Nil;
   CurRec^.Prev:= PrevRec;
   LastRec:= CurRec
  END;
  Text_TO_RecAna(LirLis,F,Frm,a,W,END_Fi)
 END
END;{Text_To_Ptr}

PROCEDURE List_DbLinkList(Frm:Forme; Ch:Char; FirstRec,LastRec:Rec_Ptr);
VAR
 {PrevRec,} CurRec: Rec_Ptr;
 PROCEDURE ListForwards;
 BEGIN
  CurRec:= FirstRec;
  WHILE CurRec<>Nil DO BEGIN
//Sho_ArrChamp(2,frm.Nb_Champ,CurRec^.Rec.Champ);
//Sho_Vec(1,Frm,CurRec^.Rec.Don)
   CurRec:= CurRec^.Next
  END
 END;{ListForwards}
 PROCEDURE ListBackwards;
 BEGIN
  CurRec:= LastRec;
  WHILE CurRec<>Nil DO BEGIN
//Sho_ArrChamp(2,frm.Nb_Champ,CurRec^.Rec.Champ);
//Sho_Vec(1,Frm,CurRec^.Rec.Don);
   CurRec:= CurRec^.Prev
  END
 END;//ListBackwards
BEGIN
 IF ch='F' THEN ListForwards
 ELSE IF ch='B' THEN ListBackwards;
END;//List_DbLinkList

PROCEDURE Free_DbLinkList(VAR FirstRec,LastRec:Rec_Ptr);
VAR PrevRec, CurRec: Rec_Ptr;
BEGIN
 CurRec:= LastRec;
 WHILE CurRec<>Nil DO BEGIN
  PrevRec:=CurRec^.Prev;
  Dispose(CurRec);
  CurRec:=PrevRec
 END
END;//Free_DbLinkList

PROCEDURE Ptr_To_Spider(VAR CurRec:Rec_Ptr;Auto,Tic,X_Y_,S_XY_,REE:Boolean;Frm:Forme;Yes:ArrEle_Bool;VAR Nb_Spd:Byte;VAR Dim:WORD);
//Nb_Spd: numb. of spidergrams; Dim: total numb. of points
VAR
 i: byte;
 W: ArrEle_Sing;
 Y: single;
 rx, ry: Rec_Ana;
 OK, OKK: Boolean;
//
 s: string; rcol, qsym: byte;
BEGIN
 Nb_Spd:=0; P_Max[Cor_Y]:=0; P_Min[Cor_Y]:=0;
 Dim:=0; OKK:=TRUE;
 WHILE (CurRec<>Nil) AND OKK DO BEGIN
  Rx:=CurRec^.Rec;

  OK:=TRUE;
  Ry:=Rx;
  IF X_Y_ OR S_XY_ THEN BEGIN
   OK:=(CurRec^.DonY<>NIL);
   IF OK THEN BEGIN
    Ry.Don:=CurRec^.DonY^;
    IF S_XY_ THEN Rx.Don:=Ry.Don
   END
  END;

  IF OK AND ((I_SelX=0) OR (position(Signif,rx.champ[I_SelX],Lis_SelX)>0))
  THEN BEGIN
   w:=Rx.Don;
   Cnv_Vec(Met,ArAtm[CorX],ArAtm[CorX].Nb_Ele,w);
   Prm_Vec(PrmSpd,w);
   OK:=FALSE;
   FOR i:=1 TO FrmSpd.Nb_Ele DO IF w[i]>0 THEN OK:=TRUE;
//
   IF OK AND Auto THEN BEGIN
    s:=rx.champ[I_SymCol];//.SYMCOL
    rcol:=pos(UpCase(S[1]),CodColor);
    if length(S)>2
    then qsym:=position(length_codsym,'_'+copy(s,2,2),CodSymbol)
    ELSE qsym:=Sym_SF;
    OK:=(rcol>0)AND(qsym>0)
   END;
//
   IF OK AND Tic THEN BEGIN
    s:=rx.champ[I_Tic];
    i:=position(signif,s,LisAutomatic);
    if i<40 then begin
     rcol:=(i mod Max_Coul) + 1;
     qsym:=4*(i div Max_Coul) + 1 //=1,5,9,...
    end
    else begin rcol:=0; qsym:=0 end; //i>40
    OK:=(rcol>0)AND(qsym>0)
   END;
//
   IF OK THEN BEGIN
    IF Ree THEN Calc_GdSm(PrmSpd,FrmSpd.Nb_Ele,VecSpd,W);
    inc(Nb_Spd);
    FOR i:= 1 TO FrmSpd.Nb_Ele DO
    IF (w[i]>0) AND (Yes[i]) THEN BEGIN
     inc(Dim);
     nettoie(Rx.Champ[I_Label]);
     ArLabSpd[Nb_Spd]:=Rx.Champ[I_Label];
     arN[Dim]:='';
     if Tic or Auto then arC[Dim]:=rcol else arC[Dim]:=(Nb_Spd-1) MOD Max_Coul + 1;
     if Tic or Auto then arS[Dim]:=qsym else arS[Dim]:=ArrPrm_SymSpd[Nb_Spd];
     Y:=w[i]/VecSpd[i];
     P_Max[Cor_Y]:=Max(P_Max[Cor_Y],Y);
     P_Min[Cor_Y]:=Min(P_Min[Cor_Y],Y);
//     if Y>P_Max[Cor_Y] then P_Max[Cor_Y]:= Y;
//     if Y<P_Min[Cor_Y] then P_Min[Cor_Y]:= Y;
     arP[Dim][Cor_X]:=i-1; arP[Dim][Cor_Y]:=Y
    END
   END; //IF OK
   IF Tic or Auto THEN OKK:=(Dim<Max_Point)
   ELSE OKK:=(Nb_Spd<Max_Spd)
  END; //IF OK AND ((I_SelX=0) or selection ok)
  CurRec:= CurRec^.Next
 END //WHILE (CurRec<>Nil) AND OKK DO
END;//Ptr_To_Spider

PROCEDURE Zero_ArPoint;
VAR I:Word;
BEGIN
 FOR i:= 1 to Max_Point DO BEGIN
  arP[i][Cor_X]:= 0; {X}
  arP[i][Cor_Y]:= 0; {Y}
  arN[i]:= '';       {label}
  arC[i]:= 0;        {color}
  arS[i]:= 0         {symbole}
 END;
END;

PROCEDURE Ptr_To_XY(FirstRec:Rec_Ptr;X_Y_,S_XY_,Auto,Tic,Trg:boolean;MaxChamp:Byte;VAR Dim:Word);
VAR
 {PrevRec,} CurRec: Rec_Ptr;
 i, rcol, qsym:byte; //k: byte;
 t, point_z:real;
 rx, ry: Rec_Ana;
 ok, first:     boolean;
 point:         p_real;
 c:             Coor_XY;
 s:             string;
procedure UpDateLis(I: byte; s:string; var L,LLeg:string);
begin
 IF (Length(L)<245) AND (Position(Signif,S,L)=0)
 THEN BEGIN
  L:= L + S;
  LLeg:= LLeg + rx.champ[I]
 END
end;
BEGIN
 Dim:= 0; Point_Z:=0;
 IF I_SymCol=0 THEN Auto:=False;
 if Auto then LisAutoSymCol:='';
 LisAutoLegend:='';
 Lis_Coul:=''; Lis_Symb:=''; Lis_CoulLeg:=''; Lis_SymbLeg:='';
 Zero_ArPoint;
//IF Nb_AnaY=0 THEN halt(0);
 first:= true; //used FOR minmax
 CurRec:= FirstRec;
 WHILE CurRec<>Nil DO BEGIN
  Rx:=CurRec^.Rec;
  Ry:=Rx;
  IF X_Y_ OR S_XY_ THEN BEGIN
   OK:=(CurRec^.DonY<>NIL);
   IF OK THEN BEGIN
    Ry.Don:=CurRec^.DonY^;
    IF S_XY_ THEN Rx.Don:=Ry.Don
   END
  END ELSE OK:=TRUE;
  IF (I_SelX>0)
  THEN OK:=OK AND (position(Signif,rx.champ[I_SelX],Lis_SelX)>0);
  rcol:=1; qsym:=1;
  IF OK THEN BEGIN
   IF Auto THEN BEGIN
    s:=rx.champ[I_SymCol];{.SYMCOL}
    //new-new format: BSF, e.g. B for Black, Symbol=SquareFilled, ...
    rcol:=pos(UpCase(S[1]),CodColor);
    if rcol>0 then UpDateLis(I_Legend,s,LisAutoSymcol,LisAutoLegend);
    if length(S)>2
    then qsym:=position(length_codsym,'_'+copy(s,2,2),CodSymbol)
    ELSE qsym:=Sym_SF;
   END
   ELSE BEGIN
    if Tic then begin
     s:=rx.champ[I_Tic];
     i:=position(signif,s,LisAutomatic);
     if i<40 then begin
      rcol:=(i mod Max_Coul) + 1;
      qsym:=4*(i div Max_Coul) + 1 //=1,5,9,...
     end
     else begin rcol:=0; qsym:=0 end //i>40
    end
    else begin
     IF I_Symb=0 THEN qsym:=Sym_SF {default symbol}
     ELSE BEGIN
      s:=rx.champ[I_Symb];
      qsym:= position(Length_Codsym,'_'+s,CodSymbol);
      if qsym>0 then UpDateLis(I_SymbLeg,s,Lis_Symb,Lis_SymbLeg);
     END;//i_symb
     IF I_Coul=0 THEN rcol:=2 {default color}
     ELSE BEGIN
      s:=rx.champ[I_Coul];
      rcol:= position(1,copy(s,1,1),CodColor);
      if rcol>0 then UpDateLis(I_CoulLeg,s,Lis_Coul,Lis_CoulLeg);
     END //i_coul
    end //i_tic
   END
  END;

  IF OK AND (qsym>0) AND (rcol>0) THEN
   point[Cor_X]:= Calc_Exp(rx.don, ArAtm[CorX], ArExp[CorX], ok)
    ELSE ok:= false;
  IF ok THEN point[Cor_Y]:= Calc_Exp(ry.don, ArAtm[CorY], ArExp[CorY], ok);

  IF Trg THEN BEGIN
   IF ok THEN point_Z:= Calc_Exp(rx.don, ArAtm[CorZ], ArExp[CorZ], ok);
   ok:= ok AND (abs(100*point[Cor_X]+100*point[Cor_Y]+100*point_Z)>0.001);
   IF ok THEN BEGIN
    t:= point[Cor_X]+point[Cor_Y]+point_Z;
    point[Cor_X]:= 100*point[Cor_X]/t;
    point[Cor_Y]:= 100*point[Cor_Y]/t
   END {IF ok}
  END; {IF B_Trg}

  IF ok THEN BEGIN
//minmax
   IF FIRST THEN BEGIN p_max:= point; p_min:= point; first:= false END
   ELSE BEGIN
    FOR c:= Cor_X to Cor_Y DO BEGIN
//IF point[c]>p_max[c] THEN p_max[c]:= point[c];
//IF point[c]<p_min[c] THEN p_min[c]:= point[c];
     p_max[c]:=Max(p_max[c],point[c]);
     p_min[c]:=Min(p_min[c],point[c]);
    END
   END;
   IF Dim<Max_Point THEN BEGIN
    inc(Dim); //k:= Dim;
    arP[Dim]:= point;
    nettoie(rx.champ[I_Label]);
    arN[Dim]:=rx.champ[I_Label];
    arS[Dim]:=qsym;
    arC[Dim]:=rcol
   END;
  END; //IF ok

  CurRec:= CurRec^.Next
 END
END;//Ptr_To_XY

PROCEDURE Ptr_To_Pro(FirstRec:Rec_Ptr;N:Byte;X_Y_,S_XY_,Norm_Max:Boolean;Yes_Pro:ArrEle_Bool;VAR Dim:Word);
VAR
 {PrevRec,} CurRec: Rec_Ptr;
 I_Ele: Byte; //Clr,Smb: byte;
 i:Word;
 rx, ry: Rec_Ana;
 ok, first:     boolean;
 point:         p_real;
 c:             Coor_XY;
 Max,Min:       ArrEle_Sing;
BEGIN
 WITH ArExp[CorY] DO BEGIN Num_Trm:=1; Den_Trm:=0 END;
 FOR i:= 1 to Max_Point DO BEGIN
  arP[i][Cor_X]:= 0; {X}
  arP[i][Cor_Y]:= 0; {Y}
  arN[i]:= '';       {label}
  arC[i]:= 0;        {color}
  arS[i]:= 0         {symbole}
 END;
//IF Nb_AnaY=0 THEN halt(0);
 first:= true; //used FOR minmax
//Clr:=0; Smb:=0;
 Dim:= 0;

 IF Dim<Max_Point THEN BEGIN
  FOR I_Ele:= 1 TO N DO IF Yes_Pro[I_Ele] THEN BEGIN
   //INC(Clr); INC(Smb);
   ArExp[CorY].Num_Ind[1]:=I_Ele;
   Max[I_Ele]:=-3E4; Min[I_Ele]:=3E4;

   CurRec:= FirstRec;

   WHILE CurRec<>Nil DO BEGIN
    Rx:=CurRec^.Rec;

    OK:=TRUE;
    Ry:=Rx;
    IF X_Y_ OR S_XY_ THEN BEGIN
     OK:=(CurRec^.DonY<>NIL);
     IF OK THEN BEGIN
      Ry.Don:=CurRec^.DonY^;
      IF S_XY_ THEN Rx.Don:=Ry.Don
     END
    END;

    OK:= OK AND ((I_SelX=0) OR (position(Signif,rx.champ[I_SelX],Lis_SelX)>0));
    IF OK THEN point[Cor_X]:=Calc_Exp(rx.don, ArAtm[CorX], ArExp[CorX], ok);
    IF OK THEN point[Cor_Y]:=Calc_Exp(ry.don, ArAtm[CorY], ArExp[CorY], ok);
    IF OK THEN BEGIN
 {minmax dessin}
     IF FIRST THEN BEGIN p_max:= point; p_min:= point; first:= false END
     ELSE FOR c:= Cor_X to Cor_Y DO BEGIN
       //P_Max[C]:=Max(P_Max[C],point[c]);
       //P_Min[C]:=Min(P_Min[C],point[c]);
       IF point[c]>p_max[c] THEN p_max[c]:= point[c];
       IF point[c]<p_min[c] THEN p_min[c]:= point[c];
     END;
     IF point[Cor_Y]>Max[I_Ele] THEN Max[I_Ele]:=point[Cor_Y];
     IF point[Cor_Y]<Min[I_Ele] THEN Min[I_Ele]:=point[Cor_Y];
//
     inc(Dim);
     arP[Dim]:= point;
//
     nettoie(rx.champ[I_Label]);
//
     arN[Dim]:=rx.champ[I_Label];
     arS[Dim]:=(I_Ele-1) mod Max_Symb + 1;
     arC[Dim]:=(I_Ele-1) mod Max_Coul + 1;
//used for Color, and also to keep in memory which element it belongs to}
    END; //IF ok
    CurRec:= CurRec^.Next
   END //WHILE CurRec<>Nil
  END //IF Yes_Pro[I_Ele]
 END; //IF Dim<Max_Point

{Norm_Max}
 IF Norm_Max AND (Dim>0) THEN BEGIN
  FOR I:=1 TO Dim DO BEGIN
   I_Ele:=arC[I];{Color of point is used to know which series it belongs to}
   IF (Max[I_Ele]>0) THEN arP[I][Cor_Y]:=100*arP[I][Cor_Y]/Max[I_Ele]
  END;{IF (Dim>0)}
  P_Min[Cor_Y]:=0.0;P_Max[Cor_Y]:=99
 END;

 FOR I:=1 TO Dim DO arC[I]:= (arC[i]-1) mod Max_Coul + 1;

END;//Ptr_To_Pro

PROCEDURE Ptr_To_Histo(FirstRec:Rec_Ptr;X_Y_,S_XY_,B_Log:BOOLEAN;VAR Nb_Histo:Byte;VAR Dim:WORD);
CONST
 Nb_Class = 30;
 Max_Histo = 8;
 Min_Cas = 4; //nbre a partir duquel on dessine un histo
VAR
 Histo: Array[0..Max_Histo] of Array[0..Nb_Class] of byte;
//Histo[I_Histo][0] contains number of measures in I_Histo
 First,OK_His: Boolean;
 XMin,XMax:Single;
 Lis_Tri: string;
 I_Class, I_Histo: Byte;
 x,{ y,} Delta: Single;
 CurRec: Rec_Ptr;
 Rx,Ry: Rec_Ana;
 OK: Boolean;
BEGIN
 Zero_ArPoint;
 XMax:=-3E4; XMin:=3E4;
 IF I_SelX>0 THEN Lis_Tri:=Lis_SelX ELSE Lis_Tri:='';
 IF I_SelX>0 THEN Nb_Histo:=Length(Lis_Tri) DIV Signif
 ELSE Nb_Histo:=1;
 OK_His:=FALSE;
 IF Nb_Histo>Max_Histo THEN Nb_Histo:=Max_Histo;
 FOR I_Histo:=0 TO Nb_Histo DO
  FOR I_Class:=0 TO Nb_Class DO Histo[I_Histo][I_Class]:=0;

//calcul ArMin,ArMax
 CurRec:= FirstRec;
 FIRST:=TRUE;
 WHILE CurRec<>Nil DO BEGIN
  Rx:=CurRec^.Rec;

  OK:=TRUE;
  Ry:=Rx;
  IF X_Y_ OR S_XY_ THEN BEGIN
   OK:=(CurRec^.DonY<>NIL);
   IF OK THEN BEGIN
    Ry.Don:=CurRec^.DonY^;
    IF S_XY_ THEN Rx.Don:=Ry.Don
   END
  END;

  IF OK THEN BEGIN

  IF (I_SelX>0) THEN BEGIN
   I_Histo:=position(Signif,rx.champ[I_SelX],Lis_Tri);
   ArLabSpd[I_Histo]:=rx.champ[I_SelX];
   nettoie(ArLabSpd[I_Histo]);
  END
  ELSE BEGIN I_Histo:=1; ArLabSpd[1]:='ALL' END;
  OK:=(I_Histo>0)AND(I_Histo<=Nb_Histo);
  IF OK THEN X:=Calc_Exp(rx.don,ArAtm[CorX],ArExp[CorX],ok) ELSE X:=0;
  IF B_Log THEN OK:= OK AND (X>0);
  IF B_Log AND OK THEN X:=Log10(X);
  IF OK THEN BEGIN
   IF FIRST THEN BEGIN XMax:=x; XMin:=x; first:=false END
   ELSE BEGIN
    IF x>XMax THEN XMax:=x;
    IF x<XMin THEN XMin:=x;
   END
  END;//IF ok

  END;

  CurRec:= CurRec^.Next
 END;//WHILE CurRec<>Nil
 Delta:= XMax-XMin;
//end Calcul ArMin,ArMax

 Dim:=0; X:=0;
 IF Dim<Max_Point THEN BEGIN
  IF Abs(Delta)>Epsilon THEN BEGIN
   CurRec:= FirstRec;
   WHILE CurRec<>Nil DO BEGIN
    Rx:=CurRec^.Rec;

  OK:=TRUE;
  Ry:=Rx;
  IF X_Y_ OR S_XY_ THEN BEGIN
   OK:=(CurRec^.DonY<>NIL);
   IF OK THEN BEGIN
    Ry.Don:=CurRec^.DonY^;
    IF S_XY_ THEN Rx.Don:=Ry.Don
   END
  END;

  IF OK THEN BEGIN

    IF I_SelX>0 THEN I_Histo:=position(Signif,rx.champ[I_SelX],Lis_Tri)
    ELSE  I_Histo:=1;
    OK:=(I_Histo>0)AND(I_Histo<=Nb_Histo);
    IF OK THEN X:= Calc_Exp(rx.don, ArAtm[CorX], ArExp[CorX], ok);
    IF B_Log THEN OK:= OK AND (X>0);
    IF B_Log AND OK THEN X:=Log10(X);
    IF OK THEN BEGIN
     x:=(x-XMin)/Delta;
     IF x<=0.0 THEN x:=0.01; IF x>=1.0 THEN x:=0.99;
     I_Class:= trunc(Nb_Class*x)+1;
     inc(Histo[I_Histo][0]);{nbr total de cas dans Histo[I_Histo]}
     OK_His:=OK_His OR (Histo[I_Histo][0]>10);
     inc(Histo[I_Histo][I_Class]);
     inc(Histo[0][0]);{Hissto[0] = total histogram}
     inc(Histo[0][I_Class]);
//
     inc(Dim);
     IF ODD(I_Histo) THEN arP[Dim][Cor_X]:=XMin+(I_Class)*(XMax-XMin)/Nb_Class
     ELSE arP[Dim][Cor_X]:=XMin+(I_Class+1/2)*(XMax-XMin)/Nb_Class;
     arP[Dim][Cor_Y]:=10*(I_Histo-1)
     + 3*Histo[I_Histo][I_Class];
     arN[Dim]:='';
     IF ODD(I_Histo)THEN arS[Dim]:=Sym_SF
     ELSE arS[Dim]:=Sym_SF; {(I_Histo - 1)MOD Max_Symb + 1}
     arC[Dim]:=(I_Histo - 1)MOD Max_Coul + 1;
//
    END; //IF ok

    END;

    CurRec:= CurRec^.Next
   END;{WHILE CurRec<>Nil}
 (**
   Dim:=0;
   IF OK_His THEN FOR I_Histo:= 1 TO Nb_Histo DO BEGIN
    IF Histo[I_Histo][0]>Min_Cas THEN
    FOR I_Class:= 1 TO Nb_Class DO
    IF Histo[I_Histo][I_Class]>0 THEN
    FOR I:=1 TO Histo[I_Histo][I_Class] DO BEGIN
     inc(Dim);
     IF ODD(I_Histo) THEN arP[Dim][Cor_X]:=XMin+(I_Class)*(XMax-XMin)/Nb_Class
     ELSE arP[Dim][Cor_X]:=XMin+(I_Class+1/2)*(XMax-XMin)/Nb_Class;
     arP[Dim][Cor_Y]:=10*(I_Histo-1) + 3*I;
     arN[Dim]:='';
     arS[Dim]:=(I_Histo - 1)MOD Max_Symb + 1;
     arC[Dim]:=(I_Histo - 1)MOD Max_Coul + 1;
    END;
   END; {FOR I_Histo}
 **)
 (**
     y:=5*I_Histo + 70*Histo[I_Histo][I_Class]/Histo[I_Histo][0];
     inc(Dim);
     arS[Dim]:=0;arC[Dim]:=(I_Histo - 1)MOD Max_Coul + 1;
     arN[Dim]:='';
     arP[Dim][Cor_X]:=XMin+(I_Class-1)*(XMax-XMin)/Nb_Class;
     arP[Dim][Cor_Y]:=y;
     inc(Dim);
     arS[Dim]:=0;arC[Dim]:=(I_Histo - 1)MOD Max_Coul + 1;
     arP[Dim][Cor_X]:=XMin+(I_Class)*(XMax-XMin)/Nb_Class;
     arP[Dim][Cor_Y]:=y;
 **)
   P_Max[Cor_X]:=XMax;P_Min[Cor_X]:=XMin;
   Def_Bornes(Nega_Is_Ok,False,Cor_X);
   P_Hih[Cor_Y]:=100;P_Low[Cor_Y]:=0;
 (**
   P_Hih[Cor_X]:=10*TRUNC(XMax/10);P_Low[Cor_X]:=10*TRUNC(XMin/10)+0.1;
   P_Hih[Cor_Y]:=100;P_Low[Cor_Y]:=0;
   P_Max[Cor_X]:=XMax;P_Min[Cor_X]:=XMin;
 **)
  END;{IF Abs(Delta)>Epsilon}
 END;{IF Dim < Max_Point}

(*convert to percent*)
 FOR I_Histo:= 0 TO Nb_Histo DO BEGIN
  IF Histo[I_Histo][0]>0 THEN
  FOR I_Class:= 1 TO Nb_Class DO
   Histo[I_Histo][I_Class]:= Trunc(100*Histo[I_Histo][I_Class]/Histo[I_Histo][0]);
 END; {FOR I_Histo}
(**)
END;{Ptr_To_Histo}

end.


