UNIT LibStc; {calcul de formules structurales}

INTERFACE

USES Libio;

PROCEDURE Init_LibStc(VAR FrmIn,FrmOut:Forme; VAR RecAtom:Rec_Atom);
PROCEDURE Close_LibStc;
PROCEDURE Traite_Stc(FrmStc:Forme;R:Rec_Ana;RecAtom:Rec_Atom);

IMPLEMENTATION

uses sysutils;

CONST
 Sep = #9;
 Def_Oxy:BYTE=24;
 LisStc = 'NAK.CAMGMNFETIALSIP.F.CLS.V.CRNIZNSRBANBTAZRLACEY.THU.';
 LisMin =
'WHOLEBIOTIMUSCOCHLORAMPHIHBL..HORNBCORDIFELSPFELDS'
{01   02   03   04   05   06   07   08   09   10   }
+
'PLAGIKFELSPXN..CPX..OPX..PYROXOLIVIGARNEGRENASPHEN'
{11   12   13   14   15   16   17   11   19   20   }
+
'APATIEPIDOSILLIANDALSPINEMAGNEAEGIRHUMITPEROVPYROC'
{21   22   23   24   25   26   27   28   29   30   }
+
'MICA.DIOPSAUGITTITANRICHT';
{31   32   33   34   35   36   37   31   39   40   }

 Debug= False;

VAR
 F_Stc, F_Rst, F_Mic, F_Amp, F_Pxn,
 F_Spi, F_Prc, F_Gar, F_Fsp, F_Apa: Text;
 Stc_Open, Rst_Open, Spi_Open,
 Mic_Open, Amp_Open, Pxn_Open,
 Gar_Open, Fsp_Open, Prc_Open,
 Apa_Open: Boolean;
 I_Min: Byte;
 Cur_Lis, Cur_Nam: STRING;
 PrmStc: ArrEle_Byte;
 pNA,pK_,pCA,pMG,pMN,pFE,pTI,pAL,pSI,
 pP_,pC_,pF_,pCL,pCR,pV_,
 pNI,pZN,pBA,pSR,pNB,pTA,pZR,
 pLA,pCE,pY_,pTH,pU_,pS_: BYTE;

function R_Str(R: Real): String;
var S: String;
begin
 if R <0.001 then s:='_' else s:=FloatToStrF(R,ffFixed,4,6);
 R_Str:=s
end;

PROCEDURE Don_To_Text2(VAR F_Out:TextFile;Frm:Forme;Arr_B:ArrEle_Bool;Arr_S:ArrEle_Sing);
VAR i: Byte;
BEGIN
//FOR i:= 1 TO Frm.Nb_Ele DO IF Arr_B[i] THEN Write(F_Out, Real_StrOld(Arr_S[i]), #9)
 FOR i:= 1 TO Frm.Nb_Ele DO IF Arr_B[i] THEN Write(F_Out, Arr_S[i]:8:3, #9)
END;{Don_To_Text2}

PROCEDURE Init_Fo(VAR Fo:Text; FrmOut:Forme; Nam:Str_Signif; VAR b:Boolean);
VAR i:Byte;
BEGIN
 {$I-}Assign(Fo, Dir_Stc+Nam+'.txt'); Rewrite(Fo);{$I+}
 B:=(IoResult=0);
 IF B THEN BEGIN
  Writeln(Fo,'_',Nam);
  TitChamp_To_Text(Fo,FrmOut);

  IF Pos(UpperCase(Nam),'.OTHERS.ALL')>0 THEN Write(Fo,'OXY',Sep);

  Nam:='.'+copy(Nam,1,3);
  case pos(Nam,'.MIC.AMP.PYR.GAR.SPI.PIR') of
   21: BEGIN {PYROCHLORE}
    Write(Fo,'Nb',Sep,'Ta',Sep,'Zr',Sep,'Ti',Sep,'Fe',Sep,'Si',Sep,'Al',Sep,'P.',Sep,'B_Sit',Sep);
    Write(Fo,'Na',Sep,'K.',Sep,'Ca',Sep,'Mg',Sep,'Mn',Sep,'Sr',Sep,'Ba',Sep,'La',Sep,'Ce',Sep,'Y.',Sep,'Th',Sep,'U.',Sep,'A_Sit',Sep)
   END;
   17: BEGIN {SPINEL}
    Write(Fo,'Mg',Sep,'Fe_2',Sep,'Mn',Sep,'Ni',Sep,'Ca',Sep);
    Write(Fo,'Ti',Sep,'Al',Sep,'Fe_3',Sep,'Cr',Sep,'V.',Sep,'Mg_Rt',Sep);
   END;
   ELSE WITH FrmOut DO
    FOR i:= 1 TO Nb_Ele DO IF Arr_Vrai[i] THEN Write(Fo, Tit_Oxy[i],sep);
  end;{case}

  case pos(Nam,'.MIC.AMP.PYR.GAR.SPI.PYR') of
  {             1   5   9   13  17  21       }
   1: begin
     Write(Fo,'K+Na',Sep,'Octa',Sep,'Al_Oc',Sep,'Al_Tt',Sep,'Fe_Tt',Sep);
     Write(Fo,'Fe3_Oc',Sep,'Fe2_Oc',Sep,'Mg_Rt',Sep,'Charge',Sep);
   end;
   5:  Write(Fo,'A',Sep,'Octa',Sep,'Al_Oc',Sep,'Al_Tt',Sep,'Si',Sep,'Mg_Rt',Sep);
   9:  Write(Fo,'Octa',Sep,'Al_Oc',Sep,'Al_Tt',Sep,'Fe_2',Sep,'Fe_3',Sep,'Mg_Rt',Sep);
   13: Write(Fo,'Octa',Sep,'Al_Oc',Sep,'Al_Tt',Sep,'Fe_2',Sep,'Fe_3',Sep,'Mg_Rt',Sep);
  end;

  WriteLn(Fo);
 END
END;{Init_Fo}

PROCEDURE Init_PrmStc;
BEGIN
 pSi:=Position(2,'SI',LisStc);pAl:=Position(2,'AL',LisStc);
 pNa:=Position(2,'NA',LisStc);pK_:=Position(2,'K.',LisStc);
 pTi:=Position(2,'TI',LisStc);pFe:=Position(2,'FE',LisStc);
 pMg:=Position(2,'MG',LisStc);pMn:=Position(2,'MN',LisStc);
 pP_:=Position(2,'P.',LisStc);pCa:=Position(2,'CA',LisStc);
 pF_:=Position(2,'F.',LisStc);pCl:=Position(2,'CL',LisStc);
 pSr:=Position(2,'SR',LisStc);pBa:=Position(2,'BA',LisStc);
 pC_:=Position(2,'C.',LisStc);pCr:=Position(2,'CR',LisStc);pV_:=Position(2,'V.',LisStc);
 pNb:=Position(2,'NB',LisStc);pTa:=Position(2,'TA',LisStc);pZr:=Position(2,'ZR',LisStc);
 pNi:=Position(2,'NI',LisStc);pZn:=Position(2,'ZN',LisStc);
 pTh:=Position(2,'TH',LisStc);pU_:=Position(2,'U.',LisStc);pS_:=Position(2,'S.',LisStc);
 pLa:=Position(2,'LA',LisStc);pCe:=Position(2,'CE',LisStc);pY_:=Position(2,'Y.',LisStc);
END;{Init_PrmStc}

PROCEDURE Init_LibStc(VAR FrmIn,FrmOut:Forme; VAR RecAtom:Rec_Atom);
VAR
 i: Byte;
 l: String;
BEGIN
 L:= LisStc;
 Init_PrmStc;
 Ent_Prm(FrmIn,FrmOut,L,PrmStc);
 Cur_Lis:=FrmIn.Lis_Ele;
 FOR i:= 1 TO FrmOut.Nb_Ele DO Arr_Vrai[i]:=(PrmStc[i]>0);
 Calc_Recatom(FrmOut,RecAtom);
 WITH FrmOut DO FOR i:= 1 TO Nb_Ele DO Tit_Oxy[i]:= RecAtom.Tit_Ele[i];
 I_Min:=2;
 Init_Fo(F_Stc,FrmOut,'ALL',Stc_Open);
 Init_Fo(F_Rst,FrmOut,'OTHERS',Rst_Open);
 Mic_Open:=False; Amp_Open:=False; Pxn_Open:=False;
 Gar_Open:=False; Fsp_Open:=False; Prc_Open:=False;
 Spi_Open:=False; Apa_Open:=False;
END;{Init_LibStc}

PROCEDURE Mica(F:Forme; R:Rec_Ana);
VAR
 v: ArrEle_Sing;
 i: byte;
 Tot, K_Site, Mg_Rat, Al_Tetr, Fe_Tetr, Al_Octa, Fe_Octa, Octa, Fe2Oc, Fe3Oc, x: single;
 ChargeOcta, ChargeTetra, ChargeKSite: single;
BEGIN
 V:=R.Don;
 tot:=0.0; for i:= 1 to F.Nb_Ele do tot:=tot+v[i];
 K_Site:= v[pNa]+v[pK_]+v[pCa]+v[pBA];
 tot:= tot + v[pTi] - K_Site - v[pCL] - v[pF_];
//for each Ti atom, make one vacancy in octa-site
//tot:=v[pMG]+v[pMN]+v[pFE]+v[pTI]+v[pAL]+v[pSI]
//+v[pCR]+v[pV_]+v[pNI]+v[pZN]+v[pSR];
//normalize to Sum Of Cations other Than K,Na,Ca,Ba = 7.0
 IF tot>Epsilon THEN FOR i:=1 TO F.Nb_Ele DO v[i]:=v[i]*7.0/tot;
 K_Site:= v[pNa]+v[pK_]+v[pCa]+v[pBA];
//IF K_Site>1.0 THEN FOR i:=1 TO F.Nb_Ele DO v[i]:=v[i]*1.0/K_Site;
//IF v[pSi]>3.0 THEN BEGIN x:=v[pSi]; FOR i:=1 TO F.Nb_Ele DO v[i]:=v[i]*3.0/x END;

 IF NOT Mic_Open THEN Init_Fo(F_Mic,F,'MICA',Mic_Open);
//NAK.CAMGMNFETIALSIP.F.CL
//1 2 3 4 5 6 7 8 9 101112
 IF Mic_Open THEN BEGIN
  Champ_To_Text(F_Mic,F,R.Champ);Don_To_Text2(F_Mic,F,Arr_Vrai,V);
//kinoshitalite=BaMg3Si2Al2= K+Si=Ba+AlTetr
  Al_Octa:= v[pAl]; Al_Tetr:=0.0;
  Fe_Tetr:=0.0;     Fe_Octa:=v[pFe];
  Fe2Oc:=Fe_Octa;   Fe3Oc:=0.0;
  IF v[pSi]<=4.0 THEN BEGIN
   IF v[pSi]+v[pAl]>=4.0 THEN BEGIN
    Al_Tetr:=4-v[pSi];
    Al_Octa:=v[pAl]-Al_Tetr;
   END
   ELSE BEGIN
    Al_Octa:=0.0;Al_Tetr:=v[pAl];
    IF (v[pSi]+v[pAl]+v[pFe])>=4.0 THEN BEGIN
     Fe_Tetr:=4.0-v[pSi]-v[pAl];
     Fe_Octa:=v[pFe]-Fe_Tetr
    END
    ELSE BEGIN
     Fe_Tetr:=v[pFe]; Fe_Octa:=0.0
    END
   END;
   IF Al_Tetr>1 THEN BEGIN
    IF (Al_Tetr - 1 - v[pBa])>Al_Octa THEN BEGIN
     IF Fe_Octa > (Al_Tetr - 1 - v[pBa] - Al_Octa) THEN BEGIN
      Fe3Oc:= Al_Tetr - 1 - v[pBa] - Al_Octa;
      Fe2Oc:= Fe_Octa - Fe3Oc
     END
     ELSE BEGIN
      Fe3Oc:=Fe_Octa; Fe2Oc:=0.0
     END
    END
   END;//IF Al_Tetr>1
  END;//IF v[pSi]<=4.0

  Mg_Rat:=v[pMg]+v[pMn]+Fe2Oc;{MGMNFE}
  IF Mg_Rat>Epsilon THEN Mg_Rat:=v[pMg]/Mg_Rat ELSE Mg_rat:=0.0;
  Octa:=v[pMg]+v[pMn]+Fe_Octa+2*v[pTi]+Al_Octa;//includes 1 vacancy for each Ti !!
  ChargeOcta:=2*v[pMg]+2*v[pMn]+2*Fe2Oc+3*Fe3Oc+4*v[pTi]+3*Al_Octa;
  ChargeTetra:=4*v[pSi]+3*Al_Tetr+3*Fe_Tetr;
  ChargeKSite:=v[pNa]+v[pK_]+2*v[pCa]+2*v[pBa];
  K_Site:= v[pNa]+v[pK_]+v[pCa]+v[pBa];
  write(F_Mic,Real_StrOld(K_Site),Sep,Real_StrOld(Octa),Sep,Real_StrOld(Al_Octa),Sep);
  write(F_Mic,Real_StrOld(Al_Tetr),Sep,Real_StrOld(Fe_Tetr),Sep,Real_StrOld(Fe3Oc),Sep,Real_StrOld(Fe2Oc),Sep,Real_StrOld(Mg_Rat),Sep);
  write(F_Mic,Real_Str(ChargeOcta+ChargeTetra+ChargeKSite));
//write(F_Mic,Real_Str(ChargeOcta),Sep,Real_Str(ChargeTetra),Sep,Real_Str(ChargeOcta+ChargeTetra));
  WriteLn(F_Mic)
 END
END;{Mica}

PROCEDURE Amphibole(F:Forme; R:Rec_Ana);
VAR
 v: ArrEle_Sing;
 i: Byte;
 Mg_Rat, A_Site, Si, Al_Tetr, Al_Octa, Octa: single;
BEGIN
 V:=R.Don;
 IF NOT Amp_Open THEN Init_Fo(F_Amp,F,'AMPHIBO',Amp_Open);
 IF Amp_Open THEN BEGIN
  Si:=v[pSi];
  IF Si>8.0 THEN FOR i:=1 TO F.Nb_Ele DO v[i]:=v[i]*8.0/Si;
  Champ_To_Text(F_Amp,F,R.Champ);Don_To_Text2(F_Amp,F,Arr_Vrai,V);
 {NAK.CAMgMNFETIALSIP.F.CL}
 {1 2 3 4 5 6 7 8 9 101112}
 (**
       A______B______C______T______O______(OH,F,Cl)
       0-1____2______5______8______22_____2________

       A______M4_____M1..M3_T______
       Na     Ca     Al     SiAl
       K      MgFeMn Fe3
       #      Na     Fe2Ti
                     MnMg

  CM=  A0     FM7__________ Si8

  TR=  A0     Ca2    FM5    Si8---------Ca-Amphib
  RI=  NaK    NaCa   Mg5    Si8          |
  ED=  NaK    Ca2    FM5    Al_Si7 =     |
                            |  |   =HORNBLENDES
  PA=  NaK    Ca2    FM4Al  Al2Si6 =

  Edenitic substitø      ED____<-->_____TR
                     NaK + Al      #  + Si
                     A     T       A    T
  Tschermak Substitø     PA____<-->_____ED
                     Al  + Al      FM   + Si
                     M1M3  T       M1M3   T
 **)
  A_Site:=v[pNa]+v[pK_];
  Al_Octa:=v[pAl]; Al_Tetr:=0.0;
  IF v[pSi]<=8.0 THEN BEGIN
   IF v[pSi]+v[pAl]>8 THEN BEGIN
    Al_Tetr:= 8.0-v[pSi];
    Al_Octa:= v[pAl]-Al_Tetr
   END
   ELSE BEGIN
    Al_Tetr:= v[pAl];
    Al_Octa:= 0.0
   END
  END;

  Octa:=v[pMg]+v[pMn]+v[pFe];
  IF Octa>Epsilon THEN Mg_Rat:=v[pMg]/Octa ELSE Mg_Rat:=0.0;
  Octa:=Octa+v[pCa]+v[pTi]+Al_Octa;
  write(F_Amp,Real_StrOld(A_Site),Sep,Real_StrOld(Octa),Sep,Real_StrOld(Al_Octa),Sep);
  write(F_Amp,Real_StrOld(Al_Tetr),Sep,Real_StrOld(Si),Sep,Real_StrOld(Mg_Rat),Sep);
  WriteLn(F_Amp)
 END
END;{Amphibole}

PROCEDURE Pyroxene(F:Forme; R:Rec_Ana);
VAR
 v: ArrEle_Sing;
 i: Byte;
 Fe_3, Fe_2, Mg_Rat, Si, Al_Tetr, Al_Octa, Octa: single;
BEGIN
 V:=R.Don;
 IF NOT Pxn_Open THEN Init_Fo(F_Pxn,F,'PYROXEN',Pxn_Open);
 IF Pxn_Open THEN BEGIN
  Si:= v[pSi];
  IF Si>2.0 THEN {normalize all cations to Si=2}
   FOR i:= 1 TO F.Nb_Ele DO v[i]:=v[i]*2.0/Si;
  Champ_To_Text(F_Pxn,F,R.Champ);Don_To_Text2(F_Pxn,F,Arr_Vrai,V);
 (**
    MgMg.SiSi.O6 ENSTATITE
    MgAl.AlSi.O6 TSCHERMAK
    NaFe.SiSi.O6 AEGIRINE
    NaAl.SiSi.O6 JADEITE
 **)
 {NAK.CAMgMNFETIALSIP.F.CL}
 {1 2 3 4 5 6 7 8 9 101112}
  Al_Octa:=v[pAl]; Al_Tetr:= 0.0;
  IF v[pSi]<2.0 THEN BEGIN
   IF v[pAl]>2-v[pSi] THEN BEGIN
    Al_Tetr:=2-v[pSi]; Al_Octa:=v[pAl]-Al_Tetr
   END
   ELSE BEGIN Al_Tetr:= v[pAl]; Al_Octa:= 0.0
   END
  END;
  v[pAl]:=Al_Octa;
 (** Al_Octa+2*Ti+Fe_3=Na+K+Al_Tetr **)
  Fe_3:=0.0; Fe_2:= v[6];
  IF (v[pNa]+v[pK_]+Al_Tetr)>(Al_Octa+2*v[pTi])
   THEN Fe_3:=(v[pNa]+v[pK_]+Al_Tetr)-(Al_Octa+2*v[pTi]);
  IF Fe_3<Fe_2 THEN Fe_2:= Fe_2 - Fe_3
  ELSE BEGIN Fe_3:=Fe_2; Fe_2:= 0.0 END;

  Octa:=v[pNa]+v[pK_]+v[pCa]+v[pMg]+v[pMn]+v[pFe]+v[pAl]+v[pTi];
  v[pFe]:=Fe_2;
  Mg_Rat:=v[pMg]+v[pMn]+v[pFe];
  IF Mg_Rat>Epsilon THEN Mg_Rat:=v[pMg]/Mg_Rat ELSE Mg_Rat:=0.0;
  write(F_Pxn,Real_StrOld(Octa),Sep,Real_StrOld(Al_Octa),Sep,Real_StrOld(Al_Tetr),Sep);
  write(F_Pxn,Real_StrOld(Fe_2),Sep,Real_StrOld(Fe_3),Sep,Real_StrOld(Mg_Rat),Sep);
  WriteLn(F_Pxn)
 END
END;{Pyroxene}

PROCEDURE Garnet(F:Forme;R:Rec_Ana);
VAR
 v: ArrEle_Sing;
 i: Byte;
 Fe_3, Fe_2, Mg_Rat, Tetra, Si, Al, Al_Tetr, Al_Octa, Octa: single;
BEGIN
 V:=R.Don;
(**
 3     2     3     12
 |     |     |     |
 Mg    Al    Si    O
 Fe2   Fe3   Al_4
 Mn    Ti
 Ca    Cr

 PYROPE======Mg3_Al2_Si3    OUVAROV.====Ca3_Cr2_Si3
 ALMANDIN====Fe3_Al2_Si3    ANDRADITE===Ca3_Fe2_Si3
 SPESSART.===Mn3_Al2_Si3    GROSSULAR===Ca3_Al2_Si3

**)
 IF NOT Gar_Open THEN  Init_Fo(F_Gar,F,'GARNET',Gar_Open);
 IF Gar_Open THEN BEGIN
  Tetra:=3.0;
  Si:= v[pSi];
  IF Si>Tetra THEN {normalize all cations to Si=Tetra}
   FOR i:= 1 TO F.Nb_Ele DO v[i]:=v[i]*Tetra/Si;
  Champ_To_Text(F_Gar,F,R.Champ);Don_To_Text2(F_Gar,F,Arr_Vrai,V);

  Si:= v[pSi];
  Al:= v[pAl];
  Al_Octa:= Al; Al_Tetr:= 0.0;
  IF Si<Tetra THEN BEGIN
   IF Al>Tetra-Si THEN BEGIN
    Al_Tetr:=Tetra-Si;
    Al_Octa:=Al-Al_Tetr
   END
   ELSE BEGIN
    Al_Tetr:= Al;
    Al_Octa:= 0.0
   END
  END;
  v[pAl]:=Al_Octa;
  Fe_2:=v[pFe]; Fe_3:=0.0;
  IF 2.0>(Al_Octa+v[pTi]) THEN BEGIN
   IF (2.0-(Al_Octa+v[pTi]))>Fe_2 THEN BEGIN
    Fe_3:=Fe_2; Fe_2:=0.0
   END
   ELSE BEGIN
    Fe_3:=2.0-(Al_Octa+v[pTi]); Fe_2:= Fe_2-Fe_3
   END
  END;
  Octa:=v[pNa]+v[pK_]+v[pCa]+v[pMg]+v[pMn]+v[pFe]+v[pAl]+v[pTi];
  v[pFe]:=Fe_2;
  Mg_Rat:= v[pMg]+v[pMn]+v[pFe];
  IF Mg_Rat>Epsilon THEN Mg_Rat:=v[pMg]/Mg_Rat ELSE Mg_Rat:=0.0;
 (**)
  write(F_Gar,Real_StrOld(Octa),Sep,Real_StrOld(Al_Octa),Sep,Real_StrOld(Al_Tetr),Sep);
  write(F_Gar,Real_StrOld(Fe_2),Sep,Real_StrOld(Fe_3),Sep,Real_StrOld(Mg_Rat),Sep);
  WriteLn(F_Gar)
 END
END;{Garnet}

PROCEDURE Felspar(F:Forme;R:Rec_Ana);
VAR v: ArrEle_Sing;
BEGIN
 V:=R.Don;
 IF NOT Fsp_Open THEN  Init_Fo(F_Fsp,F,'FELSPAR',Fsp_Open);
 Champ_To_Text(F_Fsp,F,R.Champ);Don_To_Text2(F_Fsp,F,Arr_Vrai,R.Don);
 Write(F_Fsp, Real_StrOld(v[pNa]+v[pK_]+v[pCa]),Sep);
 Write(F_Fsp, Real_StrOld(v[pSi]+v[pAl]),Sep);
 WriteLn(F_Fsp)
END;{Felspar}

PROCEDURE Apatite(F:Forme;R:Rec_Ana);
VAR v: ArrEle_Sing; i:byte; x: single;
BEGIN
 IF NOT Apa_Open THEN Init_Fo(F_Apa,F,'APAT',Apa_Open);
 V:=R.Don;
 x:=R.Don[pP_];
 IF x>6.0 THEN FOR i:=1 TO F.Nb_Ele DO v[i]:=v[i]*6.0/x;
 Champ_To_Text(F_Apa,F,R.Champ);Don_To_Text2(F_Apa,F,Arr_Vrai,V);
 WriteLn(F_Apa)
END;//Apatite

PROCEDURE Spinel(F:Forme;R:Rec_Ana);
VAR v: ArrEle_Sing; i:byte; _div, _triv, Fe_3, Fe_2, Mg_Rat: single;
BEGIN
 V:=R.Don;
 _div:= v[pMg] + v[pFe] + v[pMn] + v[pNi];
 _triv:= v[pCr] + v[pAl]+ v[pTi];
//valence of Fe not known
//>>work on (sum of cations)=3 instead of sum of valencies !!!
 if (_div+_triv)>Epsilon then
 for i:=1 to F.Nb_Ele do v[i]:=v[i]*3/(_div+_triv);

 _div:= v[pMg] + v[pFe] + v[pMn] + v[pNi] - v[pTi];
//Titanium calculated as ilmenite molecule, i.e. FeO.TiO2
//>> for each Ti there is a divalent cation not available for spinel
 _triv:= v[pCr] + v[pAl];
 Fe_2:= v[pFe]; Fe_3:= 0.0;
 if (_triv<2*_div) then begin
  Fe_3:=(2*_div - _triv)/3;
  Fe_2:=Fe_2 - Fe_3;
 end;

 _div:= v[pMg] + Fe_2 + v[pMn] + v[pNi]; _triv:= Fe_3 + v[pCr] + v[pAl];
 IF _div>Epsilon THEN Mg_Rat:=v[pMg]/_div ELSE Mg_Rat:=0.0;

 R.Don:=v;
 IF NOT Spi_Open THEN  Init_Fo(F_Spi,F,'SPINEL',Spi_Open);
 Champ_To_Text(F_Spi,F,R.Champ);
 Write(F_Spi,Real_StrOld(v[pMg]),Sep,Real_StrOld(Fe_2),Sep,Real_StrOld(v[pMn]),Sep);
 Write(F_Spi,Real_StrOld(v[pNi]),Sep,Real_StrOld(v[pCa]),Sep);
 Write(F_Spi,Real_StrOld(v[pTi]),Sep,Real_StrOld(v[pAl]),Sep);
 Write(F_Spi,Real_StrOld(Fe_3),Sep,Real_StrOld(v[pCr]),Sep);
 Write(F_Spi,Real_StrOld(v[pV_]),Sep,Real_StrOld(Mg_Rat),Sep);
 WriteLn(F_Spi)
END;{Spinel}

PROCEDURE Pyrochlor(F:Forme;R:Rec_Ana);
VAR v:ArrEle_Sing; B_Site,A_Site:Single; i:Byte;
BEGIN
 V:=R.Don;
 IF NOT Prc_Open THEN  Init_Fo(F_Prc,F,'PIROCHL',Prc_Open);
 B_Site:=v[pNb]+v[pTa]+v[pZr]
        +v[pTi]+v[pFe]+v[pSi]+v[pAl]+v[pP_];
 IF B_Site>Epsilon THEN
  FOR i:=1 TO F.Nb_Ele DO V[i]:=V[i]*2.0/B_Site;
 B_Site:=2.0;
 A_Site:=v[pNa]+v[pK_]+v[pCa]+v[pMg]+v[pMn]
        +v[pSr]+v[pBa]+v[pLa]+v[pCe]+v[pY_]+v[pTh]+v[pU_];

 Champ_To_Text(F_Prc,F,R.Champ);
 Write(F_Prc,Real_StrOld(v[pNb]),Sep,Real_StrOld(v[pTa]),Sep,Real_StrOld(v[pZr]),Sep);
 Write(F_Prc,Real_StrOld(v[pTi]),Sep,Real_StrOld(v[pFe]),Sep,Real_StrOld(v[pSi]),Sep);
 Write(F_Prc,Real_StrOld(v[pAl]),Sep,Real_StrOld(v[pP_]),Sep,Real_StrOld(B_Site),Sep);
 Write(F_Prc,Real_StrOld(v[pNa]),Sep,Real_StrOld(v[pK_]),Sep,Real_StrOld(v[pCa]),Sep);
 Write(F_Prc,Real_StrOld(v[pMg]),Sep,Real_StrOld(v[pMn]),Sep,Real_StrOld(v[pSr]),Sep,Real_StrOld(v[pBa]),Sep);
 Write(F_Prc,Real_StrOld(v[pLa]),Sep,Real_StrOld(v[pCe]),Sep,Real_StrOld(v[pY_]),Sep);
 Write(F_Prc,Real_StrOld(v[pTh]),Sep,Real_StrOld(v[pU_]),Sep,Real_StrOld(A_Site));
 WriteLn(F_Prc)
END;

PROCEDURE Traite_Stc(FrmStc:Forme;R:Rec_Ana;RecAtom:Rec_Atom);
VAR
 i, Oxy: Byte;
 v,w: ArrEle_Sing; v1,v2:ArrEle_Byte;
 Tot: Single;
 s: Str_Signif;
BEGIN
 V:=R.Don;
 S:=R.Champ[I_Min];
 Prm_Vec(PrmStc, v);

 Tot:= 0.0;
 WITH RecAtom DO
 FOR i:= 1 TO FrmStc.Nb_Ele DO BEGIN
  if v[i]>0.0 then v[i]:= v[i] / (Atomic_Weit[i] * Stoch[i] + 16.00 * Nb_Oxy[i])
  else v[i]:=0.0;
  w[i]:=Atomic_Weit[i];
  v1[i]:=stoch[i];
  v2[i]:=Nb_Oxy[i];
  Tot:= Tot + v[i] * Nb_Oxy[i];{total of valencies}
 END;

(**
 Champ_To_Text(F_Stc,FrmStc,R.Champ); Write(F_Stc,Oxy,Sep);
 Don_To_Text2(F_Stc,FrmStc,Arr_Vrai,V); WriteLn(F_Stc);
**)

 Oxy:=Def_Oxy;

 CASE Position(5, s, LisMin) Of
  0:            Oxy:= Def_Oxy;
  1:            Oxy:= 0; {Analyse Roche Totale}
  2,3,4,31:    	Oxy:= 11;{BIOTIMUSCOCHLORMICA}
  5,6,7,35:   	Oxy:= 23;{AMPHIHBL..HORNB}
  8:            Oxy:= 18;{CORDI}
  9,10,11,12:   Oxy:= 8; {FELSPFELDSPLAGIKFELS}
  13,14,15,16,27,32,33:Oxy:= 6; {PXN..CPX..OPX..PYROXAEGIDIOPS}
  29:           Oxy:= 3; {PEROV}
  17,25,26:     Oxy:= 4; {OLIVIMAGNESPINE}
  18,19:        Oxy:= 12;{GARNEGRENA}
  20,34:        Oxy:= 5; {SPHENTITAN}
  28:           Oxy:= 17;
  21:           Oxy:= 26;{APATI}
  22:           Oxy:= 25;{EPIDO}
  23,24:        Oxy:= 5; {SILLIM,ANDALOUSITE,...}
 END;
 FOR i:= 1 TO FrmStc.Nb_Ele DO
 IF Tot<>0 THEN
  (**IF Oxy>0 THEN**) v[i]:=v[i]*(Oxy/Tot)*RecAtom.Stoch[i]
  (**ELSE WITH Def_RecAtom DO BEGIN
   IF Nb_Oxy[i]>0 THEN v[i]:= 100*v[i]*Stoch[i] {from PERCENT to PERMIL}
   ELSE v[i]:= v[i]*Stoch[i]/100 {from PPM to PERMIL}
  END**)
 ELSE v[i]:= 0.0;

 R.Don:=V;
 Champ_To_Text(F_Stc,FrmStc,R.Champ); Write(F_Stc,Oxy,Sep);
 Don_To_Text2(F_Stc,FrmStc,Arr_Vrai,R.Don); WriteLn(F_Stc);

(**
 Champ_To_Text(F_Stc,FrmStc,R.Champ); Write(F_Stc,Oxy,Sep);
 for i:= 1 TO FrmStc.Nb_Ele DO write(F_Stc,v1[i],sep); WriteLn(F_Stc);
 Champ_To_Text(F_Stc,FrmStc,R.Champ); Write(F_Stc,Oxy,Sep);
 for i:= 1 TO FrmStc.Nb_Ele DO write(F_Stc,v2[i],sep); WriteLn(F_Stc);
 Champ_To_Text(F_Stc,FrmStc,R.Champ); Write(F_Stc,Oxy,Sep);
 Don_To_Text2(F_Stc,FrmStc,Arr_Vrai,w); WriteLn(F_Stc);
**)

 CASE Position(5, s, LisMin) Of
  2,3,4,31:    	Mica(FrmStc,R);
  5,6,7,35:    	Amphibole(FrmStc,R);
  9,10,11,12:   Felspar(FrmStc,R);
  13,14,15,16,27,32,33:Pyroxene(FrmStc,R);
  21:           if pP_>0 then Apatite(FrmStc,R);
  25,26:	Spinel(FrmStc,R);
  18,19:	Garnet(FrmStc,R);
  30:           Pyrochlor(FrmStc,R)
  ELSE  BEGIN
   Champ_To_Text(F_Rst,FrmStc,R.Champ); Write(F_Rst,Oxy,Sep);
   Don_To_Text2(F_Rst,FrmStc,Arr_Vrai,R.Don); WriteLn(F_Rst);
  END
 END
END;{Traiter}

PROCEDURE Close_LibStc;
 PROCEDURE Fermer(B: Boolean; VAR Fo: Text);
 BEGIN IF B THEN BEGIN WriteLn(Fo); CloseFile(Fo) END END;{Fermer}
BEGIN
 Fermer(Stc_Open,F_Stc);Fermer(Rst_Open,F_Rst);
 Fermer(Mic_Open,F_Mic);Fermer(Amp_Open,F_Amp);
 Fermer(Pxn_Open,F_Pxn);Fermer(Gar_Open,F_Gar);
 Fermer(Fsp_Open,F_Fsp);Fermer(Prc_Open,F_Prc);
 Fermer(Spi_Open,F_Spi);Fermer(Apa_Open,F_Apa)
END;{Close_LibStc}

BEGIN
END.

