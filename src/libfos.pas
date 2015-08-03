UNIT LibFos;

INTERFACE

USES LibIo;

PROCEDURE Init_LibFos(VAR FrmIn,FrmOut:Forme; VAR RecAtom:Rec_Atom);
PROCEDURE Close_LibFos;
PROCEDURE Traite_Fos(FrmIn,FrmOut:Forme;R:Rec_Ana;RA:Rec_Atom);

IMPLEMENTATION

CONST
 Lis_Nrm = 'SIALK.NAFEMNMGCASRP.TINBC.';
VAR
 F_Nrm, F_Mas: 	Text;
 Prm_Nrm:	ArrEle_Byte;
 pSi, pAl, pTi, pK, pNa, pFe, pMn, pP, pCa, pC, pMg, pSr, pBa, pNb, pZr: Byte;

FUNCTION Sum_Vec(v: ArrEle_Sing; i1, i2: Byte): Single;
VAR s: Single; i: Byte;
BEGIN
 s:= 0.0; FOR i:= i1 TO i2 DO s:= s + v[i]; Sum_Vec:= s
END;{Sum_Vec}

PROCEDURE En_Tete(VAR Fo:Text; Frm:Forme);
VAR i: Byte; s:String;
BEGIN
 WITH Frm DO FOR i:=1 TO Nb_Champ DO BEGIN
  s:=copy(Tit_Champ[i],1,signif-1); Skip_Rite(['.'],s);
  Write(Fo, Delim_Car, s, #9)
 END;
 Write(Fo,'Carb',#9,'Calc',#9,'Apat',#9,'Cran',#9,'Wavel',#9,'P_Free',#9,'Sr_Rat',#9);
 Write(Fo,'K.',#9,'Na',#9,'Al',#9,'Si',#9,'Mg',#9,'Fe',#9);
 Write(Fo,'AlcFsp',#9,'ALK_exc',#9,'Al_exc',#9,'Si_exc');
 WriteLn(Fo)
END;{En_Tete}

PROCEDURE Traite_Fos(FrmIn,FrmOut:Forme;R:Rec_Ana;RA:Rec_Atom);
VAR
 v:ArrEle_Sing;
 i: Byte;
 Carb,Sr_Rat,Ti_Rat,Na_Rat,Zrc,Apat,Cran,Wavl,Calc,Nafs,Kfsp,Kaol: Single;
 Si_x, Al_x, Si, Al, Ti, K, Na, AlcFsp, Alk_x, Fe, P, Ca, Mg: Single;

PROCEDURE Mol_To_Poi;
VAR x:Single; Tot:Single; MW:ArrEle_Sing;
BEGIN
(**
APATITE         5.CAO + 3/2.P2O5
                5*(MW[pCa]+Sr_Rat*MW[pSr])+1.5*MW[pP]
CRANDALLITE     CAO + P2O5 + 3/2.AL2O3
                (MW[pCa]+Sr_Rat*MW[pSr])+MW[pP]+1.5*MW[pAl]
WAVELLITE       P2O5 + 3/2.AL2O3
                (MW[7]+Sr_Rat*MW[8])+MW[9]+1.5*MW[2]
KFELSPAR        1/2.K2O + 1/2.AL2O3 + 3.SIO2
                MW[4]/2+MW[5]/2+MW[2]/2+3*MW[1]
**)
 MW:=RA.Molar_Weit;
 x:= (1-Sr_Rat)*MW[pCa]+Sr_Rat*MW[pSr];
 Calc:= Calc *(x+44.0{CO2});{(Ca+Sr)O + CO2}
 Apat:= Apat*(5*x+1.5*MW[pP]);{Apat=Ca5.P3}
 Cran:= Cran*(x+MW[pP]+MW[pAl]*3/2);{Cran=Ca1.P2.Al3}
 Wavl:= Wavl*(MW[pP]+MW[pAl]*3/2);
 Kfsp:= Kfsp*(MW[pK]/2+MW[pAl]/2+3*MW[pSi]);
 Nafs:= Nafs*(MW[pNa]/2+MW[pAl]/2+3*MW[pSi]);
 Kaol:= Kaol*(2*MW[pSi]+MW[pAl]);
 Al:= Al*MW[pAl]/2;
 Si:= Si*MW[pSi];
 V[pFe]:= V[pFe]*MW[pFe]/2;   {Fe2O3}
 V[pNb]:= V[pNb]*MW[pNb]/2;{Nb2O5}{not Corrected for Ti ?}

 Tot:= (Calc+Apat+Cran+Wavl+Kfsp+Nafs+Kaol+Al+Si+V[pFe]+V[pNb])/100;
 Calc:=Calc/Tot;
 Apat:=Apat/Tot;
 Cran:=Cran/Tot;
 Wavl:=Wavl/Tot;
 Kfsp:=Kfsp/Tot;
 Nafs:=Nafs/Tot;
 Kaol:=Kaol/Tot;
 Al:=Al/Tot;
 Si:=Si/Tot;
{
 Al_x:=Al_x/Tot;
 Si_x:=Si_x/Tot;
}
 V[pNb]:=V[pNb]/Tot;
 V[pFe]:=V[pFe]/Tot;
END;{Mol_To_Poi}

PROCEDURE Pyrochlore;
BEGIN
 Ti:=v[pTi]; Ti_Rat:=0;{Ti content of pyrochlore}
 IF Ti<=v[pNb]/10 THEN BEGIN
  Ti:=0.0; v[pNb]:=v[pNb]+v[pTi];
  IF v[pNb]>Epsilon THEN Ti_Rat:= v[pTi]/v[pNb];
 END
 ELSE BEGIN
  Ti:=Ti-v[pNb]/10; v[pNb]:=v[pNb]+v[pTi]/10;
  Ti_Rat:= 1/11
 END;
 v[pTi]:=Ti
END;{Pyrochlore}

PROCEDURE Zircon;
BEGIN
 IF v[pSi]<=v[pZr] THEN BEGIN
  Zrc:=Si; Si:=0.0; v[pZr]:=v[pZr]-Si {excess Zr}
 END
 ELSE BEGIN Si:=Si - v[pZr]; Zrc:=v[pZr]; v[pZr]:=0.0 END
END;{Zircon}

PROCEDURE Phosphates;
BEGIN
{Apat=Ca5.P3; Cran=Ca1.P2.Al3}
 IF Ca>=P*5/3 THEN BEGIN {Apat+Calc}
  Apat:=P/3; Calc:= Ca-P*5/3; P:=0.0;
 END
 ELSE BEGIN
  IF Ca>=P/2 THEN BEGIN {Cran+Apat}
   IF Al>=3*(5*P-3*Ca)/7 THEN BEGIN {Cran+Apat+Al,noCa,noP}
    Apat:=(2*Ca-P)/7;
    Cran:=(5*P-3*Ca)/7;
    Ca:=0;{Ca - 5*Apat - Cran;}
    P:= 0;{P - 3*Apat - 2*Cran}
    Al:=Al-3*Cran;
   END
   ELSE BEGIN {Cran+Apat+Ca,noAl,noP}
    Apat:=(2*Ca-P)/7;
    Cran:=Al/3;
    Calc:=Ca-5*Apat-Cran;{>0,because it =(5P-3Ca)/7-Al}
    Al:=0;
    P:=P-3*Apat-2*Al/3
   END
  END
  ELSE BEGIN {Cran+Exc_P}
   Cran:= Ca;
   P:=P-2*Cran;
   Al:=Al-3*Cran
  END
 END;

{Wavellite P2.Al3}
 IF (Al>0) AND (P>0) THEN
 IF P>Al*2/3 THEN BEGIN
  Wavl:= Al/3; P:= P - Wavl*2; Al:= 0.0
 END ELSE BEGIN
  Wavl:= P/2; Al:= Al - Wavl*3; P:= 0.0
 END;
END;{Phosphate}

PROCEDURE NaAlSi3O8;
BEGIN
 IF (Na<=Si/3) THEN BEGIN
  IF (Na<=Al) THEN BEGIN
   Si:=Si-3*Na; Al:=Al-Na; Nafs:=Na; Na:=0.0
  END
  ELSE BEGIN
   Si:=Si-3*Al; Na:=Na-Al; Nafs:=Al; Al:=0.0
  END
 END
 ELSE BEGIN
  IF (Na<=Al) THEN BEGIN
   Na:=Na-Si/3; Al:=Al-Na; Nafs:=Si/3; Si:= 0.0
  END
  ELSE BEGIN
   IF Al<=Si/3 THEN BEGIN {Al<Si/3<Na}
    Na:=Na-Al; Si:=Si-3*Al; Nafs:=Al; Al:=0.0
   END
   ELSE BEGIN {Si/3<Al<Na}
    Na:=Na-Si/3; Al:=Al-Si/3; Nafs:=Si/3; Si:=0.0
   END
  END
 END
END;{NaAlSi3O8}

PROCEDURE KAlSi3O8;
BEGIN
 IF (K<=Si/3) THEN BEGIN
  IF (K<=Al) THEN BEGIN
   Si:=Si-3*K; Al:=Al-K; Kfsp:=K; K:=0.0
  END
  ELSE BEGIN
   Si:=Si-3*Al; K:=K-Al; Kfsp:=Al; Al:=0.0
  END
 END
 ELSE BEGIN
  IF (K<=Al) THEN BEGIN
   K:=K-Si/3; Al:=Al-K; Kfsp:=Si/3; Si:= 0.0
  END
  ELSE BEGIN
   IF Al<=Si/3 THEN BEGIN {Al<Si/3<K}
    K:=K-Al; Si:=Si-3*Al; Kfsp:=Al; Al:=0.0
   END
   ELSE BEGIN {Si/3<Al<K}
    K:=K-Si/3; Al:=Al-Si/3; Kfsp:=Si/3; Si:=0.0
   END
  END
 END;
END;{KAlSi3O8}

PROCEDURE Kaolinite;
BEGIN
 IF (Si>0) AND (Al>0) THEN BEGIN
  IF Al>=Si THEN BEGIN
   Kaol:= Si/2;
   Al:= Al-Si;
   Si:= 0.0;
  END
  ELSE BEGIN
   Kaol:= Al/2;
   Si:= Si - Al;
   Al:= 0.0;
  END
 END
END;{Kaolinite}

BEGIN
 V:=R.Don;V[0]:=0;
 Prm_Vec(Prm_Nrm, v); V[0]:=0;
 {from Weight to Moles}
 FOR i:= 1 TO FrmOut.Nb_Ele DO WITH RA DO
  IF Molar_Weit[i]>Epsilon THEN v[i]:= v[i]/Molar_Weit[i] ELSE v[i]:=0;
 {v1:=v;}
 {from Mole to Atom, from PERCENT to PER10MIL}
 FOR i:= 1 TO FrmOut.Nb_Ele DO v[i]:= 100*v[i]*RA.Stoch[i];
 {v2:=v;}

 Carb:=v[pC];
 Al:=v[pAl]; Si:=v[pSi];
 K:=v[pK];   Na:=v[pNa];
 Ca:=Sum_Vec(v,pCa,pSr); P:=v[pP];
 Fe:=Sum_Vec(v,pFe,pMn); Mg:=v[pMg];
 IF Ca>Epsilon THEN Sr_Rat:=v[pSr]/Ca ELSE Sr_Rat:= 0.0;

 Apat:=0.0; Calc:=0; Cran:=0.0; Wavl:=0.0;
 Nafs:=0.0;  Kfsp:=0.0; Kaol:= 0.0; AlcFsp:=0.0;
 Alk_x:=Na+K; Si_x:= Si; Al_x:= Al;
 IF Alk_x>Epsilon THEN Na_Rat:=Na/Alk_x ELSE Na_Rat:=0;

 Pyrochlore;
 Zircon;
 Phosphates;
 {NaAlSi3O8}
 {KAlSi3O8}
 IF Alk_x>Al_x THEN BEGIN
  IF Al_x>Si_x/3 THEN BEGIN
   AlcFsp:=Si_x/3; Al_x:=Al_x-Si_x/3; Alk_x:=Alk_x-Si_x/3; Si_x:=0
  END ELSE BEGIN
   AlcFsp:=Al_x; Si_x:=Si_x-3*Al_x; Alk_x:=Alk_x-Al_x; Al_x:=0
  END
 END
 ELSE BEGIN
  IF Alk_x>Si_x/3 THEN BEGIN
   AlcFsp:=Si_x/3; Al_x:=Al_x-Si_x/3; Alk_x:=Alk_x-Si_x/3; Si_x:=0
  END ELSE BEGIN
   AlcFsp:=Alk_x; Si_x:=Si_x-3*Alk_x; Al_x:=Al_x-Alk_x; Alk_x:=0
  END
 END;
 {Kaolinite}

 Champ_TO_Text(F_Nrm,FrmOut,R.Champ);
 Write(F_Nrm,Real_Str(Carb),#9,Real_Str(Calc),#9,Real_Str(Apat),#9,Real_Str(Cran),#9,Real_Str(Wavl),#9,Real_Str(P),#9,Real_Str(Sr_Rat),#9);
 Write(F_Nrm,Real_Str(K),#9,Real_Str(Na),#9,Real_Str(Al),#9,Real_Str(Si),#9,Real_Str(Mg),#9,Real_Str(Fe),#9);
 Write(F_Nrm,Real_Str(AlcFsp),#9,Real_Str(Alk_x),#9,Real_Str(Al_x),#9,Real_Str(Si_x),#9);
 WriteLn(F_Nrm);

(**
 Write(Fo,'Carb',#9,'Calc',#9,'Apat',#9,'Cran',#9,'Wavel',#9,'P_Free',#9,'Sr_Rat',#9);
 Write(Fo,'K.',#9,'Na',#9,'Al',#9,'Si',#9,'Mg',#9,'Fe',#9);
 Write(Fo,'AlcFsp',#9,'Alc_x',#9,'Al_x',#9,'Si_x');
**)
END;{Traiter}

PROCEDURE Init_Fo(VAR Fo:Text;FrmOut:Forme;S:STRING);
BEGIN
 Assign(Fo,Dir_Dat+S); Rewrite(Fo);
 En_Tete(Fo,FrmOut)
END;{Init_Fo}

PROCEDURE Fermer(VAR Fo: Text);
BEGIN WriteLn(Fo, '!'); Close(Fo) END;{Fermer}

PROCEDURE Init_PrmNrm;
BEGIN
 pSi:=Position(2,'SI',Lis_Nrm);pAl:=Position(2,'AL',Lis_Nrm);
 pTi:=Position(2,'TI',Lis_Nrm);pK:=Position(2,'K.',Lis_Nrm);
 pNa:=Position(2,'NA',Lis_Nrm);pFe:=Position(2,'FE',Lis_Nrm);
 pP:=Position(2,'P.',Lis_Nrm); pCa:=Position(2,'CA',Lis_Nrm);
 pMg:=Position(2,'MG',Lis_Nrm); pSr:=Position(2,'SR',Lis_Nrm);
 pNb:=Position(2,'NB',Lis_Nrm); pZr:=Position(2,'ZR',Lis_Nrm);
 pBa:=Position(2,'BA',Lis_Nrm); pC:=Position(2,'C.',Lis_Nrm);
 pMn:=Position(2,'MN',Lis_Nrm);
END;{Init_PrmNrm}

PROCEDURE Init_LibFos(VAR FrmIn,FrmOut:Forme; VAR RecAtom:Rec_Atom);
VAR
 i: Byte;
 l: String;
BEGIN
 FrmOut:= FrmIn;
 L:= Lis_Nrm;
 Init_PrmNrm;
 Ent_Prm(FrmIn, FrmOut, L, Prm_Nrm);
 Calc_Recatom(FrmOut,RecAtom);
 WITH FrmOut DO FOR i:= 1 TO Nb_Ele DO Tit_Oxy[i]:= RecAtom.Tit_Ele[i];
 Init_Fo(F_Nrm,FrmOut,'FOSFATE.TXT');
 Init_Fo(F_Mas,FrmOut,'FOSFATE.MAS');
END;{Init_LibNrm}

PROCEDURE Close_LibFos;
BEGIN
 Close(F_Nrm); Close(F_Mas)
END;{Close_LibNrm}

END.


