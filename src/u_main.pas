unit u_main;

interface

uses
  Windows,Messages,SysUtils,Classes,Graphics,Controls,Menus,
  Forms,Dialogs,StdCtrls,Buttons,ExtCtrls,checklst,ComCtrls,ShellApi;

type
  TMainForm = class(TForm)
    ListBoxField: TListBox;
    ListBoxSelec: TListBox;
    ListBoxX: TListBox;
    ListBoxY: TListBox;
    ListBoxZ: TListBox;

    Cur_X:  TEdit;
    Cur_Y:  TEdit;
    Cur_Z:  TEdit;
    LabelX: TLabel;
    LabelY: TLabel;
    LabelZ: TLabel;
    MainMenu: TMainMenu;

    mnMain:   TMenuItem;
    mnReload: TMenuItem;
    mnQuit:   TMenuItem;
//mnDiagrams
    mnDiagrams: TMenuItem;
    mnDesXY:    TMenuItem;
    mnDesTrg:   TMenuItem;
    mnProfiler: TMenuItem;
    mnHisto:    TMenuItem;
//mnSpidergram
    mnSpider: TMenuItem;
//mnSelection
    mnSymCol:    TMenuItem;
    mnLabel:     TMenuItem;
    mnSelection: TMenuItem;
    mnAuto:      TMenuItem;
    mnSymbol:    TMenuItem;
    mnColors:    TMenuItem;
    mnNoSymCol:  TMenuItem;

    mnConv:   TMenuItem;
    mnPpmOxy: TMenuItem;
    mnAtom:   TMenuItem;
    mnOxyPpm: TMenuItem;
    mnPermute:TMenuItem;
    mnStc:    TMenuItem;
    mnClay:   TMenuItem;
    mnFos:    TMenuItem;

    rgpNumDen: TRadioGroup;

    cbLogX: TCheckBox;
    cbLogY: TCheckBox;
    cbLogHis: TCheckBox;
    cbComplex: TCheckBox;
    cbAtomX: TCheckBox;
    cbAtomY: TCheckBox;
    cbAtomZ: TCheckBox;

    btnDraw: TButton;
    btnRead: TButton;
    edXHih: TEdit;
    edXLow: TEdit;
    edBrbX: TEdit;
    updoBrbX: TUpDown;
    edYHih: TEdit;
    edYLow: TEdit;
    edBrbY: TEdit;
    updoBrbY: TUpDown;
    Den_X: TEdit;
    Den_Y: TEdit;
    FactX: TEdit;
    FactY: TEdit;
    FactZ: TEdit;

    lbXMin: TLabel;
    lbXmax: TLabel;
    lbYMax: TLabel;
    lbYMin: TLabel;

    cbMuet: TCheckBox;
    lbList: TLabel;

    ChkListSelec: TCheckListBox;
    rgpComplex: TRadioGroup;
//  lbTitFil: TLabel;
    ChkListProfil: TCheckListBox;
    cbProfil: TCheckBox;
    cbNorm: TCheckBox;
    cbZeroOk: TCheckBox;
    lbY: TLabel;
    lbX: TLabel;
    btnSav: TButton;
    btnSavNew: TButton;
    updoEd2: TUpDown;
    updoEd4: TUpDown;
    lisBoxSpd: TListBox;
    mnTest: TMenuItem;
    cbTrgDown: TCheckBox;
    ShowSymbolsColors: TMenuItem;
    lb_Max: TLabel;
    lb_Min: TLabel;
    lb_Brb: TLabel;
    mnOutput: TMenuItem;
    mnEdit: TMenuItem;
    mnClipSingle: TMenuItem;
    mnClipLisXY: TMenuItem;
    mnClipX_Y: TMenuItem;
    mnAutomatic: TMenuItem;
    mnRHA: TMenuItem;

    procedure FormCreate(Sender: TObject);
    procedure ListBoxXClick(Sender: TObject);
    procedure mnQuitClick(Sender: TObject);
    procedure mnDesXYClick(Sender: TObject);
    procedure mnDesOutputClick(Sender: TObject);
    procedure mnDesTrgClick(Sender: TObject);
    procedure cbLogHisClick(Sender: TObject);
    procedure cbLogXClick(Sender: TObject);
    procedure cbLogYClick(Sender: TObject);
    procedure rgpNumDenClick(Sender: TObject);
    procedure btnDrawClick(Sender: TObject);
    procedure ListBoxYClick(Sender: TObject);
    procedure ListBoxZClick(Sender: TObject);
    procedure cbAtomXClick(Sender: TObject);
    procedure cbAtomYClick(Sender: TObject);
    procedure cbAtomZClick(Sender: TObject);
    procedure cbComplexClick(Sender: TObject);
    procedure mnPpmOxyClick(Sender: TObject);
    procedure ListBoxFieldClick(Sender: TObject);
    procedure cbMuetClick(Sender: TObject);
    procedure mnAutoClick(Sender: TObject);
    procedure mnSymbolClick(Sender: TObject);
    procedure mnColorsClick(Sender: TObject);
    procedure mnAtomClick(Sender: TObject);
    procedure mnLabelClick(Sender: TObject);
    procedure mnSelectionClick(Sender: TObject);
    procedure ChkListSelecClick(Sender: TObject);
    procedure rgpComplexClick(Sender: TObject);
    procedure btnReadClick(Sender: TObject);
    procedure mnPermuteClick(Sender: TObject);
    procedure mnProfilerClick(Sender: TObject);
    procedure ChkListProfilClick(Sender: TObject);
    procedure cbProfilClick(Sender: TObject);
    procedure cbNormClick(Sender: TObject);
    procedure btnSavClick(Sender: TObject);
    procedure btnSavNewClick(Sender: TObject);
    procedure mnNoSymColClick(Sender: TObject);
    procedure mnOxyPpmClick(Sender: TObject);
    procedure mnHistoClick(Sender: TObject);
    procedure mnFieldAutoClick(Sender: TObject);
    procedure mnStcClick(Sender: TObject);
    procedure cbZeroOkClick(Sender: TObject);
    procedure mnClayClick(Sender: TObject);
    procedure mnFosClick(Sender: TObject);
    procedure mnReloadClick(Sender: TObject);
    procedure mnSpiderClick(Sender: TObject);
    procedure lisBoxSpdClick(Sender: TObject);
    procedure mnTestClick(Sender: TObject);
    procedure cbTrgDownClick(Sender: TObject);
    procedure edBrbYChange(Sender: TObject);
    procedure updoBrbYClick(Sender: TObject; Button: TUDBtnType);
    procedure ShowSymbolsColorsClick(Sender: TObject);
    procedure mnOutputClick(Sender: TObject);
    procedure mnClipX_YClick(Sender: TObject);
    procedure mnClipSingleClick(Sender: TObject);
    procedure mnClipLisXYClick(Sender: TObject);
    procedure mnAutomaticClick(Sender: TObject);
    procedure mnRHAClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  MainForm: TMainForm;

implementation

uses
  LibIo, LibPtr, LibGrf, LibPs, LibStc, LibArg, LibFos,
  u_output, u_image, u_symcolix, u_edit;

{$R *.DFM}
const
 Debug=False;     
type
 TChoix =(ChVid,ChAut,ChSym,ChCol,ChTic,ChLab,ChSel);
 TDess = (NoDes,DesXY,DesTrg,DesPro,DesHis,DesSpd);
var
 Ps_Possible: boolean;
 Ps_Header: string;
 B_Excel: boolean;

 Dir_Def,Nom_FiX,Nom_FiY,Nom_FiS: string;
 FirstRec,LastRec,SpdRec: Rec_Ptr;
//Bitmap: TBitmap;
 B_Auto,B_Tic,B_Profil,OK_Pro,B_NormMax: Boolean;
 Spd_Count,Plot_Count: byte;
 Yes_Pro,Yes_Spd,Yes_EleX,Yes_EleY: ArrEle_Bool;
 Nb_Point: Word;

 ArChg: Array[Coor_XY] of Boolean;
 ArFrm: Array[CoorXYZ] of Forme;
 ArTit: Array[CoorXYZ] of string;
 ArTT: Array[CoorXYZ] of Str2;
 ArFact: Array[CoorXYZ] of Single;
 FrmList: Forme;

 NumDen: byte;   {=rgpNumDen.ItemIndex}
 CurChoi: TChoix;{(ChVid,ChAut,ChSym,ChTic,ChCol,ChLab,ChSel)}
 CurExp: byte;  {=rgpComplex.ItemIndex}
 CDes: TDess;    {(NoDes,DesXY,DesTrg,DesSpd)}

//Spider,Histo
 DimSpd, Nb_Spd: byte;
 Ree,Log_His:BOOLEAN;
 NomSpd: string;
//XY
 Barb_Auto: boolean;
 BarbX, BarbY: byte;
 SufX,SufY: string;
 XChekd,YChekd,ZChekd: boolean;

 Dim_Sel: byte;
 INum, IDen: Array[CoorXYZ] of Byte;

 C_Ps, N_Ps, I_Ps: byte;
 B_PsLabels,B_PsGreyScale,B_PsFrame,B_PsSymbol: boolean;
 B_Dual_XY,B_Samp_XY: boolean;

PROCEDURE PsPlot_Diag;
VAR
 fTrg: byte;
 x0,y0,xmm,ymm,xmm_,ymm_:integer;
 LowLef_X,LowLef_Y:integer;
 t,u: string;

 FUNCTION Plot_Name(Count:byte): string;
 VAR {f: Text; j: Byte;} s: string;
 BEGIN
  s:= chr(Count div 10 + Ord('0')) + chr(Count mod 10 + Ord('0'));
  CASE CDes OF
   DesXY: s:=s+ArTT[CorX]+ArTT[CorY];
   DesPro: s:=s+'PRO';
   DesSpd: s:=s+'SPD';
   DesHis: s:=s+'HIS';
   DesTrg: s:=s+ArTT[CorX]+ArTT[CorY]+ArTT[CorZ]
  END;
  Plot_Name:=s
 END;{Plot_Name}

 PROCEDURE Title;
 var t: string;
 BEGIN
  t:='___________';
(**
  IF B_Dual_XY
  THEN t:=ArFrm[CorX].Nom_Fil+' /X='+ Nom_FiX + '; Y='+ Nom_FiY
  ELSE t:=ArFrm[CorX].Nom_Fil+' /File=' + Nom_FiX;
**)
  Plot_String(0,ymm+2*y0{mm},6,t)
 END;

BEGIN{PsPlot_Diag}
 if B_PsGreyScale then Ps_Header:=Dir_Def+'_headergrey.ps'
 else Ps_Header:=Dir_Def+'_header.ps';

 while pos('.',SufX)<>0 do delete(SufX,length(SufX),1);
 while pos('.',SufY)<>0 do delete(SufY,length(SufY),1);

 xmm:=100;ymm:=100;
 Ps_Scale:=12;
 fTrg:=75;
 LowLef_X:=10;LowLef_Y:=140;

 u:='';//Nom_FiX;
 if u<>'' then while pos('.',u)<>0 do delete(u,length(u),1);
 x0:=10;y0:=10;//mm coordinates of 0,0 of diagram
 case C_Ps of
  1: begin
   N_Ps:=1;
   //xmm:=100;ymm:=100;Ps_Scale:=16;fTrg:=70;
   xmm:=100;ymm:=100;Ps_Scale:=12;fTrg:=70;
   LowLef_X:=10;
   LowLef_Y:=290-(round(ymm*Ps_Scale/10)+2*y0)
  end;
  2: begin
   N_Ps:=1;
   //xmm:=83;ymm:=83;Ps_Scale:=12;fTrg:=83;
   xmm:=70;ymm:=70;Ps_Scale:=10;fTrg:=100;
   //if (CDes=DesTrg) and B_TrgDown then LowLef_X:=5 + xmm div 2 else
   LowLef_X:=10;
   LowLef_Y:=290 - I_Ps*(round(ymm*Ps_Scale/10)+2*y0+10)
  end;
  3: begin
   N_Ps:=1;
   xmm:=80;ymm:=60;
   Ps_Scale:=12;
   LowLef_X:=10;
   LowLef_Y:=290 - I_Ps*(round(ymm*Ps_Scale/10)+2*y0);
   fTrg:=80;
  end;
  4: begin
   N_Ps:=6;
   xmm:=70;ymm:=70;
   Ps_Scale:=10;
   fTrg:=100;
   if odd(I_Ps) then LowLef_X:=5
   else LowLef_X:=5+(round(xmm*Ps_Scale/10)+2*x0);
   LowLef_Y:=290 - ((I_Ps-1) div 2 + 1)*(round(ymm*Ps_Scale/10)+2*y0)
  end
 end;
 xmm_:=round(xmm*Ps_Scale/10);ymm_:=round(ymm*Ps_Scale/10);

//labels only
 IF (N_Ps=1) AND (CDes IN [DesXY,DesTrg]) THEN BEGIN
  IF Plot_Open
(Ps_Header,u,'lab_'+Plot_Name(Plot_Count),LowLef_X,LowLef_Y,'_','_','_',Lis_SelX)
  THEN BEGIN
   //Plot_ZeroCadre(0,0,xmm_+2*x0,ymm_+2*y0,10);
   Plot_ZeroCadre(0,0,xmm_+2*x0,ymm_+2*y0);
   Plot_Labels(true,x0,y0,xmm,ymm,CDes=DesTrg,1.1,1.1{Tol_X,Tol_Y},Nb_Point);
   Plot_Close
  END
 END;

//color/symbols only
 IF (N_Ps=1) AND (CDes IN [DesXY,DesTrg]) THEN BEGIN
  IF Plot_Open
(Ps_Header,u,'leg_'+Plot_Name(Plot_Count),LowLef_X,LowLef_Y,'_','_','_',Lis_SelX)
  THEN BEGIN
   Plot_SymCol(5,ymm_+2*y0-10,20,B_Auto,B_Tic,I_Symb>0,I_Coul>0);
   Plot_Close
  END
 END;

//whole drawing
 IF Plot_Open
 (Ps_Header,u,Plot_Name(Plot_Count),
  LowLef_X,LowLef_Y,
  '_','_','_',Lis_SelX)
 THEN BEGIN
//Title
  //if N_Ps<>1 then
  Plot_ZeroCadre(0,0,xmm_+2*x0,ymm_+2*y0);
  CASE CDes OF
   DesTrg: BEGIN
    Plot_TrgCadre(fTrg,x0,y0,xmm_,ArTit[CorX],ArTit[CorY],ArTit[CorZ]);
    Plot_Dessin(TRUE,1,1{Tol_X,Tol_Y},Nb_Point);
    //if B_PsLabels then
     //Plot_Labels(false,x0,y0,xmm_,ymm_,True,1.1,1.1,Nb_Point);
   END;
   DesXY: BEGIN
    Plot_TolCadre(false,B_PsFrame,x0,y0,xmm,ymm,BarbX,BarbY,ArTit[CorX]+SufX,ArTit[CorY]+SufY,1,1{Tol_X,Tol_Y});{1,1,8,8,}
    Plot_Dessin(FALSE{Trg},1.1,1.1{Tol_X,Tol_Y},Nb_Point);
    //if B_PsLabels then
     //Plot_Labels(false,x0,y0,xmm_,ymm_,False,1.1,1.1,Nb_Point);
   END;
   DesPro: BEGIN
    Plot_LabelsPro(xmm_+2*x0+5,ymm_+2*y0-5,10,ArFrm[CorY],Yes_Pro);
    Plot_TolCadre(false,B_PsFrame,x0,y0,xmm,ymm,BarbX,BarbY,ArTit[CorX],''{ArTit[CorY]},1,1);
    Plot_Profil(x0,y0,xmm,ymm,1,1.1{Tol_X,Tol_Y},Nb_Point);
    Plot_Dessin(FALSE{Trg},1,1{Tol_X,Tol_Y},Nb_Point);
   END;
   DesHis: BEGIN
    Plot_TolCadre(false,B_PsFrame,x0,y0,xmm,ymm,BarbX,BarbY,ArTit[CorX],'Nr.Cases',1,1{Tol_X,Tol_Y});{1,1,8,8,}
    Plot_Dessin(FALSE{Trg},1,1{Tol_X,Tol_Y},Nb_Point);
    Plot_LabSpd(True,xmm_+2*x0+5,y0,Nb_Spd)
   END;
   DesSpd: BEGIN
    t:='Spd='+NomSpd+'/'+Lis_SelX;
    IF (N_Ps<=3) THEN Plot_String({xmm_+2*x0+}5,ymm_+2*y0-5,8,t);
    Plot_TolCadre(TRUE,TRUE,x0,y0,xmm,ymm,0,1,'','',1,1);
    Plot_TitSpd(FrmSpd,PrmSpd);
    Plot_Profil(x0,y0,xmm_,ymm_,1,1.1{Tol_X,Tol_Y},Nb_Point);
    if not B_PsSymbol then Plot_Dessin(FALSE{Trg},1,1{Tol_X,Tol_Y},Nb_Point);
    if not B_Auto then Plot_LabSpd(False,xmm_+2*x0+5,ymm_+2*y0-5,Nb_Spd)
   END;
  END;
  Plot_End;
  if I_Ps=N_Ps then begin Plot_Close; I_Ps:=0 end
  //Plot_Close; I_Ps:=0
 END
END; {PsPlot_Diag}

procedure Show_Bornes(C:Coor_XY;Ch:char;e1,e2:TEdit;lbMax,lbMin:TLabel);
begin
 lbMax.Caption:=Real_Str(P_Max[C]);
 lbMin.Caption:=Real_Str(P_Min[C]);
 if Log[C] then begin
  e1.Text:=Real_Str(Log_Max[c]); e2.Text:=IntToStr(Log_Mod[c]);
  e1.Hint:=Ch+' MAXI'; e2.Hint:=Ch+' MODULES';
  end
  else begin
  e1.Text:=Real_Str(P_Hih[c]); e2.Text:=Real_Str(P_Low[c]);
  e1.Hint:=Ch+' MAXI'; e2.Hint:='='+Ch+' MINI';
 end
end;{Show_Bornes}

procedure Show_ListBox(YesEle:ArrEle_Bool; LisBox:TListBox; C:CoorXYZ);
var i: byte;
begin
 LisBox.Clear;
 if ArExp[C].Atomic then with ArAtm[C] do
  for i:= 1 to Nb_Ele do if YesEle[i] then LisBox.Items.Add(Tit_Ele[i])
  else LisBox.Items.Add('__')
 else with ArFrm[C] do
  for i:= 1 to Nb_Ele do if YesEle[i] then LisBox.Items.Add(Tit_Oxy[i])
  else LisBox.Items.Add('__')
end;{Show_ListBox}

procedure Show_SymColControls;
begin
 with MainForm do begin
  mnSymCol.Visible:=CDes in [DesXY,DesTrg,DesSpd,DesPro,DesHis];
  mnSelection.Visible:=CDes in [DesXY,DesTrg,DesSpd,DesPro,DesHis];
  ListBoxField.Visible:=CDes in [DesXY,DesTrg,DesSpd,DesPro,DesHis];
  lbList.Visible:=ListBoxField.Visible;
  lisBoxSpd.Visible:=CDes=DesSpd;
  mnAuto.Visible:=CDes in [DesXY,DesTrg,DesSpd];
  mnAutomatic.Visible:=CDes in [DesXY,DesTrg,DesSpd];
  mnSymbol.Visible:=CDes in [DesXY,DesTrg];
  mnColors.Visible:=CDes in [DesXY,DesTrg];
  mnNoSymCol.Visible:=CDes in [DesXY,DesTrg,DesSpd];
  mnLabel.Visible:=CDes in [DesXY,DesTrg,DesSpd];
 end
end;{Show_SymColControls}

procedure Show_ChkListProfil;
var i: byte;
begin
 with MainForm do begin
  ChkListProfil.clear;
  CASE CDes OF
   DesPro: BEGIN
    if ArExp[CorY].Atomic then with ArAtm[CorY] do
     for i:= 1 to Nb_Ele do if Yes_EleY[i] then ChkListProfil.Items.Add(Tit_Ele[i])
     else ChkListProfil.Items.Add('__')
    else with ArFrm[CorY] do
     for i:= 1 to Nb_Ele do if Yes_EleY[i] then ChkListProfil.Items.Add(Tit_Oxy[i])
     else ChkListProfil.Items.Add('__')
   END;
   DesSpd: BEGIN
     with FrmSpd do for i:= 1 to Nb_Ele do
     if PrmSpd[i]<>0 then ChkListProfil.Items.Add(Tit_Oxy[i]+' ='+Real_Str(VecSpd[i]))
   END
  END{CASE}
 end {with MainForm}
end;{Show_ChkListProfil}

procedure Show_ArFact;
begin
 with MainForm do begin
//  FactX.Visible:=(not B_Complex)and(CDes in [DesTrg]);
//  FactY.Visible:=(not B_Complex)and(CDes in [DesTrg]);
//  FactZ.Visible:=(not B_Complex)and(CDes=DesTrg);
  FactX.Visible:=ListBoxX.Visible
  and ((not B_Complex) OR (B_Complex and (NumDen=0)));
  FactY.Visible:=ListBoxY.Visible
  and ((not B_Complex) OR (B_Complex and (NumDen=0)));
  FactZ.Visible:=ListBoxZ.Visible
  and ((not B_Complex) OR (B_Complex and (NumDen=0)));
  FactX.text:=Real_Str(ArFAct[CorX]);
  FactY.text:=Real_Str(ArFAct[CorY]);
  FactZ.text:=Real_Str(ArFAct[CorZ]);
 end
end;

procedure Show_XYZControls;
var B1,B1H,B2,B2H: Boolean;
begin
{(NoDes,DesXY,DesTrg,Spd)}
 B1:=CDes in [DesTrg,DesXY];
 B2:=CDes in [DesTrg,DesXY,DesPro];
 B1H:=CDes in [DesTrg,DesXY,DesHis];
 B2H:=CDes in [DesTrg,DesXY,DesPro,DesHis];
{}
 if CDes=DesTrg then begin
  Log[Cor_X]:=False; Log[Cor_Y]:=False
 end;
 Show_SymColControls;
 Show_ArFact;
 if CDes in [DesPro,DesSpd] then Show_ChkListProfil;

 with MainForm do begin
  Caption:=ArFrm[CorX].Nom_Fil;
  if CDes=DesSpd then Caption:='spd='+NomSpd;

  ListBoxSelec.visible:=False;
  ChkListSelec.visible:=False;

  ChkListProfil.visible:=(CDes in [DesPro,DesSpd]);
  cbProfil.Visible:=(CDes=DesPro);
  cbProfil.Checked:=B_Profil;
  cbNorm.Visible:=(CDes=DesPro);
  cbNorm.Checked:=B_NormMax;

  cbMuet.visible:=B1; cbMuet.checked:=B1 and B_Muet;

  edXHih.Visible:=B2H;edXLow.Visible:=B2H;
  edYHih.Visible:=B2;edYLow.Visible:=B2;
  edBrbX.visible:=(CDes=DesXY);
  edBrbY.visible:=(CDes=DesXY);
  lb_Brb.visible:=(CDes=DesXY);
  lb_Max.Visible:=B2H;
  lb_Min.Visible:=B2H;

  lbXMax.visible:=B2H;lbXMin.visible:=B2H;
  lbYMax.visible:=B2;lbYMin.visible:=B2;
  lbX.visible:=B2H;
  lbY.visible:=B2;

  cbZeroOk.Visible:=B2H;
  cbZeroOk.Checked:=Nega_Is_Ok;

  ListBoxX.Visible:=(CDes<>NoDes);
  ListBoxY.Visible:=B1;
  ListBoxZ.Visible:=(CDes=DesTrg);
  Show_ArFact;
  
  cbAtomX.Visible:=B1 OR (CDes=DesHis); cbAtomY.Visible:=B2;
  cbAtomZ.Visible:=(CDes=DesTrg);
  cbAtomX.checked:=ArExp[CorX].Atomic;
  cbAtomY.checked:=ArExp[CorY].Atomic;
  cbAtomZ.checked:=ArExp[CorZ].Atomic;
  Cur_X.Visible:=B1H; Cur_Y.Visible:=B1;
  Cur_Z.Visible:=(CDes=DesTrg);
{log}
  cbLogX.Visible:=(CDes=DesXY);
  cbLogY.Visible:=(CDes in [DesXY,DesPro]);
  cbLogX.checked:=Log[Cor_X];
  cbLogY.checked:=Log[Cor_Y];
  cbLogHis.Visible:=(CDes=DesHis);
  cbLogHis.checked:=Log_His;
  updoEd2.Visible:=edXLow.Visible and cbLogX.checked;
  updoEd4.Visible:=edYLow.Visible and cbLogY.checked;
  updoBrbX.Visible:=edBrbX.Visible;
  updoBrbY.Visible:=edBrbY.Visible;

{TrgDown}
  cbTrgDown.Visible:=(CDes=DesTrg);
  cbTrgDown.checked:=B_TrgDown;

{labels des colonnes X, Y, Z}
  LabelX.Visible:=B2;
  LabelY.Visible:=B2;
  LabelZ.Visible:=(CDes=DesTrg);
{}
  cbComplex.Visible:=B1H;{B1H}
  cbComplex.Checked:=B_Complex;
  rgpNumDen.Visible:=(CDes IN [DesXY,DesHis]) and B_Complex;
  NumDen:=0; rgpNumDen.ItemIndex:=NumDen;
  rgpComplex.Visible:=B1 and B_Complex;{B1H}
  rgpComplex.ItemIndex:=0;
  Den_X.Visible:=(CDes IN [DesXY,DesHis]) and B_Complex;
  Den_Y.Visible:=(CDes=DesXY) and B_Complex;
  btnRead.Visible:=(CDes in [DesSpd,DesPro]);
  btnDraw.Visible:= False;
  btnSav.Visible:= False;
  btnSavNew.Visible:= False
  {cbPlus.Visible:=B_Complex; cbMinus.Visible:=B_Complex;}
 end;
 if B1 then begin
  Show_ListBox(Yes_EleX,MainForm.ListBoxX,CorX);
  Show_ListBox(Yes_EleY,MainForm.ListBoxY,CorY);
  if (CDes=DesTrg) then Show_ListBox(Yes_EleX,MainForm.ListBoxZ,CorZ)
 end;
 if (CDes=DesHis) THEN Show_ListBox(Yes_EleX,MainForm.ListBoxX,CorX);
end;{Show_XYZControls}

procedure Show_FilControls(B:Boolean);
begin
 with MainForm do begin
  Caption:=ArFrm[CorX].Nom_Fil;
//lbTitFil.Visible:=B;

  mnDiagrams.visible:=B;
  mnSpider.visible:=B;
  mnDesXY.visible:=B;
  mnDesTrg.visible:=B;
  mnProfiler.visible:=B and not B_Samp_XY;
  mnHisto.visible:=B;
  mnSymCol.Visible:=CDes in [DesXY,DesTrg,DesSpd];
  mnConv.visible:=B;// and not B_Samp_XY;
  mnStc.Visible:=B AND (pos('MINERA',ArFrm[CorX].Tit_Champ[2])>0);

  btnDraw.Visible:=False;
  btnSav.Visible:= False;
  btnSavNew.Visible:= False;
  ListBoxField.Visible:=CDes in [DesXY,DesTrg,DesSpd];
  lbList.Visible:=ListBoxField.Visible;
  ListBoxSelec.Visible:=False;
  ChkListSelec.visible:=False;
 end
end;{Show_FilControls}

procedure Ouvrir_File(Lir_XY,Lir_Samp,Lir_Y:Boolean;Fil_Nam:string;var Frm:Forme);
var
 i:byte;
 Fi:TextFile; OK:BOOLEAN;
begin
 B_Auto:=False; I_Symb:=0; I_Coul:=0;
 CurChoi:=ChVid;
 {$I-}AssignFile(Fi,Fil_Nam); Reset(Fi);{$I+}
 OK:=IOResult=0;
 IF NOT OK THEN ShowMessage('??? File Currently Open ???')
 ELSE BEGIN
  Text_To_Forme(Fi,Frm,Lir_Samp,OK);
  IF Lir_Samp AND (Frm.Nb_Champ>0) THEN OK:=TRUE;
  IF NOT OK THEN ShowMessage('??? Problem in Input File Format???')
  ELSE BEGIN
   IF Lir_XY OR Lir_Samp THEN BEGIN
    if Frm.Nom_Fil='' then Frm.Nom_Fil:='_';//Nom_FiX;
//  MainForm.Caption:=Frm.Nom_Fil;
    Show_FilControls(True);
  (****)
    MainForm.ListBoxField.Clear;
    with Frm do
     for i:= 1 to Nb_Champ do
      MainForm.ListBoxField.Items.Add(Frm.Tit_Champ[i]);

    MainForm.ListBoxX.Clear;
    with Frm do
     for i:= 1 to Nb_Ele do
      MainForm.ListBoxX.Items.Add(Frm.Tit_Oxy[i]);
    Free_DbLinkList(FirstRec,LastRec);
    Text_To_Ptr(Lir_Samp,Nega_Is_Ok,Fi,Frm,Yes_EleX,FirstRec,LastRec);
    Yes_EleY:=Yes_EleX
   END
   ELSE BEGIN {IF Lir_Y}
    TextY_To_PtrXY(Lir_Y,B_Samp_XY,Fi,ArFrm[CorX],Frm,Yes_EleY,FirstRec,LastRec);
    IF B_Samp_XY THEN BEGIN
     MainForm.ListBoxX.Clear;
     FOR i:= 1 TO Frm.Nb_Ele DO
      MainForm.ListBoxX.Items.Add(Frm.Tit_Oxy[i]);
     Yes_EleX:=Yes_EleY
    END
   END
  END;
  CloseFile(Fi)
 END;
 if OK and (Frm.Nb_Ele>1) and (Frm.Nb_Champ>1) then
 with MainForm do begin
  mnSpider.visible:=True;
  mnDesXY.visible:=True;
  mnDesTrg.visible:=True;
  mnConv.visible:=not B_Samp_XY;
  ListBoxField.Visible:=TRUE;
  lbList.Visible:=ListBoxField.Visible;
 end
end;{Ouvrir_File}

procedure Init_XYZ;
var C: CoorXYZ;
begin
 B_TrgDown:=FALSE;
 Log[Cor_X]:=False; Log[Cor_Y]:=False; Log_His:=FALSE;
 ArChg[Cor_X]:=True; ArChg[Cor_Y]:=True;
 FOR C:=CorX TO CorZ DO BEGIN
  ArFact[C]:=1;
  INum[C]:=1; IDen[C]:=1;
  ArExp[C]:=Def_Exp;
  Titr_Exp(ArExp[C],ArFrm[C],ArAtm[C]);
  ArTit[C]:=ArExp[C].Titre
 END;
 WITH MainForm DO BEGIN
  Cur_X.Text:=ArTit[CorX]; Cur_Y.Text:=ArTit[CorY]; Cur_Z.Text:=ArTit[CorZ];
  {Den_X.Text:=ArExp[CorX].Den_Tit; Den_Y.Text:=ArExp[CorY].Den_Tit}
 END;
 I_SelX:=0;
 B_Complex:=False; B_Muet:=false;
 OK_Pro:=FALSE; B_NormMax:=FALSE; B_Profil:=False;
 NumDen:=0;
end;{init_XYZ}

procedure Init_File;
var C: CoorXYZ;
begin
 IF B_Samp_XY THEN BEGIN
  ArFrm[CorZ]:=ArFrm[CorY];
  ArFrm[CorX]:=ArFrm[CorY]
 END
 ELSE BEGIN
  ArFrm[CorZ]:=ArFrm[CorX];
  IF NOT B_Dual_XY THEN ArFrm[CorY]:=ArFrm[CorX]
 END;
 FOR C:=CorX TO CorZ DO BEGIN
  ArExp[C]:=Def_Exp;
  Calc_Recatom(ArFrm[C],ArAtm[C]);
  Titr_Exp(ArExp[C],ArFrm[C],ArAtm[C]);
  ArTit[C]:=ArExp[C].Titre
 END;
 I_Label:=1;B_Auto:=FALSE;
 Init_XYZ
end;{Init_File}

procedure Read_LogFile;
var
 f: TextFile;
 s,s1,s2,s3: string;
 i:integer;
 Ok_Dat,Ok_Ps,Ok_Dir,Ok_Stc: boolean;
begin
//default: if no file names in log, then work with Test files
//B_Samp_XY:=TRUE; B_Dual_XY:=FALSE;
 B_Samp_XY:=FALSE; B_Dual_XY:=FALSE;
 Nom_FiS:='_colsym.sav';
 Nom_FiX:='_xy.sav';

 s1:=''; s2:=''; s3:='';

 Log_File:=Dir_Def+'_log.txt';
 AssignFile(F,Log_File);
 {$I-}Reset(F);{$I+}
 IF IoResult=0 THEN BEGIN
  readln(F,S);
  WHILE (S[1]<>'#') and (pos('END',UpperCase(S))<>1) DO BEGIN
   S:=UpperCase(S);
   if pos('RES=',s)=1 then begin
    delete(s,1,pos('=',s));
    i:=StrToInt(s);
    if (i>=400) and (i<=800) then begin
     BitW:=i; BitH:=i
    end
   end;
   if pos('LIS=',s)=1 then begin
    delete(s,1,pos('=',s));
    if s<>'' then s1:=s
   end;
   if pos('X=',s)=1 then begin
    delete(s,1,pos('=',s));
    if s<>'' then s2:=s
   end;
   if pos('Y=',s)=1 then begin
    delete(s,1,pos('=',s));
    if s<>'' then s3:=s
   end;
   readln(F,S)
  END;//while
  CloseFile(F)
 END;
 OK_Dat:=FALSE;OK_Ps:=FALSE;Ok_Dir:=FALSE;
 Dir_Dat:=Dir_Def+'DAT'+DirCar;
 Dir_Ps:=Dir_Def+'PS'+DirCar;
 Ok_Dat:=Test_Dir(Dir_Dat);
 Ok_Ps:=Test_Dir(Dir_Ps);
 Dir_Stc:=IncludeTrailingPathDelimiter(Dir_Dat+'STC');
 Ok_Stc:=Test_Dir(Dir_Stc);

 if (s1<>'') and (s2<>'') and (s3<>'') then begin
  if (s1<>s2) and (s2=s3) then begin
   B_Samp_XY:=TRUE; B_Dual_XY:=FALSE;
   Nom_FiS:=s1;
   Nom_FiX:=s2;
   MainForm.mnReload.Visible:=TRUE
  end;
  if (s1=s2) and (s2<>s3) then begin
   B_Dual_XY:=TRUE; B_Samp_XY:=FALSE;
   Nom_FiX:=s1;
   Nom_FiY:=s3;
   MainForm.mnReload.Visible:=TRUE
  end;
  if (s1=s2) and (s2=s3) then begin
   B_Dual_XY:=FALSE; B_Samp_XY:=FALSE;
   Nom_FiX:=s1;
   MainForm.mnReload.Visible:=TRUE
  end
 end
end;{Read_LogFile}

procedure EffaceImage(var p: TPaintBox);
begin
 with p do begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := clWhite;
  Canvas.Pen.Color   := clBlack;
  Canvas.Rectangle(0,0,Width,Height)
 end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var f: TextFile;
begin
 B_Excel:=TRUE;
 BitW:=530; BitH:=BitW;
 Dir_Def:=ExtractFilePath(ParamStr(0));
 I_Ps:=0; C_Ps:=2; N_Ps:=1; Ps_Open:=FALSE;
 B_PsLabels:=FALSE;
 B_PsGreyScale:=FALSE;
 B_PsSymbol:=FALSE;
 B_PsFrame:=TRUE;
 CDes:=NoDes;
 Nom_FiX:=''; Nom_FiY:=''; Nom_FiS:=''; Plot_Count:=0; Spd_Count:=0;
 B_Dual_XY:=False; B_Samp_XY:=False;
 FirstRec:=NIL; LastRec:=NIL;
 SufX:='';SufY:='';

 Read_LogFile;

 Ps_Header:=Dir_Def+'_header.ps';
 AssignFile(F,Ps_Header);
 {$I-}Reset(F);{$I+}
 Ps_Possible:=(IoResult=0);
 IF Ps_Possible THEN CloseFile(F);
 Ps_Header:=Dir_Def+'_headergrey.ps';
 AssignFile(F,Ps_Header);
 {$I-}Reset(F);{$I+}
 Ps_Possible:=(IoResult=0) and Ps_Possible;
 IF (IoResult=0) THEN CloseFile(F);

 SpdFile:=Dir_Def+'_spiders.txt';

 mnMain.Visible:= TRUE;
 lbList.visible:=TRUE;
 Show_FilControls(False);
 Show_XYZControls;
 mnQuit.visible:=TRUE;
end;{TMainForm.FormCreate}

procedure Save_FileNames(var f: textfile);
begin
 if B_Samp_XY then begin
  writeln(f,'LIS=',Nom_FiS);{samples}
  writeln(f,'X=',Nom_FiX);{X}
  writeln(f,'Y=',Nom_FiX);{Y}
 end

 else
 if B_Dual_XY then begin
  writeln(f,'LIS=',Nom_FiX);{samples}
  writeln(f,'X=',Nom_FiX);{X}
  writeln(f,'Y=',Nom_FiY);{Y}
 end
 else begin
  writeln(f,'LIS=',Nom_FiX);{samples}
  writeln(f,'X=',Nom_FiX);{X}
  writeln(f,'Y=',Nom_FiX);{Y}
 end
end;{Save_FileNames}

procedure TMainForm.mnQuitClick(Sender: TObject);
var F: TextFile;
begin
 if Ps_Open then Plot_Close;
 AssignFile(F,Log_File); Rewrite(F);
 WriteLn(F,'RES=',BitW);
 Save_FileNames(F);
 WriteLn(F,'#environment parameters and last files used');
 WriteLn(F,'#do not insert blank characters !!');
 CloseFile(F);
 close
end;

procedure Init_Spider(c_spd:byte);
var fi: TextFile; OK: Boolean; R: Rec_Ana; i: byte; LisSpd: string; W:word;
begin
 Nb_Spd:=0; Nb_Point:=0;
 W:=0;
 I_SelX:=0; I_Label:=1;
 CDes:=DesSpd;
 SpdRec:=FirstRec;
 Yes_Spd:=Arr_Vrai;
 FOR i:=0 TO Max_Ele DO VecSpd[i]:=1.0;
 FOR i:=0 TO Max_Ele DO PrmSpd[i]:= i;

//read spd format data from file
{$I-}
 AssignFile(Fi,SpdFile); Reset(Fi);
{$I+}
 for i:=1 to c_spd do begin
  readln(Fi,NomSpd);
  Skip_Rite([_t],NomSpd);
  Text_To_Forme(Fi,FrmSpd,FALSE,OK);
  Text_To_RecAna(False,Fi,FrmSpd,R,W,OK);
 end;
 CloseFile(Fi);

 SufSpd:=R.Champ[1];
 LisSpd:=FrmSpd.Lis_Ele;
 Ree:=pos('SMEUGD',LisSpd)<>0;

 Calc_Recatom(ArFrm[CorX],Def_Recatom);

 FOR i:=1 TO FrmSpd.Nb_Ele DO BEGIN
  IF R.Don[i]>0 THEN VecSpd[i]:=R.Don[i];
  PrmSpd[i]:= Position(2,Copy(LisSpd, 2*i-1, 2),ArFrm[CorX].Lis_Ele);
  IF PrmSpd[i]<>0
  THEN FrmSpd.Tit_Oxy[i]:=Def_Recatom.Tit_Ele[PrmSpd[i]]
  ELSE FrmSpd.Tit_Oxy[i]:= Copy(LisSpd, 2*i-1, 2)
 END;

 Log[Cor_X]:=False; Log[Cor_Y]:=True;
 P_Min[Cor_X]:=0;P_Hih[Cor_X]:=FrmSpd.Nb_Ele;
 Show_XYZControls;
end;{Init_Spider}

procedure TMainForm.mnDesXYClick(Sender: TObject);
begin
 if Ps_Open then Plot_Close;
 I_Ps:=0;
 XChekd:=false;YChekd:=false;ZChekd:=false;
 CDes:=DesXY;
 Init_XYZ;
 Show_XYZControls;
end;{TMainForm.mnDesXYClick}

procedure TMainForm.mnDesOutputClick(Sender: TObject);
begin
 if Ps_Open then Plot_Close;
 I_Ps:=0;
 with OutPutForm do begin
  edX.text:=SufX; edY.Text:=SufY;
  cbPsLabels.Checked:=B_PsLabels;
  cbPsGreyScale.Checked:=B_PsGreyScale;
  cbPsSymbol.Checked:=B_PsSymbol;
  cbPsFrame.Checked:=B_PsFrame;
  cbExcelView.Checked:=B_Excel;
  rgpDiagramSize.ItemIndex:=C_Ps-1;
 end;
 OutPutForm.ShowModal;
 C_Ps:=OutPutForm.rgpDiagramSize.ItemIndex+1;
 B_PsLabels:=OutPutForm.cbPsLabels.Checked;
 B_PsGreyScale:=OutPutForm.cbPsGreyScale.Checked;
 B_PsSymbol:=OutPutForm.cbPsSymbol.Checked;//for spiders
 B_PsFrame:=OutPutForm.cbPsFrame.Checked;
 B_Excel:=OutPutForm.cbExcelView.Checked;
 case C_Ps of
  1,2,3: N_Ps:=1;
  4: N_Ps:=6
 end;
 btnSav.Caption:='SAVE '+chr(I_Ps+Ord('0')+1)+'/'+chr(N_Ps+Ord('0'));
 SufX:=OutPutForm.edX.text; SufY:=OutPutForm.edY.Text;
end;

procedure TMainForm.mnDesTrgClick(Sender: TObject);
begin
 if Ps_Open then Plot_Close;
 I_Ps:=0;
 B_TrgDown:=FALSE;
 XChekd:=false;YChekd:=false;ZChekd:=false;
 CDes:=DesTrg;
 Init_XYZ;
 X0Trg:=0; X1Trg:=100; Y0Trg:=0; Y1Trg:=100;
 Show_XYZControls;
end;{TMainForm.mnDesTrgClick}

procedure TMainForm.mnProfilerClick(Sender: TObject);
begin
 if Ps_Open then Plot_Close;
 I_Ps:=0;
 CDes:=DesPro;
 BtnDraw.Visible:=FALSE;
 Yes_Pro:=Arr_Faux; B_NormMax:=False; OK_Pro:=FALSE;
 Init_XYZ;
 Show_XYZControls;
end;{TMainForm.mnProfilerClick}

procedure TMainForm.cbLogHisClick(Sender: TObject);
begin Log_His:= cbLogHis.checked end;

procedure TMainForm.cbLogXClick(Sender: TObject);
begin
 Log[Cor_X]:= cbLogX.checked;
 updoEd2.Visible:=Log[Cor_X];
 edBrbX.Visible:=not Log[Cor_X];
 updoBrbX.Visible:=not Log[Cor_X];
end;

procedure TMainForm.cbLogYClick(Sender: TObject);
begin
 Log[Cor_Y]:= cbLogY.checked;
 updoEd4.Visible:=Log[Cor_Y];
 edBrbY.Visible:=not Log[Cor_Y];
 updoBrbY.Visible:=not Log[Cor_Y];
end;

procedure TMainForm.cbTrgDownClick(Sender: TObject);
begin
 B_TrgDown:=cbTrgDown.Checked
end;

procedure TMainForm.rgpNumDenClick(Sender: TObject);
begin
 NumDen:=rgpNumDen.ItemIndex;
 CurExp:=0; rgpComplex.ItemIndex:=0;
 Show_ArFact
end;

procedure Input_Bornes;
begin
 with MainForm do CASE CDes OF
  DesTrg: begin
   updoEd2.Visible:=False; updoEd4.Visible:=False; edYHih.visible:=False;
   X0Trg:= Fun_Intg2(edXLow, 'X', X0Trg, 0, 100);
   X1Trg:= Fun_Intg2(edXHih, 'X', X1Trg, X0Trg, 100);
   Y0Trg:= Fun_Intg2(edYLow, 'Y', Y0Trg, 0, 100);
   Y1Trg:= Y0Trg + X1Trg - X0Trg;
  end;

  DesSpd: begin
   updoEd4.Visible:=True; edYHih.visible:=True; edYLow.visible:=True;
   P_Low[Cor_X]:=0;//P_Hih[Cor_X]:=FrmSpd.Nb_Ele;
   Log_Max[Cor_Y]:=Fun_Sing2(edYHih,'Y',Log_Max[Cor_Y],0.1,1E5);
   Log_Mod[Cor_Y]:=Fun_Intg2(edYLow,'Y',Log_Mod[Cor_Y],1,5);
  end;

  DesXY,DesPro,DesHis: begin
   edXHih.visible:=True; edXLow.visible:=True;
   edYHih.visible:=CDes<>DesHis; edYLow.visible:=CDes<>DesHis;
   if Log[Cor_X] then begin
     updoEd2.Visible:=True;
     Log_Max[Cor_X]:=Fun_Sing2(edXHih,'X',Log_Max[Cor_X],0.1,1E5);
     Log_Mod[Cor_X]:=Fun_Intg2(edXLow,'X',Log_Mod[Cor_X],1,5);
   end
   else begin
    updoEd2.Visible:=False;
    P_Hih[Cor_X]:=Fun_Sing2(edXHih,'X',P_Hih[Cor_X],-1E5,1E5);
    P_Low[Cor_X]:=Fun_Sing2(edXLow,'X',P_Low[Cor_X],-1E5,1E5);
    if ABS(P_Hih[Cor_X]-P_Low[Cor_X])<Epsilon
     then P_Hih[Cor_X]:=P_Low[Cor_X]+1;
    BarbX:=Fun_Intg2(edBrbX,'-',BarbX,1,50);
   end;

   if CDes<>DesHis then begin
    if Log[Cor_Y] then begin
     updoEd4.Visible:=True;
     Log_Max[Cor_Y]:=Fun_Sing2(edYHih,'Y',Log_Max[Cor_Y],0.1,1E5);
     Log_Mod[Cor_Y]:=Fun_Intg2(edYLow,'Y',Log_Mod[Cor_Y],1,5);
    end
    else begin
     updoEd4.Visible:=False;
     P_Hih[Cor_Y]:=Fun_Sing2(edYHih,'Y',P_Hih[Cor_Y],-1E5,1E5);
     P_Low[Cor_Y]:=Fun_Sing2(edYLow,'Y',P_Low[Cor_Y],-1E5,1E5);
     if ABS(P_Hih[Cor_Y]-P_Low[Cor_Y])<Epsilon
      then P_Hih[Cor_Y]:=P_Low[Cor_Y]+1;
     BarbY:=Fun_Intg2(edBrbY,'-',BarbY,1,50);
{
     edBrbY.Text:=IntToStr(Cal_Barb(P_Hih[Cor_Y] - P_Low[Cor_Y]));
     BarbY:= Cal_Barb(P_Hih[Cor_Y] - P_Low[Cor_Y]);
}
    end
   end
   else begin {CDes=DesHis}
    P_Hih[Cor_Y]:=100;P_Low[Cor_Y]:=0;
   end;

  end {DesXY,,DesPro,DesHis}

 end; {CASE CDes}
end;{Input_Bornes}

procedure Update_ValXYZ;
begin
 CASE CDes OF
  DesPro: IF OK_Pro THEN BEGIN
   Ptr_To_Pro
(FirstRec,ArFrm[CorY].Nb_Ele,B_Dual_XY,B_Samp_XY,B_NormMax,Yes_Pro,Nb_Point);
   Def_Bornes(Nega_Is_Ok,FALSE,Cor_X);
   Def_Bornes(Nega_Is_Ok,FALSE,Cor_Y);
   with MainForm do begin
    Show_Bornes(Cor_X,'X',edXHih,edXLow,lbXMax,lbXMin);
    Show_Bornes(Cor_Y,'Y',edYHih,edYLow,lbYMax,lbYmin)
   end
  END;//DesPro
  DesHis: BEGIN
   Ptr_To_Histo(FirstRec,B_Dual_XY,B_Samp_XY,Log_His,Nb_Spd,Nb_Point);
   with MainForm do Show_Bornes(Cor_X,'X',edXHih,edXLow,lbXMax,lbXMin);
  END;
  DesXY,DesTrg: BEGIN
   Ptr_To_XY(FirstRec,B_Dual_XY,B_Samp_XY,B_Auto,B_Tic,CDes=DesTrg,ArFrm[CorX].Nb_Champ,Nb_Point);
   IF ArChg[Cor_X] then Def_Bornes(Nega_Is_Ok,CDes=DesTrg,Cor_X);
   IF ArChg[Cor_Y] then Def_Bornes(Nega_Is_Ok,CDes=DesTrg,Cor_Y);
   with MainForm do begin
    Show_Bornes(Cor_X,'X',edXHih,edXLow,lbXMax,lbXMin);
    Show_Bornes(Cor_Y,'Y',edYHih,edYLow,lbYMax,lbYmin)
   end
  END
 END
end;{Update_ValXYZ}

procedure TMainForm.btnReadClick(Sender: TObject);
begin
 CASE CDes OF

  DesSpd: BEGIN
   lisBoxSpd.Visible:=false;
   Ptr_To_Spider(SpdRec,B_Auto,B_Tic,B_Dual_XY,B_Samp_XY,Ree,ArFrm[CorX],Yes_Spd,Nb_Spd,Nb_Point);
   IF SpdRec=NIL THEN btnRead.Visible:=False;
   IF Nb_Spd>0 THEN BEGIN
    Def_Bornes(Nega_Is_Ok,False,Cor_Y);
    edYHih.Visible:=TRUE;edYLow.Visible:=TRUE;
    lbYMax.visible:=True;lbYMin.visible:=True;
    Show_Bornes(Cor_Y,'Y',edYHih,edYLow,lbYMax,lbYmin);
    Input_Bornes;
    btnDraw.Visible:=TRUE
   END
  END;{DesSpd}

  DesPro: IF OK_Pro THEN BEGIN
   Update_ValXYZ;
   if Nb_Point>0 then btnDraw.Visible:=TRUE
  END

 END//CASE
end;//TMainForm.btnReadClick

procedure TMainForm.btnDrawClick(Sender: TObject);
var I_Spd:BYTE;
begin
 while pos('.',SufX)<>0 do delete(SufX,length(SufX),1);
 while pos('.',SufY)<>0 do delete(SufY,length(SufY),1);
 with ImageForm do begin
  Image.Width:=BitW; Image.Height:=BitH;
  if CDes=DesSpd then Caption:=NomSpd+' '+SufSpd
 end;
 ImageForm.Show;
 EffaceImage(ImageForm.Image);
 CASE CDes OF
  DesSpd: if Nb_Point>2 then begin
   Input_Bornes;
   if Nb_Point>2 then begin
    Init_Grf(30,30,30,30);
    Cad_Spd(ImageForm.Image.Canvas,PrmSpd,FrmSpd);
    Des_Crt(ImageForm.Image.Canvas,True,False,False,False,Nb_Point,4);
   end;

   EffaceImage(SymColForm.Image);
   SymColForm.Show;
   if not (B_Auto or B_Tic) then for I_Spd:= 1 to Nb_Spd do
    Label_Spd(SymColForm.Image.Canvas,I_Spd,ArLabSpd[I_Spd])
    //else Des_SymCol(SymColForm.Image.Canvas,true,false,false)
  end;

  DesTrg: begin
   Input_Bornes;
   Init_Grf(5,5,35,25);
   TrgCad_Crt(ImageForm.Image.Canvas,True,ArTit[CorX],ArTit[CorY],ArTit[CorZ],'');
   Des_Crt(ImageForm.Image.Canvas,False,False,TRUE,False,Nb_Point,4);
  end;

  DesXY: BEGIN
   Input_Bornes;
   Init_Grf(30,30,30,30);
   Cad_Crt(ImageForm.Image.Canvas,True,ArTit[CorX]+SufX,ArTit[CorY]+SufY,'',BarbX,BarbY);
   Des_Crt(ImageForm.Image.Canvas,False,False,False,True,Nb_Point,4);
   ArChg[Cor_X]:=False; ArChg[Cor_Y]:=False;
  end;

  DesHis: IF Nb_Point>0 THEN BEGIN
   Input_Bornes;
   BarbY:= 10;
   BarbX:= Cal_Barb(P_Hih[Cor_X] - P_Low[Cor_X]);
   Init_Grf(30,30,30,30);
   Cad_Crt(ImageForm.Image.Canvas,True,ArTit[CorX],'Nr.Cases','',BarbX,BarbY);
   Des_Crt(ImageForm.Image.Canvas,False,False,False,True,Nb_Point,4);

   EffaceImage(SymColForm.Image);
   SymColForm.Show;
   for I_Spd:= 1 to Nb_Spd do
    Label_Histo(SymColForm.Image.Canvas,I_Spd,ArLabSpd[I_Spd]);

   CurExp:=0; rgpComplex.ItemIndex:=0;
   ArChg[Cor_X]:=False; ArChg[Cor_Y]:=False;
  end;

  DesPro: BEGIN
   Input_Bornes;
   BarbX:= Cal_Barb(P_Hih[Cor_X] - P_Low[Cor_X]);
   BarbY:= Cal_Barb(P_Hih[Cor_Y] - P_Low[Cor_Y]);
   Init_Grf(30,30,30,30);
   B_Muet:=TRUE;
   B_PsLabels:=FALSE;
// B_PsGreyScale:=FALSE;
   B_PsFrame:=TRUE;
   Cad_Crt(ImageForm.Image.Canvas,True,ArTit[CorX],'',{ArTit[CorY],}'',BarbX,BarbY);
   Des_Crt(ImageForm.Image.Canvas,B_Profil,False,False,True,Nb_Point,4);

   EffaceImage(SymColForm.Image);
   SymColForm.Show;
   Label_Profil(SymColForm.Image.Canvas,ArFrm[CorY],Yes_Pro);
  end
 end;

 if (CDes in [DesXY,DesTrg])
 and (B_Auto or B_Tic or (I_Symb>0) or (I_Coul>0))
 THEN BEGIN
// SymColForm.Image.Width:=80;
  EffaceImage(SymColForm.Image);
  SymColForm.Show;
  Des_SymCol(SymColForm.Image.Canvas,B_Auto,B_Tic,I_Symb>0,I_Coul>0)
//IF B_Auto THEN Des_SymCol(SymColForm.Image.Canvas,true,false,false,false)
//ELSE IF (I_Symb>0) or (I_Coul>0) THEN
// Des_SymCol(SymColForm.Image.Canvas,False,False,I_Symb>0,I_Coul>0);
 end;

 btnSav.Caption:='SAVE '+chr(I_Ps+Ord('0')+1)+'/'+chr(N_Ps+Ord('0'));
 btnSav.Visible:=Ps_Possible;
 if I_Ps>0 then btnSavNew.Visible:= true
end;{TMainForm.btnDrawClick}

PROCEDURE Update_Exp_Tit(C:CoorXYZ; i:byte; var snum,sden:TEdit);
BEGIN
 WITH ArExp[C] DO BEGIN
  IF NumDen=0 THEN BEGIN
   IF CurExp=0 THEN Num_Trm:=1
   ELSE IF Num_Trm<Max_Trm THEN INC(Num_Trm);
   IF CurExp=2 THEN Num_Kof[Num_Trm]:=-ArFact[C] ELSE Num_Kof[Num_Trm]:=ArFact[C];
   Num_Ind[Num_Trm]:=i
  END;
  IF NumDen=1 THEN BEGIN
   IF CurExp=0 THEN Den_Trm:=1
   ELSE IF Den_Trm<Max_Trm THEN INC(Den_Trm);
   IF CurExp=2 THEN Den_Kof[Den_Trm]:=-1 ELSE Den_Kof[Den_Trm]:=1;
   Den_Ind[Den_Trm]:=i
  END;
 END;

 Titr_Exp(ArExp[C],ArFrm[C],ArAtm[C]);
 ArTit[C]:=TrimLeft(ArExp[C].Titre);
 IF (CDes=DESHis) AND Log_His THEN ArTit[CorX]:='LOG '+ArTit[CorX];
 ArTT[c]:=ArAtm[C].Tit_Ele[ArExp[C].Num_Ind[1]];
 IF ArTT[c][2]='.' THEN ArTT[c][2]:='_';

 snum.Text:=ArExp[C].Num_Tit;
 if (C<>CorZ) then
  if B_Complex then sden.Text:=ArExp[C].Den_Tit else sden.Text:=''
END;{Update_Exp_Tit}

PROCEDURE Update_Tit(C:CoorXYZ; var snum,sden:TEdit);
BEGIN
 Titr_Exp(ArExp[C],ArFrm[C],ArAtm[C]);
 ArTit[C]:=ArExp[C].Titre;
 ArTT[c]:=ArAtm[C].Tit_Ele[ArExp[C].Num_Ind[1]];
 IF ArTT[c][2]='.' THEN ArTT[c][2]:='_';

 snum.Text:=ArExp[C].Num_Tit;
 if (C<>CorZ) then
  if B_Complex then sden.Text:=ArExp[C].Den_Tit else sden.Text:=''
END;{Update_Tit}

procedure TMainForm.ListBoxXClick(Sender: TObject);
var i: byte;
begin
 btnSav.Visible:=False;
 btnSavNew.Visible:= False;
 i:=ListBoxX.ItemIndex;
 IF NumDen=0 THEN Cur_X.Text:=ListBoxX.Items.Strings[i];
 IF NumDen=1 THEN Den_X.Text:=ListBoxX.Items.Strings[i];
// IF (not B_Complex) and(CDes=DesTrg) THEN
 ArFact[CorX]:=Fun_Sing2(FactX,'X',1,0,10000);
 IF CDes=DesPro THEN BEGIN
  Sort_DbLinkList(0,i+1,FirstRec,LastRec);
  Update_Exp_Tit(CorX,i+1,Cur_X,Den_X);
 END
 ELSE BEGIN
  Update_Exp_Tit(CorX,i+1,Cur_X,Den_X);
  ArChg[Cor_X]:=TRUE;
  Update_ValXYZ
 END;
 XChekd:=TRUE;
 IF CDes=DesHis THEN btnDraw.Visible:=TRUE;
 IF CDes=DesXY THEN btnDraw.Visible:=YChekd;
 IF CDes=DesTrg THEN btnDraw.Visible:=YChekd AND ZChekd;
end;{ListBoxXClick}

procedure TMainForm.ListBoxYClick(Sender: TObject);
var i: byte;
begin
 btnSav.Visible:=False;
 btnSavNew.Visible:= False;
 i:=ListBoxY.ItemIndex;
 IF NumDen=0 THEN Cur_Y.Text:=ListBoxY.Items.Strings[i];
 IF NumDen=1 THEN Den_Y.Text:=ListBoxY.Items.Strings[i];
// IF (not B_Complex) and(CDes=DesTrg) THEN
 ArFact[CorY]:=Fun_Sing2(FactY,'Y',1,0,10000);
 Update_Exp_Tit(CorY,i+1,Cur_Y,Den_Y);
 ArChg[Cor_Y]:=TRUE;
 Update_ValXYZ;
 YChekd:=TRUE;
 IF CDes=DesXY THEN btnDraw.Visible:=XChekd;
 IF CDes=DesTrg THEN btnDraw.Visible:=XChekd AND ZChekd;
end;{ListBoxYClick}

procedure TMainForm.ListBoxZClick(Sender: TObject);
var i: byte; e: TEdit;
begin
 btnSav.Visible:=False;
 btnSavNew.Visible:= False;
 i:=ListBoxZ.ItemIndex;
 Cur_Z.Text:=ListBoxZ.Items.Strings[i];
// IF (not B_Complex) {and(CDes=DesTrg)} THEN
 ArFact[CorZ]:=Fun_Sing2(FactZ,'Z',1,0,10000);
 Update_Exp_Tit(CorZ,i+1,Cur_Z,e);
 Update_ValXYZ;
 ZChekd:=TRUE;
 btnDraw.Visible:=YChekd AND XChekd;
end;{ListBoxZClick}

procedure TMainForm.cbAtomXClick(Sender: TObject);
begin
 CurExp:=0;
 rgpComplex.ItemIndex:=0;
 ArFact[CorX]:=1;INum[CorX]:=1; IDen[CorX]:=1;ArExp[CorX]:=Def_Exp;
 ArExp[CorX].Atomic:=cbAtomX.checked;
 Show_ListBox(Yes_EleX,MainForm.ListBoxX,CorX);
 Update_Tit(CorX,Cur_X,Den_X);
 Update_ValXYZ
end;

procedure TMainForm.cbAtomYClick(Sender: TObject);
begin
 CurExp:=0;
 rgpComplex.ItemIndex:=0;
 ArFact[CorY]:=1;INum[CorY]:=1;IDen[CorY]:=1;ArExp[CorY]:=Def_Exp;
 ArExp[CorY].Atomic:=cbAtomY.checked;
 CASE CDes OF
  DesXY,DesTrg: begin
   Show_ListBox(Yes_EleY,MainForm.ListBoxY,CorY);
   Update_Tit(CorY,Cur_Y,Den_Y);
   Update_ValXYZ
  end;
  DesPro: Show_ChkListProfil
 END
end;

procedure TMainForm.cbAtomZClick(Sender: TObject);
var e: TEdit;
begin
 CurExp:=0;
 rgpComplex.ItemIndex:=0;
 ArFact[CorZ]:=1;INum[CorZ]:=1;IDen[CorZ]:=1;ArExp[CorZ]:=Def_Exp;
 ArExp[CorZ].Atomic:=cbAtomZ.checked;
 Show_ListBox(Yes_EleX,MainForm.ListBoxZ,CorZ);
 Update_Tit(CorZ,Cur_Z,e);
 Update_ValXYZ
end;

procedure TMainForm.cbComplexClick(Sender: TObject);
var C:CoorXYZ;
begin
 B_Complex:=cbComplex.checked;
 rgpComplex.Visible:=B_Complex;
 rgpNumDen.visible:= B_Complex and (CDes IN [DesXY,DesHis]);
 Den_X.Visible:=B_Complex and (CDes IN [DesXY,DesHis]);
 Den_Y.Visible:=B_Complex and (CDes=DesXY);
 if not B_Complex then begin
  CurExp:=0; rgpComplex.ItemIndex:=0;
  for C:=CorX to CorZ do begin
   ArExp[C].num_Trm:=1;ArExp[C].Den_Trm:=0
  end;
  NumDen:=0; rgpNumDen.ItemIndex:=0
 end;
 if B_Complex then FOR C:=CorX TO CorZ DO BEGIN
  ArFact[C]:=1;
  ArExp[C].Num_Kof[1]:=1
 END;
 Show_ArFact;
 Update_Tit(CorX,Cur_X,Den_X);
 Update_Tit(CorY,Cur_Y,Den_Y);
 Update_ValXYZ
end;{TMainForm.cbComplexClick}

PROCEDURE Cnv_File(t:string;Cnv:ModCnv);
var
 Fo: TextFile; FrmCnv: Forme;
 PrmCnv: ArrEle_Byte;
 CurRec: Rec_Ptr;
 R: Rec_Ana; i: byte;
 s: string;
begin
  s:= Lis_52;
  //if Ok_Dat then begin
  AssignFile(Fo,Dir_Dat+t+'.txt'); Rewrite(Fo);
  if Cnv in [Oxy,Atm,Rha] then Calc_RecAtom(ArFrm[CorX],ArAtm[CorX]);
  FrmCnv:=ArFrm[CorX];
  if Cnv = Prm then Ent_Prm2(ArFrm[CorX],FrmCnv,s,PrmCnv);
  CASE Cnv OF
   Oxy: FrmCnv.Tit_Oxy:=ArAtm[CorX].Tit_Ox;
   Atm,Met,Rha: BEGIN
    FOR i:=1 TO ArFrm[CorX].Nb_Ele DO
     FrmCnv.Tit_Oxy[i]:=ArAtm[CorX].Tit_Ele[i]
   END
  END;
  Forme_To_Text(Fo, frmCnv);
  CurRec:=FirstRec;
  WHILE CurRec<>NIL DO BEGIN
   R:=CurRec^.Rec;
   if Cnv in [Oxy,Atm,Met,Rha]
     then Cnv_Vec(Cnv,ArAtm[CorX],FrmCnv.Nb_Ele,R.Don);
   if Cnv=Prm then Prm_Vec(PrmCnv,R.Don);
   if Cnv=Rha then Rha_Vec(Fo,FrmCnv,16,R)
   else RecAna_To_Text(Fo,FrmCnv,Arr_Vrai,R);
   CurRec:= CurRec^.Next
  END;{WHILE CurRec<>Nil}
  CloseFile(Fo);
  IF B_Excel THEN ShellExecute(0,'OPEN','Excel.exe',PChar(Dir_Dat+t+'.txt'),Nil, SW_SHOW);
// end
END;{Cnv_File}

procedure TMainForm.mnRHAClick(Sender: TObject);
begin Cnv_File('RHA',Rha) end;

procedure TMainForm.mnPermuteClick(Sender: TObject);
begin Cnv_File('PRM',Prm) end;

procedure TMainForm.mnPpmOxyClick(Sender: TObject);
begin Cnv_File('OXY',Oxy) end;

procedure TMainForm.mnOxyPpmClick(Sender: TObject);
begin Cnv_File('PPM',Met) end;

procedure TMainForm.mnAtomClick(Sender: TObject);
begin Cnv_File('ATM',Atm) end;

procedure TMainForm.ListBoxFieldClick(Sender: TObject);
var i, j: byte; s: string;
begin
 lisBoxSpd.Visible:=FALSE;
 i:=ListBoxField.ItemIndex + 1;
 s:=ArLisChamp[i];
 I_Tic:=0;
 if CurChoi in [ChSym,ChCol,ChTic,ChLab,ChAut] then begin
  ListBoxSelec.visible:=TRUE;
  ChkListSelec.visible:=False;
  ListBoxSelec.clear;
  for j:=1 to length(s) div Signif do
   ListBoxSelec.Items.Add(copy(s,(j-1)*signif+1,signif));
  CASE CurChoi OF
   ChTic: begin
    I_Tic:=i; LisAutomatic:=ArLisChamp[i];
    I_Symb:=0; I_Coul:=0; B_Tic:=true;
//  IF I_Symb<FrmList.Nb_Champ THEN I_SymbLeg:=I_Symb+1 ELSE I_SymbLeg:=I_Symb;
    B_Auto:=False
   end;
   ChSym: begin
    I_Symb:=i;Lis_Symb:=ArLisChamp[i];
    IF I_Symb<FrmList.Nb_Champ THEN I_SymbLeg:=I_Symb+1
    ELSE I_SymbLeg:=I_Symb;
    B_Auto:=False; B_Tic:=False
   end;
   ChCol: begin
    I_Coul:=i;Lis_Coul:=ArLisChamp[i];
    IF I_Coul<FrmList.Nb_Champ THEN I_CoulLeg:=I_Coul+1
    ELSE I_CoulLeg:=I_Coul;
    B_Auto:=False; B_Tic:=False
   end;
   ChAut: begin
    I_SymCol:=i;LisAutoSymCol:=ArLisChamp[i];
    I_Symb:=0; I_Coul:=0;
    IF I_SymCol<FrmList.Nb_Champ THEN I_Legend:=I_SymCol+1
    ELSE I_Legend:=I_SymCol;
    B_Auto:=True; B_Tic:=False
   end;
   ChLab: I_Label:= i
  END;
  if CDes in [DesXY,DesTrg] then Update_ValXYZ;
  if CDes=DesSpd then begin
   SpdRec:=FirstRec;
   btnRead.Visible:=TRUE
  end
 end;
 if CurChoi=ChSel then begin
  ListBoxSelec.visible:=FALSE;
  ChkListSelec.visible:=TRUE;
  ChkListSelec.clear;
  for j:=1 to length(s) div Signif do
   ChkListSelec.Items.Add(copy(s,(j-1)*signif+1,signif));
  Dim_Sel:=length(s) div Signif;
  I_SelX:=i;
  Lis_SelX:=s;
  if CDes in [DesPro,DesXY,DesTrg,DesHis] then Update_ValXYZ;
  if CDes=DesSpd then begin
   SpdRec:=FirstRec;
   btnRead.Visible:=TRUE
  end
 end;
end;{TMainForm.ListBoxFieldClick}

procedure TMainForm.cbMuetClick(Sender: TObject);
begin B_Muet:=cbMuet.checked end;

procedure TMainForm.mnAutoClick(Sender: TObject);
begin
 lbList.Caption:='FIELD for COLOR/SYMBOL';
 CurChoi:=ChAut; B_Auto:=True; B_Tic:=False;
 I_Symb:=0; I_Coul:=0; I_Tic:=0;
//I_Label:=1;
end;

procedure TMainForm.mnSymbolClick(Sender: TObject);
begin
 lbList.Caption:='Choose FIELD for SYMBOLS';
 CurChoi:=ChSym; B_Auto:=FALSE
end;

procedure TMainForm.mnColorsClick(Sender: TObject);
begin
 lbList.Caption:='Choose FIELD for COLORS';
 CurChoi:=ChCol; B_Auto:=FALSE
end;

procedure TMainForm.mnLabelClick(Sender: TObject);
begin lbList.Caption:='Choose FIELD for LABEL'; CurChoi:=ChLab end;

procedure TMainForm.mnSelectionClick(Sender: TObject);
begin lbList.Caption:='Choose FIELD for SELECTION'; CurChoi:=ChSel end;

procedure TMainForm.ChkListSelecClick(Sender: TObject);
var i: byte; s: string;
begin
 s:='';
 if (I_SelX>0) and (CurChoi=ChSel) and (Dim_Sel>0) then for i:= 1 to Dim_Sel do
 if ChkListSelec.Checked[i-1] then s:=s+ChkListSelec.Items[i-1];
 if s<>'' then Lis_SelX:=s;
 case CDes of
  DesPro,DesXY,DesTrg: Update_ValXYZ;
  DesSpd: begin
   SpdRec:=FirstRec;
   btnRead.Visible:=TRUE
  end
 end
end;

procedure TMainForm.rgpComplexClick(Sender: TObject);
begin CurExp:=rgpComplex.ItemIndex end;

procedure TMainForm.ChkListProfilClick(Sender: TObject);
var i, j: byte; s: string;
begin
 CASE CDes OF
  DesPro: BEGIN
   OK_Pro:=False; s:='';
   for i:= 1 to ArFrm[CorX].Nb_Ele do begin
    Yes_Pro[i]:=ChkListProfil.Checked[i-1];
    if Yes_Pro[i] then s:=s+ArFrm[CorX].Tit_Oxy[i]+' ';
    if Yes_Pro[i] then OK_Pro:=TRUE
   end;
   ArTit[CorY]:=s;
   {IF OK_Pro THEN BtnDraw:=visible}
  END;
  DesSpd: BEGIN
   j:=0;
   for i:= 1 to FrmSpd.Nb_Ele do begin
    if PrmSpd[i]>0 THEN inc(j);
    if j>0 then Yes_Spd[i]:=NOT ChkListProfil.Checked[j-1]
   end
  END
 END
end;

procedure TMainForm.cbProfilClick(Sender: TObject);
begin B_Profil:=cbProfil.Checked end;

procedure TMainForm.cbNormClick(Sender: TObject);
begin B_NormMax:=cbNorm.Checked end;

procedure TMainForm.btnSavClick(Sender: TObject);
begin
 IF Nb_Point>0 THEN BEGIN
  inc(Plot_Count);
  IF Plot_Count>99 THEN Plot_Count:= 99;
  if Ps_Possible and (CDes in [DesXy,DesTrg,DesPro,DesSpd,DesHis])
  then begin
   inc(I_Ps); PsPlot_Diag
  end;
 END;
 btnSav.Visible:= False;
 btnSavNew.Visible:= False
end;

procedure TMainForm.btnSavNewClick(Sender: TObject);
begin
 if Ps_Open then Plot_Close;
 I_Ps:=0;
 IF Nb_Point>0 THEN BEGIN
  inc(Plot_Count);
  IF Plot_Count>99 THEN Plot_Count:= 99;
  if Ps_Possible and (CDes in [DesXy,DesTrg,DesPro,DesSpd,DesHis])
  then begin
   inc(I_Ps); PsPlot_Diag
  end;
 END;
 btnSav.Visible:= False;
 btnSavNew.Visible:= False
end;

procedure TMainForm.mnNoSymColClick(Sender: TObject);
begin
 lbList.Caption:='FIELDS';
 CurChoi:=ChVid; B_Auto:=False; B_Tic:=False;
 I_Symb:=0; I_Coul:=0; {I_Label:=1;} I_Tic:= 0
end;

procedure TMainForm.mnHistoClick(Sender: TObject);
begin
 if Ps_Open then Plot_Close;
 I_Ps:=0;
 CDes:=DesHis;
 Init_XYZ;
 Show_XYZControls;
 btnDraw.Visible:=False;
end;{TMainForm.mnHistoClick}

procedure TMainForm.mnFieldAutoClick(Sender: TObject);
begin lbList.Caption:='SELECTION'; CurChoi:=ChAut end;

procedure Stc_Arg_Fos(b:byte);
var
 FrmIn,FrmOut,FrmOut2:Forme;RecAtom:Rec_Atom;
 CurRec:Rec_Ptr;R:Rec_Ana; s: string;
begin
//ExtractFileName(Nom_FiX);
 FrmIn:=ArFrm[CorX];
 CASE B OF
  1: Init_LibStc(FrmIn,FrmOut,RecAtom);
  3: Init_LibFos(FrmIn,FrmOut,RecAtom);
  2: Init_LibArg(FrmIn,FrmOut,FrmOut2,RecAtom)
 END;

 CurRec:=FirstRec;
 WHILE CurRec<>NIL DO BEGIN
  R:=CurRec^.Rec;
  CASE B OF
   1: Traite_Stc(FrmOut,R,RecAtom);
   2: Traite_Arg(FrmIn,FrmOut,FrmOut2,R,RecAtom);
   3: Traite_Fos(FrmIn,FrmOut,R,RecAtom)
  END;
  CurRec:= CurRec^.Next
 END;

 CASE B OF
  1: begin Close_LibStc; s:= Dir_Stc+'ALL.TXT' end;
  2: begin Close_LibArg; s:= Dir_Dat+'ARGILE.TXT' end;
  3: begin Close_LibFos; s:= Dir_Dat+'FOSFATE.TXT' end
 END;
(**)
 with EditForm do begin
  Caption:=s;
  btnPaste.Visible:=FALSE;
  memo.Lines.LoadFromFile(s);
 end;
 EditForm.ShowModal;
(**)
end;//Stc_Arg_Fos

procedure TMainForm.mnStcClick(Sender: TObject);
begin Stc_Arg_Fos(1) end;

procedure TMainForm.mnClayClick(Sender: TObject);
begin
 Stc_Arg_Fos(2);
end;

procedure TMainForm.cbZeroOkClick(Sender: TObject);
begin Nega_Is_Ok:=cbZeroOk.Checked end;

procedure TMainForm.mnFosClick(Sender: TObject);
begin Stc_Arg_Fos(3) end;

procedure Reload(var s:string);
begin
 CDes:=NoDes;
 if B_Samp_XY then begin
  B_Dual_XY:=False; SufX:=''; SufY:='';
  Ouvrir_File(False,True,False,Dir_Def+Nom_FiS,ArFrm[CorX]);
  FrmList:=ArFrm[CorX];
  Ouvrir_File(False,False,True,Dir_Def+Nom_FiX,ArFrm[CorY]);
  Show_XYZControls;
  Init_File;
  s:='TIKAZU '+Nom_FiS+' / '+Nom_FiX
 end

 else
 if B_Dual_XY then begin
  B_Dual_XY:=True; B_Samp_XY:=False;
  Ouvrir_File(True,False,False,{False,}Dir_Def+Nom_FiX,ArFrm[CorX]);
  FrmList:=ArFrm[CorX];
  Ouvrir_File(False,False,True,{False,}Dir_Def+Nom_FiY,ArFrm[CorY]);
  Show_XYZControls;
  Init_File;
  s:='TIKAZU '+Nom_FiX+' / '+Nom_FiY;
  SufX:='_'+Nom_FiX; SufY:='_'+Nom_FiY;
 end

 else begin
  B_Dual_XY:=False;B_Samp_XY:=False;
  Ouvrir_File(True,False,False,Dir_Def+Nom_FiX,ArFrm[CorX]);
  FrmList:=ArFrm[CorX];
  Init_File;
  s:='TIKAZU '+Nom_FiX;
 end
end;//Reload

procedure TMainForm.mnClipX_YClick(Sender: TObject);
var s: string;
begin
 with EditForm do begin
  btnPaste.Visible:=TRUE;
  btnSavLXY.Visible:=FALSE;
  btnSavLis.Visible:=FALSE;
  btnSavDat.Visible:=FALSE;
  btnSavX.Visible:=TRUE;
  btnSavY.Visible:=TRUE
 end;
 ChDir(Dir_Def);
 EditForm.ShowModal;
 B_Dual_XY:=True; B_Samp_XY:=False;
 Nom_FiX:='_x.txt';
 Nom_FiY:='_y.txt';
 Reload(s)
end;//mnClipX_YClick

procedure TMainForm.mnReloadClick(Sender: TObject);
var s: string;
begin
 Reload(s); Caption:=s
end;{mnReloadClick}

procedure TMainForm.mnSpiderClick(Sender: TObject);
var Fi: TextFile; s: string;
begin
 if Ps_Open then Plot_Close;
 I_Ps:=0;
 Show_XYZControls;
 listBoxX.Visible:=TRUE;
 listBoxY.Visible:=FALSE;
 Show_ArFact;
 //ChkListSelec.visible:=FALSE;
 lisBoxSpd.visible:=TRUE;
 lisBoxSpd.clear;
 //read spd format data from file
 DimSpd:=0;
{$I-}
 AssignFile(Fi,SpdFile); Reset(Fi);
{$I+}
 if IoResult=0 then begin
  readln(Fi,s);
  while (s<>'') and not (s[1] in Fin_Car) do begin
    Skip_Rite([_t],s);
    inc(DimSpd);
    lisBoxSpd.Items.Add(s);
    readln(Fi);readln(Fi);
    readln(Fi,s)
  end;
  CloseFile(Fi)
 end
end;{mnSpiderClick}

procedure TMainForm.lisBoxSpdClick(Sender: TObject);
var i:byte;
begin
 i:=lisBoxSpd.ItemIndex + 1;
 if (DimSpd>0) and (i<=DimSpd) then Init_Spider(i);
end;

procedure TMainForm.mnTestClick(Sender: TObject);
var s:string;
begin
 B_Samp_XY:=FALSE; B_Dual_XY:=FALSE;
 //Nom_FiS:='_colsym.sav';
 //Nom_FiX:='_xy.sav';
 Reload(s);
 Caption:=s
end;{mnTestClick}

procedure TMainForm.edBrbYChange(Sender: TObject);
begin
 BarbY:=StrToInt(edBrbY.Text);
end;

procedure TMainForm.updoBrbYClick(Sender: TObject; Button: TUDBtnType);
begin
 BarbY:=StrToInt(edBrbY.Text);
end;

procedure TMainForm.ShowSymbolsColorsClick(Sender: TObject);
(**
var
  Bitmap : TBitMap;
begin
  Bitmap := TBitmap.Create;
  try
    with Bitmap do begin
      LoadFromFile('xxx.bmp');
      Transparent := True;
      TransParentColor := BitMap.canvas.pixels[50,50];
      ImageForm.Image.Canvas.Draw(0,0,BitMap);
      TransparentMode := tmAuto;
      ImageForm.Image.Canvas.Draw(50,50,BitMap);
    end;
  finally
    Bitmap.Free;
  end;
end;
**)
begin
 with ImageForm do begin
  Image.Width:=BitW; Image.Height:=BitH
 end;
 ImageForm.Show;
 EffaceImage(ImageForm.Image);
 Test_Grf(ImageForm.Image.Canvas,Nb_Point);
end;
(**)
procedure TMainForm.mnOutputClick(Sender: TObject);
begin
 if Ps_Open then Plot_Close;
 I_Ps:=0;
 with OutPutForm do begin
  edX.text:=SufX; edY.Text:=SufY;
  cbPsLabels.Checked:=B_PsLabels;
  cbPsGreyScale.Checked:=B_PsGreyScale;
  cbPsSymbol.Checked:=B_PsSymbol;
  cbPsFrame.Checked:=B_PsFrame;
  rgpDiagramSize.ItemIndex:=C_Ps-1;
 end;
 OutPutForm.ShowModal;
 C_Ps:=OutPutForm.rgpDiagramSize.ItemIndex+1;
 B_PsLabels:=OutPutForm.cbPsLabels.Checked;
 B_PsGreyScale:=OutPutForm.cbPsGreyScale.Checked;
 B_PsSymbol:=OutPutForm.cbPsSymbol.Checked;
 B_PsFrame:=OutPutForm.cbPsFrame.Checked;
 case C_Ps of
  1,2,3: N_Ps:=1;
  4: N_Ps:=6
 end;
 btnSav.Caption:='SAVE '+chr(I_Ps+Ord('0')+1)+'/'+chr(N_Ps+Ord('0'));
 SufX:=OutPutForm.edX.text; SufY:=OutPutForm.edY.Text;

 if B_PsGreyScale then Ps_Header:=Dir_Def+'_headergrey.ps'
 else Ps_Header:=Dir_Def+'_header.ps';
end;

procedure TMainForm.mnClipSingleClick(Sender: TObject);
var s: string;
begin
 with EditForm do begin
  btnPaste.Visible:=TRUE;
  btnSavLXY.Visible:=TRUE;
  btnSavLis.Visible:=FALSE;
  btnSavDat.Visible:=FALSE;
  btnSavX.Visible:=FALSE;
  btnSavY.Visible:=FALSE
 end;
 ChDir(Dir_Def);
 EditForm.ShowModal;
 B_Dual_XY:=False;B_Samp_XY:=False;
 Nom_FiX:='_xy.txt';
 Reload(s)
end;

procedure TMainForm.mnClipLisXYClick(Sender: TObject);
var s: string;
begin
 with EditForm do begin
  btnPaste.Visible:=TRUE;
  btnSavLXY.Visible:=FALSE;
  btnSavLis.Visible:=TRUE;
  btnSavDat.Visible:=TRUE;
  btnSavX.Visible:=FALSE;
  btnSavY.Visible:=FALSE
 end;
 ChDir(Dir_Def);
 EditForm.ShowModal;
 B_Samp_XY:=TRUE; B_Dual_XY:=FALSE;
 Nom_FiS:='_colsym.txt';
 Nom_FiX:='_xy.txt';
 Reload(s)
end;

procedure TMainForm.mnAutomaticClick(Sender: TObject);
begin
 lbList.Caption:='Choose FIELD for SYMBOLS';
 CurChoi:=ChTic; B_Auto:=FALSE;
 I_Symb:=0; I_Coul:=0;
end;

end.

procedure Read_LogFile;
var
 f: TextFile;
 s: string;
 i:integer;
 Ok_Dat,Ok_Ps,Ok_Dir,Ok_Stc: boolean;
begin
 Log_File:=Dir_Def+'_log.txt';
 AssignFile(F,Log_File);
 {$I-}Reset(F);{$I+}
 IF IoResult=0 THEN BEGIN
  readln(F,S);
  WHILE (S[1]<>'#') and (pos('END',UpperCase(S))<>1) DO BEGIN
   S:=UpperCase(S);
   if pos('RES=',s)=1 then begin
    delete(s,1,pos('=',s));
    i:=StrToInt(s);
    if (i>=400) and (i<=800) then begin
     BitW:=i; BitH:=i
    end
   end;
   readln(F,S)
  END;//while
  CloseFile(F)
 END;
 OK_Dat:=FALSE;OK_Ps:=FALSE;Ok_Dir:=FALSE;
 Dir_Dat:=Dir_Def+'DAT'+DirCar;
 Dir_Ps:=Dir_Def+'PS'+DirCar;
 Ok_Dat:=Test_Dir(Dir_Dat);
 Ok_Ps:=Test_Dir(Dir_Ps);
 Dir_Stc:=IncludeTrailingPathDelimiter(Dir_Dat+'STC');
 Ok_Stc:=Test_Dir(Dir_Stc);
end;{Read_LogFile}



