program geoxdraw;

uses
  Dialogs,
  Forms,
  u_main in 'u_main.pas' {MainForm},
  u_output in 'u_output.pas' {OutputForm},
  u_image in 'u_image.pas' {ImageForm},
  u_symcolix in 'u_symcolix.pas' {SymColForm},
  u_edit in 'u_edit.pas' {EditForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Drawing Tools for Geochemical Data';
  ShowMessage('J.Moutte, SPIN, Ecole des Mines, Saint Etienne');
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TOutputForm, OutputForm);
  Application.CreateForm(TImageForm, ImageForm);
  Application.CreateForm(TSymColForm, SymColForm);
  Application.CreateForm(TEditForm, EditForm);
  Application.Run;
end.
