unit u_output;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TOutputForm = class(TForm)
    rgpDiagramSize: TRadioGroup;
    btnOK: TButton;
    cbPsLabels: TCheckBox;
    edX: TEdit;
    edY: TEdit;
    edTitle: TEdit;
    cbPsFrame: TCheckBox;
    cbPsSymbol: TCheckBox;
    cbPsGreyScale: TCheckBox;
    cbExcelView: TCheckBox;
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  OutputForm: TOutputForm;

implementation

{$R *.dfm}

procedure TOutputForm.btnOKClick(Sender: TObject);
begin
 close
end;

end.
