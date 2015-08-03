unit u_edit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TEditForm = class(TForm)
    Memo: TMemo;
    btnPaste: TButton;
    btnSavLis: TButton;
    btnSavLXY: TButton;
    BitBtnOK: TBitBtn;
    btnSavX: TButton;
    btnSavY: TButton;
    btnSavDat: TButton;
    procedure btnPasteClick(Sender: TObject);
    procedure btnSavLisClick(Sender: TObject);
    procedure BitBtnOKClick(Sender: TObject);
    procedure btnSavLXYClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSavXClick(Sender: TObject);
    procedure btnSavYClick(Sender: TObject);
    procedure btnSavDatClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  EditForm: TEditForm;

implementation

{$R *.dfm}

procedure TEditForm.btnPasteClick(Sender: TObject);
begin
 Memo.Clear;
 Memo.PasteFromClipBoard
end;

procedure TEditForm.btnSavLisClick(Sender: TObject);
begin
 Memo.Lines.Append('§');
 Memo.Lines.SaveToFile('_colsym.txt');
// Memo.Clear;
end;

procedure TEditForm.BitBtnOKClick(Sender: TObject);
begin
 Close
end;

procedure TEditForm.btnSavLXYClick(Sender: TObject);
begin
 Memo.Lines.Append('§');
 Memo.Lines.SaveToFile('_xy.txt');
// Memo.Clear
end;

procedure TEditForm.FormCreate(Sender: TObject);
begin
 Memo.Clear
end;

procedure TEditForm.btnSavXClick(Sender: TObject);
begin
 Memo.Lines.Append('§');
 Memo.Lines.SaveToFile('_x.txt');
// Memo.Clear;
end;

procedure TEditForm.btnSavYClick(Sender: TObject);
begin
 Memo.Lines.Append('§');
 Memo.Lines.SaveToFile('_y.txt');
// Memo.Clear;
end;

procedure TEditForm.btnSavDatClick(Sender: TObject);
var s: string;
begin
 Memo.Lines.Append('§');
 Memo.Lines.SaveToFile('_xy.txt');
// Memo.Clear;
end;

end.
