unit u_image;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

type
  TImageForm = class(TForm)
    Image: TPaintBox;
    procedure Input_XY(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ImageForm: TImageForm;

implementation

{$R *.dfm}

uses LibIo, LibGrf;

procedure TImageForm.Input_XY(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
(**
  if Button=mbLeft then begin
   P_Low[Cor_Y]{MainForm.edYLow.Text}:=Y;
   P_Low[Cor_X]{MainForm.edXLow.Text}:=X
  end;
  if Button=mbRight then begin
   P_Low[Cor_Y]{MainForm.edYHih.Text}:=Y;
   P_Low[Cor_X]{MainForm.edXLow.Text}:=X
  end
**)
end;

end.
