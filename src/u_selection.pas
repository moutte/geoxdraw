unit u_selection;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst;

type
  TSelectForm = class(TForm)
    CheckListBox1: TCheckListBox;
    btnOK: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SelectForm: TSelectForm;

implementation

{$R *.dfm}

end.
