unit unitmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ImageButton;

type

  { TFormMain }

  TFormMain = class(TForm)
    ImageButtonRepeat: TImageButton;
    ImageButtonSettings: TImageButton;
    ImageButtonShuffle: TImageButton;
    ImageButtonNavi: TImageButton;
    ImageButtonNextFolder: TImageButton;
    ImageButtonNextTrack: TImageButton;
    ImageButtonPlay: TImageButton;
    ImageButtonPause: TImageButton;
    ImageButtonPrevFolder: TImageButton;
    ImageButtonPrevTrack: TImageButton;
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.frm}

{ TFormMain }

procedure TFormMain.FormShow(Sender: TObject);
begin
  Left:=0;
  Top:=0;
end;

end.

