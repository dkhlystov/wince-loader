unit ImageButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type
  TButtonType = (btButton, btCheckBox, btSwitch);


  { TImageButton }

  TImageButton = class(TGraphicControl)
  private
    FButtonState: Integer;
    FBuffer: Array of TBitmap;
    FButtonType: TButtonType;
    FChecked: Boolean;
    FOnChange: TNotifyEvent;
    FOnClick: TNotifyEvent;
    FPicture: TPicture;
    FDown: Boolean;
    FSwitchCount: Integer;
    FSwitchIndex: Integer;
    procedure SetButtonType(AValue: TButtonType);
    procedure SetChecked(AValue: Boolean);
    procedure SetPicture(AValue: TPicture);
    procedure SetSwitchCount(AValue: Integer);
    procedure SetSwitchIndex(AValue: Integer);
  protected
    procedure PrepareBuffer;
    procedure Paint; override;
    procedure DoOnPictureChange(Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Resize; override;
    procedure UpdateButtonState;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ButtonType: TButtonType read FButtonType write SetButtonType default btButton;
    property Checked: Boolean read FChecked write SetChecked default False;
    property Picture: TPicture read FPicture write SetPicture;
    property SwitchCount: Integer read FSwitchCount write SetSwitchCount default 1;
    property SwitchIndex: Integer read FSwitchIndex write SetSwitchIndex default 0;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Loader',[TImageButton]);
end;

{ TImageButton }

procedure TImageButton.SetButtonType(AValue: TButtonType);
begin
  if FButtonType=AValue then Exit;
  FButtonType:=AValue;
  PrepareBuffer;
  UpdateButtonState;
end;

procedure TImageButton.SetChecked(AValue: Boolean);
begin
  if FChecked=AValue then Exit;
  FChecked:=AValue;
  UpdateButtonState;
end;

procedure TImageButton.SetPicture(AValue: TPicture);
begin
  FPicture.Assign(AValue);
end;

procedure TImageButton.SetSwitchCount(AValue: Integer);
begin
  if AValue<1 then AValue:=1;
  if FSwitchCount=AValue then Exit;
  FSwitchCount:=AValue;
  PrepareBuffer;
  Invalidate;
end;

procedure TImageButton.SetSwitchIndex(AValue: Integer);
begin
  if AValue<0 then AValue:=0;
  if AValue>FSwitchCount-1 then AValue:=FSwitchCount-1;
  if FSwitchIndex=AValue then Exit;
  FSwitchIndex:=AValue;
  if FButtonType=btSwitch then UpdateButtonState;
end;

procedure TImageButton.PrepareBuffer;
var
  Count:Integer;
  I:Integer;
begin
  //remove old
  for I:=0 to Length(FBuffer)-1 do FBuffer[I].Free;

  //image count
  case FButtonType of
    btButton: Count:=4;
    btCheckBox: Count:=4;
    btSwitch: Count:=FSwitchCount;
  end;

  //init images
  SetLength(FBuffer, Count);
  for I:=0 to Count-1 do begin
    FBuffer[I]:=TBitmap.Create;
    FBuffer[I].Width:=Width;
    FBuffer[I].Height:=Height;
    FBuffer[I].Canvas.Draw(0,-Height*I,FPicture.Graphic);
  end;
end;

procedure TImageButton.Paint;
begin
  Canvas.Draw(0,0,FBuffer[FButtonState]);
end;

procedure TImageButton.DoOnPictureChange(Sender: TObject);
begin
  PrepareBuffer;
  Invalidate;
end;

procedure TImageButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FDown:=True;
  UpdateButtonState;
end;

procedure TImageButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FDown:=False;
  if Enabled then begin
    if FButtonType=btCheckBox then begin
      FChecked:=not FChecked;
      if Assigned(FOnChange) then FOnChange(Self);
    end else if FButtonType=btSwitch then begin
      Inc(FSwitchIndex);
      if FSwitchIndex>FSwitchCount-1 then FSwitchIndex:=0;
        if Assigned(FOnChange) then FOnChange(Self);
    end;
    if Assigned(FOnClick) then FOnClick(Self);
  end;
  UpdateButtonState;
end;

procedure TImageButton.Resize;
begin
  PrepareBuffer;
end;

procedure TImageButton.UpdateButtonState;
var
  State: Integer;
begin
  case FButtonType of
    btButton: begin
      if not Enabled then begin
        State:=2;
      end else if FDown then begin
        State:=1;
      end else begin
        State:=0;
      end;
    end;

    btCheckBox: begin
      if not Enabled then begin
        State:=3;
      end else if FDown then begin
        State:=2;
      end else if FChecked then begin
        State:=1;
      end else begin
        State:=0;
      end;
    end;

    btSwitch: begin
      State:=FSwitchIndex;
    end;
  end;

  if State=FButtonState then Exit;
  FButtonState:=State;
  Invalidate;
end;

constructor TImageButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPicture:=TPicture.Create;

  FDown:=False;
  FButtonType:=btButton;
  FChecked:=False;
  FSwitchCount:=1;
  FSwitchIndex:=0;
  FPicture.OnChange:=@DoOnPictureChange;
  UpdateButtonState;
end;

destructor TImageButton.Destroy;
var
  I:Integer;
begin
  FPicture.Free;
  for I:=0 to Length(FBuffer)-1 do FBuffer[I].Free;

  inherited Destroy;
end;

end.
