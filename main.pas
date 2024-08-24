unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Particle, Math;

const
  ParticlesCount = 50;

  MinSize = 4;
  MaxSize = 6;

  MinVelocityX = -5;
  MaxVelocityX = 5;

  MinVelocityY = -3;
  MaxVelocityY = -10;

  ColorStep = 3;

  SizeCoefX = 0.8;
  SizeCoefY = 0.75;

  Colors: array[1..10] of TColor = (
    TColor($FF8A8AFF),
    TColor($9DBDFFFF),
    TColor($0D7C66FF),
    TColor($F5F7F8FF),
    TColor($7C00FEFF),
    TColor($FDFFC2FF),
    TColor($98EECCFF),
    TColor($FFA3FDFF),
    TColor($14C38EFF),
    TColor($94B3FDFF)
    );

type

  { TFormMain }

  TFormMain = class(TForm)
    TimerPhysics: TTimer;

    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure TimerPhysicsTimer(Sender: TObject);

  private
    Particles: array[1..ParticlesCount] of TParticle;
    PColor: TColor;
    IsCreated: boolean;

    procedure MakeParticles(X, Y: single);
    procedure UpdateParticles;
  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  IsCreated := False;
  FormMain.Left := (Screen.Width - ClientWidth) div 2;
  FormMain.Top := (Screen.Height - ClientHeight) div 2;
end;

procedure TFormMain.FormClick(Sender: TObject);
var
  Pt: TPoint;
begin
  Pt := Mouse.CursorPos;
  Pt := ScreenToClient(Pt);
  MakeParticles(Pt.X, Pt.Y);
end;

procedure TFormMain.FormPaint(Sender: TObject);
var
  I: integer;
  X, Y: integer;
begin
  Canvas.Brush.Color := clBlack;
  Canvas.FillRect(0, 0, ClientWidth, ClientHeight);
  Canvas.Pen.Style := psClear;

  if isCreated then
  begin
    Canvas.Brush.Color := PColor;
    for I := 0 to High(Particles) do
    begin
      X := Trunc(Particles[I].X);
      Y := Trunc(Particles[I].Y);
      Canvas.Ellipse(X, Y, X + Particles[I].Size, Y + Particles[I].Size);
    end;
  end;
end;

procedure TFormMain.TimerPhysicsTimer(Sender: TObject);
begin
  if isCreated then UpdateParticles;
  Invalidate;
end;

procedure TFormMain.MakeParticles(X, Y: single);
var
  I: integer;
  Size: integer;
  Vx, Vy: single;
begin
  IsCreated := True;
  PColor := Colors[RandomRange(Low(Colors), High(Colors) + 1)];
  for I := 1 to High(Particles) do
  begin
    Size := RandomRange(MinSize, MaxSize);
    Vx := (MinVelocityX + Random * (MaxVelocityX - MinVelocityX)) / (Size * SizeCoefX);
    Vy := (MinVelocityY + Random * (MaxVelocityY - MinVelocityY)) / (Size * SizeCoefY);
    Particles[I] := TParticle.Create(Size, X, Y, Vx, Vy);
  end;
end;

procedure TFormMain.UpdateParticles;
var
  I: integer;
  R, G, B: byte;
begin
  R := Max(Red(PColor) - ColorStep, 0);
  G := Max(Green(PColor) - ColorStep, 0);
  B := Max(Blue(PColor) - ColorStep, 0);
  PColor := RGBToColor(R, G, B);
  for I := 1 to High(Particles) do
  begin
    Particles[I].Update;
  end;
end;

end.
