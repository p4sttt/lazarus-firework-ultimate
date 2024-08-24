unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Particle, Math;

const
  ParticlesCount = 20;

  MinSize = 4;
  MaxSize = 6;

  MinVelocityX = -1.5;
  MaxVelocityX = 1.5;

  MinVelocityY = 0;
  MaxVelocityY = -4;

  ColorStep = 1;

  BoundsOffset = 50;

type

  { TFormMain }

  TFormMain = class(TForm)
    CreateFireworkTimer: TTimer;
    TimerPhysics: TTimer;

    procedure CreateFireworkTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure TimerPhysicsTimer(Sender: TObject);

  private
    Particles: array[1..ParticlesCount] of TParticle;
    PColor: byte;
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

procedure TFormMain.CreateFireworkTimerTimer(Sender: TObject);
var
  X, Y: integer;
begin
  X := RandomRange(BoundsOffset, ClientWidth - BoundsOffset);
  Y := RandomRange(BoundsOffset, ClientHeight - BoundsOffset);
  MakeParticles(X, Y);
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
    Canvas.Brush.Color := RGBToColor(PColor, PColor, PColor);
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
  PColor := 255;
  for I := 1 to High(Particles) do
  begin
    Size := RandomRange(MinSize, MaxSize);
    Vx := MinVelocityX + Random * (MaxVelocityX - MinVelocityX);
    Vy := MinVelocityY + Random * (MaxVelocityY - MinVelocityY);
    Particles[I] := TParticle.Create(Size, X, Y, Vx, Vy);
  end;
end;

procedure TFormMain.UpdateParticles;
var
  I: integer;
begin
  PColor := Max(PColor - ColorStep, 0);
  for I := 1 to High(Particles) do
  begin
    Particles[I].Update;
  end;
end;

end.
