unit Particle;

{$mode ObjFPC}{$H+}

interface

uses
  Physics;

type

  { TParticle }

  TParticle = class
  private
    FSize: integer;
    FPosition: TVector2D;
    FVelocity: TVector2D;

    function GetSize: integer;
    function GetX: single;
    function GetY: single;
    function GetVx: single;
    function GetVy: single;

    procedure SetSize(ASize: integer);
    procedure SetX(AX: single);
    procedure SetY(AY: single);
    procedure SetVx(AVx: single);
    procedure SetVy(AVy: single);

    procedure UpdatePosition;
    procedure UpdateVelocity;
  public
    constructor Create(ASize: integer; AX, AY, AVx, AVy: single); overload;
    constructor Create; overload;

    procedure Update;
    procedure InverseVx;
    procedure InverseVy;

    property Size: integer read GetSize write SetSize;
    property X: single read GetX write SetX;
    property Y: single read GetY write SetY;
    property Vx: single read GetVx write SetVx;
    property Vy: single read GetVy write SetVy;
  end;


implementation

{ TParticle }

function TParticle.GetSize: integer;
begin
  Result := Self.FSize;
end;

function TParticle.GetX: single;
begin
  Result := Self.FPosition.X;
end;

function TParticle.GetY: single;
begin
  Result := Self.FPosition.Y;
end;

function TParticle.GetVx: single;
begin
  Result := Self.FVelocity.X;
end;

function TParticle.GetVy: single;
begin
  Result := Self.FVelocity.Y;
end;

procedure TParticle.SetSize(ASize: integer);
begin
  if ASize < 0 then
    Self.FSize := 10
  else
    Self.FSize := ASize;
end;

procedure TParticle.SetX(AX: single);
begin
  Self.FPosition.X := AX;
end;

procedure TParticle.SetY(AY: single);
begin
  Self.FPosition.Y := AY;
end;

procedure TParticle.SetVx(AVx: single);
begin
  Self.FVelocity.X := AVx;
end;

procedure TParticle.SetVy(AVy: single);
begin
  Self.FVelocity.Y := AVy;
end;

procedure TParticle.UpdatePosition;
begin
  Self.X := Self.X + Self.Vx + g.X / 2;
  Self.Y := Self.Y + Self.Vy + g.Y / 2;
end;

procedure TParticle.UpdateVelocity;
begin
  Self.Vx := Self.Vx + g.X;
  Self.Vy := Self.Vy + g.Y;

  if Abs(Self.Vx) < MinVelocity then Self.Vx := 0;
  if Abs(Self.Vy) < MinVelocity then Self.Vy := 0;
end;

constructor TParticle.Create(ASize: integer; AX, AY, AVx, AVy: single);
begin
  Self.Size := ASize;
  Self.FPosition.X := AX;
  Self.FPosition.Y := AY;
  Self.FVelocity.X := AVx;
  Self.FVelocity.Y := AVy;
end;

constructor TParticle.Create;
begin
  Self.Size := 10;
  Self.FPosition.X := 0;
  Self.FPosition.Y := 0;
  Self.FVelocity.X := 5;
  Self.FVelocity.Y := -5;
end;

procedure TParticle.Update;
begin
  Self.UpdatePosition;
  Self.UpdateVelocity;
end;

procedure TParticle.InverseVx;
begin
  Self.Vx := -Self.Vx * VelocityLossCoef;
end;

procedure TParticle.InverseVy;
begin
  Self.Vy := -Self.Vy * VelocityLossCoef;
end;

end.
