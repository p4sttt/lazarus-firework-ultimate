unit Physics;

{$mode ObjFPC}{$H+}

interface

const
  VelocityLossCoef = 0.8;
  MinVelocity = 1 / 1000;

type

  { TVector2D }

  TVector2D = record
    X: single;
    Y: single;
  end;

  TPackedVector2D = ^TVector2D;


const
  g: TVector2D = (X: 0; Y: 0.1);

implementation

end.
