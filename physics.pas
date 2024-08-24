unit Physics;

{$mode ObjFPC}{$H+}

interface

type

  { TVector2D }

  TVector2D = record
    X: single;
    Y: single;
  end;

const
  g: TVector2D = (X: 0; Y: 0.1);

implementation

end.
