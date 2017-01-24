unit vector;
unit vector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
Rvector = array of single;
Gvector = array of integer;
Function CorrIdr(mask:Gvector;Z1,Z2: Rvector;aantal:integer):double;
Function MeanIdr(mask:Gvector;z: Rvector;aantal:integer):double;
Function stdvIdr(mask:Gvector;z: Rvector;aantal:integer):double;
procedure sortvec(var Z:Rvector;iLo,iHi:Integer);


implementation

procedure sortvec(var Z:Rvector;iLo,iHi:Integer);
var
    Lo, Hi: Integer;
    Mid,T : double;
begin

    Lo := iLo;
    Hi := iHi;
    Mid := Z[(Lo + Hi) div 2];
    repeat
      while Z[Lo] < Mid do Inc(Lo);
      while Z[Hi] > Mid do Dec(Hi);
      if Lo <= Hi then
      begin
        T := Z[Lo];
        Z[Lo] := Z[Hi];
        Z[Hi] := T;
       // helprow := row[lo];
       // row[lo]:=row[hi];
       // row[hi]:=helprow;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then Sortvec(Z, iLo, Hi);
    if Lo < iHi then Sortvec(Z, Lo, iHi);

end;

Function MeanIdr(mask:Gvector;z: Rvector;aantal:integer):double;
var
mean:double;
number,i:integer;
begin
mean:=0.0; number:=0;
for i:=1 to aantal do
 begin
  If mask[i]<>0 then
   begin
    mean:=mean+Z[i];
    Inc(number);
   end;
  end;
Result:=mean/number;
end;


Function stdvIdr(mask:Gvector;z: Rvector;aantal:integer):double;
var
mean,stdv:double;
number,i:integer;
begin
mean:=MeanIdr(mask,z,aantal);
stdv:=0.0;number:=0;
for i:=1 to aantal do
 begin
  If mask[i]<>0 then
   begin
    stdv:=stdv+sqr(Z[i]-mean);
    Inc(number);
   end;
  end;
Result:=sqrt(stdv/(number-1));
end;

Function CorrIdr(mask:Gvector;Z1,Z2: Rvector;aantal:integer):double;
var
number,i:integer;
mean1,mean2,stdv1,stdv2,corr:double;
begin
corr:=0.0;number:=0;
mean1:=MeanIdr(mask,Z1,aantal);mean2:=MeanIdr(mask,Z2,aantal);
stdv1:=StdvIdr(mask,Z1,aantal);stdv2:=StdvIdr(mask,Z2,aantal);
for i:=1 to aantal do
 begin
  If mask[i]<>0 then
   begin
    corr:=corr+ (((Z1[i]-mean1)/stdv1)*((Z2[i]-mean2)/stdv2));
    Inc(number);
   end;
  end;
Corridr:=corr/(number-1);
end;



end.

