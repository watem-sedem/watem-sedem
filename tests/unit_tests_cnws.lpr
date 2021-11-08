program unit_tests_cnws;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, test_sgrd;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title:='unit_tests_cnws';
  Application.Run;
  Application.Free;
end.
