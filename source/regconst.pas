unit regconst;

{$mode Delphi}{$H+}

interface

type
  TTokenType = (ttUnknown, ttProject, ttOrganisation, ttGUID);

const
  TokenTypeStr : array[TTokenType] of string = ('unknown', '%%PROJECT%%', '%%ORGANISATION%%', '%%GUID%%');

  Read = True;
  Write = False;

  _ItemIndex = 'ItemIndex';

implementation

end.

