unit regconst;

interface

type
  TTokenType = (ttUnknown, ttProject, ttOrganisation, ttGUID);
const
  TokenTypeStr : array[TTokenType] of string = ('unknown', '%%PROJECT%%', '%%ORGANISATION%%', '%%GUID%%');
  Read = True;
  Write = False;

implementation

end.

