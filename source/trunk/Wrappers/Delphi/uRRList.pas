unit uRRList;

interface

Uses SysUtils, Classes, uSBWCommon, uSBWComplex, uSBWArray;

type
  TRRList = class;

  TRRListType = (rrList, rrDouble, rrString);

  TRRListItem = class (TObject)
            public
                DataType : TRRListType;
                d : double;
                str : AnsiString;
                list : TRRList;
                function getDouble : double;
                function getString : AnsiString;
                function getList : TRRList;

                constructor Create (d : double);  overload;
                constructor Create (str : AnsiString); overload;
                constructor Create (list : TRRList); overload;

                destructor  Destroy; override;
  end;


  TRRList = class (TList)
             protected
               function Get (Index : integer) : TRRListItem;
               procedure Put (Index : integer; Item : TRRListItem);
             public
               destructor Destroy; override;
               function  Add (Item : TRRListItem) : integer;
               procedure Delete (Index : Integer);
               procedure copy (src : TRRList);
               property  Items[Index : integer] : TRRListItem read Get write Put; default;
               //function  Count : integer;
  end;

implementation

Uses uSBWUtils;


constructor TRRListItem.Create (d : double);
begin
  inherited Create;
  DataType := rrDouble;
  Self.d := d;
end;


constructor TRRListItem.Create (str : AnsiString);
begin
  inherited Create;
  DataType := rrString;
  Self.str := str;
end;


constructor TRRListItem.Create (list : TRRList);
begin
  inherited Create;
  DataType := rrList;
  Self.list := List;
end;


destructor TRRListItem.Destroy;
begin
  case DataType of
     rrList : List.Free;
  end;
  inherited Destroy;
end;


// --------------------------------------------------------------------------

function TRRListItem.getDouble : Double;
begin
  if DataType <> rrDouble then
     raise Exception.Create ('Double expected in List item');
  result := d;
end;


function TRRListItem.getString : AnsiString;
begin
  if DataType <> rrString then
     raise Exception.Create ('String expected in List item');
  result := str;
end;


function TRRListItem.getList : TRRList;
begin
  if DataType <> rrList then
     raise Exception.Create ('List expected in List item');
  result := list;
end;

// ----------------------------------------------------------------------


function TRRList.Get (Index : integer) : TRRListItem;
begin
  result := TRRListItem(inherited Get(index));
end;


procedure TRRList.Put (Index : integer; Item : TRRListItem);
begin
  inherited Put (Index, Item);
end;

//function TRRList.Count : integer;
//begin
//  result := count;
//end;

function TRRList.Add (Item : TRRListItem) : integer;
begin
  result := inherited Add (Item);
end;


procedure TRRList.Delete (Index : Integer);
begin
  Items[Index].Free;
  Items[Index] := nil;
  inherited Delete (Index);
end;


destructor TRRList.Destroy;
var i : integer;
begin
  for i := 0 to Count - 1 do
      Items[i].Free;
  inherited Destroy;
end;


procedure TRRList.copy (src: TRRList);
var i : integer;
begin
  self.Clear;
  for i := 0 to src.count - 1 do
      begin
      case src[i].DataType of
          rrDouble  : self.add (TRRListItem.Create(src[i].d));
          rrString  : self.add (TRRListItem.Create(src[i].str));
          rrList    : self.add (TRRListItem.Create(src[i].list));
      else
          raise Exception.Create ('Unknown data type while copying SBWList');
      end;
      end;
end;


// ----------------------------------------------------------------------


end.

