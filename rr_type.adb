----------------------------------------------------------------
-- IRONSIDES - DNS SERVER
--
-- By: Martin C. Carlisle and Barry S. Fagin
--     Department of Computer Science
--     United States Air Force Academy
--
-- This is free software; you can redistribute it and/or 
-- modify without restriction.  We do ask that you please keep
-- the original author information, and clearly indicate if the
-- software has been modified.
--
-- This software is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
----------------------------------------------------------------

--with Ada.Text_IO;
package body Rr_Type is

   ----------------------
   -- DomainNameLength --
   ----------------------

   function DomainNameLength
     (Name : DomainNameStringType)
      return DomainNameStringTypeIndex
   is
      Index : DomainNameStringTypeIndex;
   begin
      Index := DomainNameStringTypeIndex'First;
      while Index<MaxDomainNameLength and then Name(DomainNameStringTypeIndex'First)/=' ' and then Name(Index+1)/=' ' loop
         --# assert Index<MaxDomainNameLength and
         --#   (for all Q in DomainNameStringTypeIndex range 1..Index+1 => (Name(Q)/= ' '));
         Index := Index + 1;
      end loop;
      return Index;
   end DomainNameLength;

   --returns "Left.Right" if room within DomainNameStringType
   --unless Left is "@", in which case just returns Right
   procedure AppendDomainNames(
      Left : in out DomainNameStringType;
      Right: in DomainNameStringType;
      Success: in out boolean)
   is
      ORIGIN_CHAR : constant Character := '@';  --also definied in parser_utilities.adb
      LengthL : DomainNameStringTypeIndex;
      LengthR : DomainNameStringTypeIndex;
   begin
      LengthL := DomainNameLength(Left);
      LengthR := DomainNameLength(Right);
      if LengthL = DomainNameStringTypeIndex'Last then
         Success := False;
      end if;
      if ((LengthL + 1) + LengthR) > MaxDomainNameLength then
         Success := False;
      end if;
      if Success then
         --if appending to origin symbol, treat it as null
         if (Left(LengthL) = ORIGIN_CHAR and LengthL = 1) then
            Left := Right;
         else
            Left(LengthL+1) := '.';  --Examiner caught a possible RTE error here, impressive!
            for I in Integer range 1..LengthR loop
               --# assert lengthL >= DomainNameStringTypeIndex'First
               --# and lengthL < DomainNameStringTypeIndex'Last
               --# and ((LengthL + 1) + LengthR) <= MaxDomainNameLength
               --# and lengthR% = lengthR;
               Left((LengthL+1)+I) := Right(I);
            end loop;
         end if;
      end if;
   end AppendDomainNames;

   ----------------------
   -- WireNameLength --
   ----------------------

   function WireNameLength
     (Name : WireStringType)
      return WireStringTypeIndex
   is
      Index : WireStringTypeIndex;
   begin
      Index := WireStringTypeIndex'First;
      while Index<MaxDomainNameLength+1 and Name(Index)/=Character'Val(0) loop
         --# assert Index<MaxDomainNameLength+1 and
         --#   (for all Q in WireStringTypeIndex range 1..Index => (Name(Q)/=Character'Val(0)));
         Index := Index + 1;
      end loop;
      return Index;
   end WireNameLength;

   function ConvertStringToDomainName(S: in String) return DomainNameStringType
         is
      newdomainName : DomainNameStringType := blankDomainName;
   begin
      for I in Integer range 1..S'Length loop
         --# assert true;
         newdomainName(I) := S(I);
      end loop;
      return newdomainName;
   end ConvertStringToDomainName;


--converts "abcd.efg.hijkl.edu." to "\x04abcd\x03efg\x05hijkl\x03edu\x00", the format of
   --a domain name as it comes off the wire.  Periods disappear, each field is prefixed
   --by its length in bytes.  Note null byte at end, if original ends in a period.
   function ConvertDomainNameToWire(
                  domainNameVersion: in DomainNameStringType) return WireStringType
     is
   LengthOfDomainName : DomainNameStringTypeIndex;
   WireVersion : WireStringType;

      function FindPeriod(Token : in DomainNameStringType; Position : LineLengthIndex) return Character is
   	  result : LineLengthIndex;
   	  begin      Result := Position;
   	    while Result < DomainNameStringTypeIndex'last and then Token(Result)/='.' loop
   	    --# assert Result >= Position and Result < LineLengthIndex'Last;
            Result := Result + 1;
   	    end loop;
   	    return Character'Val(Result-Position);
      end FindPeriod;

   begin
      LengthOfDomainName := DomainNameLength(domainNameVersion);
      wireVersion := BlankWire;
      for I in integer range DomainNameStringTypeIndex'First..LengthOfDomainName-1 loop
         --# assert LengthOfDomainName>=1 and LengthOfDomainName<=MaxDomainNameLength;
         if domainNameVersion(I)='.' then
            WireVersion(I+1) := FindPeriod(domainNameVersion,I+1);
         else
            WireVersion(I+1) := domainNameVersion(I);
         end if;
      end loop;
      WireVersion(DomainNameStringType'First) := FindPeriod(domainNameVersion,DomainNameStringType'First);
      wireVersion(LengthOfDomainName+1) := Character'Val(0);
      return wireVersion;
   end ConvertDomainNameToWire;

   function ConvertStringToWire(S: in String) return WireStringType
      is
   begin
      return ConvertDomainNameToWire(convertStringToDomainName(S));
   end ConvertStringToWire;


end Rr_Type;
