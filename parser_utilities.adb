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

with non_spark_stuff, error_msgs, Ada.Characters.Handling, Ada.Characters.Latin_1, Rr_Type.A_Record_Type,
   rr_type.soa_record_type;
use type Rr_Type.RrItemType;

   PACKAGE BODY Parser_Utilities IS

   BLANK : constant Character := ' ';
   TAB : constant Character := Ada.Characters.Latin_1.HT;
   COMMENT_CHAR : constant Character := ';';
   CONTROL_CHAR : constant Character := '$';
   ORIGIN_CHAR : constant Character := '@';  --also definied in rr_type.adb
   L_PAREN: constant Character := '(';
   R_PAREN: constant Character := ')';

   SecondsInAMinute: constant natural := 60;
   MinutesInAnHour: constant natural  := 60;
   HoursInADay: constant natural  := 24;
   DaysInAWeek: constant Natural  := 7;
   MaxDaysInAMonth : constant Natural := 31;
   MonthsInAYear: constant natural  := 12;

   --CH, CS, HS and IN are only valid namespace classes
   function isClass(s : in rr_type.LineFromFileType; begIdx : in rr_type.LineLengthIndex;
                    endIdx : in rr_type.LineLengthIndex) return boolean is
      Retval : Boolean;
      subtype two_range is integer range 1..2;
      subtype string2 is string(two_range);
      Class : String2 := "  ";
   begin
      --helps prover
      if begIdx = endIdx-1 then
         Class(1) := Ada.Characters.Handling.To_Upper(S(BegIdx));
         Class(2) := Ada.Characters.Handling.To_Upper(S(endIdx));
            Retval := (Class="CH") or else (Class="CS") or else (Class="HS")
               or else (Class="IN");
      else
         retval := false;
      end if;
      return retval;
   end IsClass;

   --For now, only {A,AAAA,CNAME,DNSKEY,MX,NS,PTR,SOA} records recognized
   --{RP,SRV,TXT recognized but not implemented}
   function isRecord(s : in rr_type.LineFromFileType; begIdx : in rr_type.LineLengthIndex;
                     endIdx : in rr_type.LineLengthIndex) return boolean
   --# pre begIdx <= endIdx;
   is
      firstChar : character;
      lastChar : character;
      lengthToken : rr_type.LineLengthIndex;
      retval : boolean;
   begin
      firstChar:= Ada.Characters.Handling.To_Upper(S(BegIdx));
      lastChar := Ada.Characters.Handling.To_Upper(S(EndIdx));
      lengthToken := (endIdx-begIdx)+1;
      --ugly but it gets the job done
      --A
      IF firstChar = 'A' AND LengthToken = 1 THEN
         Retval := True;
      --NS
      elsif firstChar = 'N' and lastChar = 'S' and lengthToken = 2 then
         Retval := True;
      --MX
      elsif firstChar = 'M' and lastChar = 'X' and lengthToken = 2 then
         Retval := True;
      --RP
      elsif firstChar = 'R' and lastChar = 'P' and lengthToken = 2 then
         Retval := True;
      --PTR
       --formulation of third condition helps prover
      elsif firstChar = 'P' and lastChar = 'R' and begIdx <= endIdx-2 then
         if Ada.Characters.Handling.to_upper(s(begIdx+1)) = 'T' then
            Retval := true;
         else
            RetVal := false;
         end if;
      --SOA
      --formulation of third condition helps prover
      elsif firstChar = 'S' and lastChar = 'A' and begIdx <= endIdx-2 then
         if Ada.Characters.Handling.to_upper(s(begIdx+1)) = 'O' then
            Retval := true;
         else
            RetVal := false;
         end if;
      --SRV
      --formulation of third condition helps prover
      elsif firstChar = 'S' and lastChar = 'V' and begIdx <= endIdx-2 then
         if Ada.Characters.Handling.to_upper(s(begIdx+1)) = 'R' then
            Retval := true;
         else
            RetVal := false;
         end if;
      --TXT
      --formulation of third condition helps prover
      elsif firstChar = 'T' and lastChar = 'T' and begIdx <= endIdx-2 then
         if Ada.Characters.Handling.to_upper(s(begIdx+1)) = 'X' then
            Retval := true;
         else
            RetVal := false;
         end if;
      --AAAA
      --formulation of third condition helps prover
      elsif firstChar = 'A' and lastChar = 'A' and begIdx <= endIdx-3 then
         if Ada.Characters.Handling.to_upper(s(begIdx+1)) = 'A' and
            Ada.Characters.Handling.to_upper(s(begIdx+2)) = 'A' then
            Retval := true;
         else
            RetVal := false;
         end if;

      --NSEC
      --formulation of third condition helps prover
      elsif firstChar = 'N' and lastChar = 'C' and begIdx <= endIdx-3 then
         if Ada.Characters.Handling.to_upper(s(begIdx+1)) = 'S' and
            Ada.Characters.Handling.to_upper(s(begIdx+2)) = 'E' then
            Retval := true;
         else
            RetVal := false;
         end if;

      --CNAME
      --formulation of third condition helps prover
      elsif firstChar = 'C' and lastChar = 'E' and begIdx <= endIdx-4 then
         if Ada.Characters.Handling.to_upper(s(begIdx+1)) = 'N' and
            Ada.Characters.Handling.to_upper(s(begIdx+2)) = 'A' and
            Ada.Characters.Handling.to_upper(s(begIdx+3)) = 'M' then
            Retval := true;
         else
            RetVal := false;
         end if;

      --RRSIG
      --formulation of third condition helps prover
      elsif firstChar = 'R' and lastChar = 'G' and begIdx <= endIdx-4 then
         if Ada.Characters.Handling.to_upper(s(begIdx+1)) = 'R' and
            Ada.Characters.Handling.to_upper(s(begIdx+2)) = 'S' and
            Ada.Characters.Handling.to_upper(s(begIdx+3)) = 'I' then
            Retval := true;
         else
            RetVal := false;
         end if;

      --DNSKEY
      --formulation of third condition helps prover
      elsif firstChar = 'D' and lastChar = 'Y' and begIdx <= endIdx-5 then
         if Ada.Characters.Handling.to_upper(s(begIdx+1)) = 'N' and
            Ada.Characters.Handling.To_Upper(S(BegIdx+2)) = 'S' and
            Ada.Characters.Handling.to_upper(s(begIdx+3)) = 'K' and
            Ada.Characters.Handling.to_upper(s(begIdx+4)) = 'E' then
            Retval := true;
         else
            RetVal := false;
         end if;
      ELSE
         retVal := false;
      end if;
      return retval;
   END IsRecord;

   --Called only when s(begIdx..endIdx) is valid record type indicator
    --For now, only {A,AAAA,CNAME,DNSKEY,MX,NS,NSEC,PTR,SOA} records supported
    --{RP,SRV,TXT recognized but not implemented}
   function getRecordType(s : in rr_type.LineFromFileType; begIdx : in rr_type.LineLengthIndex;
                          endIdx : in rr_type.LineLengthIndex) return dns_types.Query_Type
   is
      firstChar : character;
      lastChar : character;
      lengthToken : rr_type.LineLengthIndex;
      retval : dns_types.Query_Type;
   begin
      firstChar := Ada.Characters.Handling.to_upper(s(begIdx));
      lastChar := Ada.Characters.Handling.to_upper(s(endIdx));
      lengthToken := (endIdx-begIdx)+1;
      --ugly but it gets the job done
      --A
      if firstChar = 'A' and lengthToken = 1 then
         Retval := Dns_Types.A;
      --NS
      elsif firstChar = 'N' and lastChar = 'S' and lengthToken = 2 then
         Retval := Dns_Types.NS;
      --MX
      elsif firstChar = 'M' and lastChar = 'X' and lengthToken = 2 then
         Retval := Dns_Types.MX;
      --RP
      elsif firstChar = 'R' and lastChar = 'P' and lengthToken = 2 then
         Retval := DNS_Types.UNIMPLEMENTED;
      --PTR
      --formulation of third condition helps prover
      elsif firstChar = 'P' and lastChar = 'R' and begIdx <= endIdx-2 then
         if Ada.Characters.Handling.to_upper(s(begIdx+1)) = 'T' then
            Retval := Dns_Types.PTR;
         else
            RetVal := dns_types.ERROR;
         end if;
      --SOA
      --formulation of third condition helps prover
      elsif firstChar = 'S' and lastChar = 'A' and begIdx <= endIdx-2 then
         if Ada.Characters.Handling.to_upper(s(begIdx+1)) = 'O' then
            Retval := dns_types.SOA;
         else
            RetVal := dns_types.ERROR;
         end if;
      --SRV
      --formulation of third condition helps prover
      elsif firstChar = 'S' and lastChar = 'V' and begIdx <= endIdx-2 then
         if Ada.Characters.Handling.to_upper(s(begIdx+1)) = 'R' then
            Retval := dns_types.UNIMPLEMENTED;
         else
            RetVal := dns_types.ERROR;
         end if;
      --TXT
      --formulation of third condition helps prover
      elsif firstChar = 'T' and lastChar = 'T' and begIdx <= endIdx-2 then
         if Ada.Characters.Handling.to_upper(s(begIdx+1)) = 'X' then
            Retval := dns_types.UNIMPLEMENTED;
         else
            RetVal := dns_types.ERROR;
         end if;
      --AAAA
      --formulation of third condition helps prover
      elsif firstChar = 'A' and lastChar = 'A' and begIdx <= endIdx-3 then
         if Ada.Characters.Handling.to_upper(s(begIdx+1)) = 'A' and
            Ada.Characters.Handling.to_upper(s(begIdx+2)) = 'A' then
            Retval := Dns_Types.AAAA;
         else
            RetVal := Dns_Types.ERROR;
         end if;
      --NSEC
      --formulation of third condition helps prover
      elsif firstChar = 'N' and lastChar = 'C' and begIdx <= endIdx-3 then
         if Ada.Characters.Handling.to_upper(s(begIdx+1)) = 'S' and
            Ada.Characters.Handling.to_upper(s(begIdx+2)) = 'E' then
            Retval := Dns_Types.NSEC;
         else
            RetVal := Dns_Types.ERROR;
         end if;
      --CNAME
      --formulation of third condition helps prover
      elsif firstChar = 'C' and lastChar = 'E' and begIdx <= endIdx-4 then
         if Ada.Characters.Handling.to_upper(s(begIdx+1)) = 'N' and
            Ada.Characters.Handling.to_upper(s(begIdx+2)) = 'A' and
            Ada.Characters.Handling.to_upper(s(begIdx+3)) = 'M' then
            Retval := Dns_Types.CNAME;
         else
            RetVal := Dns_Types.ERROR;
         end if;
      --RRSIG
      --formulation of third condition helps prover
      elsif firstChar = 'R' and lastChar = 'G' and begIdx <= endIdx-4 then
         if Ada.Characters.Handling.to_upper(s(begIdx+1)) = 'R' and
            Ada.Characters.Handling.to_upper(s(begIdx+2)) = 'S' and
            Ada.Characters.Handling.to_upper(s(begIdx+3)) = 'I' then
            Retval := Dns_Types.RRSIG;
         else
            RetVal := Dns_Types.ERROR;
         end if;
      --DNSKEY
      --formulation of third condition helps prover
      elsif firstChar = 'D' and lastChar = 'Y' and begIdx <= endIdx-5 then
         if Ada.Characters.Handling.to_upper(s(begIdx+1)) = 'N' and
            Ada.Characters.Handling.To_Upper(S(BegIdx+2)) = 'S' and
            Ada.Characters.Handling.to_upper(s(begIdx+3)) = 'K' and
            Ada.Characters.Handling.To_Upper(S(BegIdx+4)) = 'E' then
            RetVal := Dns_Types.DNSKEY;
         else
            RetVal := Dns_Types.ERROR;
         end if;
      ELSE
         RetVal := Dns_Types.UNIMPLEMENTED;
      end if;
      return retval;
   end GetRecordType;

 procedure CheckAndAppendOrigin(target : in out Rr_Type.DomainNameStringType;
         Origin : in Rr_Type.DomainNameStringType;
         CurrentLine : in rr_type.LineFromFileType;
         LastPos : in Rr_Type.Linelengthindex;
         LineCount : in unsigned_types.unsigned32;
         Success : in out boolean
         )
   is
   begin
      --if target does not end in '.', append Origin string
      if target(Rr_Type.DomainNameLength(target)) /= '.'then
         if origin = Rr_Type.BlankDomainName then
            error_msgs.printBlankOriginWarning(currentLine, lastPos, lineCount);
         else
            Rr_Type.appendDomainNames(target, origin, Success);
            if not Success then
               error_msgs.printAppendDomainLengthErrorInfo(currentLine, lastPos, lineCount);
            end if;
         end if;
      end if; --check for domain name ending in '.'
   end CheckAndAppendOrigin;

 procedure checkValidHostName(Name: Rr_Type.DomainNameStringType; success: in out boolean)
      is
         Length: Rr_Type.DomainNameStringTypeIndex;
         retVal : boolean := true;
      begin
         --only characters allowed are alphanumerics, '.' and '-'

         Length := Rr_Type.DomainNameLength(Name);

         --has to start and end with alphanumeric
         --use length-1 because name(length) is always a period, doesn't count
         --SPARK caught runtime error for length=1, nice!
         if Name(1) = '.' or Name(1) = '-' or (length > 1 and then (Name(Length-1) = '.' or Name(Length-1) = '-')) then
            RetVal := False;
         else
            for I in Integer range 2..Length-2 loop
               --# assert true;
               --periods must come after alphanumeric
               if Name(I) = '.' then
                  if (Name(I-1) = '.' or Name(I-1) = '-') then
                     RetVal := False;
                  end if;
               --if not period, must be alphanumeric or underscore
               elsif not Ada.Characters.Handling.Is_Alphanumeric(Name(I)) and Name(I) /= '-' then
                  RetVal := False;
               end if;
            end loop;
         end if;
         success := success and retVal;
      end CheckValidHostName;

   --Just like findNextToken below , but starts at beginning of line, no index values
   --returned.  Only used to see what the first token on the line is.
   procedure findFirstToken(s : in rr_type.LineFromFileType; length : in rr_type.LineLengthIndex;
                            T : out rr_type.rrItemType)
   is

      begIdx : rr_type.LineLengthIndex := 1;
      endIdx : rr_type.LineLengthIndex;
      ContainsOnlyNumbers : boolean;
      containsOnlyLetters : boolean;
      ContainsPeriod : Boolean;
      containsColon : boolean;
      ContainsDecimalNumbers : Boolean;
      ContainsHexNumbers : Boolean;
      ContainsLetters : Boolean;
      --NOTE:  Can domain names have anything other than letters, numbers or periods?
   BEGIN
      while begIdx < length and then (s(begIdx) = BLANK or s(begIdx) = TAB) loop
         --# assert begIdx >= 1 and begIdx < length;
         begIdx := begIdx + 1;
      end loop;

      endIdx := begIdx;
      while endIdx < length and then (s(endIdx) /= BLANK and s(endIdx) /= TAB
         and s(endIdx) /= COMMENT_CHAR and s(endIdx) /= L_PAREN and s(endIdx) /= R_PAREN) loop
         --# assert begIdx >= 1 and begIdx <= length and
         --#    endIdx >= begIdx and endIdx < length;
         endIdx := endIdx + 1;
      END LOOP;
      if (s(endIdx) = BLANK or s(endIdx) = TAB or s(endIdx) = COMMENT_CHAR) and endIdx > 1 and endIdx > begIdx then
         EndIdx := EndIdx-1;
      end if;
      ContainsOnlyNumbers := true;
      ContainsOnlyLetters := True;

      ContainsPeriod := False;
      containsColon := false;
      ContainsDecimalNumbers := False;
      ContainsHexNumbers := false;
      ContainsLetters := False;

      FOR I IN Integer RANGE BegIdx..EndIdx LOOP
         --# assert begIdx >= 1 and begIdx <= length and
         --#    endIdx >= begIdx and endIdx <= length;
         ContainsOnlyNumbers := ContainsOnlyNumbers AND Ada.Characters.Handling.Is_Digit(S(I));
         ContainsDecimalNumbers := ContainsDecimalNumbers or Ada.Characters.Handling.Is_Digit(S(I));
         containsHexNumbers := containsHexNumbers or Ada.Characters.Handling.Is_Hexadecimal_Digit(S(I));
         ContainsOnlyLetters := ContainsOnlyLetters AND Ada.Characters.Handling.Is_Letter(S(I));
         containsLetters := containsLetters or Ada.Characters.Handling.Is_Letter(S(I));
         ContainsPeriod := ContainsPeriod or S(I) = '.';
         ContainsColon := ContainsColon or S(I) = ':';
      end loop;

      --figure out what we've got
      IF S(BegIdx) = COMMENT_CHAR then
         T := Rr_Type.Comment;
      elsIF S(BegIdx) = CONTROL_CHAR then
         T := Rr_Type.Control;
      elsIF S(BegIdx) = L_PAREN THEN
         T := Rr_Type.LParen;
      elsIF S(BegIdx) = R_PAREN THEN
         T := Rr_Type.RParen;
      --@ counts as a domain name, will be replaced with $ORIGIN
      --. counts as domain name, could appear as value of $ORIGIN
      elsIf (S(BegIdx) = ORIGIN_CHAR or S(BegIdx) = '.') and BegIdx = EndIdx then
         T := Rr_Type.DomainNameOrTimeSpec;
      elsif containsDecimalNumbers and containsOnlyNumbers then
         T := Rr_Type.Number;
      elsif containsDecimalNumbers and containsPeriod and not containsLetters and not containsColon then
         T := Rr_Type.Ipv4;
      elsif containsHexNumbers and containsColon and not containsPeriod then
         T := Rr_Type.Ipv6;
      elsif containsOnlyLetters and isClass(s,begIdx,endIdx) then
         T := rr_Type.Class;
      elsif containsOnlyLetters and isRecord(s,begIdx,endIdx) then
         T := rr_Type.RecordIndicator;
      elsif containsLetters then
         T := Rr_Type.DomainNameOrTimeSpec;
      elsif BegIdx = Length then  --this means blank line, treated as a comment
         T := rr_type.Comment;
      else --error of some sort, e.g. invalid class or record type
         T := Rr_Type.Other;
      end if;
      --Ada.Text_Io.Put("TOKEN = " & Rr_Type.RrItemType'Image(T));
      --ada.text_io.new_line;

   end findFirstToken;


   procedure findNextToken(s : in rr_type.LineFromFileType; length : in rr_type.LineLengthIndex;
                           begIdx : in out rr_type.LineLengthIndex;
                           endIdx : out rr_type.LineLengthIndex;
                           T : out rr_type.rrItemType)
   is
      ContainsOnlyNumbers : boolean;
      containsOnlyLetters : boolean;
      ContainsPeriod : Boolean;
      containsColon : boolean;
      ContainsDecimalNumbers : Boolean;
      ContainsHexNumbers : Boolean;
      ContainsLetters : Boolean;
      --NOTE:  Can domain names have anything other than letters, numbers or periods?
   BEGIN
      while begIdx < length and then (s(begIdx) = BLANK or s(begIdx) = TAB) loop
         --# assert begIdx < length;
         begIdx := begIdx + 1;
      end loop;
      endIdx := begIdx;
      while endIdx < length and then (s(endIdx) /= BLANK and s(endIdx) /= TAB
            and S(EndIdx) /= COMMENT_CHAR and S(EndIdx) /= L_PAREN and S(EndIdx) /= R_PAREN
         and s(endIdx) /= ORIGIN_CHAR) loop
         --# assert begIdx <= length and
         --#    endIdx >= begIdx and endIdx < length;
         endIdx := endIdx + 1;
      END LOOP;
      if (S(EndIdx) = BLANK or S(EndIdx) = TAB or S(EndIdx) = COMMENT_CHAR)
         and endIdx > 1 and endIdx > begIdx then
         EndIdx := EndIdx-1;
      end if;

      ContainsOnlyNumbers := true;
      ContainsOnlyLetters := True;

      ContainsPeriod := False;
      containsColon := false;
      ContainsDecimalNumbers := False;
      ContainsHexNumbers := false;
      ContainsLetters := False;

      FOR I IN Integer RANGE BegIdx..EndIdx LOOP
         ContainsOnlyNumbers := ContainsOnlyNumbers AND S(I) >= '0' AND S(I) <= '9';
         --# assert begIdx >= 1 and begIdx <= length and endIdx >= begIdx and endIdx <= length
         --#    and EndIdx = EndIdx%
         --#    and ((ContainsOnlyNumbers = true)  ->
         --#         (for all J in integer range BegIdx..I => (S(J) >= '0' AND S(J) <= '9')));
         ContainsDecimalNumbers := ContainsDecimalNumbers or (S(I) >= '0' AND S(I) <= '9');
         containsHexNumbers := containsHexNumbers or Ada.Characters.Handling.Is_Hexadecimal_Digit(S(I));
         ContainsOnlyLetters := ContainsOnlyLetters AND Ada.Characters.Handling.Is_Letter(S(I));
         containsLetters := containsLetters or Ada.Characters.Handling.Is_Letter(S(I));
         ContainsPeriod := ContainsPeriod or S(I) = '.';
         ContainsColon := ContainsColon or S(I) = ':';
      end loop;

      --figure out what we've got
      IF S(BegIdx) = COMMENT_CHAR then
         T := Rr_Type.Comment;
      elsIF S(BegIdx) = CONTROL_CHAR then
         T := Rr_Type.Control;
      elsIF S(BegIdx) = L_PAREN THEN
         T := Rr_Type.LParen;
      elsIF S(BegIdx) = R_PAREN THEN
         T := Rr_Type.RParen;
      --@ counts as a domain name, will be replaced with $ORIGIN
      --. counts as domain name, could appear as value of $ORIGIN
      elsIf (S(BegIdx) = ORIGIN_CHAR or S(BegIdx) = '.') and BegIdx = EndIdx then
         T := Rr_Type.DomainNameOrTimeSpec;
      elsif ContainsOnlyNumbers then
         T := Rr_Type.Number;
      elsif containsDecimalNumbers and containsPeriod and not containsLetters and not containsColon then
         T := Rr_Type.Ipv4;
      elsif containsHexNumbers and containsColon and not containsPeriod then
         T := Rr_Type.Ipv6;
      elsif containsOnlyLetters and isClass(s,begIdx,endIdx) then
         T := rr_Type.Class;
      elsif containsOnlyLetters and isRecord(s,begIdx,endIdx) then
         T := rr_Type.RecordIndicator;
      elsif containsLetters then
         T := Rr_Type.DomainNameOrTimeSpec;
      elsif BegIdx = Length then  --this means blank line, treated as a comment
         T := rr_type.Comment;
      else --error of some sort, e.g. invalid class or record type
         T := Rr_Type.Other;
      end if;
   END FindNextToken;

      --true if Char is s,m,h,d,w or uppercase equivalent
      function IsMult(Char: in Character) return Boolean
      is
         C : Character;
         retVal : boolean;
      begin
         C := Ada.Characters.Handling.To_Upper(Char);
         if C = 'S' or C = 'M' or C = 'H' or C = 'D' or C = 'W' then
            RetVal := True;
         else
            RetVal := False;
         end if;
         return retVal;
      end IsMult;

   procedure convert8BitUnsigned(value: out Unsigned_Types.Unsigned8;
      ZoneFileLine : in rr_type.LineFromFileType;
      BegIdx : in rr_type.LineLengthIndex;
      EndIdx : in rr_type.LineLengthIndex;
      Success : in out boolean)
   is
      digitVal : natural;
      tmpVal : natural;
   begin
      tmpVal := 0;
      value := 0;
      for i in integer range begIdx..endIdx loop
         --# assert begIdx >= 1 and tmpVal <= unsigned_types.MAX_8BIT_VAL;
         digitVal := Character'Pos(zoneFileLine(I)) - Character'Pos('0');
         TmpVal := 10*TmpVal + DigitVal;
         exit when tmpVal > unsigned_types.MAX_8BIT_VAL;
      end loop;
      --make sure it's not too big
      if tmpVal > unsigned_types.MAX_8BIT_VAL then
         success := false;
      else --have a valid preference value
         value := unsigned_types.Unsigned8(tmpVal);
      end if;
   end convert8BitUnsigned;

   procedure convert16BitUnsigned(value: out Unsigned_Types.Unsigned16;
      ZoneFileLine : in rr_type.LineFromFileType;
      BegIdx : in rr_type.LineLengthIndex;
      EndIdx : in rr_type.LineLengthIndex;
      Success : in out boolean)
   is
      digitVal : natural;
      tmpVal : natural;
   begin
      tmpVal := 0;
      value := 0;
      for i in integer range begIdx..endIdx loop
         --# assert begIdx >= 1 and tmpVal <= unsigned_types.MAX_16BIT_VAL;
         digitVal := Character'Pos(zoneFileLine(I)) - Character'Pos('0');
         TmpVal := 10*TmpVal + digitVal;
         exit when tmpVal > unsigned_types.MAX_16BIT_VAL;
      end loop;
      --make sure it's not too big
      if tmpVal > unsigned_types.MAX_16BIT_VAL then
         success := false;
      else --have a valid 16-bit value
         value := unsigned_types.Unsigned16(tmpVal);
      end if;
   end Convert16BitUnsigned;

   procedure convert32BitUnsigned(value: out Unsigned_Types.Unsigned32;
      ZoneFileLine : in rr_type.LineFromFileType;
      BegIdx : in rr_type.LineLengthIndex;
      EndIdx : in rr_type.LineLengthIndex;
      Success : in out boolean)
   is
      digitVal : long_long_integer;
      tmpVal : long_long_integer;
   begin
      tmpVal := 0;
      value := 0;
      for i in integer range begIdx..endIdx loop
         --# assert begIdx >= 1 and tmpVal >= 0 and tmpVal <= unsigned_types.MAX_32BIT_VAL;
         digitVal := Character'Pos(zoneFileLine(I)) - Character'Pos('0');
         TmpVal := 10*TmpVal + digitVal;
         exit when tmpVal > unsigned_types.MAX_32BIT_VAL;
      end loop;
      --make sure it's not too big
      if tmpVal > unsigned_types.MAX_32BIT_VAL then
         success := false;
      else --have a valid 32-bit value
         value := unsigned_types.Unsigned32(tmpVal);
      end if;
   end convert32BitUnsigned;

   --converts the time specifier at s(begIdx..endIdx) into a time value in seconds, returned in RetVal.
   --a time specifier is either a Num or one or more Blobs.  A Num is a string of decimal digits.
   --A Blob is a Num followed by a Mult.  A Mult is one of {s,m,h,w,d} and their upper-case equivalents.
   --Sets Success to false if the time specifier is invalid
   procedure ConvertTimeSpec(S : in Rr_Type.LineFromFileType; begIdx: in rr_type.LineLengthIndex;
      EndIdx: in Rr_Type.LineLengthIndex; RetVal : out Unsigned_Types.Unsigned32; Success: in out Boolean)
   is
      Tmp : Long_Long_Integer := 0;
      Blob : Long_Long_Integer := 0;
      CurrentChar : Character;

      function MultValue(Char : in Character) return natural
      --# pre isMult(Char);
      --# return Value => Value >= 0 and Value <= 60*60*24*7;
            is
         C : Character;
         retVal : natural;
      begin
         C := Ada.Characters.Handling.To_Upper(Char);
         case C is
            when 'S' =>
               RetVal := 1;
            when 'M' =>
               RetVal := SecondsInAMinute;
            when 'H' =>
               RetVal := SecondsInAMinute*MinutesInAnHour;
            when 'D' =>
               RetVal := (SecondsInAMinute*MinutesInAnHour)*HoursInADay;
            when 'W' =>
               RetVal := ((SecondsInAMinute*MinutesInAnHour)*HoursInADay)*DaysInAWeek;
            when others =>
               RetVal := 0;   --will never happen if precondition is met
         end case;
         return RetVal;
      end MultValue;

   begin
      for I in Integer range BegIdx..EndIdx loop
         --#assert Blob >= 0 and Blob <= Rr_Type.SOA_Record_Type.MAX_TIME_VAL
         --#and Tmp >= 0 and Tmp <= Rr_Type.SOA_Record_Type.MAX_TIME_VAL;
         CurrentChar := s(I);
         if CurrentChar >= '0' and CurrentChar <= '9' then
            Blob := 10*Blob + (Character'Pos(CurrentChar)-Character'Pos('0'));
            if Blob > Rr_Type.SOA_Record_Type.MAX_TIME_VAL then
               Success := False;
            end if;
         elsif IsMult(CurrentChar) then
            if (Blob = 0) then
               Success := False;
            else
               Blob := Blob*long_long_integer(MultValue(CurrentChar));
               Tmp := Tmp + Blob;
               if Blob > Rr_Type.Soa_Record_Type.MAX_TIME_VAL or Tmp > Rr_Type.Soa_Record_Type.MAX_TIME_VAL then
                  Success := False;
               end if;
               Blob := 0;
            end if;
         else
            Success := False;
         end if;
         exit when Success = False;
      end loop;
      if not Success then
         Tmp := 0;
      elsif Tmp = 0 then  --handle case when time specifier is just a number (no mult)
         Tmp := Blob;
      end if;
      retVal := unsigned_types.unsigned32(Tmp);
   end ConvertTimeSpec;

   procedure ConvertTimeString(TimeVal : out Unsigned_Types.Unsigned32;
      TimeString: in Rr_Type.Rrsig_Record_Type.TimeStringType;
      Success : in out boolean)
   is
      Year : Natural := 0;
      Month : Natural:= 0;
      Day : Natural := 0;
      Hour: Natural := 0;
      Minute : Natural := 0;
      Second : Natural := 0;
      Num : Natural;
   begin
      for I in Rr_Type.Rrsig_Record_Type.TimeStringTypeIndex loop
         --help the prover show these values remain in type
         --#assert year <= rr_type.rrsig_record_type.MAX_YEAR and month <= 12 and day <= 31
         --#   and hour <= 23 and minute <= 59 and second <= 59;
         Num := Character'Pos(TimeString(I))- Character'Pos('0');
         if (I <= 4) then
            Year := 10*Year + Num;
         elsif (I <= 6) then
            Month := 10*Month + Num;
         elsif (I <= 8) then
            Day:= 10*Day + Num;
         elsif (I <= 10) then
            Hour := 10*Hour + Num;
         elsif (I <= 12) then
            Minute:= 10*Minute + Num;
         elsif (I <= 14) then --flagged, but leave as is in case TimeStringTypeIndex ever changed
            Second := 10*Second + Num;
         end if;
         exit when Year > Rr_Type.Rrsig_Record_Type.MAX_YEAR or Month > MonthsInAYear
            or Day > MaxDaysInAMonth or Hour >= HoursInADay or Minute >= MinutesInAnHour
            or Second >= SecondsInAMinute;
      end loop;
      if (Year > Rr_Type.Rrsig_Record_Type.MAX_YEAR or Month > MonthsInAYear
         or Day > MaxDaysInAMonth or Hour >= HoursInADay or Minute >= MinutesInAnHour
         or Second >= SecondsInAMinute) then
         Success := False;
         timeVal := 0;
      else
         timeVal := Non_Spark_Stuff.Time_Of(Year,Month,Day,Hour,Minute,Second);
      end if;
   end ConvertTimeString;

   procedure AddToKeyR(RRSIG_Rec : in out rr_type.RRSIG_Record_type.rrsigrecordtype;
      s: in Rr_Type.LineFromFileType;
      Length : in Rr_Type.LineLengthIndex;
      AllDone : out boolean;    --this is needed because line might terminate with ')'
      success : in out boolean)
   is
      BegIdx : Rr_Type.LineLengthIndex;
      EndIdx : Rr_Type.LineLengthIndex;
      returnedType : rr_type.rrItemType;
   begin
      --find start of key fragment
      BegIdx := 1;
      while begIdx < length and then (s(begIdx) = BLANK or s(begIdx) = TAB) loop
         --# assert begIdx >= 1 and begIdx < length;
         begIdx := begIdx + 1;
      end loop;

      --find end of key fragment
      EndIdx := BegIdx;
      while EndIdx < length and then (s(EndIdx) /= BLANK and s(EndIdx) /= TAB) loop
         --# assert begIdx >= 1 and begIdx <= endIdx and endIdx >= 1 and endIdx < length;
         endIdx:= endIdx + 1;
      end loop;

      --check if key too long
      if RRSIG_Rec.SignatureLength + ((EndIdx-BegIdx)+1) > Rr_Type.RRSIG_Record_Type.MaxRRSigLength then
         Success := False;
      else
         --add key fragment on to key field of record
         for I in Integer range BegIdx..EndIdx loop
            --# assert BegIdx = begIdx% and EndIdx = EndIdx% and I >= BegIdx and I <= EndIdx
            --# and BegIdx >= 1
            --# and RRSIG_Rec.SignatureLength + ((EndIdx-BegIdx)+1) <= Rr_Type.RRSIG_Record_Type.MaxRRSigLength;
            RRSIG_Rec.Signature(RRSIG_Rec.SignatureLength+((I-BegIdx)+1)) := S(I);
         end loop;
         --update signature length
         RRSIG_Rec.SignatureLength := RRSIG_Rec.SignatureLength + ((endIdx-begIdx) + 1);
      end if;

      --SPARK caught runtime error if no ')', nice!
      if EndIdx < Length then
         --if line ends with right paren then all done
         begIdx := endIdx + 1;
         FindNextToken(s, Length, BegIdx, EndIdx, ReturnedType);

         --if ')' found, record complete, can insert in table
         --begIdx <= endIdx always true, makes flow errors go away
         if ReturnedType = Rr_Type.RParen and BegIdx <= EndIdx then
            AllDone := True;
         else
            AllDone := False;   --makes flow error go away
         end if;
      else   --nothing after the key fragment, need to keep going
         AllDone := False;
      end if;
   end AddToKeyR;

   procedure AddToKey(DNSKEY_Rec : in out rr_type.dnskey_record_type.DNSKeyRecordType;
      s: in Rr_Type.LineFromFileType;
      Length : in Rr_Type.LineLengthIndex;
      success : in out boolean)
   is
      BegIdx : Rr_Type.LineLengthIndex;
      endIdx : rr_type.LineLengthIndex;
   begin
      --find start of key fragment
      BegIdx := 1;
      while begIdx < length and then (s(begIdx) = BLANK or s(begIdx) = TAB) loop
         --# assert begIdx >= 1 and begIdx < length;
         begIdx := begIdx + 1;
      end loop;

      --find end of key fragment
      EndIdx := BegIdx;
      while EndIdx < length and then (s(EndIdx) /= BLANK and s(EndIdx) /= TAB) loop
         --# assert begIdx >= 1 and begIdx <= endIdx and endIdx >= 1 and endIdx < length;
         endIdx:= endIdx + 1;
      end loop;

      --check if key too long
      if DNSKEY_Rec.KeyLength + ((EndIdx-BegIdx)+1) > Rr_Type.Dnskey_Record_Type.MaxDNSKeyLength then
         Success := False;
      else
         --add key fragment on to key field of record
         for I in Integer range BegIdx..EndIdx loop
            --# assert BegIdx = begIdx% and EndIdx = EndIdx% and I >= BegIdx and I <= EndIdx
            --# and BegIdx >= 1
            --# and DNSKEY_Rec.KeyLength + ((EndIdx-BegIdx)+1) <= Rr_Type.Dnskey_Record_Type.MaxDNSKeyLength;
            DNSKEY_Rec.Key(DNSKEY_Rec.KeyLength+((I-BegIdx)+1)) := S(I);
         end loop;
         --update key length
         DNSKEY_Rec.KeyLength := DNSKEY_Rec.KeyLength + ((EndIdx-BegIdx) + 1);
      end if;
   end AddToKey;

   --convert ipv6 address at s(begIdx..endIdx) into 8 unsigned 16-bit integers
   --expects eight valid 16-bit hex numbers separated by colons
   --returns INVALID_IPV6_ADDR if conversion unsuccessful
   function convertIpv6(s : in rr_type.LineFromFileType;
                        begIdx: in rr_type.LineLengthIndex;
                        endIdx: in rr_type.LineLengthIndex) RETURN rr_type.aaaa_record_type.IPV6AddrType
   is
      NUM_FIELDS: constant Natural := 8;
      REQ_NUM_SEPARATORS : constant Natural := NUM_FIELDS-1;
      FIELD_SEPARATOR : constant Character := ':';
      subtype SeparatorIndexType is Integer range 1..REQ_NUM_SEPARATORS+1;  --could go one higher if processing bad ip
      type SeparatorIndexArrayType is array(SeparatorIndexType) of Rr_Type.LineLengthIndex;

      MAX_DIGITS_IN_FIELD: constant Natural := 4;
      IPV6_RADIX : constant unsigned_types.unsigned16 := 16;

      SeparatorIndexArray : SeparatorIndexArrayType := SeparatorIndexArrayType'(others => 1);
      Ctr : Rr_Type.LineLengthIndex;
      NumSeparators : Natural;
      fieldTotal : unsigned_types.unsigned16;
      numDigitsInField : Natural;

      RetVal : Rr_Type.Aaaa_Record_Type.IPV6AddrType := Rr_Type.Aaaa_Record_Type.INVALID_IPV6_ADDR;

      function SeparatorsOK(Line : in Rr_Type.LineFromFileType; SArray : in SeparatorIndexArrayType) return Boolean is
         retVal : boolean := true;
      begin
         --no need to check the last entry, that will have been detected elsewhere
         for I in integer range SeparatorIndexType'First..SeparatorIndexType'last-1 loop
            --# assert true;
            if line(SArray(I)) /= FIELD_SEPARATOR or SArray(I) = SArray(I+1) - 1 then
               RetVal := false;
               exit;
            end if;
         end loop;
         return retVal;
      end SeparatorsOK;

   begin
      Ctr := BegIdx;
      NumSeparators := 0;
      FieldTotal := 0;
      NumDigitsInField := 0;

      while Ctr < EndIdx and then (NumSeparators <= REQ_NUM_SEPARATORS and NumDigitsInField <= MAX_DIGITS_IN_FIELD) loop
         --# assert ctr < endIdx and numSeparators <= REQ_NUM_SEPARATORS and numDigitsInField <= MAX_DIGITS_IN_FIELD;
         if Ada.Characters.Handling.Is_Decimal_Digit(S(Ctr)) then
            fieldTotal := IPV6_RADIX*fieldTotal + (Character'Pos(S(Ctr))-Character'Pos('0'));
            NumDigitsInField := NumDigitsInField +1;
         elsif Ada.Characters.Handling.Is_Hexadecimal_Digit(S(Ctr)) then
            FieldTotal := IPV6_RADIX*FieldTotal + (Character'Pos(Ada.Characters.Handling.To_Upper(S(Ctr)))-Character'Pos('A'))+10;
            NumDigitsInField := NumDigitsInField +1;
         else
            NumSeparators := NumSeparators+1;
            SeparatorIndexArray(NumSeparators) := Ctr;
            retVal(numSeparators) := fieldTotal;
            fieldTotal := 0;
            numDigitsInField := 0;
         end if;
         ctr := ctr + 1;
      end loop;
      --endIdx might be the maximum value possible, so must catch last character here
      if Ctr = EndIdx and numSeparators <= REQ_NUM_SEPARATORS then
         if Ada.Characters.Handling.Is_Decimal_Digit(S(Ctr)) then
            fieldTotal := IPV6_RADIX*fieldTotal + (Character'Pos(S(Ctr))-Character'Pos('0'));
            NumDigitsInField := NumDigitsInField +1;
            retVal(numSeparators+1) := fieldTotal;
         elsif Ada.Characters.Handling.Is_Hexadecimal_Digit(S(Ctr)) then
            FieldTotal := IPV6_RADIX*FieldTotal + (Character'Pos(Ada.Characters.Handling.To_Upper(S(Ctr)))-Character'Pos('A'))+10;
            NumDigitsInField := NumDigitsInField +1;
            retVal(numSeparators+1) := fieldTotal;
         else  --if last character is anything but an integer, force an error
            NumSeparators := NUM_FIELDS;
         end if;
      end if;

      if not (SeparatorsOK(s, SeparatorIndexArray) and numSeparators = REQ_NUM_SEPARATORS and numDigitsInField <= MAX_DIGITS_IN_FIELD) then
         RetVal := Rr_Type.Aaaa_Record_Type.INVALID_IPV6_ADDR;
      end if;

      return retVal;
   end ConvertIpv6;

   --convert ipv4 address  at s(begIdx..endIdx) into a 32-bit number
   --returns 0 if conversion unsuccessful
   function convertIpv4(s : in rr_type.LineFromFileType;
                        begIdx: in rr_type.LineLengthIndex;
                        endIdx: in rr_type.LineLengthIndex) return unsigned_types.unsigned32
         is
      NUM_BYTES : constant Natural := 4;
      REQ_NUM_SEPARATORS : constant Natural := NUM_BYTES-1;
      BYTE_SEPARATOR : constant Character := '.';
      subtype SeparatorIndexType is Integer range 1..REQ_NUM_SEPARATORS+1;  --could go one higher if processing bad ip
      type SeparatorIndexArrayType is array(SeparatorIndexType) of Rr_Type.LineLengthIndex;

      MAX_BYTE_VALUE : CONSTANT unsigned_types.unsigned32 := 255;
      MAX_DIGITS_PER_BYTE : constant Natural := 3;
      IPV4_RADIX : constant unsigned_types.unsigned32:= 10;

      SeparatorIndexArray : SeparatorIndexArrayType := SeparatorIndexArrayType'(others => 1);

      Ctr : Rr_Type.LineLengthIndex;
      NumSeparators : Natural;
      ByteTotal : unsigned_types.unsigned32;
      numDigitsInByte : Natural;
      GrandTotal : unsigned_types.unsigned32;
      RetVal : Unsigned_Types.Unsigned32;

      function SeparatorsOK(Line : in Rr_Type.LineFromFileType; SArray : in SeparatorIndexArrayType) return Boolean is
         retVal : boolean ;
      begin
         if Line(SArray(1)) = BYTE_SEPARATOR and Line(SArray(2)) = BYTE_SEPARATOR and Line(SArray(3)) = BYTE_SEPARATOR
               and Sarray(1) /= Sarray(2)-1 and Sarray(2) /= Sarray(3)-1 then
            retVal := true;
         else
            retVal := False;
         end if;
         return retVal;
      end SeparatorsOK;

   begin
      Ctr := BegIdx;
      NumSeparators := 0;
      ByteTotal := 0;
      numDigitsInByte := 0;
      GrandTotal := 0;
      while Ctr < endIdx and then (NumSeparators <= REQ_NUM_SEPARATORS and ByteTotal <= MAX_BYTE_VALUE
            and numDigitsInByte <= MAX_DIGITS_PER_BYTE) loop
         --# assert ctr < endIdx and numSeparators <= REQ_NUM_SEPARATORS and numDigitsInByte <= MAX_DIGITS_PER_BYTE;
         if Ada.Characters.Handling.Is_Digit(S(Ctr)) then
            ByteTotal := IPV4_RADIX*ByteTotal + (Character'Pos(S(Ctr))-Character'Pos('0'));
            numDigitsinbyte := numDigitsInByte+1;
         else
            NumSeparators := NumSeparators+1;
            SeparatorIndexArray(NumSeparators) := Ctr;
            grandTotal := (MAX_BYTE_VALUE+1)*grandTotal + byteTotal;
            ByteTotal := 0;
            numDigitsInByte := 0;
         end if;
         ctr := ctr+1;
      end loop;

      --endIdx might be the maximum value possible, so must catch last character here
      if Ctr = EndIdx then
         if Ada.Characters.Handling.Is_Digit(S(Ctr)) then
            ByteTotal := 10*ByteTotal + (Character'Pos(S(Ctr))-Character'Pos('0'));
            NumDigitsinbyte := NumDigitsInByte+1;
            grandTotal := (MAX_BYTE_VALUE+1)*grandTotal + byteTotal;
         else  --if last character is anything but an integer, force an error
            NumSeparators := NUM_BYTES;
         end if;
      end if;
      if SeparatorsOK(s, separatorIndexArray) and NumSeparators = REQ_NUM_SEPARATORS and NumDigitsInByte <= MAX_DIGITS_PER_BYTE
            and ByteTotal <= MAX_BYTE_VALUE then
         RetVal := grandTotal;
      else
         RetVal := rr_type.a_record_type.INVALID_IPV4_ADDR;
      end if;
      return retVal;
   end ConvertIpv4;



END Parser_Utilities;

