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

WITH Ada.Characters.Handling, Error_Msgs, Parser_Utilities, Rr_Type.Aaaa_Record_Type,
   Rr_Type.Mx_Record_Type, Rr_Type.Soa_Record_Type,
   dns_types, unsigned_types, Ada.Characters.Latin_1;
--With Ada.Text_IO, ada.Integer_Text_IO;
USE TYPE Rr_Type.RrItemType, Rr_Type.Aaaa_Record_Type.IPV6AddrType,
   Dns_Types.Query_Type, Unsigned_Types.Unsigned32, Unsigned_Types.Unsigned16,
      unsigned_types.Unsigned8;

package body zone_file_parser is

   procedure ParseDNSKeyHeader(DNSKEY_Rec : out rr_type.dnskey_record_type.DNSKeyRecordType;
      zoneFileLine : in rr_type.LineFromFileType;
      ZLength : in Rr_Type.LineLengthIndex;
      Success : in out Boolean)
   is
      CORRECT_PROTOCOL_VALUE : constant unsigned_types.Unsigned8 := 3;
      begIdx : rr_type.LineLengthIndex := 1;
      endIdx : rr_type.LineLengthIndex;
      foundType : rr_type.rrItemType;
   begin
      --SPARK requires all fields of OUT parameter records be assigned
      DNSKEY_Rec := Rr_Type.Dnskey_Record_Type.BlankDNSKeyRecord;

      --find the record type indicator, which must be present since this routine was called
      parser_utilities.findNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, foundType);

      --second condition always true if precondition of routine is met, but it helps the prover
      WHILE FoundType /= rr_type.RecordIndicator and endIdx < zlength LOOP
         --# assert endIdx < zlength;
         begIdx := ((begIdx-begIdx)+endIdx)+1;  --makes flow error go away
         parser_utilities.findNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, foundType);
      END LOOP;
      --parse FLAG field (unsigned 16-bit #)
      if (endIdx = zLength) then
         success := false;
      else
         BegIdx := EndIdx + 1;
         parser_utilities.findNextToken(zoneFileLine, zLength, begIdx, endIdx, foundType);
         if foundType /= rr_type.Number then
            success := false;
         ELSE --convert token to 16-bit number
            parser_utilities.convert16BitUnsigned(DNSKEY_Rec.flags, zoneFileLine, begIdx, endIdx, success);
         end if;
      end if;

      --parse PROTOCOL field (unsigned 8-bit #, must be 3)
      if Success then
         if (endIdx = zLength) then
            success := false;
         else
            begIdx := endIdx + 1;
            parser_utilities.findNextToken(zoneFileLine, zLength, begIdx, endIdx, foundType);
            if foundType /= rr_type.Number then
               success := false;
            ELSE --convert token to unsigned 8-bit number
               parser_utilities.convert8BitUnsigned(DNSKEY_Rec.protocol, zoneFileLine, begIdx, endIdx, success);
            end if;
            if DNSKEY_Rec.Protocol /= CORRECT_PROTOCOL_VALUE then
               Success := False;
            end if;
         end if;
      end if;

      --parse ALGORITHM field (unsigned 8-bit #)
      if Success then
         if (endIdx = zLength) then
            success := false;
         else
            begIdx := endIdx + 1;
            parser_utilities.findNextToken(zoneFileLine, zLength, begIdx, endIdx, foundType);
            if foundType /= rr_type.Number then
               success := false;
            ELSE --convert token to unsigned 8-bit number
               parser_utilities.convert8BitUnsigned(DNSKEY_Rec.algorithm, zoneFileLine, begIdx, endIdx, success);
            end if;
         end if;
      end if;
   end ParseDNSKeyHeader;

   procedure ParseRRSig2ndLine(RRSIG_Rec:  out rr_type.rrsig_record_type.RRSIGRecordType;
      zoneFileLine : in rr_type.LineFromFileType;
      ZLength : in Rr_Type.LineLengthIndex;
      Success : in out Boolean)
   is
      BegIdx : Rr_Type.LineLengthIndex;
      EndIdx : Rr_Type.LineLengthIndex;
      LengthOfToken : Rr_Type.LineLengthIndex;
      FoundType : Rr_Type.RrItemType;
      TimeString : Rr_Type.Rrsig_Record_Type.TimeStringType :=
         Rr_Type.Rrsig_Record_Type.TimeStringType'(others => ' ');
   begin
      RRSIG_Rec := Rr_Type.Rrsig_Record_Type.BlankRRSIGRecord;
      --parse SIG_INCEPTION field (YYYYMMDDHHmmSS or unsigned 32-bit #)
      begIdx := 1;
      parser_utilities.findNextToken(zoneFileLine, zLength, begIdx, endIdx, foundType);
      Success := Success and (foundType = rr_type.Number);   --fail if not a number

      if Success then   --next token is a number
         --convert token to unsigned 32-bit number
         --If it's not of length timeStringLength, can't be YYYYMMDDHHmmSS
         if ((EndIdx-BegIdx)+1 /= rr_type.rrsig_record_type.timeStringLength) then
            Parser_Utilities.Convert32BitUnsigned(RRSIG_Rec.OrigTTL, ZoneFileLine,
               BegIdx, EndIdx, Success);
         else --convert YYYYMMDDHHmmSS to #secs since 1 Jan 1970 00:00:00 UTC
            for I in Rr_Type.Rrsig_Record_Type.TimeStringTypeIndex loop
               --# assert begIdx = begIdx% and endIdx = endIdx% and zLength = zlength%
               --# and endIdx <= zLength
               --# and endIdx-begIdx = rr_type.rrsig_record_type.timeStringLength-1
               --# and I <= Rr_Type.Rrsig_Record_Type.TimeStringTypeIndex'Last;
               TimeString(I) := ZoneFileLine(BegIdx+(I-1));
            end loop;
            --establish and prove the precondition for convertTimeString (all numbers)
            for I in Rr_Type.Rrsig_Record_Type.TimeStringTypeIndex loop
               --# assert for all J in Integer range 1..I-1 => (TimeString(J) >= '0' and TimeString(J) <= '9');
               if (TimeString(I) < '0' or TimeString(I) > '9') then
                  Success := False;
                  exit;
               end if;
            end loop;
            if Success then
               parser_utilities.convertTimeString(RRSIG_Rec.SigInception, TimeString, Success);
            end if;
         end if; --timeString or simple number
      end if; --successful so far

      --KEY_TAG
      Success := Success and (EndIdx < ZLength);   --fail if at EOL
      if Success then
         begIdx := endIdx + 1;
         Parser_Utilities.FindNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, FoundType);
         Success := (FoundType = Rr_Type.Number);
         if Success then
            Parser_Utilities.Convert16BitUnsigned(RRSIG_Rec.KeyTag, ZoneFileLine, BegIdx,
               EndIdx, Success);
         end if;
      end if;

      --SIGNER_NAME
      Success := Success and (EndIdx < ZLength);   --fail if at EOL
      if Success then
         begIdx := endIdx + 1;
         Parser_Utilities.FindNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, FoundType);
         Success := (FoundType = Rr_Type.DomainNameOrTimeSpec);
         if Success then
            --SPARK caught these parens in the wrong place, nice!
            LengthofToken := (EndIdx-BegIdx)+1;
            if LengthOfToken > Rr_Type.MaxDomainNameLength then
               success := false;
               Error_Msgs.PrintDomainLengthErrorInfo(ZoneFileLine, BegIdx, EndIdx);
            else
               --have a domain name, but must still pad it
               for I in Integer range begIdx..endIdx loop
                  --# assert begIdx >= 1 and endIdx-begIdx+1 <= rr_type.MaxDomainNameLength
                  --# and endIdx = endIdx%;
                  RRSIG_Rec.signerName((I+1)-begIdx) := ZoneFileLine(I);
               end loop;
               for I in Integer range endIdx+1..rr_type.maxDomainNameLength loop
                  -- Remind prover that endIdx is in type
                  --# assert endIdx >= 1;
                  RRSIG_Rec.signerName(I) := ' ';
               end loop;
            end if;
         end if;
      end if;

      --to do
   end ParseRRSig2ndLine;

   --this is only called from ParseRRSigHeader, created because otherwise ParseRRSigHeader
   --is too complex for Examiner
   procedure ParseTypeCoveredAlgorithmNumLabels(RRSIG_Rec : in out Rr_Type.Rrsig_Record_Type.RRSIGRecordType;
      ZoneFileLine : in Rr_Type.LineFromFileType;
      ZLength : in Rr_Type.LineLengthIndex;
      EndIdx : in out Rr_Type.LineLengthIndex;
      Success : in out Boolean)
   --# derives RRSIG_Rec from RRSIG_Rec, zoneFileLine, zLength, endIDx, success
   --# & endIdx, success from zoneFileLine, zLength, endIdx, success;
  is
      begIdx : rr_type.LineLengthIndex;
      FoundType : Rr_Type.RrItemType;
   begin

      --TYPE_COVERED
      Success := Success and (EndIdx < ZLength);   --fail if at EOL
      if Success then
         begIdx := endIdx + 1;
         Parser_Utilities.FindNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, FoundType);
         Success := (FoundType = Rr_Type.RecordIndicator);
         if Success then
            RRSIG_Rec.TypeCovered :=  Parser_Utilities.GetRecordType(ZoneFileLine, BegIdx, EndIdx);
         end if;
      end if;

      --ALGORITHM
      Success := Success and (EndIdx < ZLength);   --fail if at EOL
      if Success then
         begIdx := endIdx + 1;
         Parser_Utilities.FindNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, FoundType);
         Success := (FoundType = Rr_Type.Number);
         if Success then
            Parser_Utilities.Convert8BitUnsigned(RRSIG_Rec.Algorithm, ZoneFileLine, BegIdx,
               EndIdx, Success);
         end if;
      end if;

      --NUM_LABELS
      Success := Success and (EndIdx <ZLength);   --fail if at EOL
      if Success then
         begIdx := endIdx + 1;
         Parser_Utilities.FindNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, FoundType);
         Success := (FoundType = Rr_Type.Number);
         if Success then
            Parser_Utilities.Convert8BitUnsigned(RRSIG_Rec.NumLabels, ZoneFileLine, BegIdx,
               EndIdx, Success);
         end if;
      end if;

   end ParseTypeCoveredAlgorithmNumLabels;

   procedure ParseRRSigHeader(RRSIG_Rec:  out rr_type.rrsig_record_type.RRSIGRecordType;
      zoneFileLine : in rr_type.LineFromFileType;
      ZLength : in Rr_Type.LineLengthIndex;
      Success : in out Boolean)
   is
      begIdx : rr_type.LineLengthIndex := 1;
      endIdx : rr_type.LineLengthIndex;
      FoundType : Rr_Type.RrItemType;

      TimeString : Rr_Type.Rrsig_Record_Type.TimeStringType :=
         Rr_Type.Rrsig_Record_Type.TimeStringType'(others => ' ');

   begin
      RRSIG_Rec := Rr_Type.Rrsig_Record_Type.BlankRRSIGRecord;

      --find the record type indicator (RRSIG), which must be present since this routine was called
      parser_utilities.findNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, foundType);

      --second condition always true if precondition of routine is met, but it helps the prover
      WHILE FoundType /= rr_type.RecordIndicator and endIdx < zlength LOOP
         --# assert endIdx < zlength;
         begIdx := ((begIdx-begIdx)+endIdx)+1;  --makes flow error go away
         parser_utilities.findNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, foundType);
      END LOOP;

      --parse TYPE_COVERED, ALGORITHM and NUM_LABELS fields
      ParseTypeCoveredAlgorithmNumLabels(RRSIG_Rec, ZoneFileLine, ZLength, EndIdx, Success);

      --parse ORIG_TTL field (time spec)
      Success := Success and (endIdx < zLength);   --fail if at EOL
      if Success then
         begIdx := endIdx + 1;
         Parser_Utilities.FindNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, FoundType);
         if FoundType = Rr_Type.Number then
            Parser_Utilities.Convert32BitUnsigned(RRSIG_Rec.OrigTTL, ZoneFileLine, BegIdx, EndIdx,
               Success);
         elsif FoundType = rr_type.DomainNameOrTimeSpec then
            Parser_Utilities.ConvertTimeSpec(ZoneFileLine, BegIdx, EndIdx, RRSIG_Rec.OrigTTL,
               Success);
         else
            Success := false;
         end if;
      end if;

      --parse SIG_EXPIRATION field (YYYYMMDDHHmmSS or unsigned 32-bit #)
      Success := Success and (endIdx < zLength);   --fail if at EOL
      if Success then
         begIdx := endIdx + 1;
         parser_utilities.findNextToken(zoneFileLine, zLength, begIdx, endIdx, foundType);
         Success := (foundType = rr_type.Number);   --fail if not a number
      end if;

      if Success then   --next token is a number
         --convert token to unsigned 32-bit number
         --If it's not of length timeStringLength, can't be YYYYMMDDHHmmSS
         if ((EndIdx-BegIdx)+1 /= rr_type.rrsig_record_type.timeStringLength) then
            Parser_Utilities.Convert32BitUnsigned(RRSIG_Rec.OrigTTL, ZoneFileLine,
               BegIdx, EndIdx, Success);
         else --convert YYYYMMDDHHmmSS to #secs since 1 Jan 1970 00:00:00 UTC
            for I in Rr_Type.Rrsig_Record_Type.TimeStringTypeIndex loop
               --# assert begIdx = begIdx% and endIdx = endIdx% and zLength = zlength%
               --# and endIdx <= zLength
               --# and endIdx-begIdx = rr_type.rrsig_record_type.timeStringLength-1
               --# and I <= Rr_Type.Rrsig_Record_Type.TimeStringTypeIndex'Last;
               TimeString(I) := ZoneFileLine(BegIdx+(I-1));
            end loop;
            --establish and prove the precondition for convertTimeString (all numbers)
            for I in Rr_Type.Rrsig_Record_Type.TimeStringTypeIndex loop
               --# assert for all J in Integer range 1..I-1 => (TimeString(J) >= '0' and TimeString(J) <= '9');
               if (TimeString(I) < '0' or TimeString(I) > '9') then
                  Success := False;
                  exit;
               end if;
            end loop;
            if Success then
               parser_utilities.convertTimeString(RRSIG_Rec.SigExpiration, TimeString, Success);
            end if;
         end if; --timeString or simple number
      end if; --successful so far

      --parse left paren
      Success := Success and (endIdx < zLength);   --fail if at EOL
      if Success then
         begIdx := endIdx + 1;
         Parser_Utilities.FindNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, FoundType);
         --make bogus flow errors go away
         if (FoundType /= Rr_Type.LParen) and (begIdx = begIdx) and (endIdx = endIdx) then
            Success := False;
         end if;
      end if;
   end ParseRRSigHeader;


   --precondition is that zoneFileLine contains a record indicator, but can't
   --express that in SPARK
   procedure parseDomainName(newDomainName : out rr_type.DomainNameStringType;
                                      zoneFileLine : in rr_type.LineFromFileType;
                                      zLength : in rr_type.LineLengthIndex; success : in out boolean)
   is
      begIdx : rr_type.LineLengthIndex := 1;
      endIdx : rr_type.LineLengthIndex;
      foundType : rr_type.rrItemType;
      lengthOfToken : rr_type.LineLengthIndex;

   BEGIN
      newDomainName := rr_type.blankDomainName;
      --find the record type indicator, which must be present since this routine was called
      parser_utilities.findNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, foundType);

      --second condition always true if precondition of routine is met, but it helps the prover
      WHILE FoundType/= rr_type.RecordIndicator and endIdx < zlength LOOP
         --# assert endIdx < zlength;
         begIdx := ((begIdx-begIdx)+endIdx)+1;  --makes flow error go away
         parser_utilities.findNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, foundType);
      END LOOP;

      --check to make sure something is after the record type, find its position if so
      if (endIdx >= zLength) then
         success := false;
      else
         begIdx := endIdx+1;
         Parser_Utilities.FindNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, FoundType);
         --don't bother checking if FoundType is a domainName, just take whatever is there
         LengthOfToken := (EndIdx-BegIdx)+1;
         --superfluous test makes flow error go away
         if (lengthOfToken  > rr_type.MaxDomainNameLength and FoundType = FoundType) then
            success := false;
            Error_Msgs.PrintDomainLengthErrorInfo(ZoneFileLine, BegIdx, EndIdx);
         else
            --have a domain name, but must still pad it
            for I in Integer range begIdx..endIdx loop
               --# assert begIdx >= 1 and endIdx-begIdx+1 <= rr_type.MaxDomainNameLength
               --# and endIdx = endIdx%;
               NewDomainName((I+1)-begIdx) := ZoneFileLine(I);
            end loop;
            for I in Integer range endIdx+1..rr_type.maxDomainNameLength loop
               -- Remind prover that endIdx is in type
               --# assert endIdx >= 1;
               newDomainName(I) := ' ';
            end loop;
         end if;
      end if;
   END ParseDomainName;

   --just like ParseDomainName, but also grabs character string after it
   procedure ParseDomainNameAndRRString(
      NewDomainName : out Rr_Type.DomainNameStringType;
      RRString : out Rr_type.LineFromFileType;
      zoneFileLine : in rr_type.LineFromFileType;
      ZLength : in Rr_Type.LineLengthIndex;
      success : in out boolean)
   is
      begIdx : rr_type.LineLengthIndex := 1;
      endIdx : rr_type.LineLengthIndex;
      foundType : rr_type.rrItemType;
      lengthOfToken : rr_type.LineLengthIndex;

   BEGIN
      RRString := rr_type.BlankLine;
      newDomainName := rr_type.blankDomainName;
      --find the record type indicator, which must be present since this routine was called
      parser_utilities.findNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, foundType);

      --second condition always true if precondition of routine is met, but it helps the prover
      WHILE FoundType/= rr_type.RecordIndicator and endIdx < zlength LOOP
         --# assert endIdx < zlength;
         begIdx := ((begIdx-begIdx)+endIdx)+1;  --makes flow error go away
         parser_utilities.findNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, foundType);
      END LOOP;

      --check to make sure something is after the record type, find its position if so
      if (endIdx >= zLength) then
         success := false;
      else
         begIdx := endIdx+1;
         Parser_Utilities.FindNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, FoundType);
         --don't bother checking if FoundType is a domainName, just take whatever is there
         LengthOfToken := (EndIdx-BegIdx)+1;
         --superfluous test makes flow error go away
         if (lengthOfToken  > rr_type.MaxDomainNameLength and FoundType = FoundType) then
            success := false;
            Error_Msgs.PrintDomainLengthErrorInfo(ZoneFileLine, BegIdx, EndIdx);
         else
            --have a domain name, but must still pad it
            for I in Integer range begIdx..endIdx loop
               --# assert begIdx >= 1 and endIdx-begIdx+1 <= rr_type.MaxDomainNameLength
               --# and endIdx = endIdx%;
               NewDomainName((I+1)-begIdx) := ZoneFileLine(I);
            end loop;
            for I in Integer range endIdx+1..rr_type.MaxDomainNameLength loop
               -- Remind prover that endIdx is in type
               --# assert endIdx >= 1;
               newDomainName(I) := ' ';
            end loop;
         end if;
         --this is the extra code
         if (endIdx >= zLength) then
            success := false;
         else
            BegIdx := EndIdx+1;
            Parser_Utilities.FindNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, FoundType);
            --rrstring is everything after the domain name to the end of the line
            EndIdx := (EndIdx - EndIdx) + ZLength;  --make flow error go away
            LengthOfToken := (EndIdx-BegIdx)+1;
            --superfluous test makes flow error go away
            --for now, rrstring length max is same as domain name length max
            if (lengthOfToken  > rr_type.MaxDomainNameLength and FoundType = FoundType) then
               success := false;
               Error_Msgs.PrintRRStringLengthErrorInfo(ZoneFileLine, BegIdx, EndIdx);
            else
            --have an rrstring, but must still pad it
               for I in Integer range begIdx..endIdx loop
                  --# assert begIdx >= 1 and endIdx-begIdx+1 <= rr_type.MaxDomainNameLength
                  --# and endIdx = endIdx%;
                  RRString((I+1)-begIdx) := ZoneFileLine(I);
               end loop;
               for I in Integer range ((endIdx+2)-begIdx)..rr_type.lineLengthIndex'Last loop
                  -- Remind prover of postcondition from FindNextToken
                  --# assert endIdx >= begIdx;
                  RRString(I) := ' ';
            end loop;
         end if;
      end if;
   end if;
   END ParseDomainNameAndRRString;

   procedure ParseTimeSpec(newTimeSpec : out Unsigned_Types.Unsigned32;
      zoneFileLine : in Rr_Type.LineFromFileType;
      zLength : in Rr_Type.LineLengthIndex; success : in out Boolean)
   is
      begIdx : rr_type.LineLengthIndex := 1;
      endIdx : rr_type.LineLengthIndex;
      foundType : rr_type.rrItemType;
   begin
      newTimeSpec := 0;
      parser_utilities.findNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, foundType);

      --next token must be a time specifier or number, otherwise error
      if (foundType /= rr_type.DomainNameOrTimeSpec and foundType /= rr_type.Number) then
         success := false;
      else
         parser_utilities.ConvertTimeSpec(zoneFileLine, begIdx, endIdx, newTimeSpec, success);
      end if;  --number found?
   end ParseTimeSpec;

   procedure ParseControlLine(newOrigin : in out rr_type.DomainNameStringType;
                              NewTTL : in out Unsigned_Types.Unsigned32;
                              zoneFileLine : in rr_type.LineFromFileType;
                              ZLength : in Rr_Type.LineLengthIndex; Success : in out Boolean)
   is
      begIdx : rr_type.LineLengthIndex := 1;
      EndIdx : Rr_Type.LineLengthIndex;
      FoundType : Rr_Type.RrItemType;
      TmpLine : rr_type.LineFromFileType := rr_type.LineFromFileType'(others => ' ');
   begin

      --find the control command, which must be present since this routine was called
      Parser_Utilities.FindNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, FoundType);
      if FoundType = Rr_Type.Control then --should always be true
         --only $TTL and $ORIGIN currently recogznied
         --phrasing conditions this way helps the prover
         if BegIdx <= EndIdx-6 then --$ORIGIN?
            if ZoneFileline(BegIdx+1) = 'O' and
               ZoneFileline(BegIdx+2) = 'R' and
               ZoneFileline(BegIdx+3) = 'I' and
               ZoneFileline(BegIdx+4) = 'G' and
               ZoneFileline(BegIdx+5) = 'I' and
               ZoneFileline(BegIdx+6) = 'N' then
               if begIdx < Zlength and endIdx < Zlength then
                  Begidx := EndIdx + 1;
                  Parser_Utilities.FindNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, FoundType);
               end if;
               if FoundType /= Rr_Type.DomainNameOrTimeSpec then
                  Success := False;
               elsif ZoneFileLine(EndIdx) /= '.' then
                  Success := False;   --argument to $ORIGIN must end with '.'
               else
                  --create a line with a dummy record indicator and the domain name
                  --so it can be parsed with ParseDomainName
                  for I in Integer range BegIdx..EndIdx loop
                     --# assert begIdx >= 1 and begIdx <= endIdx;
                     tmpLine(I) := zoneFileLine(I);
                  end loop;
                  --we know the domain name can't start before pos 6,
                  --so this will work
                  TmpLine(1) := 'A';
                  ParseDomainName(NewOrigin, TmpLine, Rr_Type.MaxLineLength, Success);
               end if;
            else
               Success := False;
            end if;  --control token of length 6
         elsif BegIdx <= EndIdx-3 then --$TTL?
            if ZoneFileline(BegIdx+1) = 'T' and
               ZoneFileline(BegIdx+2) = 'T' and
               ZoneFileline(BegIdx+3) = 'L' then
               if begIdx < Zlength and endIdx < Zlength then
                  Begidx := EndIdx + 1;
                  Parser_Utilities.FindNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, FoundType);
               end if;
               if FoundType /= Rr_Type.DomainNameOrTimeSpec and FoundType /= Rr_Type.Number then
                  Success := False;
               else
                  --create a line with only the time spec on it so it can be parsed
                  --with ParseTimeSpec
                  for I in Integer range BegIdx..EndIdx loop
                     --# assert begIdx >= 1 and begIdx <= endIdx;
                     tmpLine(I) := zoneFileLine(I);
                  end loop;
                  ParseTimeSpec(NewTTL, TmpLine, Rr_Type.MaxLineLength, Success);

               end if;
            else
               Success := False;
            end if;  --control token of length 3

         end if; --$TTL or $ORIGIN
      else
         Success := False;
      end if; --whether or not control token found
   end ParseControlLine;

   procedure parseSerialNumber(newSerialNumber : out unsigned_types.Unsigned32;
                               zoneFileLine : in rr_type.LineFromFileType;
                               ZLength : in Rr_Type.LineLengthIndex;
                               success : in out boolean)
   is
      begIdx : rr_type.LineLengthIndex := 1;
      endIdx : rr_type.LineLengthIndex;
      foundType : rr_type.rrItemType;
      digitVal : unsigned_types.unsigned32;
      TmpVal : long_long_integer;

   BEGIN
      newSerialNumber := 0;

      parser_utilities.findNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, foundType);

      --next token must be a number, otherwise error
      if (foundType /= rr_type.Number) then
         success := false;
      else
         tmpVal := 0;
         for i in integer range begIdx..endIdx loop
            --# assert begIdx >= 1 and tmpVal >= 0 and tmpVal <= rr_type.SOA_record_type.MAX_SERIAL_VAL;
            digitVal := Character'Pos(zoneFileLine(I)) - Character'Pos('0');
            TmpVal := 10*TmpVal + long_long_integer(digitVal);
            --bail out early if serial# is too large
            exit when tmpVal > rr_type.SOA_record_type.MAX_SERIAL_VAL;
         end loop;
         --make sure it's not too big
         if tmpVal > rr_type.SOA_record_type.MAX_SERIAL_VAL then
            success := false;
         else --have a valid serial number
            newSerialNumber := unsigned_types.unsigned32(tmpVal);
         end if;  --too large pref?
      end if;  --number found?
   end ParseSerialNumber;

   --precondition is that zoneFileLine contains a record indicator
   --Data item after "MX" record indicator should be a valid 16-bit integer
   procedure parsePrefandDomainName(newPref : out unsigned_types.Unsigned16;
                                    newDomainName : out rr_type.DomainNameStringType;
                                    zoneFileLine : in rr_type.LineFromFileType;
                                    zLength : in rr_type.LineLengthIndex; success : in out boolean)
   is
      begIdx : rr_type.LineLengthIndex := 1;
      endIdx : rr_type.LineLengthIndex;
      foundType : rr_type.rrItemType;
      digitVal : unsigned_types.unsigned16;
      TmpVal : unsigned_types.unsigned16;
      lengthofToken : rr_type.LineLengthIndex;

   BEGIN
      newPref := 0;
      newDomainName := rr_type.blankDomainName;

       --find the record type indicator, which must be present since this routine was called
      parser_utilities.findNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, foundType);

      --second condition always true if precondition of routine is met, but it helps the prover
      WHILE FoundType/= rr_type.RecordIndicator and endIdx < zlength LOOP
         --# assert endIdx < zlength;
         begIdx := ((begIdx-begIdx)+endIdx)+1;  --makes flow error go away
         parser_utilities.findNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, foundType);
      END LOOP;

      --check to make sure something is after the record type, find its position if so
      if (endIdx = zLength) then
         success := false;
      else
         begIdx := endIdx + 1;
         parser_utilities.findNextToken(zoneFileLine, zLength, begIdx, endIdx, foundType);
         if foundType /= rr_type.Number then
            success := false;
         ELSE --convert token to number
            tmpVal := 0;
            for i in integer range begIdx..endIdx loop
               --# assert begIdx >= 1 and tmpVal <= rr_type.mx_record_type.MAX_PREF_VAL;
               digitVal := Character'Pos(zoneFileLine(I)) - Character'Pos('0');
               TmpVal := 10*TmpVal + digitVal;
               exit when tmpVal > rr_type.mx_record_type.MAX_PREF_VAL;
            end loop;
            --make sure it's not too big
            if tmpVal > rr_type.mx_record_type.MAX_PREF_VAL then
               success := false;
            else --have a valid preference value
               newPref := tmpVal;
               --next token must be a domain name
               if (endIdx >= zLength) then  -- use of ">=" helps the prover
         	      success := false;
               else
                  begIdx := endIdx+1;
         	  parser_utilities.findNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, FoundType);
         	  lengthOfToken := (endIdx-begIdx)+1;
         	  if foundType /= rr_type.DomainNameorTimeSpec then
            	     success := false;
         	  elsif (lengthOfToken > rr_type.MaxDomainNameLength) then
            	     success := false;
            	     Error_Msgs.PrintDomainLengthErrorInfo(ZoneFileLine, BegIdx, EndIdx);
         	  else
            	     --have a domain name, but must still pad it
            	     for I in Integer range begIdx..endIdx loop
               		--# assert begIdx >= 1 and endIdx-begIdx+1 <= rr_type.MaxDomainNameLength
               		--# and endIdx = endIdx%;
               		NewDomainName((I+1)-begIdx) := ZoneFileLine(I);
            	     end loop;
            	     for I in Integer range endIdx+1..rr_type.maxDomainNameLength loop
               		-- Remind prover that endIdx is in type
               		--# assert endIdx >= 1;
               		newDomainName(I) := ' ';
            	     end loop;
                  end if;  --valid domain name?
               end if;  --end of string reached after preference value?
            end if;  --too large pref?
         end if;  --number found?
      end if;  --end of string reached after record indicator?
   end parsePrefAndDomainName;

   procedure parseIpv4(newIpv4 : out unsigned_types.Unsigned32;
                       zoneFileLine : in rr_type.LineFromFileType;
                        zLength : in rr_type.LineLengthIndex; success : in out boolean)
   is
      begIdx : rr_type.LineLengthIndex := 1;
      endIdx : rr_type.LineLengthIndex;
      foundType : rr_type.rrItemType;
      TmpVal : unsigned_types.unsigned32;

   BEGIN
      newIpv4 := 0;
      Parser_Utilities.FindNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, FoundType);
      --loop until you find an ipV4
      WHILE FoundType /= rr_type.Ipv4 and endIdx < zlength LOOP
         --# assert endIdx < zlength;
         begIdx := ((begIdx-begidx)+endIdx)+1;
         parser_utilities.findNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, foundType);
      END LOOP;

      if FoundType /= rr_type.Ipv4 and EndIdx >= ZLength then	-- ">=" helps the prover
         success := false;
      else
         TmpVal := parser_utilities.ConvertIpv4(ZoneFileLine, begIdx, endIdx);
         if TmpVal = 0 then
            Success := False;
         else
            newIpv4 := tmpVal;
         end if;
      end if;
   end parseIpv4;

   procedure parseIpv6(newIpv6 : out rr_type.aaaa_record_type.IPV6AddrType;
                       zoneFileLine : in rr_type.LineFromFileType;
                        zLength : in rr_type.LineLengthIndex; success : in out boolean)
   is
      begIdx : rr_type.LineLengthIndex := 1;
      endIdx : rr_type.LineLengthIndex;
      foundType : rr_type.rrItemType;

   BEGIN
      newIpv6 := rr_type.aaaa_record_type.INVALID_IPV6_ADDR;
      Parser_Utilities.FindNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, FoundType);

      --loop until you find an ipV6
      WHILE FoundType /= rr_type.Ipv6 and endIdx < zlength LOOP
         --# assert endIdx < zlength;
         begIdx := ((begIdx-begidx)+endIdx)+1;
         parser_utilities.findNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, foundType);
      END LOOP;

      if FoundType /= rr_type.Ipv6 and EndIdx >= ZLength then	-- ">=" helps the prover
         success := false;
      else
         newIpv6 := parser_utilities.ConvertIpv6(ZoneFileLine, begIdx, endIdx);
         if newIpv6 = rr_type.aaaa_record_type.INVALID_IPV6_ADDR then
            success := false;
         end if;
      end if;
   end ParseIpv6;

 --precondition is that zoneFileLine contains a record indicator, but can't
   --express that in SPARK
   --Basically like parseDomainName done twice
   procedure ParseNameServerAndEmail(newNameServer : out Rr_Type.DomainNameStringType;
                                      newEmail : out rr_type.DomainNameStringType;
                                      zoneFileLine : in rr_type.LineFromFileType;
                                      zLength : in rr_type.LineLengthIndex; success : in out boolean)
   is
      begIdx : rr_type.LineLengthIndex := 1;
      endIdx : rr_type.LineLengthIndex;
      foundType : rr_type.rrItemType;
      lengthOfToken : rr_type.LineLengthIndex;

   BEGIN
      NewNameServer := Rr_Type.BlankDomainName;
      newEmail := rr_type.blankDomainName;
      --find the record type indicator, which must be present since this routine was called
      parser_utilities.findNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, foundType);

      --second condition always true if precondition of routine is met, but it helps the prover
      WHILE FoundType/= rr_type.RecordIndicator and endIdx < zlength LOOP
         --# assert endIdx < zlength;
         begIdx := ((begIdx-begIdx)+endIdx)+1;  --makes flow error go away
         parser_utilities.findNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, foundType);
      END LOOP;

      --check to make sure something is after the record type, find its position if so
      if (EndIdx >= ZLength) then
         success := false;
      else
         begIdx := endIdx+1;
         parser_utilities.findNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, FoundType);
         lengthOfToken := (endIdx-begIdx)+1;
         if foundType /= rr_type.DomainNameOrTimeSpec then
            Success := False;
         elsif (LengthOfToken  > Rr_Type.MaxDomainNameLength) then
            success := false;
            Error_Msgs.PrintDomainLengthErrorInfo(ZoneFileLine, BegIdx, EndIdx);
         else
            --have a domain name, but must still pad it
            for I in Integer range begIdx..endIdx loop
               --# assert begIdx >= 1 and endIdx-begIdx+1 <= rr_type.MaxDomainNameLength
               --# and endIdx = endIdx%;
               newNameServer((I+1)-begIdx) := ZoneFileLine(I);
            end loop;
            for I in Integer range endIdx+1..rr_type.maxDomainNameLength loop
               -- Remind prover that endIdx is in type
               --# assert endIdx >= 1;
               newNameServer(I) := ' ';
            end loop;
         end if;

      end if;

      --do it all again to get the email
       --check to make sure something is after the name server, find its position if so
      if (EndIdx >= ZLength) then
         success := false;
      else
         begIdx := endIdx+1;
         parser_utilities.findNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, FoundType);
         lengthOfToken := (endIdx-begIdx)+1;
         if FoundType /= Rr_Type.DomainNameOrTimeSpec then
            success := false;
         elsif (lengthOfToken  > rr_type.MaxDomainNameLength) then
            success := false;
            Error_Msgs.PrintDomainLengthErrorInfo(ZoneFileLine, BegIdx, EndIdx);
         else
            --have a domain name, but must still pad it
            for I in Integer range begIdx..endIdx loop
               --# assert begIdx >= 1 and endIdx-begIdx+1 <= rr_type.MaxDomainNameLength
               --# and endIdx = endIdx%;
               newEmail((I+1)-begIdx) := ZoneFileLine(I);
            end loop;
            for I in Integer range endIdx+1..rr_type.maxDomainNameLength loop
               -- Remind prover that endIdx is in type
               --# assert endIdx >= 1;
               newEmail(I) := ' ';
            end loop;
         end if;
      end if;
   END ParseNameServerAndEmail;

   --domain name, TTL and class on a line are all optional, if blank default to last known values
   --succeeds if a record type is found
   procedure ParseOwnerTTLClassAndRecordType(
                              newOwner : in out rr_type.DomainNameStringType;
                              newTTL : in out unsigned_types.Unsigned32;
                              newClass : in out rr_type.classType;
                              newType : in out dns_types.Query_Type;
                      	        zoneFileLine : in rr_type.LineFromFileType;
                              zLength : in rr_type.LineLengthIndex; success : in out boolean) is
      begIdx : rr_type.LineLengthIndex := 1;
      endIdx : rr_type.LineLengthIndex;
      Token : Rr_Type.LineFromFileType := Rr_Type.LineFromFileType'(others => ' ');
      lengthOfToken : rr_type.LineLengthIndex;
      TokenType : Rr_Type.RrItemType;
      recordTypeFound : boolean := false;
      firstChar : Character;
      LastChar : Character;

   begin
      --if 1st char isn't blank, then first token is a domainName regardless of
      --returned value of TokenType
      if (zoneFileLine(1) /= ' ') and (zoneFileLine(1) /= Ada.Characters.Latin_1.HT) then
         Parser_Utilities.FindNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, TokenType);
         lengthOfToken := ((endIdx-begIdx)+1);
         for i in integer range begIdx..endIdx loop
           --# assert begIdx >= 1 and begIdx <= endIdx;
           token((i+1)-begIdx) := zoneFileLine(i);
         end loop;
         if (lengthOfToken >= rr_type.MaxDomainNameLength) then
            success := false;
            error_msgs.printDomainLengthErrorInfo(zoneFileLine, begIdx, endIdx);
         else
            newOwner := rr_type.BlankDomainName;  --in case previous owner longer than new one
            for I in Integer range 1..LengthOfToken loop
               --# assert lengthOfToken = lengthOfToken%
               --# and lengthOfToken < rr_type.MaxDomainNameLength;
               newOwner(I) := token(I);
            end loop;
         end if;
         --line must contain a record type or the parse fails
         --use of ">=" helps the prover
         if (endIdx >= zlength or tokenType = rr_type.Comment) then
            Success := False;
      	 else
	    begIdx := endIdx+1;
         end if;
      end if;

      --now keep going until you find a record type, which will be present in
      --every valid record in the file
      WHILE (NOT RecordTypeFound) AND Success LOOP
         Parser_Utilities.FindNextToken(ZoneFileLine, ZLength, BegIdx, EndIdx, TokenType);
         --postcondition from findNextToken()
         --# assert begIdx <= endIdx;
         for i in integer range begIdx..endIdx loop
           --# assert begIdx >= 1 and begIdx <= endIdx;
           token((i+1)-begIdx) := zoneFileLine(i);
         end loop;
         case TokenType is
            when Rr_Type.Number | Rr_Type.DomainNameOrTimeSpec =>
               Parser_Utilities.ConvertTimeSpec(ZoneFileLine, BegIdx, EndIdx, NewTTL, Success);
            when Rr_Type.Class =>
               firstChar := Ada.Characters.Handling.To_Upper(token(begIdx));
               lastChar := Ada.Characters.Handling.To_Upper(token(endIdx));
               if firstChar = 'C' and lastChar = 'H' then
                  newClass := rr_type.CH;
               elsif firstChar = 'C' and lastChar = 'S' then
                  newClass := rr_type.CS;
	           elsif firstChar = 'H' and lastChar = 'S' then
                  newClass := rr_type.HS;
	           elsif firstChar = 'I' and lastChar = 'N' then
	              newClass := rr_type.INTERNET;
               END IF;
            WHEN Rr_Type.RecordIndicator =>
               RecordTypeFound := True;  --this means we have found a valid record type indicator
               NewType := Parser_Utilities.GetRecordType(ZoneFileLine, BegIdx, EndIdx);
            when others =>
               Success := False;
	    end case;

        --line must contain a record type or the parse fails
        --use of ">=" helps the prover
        if (endIdx >= zlength or tokenType = rr_type.Comment) then
           if not recordTypeFound then
              Success := False;
           end if;
      	 else
	       begIdx := endIdx+1;
        end if;
      END LOOP; --while record type token not found and line still has tokens

   end parseOwnerTTLCLassAndRecordType;
end zone_file_parser;
