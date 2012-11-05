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

with Ada.Text_IO, Ada.Integer_Text_IO;
with Rr_Type.Dnskey_Record_Type;

package body Error_Msgs is
--# hide error_msgs
   package M_IO is new Ada.Text_IO.Modular_IO (Unsigned_Types.Unsigned32);

procedure printUnsupportedRecordWarning(currentLine : in rr_type.LineFromFileType;
   Length : IN Natural; LineCount : in Unsigned_Types.Unsigned32) is
begin
   Ada.Text_Io.Put("Warning:  Unsupported record type at line # ");
   m_io.put(item => lineCount, width => 1);
   Ada.Text_IO.Put(" in zone file: ");
   Ada.Text_Io.New_Line;
   Ada.Text_Io.New_Line;
   Ada.Text_Io.Put("    ");
   Ada.Text_Io.Put(currentLine(1..length));
   Ada.Text_Io.New_Line;
   Ada.Text_Io.New_Line;
   Ada.Text_Io.Put("Record ignored ");
   Ada.Text_IO.New_Line;

end PrintUnsupportedRecordWarning;

procedure printZeroTTLWarning(currentLine : in rr_type.LineFromFileType;
   Length : IN Natural; LineCount : in Unsigned_Types.Unsigned32) is
begin
   Ada.Text_Io.Put("Warning:  Default TTL of zero implied at line # ");
   m_io.put(item => lineCount, width => 1);
   Ada.Text_IO.Put(" in zone file: ");
   Ada.Text_Io.New_Line;
   Ada.Text_Io.New_Line;
   Ada.Text_Io.Put("    ");
   Ada.Text_Io.Put(currentLine(1..length));
   Ada.Text_Io.New_Line;
   Ada.Text_Io.New_Line;
   Ada.Text_Io.Put("Possible missing $TTL control statement ");
   Ada.Text_IO.New_Line;
end PrintZeroTTLWarning;

procedure printBlankOriginWarning(currentLine : in rr_type.LineFromFileType;
   Length : IN Natural; LineCount : in Unsigned_Types.Unsigned32) is
begin
   Ada.Text_Io.Put("Warning:  Default blank ORIGIN appended");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.put("to domain name at line # ");
   m_io.put(item => lineCount, width => 1);
   Ada.Text_IO.Put(" in zone file: ");
   Ada.Text_Io.New_Line;
   Ada.Text_Io.New_Line;
   Ada.Text_Io.Put("    ");
   Ada.Text_Io.Put(currentLine(1..length));
   Ada.Text_Io.New_Line;
   Ada.Text_Io.New_Line;
   Ada.Text_Io.Put("Possible missing $ORIGIN control statement ");
   Ada.Text_IO.New_Line;
   Ada.Text_Io.Put("or missing period at end of domain name ");
   Ada.Text_IO.New_Line;
end PrintBlankOriginWarning;

procedure printParseErrorInfo(currentLine : in rr_type.LineFromFileType;
      Length : in Natural; LineCount : in Unsigned_Types.Unsigned32) is
begin

      Ada.Text_Io.Put("Invalid or unsupported record on line # ");
      m_io.put(item => lineCount, width => 1);
      Ada.Text_IO.Put(" in zone file: ");
      Ada.Text_Io.New_Line;
      Ada.Text_Io.New_Line;
      Ada.Text_Io.Put("    ");
      Ada.Text_Io.Put(currentLine(1..length));
      Ada.Text_Io.New_Line;
      Ada.Text_Io.New_Line;
      Ada.text_io.Put("Remaining lines in zone file ignored.");
      Ada.Text_Io.New_Line;
END PrintParseErrorInfo;

procedure printSOARecordErrorInfo(currentLine : in rr_type.LineFromFileType;
      Length : in Natural; LineCount : in Unsigned_Types.Unsigned32) is
begin

      Ada.Text_Io.Put("Disallowed SOA record on line # ");
      m_io.put(item => lineCount, width => 1);
      Ada.Text_IO.Put(" of zone file: ");
      Ada.Text_Io.New_Line;
      Ada.Text_Io.New_Line;
      Ada.Text_Io.Put("    ");
      Ada.Text_Io.Put(currentLine(1..length));
      Ada.Text_Io.New_Line;
      Ada.Text_Io.New_Line;
      Ada.text_io.Put("Only one SOA record is permitted in a zone file (RFC 1035)");
      Ada.Text_Io.New_Line;
      Ada.text_io.Put("Remaining lines in zone file ignored.");
      Ada.Text_Io.New_Line;
END PrintSOARecordErrorInfo;

procedure printAppendDomainLengthErrorInfo(currentLine : in rr_type.LineFromFileType;
      Length : IN Natural; LineCount : IN unsigned_types.Unsigned32) IS
begin
   Ada.Text_IO.Put("ERROR IN ZONE FILE on line # " & Unsigned_Types.Unsigned32'Image(LineCount));
   Ada.Text_IO.Put(" of zone file:");
   Ada.Text_Io.New_Line;
   Ada.Text_Io.New_Line;
   Ada.Text_Io.Put("    ");
   Ada.Text_Io.Put(currentLine(1..length));
   Ada.Text_IO.New_Line;
   Ada.Text_IO.new_line;
   Ada.Text_IO.Put("domain name after appending with $ORIGIN is too long ");
   Ada.Text_IO.new_line;
   Ada.Text_IO.Put("(maximum length is ");
   Ada.Integer_Text_IO.Put(Item => Rr_Type.MaxDomainNameLength-1, Width => 1);
   Ada.Text_IO.Put(" chars)");
   Ada.Text_Io.New_Line;
   Ada.Text_Io.New_Line;
   Ada.Text_Io.Put("Domain name must either be changed, or source code recompiled ");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put("(with larger value of rr_type.MaxDomainNameLength) and revalidated.");
   Ada.Text_Io.New_Line;
   Ada.text_io.Put("Remaining lines in zone file ignored.");
   Ada.Text_Io.New_Line;
end printAppendDomainLengthErrorInfo;

procedure printLineLengthErrorInfo(currentLine : in rr_type.LineFromFileType;
      Length : IN Natural; LineCount : IN unsigned_types.Unsigned32) IS
BEGIN
   Ada.Text_IO.Put("ERROR:  Line #");
   m_IO.Put(Item => LineCount, Width => 1);
   Ada.Text_IO.put(" in zone file is too long (");
   Ada.Integer_Text_IO.Put(item => length, width => 1);
   Ada.Text_IO.Put(" chars, maximum length is ");
   Ada.Integer_Text_IO.Put(Item => Rr_Type.MaxLineLength-1, Width => 1);
   Ada.Text_IO.Put(")");
   Ada.Text_Io.New_Line;
   Ada.Text_Io.New_Line;
   Ada.Text_Io.Put("    ");
   Ada.Text_Io.Put(currentLine(1..length));
   Ada.Text_Io.New_Line;
   Ada.Text_Io.New_Line;
   Ada.Text_Io.Put("Line must either be broken up, or source code recompiled ");
   Ada.Text_IO.Put("(with larger value of rr_type.MaxLineLength) and revalidated.");
   Ada.Text_Io.New_Line;
   Ada.text_io.Put("Remaining lines in zone file ignored.");
   Ada.Text_Io.New_Line;
END PrintLineLengthErrorInfo;

PROCEDURE printDomainLengthErrorInfo(CurrentLine : IN Rr_Type.LineFromFileType;
      BegIdx: IN Natural; EndIdx : IN Natural) IS
BEGIN
   Ada.Text_IO.put("ERROR IN ZONE FILE: domain name is too long (");
   Ada.Integer_Text_IO.Put(item => endIdx-begIdx+1, width => 1);
   Ada.Text_IO.Put(" chars, maximum length is ");
   Ada.Integer_Text_IO.Put(Item => Rr_Type.MaxDomainNameLength-1, Width => 1);
   Ada.Text_IO.Put(")");
   Ada.Text_Io.New_Line;
   Ada.Text_Io.New_Line;
   Ada.Text_Io.Put("    ");
   Ada.Text_Io.Put(currentLine(begIdx..endIdx));
   Ada.Text_Io.New_Line;
   Ada.Text_Io.New_Line;
   Ada.Text_Io.Put("Domain name must either be changed, or source code recompiled ");
   Ada.Text_IO.Put("(with larger value of rr_type.MaxDomainNameLength) and revalidated.");
   Ada.Text_Io.New_Line;
   Ada.text_io.Put("Remaining lines in zone file ignored.");
   Ada.Text_Io.New_Line;
END PrintDomainLengthErrorInfo;

PROCEDURE printRRStringLengthErrorInfo(CurrentLine : IN Rr_Type.LineFromFileType;
      BegIdx: IN Natural; EndIdx : IN Natural) IS
BEGIN
   Ada.Text_IO.put("ERROR IN ZONE FILE: resource record string is too long (");
   Ada.Integer_Text_IO.Put(item => endIdx-begIdx+1, width => 1);
   Ada.Text_IO.Put(" chars, maximum length is ");
   Ada.Integer_Text_IO.Put(Item => Rr_Type.MaxDomainNameLength-1, Width => 1);
   Ada.Text_IO.Put(")");
   Ada.Text_Io.New_Line;
   Ada.Text_Io.New_Line;
   Ada.Text_Io.Put("    ");
   Ada.Text_Io.Put(currentLine(begIdx..endIdx));
   Ada.Text_Io.New_Line;
   Ada.Text_Io.New_Line;
   Ada.Text_Io.Put("Either NSEC record must be changed, or source code recompiled ");
   Ada.Text_IO.Put("(with redefinition of rr_type.nsec_record_type.recordListType) and revalidated.");
   Ada.Text_Io.New_Line;
   Ada.text_io.Put("Remaining lines in zone file ignored.");
   Ada.Text_Io.New_Line;
END PrintRRStringLengthErrorInfo;

procedure printKeyLengthErrorInfo(currentLine : in rr_type.LineFromFileType;
      Length : IN Natural; LineCount : IN unsigned_types.Unsigned32) IS
BEGIN
   Ada.Text_IO.Put("ERROR:  Key fragment at line #");
   m_IO.Put(Item => LineCount, Width => 1);
   Ada.Text_IO.put(" in zone file makes key too long (maximum length is ");
   Ada.Integer_Text_IO.Put(Item => Rr_Type.dnskey_record_type.maxDNSKeyLength, Width => 1);
   Ada.Text_IO.Put(")");
   Ada.Text_Io.New_Line;
   Ada.Text_Io.New_Line;
   Ada.Text_Io.Put("    ");
   Ada.Text_Io.Put(currentLine(1..length));
   Ada.Text_Io.New_Line;
   Ada.Text_Io.New_Line;
   Ada.Text_Io.Put("Server source code must be recompiled ");
   Ada.Text_IO.Put("(with larger value of Rr_Type.dnskey_record_type.maxDNSKeyLength or ");
   Ada.Text_IO.New_Line;
   ada.Text_IO.put("Rr_Type.dnskey_record_type.maxRRSIGLength as appropriate) and revalidated.");
   Ada.Text_Io.New_Line;
   Ada.text_io.Put("Remaining lines in zone file ignored.");
   Ada.Text_Io.New_Line;
END PrintKeyLengthErrorInfo;


PROCEDURE printDNSTableFullInfo(CurrentLine : IN Rr_Type.LineFromFileType;
      LineCount : IN Unsigned_Types.Unsigned32) is
begin
   Ada.Text_IO.Put("ERROR:  Zone file contains too many records,");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put("or more than one SOA record for a given domain.");
   ada.Text_IO.New_Line;
   ada.Text_IO.put("There is no room for the record at line # ");
   m_IO.Put(Item => LineCount, Width => 1);
   Ada.Text_Io.New_Line;
   Ada.Text_Io.New_Line;
   Ada.Text_Io.Put("    ");
   Ada.Text_Io.Put(currentLine);
   Ada.Text_Io.New_Line;
   Ada.Text_Io.New_Line;
   Ada.Text_IO.Put("If the problem is due to multiple SOA records for a given domain,");
   Ada.Text_IO.new_Line;
   Ada.Text_IO.Put("you will need to remove the extra SOA records from the zone file.");
   Ada.Text_IO.new_Line;
   Ada.Text_Io.Put("Otherwise, shut down and rebuild the server, with source code recompiled ");
   Ada.Text_IO.new_Line;
   Ada.Text_IO.Put("(use larger value(s) of rr_type.MaxNumRecords and/or rr_type.NumBuckets) and revalidated.");
   Ada.Text_Io.New_Line;
   Ada.text_io.Put("Remaining lines in zone file ignored.");
   Ada.Text_Io.New_Line;
end PrintDNSTableFullInfo;

--should only be called in conjunction with file reading "Success" variable set to false
PROCEDURE PrintMissingSOARecordInfo is
begin
   Ada.Text_IO.Put("ERROR:  First record in zone file must be an SOA record.");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put("Remaining lines in zone file ignored.");
   ada.Text_IO.New_Line;
end PrintMissingSOARecordInfo;

end error_msgs;
