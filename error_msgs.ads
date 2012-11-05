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

--all error diagnostic I/O goes here.  Spark I/O libraries are a pain,
--use regular Ada I/O instead and just hide it from the prying eyes of the examiner.
WITH Rr_Type;
with Unsigned_Types;
--# inherit rr_type, unsigned_types, Rr_Type.Dnskey_Record_Type;
package Error_Msgs is

procedure printUnsupportedRecordWarning(currentLine : in rr_type.LineFromFileType;
   Length : IN Natural; lineCount : in unsigned_types.Unsigned32);
   --# derives null from currentLine, length, lineCount;

procedure printZeroTTLWarning(currentLine : in rr_type.LineFromFileType;
   Length : IN Natural; lineCount : in unsigned_types.Unsigned32);
   --# derives null from currentLine, length, lineCount;

procedure printBlankOriginWarning(currentLine : in rr_type.LineFromFileType;
   Length : IN Natural; lineCount : in unsigned_types.Unsigned32);
   --# derives null from currentLine, length, lineCount;

procedure printParseErrorInfo(currentLine : in rr_type.LineFromFileType;
   Length : IN Natural; lineCount : in unsigned_types.Unsigned32);
   --# derives null from currentLine, length, lineCount;

PROCEDURE printLineLengthErrorInfo(CurrentLine : IN Rr_Type.LineFromFileType;
   length : IN Natural; lineCount : IN unsigned_types.Unsigned32);
   --# derives null from currentLine, length, lineCount;

PROCEDURE printSOARecordErrorInfo(CurrentLine : IN Rr_Type.LineFromFileType;
   length : IN Natural; lineCount : IN unsigned_types.Unsigned32);
   --# derives null from currentLine, length, lineCount;

PROCEDURE printAppendDomainLengthErrorInfo(CurrentLine : IN Rr_Type.LineFromFileType;
   length : IN Natural; lineCount : IN unsigned_types.Unsigned32);
   --# derives null from currentLine, length, lineCount;

PROCEDURE printDomainLengthErrorInfo(CurrentLine : IN Rr_Type.LineFromFileType;
   begIdx: IN Natural; endIdx : IN Natural);
   --# derives null from currentLine, begIdx, endIdx;

PROCEDURE printRRStringLengthErrorInfo(CurrentLine : IN Rr_Type.LineFromFileType;
   begIdx: IN Natural; endIdx : IN Natural);
   --# derives null from currentLine, begIdx, endIdx;

PROCEDURE printDNSTableFullInfo(CurrentLine : IN Rr_Type.LineFromFileType;
   lineCount : IN unsigned_types.Unsigned32);
   --# derives null from currentLine, lineCount;

PROCEDURE printKeyLengthErrorInfo(CurrentLine : IN Rr_Type.LineFromFileType;
   length : IN Natural; lineCount : IN unsigned_types.Unsigned32);
   --# derives null from currentLine, length, lineCount;

PROCEDURE PrintMissingSOARecordInfo;
   --# derives ;

end error_msgs;
