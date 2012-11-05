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

WITH Dns_Types, Rr_Type, Rr_Type.Aaaa_Record_Type, Rr_Type.Dnskey_Record_Type,
   Rr_Type.Rrsig_Record_Type, Unsigned_Types;
USE TYPE Unsigned_Types.Unsigned16, Unsigned_Types.Unsigned32;
--# inherit dns_types, Ada.Characters.Latin_1, Ada.Characters.Handling, error_msgs, rr_type, rr_type.a_record_type,
--#   rr_type.aaaa_record_type, rr_type.dnskey_record_type, rr_type.rrsig_Record_Type,
--#   rr_type.soa_record_type, unsigned_types, non_spark_stuff;

--with Ada.Text_IO, Ada.Integer_Text_IO;

PACKAGE Parser_Utilities IS

   procedure CheckAndAppendOrigin(target : in out Rr_Type.DomainNameStringType;
         Origin : in Rr_Type.DomainNameStringType;
         CurrentLine : in rr_type.LineFromFileType;
         LastPos : in Rr_Type.Linelengthindex;
         LineCount : in unsigned_types.unsigned32;
         Success : in out boolean);
   --# derives target, success from target, origin, success
   --# & null from currentLine, lastPos, lineCount;

   procedure checkValidHostName(Name: Rr_Type.DomainNameStringType; success: in out boolean);
      --# derives success from name, success;

   procedure findFirstToken(s : in rr_type.LineFromFileType;
                            length : in rr_type.LineLengthIndex;
                            T : OUT Rr_Type.RrItemType);
   --# derives T from s, length;

   procedure findNextToken(s : in rr_type.LineFromFileType;
                           length : in rr_type.LineLengthIndex;
                           BegIdx : in OUT rr_type.LineLengthIndex;
                           EndIdx : OUT rr_type.LineLengthIndex;
                           T : OUT Rr_Type.RrItemType);
   --# derives begIdx, endIdx, T from s, length, begIdx;
   --# pre begIdx <= length;
   --# post begIdx <= endIdx and begIdx <= length and endIdx <= length
   --#  and ((T = rr_type.Number) ->
   --#      (for all I in integer range begIdx..endIdx => (S(I) >= '0' and S(I) <= '9')));

   procedure convert8BitUnsigned(value: out Unsigned_Types.Unsigned8;
      ZoneFileLine : in rr_type.LineFromFileType;
      BegIdx : in rr_type.LineLengthIndex;
      EndIdx : in rr_type.LineLengthIndex;
      Success : in out Boolean);
   --# derives value from zoneFileLine, begIdx, endIdx & success from zoneFileLine, begIdx, endIdx, success;
   --# pre for all I in integer range BegIdx..EndIdx => (ZoneFileLine(I) >= '0' and ZoneFileLine(I) <= '9');

   procedure convert16BitUnsigned(value: out Unsigned_Types.Unsigned16;
      ZoneFileLine : in rr_type.LineFromFileType;
      BegIdx : in rr_type.LineLengthIndex;
      EndIdx : in rr_type.LineLengthIndex;
      Success : in out Boolean);
   --# derives value from zoneFileLine, begIdx, endIdx & success from zoneFileLine, begIdx, endIdx, success;
   --# pre for all I in integer range BegIdx..EndIdx => (ZoneFileLine(I) >= '0' and ZoneFileLine(I) <= '9');

   procedure convert32BitUnsigned(value: out Unsigned_Types.Unsigned32;
      ZoneFileLine : in rr_type.LineFromFileType;
      BegIdx : in rr_type.LineLengthIndex;
      EndIdx : in rr_type.LineLengthIndex;
      Success : in out Boolean);
   --# derives value from zoneFileLine, begIdx, endIdx & success from zoneFileLine, begIdx, endIdx, success;
   --# pre for all I in integer range BegIdx..EndIdx => (ZoneFileLine(I) >= '0' and ZoneFileLine(I) <= '9');

   procedure ConvertTimeSpec(S : in Rr_Type.LineFromFileType; begIdx: in rr_type.LineLengthIndex;
      endIdx: in Rr_Type.LineLengthIndex; RetVal : out Unsigned_Types.Unsigned32; success: in out Boolean);
   --# derives retVal,success from S, begIdx, endIdx, success;

   procedure ConvertTimeString(TimeVal : out Unsigned_Types.Unsigned32;
      TimeString: in Rr_Type.Rrsig_Record_Type.TimeStringType;
      Success : in out Boolean);
   --# derives timeVal from timeString & success from timeString, success;
   --# pre for all I in Rr_Type.Rrsig_Record_Type.TimeStringTypeIndex => (timeString(I) >= '0' and timeString(I) <= '9');

   procedure AddToKeyR(RRSIG_Rec : in out rr_type.rrsig_record_type.RRSIGRecordType;
                      S: in Rr_Type.LineFromFileType;
                      Length : in Rr_Type.LineLengthIndex;
                      allDone : out boolean;
                      success : in out boolean);
   --# derives RRSIG_Rec from RRSIG_Rec, S, Length & AllDone from S, Length
   --#   & Success from RRSIG_Rec, S, Length, Success;

   procedure AddToKey(DNSKEY_Rec : in out rr_type.dnskey_record_type.DNSKeyRecordType;
                      S: in Rr_Type.LineFromFileType;
                      Length : in Rr_Type.LineLengthIndex;
                      success : in out boolean);
   --# derives DNSKEY_Rec from DNSKEY_Rec, S, Length & Success from DNSKEY_Rec, S, Length, Success;

   function convertIpv4(s : in rr_type.LineFromFileType;
                        begIdx: IN rr_type.LineLengthIndex;
                        EndIdx: IN Rr_Type.LineLengthIndex) RETURN Unsigned_Types.Unsigned32;

   function convertIpv6(s : in rr_type.LineFromFileType;
                        begIdx: in rr_type.LineLengthIndex;
                        endIdx: in rr_type.LineLengthIndex) RETURN rr_type.aaaa_record_type.IPV6AddrType;

   function getRecordType(s : in rr_type.LineFromFileType;
                          begIdx : in rr_type.LineLengthIndex;
                    	  endIdx : in rr_type.LineLengthIndex) return dns_types.Query_Type;
   --# pre begIdx <= endIdx;


END Parser_Utilities;

