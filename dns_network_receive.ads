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

WITH DNS_Types;
with DNS_Network;
--# inherit DNS_Types,DNS_Network;
PACKAGE Dns_Network_Receive IS
   procedure Receive_DNS_Packet_TCP
     (Packet : out DNS_Types.DNS_Tcp_Packet;
      Number_Bytes   : out DNS_Types.Packet_Length_Range;
      Socket : in DNS_Network.DNS_Socket;
      Failure : out Boolean);
   --# global in out DNS_Network.Network;
   --# derives DNS_Network.Network, Packet, Number_Bytes, Failure from DNS_Network.Network, Socket;
   --# post (not Failure) -> (Number_Bytes >= DNS_Types.Packet_Length_Range(1+DNS_Types.Header_Bits/8)
   --#      and Number_Bytes <= DNS_Network.MAX_QUERY_SIZE);

   -- Reads a single UDP packet from network on port 53
   -- Last is the number of bytes read (assuming no failure)
   procedure Receive_DNS_Packet(
      Packet : out DNS_Types.DNS_Packet;
      Number_Bytes  : out DNS_Types.Packet_Length_Range;
      Reply_Address : out DNS_Network.Network_Address_and_Port;
      Failure : out Boolean);
   --# global in out DNS_Network.Network;
   --# derives DNS_Network.Network, Packet, Number_Bytes, Reply_Address, Failure from DNS_Network.Network;
   --# post (not Failure) -> (Number_Bytes >= DNS_Types.Packet_Length_Range(1+DNS_Types.Header_Bits/8)
   --#      and Number_Bytes <= DNS_Network.MAX_QUERY_SIZE);
END Dns_Network_Receive;

