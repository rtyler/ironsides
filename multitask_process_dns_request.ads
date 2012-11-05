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


with DNS_Network;
--# inherit DNS_types, DNS_Table_Pkg, DNS_Network, System, Protected_SPARK_IO_05, Process_DNS_Request;
package Multitask_Process_Dns_Request is
   procedure Process_Request_Tcp(
      Reply_Socket : in DNS_Network.DNS_Socket);
      --# global in out Protected_SPARK_IO_05.SPARK_IO_PO;
      --#        in DNS_Table_Pkg.DNS_Table;
      --#        in out DNS_Network.Network;
      --# derives DNS_Network.Network from DNS_Network.Network, Reply_Socket,
      --#                                  DNS_Table_Pkg.DNS_Table &
      --#         Protected_SPARK_IO_05.SPARK_IO_PO from *, DNS_Network.Network, Reply_Socket,
      --#                                                DNS_Table_Pkg.DNS_Table;

end Multitask_Process_Dns_Request;

