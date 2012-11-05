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

WITH DNS_Network;
with DNS_Network_Receive;
with Process_Dns_Request;
with DNS_Types;
with Ada.Synchronous_Task_Control;
--# inherit DNS_Types, DNS_Table_Pkg, DNS_Network_Receive, DNS_Network,
--#     Protected_SPARK_IO_05,
--#     process_dns_request,
--#     Ada.Synchronous_Task_Control;
package Udp_Dns_Package
--# own protected Startup_Suspension (Suspendable);
--#     task The_Task : Udp_Dns_Task;
is
   procedure Initialization_Done;
   --# global out Startup_Suspension;
   --# derives Startup_Suspension from ;

   task type Udp_Dns_Task
--# global in out DNS_Network.Network;
--#        in out Protected_SPARK_IO_05.SPARK_IO_PO;
--#        in DNS_Table_Pkg.DNS_Table;
--#        out Startup_Suspension;
--# derives DNS_Network.Network from DNS_Table_Pkg.DNS_Table, DNS_Network.Network &
--#         Protected_SPARK_IO_05.SPARK_IO_PO from *, DNS_Table_Pkg.DNS_Table, DNS_Network.Network &
--#         Startup_Suspension from  ;
--# declare suspends => Startup_Suspension;
   is
      pragma Priority(0);
   end Udp_Dns_Task;

end Udp_dns_package;
