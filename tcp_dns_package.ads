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
with Multitask_Process_Dns_Request;
with Ada.Synchronous_Task_Control;
--# inherit DNS_Types, DNS_Network, multitask_process_dns_request, Protected_SPARK_IO_05,
--#     Ada.Synchronous_Task_Control, DNS_Table_Pkg;
package Tcp_Dns_Package
--# own protected Startup_Suspension (Suspendable);
--#     task The_Task : Tcp_Dns_Task;
is
   procedure Initialization_Done;
   --# global out Startup_Suspension;
   --# derives Startup_Suspension from ;

   task type Tcp_Dns_Task
--# global in out DNS_Network.Network;
--#        in out Protected_SPARK_IO_05.SPARK_IO_PO;
--#        in DNS_Table_Pkg.DNS_Table;
--#        out Startup_Suspension;
--# derives DNS_Network.Network from DNS_Network.Network, DNS_Table_Pkg.DNS_Table &
--#         Protected_SPARK_IO_05.SPARK_IO_PO from *, DNS_Table_Pkg.DNS_Table, DNS_Network.Network &
--#         Startup_Suspension from  ;
--# declare suspends => Startup_Suspension;
   is
      pragma Priority(0);
   end Tcp_Dns_Task;

end tcp_dns_package;
