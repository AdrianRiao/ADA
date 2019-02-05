with Chat_Messages;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Lower_Layer_UDP;
with Maps_G;
with Ada.Calendar;
with Gnat.Calendar.Time_IO;
with Clients_Chat;
with Ada.Command_Line;
with End_Points;

package Handlers is

	package CM renames Chat_Messages;
	package ATIO renames Ada.Text_IO;
	package ASU renames Ada.Strings.Unbounded;
	package LLU renames Lower_Layer_UDP;
	package CC renames Clients_Chat;
	package ACL renames Ada.Command_Line;
	package AC renames Ada.Calendar;
	use type ASU.Unbounded_String;
	use type CM.Message_Type;
	use type AC.Time;
	use type CC.Client_Type;

	Extract_Server_Error: exception;
	procedure Client_Handler (From: in LLU.End_Point_Type;
							  To: in LLU.End_Point_Type;
							  P_Buffer: access LLU.Buffer_Type);
							  
	
	Error_Message: exception;						  
	procedure Server_Handler (From: in LLU.End_Point_Type;
							  To: in LLU.End_Point_Type;
							  P_Buffer: access LLU.Buffer_Type);
	
	
	procedure Print_Active_Clients;
	
	
	procedure Print_Inactive_Clients;

end Handlers;
