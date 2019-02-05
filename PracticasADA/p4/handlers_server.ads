with Chat_Messages;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Lower_Layer_UDP;
with Ordered_Maps_G;
with Hash_Maps_G;
with Ada.Calendar;
with Gnat.Calendar.Time_IO;
with Clients_Chat;
with Ada.Command_Line;
with End_Points;
with Seq_Num;
with Maps_G;
with Ordered_List_G;
with Ada.Real_Time;
with Protected_Ops;

package Handlers_Server is

	package CM renames Chat_Messages;
	package ATIO renames Ada.Text_IO;
	package ASU renames Ada.Strings.Unbounded;
	package LLU renames Lower_Layer_UDP;
	package CC renames Clients_Chat;
	package ACL renames Ada.Command_Line;
	package AC renames Ada.Calendar;
	package SN renames Seq_Num;
	package ART renames Ada.Real_Time;
	package PO renames Protected_Ops;
	use type ASU.Unbounded_String;
	use type CM.Message_Type;
	use type CM.Pend_Msgs_Ident;
	use type AC.Time;
	use type CC.Client_Type;
	use type SN.Seq_N_T;
	use type ART.Time;
	use type LLU.End_Point_Type;

	Server_EP, Client_EP_Handler: LLU.End_Point_Type;
	Plazo_Retrans: Duration;
	Max_Retrans: Natural;
	
	--Numero de secuencia de mis mensajes salientes
	My_Seq_N: SN.Seq_N_T;
	
	--Numero de secuencia de los mensajes asentidos por los clientes
	Seq_N_Ack: SN.Seq_N_T;
	
	--Características del mensaje Server
	Who: ASU.Unbounded_String; --Quién envía el mensaje server
	Text: ASU.Unbounded_String;
	
	
	Error_Message: exception;				  
	procedure Server_Handler (From: in LLU.End_Point_Type;
							  To: in LLU.End_Point_Type;
							  P_Buffer: access LLU.Buffer_Type);
	
	
	procedure Print_Active_Clients;
	
	
	procedure Print_Inactive_Clients;
	
end Handlers_Server;
