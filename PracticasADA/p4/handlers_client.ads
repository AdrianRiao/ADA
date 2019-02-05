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

package Handlers_Client is

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

	package Map_Pending_Msgs is new Maps_G(CM.Pend_Msgs_Ident,
										   CM.Pend_Msgs_Data,
										   100,
										   CM."=");
										   
	package Retransmission_Times_List is new Ordered_List_G(ART.Time,
															CM.Pend_Msgs_Ident,
															ART."=",
															ART."<",
															100);
										   
	package MPM renames Map_Pending_Msgs;
	package RTL renames Retransmission_Times_List;
	
	Pending_Msgs: MPM.Map;
	Retransmission_Times: RTL.Map;
	Max_Retrans: Natural;
	Client_EP_Handler, Server_EP: LLU.End_Point_Type;
	Nick, Text: ASU.Unbounded_String;
	Plazo_Retrans: Duration;
	
	--indica si he sido aceptado en el chat
	Accepted: Boolean;
	
	--Numero de secuencia de mis mensajes salientes
	--(El primer mensaje es el Init)
	Seq_N: SN.Seq_N_T:= SN.Seq_N_T'First + 1;
	
	--Numero de secuencia de los mensajes asentidos por el servidor
	Seq_N_Ack: SN.Seq_N_T;
	
	--Es el número de secuencia que espero recibir
	--(El primero recibido es el Welcome)
	Seq_N_A: SN.Seq_N_T:= SN.Seq_N_T'First + 1;
	
	Extract_Server_Error: exception;
	procedure Client_Handler (From: in LLU.End_Point_Type;
							  To: in LLU.End_Point_Type;
							  P_Buffer: access LLU.Buffer_Type);
	
	
	--Guarda el mensaje Logout en la lista de mensajes pendientes
	procedure Save_Logout_MPM;
							   
	--Guarda el mensaje Writer en la lista de mensajes pendientes				   
	procedure Save_Writer_MPM;
	
							   
	--Guarda el mensaje (Logout o Writer)
	--en la lista de tiempos de retransmisión				   
	procedure Save_Msg_RTL;
	
	
	procedure Client_Timed_Procedure;
							
	
	function No_Pending_Msgs return Boolean;

end Handlers_Client;
