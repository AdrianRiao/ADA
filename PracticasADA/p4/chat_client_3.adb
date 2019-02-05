with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with Lower_Layer_UDP;
with Chat_Messages;
with Ada.Strings.Unbounded;
with Seq_Num;
with Protected_Ops;
with Ada.Real_Time;
with Handlers_Client;

procedure Chat_Client_3 is
	

	package ATIO renames Ada.Text_IO;
	package ACL renames Ada.Command_Line;
	package LLU renames Lower_Layer_UDP;
	package CM renames Chat_Messages;
	package ASU renames Ada.Strings.Unbounded;
	package SN renames Seq_Num;
	package ART renames Ada.Real_Time;
	package PO renames Protected_Ops;
	package HC renames Handlers_Client;
	use type ASU.Unbounded_String;
	use type CM.Message_Type;
	use type SN.Seq_N_T;
	use type ART.Time;
										  
										  
	Format_Error: exception;
	Accepted_Error: exception;
	Extract_Welcome_Error: exception;


	function Correct_Format return Boolean is
	begin
		return ACL.Argument_Count = 6 and then
			   ACL.Argument(3) /= "server";
	end Correct_Format;
	
	
	procedure Get_Server_EP (Server_EP: out LLU.End_Point_Type) is
	
		IP: ASU.Unbounded_String;
		Port: Natural;
	begin
		IP:= ASU.To_Unbounded_String (LLU.To_IP(ACL.Argument(1)));
		Port:= Natural'Value (ACL.Argument(2));
		Server_EP:= LLU.Build (ASU.To_String(IP), Port);
	end Get_Server_EP;


	procedure Receive_Welcome(Client_EP: in LLU.End_Point_Type;
							  Accepted: out Boolean;
							  Plazo_Retrans: in Duration;
							  Max_Retrans: in Natural) is
	
		Buffer: aliased LLU.Buffer_Type(1024);
		Expired: Boolean;
		Mess: CM.Message_Type;
		Cont: Natural:= 0; --Indica las veces que enviamos el mensaje
		Retransmissions: Natural:= 0;
	begin
		loop
			LLU.Reset(Buffer);
			LLU.Receive (Client_EP, 
						 Buffer'Access, 
						 Plazo_Retrans,
						 Expired);

			if Expired and Retransmissions = Max_Retrans then
				raise Accepted_Error;
			elsif Expired and Retransmissions < Max_Retrans then
				CM.Send_Init(Client_EP, HC.Client_EP_Handler,
							 HC.Nick, HC.Server_EP);
			elsif not Expired then
				Mess := CM.Message_Type'Input (Buffer'Access);
				if Mess = CM.Welcome then
					CM.Extract_Welcome(Accepted, Buffer'Access);
				else
					raise Extract_Welcome_Error; --No debería ocurrir
				end if;
			end if;
			Cont := Cont + 1;
			Retransmissions := Cont - 1;
			
			exit when not Expired or Retransmissions = Max_Retrans;
		end loop;
	end Receive_Welcome;
	
	
	Client_EP_Receive: LLU.End_Point_Type;
	Min_Delay, Max_Delay, Fault_Pct: Natural;
begin
	
	if not Correct_Format then 
		raise Format_Error;
	end if;
	
	Min_Delay := Natural'Value (ACL.Argument(4));
	Max_Delay := Natural'Value (ACL.Argument(5));
	Fault_Pct := Natural'Value (ACL.Argument(6));
	HC.Plazo_Retrans := 2 * Duration(Max_Delay) / 1000;
	HC.Max_Retrans := 10 + ((Fault_Pct/10) * (Fault_Pct/10));
	LLU.Set_Faults_Percent (Fault_Pct);
	LLU.Set_Random_Propagation_Delay (Min_Delay, Max_Delay);
	
	Get_Server_EP(HC.Server_EP);
	LLU.Bind_Any(Client_EP_Receive);
	LLU.Bind_Any(HC.Client_EP_Handler, HC.Client_Handler'Access);
	HC.Nick:= ASU.To_Unbounded_String (ACL.Argument(3));
	CM.Send_Init(Client_EP_Receive, HC.Client_EP_Handler,
				 HC.Nick, HC.Server_EP);
	Receive_Welcome(Client_EP_Receive, 
					HC.Accepted, 
					HC.Plazo_Retrans, 
					HC.Max_Retrans);
	
	if HC.Accepted then
		ATIO.Put_Line("Mini-Chat v3.0: Welcome " &
					  ASU.To_String(HC.Nick));
		
		loop
			ATIO.Put (">> ");
			HC.Text:= ASU.To_Unbounded_String (ATIO.Get_Line);
			
			--Si no hay mensajes pendientes programo el  
			--procedimiento de gestión de retransmisiones
			if HC.No_Pending_Msgs then
				PO.Program_Timer_Procedure
				(HC.Client_Timed_Procedure'Access,
				 ART.Clock + ART.Seconds(Integer(HC.Plazo_Retrans)));
			end if;
			
			if HC.Text = ".quit" then
				CM.Send_Logout (HC.Client_EP_Handler, HC.Seq_N, 
							    HC.Nick, HC.Server_EP);
				
				PO.Protected_Call(HC.Save_Logout_MPM'Access);
				
				PO.Protected_Call(HC.Save_Msg_RTL'Access);
				
				loop
					exit when HC.No_Pending_Msgs;
				end loop;
			else
				CM.Send_Writer (HC.Client_EP_Handler, HC.Seq_N, HC.Nick,
							    HC.Text, HC.Server_EP);
							   
				PO.Protected_Call(HC.Save_Writer_MPM'Access);
				
				PO.Protected_Call(HC.Save_Msg_RTL'Access);
			end if;

			
			HC.Seq_N := HC.Seq_N + 1;
			exit when HC.Text = ".quit";
		end loop;

	else
		ATIO.Put_Line("Mini-Chat v3.0: IGNORED new user " &
					  ASU.To_String(HC.Nick) &
					  ", nick already used");
	end if;
	
	LLU.Finalize;
exception
	when Format_Error =>
		ATIO.Put_Line ("usage: chat_client_3 <PC_Server>" &
					   " <Port_Server> <Nickname> <Min_Delay>" &
					   " <Max_Delay> <Fault_Pct>");
		LLU.Finalize;
		
	when Accepted_Error =>
		ATIO.Put_Line ("Server unreachable");
		LLU.Finalize;
		
	when Except:others =>
		ATIO.Put_Line ("Excepción imprevista: " &
					   Ada.Exceptions.Exception_Name (Except) & " en: " &
					   Ada.Exceptions.Exception_Message (Except));
		LLU.Finalize;
end Chat_Client_3;
