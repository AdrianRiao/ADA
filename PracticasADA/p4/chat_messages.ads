with Lower_Layer_UDP;
with Ada.Strings.Unbounded;
with Lower_Layer_UDP;
with Seq_Num;
with End_Points;

package Chat_Messages is

	package LLU renames Lower_Layer_UDP;
	package ASU renames Ada.Strings.Unbounded;
	package SN renames Seq_Num;
	use type End_Points.LLU.End_Point_Type;
	use type SN.Seq_N_T;
	
	type Message_Type is (Init, Welcome, Writer, Server, Logout, Ack);
	
	type Pend_Msgs_Ident is record
		EP_Source: LLU.End_Point_Type;
		EP_Destination: LLU.End_Point_Type;
		Seq_N: SN.Seq_N_T;
		Retrans: Natural; --Indica el número de retrans. de este msg
	end record;
	
	type Pend_Msgs_Data is record
		Mess: Message_Type;
		Nick: ASU.Unbounded_String;
		Text: ASU.Unbounded_String;
	end record;
	
	
	procedure Send_Init(Client_EP_Receive: in LLU.End_Point_Type;
						Client_EP_Handler: in LLU.End_Point_Type;
						Nick: in ASU.Unbounded_String;
						Server_EP: in LLU.End_Point_Type);
						
						
	procedure Send_Welcome(Client_EP_Receive: in LLU.End_Point_Type;
						   Accepted: in Boolean);
						
						
	procedure Send_Writer(Client_EP_Handler: in LLU.End_Point_Type;
						  Seq_N: in SN.Seq_N_T;
						  Nick: in ASU.Unbounded_String;
						  Text: in ASU.Unbounded_String;
						  Server_EP: in LLU.End_Point_Type);
						
						
	procedure Send_Server(Server_EP: in LLU.End_Point_Type;
						  Seq_N: in SN.Seq_N_T;
						  Nick: in ASU.Unbounded_String;
						  Text: in ASU.Unbounded_String;
						  Client_EP_Handler: in LLU.End_Point_Type);
						
						
	procedure Send_Logout(Client_EP_Handler: in LLU.End_Point_Type;
						  Seq_N: in SN.Seq_N_T;
						  Nick: in ASU.Unbounded_String;
						  Server_EP: in LLU.End_Point_Type);
						  
						  
	procedure Send_Ack(From: in LLU.End_Point_Type;
					   Seq_N: in SN.Seq_N_T;
					   To: in LLU.End_Point_Type);
						  
	
	procedure Extract_Welcome(Accepted: out Boolean;
							  P_Buffer: access LLU.Buffer_Type);
							  
	
	procedure Extract_Server(Server_EP: out LLU.End_Point_Type;
							 Seq_N: out SN.Seq_N_T;
							 Nick: out ASU.Unbounded_String;
							 Text: out ASU.Unbounded_String;
							 P_Buffer: access LLU.Buffer_Type);
							 
	
	procedure Extract_Init(Client_EP_Receive: out LLU.End_Point_Type;
						   Client_EP_Handler: out LLU.End_Point_Type;
						   Nick: out ASU.Unbounded_String;
						   P_Buffer: access LLU.Buffer_Type);
	
	
	procedure Extract_Writer(Client_EP_Handler: out LLU.End_Point_Type;
							 Seq_N: out SN.Seq_N_T;
						     Nick: out ASU.Unbounded_String;
						     Text: out ASU.Unbounded_String;
						     P_Buffer: access LLU.Buffer_Type);
						     
						  
	procedure Extract_Logout(Client_EP_Handler: out LLU.End_Point_Type;
							 Seq_N: out SN.Seq_N_T;
						     Nick: out ASU.Unbounded_String;
						     P_Buffer: access LLU.Buffer_Type);
						     
						     
	procedure Extract_Ack(From: out LLU.End_Point_Type;
						  Seq_N: out SN.Seq_N_T;
						  P_Buffer: access LLU.Buffer_Type);
	

	function New_Pend_Msgs_Ident (EP_Source_A: LLU.End_Point_Type;
								  EP_Destination_A: LLU.End_Point_Type;
								  Seq_N_A: SN.Seq_N_T;
								  Retrans_A: natural) return Pend_Msgs_Ident;
								  
								  
	function New_Pend_Msgs_Data (Mess_A: Message_Type;
								 Nick_A: ASU.Unbounded_String;
								 Text_A: ASU.Unbounded_String) return Pend_Msgs_Data;
								 

	function "=" (Pend_Msgs_Ident1: Pend_Msgs_Ident; 
				  Pend_Msgs_Ident2: Pend_Msgs_Ident) return Boolean;

end Chat_Messages;
