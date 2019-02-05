with Lower_Layer_UDP;
with Ada.Strings.Unbounded;
with Lower_Layer_UDP;

package Chat_Messages is

	package LLU renames Lower_Layer_UDP;
	package ASU renames Ada.Strings.Unbounded;
	
	type Message_Type is (Init, Welcome, Writer, Server, Logout);
	
	procedure Send_Init(Client_EP_Receive: in LLU.End_Point_Type;
						Client_EP_Handler: in LLU.End_Point_Type;
						Nick: in ASU.Unbounded_String;
						Server_EP: in LLU.End_Point_Type);
						
						
	procedure Send_Welcome(Client_EP_Receive: in LLU.End_Point_Type;
						   Accepted: in Boolean);
						
						
	procedure Send_Writer(Client_EP_Handler: in LLU.End_Point_Type;
						  Nick: in ASU.Unbounded_String;
						  Text: in ASU.Unbounded_String;
						  Server_EP: in LLU.End_Point_Type);
						
						
	procedure Send_Server(Nick: in ASU.Unbounded_String;
						  Text: in ASU.Unbounded_String;
						  Client_EP_Handler: in LLU.End_Point_Type);
						
						
	procedure Send_Logout(Client_EP_Handler: in LLU.End_Point_Type;
						  Nick: in ASU.Unbounded_String;
						  Server_EP: in LLU.End_Point_Type);
						  
	
	procedure Extract_Welcome(Accepted: out Boolean;
							  P_Buffer: access LLU.Buffer_Type);
							  
	
	procedure Extract_Server(Nick: out ASU.Unbounded_String;
							 Text: out ASU.Unbounded_String;
							 P_Buffer: access LLU.Buffer_Type);
							 
	
	procedure Extract_Init(Client_EP_Receive: out LLU.End_Point_Type;
						   Client_EP_Handler: out LLU.End_Point_Type;
						   Nick: out ASU.Unbounded_String;
						   P_Buffer: access LLU.Buffer_Type);
	
	
	procedure Extract_Writer(Client_EP_Handler: out LLU.End_Point_Type;
						     Nick: out ASU.Unbounded_String;
						     Text: out ASU.Unbounded_String;
						     P_Buffer: access LLU.Buffer_Type);
						     
						  
	procedure Extract_Logout(Client_EP_Handler: out LLU.End_Point_Type;
						     Nick: out ASU.Unbounded_String;
						     P_Buffer: access LLU.Buffer_Type);

end Chat_Messages;
