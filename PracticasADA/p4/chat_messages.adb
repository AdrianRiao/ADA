package body Chat_Messages is

	procedure Send_Init(Client_EP_Receive: in LLU.End_Point_Type;
						Client_EP_Handler: in LLU.End_Point_Type;
						Nick: in ASU.Unbounded_String;
						Server_EP: in LLU.End_Point_Type) is
						
		Buffer: aliased LLU.Buffer_Type(1024);
		Mess: Message_Type:= Init;
	begin
		LLU.Reset(Buffer);
		Message_Type'Output(Buffer'Access, Mess);
		LLU.End_Point_Type'Output(Buffer'Access, Client_EP_Receive);
		LLU.End_Point_Type'Output(Buffer'Access, Client_EP_Handler);
		ASU.Unbounded_String'Output(Buffer'Access, Nick);
		LLU.Send(Server_EP, Buffer'Access);
	end Send_Init;


	procedure Send_Welcome(Client_EP_Receive: in LLU.End_Point_Type;
						   Accepted: in Boolean) is
	
		Buffer: aliased LLU.Buffer_Type(1024);
		Mess: Message_Type:= Welcome;
	begin
		LLU.Reset(Buffer);
		Message_Type'Output(Buffer'Access, Mess);
		Boolean'Output(Buffer'Access, Accepted);
		LLU.Send(Client_EP_Receive, Buffer'Access);
	end Send_Welcome;


	procedure Send_Writer(Client_EP_Handler: in LLU.End_Point_Type;
						  Seq_N: in SN.Seq_N_T;
						  Nick: in ASU.Unbounded_String;
						  Text: in ASU.Unbounded_String;
						  Server_EP: in LLU.End_Point_Type) is
	
		Buffer: aliased LLU.Buffer_Type(1024);
		Mess: Message_Type:= Writer;
	begin
		LLU.Reset(Buffer);
		Message_Type'Output(Buffer'Access, Mess);
		LLU.End_Point_Type'Output(Buffer'Access, Client_EP_Handler);
		SN.Seq_N_T'Output(Buffer'Access, Seq_N);
		ASU.Unbounded_String'Output(Buffer'Access, Nick);
		ASU.Unbounded_String'Output(Buffer'Access, Text);
		LLU.Send(Server_EP, Buffer'Access);
	end Send_Writer;


	procedure Send_Server(Server_EP: in LLU.End_Point_Type;
						  Seq_N: in SN.Seq_N_T;
						  Nick: in ASU.Unbounded_String;
						  Text: in ASU.Unbounded_String;
						  Client_EP_Handler: in LLU.End_Point_Type) is
	
		Buffer: aliased LLU.Buffer_Type(1024);
		Mess: Message_Type:= Server;
	begin
		LLU.Reset(Buffer);
		Message_Type'Output(Buffer'Access, Mess);
		LLU.End_Point_Type'Output(Buffer'Access, Server_EP);
		SN.Seq_N_T'Output(Buffer'Access, Seq_N);
		ASU.Unbounded_String'Output(Buffer'Access, Nick);
		ASU.Unbounded_String'Output(Buffer'Access, Text);
		LLU.Send(Client_EP_Handler, Buffer'Access);
	end Send_Server;


	procedure Send_Logout(Client_EP_Handler: in LLU.End_Point_Type;
						  Seq_N: in SN.Seq_N_T;
						  Nick: in ASU.Unbounded_String;
						  Server_EP: in LLU.End_Point_Type) is
	
		Buffer: aliased LLU.Buffer_Type(1024);
		Mess: Message_Type:= Logout;
	begin
		LLU.Reset(Buffer);
		Message_Type'Output(Buffer'Access, Mess);
		LLU.End_Point_Type'Output(Buffer'Access, Client_EP_Handler);
		SN.Seq_N_T'Output(Buffer'Access, Seq_N);
		ASU.Unbounded_String'Output(Buffer'Access, Nick);
		LLU.Send(Server_EP, Buffer'Access);
	end Send_Logout;
	
	
	procedure Send_Ack(From: in LLU.End_Point_Type;
					   Seq_N: in SN.Seq_N_T;
					   To: in LLU.End_Point_Type) is
	
		Buffer: aliased LLU.Buffer_Type(1024);
		Mess: Message_Type:= Ack;
	begin
		LLU.Reset(Buffer);
		Message_Type'Output(Buffer'Access, Mess);
		LLU.End_Point_Type'Output(Buffer'Access, From);
		SN.Seq_N_T'Output(Buffer'Access, Seq_N);
		LLU.Send(To, Buffer'Access);
	end Send_Ack;
	
	
	procedure Extract_Welcome(Accepted: out Boolean;
							  P_Buffer: access LLU.Buffer_Type) is
	begin
		Accepted := Boolean'Input (P_Buffer);
	end Extract_Welcome;
	
	
	procedure Extract_Server(Server_EP: out LLU.End_Point_Type;
							 Seq_N: out SN.Seq_N_T;
							 Nick: out ASU.Unbounded_String;
							 Text: out ASU.Unbounded_String;
							 P_Buffer: access LLU.Buffer_Type) is
	begin
		Server_EP := LLU.End_Point_Type'Input (P_Buffer);
		Seq_N := SN.Seq_N_T'Input (P_Buffer);
		Nick := ASU.Unbounded_String'Input (P_Buffer);
		Text := ASU.Unbounded_String'Input (P_Buffer);
	end Extract_Server;
	
	
	procedure Extract_Init(Client_EP_Receive: out LLU.End_Point_Type;
						   Client_EP_Handler: out LLU.End_Point_Type;
						   Nick: out ASU.Unbounded_String;
						   P_Buffer: access LLU.Buffer_Type) is
	begin
		Client_EP_Receive := LLU.End_Point_Type'Input (P_Buffer);
		Client_EP_Handler := LLU.End_Point_Type'Input (P_Buffer);
		Nick := ASU.Unbounded_String'Input (P_Buffer);
	end Extract_Init;
	
	
	procedure Extract_Writer(Client_EP_Handler: out LLU.End_Point_Type;
							 Seq_N: out SN.Seq_N_T;
						     Nick: out ASU.Unbounded_String;
						     Text: out ASU.Unbounded_String;
						     P_Buffer: access LLU.Buffer_Type) is
	begin
		Client_EP_Handler := LLU.End_Point_Type'Input (P_Buffer);
		Seq_N := SN.Seq_N_T'Input (P_Buffer);
		Nick := ASU.Unbounded_String'Input (P_Buffer);
		Text := ASU.Unbounded_String'Input (P_Buffer);
	end Extract_Writer;
	
	
	procedure Extract_Logout(Client_EP_Handler: out LLU.End_Point_Type;
							 Seq_N: out SN.Seq_N_T;
						     Nick: out ASU.Unbounded_String;
						     P_Buffer: access LLU.Buffer_Type) is
	begin
		Client_EP_Handler := LLU.End_Point_Type'Input (P_Buffer);
		Seq_N := SN.Seq_N_T'Input (P_Buffer);
		Nick := ASU.Unbounded_String'Input (P_Buffer);
	end Extract_Logout;
	
	
	procedure Extract_Ack(From: out LLU.End_Point_Type;
						  Seq_N: out SN.Seq_N_T;
						  P_Buffer: access LLU.Buffer_Type) is
	begin
		From := LLU.End_Point_Type'Input (P_Buffer);
		Seq_N := SN.Seq_N_T'Input (P_Buffer);
	end Extract_Ack;


	function New_Pend_Msgs_Ident (EP_Source_A: LLU.End_Point_Type;
								  EP_Destination_A: LLU.End_Point_Type;
								  Seq_N_A: SN.Seq_N_T;
								  Retrans_A: natural) return Pend_Msgs_Ident is
	begin
		return (EP_Source => EP_Source_A, 
				EP_Destination => EP_Destination_A,
				Seq_N => Seq_N_A,
				Retrans => Retrans_A);
	end New_Pend_Msgs_Ident;
	
	
	function New_Pend_Msgs_Data (Mess_A: Message_Type;
								 Nick_A: ASU.Unbounded_String;
								 Text_A: ASU.Unbounded_String) return Pend_Msgs_Data is						     
	begin
		return (Mess => Mess_A, Nick => Nick_A, Text => Text_A);
	end New_Pend_Msgs_Data;
	
	
	function "=" (Pend_Msgs_Ident1: Pend_Msgs_Ident; 
				  Pend_Msgs_Ident2: Pend_Msgs_Ident) return Boolean is
	begin
		return Pend_Msgs_Ident1.EP_Source = Pend_Msgs_Ident2.EP_Source and
			   Pend_Msgs_Ident1.EP_Destination = Pend_Msgs_Ident2.EP_Destination and
			   Pend_Msgs_Ident1.Seq_N = Pend_Msgs_Ident2.Seq_N;
	end "=";
	
end Chat_Messages;
