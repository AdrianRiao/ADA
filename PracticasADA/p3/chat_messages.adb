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
						  Nick: in ASU.Unbounded_String;
						  Text: in ASU.Unbounded_String;
						  Server_EP: in LLU.End_Point_Type) is
	
		Buffer: aliased LLU.Buffer_Type(1024);
		Mess: Message_Type:= Writer;
	begin
		LLU.Reset(Buffer);
		Message_Type'Output(Buffer'Access, Mess);
		LLU.End_Point_Type'Output(Buffer'Access, Client_EP_Handler);
		ASU.Unbounded_String'Output(Buffer'Access, Nick);
		ASU.Unbounded_String'Output(Buffer'Access, Text);
		LLU.Send(Server_EP, Buffer'Access);
	end Send_Writer;


	procedure Send_Server(Nick: in ASU.Unbounded_String;
						  Text: in ASU.Unbounded_String;
						  Client_EP_Handler: in LLU.End_Point_Type) is
	
		Buffer: aliased LLU.Buffer_Type(1024);
		Mess: Message_Type:= Server;
	begin
		LLU.Reset(Buffer);
		Message_Type'Output(Buffer'Access, Mess);
		ASU.Unbounded_String'Output(Buffer'Access, Nick);
		ASU.Unbounded_String'Output(Buffer'Access, Text);
		LLU.Send(Client_EP_Handler, Buffer'Access);
	end Send_Server;


	procedure Send_Logout(Client_EP_Handler: in LLU.End_Point_Type;
						  Nick: in ASU.Unbounded_String;
						  Server_EP: in LLU.End_Point_Type) is
	
		Buffer: aliased LLU.Buffer_Type(1024);
		Mess: Message_Type:= Logout;
	begin
		LLU.Reset(Buffer);
		Message_Type'Output(Buffer'Access, Mess);
		LLU.End_Point_Type'Output(Buffer'Access, Client_EP_Handler);
		ASU.Unbounded_String'Output(Buffer'Access, Nick);
		LLU.Send(Server_EP, Buffer'Access);
	end Send_Logout;
	
	
	procedure Extract_Welcome(Accepted: out Boolean;
							  P_Buffer: access LLU.Buffer_Type) is
	begin
		Accepted := Boolean'Input (P_Buffer);
	end Extract_Welcome;
	
	
	procedure Extract_Server(Nick: out ASU.Unbounded_String;
							 Text: out ASU.Unbounded_String;
							 P_Buffer: access LLU.Buffer_Type) is
	begin
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
						     Nick: out ASU.Unbounded_String;
						     Text: out ASU.Unbounded_String;
						     P_Buffer: access LLU.Buffer_Type) is
	begin
		Client_EP_Handler := LLU.End_Point_Type'Input (P_Buffer);
		Nick := ASU.Unbounded_String'Input (P_Buffer);
		Text := ASU.Unbounded_String'Input (P_Buffer);
	end Extract_Writer;
	
	
	procedure Extract_Logout(Client_EP_Handler: out LLU.End_Point_Type;
						     Nick: out ASU.Unbounded_String;
						     P_Buffer: access LLU.Buffer_Type) is
	begin
		Client_EP_Handler := LLU.End_Point_Type'Input (P_Buffer);
		Nick := ASU.Unbounded_String'Input (P_Buffer);
	end Extract_Logout;
end Chat_Messages;
