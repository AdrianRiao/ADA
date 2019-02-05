package body Handlers_Client is


	function No_Pending_Msgs return Boolean is
		Point : MPM.Cursor:= MPM.First(Pending_Msgs);
	begin
		return not MPM.Has_Element(Point);
	end No_Pending_Msgs;


	procedure Save_Msg_RTL is
		Key: ART.Time;
		Value: CM.Pend_Msgs_Ident;
		Retrans_Time: ART.Time;
		Retrans: Natural:= 0;
	begin
		Retrans_Time:= ART.Clock + ART.Seconds(Integer(Plazo_Retrans));
		Key:= Retrans_Time;
		
		Value:= CM.New_Pend_Msgs_Ident(Client_EP_Handler,
									   Server_EP,
									   Seq_N,
									   Retrans);
									   
		RTL.Put (Retransmission_Times, Key, Value);
	end Save_Msg_RTL;


	procedure Save_Logout_MPM is
	
		Key: CM.Pend_Msgs_Ident;
		Value: CM.Pend_Msgs_Data;
		Mess: CM.Message_Type:= CM.Logout;
		--En los mensajes Logout prescindimos del comentario
		Text: ASU.Unbounded_String:= ASU.To_Unbounded_String("");
		Retrans: Natural:= 0;
	begin
		Key:= CM.New_Pend_Msgs_Ident(Client_EP_Handler,
									 Server_EP,
									 Seq_N,
									 Retrans);
		
		Value:= CM.New_Pend_Msgs_Data(Mess, Nick, Text);	
		MPM.Put(Pending_Msgs, Key, Value);
		
	end Save_Logout_MPM;


	procedure Save_Writer_MPM is
	
		Key: CM.Pend_Msgs_Ident;
		Value: CM.Pend_Msgs_Data;
		Mess: CM.Message_Type:= CM.Writer;
		Retrans: Natural:= 0;
	begin
		Key:= CM.New_Pend_Msgs_Ident(Client_EP_Handler,
									 Server_EP,
									 Seq_N,
									 Retrans);
		
		Value:= CM.New_Pend_Msgs_Data(Mess, Nick, Text);		
		MPM.Put(Pending_Msgs, Key, Value);
	end Save_Writer_MPM;
	
	
	procedure Delete_In_Retransmission_Times is
	
		Point: RTL.Cursor:= RTL.First(Retransmission_Times);
		Done: Boolean:= False;
		Element: RTL.Element_Type;
		Key: ART.Time;
		Value: CM.Pend_Msgs_Ident;
	begin
		while RTL.Has_Element(Point) and not Done loop
			Element:= RTL.Element(Point);
			Key:= Element.Key;
			Value:= Element.Value;
			if Value.Seq_N = Seq_N_Ack then
				RTL.Delete(Retransmission_Times, Key, Done);
			end if;
			RTL.Next(Point);
		end loop;
	end Delete_In_Retransmission_Times;
	
	
	procedure Delete_In_Pending_Msgs is
	
		Point: MPM.Cursor:= MPM.First(Pending_Msgs);
		Done: Boolean:= False;
		Element: MPM.Element_Type;
		Key: CM.Pend_Msgs_Ident;
	begin
		while MPM.Has_Element(Point) and not Done loop
			Element:= MPM.Element(Point);
			Key:= Element.Key;
			if Key.Seq_N = Seq_N_Ack then
				MPM.Delete(Pending_Msgs, Key, Done);
			end if;
			MPM.Next(Point);
		end loop;
	end Delete_In_Pending_Msgs;
	
	
	procedure Delete_Pending_Msg is
	begin
		Delete_In_Pending_Msgs;
		Delete_In_Retransmission_Times;
	end Delete_Pending_Msg;
	


	procedure Retrans_Msg (Identifier: CM.Pend_Msgs_Ident) is
	
		Data: CM.Pend_Msgs_Data;
		EP_Source: LLU.End_Point_Type;
		EP_Destination: LLU.End_Point_Type;
		Seq_N: SN.Seq_N_T;
		Retrans: Natural;
		Mess: CM.Message_Type;
		Nick: ASU.Unbounded_String;
		Text: ASU.Unbounded_String;
		Success: Boolean;
		Key, Retrans_Time: ART.Time;
		Value: CM.Pend_Msgs_Ident;
	begin
		MPM.Get (Pending_Msgs, Identifier, Data, Success);
		EP_Source := Identifier.EP_Source;
		EP_Destination := Identifier.EP_Destination;
		Seq_N := Identifier.Seq_N;
		Retrans := Identifier.Retrans;
		Mess := Data.Mess;
		Nick := Data.Nick;
		Text := Data.Text;
		
		if Mess = CM.Writer then
			CM.Send_Writer (EP_Source, Seq_N, Nick,
							Text, EP_Destination);
		elsif Mess = CM.Logout then
			CM.Send_Logout (EP_Source, Seq_N, 
							Nick, EP_Destination);	
		end if;
		
		--Guardo otra vez el mensaje en Retransmission_Times
		Retrans_Time:= ART.Clock + ART.Seconds(Integer(Plazo_Retrans));
		Key:= Retrans_Time;
		
		Value:= CM.New_Pend_Msgs_Ident(Client_EP_Handler,
									   Server_EP,
									   Seq_N,
									   Retrans);
									   
		RTL.Put (Retransmission_Times, Key, Value);
	end Retrans_Msg;
	
	
	--Aumenta en 1 el contador de Retrans de un mensaje pendiente
	procedure Change_Retrans_MPM (Identifier: CM.Pend_Msgs_Ident) is

		Point: MPM.Cursor:= MPM.First(Pending_Msgs);
		Done: Boolean:= False;
		Element: MPM.Element_Type;
		Key: CM.Pend_Msgs_Ident;
	begin
		while MPM.Has_Element(Point) and not Done loop
			Element:= MPM.Element(Point);
			Key:= Element.Key;
			if Key = Identifier then
				Key.Retrans := Key.Retrans + 1;
				Done:= True;
			end if;
			MPM.Next(Point);
		end loop;
	end Change_Retrans_MPM;
	
	
	procedure Client_Timed_Procedure is

		Point : RTL.Cursor := RTL.First(Retransmission_Times);
		Element : RTL.Element_Type;
		Key : ART.Time;
		Value: CM.Pend_Msgs_Ident;
		Time_Now : ART.Time := ART.Clock;
		Done : Boolean := False;
		Success : Boolean;
	begin
		while RTL.Has_Element(Point) and not Done loop
			Element := RTL.Element(Point);
			Key := Element.Key;
			Value := Element.Value;
			
			if Key <= Time_Now then
				if Value.Retrans < Max_Retrans then
					Change_Retrans_MPM (Value);
					Value.Retrans := Value.Retrans + 1;
					RTL.Delete (Retransmission_Times, Key, Success);
					Retrans_Msg (Value);
				
				--Llega al Máximo de Retransmisiones
				else
					RTL.Delete (Retransmission_Times, Key, Success);
					MPM.Delete (Pending_Msgs, Value, Success);
				end if;
			else
				Done := True;
			end if;
		end loop;
		
		--Caso en el que haya más mensajes pendientes vuelvo a programar
		if RTL.Has_Element(Point) then
			Element := RTL.Element(Point);
			Key := Element.Key;
			PO.Program_Timer_Procedure (Client_Timed_Procedure'Access, 
										Key);
		end if;
	end Client_Timed_Procedure;
	
	
	procedure Client_Handler (From: in LLU.End_Point_Type;
							  To: in LLU.End_Point_Type;
							  P_Buffer: access LLU.Buffer_Type) is

		Nick, Text: ASU.Unbounded_String;
		Mess: CM.Message_Type;
		Server_EP: LLU.End_Point_Type;
		Client_EP_Handler: LLU.End_Point_Type:= To;
		
		--Es el número de secuencia que me llega
		Seq_N: SN.Seq_N_T; 
	begin
		if Accepted then
		
			Mess := CM.Message_Type'Input (P_Buffer);
			if Mess = CM.Server then
				CM.Extract_Server(Server_EP, Seq_N, Nick, Text, P_Buffer);
				
				if Seq_N < Seq_N_A then
					CM.Send_Ack (Client_EP_Handler ,Seq_N, Server_EP);
				elsif Seq_N = Seq_N_A then
					CM.Send_Ack (Client_EP_Handler ,Seq_N, Server_EP);
					Seq_N_A := Seq_N_A + 1;
					ATIO.New_Line;
					ATIO.Put_Line(ASU.To_String(Nick) & ": " &
								  ASU.To_String(Text));
					ATIO.Put (">> ");
				end if;
			
			elsif Mess = CM.Ack then 
				CM.Extract_Ack (Server_EP, Seq_N_Ack, P_Buffer);
				PO.Protected_Call(Delete_Pending_Msg'Access);		
			else
				raise Extract_Server_Error; --No debería ocurrir
			end if;
			
		end if;
	end Client_Handler;
	
end Handlers_Client;
