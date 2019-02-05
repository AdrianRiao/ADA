package body Clients_Chat is

	function Nick (Client: Client_Type) return ASU.Unbounded_String is
	begin
		return Client.Nick;
	end Nick;
	
	
	function EP (Client: Client_Type) return LLU.End_Point_Type is
	begin
		return Client.EP;
	end EP;
	
	
	function "=" (C1: Client_Type; C2: Client_Type) return Boolean is
	begin
		return Nick(C1) = Nick(C2) and EP(C1) = EP(C2);
	end "=";
	
	
	procedure Copy (C1: in Client_Type; C2: out Client_Type) is	
	begin
		C2.Nick:= Nick(C1);
		C2.EP:= EP(C1);
	end Copy;
	
	
	function New_Client_Chat (Nick_A: ASU.Unbounded_String;
							  EP_A: LLU.End_Point_Type) 
							  return Client_Type is
	begin
		return (Nick => Nick_A, EP => EP_A);
	end New_Client_Chat;

end Clients_Chat;
