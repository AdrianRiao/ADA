package body End_Points is

	--Sabiendo que la funcion Image devuelve un String así:
	--LOWER_LAYER.INET.UDP.UNI.ADDRESS IP: 193.147.49.72, Port:  1025
	function IP (EP: LLU.End_Point_Type) return ASU.Unbounded_String is
							   
		S: ASU.Unbounded_String;
		N: Natural:= 0;
	begin
		S:= ASU.To_Unbounded_String(LLU.Image(EP));
		
		for A in 1..2 loop
			N:= ASU.Index (S, " ");
			ASU.Tail (S, ASU.Length(S)-N);
		end loop;
		
		N:= ASU.Index (S, ",");
		ASU.Head (S, N-1);
		
		return S;
	end IP;
	
	
	--Sabiendo que la funcion Image devuelve un String así:
	--LOWER_LAYER.INET.UDP.UNI.ADDRESS IP: 193.147.49.72, Port:  1025
	function Port (EP: LLU.End_Point_Type) return ASU.Unbounded_String is
	
		S: ASU.Unbounded_String;
		N: Natural:= 0;
	begin
		S:= ASU.To_Unbounded_String(LLU.Image(EP));
	
		for A in 1..2 loop
			N:= ASU.Index (S, ":");
			ASU.Tail (S, ASU.Length(S)-N);
		end loop;
		
		--Elimino los blancos del puerto
		loop
			N:= ASU.Index (S, " ");
			if N /= 0 then
				ASU.Tail (S, ASU.Length(S)-N);
			end if;
			exit when N = 0;
		end loop;
		
		return S;
	end Port;
	

	function "=" (EP1: LLU.End_Point_Type; 
				  EP2: LLU.End_Point_Type) return Boolean is
	begin
		return IP(EP1) = IP(EP2) and Port(EP1) = Port(EP2);
	end "=";

end End_Points;
