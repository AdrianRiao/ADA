with Ada.Unchecked_Deallocation;
with Ada.Text_IO;

package body Word_Lists is

	package ATIO renames Ada.Text_IO;
	use type ASU.Unbounded_String;

	procedure Free is new
		Ada.Unchecked_Deallocation
			(Cell, Word_List_Type);



	function Repeated_word(Ptr: Word_List_Type;
						   Word:ASU.Unbounded_String) 
						   return Boolean is
	begin
		return Ptr.Word = Word;
	end Repeated_word;


	function Empty_List(List: Word_List_Type) return Boolean is
	begin
		return List = Null;
	end Empty_List;


	function Last_Cell(Ptr: Word_List_Type) return Boolean is
	begin
		return Ptr.Next = Null;
	end Last_Cell;


	procedure Add_Word (List: in out Word_List_Type; 
						Word: in ASU.Unbounded_String) is
						
		P_Aux: Word_List_Type;
	begin
		if Empty_List(List) then
			List:= new Cell;
			List.Word:= Word;
			List.Count:= 1;
		else
			P_Aux:= List;
			
			while not (Last_Cell(P_Aux) or
					   Repeated_word(P_Aux, Word)) loop		   
				P_Aux:= P_Aux.Next;
			end loop;
			
			if Repeated_word(P_Aux, Word) then
				P_Aux.Count:= P_Aux.Count + 1;
			else
				P_Aux.Next:= new Cell;
				P_Aux.Next.Word:= Word;
				P_Aux.Next.Count:= 1;
			end if;
			
		end if;
	end Add_Word;


	procedure Delete_Word (List: in out Word_List_Type; 
						   Word: in ASU.Unbounded_String) is

		P_Aux1: Word_List_Type; --Ptr que elimina la celda
		P_Aux2: Word_List_Type; --Va detrás de P_Aux1
		P_Aux3: Word_List_Type; --Va delante de P_Aux1
		Done: Boolean:= False;
	begin

		if Empty_List(List) then
			raise Word_List_Error;
		else
			P_Aux1:= List;
			P_Aux2:= List;
			
			loop
				if Repeated_Word(P_Aux1, Word) then
				
					--caso en el que solo hay una palabra en la lista
					if Last_Cell(P_Aux1) and P_Aux1 = P_Aux2 then 
						P_Aux2.Next:= Null;
						List:= Null;
						Free(P_Aux1);
						
					elsif not Last_Cell(P_Aux1) and P_Aux1 = P_Aux2 then
						P_Aux3:= P_Aux1.Next;
						List:= P_Aux3; --Uno la cadena
						P_Aux2:= Null;
						Free(P_Aux1);
						
					elsif Last_Cell(P_Aux1) then
						P_Aux2.Next:= Null;
						Free(P_Aux1);
						
					else
						P_Aux3:= P_Aux1.Next; 
						P_Aux2.Next:= P_Aux3; --Uno la cadena
						Free(P_Aux1);
					end if;
					Done:=True;
					
				elsif Last_Cell(P_Aux1) then
					raise Word_List_Error;
					
				else
					P_Aux2:= P_Aux1;
					P_Aux1:= P_Aux1.Next;
				end if;
				
				exit when Done;
			end loop;
		end if;
	end Delete_Word;


	procedure Search_Word (List: in Word_List_Type;
						   Word: in ASU.Unbounded_String;
						   Count: out Natural) is

		P_Aux: Word_List_Type;
		Done: Boolean:= False;
	begin

		if Empty_List(List) then
			Count:= 0;
		else
			P_Aux:= List;
		
			loop
			
				if Repeated_Word(P_Aux, Word) then
					Count:= P_Aux.Count;
					Done:= True;
				elsif Last_Cell(P_Aux) then
					Count:= 0;
					Done:= True;
				else
					P_Aux:= P_Aux.Next;
				end if;
					
				exit when Done;
			end loop;
		end if;
	end Search_Word;


	procedure Max_Word (List: in Word_List_Type;
						Word: out ASU.Unbounded_String;
						Count: out Natural) is

		P_Aux: Word_List_Type;
		Done: Boolean:= False;
	begin

		if Empty_List(List) then
			raise Word_List_Error;
		else
			P_Aux:= List;
			Word:= P_Aux.Word;
			Count:= P_Aux.Count;
			
			loop
				if not Last_Cell(P_Aux) then
					P_Aux:= P_Aux.Next;
					
					if P_Aux.Count > Count then
						Word:= P_Aux.Word;
						Count:= P_Aux.Count;
					end if;
				else
					Done:= True;
				end if;
				
				exit when Done;
			end loop;
		end if;
	end Max_Word;


	procedure Print_All (List: in Word_List_Type) is

		P_Aux: Word_List_Type;
		Done: Boolean:= False;
	begin

		if Empty_List(List) then
			ATIO.Put_Line("No words.");
		else
			P_Aux:= List;
			
			loop
				ATIO.Put_Line("|" &
							  ASU.To_String(P_Aux.Word) &
							  "| -" &
							  Natural'Image(P_Aux.Count));
				if not Last_Cell(P_Aux) then
					P_Aux:= P_Aux.Next;
				else
					Done:= True;
				end if;
				
				exit when Done;
			end loop;
		end if;
	end Print_All;
	
	
	procedure Delete_List (List: in out Word_List_Type) is
	begin
		while not Empty_List(List) loop
			Delete_Word(List, List.Word);
		end loop;
	end Delete_List;

end Word_Lists;
