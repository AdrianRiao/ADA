with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Word_lists;
with Ada.Characters.Handling;
with Ada.Strings.Maps;


procedure Words is

	package ACL renames Ada.Command_Line;
	package ASU renames Ada.Strings.Unbounded;
	package ATIO renames Ada.Text_IO;
	package WL renames Word_lists;
	package ACH renames Ada.Characters.Handling;
	package ASM renames Ada.Strings.Maps;
	use type ASU.Unbounded_String;
	
	Format_Error: exception;
	
	
	--Nos dice si nuestra cadena está vacía o no
	function Empty_US(US: in ASU.Unbounded_String) return Boolean is
	begin
		return US = "";
	end Empty_US;
	
	
	procedure To_Lowercase (Word: in out ASU.Unbounded_String) is
	begin
		Word:= ASU.To_Unbounded_String(ACH.To_Lower(
									   ASU.To_String(Word)));
	end To_Lowercase;
	
	
	procedure Show_Words (List: in out WL.Word_List_Type) is
	begin
		ATIO.New_Line;
		WL.Print_All(List);		  
	end Show_Words;
	
	
	procedure Search_word (List: in out WL.Word_List_Type) is
		Word: ASU.Unbounded_String;
		Count: Natural;
	begin
		ATIO.Put("Word? ");
		Word:= ASU.To_Unbounded_String(ATIO.Get_Line);
		To_Lowercase(Word);
		WL.Search_Word(List, Word, Count);
		ATIO.New_Line;
		ATIO.Put_Line("|" & ASU.To_String(Word) &
					  "| -" & Natural'Image(Count));			  
	end Search_word;
	
	
	procedure Delete_Word (List: in out WL.Word_List_Type) is
		Word: ASU.Unbounded_String;
	begin
		ATIO.Put("Word? ");
		Word:= ASU.To_Unbounded_String(ATIO.Get_Line);
		To_Lowercase(Word);
		WL.Delete_Word(List, Word);
		ATIO.New_Line;
		ATIO.Put_Line("|" & ASU.To_String(Word) &
					  "| deleted");
					  
	exception
		when WL.Word_List_Error =>
			ATIO.Put_Line("La palabra no está en la lista");
	end Delete_Word;
	
	
	procedure Add_Word (List: in out WL.Word_List_Type) is
		Word: ASU.Unbounded_String;
	begin
		ATIO.Put("Word? ");
		Word:= ASU.To_Unbounded_String(ATIO.Get_Line);
		To_Lowercase(Word);
		WL.Add_Word(List, Word);
		ATIO.Put_Line("Word |" & ASU.To_String(Word) &
					  "| added");
	end Add_Word;
	
	
	procedure Interactive_Mode (List: in out WL.Word_List_Type) is
		Entrada: Integer;
		Done: Boolean:= False;
	begin
		loop
			ATIO.New_Line;
			ATIO.Put_Line("Options");
			ATIO.Put_Line("1 Add word");
			ATIO.Put_Line("2 Delete word");
			ATIO.Put_Line("3 Search word");
			ATIO.Put_Line("4 Show all words");
			ATIO.Put_Line("5 Quit");
			ATIO.New_Line;
			ATIO.Put("Your option? ");
			
			begin
				Entrada:= Integer'Value(ATIO.Get_Line);
				
				case Entrada is
					when 1 =>
						Add_Word(List);
					when 2 =>
						Delete_Word(List);
					when 3 =>
						Search_word(List);
					when 4 =>
						Show_Words(List);
					when 5 =>
						Done:= True;
					when others =>
						ATIO.Put_Line("Entrada Inválida");
				end case;
				
			exception
				when Constraint_Error =>
					ATIO.Put_Line("Entrada Inválida");
			end;
			exit when Done;
		end loop;
		
		ATIO.New_Line;
	end Interactive_Mode;
	
	
	procedure Get_Words (Line: in out ASU.Unbounded_String;
						 List: out WL.Word_List_Type) is
		Word: ASU.Unbounded_String;
		Pos: Natural; --Posicion del Blanco
	begin
		loop
			Pos:= ASU.Index(Line, 
			ASM.To_Set(" ,.-!#$%&'*+/:;<=>?@[\]^_`{|}~""()"));
			
			case Pos is
				when 1 =>
					ASU.Tail (Line, ASU.Length(Line)-Pos);
					
				when 0 =>
				
					if not Empty_US(Line) then
						Word:= Line;
						To_Lowercase(Word);
						WL.Add_Word(List, Word);
					end if;
					
				when others =>
					Word:= ASU.Head (Line, Pos-1);
					To_Lowercase(Word);
					WL.Add_Word(List, Word);
					ASU.Tail (Line, ASU.Length(Line)-Pos);
			end case;
			
			exit when Pos = 0;
		end loop;	
	end Get_Words;
	
	
	procedure Read_Fich (Fich_Name: in ASU.Unbounded_String;
						 List: out WL.Word_List_Type) is
		Fich: ATIO.File_Type;
		End_Fich: Boolean := False;
		Line: ASU.Unbounded_String;
	begin
		ATIO.Open(Fich, ATIO.In_File, ASU.To_String(Fich_Name));
		
		loop
			begin
				Line:= ASU.To_Unbounded_String
					   (Ada.Text_IO.Get_Line(Fich));
				Get_Words(Line, List);
				
			exception
				when ATIO.End_Error =>
					End_Fich := True;
			end;
			
			exit when End_Fich;
		end loop;
		
		ATIO.Close(Fich);
	exception
		when ATIO.Name_Error =>
			ATIO.Put_Line (ASU.To_String(Fich_Name) & 
						   ": file not found");
	end Read_Fich;
	
	
	function Is_Interactive_Mode return Boolean is
	begin
		return ACL.Argument_Count = 2;
	end Is_Interactive_Mode;
	
	
	function Formato_Correcto return Boolean is
	begin
		if ACL.Argument_Count = 1 and then
		   ACL.Argument(1) /= "-i" then
			return True;
			
		elsif ACL.Argument_Count = 2 and then
		      ACL.Argument(1) = "-i" then     
			return True;
			
		else
			return False;
			
		end if;
	end Formato_Correcto;


	Fich_Name: ASU.Unbounded_String;
	List: WL.Word_List_Type;
	Max_Word: ASU.Unbounded_String;
	Max_Count: Natural;

begin

	if not Formato_Correcto then 
		raise Format_Error;
	end if;
	
	if Is_Interactive_Mode then
		Fich_Name:= ASU.To_Unbounded_String(ACL.Argument(2));
		Read_Fich(Fich_Name, List);
		Interactive_Mode(List);
	else
		Fich_Name:= ASU.To_Unbounded_String(ACL.Argument(1));
		Read_Fich(Fich_Name, List);
	end if;

	WL.Max_Word(List, Max_Word, Max_Count);
	ATIO.Put_Line ("The most frequent word: |" &
				   ASU.To_String(Max_Word) &
				   "| -" & Natural'Image(Max_Count));
	WL.Delete_List(List); --Se libera la memoria
	
exception
	when Format_Error =>
		ATIO.Put_Line ("usage: words [-i] <filename>");
		
	when WL.Word_List_Error =>
		WL.Delete_List(List); --Se libera la memoria
		
	when Except:others =>
		ATIO.Put_Line ("Excepción imprevista: " &
					   Ada.Exceptions.Exception_Name (Except) & " en: " &
					   Ada.Exceptions.Exception_Message (Except));
end Words;
