with Lower_Layer_UDP;
with Handlers;

with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Exceptions;

procedure Server is
   package LLU renames Lower_Layer_UDP;
   package ASU renames Ada.Strings.Unbounded;
   use type ASU.Unbounded_String;

   Server_EP: LLU.End_Point_Type;
   Finish: Boolean;
   C: Character;
begin
   
   Server_EP := LLU.Build ("127.0.0.1", 6123);

   -- Se ata al End_Point para poder recibir en él con un handler/manejador.
   -- Tras llamar a Bind ya se pueden estar recibiendo mensajes automáticamente
   -- en el manejador
   LLU.Bind (Server_EP, Handlers.Server_Handler'Access);

   -- A la vez que se están recibiendo mensajes en el manejador se
   -- siguen ejecutando las siguientes sentencias en el programa principal
   Ada.Text_IO.Put_Line ("Servidor arrancado");
   Ada.Text_IO.Put_Line ("Para terminar este servidor pulse 'T' o 't'");

   -- Hasta que no pulsen 'T' o 't' en el teclado no termina el servidor
   Finish := False;
   while not Finish loop
      Ada.Text_IO.Get_Immediate (C);
      if C = 'T' or C = 't' then
         Finish := True;
         LLU.Finalize;
      else
         Ada.Text_IO.Put_Line ("Para terminar este servidor pulse 'T' o 't'");
      end if;
   end loop;

exception
   when Ex:others =>
      Ada.Text_IO.Put_Line ("Excepción imprevista: " &
                            Ada.Exceptions.Exception_Name(Ex) & " en: " &
                            Ada.Exceptions.Exception_Message(Ex));
      LLU.Finalize;

end Server;
