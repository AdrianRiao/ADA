with Handlers;
with Lower_Layer_UDP;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Exceptions;

procedure Client is
   package LLU renames Lower_Layer_UDP;
   package ASU renames Ada.Strings.Unbounded;

   Server_EP: LLU.End_Point_Type;
   Client_EP: LLU.End_Point_Type;
   Buffer   : aliased LLU.Buffer_Type(1024);
   Request  : ASU.Unbounded_String;
   Salir    : Boolean;
begin

   Server_EP := LLU.Build("127.0.0.1", 6123);

   -- Construye un End_Point libre cualquiera y se ata a él con un handler
   LLU.Bind_Any (Client_EP, Handlers.Client_Handler'Access);


   -- Bucle que lee cadenas de caracteres y las manda al servidor hasta que
   -- se introduce una cadena de longitud 0.
   -- A la vez que se está ejecutando el blucle se están recibiendo mensajes
   -- en el manejador
   Salir := False;
   while not Salir loop
      LLU.Reset(Buffer);
      LLU.End_Point_Type'Output(Buffer'Access, Client_EP);

      Ada.Text_IO.Put
        ("Mensaje para el servidor (<ENTER> para terminar): ");
      Request := ASU.To_Unbounded_String(Ada.Text_IO.Get_Line);

      if ASU.Length(Request) /= 0 then
         ASU.Unbounded_String'Output(Buffer'Access, Request);
         LLU.Send(Server_EP, Buffer'Access);
      else Salir := True;
      end if;
   end loop;

   LLU.Finalize;

exception
   when Ex:others =>
      Ada.Text_IO.Put_Line ("Excepción imprevista: " &
                            Ada.Exceptions.Exception_Name(Ex) & " en: " &
                            Ada.Exceptions.Exception_Message(Ex));
      LLU.Finalize;
end Client;
