with Ada.Text_IO;
with Ada.Strings.Unbounded;

package body Handlers is

   package ASU renames Ada.Strings.Unbounded;

   procedure Server_Handler (From    : in     LLU.End_Point_Type;
                             To      : in     LLU.End_Point_Type;
                             P_Buffer: access LLU.Buffer_Type) is

      Client_EP: LLU.End_Point_Type;
      Request  : ASU.Unbounded_String;
      Reply    : ASU.Unbounded_String := ASU.To_Unbounded_String ("¡Bienvenido!");
   begin
      -- saca lo recibido en el buffer P_Buffer.all
      Client_EP := LLU.End_Point_Type'Input (P_Buffer);
      Request := ASU.Unbounded_String'Input (P_Buffer);
      Ada.Text_IO.Put ("Petición: ");
      Ada.Text_IO.Put_Line (ASU.To_String(Request));

      -- reinicializa (vacía) el buffer P_Buffer.all
      LLU.Reset (P_Buffer.all);

      -- introduce el Unbounded_String en el Buffer P_Buffer.all
      ASU.Unbounded_String'Output (P_Buffer, Reply);



      -- envía el contenido del Buffer P_Buffer.all
      LLU.Send (Client_EP, P_Buffer);
   end Server_Handler;


   procedure Client_Handler (From    : in     LLU.End_Point_Type;
                             To      : in     LLU.End_Point_Type;
                             P_Buffer: access LLU.Buffer_Type) is
      Reply    : ASU.Unbounded_String;
   begin
      -- saca del Buffer P_Buffer.all un Unbounded_String
      Reply := ASU.Unbounded_String'Input(P_Buffer);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("Respuesta: ");
      Ada.Text_IO.Put_Line(ASU.To_String(Reply));

      Ada.Text_IO.Put
        ("Mensaje para el servidor (<ENTER> para terminar): ");

   end Client_Handler;

end Handlers;

