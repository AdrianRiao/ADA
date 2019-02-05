with Ada.Text_IO;

package body Lists is
   procedure Add (List   : in out Cell_A;
                  A_Name : in     ASU.Unbounded_String;
                  A_Count: in Natural) is
      P_Aux : Cell_A;
   begin
      P_Aux := new Cell;
      P_Aux.all.Name  := A_Name;
      P_Aux.all.Count := A_Count;
      P_Aux.all.Next  := List;
      List := P_Aux;
   end Add;

   procedure Print_All (List: in Cell_A) is
      P_Aux : Cell_A;
   begin
      P_Aux := List;
      while P_Aux /= null loop
         Ada.Text_IO.Put_Line (ASU.To_String(P_Aux.Name) &
                                 ": " &
                                 P_Aux.Count'Img);
         P_Aux := P_Aux.all.Next;
      end loop;
   end Print_All;
end Lists;
