with Ada.Strings.Unbounded;

package Lists is

   package ASU renames Ada.Strings.Unbounded;

   type Cell_A is private;

   -- Interfaz
   procedure Add (List   : in out Cell_A;
                  A_Name : in     ASU.Unbounded_String;
                  A_Count: in Natural);

   procedure Print_All (List: in Cell_A);


private

   type Cell;
   type Cell_A is access Cell;

   type Cell is record
      Name : ASU.Unbounded_String;
      Count: Natural := 0;
      Next : Cell_A;
   end record;
end Lists;

