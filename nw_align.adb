with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure NW_Align is
   
   function Load_FASTA (Input : File_Type) return Unbounded_String is
      Header : String := Get_Line (Input);
   begin
      Put_Line ("Header: " & Header);
      return To_Unbounded_String (Header);
   end;
   
   function Load_FASTA (File_Name : String) return Unbounded_String is
      Input : File_Type;
      Result : Unbounded_String;
   begin
      Open (Input, In_File, File_Name);
      Result := Load_FASTA (Input);
      Close (Input);
      return Result;
   end;
   
begin
   Put_Line ("An N-W aligment will be implemented here");
   Put_Line (To_String (Load_FASTA ("seq1.fasta")));
end;
