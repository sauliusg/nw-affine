with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line; use Ada.Command_Line;

with FASTA; use FASTA;

procedure NW_Align is
   
   Default_Match_Score        : constant Integer := 1;
   Default_Mismatch_Penalty   : constant Integer := -1;
   Default_Gap_Open_Penalty   : constant Integer := -4;
   Default_Gap_Extend_Penalty : constant Integer := -2;
   
   type Direction_Type is (DIR_UNKNOWN, DIR_LEFT, DIR_DIAG, DIR_UP);
   
   type Cell_Type is record
      Score : Long_Integer;
      Direction : Direction_Type;
   end record;
   
   type Cell_Matrix_Type is array
     (Integer range <>, Integer range <>) of Cell_Type;
   
   procedure Put (M : Cell_Matrix_Type) is
   begin
      for I in M'Range(1) loop
         for J in M'Range(2) loop
            if J > 0 then
               Put (ASCII.HT);
            end if;
            Put (M (I,J).Score'Image & ", " & M (I,J).Direction'Image);
         end loop;
         New_Line;
      end loop;
   end;
   
   procedure Align
     (
      Seq1, Seq2 : in Unbounded_String;
      Aln1, Aln2 : out Unbounded_String;
      Score      : out Long_Integer;
      Match_Score        : Integer := Default_Match_Score;
      Mismatch_Penalty   : Integer := Default_Mismatch_Penalty;
      Gap_Open_Penalty   : Integer := Default_Gap_Open_Penalty; 
      Gap_Extend_Penalty : Integer := Default_Gap_Extend_Penalty
     ) is
      
      type Matrix_Type is new
        Cell_Matrix_Type (0 .. Length (Seq1), 0 .. Length (Seq2));
      
      M : Matrix_Type := (others => (others => (0, DIR_UNKNOWN)));
      
   begin
      -- Initialise the matrix:
      M (0, 0) := (0, DIR_UNKNOWN);
      
      -- Initialise the first column:
      for I in M'First(1) + 1 .. M'Last(1) loop
         M (I, 0) :=
           (
            Score => M (I - 1, 0).Score +
              (if I = 1 then Long_Integer (Gap_Open_Penalty) 
                        else Long_Integer (Gap_Extend_Penalty)),
            Direction => DIR_UP
           );
      end loop;
      
      -- Initialise the first row:
      for J in M'First(2) + 1 .. M'Last(2) loop
         M (0, J) :=
           (
            Score => M (0, J - 1).Score +
              (if J = 1 then Long_Integer (Gap_Open_Penalty) 
                        else Long_Integer (Gap_Extend_Penalty)),
            Direction => DIR_UP
           );
      end loop;
      
      -- Dynamic programming will go here:
      
      Put (Cell_Matrix_Type (M)); -- debug print the matrix
      New_Line;
   end;
   
   S1, S2 : Unbounded_String;
   
begin
   
   -- if Argument_Count >= 1 then
   --    Put_Line (To_String (Load_FASTA (Argument (1))));
   -- end if;
   
   S1 := Load_FASTA (Argument (1));
   S2 := Load_FASTA (Argument (2));
   
   declare
      Aln1, Aln2 : Unbounded_String;
      Score : Long_Integer;
   begin
      Align (S1, S2, Aln1, Aln2, Score);
   end;
   
end;
