with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line; use Ada.Command_Line;

with FASTA; use FASTA;

procedure NW_Align is
   
   INCOMPLETE_MATRIX : exception;
   
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
      for I in M'First(1) + 1 .. M'Last(1) loop
         for J in M'First(2) + 1 .. M'Last(2) loop
            declare
               Score_Up   : Long_Integer := M (I - 1, J).Score;
               Score_Left : Long_Integer := M (I, J - 1).Score;
               Score_Diag : Long_Integer := M (I - 1, J - 1).Score;
               Max_Score : Long_Integer;
            begin
               if Element (Seq1, I) = Element (Seq2, J) then
                  Score_Diag := Score_Diag + Long_Integer (Match_Score);
               else
                  Score_Diag := Score_Diag + Long_Integer (Mismatch_Penalty);
               end if;
               
               if M (I - 1, J).Direction = DIR_DIAG then
                  Score_Up := Score_Up + Long_Integer (Gap_Open_Penalty);
               else
                  Score_Up := Score_Up + Long_Integer (Gap_Extend_Penalty);
               end if;
               
               if M (I, J - 1).Direction = DIR_DIAG then
                  Score_Left := Score_Left + Long_Integer (Gap_Open_Penalty);
               else
                  Score_Left := Score_Left + Long_Integer (Gap_Extend_Penalty);
               end if;
               
               Max_Score := Long_Integer'Max
                 (Long_Integer'Max (Score_Up, Score_Left), Score_Diag);
               
               M (I,J).Score := Max_Score;
               
               if Max_Score = Score_Up then
                  M (I,J).Direction := DIR_UP;
               elsif Max_Score = Score_Left then
                  M (I,J).Direction := DIR_LEFT;
               else
                  M (I,J).Direction := DIR_DIAG;
               end if;
            end;
         end loop;
      end loop;
      
      -- Debug printout of the matrix:
      Put (Cell_Matrix_Type (M)); -- debug print the matrix
      New_Line;
      
      -- Now we know the final aligment score:
      declare
         X : Integer := M'Last(1);
         Y : Integer := M'Last(2);
      begin
         Score := M (X,Y).Score;
      end;
      
      -- Back-tracking and construction of the alignment strings:

      declare
         Rev1 : Unbounded_String := Null_Unbounded_String;
         Rev2 : Unbounded_String := Null_Unbounded_String;
         I  : Integer := M'Last(1);
         J  : Integer := M'Last(2);
      begin
         while I > 0 or J > 0 loop
            Put_Line ("I = " & I'Image & " J = " & J'Image);
            case M (I,J).Direction is
               when DIR_UP =>
                  Append (Rev1, Element (Seq1, I));
                  Append (Rev2, '-');
                  I := I - 1;
               when DIR_LEFT =>
                  Append (Rev1, '-');
                  Append (Rev2, Element (Seq2, J));
                  J := J - 1;
               when DIR_DIAG =>
                  Append (Rev1, Element (Seq1, I));
                  Append (Rev2, Element (Seq2, J));
                  I := I - 1;
                  J := J - 1;
               when DIR_UNKNOWN =>
                  raise INCOMPLETE_MATRIX with
                    "alignment matrix element " & I'Image & ", " & J'Image &
                    "was not computed";
            end case;
         end loop;
         for I in reverse 1 .. Length (Rev1) loop
            Append (Aln1, Element (Rev1, I));
         end loop;
         for J in reverse 1 .. Length (Rev2) loop
            Append (Aln2, Element (Rev2, J));
         end loop;
      end;
      
   end Align;
   
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
      
      function "+" (US : Unbounded_String) return String is
        (To_String (US));
      
   begin
      Align (S1, S2, Aln1, Aln2, Score);
      Put_Line (+Aln1);
      Put_Line (+Aln2);
   end;
   
end;
