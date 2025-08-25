---------------------------------------------------------
--  Doppler Effect Tone Audio Playback file with OpenAL and Ada
--  Ada application
--  Playback WAV file with Doppler Effect. Moving Source
--  Tone audio playback by repetition
---------------------------------------------------------
with Ada.Text_IO;           use Ada.Text_IO;
with OpenAL.Context;        use OpenAL.Context;
with Interfaces;            use Interfaces;
with OpenAL.Types;          use OpenAL.Types;
with OpenAL.Buffer;         use OpenAL.Buffer;
with OpenAL.Listener;     --  use OpenAL.Listener;
with OpenAL.Source;       --  use OpenAL.Source;
with OpenAL.Thin;
with Ada.Streams;           use Ada.Streams;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;
--
procedure Cli_Doppler_1 is

   SOUND_FILE_NAME : constant String := "testsound.wav";  --  Tone 1kHz 5sec

   --  WAV file structure
   type WAV_Header is record
      ChunkID       : String (1 .. 4);
      ChunkSize     : Unsigned_32;
      Format        : String (1 .. 4);
      Subchunk1ID   : String (1 .. 4);
      Subchunk1Size : Unsigned_32;
      AudioFormat   : Unsigned_16;
      NumChannels   : Unsigned_16;
      SampleRate    : Unsigned_32;
      ByteRate      : Unsigned_32;
      BlockAlign    : Unsigned_16;
      BitsPerSample : Unsigned_16;
      Subchunk2ID   : String (1 .. 4);  --  data
      Subchunk2Size : Unsigned_32;    --  Nubmer of Data Byte
   end record;

   --  16bit for Audio data
   type Sample_16_t_Array_Access is access all Sample_Array_16_t;
   --  16bit WAV store type
   type WAV_Data_Record is record
      Header : WAV_Header;
      Data   : Sample_16_t_Array_Access;
      Sample_Count : Natural;           -- Number of 16-bit samples
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Sample_Array_16_t, Sample_16_t_Array_Access);
--
--  Read 4 characters from file
   function Read_Chunk_ID
     (BinF : in out Ada.Streams.Stream_IO.File_Type) return String is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 4);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Ada.Streams.Stream_IO.Read (BinF, Buffer, Last);
      return Character'Val (Buffer (1)) &
             Character'Val (Buffer (2)) &
             Character'Val (Buffer (3)) &
             Character'Val (Buffer (4));
   end Read_Chunk_ID;
--
--  Read 4 Bytes as Unsigned_32 (little endian) from File
   function Read_Uint32
     (BinF : in out Ada.Streams.Stream_IO.File_Type) return Unsigned_32 is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 4);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Ada.Streams.Stream_IO.Read (BinF, Buffer, Last);
      return Unsigned_32 (Buffer (1)) +
             Unsigned_32 (Buffer (2)) * 2**8 +
             Unsigned_32 (Buffer (3)) * 2**16 +
             Unsigned_32 (Buffer (4)) * 2**24;
   end Read_Uint32;
--
--  Read 2 Bytes as Unsigned_16 (little endian)
   function Read_Uint16
     (BinF : in out Ada.Streams.Stream_IO.File_Type) return Unsigned_16 is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 2);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Ada.Streams.Stream_IO.Read (BinF, Buffer, Last);
      return Unsigned_16 (Buffer (1)) +
             Unsigned_16 (Buffer (2)) * 2**8;
   end Read_Uint16;
--
--  Helper function to skip (jump over) Bytes
   procedure Skip_Bytes
     (BinF : in out Ada.Streams.Stream_IO.File_Type; Count : Positive) is
      Current_Index : Ada.Streams.Stream_IO.Positive_Count;
   begin
      Current_Index := Ada.Streams.Stream_IO.Index (BinF);
      Ada.Streams.Stream_IO.Set_Index
        (BinF, Current_Index + Ada.Streams.Stream_IO.Positive_Count (Count));
   end Skip_Bytes;
--
--
--
   function Load_WAV_File (File_Name : String) return WAV_Data_Record is
      BinF         : Ada.Streams.Stream_IO.File_Type;
      Header       : WAV_Header;
      Data_Size    : Natural := 0;        -- Data size in bytes
      Sample_Count : Sample_Size_t;        -- Number of 16-bit samples
      Data         : Sample_16_t_Array_Access;
      Result       : WAV_Data_Record;
      Chunk_ID     : String (1 .. 4);
      Chunk_Size   : Unsigned_32;
      Fmt_Found    : Boolean := False;
      Data_Found   : Boolean := False;

   begin
      --  Open file as binary stream
      Ada.Streams.Stream_IO.Open
        (BinF, Ada.Streams.Stream_IO.In_File, File_Name);
      --  Read RIFF header
      Header.ChunkID := Read_Chunk_ID (BinF); --  Read 4 char "RIFF"
      Header.ChunkSize := Read_Uint32 (BinF); --  Read 4 Bytes Unsigned_32
      Header.Format := Read_Chunk_ID (BinF);  --  "WAVE"

      --  Verify this is a WAV file
      if Header.ChunkID /= "RIFF" or else Header.Format /= "WAVE" then
         Put_Line ("Error: Not a valid WAV file");
         Ada.Streams.Stream_IO.Close (BinF);
         Result.Header := Header;
         Result.Data := null;
         Result.Sample_Count := 0;
         return Result;
      end if;
--
      --  Parse chunks until we find fmt and data
      while
         not (Fmt_Found and Data_Found) and then
         not Ada.Streams.Stream_IO.End_Of_File (BinF) loop
         --
         Chunk_ID := Read_Chunk_ID (BinF);
         Chunk_Size := Read_Uint32 (BinF);

         if Chunk_ID = "fmt " then
         --  Read format chunk
            Header.Subchunk1ID := Chunk_ID;
            Header.Subchunk1Size := Chunk_Size;
            Header.AudioFormat := Read_Uint16 (BinF);
            Header.NumChannels := Read_Uint16 (BinF);
            Header.SampleRate := Read_Uint32 (BinF);
            Header.ByteRate := Read_Uint32 (BinF);
            Header.BlockAlign := Read_Uint16 (BinF);
            Header.BitsPerSample := Read_Uint16 (BinF);

            --  Verify this is 16-bit audio
            if Header.BitsPerSample /= 16 then
               Put_Line ("Error: Only 16-bit audio is supported, found " &
                        Unsigned_16'Image (Header.BitsPerSample) & "-bit");
               Ada.Streams.Stream_IO.Close (BinF);
               Result.Header := Header;
               Result.Data := null;
               Result.Sample_Count := 0;
               return Result;
            end if;

         --  Skip any remaining bytes in the fmt chunk
            if Chunk_Size > 16 then
               Skip_Bytes (BinF, Natural (Chunk_Size - 16));
            end if;

            Fmt_Found := True;

         elsif Chunk_ID = "data" then
         --  Found data chunk
            Header.Subchunk2ID := Chunk_ID;
            Header.Subchunk2Size := Chunk_Size;
            Data_Size := Natural (Chunk_Size);

            --  Calculate number of 16-bit samples
            Sample_Count := Sample_Size_t (Data_Size / 2);

            --  allocated memory dynamically
            Data := new Sample_Array_16_t (1 .. Sample_Size_t (Data_Size / 2));

--  Read audio data
--  Treat Data array directly as Stream_Element_Array (overlay technique)
--  This overlays the same memory area with different types.
--  Safe because Byte and Stream_Element are the same size (8 bits).
            declare
               Data_Stream_View : Stream_Element_Array
                 (1 .. Stream_Element_Offset (Data_Size));
               for Data_Stream_View'Address use Data.all'Address;
               pragma Import (Ada, Data_Stream_View);
               Last : Stream_Element_Offset;
            begin
--  Note: WAV files use little-endian format
--  If your system is big-endian, you would need to swap bytes here
--  For most modern systems (x86, x64), no conversion is needed
               Stream_IO.Read (BinF, Data_Stream_View, Last);
            end;

            Data_Found := True;
         else
         --  Skip unknown chunks (like JUNK, LIST, etc.)
            Put_Line ("Skipping chunk: " & Chunk_ID & " (size:" &
                     Unsigned_32'Image (Chunk_Size) & ")");
            Skip_Bytes (BinF, Natural (Chunk_Size));

         --  Handle odd-sized chunks (WAV chunks must be word-aligned)
            if Chunk_Size mod 2 = 1 then
               Skip_Bytes (BinF, 1);
            end if;
         end if;
      end loop;  --  End of While loop

      --  Close file
      Ada.Streams.Stream_IO.Close (BinF);

      --  Check if we found both required chunks
      if not Fmt_Found then
         Put_Line ("Error: fmt chunk not found");
         Free (Data);
         Data := null;
      end if;

      if not Data_Found then
         Put_Line ("Error: data chunk not found");
         if Data /= null then
            Free (Data);
         end if;
--  ****         Sample_Count := 0;
         Data_Size := 0;
      end if;

      --  Print header information
      Put_Line ("ChunkID=        " & Header.ChunkID);
      Put_Line ("ChunkSize=      " & Unsigned_32'Image (Header.ChunkSize));
      Put_Line ("Format=         " & Header.Format);
      Put_Line ("Subchunk1ID=    " & Header.Subchunk1ID);
      Put_Line ("Subchunk1Size=  " & Unsigned_32'Image (Header.Subchunk1Size));
      Put_Line ("Audio Format=   " & Unsigned_16'Image (Header.AudioFormat));
      Put_Line ("NumChannels=    " & Unsigned_16'Image (Header.NumChannels));
      Put_Line ("SampleRate=     " & Unsigned_32'Image (Header.SampleRate));
      Put_Line ("ByteRate=       " & Unsigned_32'Image (Header.ByteRate));
      Put_Line ("BlockAlign=     " & Unsigned_16'Image (Header.BlockAlign));
      Put_Line ("BitsPerSample=  " & Unsigned_16'Image (Header.BitsPerSample));
      Put_Line ("Subchunk2ID=    " & Header.Subchunk2ID);
      Put_Line ("Subchunk2Size=  " & Unsigned_32'Image (Header.Subchunk2Size));
      Put_Line ("Sample Count=   " & Sample_Size_t'Image (Sample_Count));

      --  Return result
      Result.Header := Header;
      Result.Data := Data;
      Result.Sample_Count := Integer (Sample_Count);
      return Result;
   end Load_WAV_File;
--
--
-------------------------------------
--  GLOBAL VARIABLES
-------------------------------------
--
   WAV_File : WAV_Data_Record;  -- Dynamic Allocation
   WAV_Byte_Length : Integer;

   Device : OpenAL.Context.Device_t;
   Context : OpenAL.Context.Context_t;
--   Buffer : Buffer_t;
--   Source : Source_t;
   Set_Active_Context : Boolean;
--   Buffers : Buffer_Array_t (1 .. 1);
--   Sources : Source_Array_t (1 .. 1);
   EndFlag : Integer := 0;
   ProcessedNr : Natural := 0;
--
--  Location X, Y, Z    3 Dimensions
   Listener_Set_Position : constant OpenAL.Types.Vector_3f_t :=
     (0.0, 0.0, 0.0);
   Listener_Set_Velocity : constant OpenAL.Types.Vector_3f_t :=
     (0.0, 0.0, 0.0);
   Listener_Orientation_Forward : constant OpenAL.Types.Vector_3f_t :=
     (0.0, 1.0, 0.0);
   Listener_Orientation_Up : constant OpenAL.Types.Vector_3f_t :=
     (0.0, 0.0, 1.0);
   Source_Set_Position : OpenAL.Types.Vector_3f_t := (0.0, 1.0, 0.0);
   Source_Set_Velocity : OpenAL.Types.Vector_3f_t := (0.0, 0.0, 0.0);
   Source_Set_Direction : constant OpenAL.Types.Vector_3f_t := (0.0, 1.0, 0.0);
   Sound_Source : OpenAL.Source.Source_t;
   Sound_Source_Array : OpenAL.Source.Source_Array_t (1 .. 1);

   Buffer_Arrt : OpenAL.Buffer.Buffer_Array_t (1 .. 2000000); --  WAV MAX Word

   TimeCnt : Integer;   --  Playback Time/Location/Speed Counter
   SetPosF, SetVelF : OpenAL.Types.Float_t;   --  Vector Calculation
--
--
-------------------------------------
-------------------------------------
--  MAIN
-------------------------------------
-------------------------------------
begin

   Put_Line ("Doppler Effect Demo");
   -------------------------
   --  Open WAV File
   -------------------------
   WAV_File := Load_WAV_File (SOUND_FILE_NAME);
   if WAV_File.Data = null then
      Put_Line ("WAV File load error");
      return;
   end if;
   Put_Line ("Load WAV File Success");

   ---------------------------
   --  Open Default Playback Device to monitor
   --  "OpenAL_Soft" is Default. Pulse Audio to choose more
   ---------------------------
   Device := OpenAL.Context.Open_Default_Device;
   if Device = Invalid_Device then
      Put_Line ("Error: Failed to open OpenAL device");
      return;
   end if;
--  Context works for 3D, Listner and Environment settings
   Context := Create_Context (Device);
   if Context = Null_Context then
      Put_Line ("Error: Failed to create OpenAL context");
      Close_Device (Device);
      return;
   end if;
--  Choose the working Context
   Set_Active_Context := Make_Context_Current (Context);
   if Set_Active_Context = False then
      Put_Line ("Error: Failed, cannot Set Active Context");
      Close_Device (Device);
      Destroy_Context (Context);
      return;
   end if;
   Put_Line ("Default Playback Device Opened");

----------------------------------------
--// Generate Buffers
--     C:  alGenBuffers(NUM_BUFFERS, g_Buffers);
--    Ada:  procedure Generate_Buffers (Buffers : in out Buffer_Array_t);
--    The Generate_Buffers procedure generates Buffers'Length buffers.
----------------------------------------
   WAV_Byte_Length := Integer (WAV_File.Header.Subchunk2Size);
   OpenAL.Thin.Gen_Buffers
     (Size => OpenAL.Types.Size_t (WAV_Byte_Length / 2), -- Words
      Buffers => Buffer_Arrt (1)'Address);

   Put_Line ("Generated Buffer, WAV_Length=" &
               Integer'Image (WAV_Byte_Length) & " Byte");
--
--
------------
--  This was not in the manual, it is in OpenAL.Thin,
--  It can set Size freely, so it is good to define maximum WAV buffer
--  Procedure Buffer_Data
--    (Buffer_ID : Types.Unsigned_Integer_t;
--     Format    : Types.Enumeration_t;
--     Data      : System.Address;
--     Size      : Types.Size_t;
--     Frequency : Types.Size_t);
--   pragma Import (C, Buffer_Data, "alBufferData");
---------------------------------------------------------------------
   OpenAL.Thin.Buffer_Data (
     Buffer_ID => 1,                    --  Types.Unsigned_Integer_t;
     Format    => OpenAL.Thin.AL_FORMAT_MONO16,       --  Types.Enumeration_t;
     Data      => WAV_File.Data (1)'Address,          --  system.Address;
     Size      => OpenAL.Types.Size_t (WAV_Byte_Length),   --  Types.Size_t;
     Frequency => OpenAL.Types.Size_t (WAV_File.Header.SampleRate) --  Size_t);
   );
--
--
--------------------------------------------------------------------
--  Generate Sources
--  alGenSources((ALuint)1, &source); //generates one or more sources,n=number
--  If error: alDeleteBuffers(NUM_BUFFERS, g_Buffers) ????
--  This was not written in the sample program
--------------------------------------------------------------------
   OpenAL.Source.Generate_Sources (Sound_Source_Array);
   Sound_Source := Sound_Source_Array (1);

   if OpenAL.Source.Is_Valid (Sound_Source) = True then
      Put_Line ("Source Is_Vlaid=True");
   else
      Close_Device (Device);
      Destroy_Context (Context);
      Put_Line ("Source Is_Valid=False");
   end if;
--
--
-----------------------------------------------------------------------
--  // Attach buffer 0 to source
--  C: alSourcei(source[0], AL_BUFFER, g_Buffers[0]);
--  if ((error = alGetError()) != AL_NO_ERROR) {
--  DisplayALError("alSourcei AL_BUFFER 0 : ", error); }
--  procedure Set_Current_Buffer
--  (Source : in Source_t;
--  Buffer : in OpenAL.Buffer.Buffer_t);
----------------------------------------------------------------------
   OpenAL.Source.Set_Current_Buffer
     (Sound_Source,
      Buffer_Arrt (1));

--------------------------------------------------------------
--  Set_Looping
--  It can repeat the source sound
--  The second parameter is "Looping", should be True or False
--  It is True when repeating
--------------------------------------------------------------
   OpenAL.Source.Set_Looping
     (Source => Sound_Source,
      Looping => True);
   Put_Line ("Source is set looping");
--
--
--
--------------------------------------------------------------------------
--  Source
--  Source_Set_Position : OpenAL.Types.Vector_3f_t := (0.0, 0.0, 0.0);
--  procedure Set_Position_Float_List
--  (Source    : in Source_t;
--   Position  : in Types.Vector_3f_t);
---------------------------------------------------------------------------
   SetPosF := -20.0;
   Source_Set_Position := (SetPosF, 1.0, 0.0);
   OpenAL.Source.Set_Position_Float_List (Sound_Source, Source_Set_Position);

   SetVelF := 100.0;
   Source_Set_Velocity := (SetVelF, 0.0, 0.0);
   OpenAL.Source.Set_Velocity_Float_List (Sound_Source, Source_Set_Velocity);

   OpenAL.Source.Set_Direction_Float_List (Sound_Source, Source_Set_Direction);
--

---------------------------------------------------------------------
--  LISTENER
--  alListener3f(AL_POSITION, 0, 0, 1.0f);
--  alListener3f(AL_VELOCITY, 0, 0, 0);
--  alListenerfv(AL_ORIENTATION, listenerOri);
--  Set example:  Listener_Set_Position(1) := 2.0;   1=X,2=Y,3=Z
----------------------------------------------------------------------
   OpenAL.Listener.Set_Position_Float_List (Listener_Set_Position);

   OpenAL.Listener.Set_Velocity_Float_List (Listener_Set_Velocity);

   OpenAL.Listener.Set_Orientation_Float
     (Listener_Orientation_Forward, Listener_Orientation_Up);
--
--
--------------------------------------------------------------------
--  Play procedure sets the source specified by Source to the playing state.
--  with OpenAL.Source;
--  procedure Play (Source : in Source_t);
--------------------------------------------------------------------
   OpenAL.Source.Play (Sound_Source);
   Put_Line ("Play(Sound_Source)");

-------------------------------------------------------------------
--  NOTE: WAITING LOOP
--  Get number of processed buffers.     NOT GOOD FOR TEST END
--  procedure Get_Buffers_Processed
-------------------------------------------------------------------

   delay 0.5;
   TimeCnt := 0;
   EndFlag := 0;

   for J in Integer range 1 .. 1000 loop --  10 sec / 10ms
      OpenAL.Thin.Get_Sourcei
        (Source_ID => OpenAL.Types.Unsigned_Integer_t (1),
         Parameter => OpenAL.Thin.AL_SOURCE_STATE,
         Value => EndFlag'Address);

      exit when EndFlag /= OpenAL.Thin.AL_PLAYING;

      delay 0.01;
      TimeCnt := TimeCnt + 1;
------------------------------
--   alSource3f(source, AL_POSITION, (nnn-1500)/300, 0, 0);
--   alSource3f(source, AL_VELOCITY, 0, 0, (nnn-2500.0)/20.0);
--   procedure Set_Position_Float
--      (Source    : in Source_t;  X: in Types.Float_t;
--                 Y: in Types.Float_t; Z: in Types.Float_t);
--   Source_Set_Position : OpenAL.Types.Vector_3f_t := (0.0, 1.0, 0.0);
--   Source_Set_Velocity : OpenAL.Types.Vector_3f_t := (0.0, 0.0, 0.0);
------------------------------
--  Set Position from left to right
      SetPosF := OpenAL.Types.Float_t (Float (TimeCnt) * 0.04 - 20.0);
      OpenAL.Source.Set_Position_Float (Sound_Source, SetPosF, 1.0, 0.0);
--
   end loop;
--
--
--  Clean up resources
   if WAV_File.Data /= null then
      Free (WAV_File.Data);
   end if;
   OpenAL.Source.Get_Buffers_Queued (Source => Sound_Source,
                                  Buffers => ProcessedNr);  --  Natural
   if ProcessedNr /= 0 then
      OpenAL.Source.Unqueue_Buffers (Source => Sound_Source,
                                  Buffers => Buffer_Arrt); --  Buffer_Array_t
   end if;
   OpenAL.Buffer.Delete_Buffers (Buffer_Arrt);
   OpenAL.Context.Close_Device (Device);    --  "OpenAL Soft")
   OpenAL.Context.Destroy_Context (Context);
   OpenAL.Source.Delete_Sources (Sound_Source_Array);

end Cli_Doppler_1;
