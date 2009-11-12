with Jack.Client;
with Jack.Port;

pragma Elaborate_All (Jack.Client);

package body Simple_Client is

  use type Jack.Client.Client_t;
  use type Jack.Client.Port_t;

  --
  -- Various data declarations.
  --

  Jack_Client  : Jack.Client.Client_t;
  Port_Input   : Jack.Client.Port_t;
  Port_Output  : Jack.Client.Port_t;
  Input_Ports  : Jack.Client.Port_Name_Set_t;
  Output_Ports : Jack.Client.Port_Name_Set_t;

  --
  -- Declarations for a generic 'Process' callback. This callback is called
  -- by the JACK library whenever any processing is required. The original
  -- C version lacked type-safety, but this version remains type-safe through
  -- the use of generics.
  --
  -- User_Data_t can be anything.
  --

  type User_Data_t is new Integer;
  type User_Data_Access_t is access User_Data_t;

  package Callbacks is new Jack.Client.Generic_Callbacks
    (User_Data_Type        => User_Data_t,
     User_Data_Access_Type => User_Data_Access_t);

  --
  -- Process_Callback simply copies an input buffer to an output buffer. The
  -- addresses returned by Jack.Port.Get_Buffer_Address come from the JACK
  -- library itself and likely point to a region of POSIX of SysV shared memory.
  -- The use of address overlays was necessary for efficiency and is entirely
  -- safe in this context.
  --

  procedure Process_Callback
    (Number_Of_Frames : in Jack.Client.Number_Of_Frames_t;
     User_Data        : in User_Data_Access_t)
  is
    type Buffer_t is array (0 .. Number_Of_Frames) of Float;

    Input_Buffer  : Buffer_t;
    Output_Buffer : Buffer_t;

    for Input_Buffer'Address  use Jack.Port.Get_Buffer_Address (Port_Input, Number_Of_Frames);
    for Output_Buffer'Address use Jack.Port.Get_Buffer_Address (Port_Output, Number_Of_Frames);
  begin
    pragma Assert (User_Data.all = 10);

    Output_Buffer := Input_Buffer;
  end Process_Callback;

  Callback_State : constant Callbacks.Process_Callback_State_Access_t :=
    new Callbacks.Process_Callback_State_t'
      (Callback  => Process_Callback'Access,
       User_Data => new User_Data_t'(10));

  --
  -- All JACK setup happens here.
  --

  procedure Init_Jack is
    Status : Jack.Client.Status_t := (others => False);
    Failed : Boolean;
  begin
    --
    -- Open a session to the JACK server. Specify that the server should
    -- NOT be started if it's not already running.
    --

    Jack.Client.Open
      (Client_Name => "simple_client_ada",
       Options     => Jack.Client.Options_t'
        (Jack.Client.Do_Not_Start_Server => True,
         others                          => False),
       Client      => Jack_Client,
       Status      => Status);
    if Jack_Client = Jack.Client.Invalid_Client then
      raise Program_Error with "could not open client";
    end if;

    --
    -- Tell the library to call Process_Callback whenever there is processing
    -- work to be done.
    --

    Callbacks.Set_Process_Callback
      (Client  => Jack_Client,
       State   => Callback_State,
       Failed  => Failed);
    if Failed then
      raise Program_Error with "could not set process callback";
    end if;

    --
    -- Register a single input and output port with the server.
    --

    Jack.Client.Port_Register
      (Client     => Jack_Client,
       Port       => Port_Input,
       Port_Name  => Jack.Client.To_Port_Name ("input"),
       Port_Type  => Jack.Client.Default_Audio_Type,
       Port_Flags => Jack.Client.Port_Flags_t'(Jack.Client.Port_Is_Input => True, others => False));
    if Port_Input = Jack.Client.Invalid_Port then
      raise Program_Error with "could not open input port";
    end if;

    Jack.Client.Port_Register
      (Client     => Jack_Client,
       Port       => Port_Output,
       Port_Name  => Jack.Client.To_Port_Name ("output"),
       Port_Type  => Jack.Client.Default_Audio_Type,
       Port_Flags => Jack.Client.Port_Flags_t'(Jack.Client.Port_Is_Output => True, others => False));
    if Port_Output = Jack.Client.Invalid_Port then
      raise Program_Error with "could not open output port";
    end if;

    --
    -- Inform the server that the client is ready for processing.
    --

    Jack.Client.Activate
      (Client => Jack_Client,
       Failed => Failed);
    if Failed then
      raise Program_Error with "could not activate client";
    end if;

    --
    -- In order to give the client some actual work to do, Get_Ports is used
    -- to locate all physical input and output ports (eg. sound hardware).
    --

    Jack.Client.Get_Ports
      (Client     => Jack_Client,
       Port_Flags => Jack.Client.Port_Flags_t'
        (Jack.Client.Port_Is_Physical => True,
         Jack.Client.Port_Is_Input    => True,
         others                       => False),
       Ports      => Input_Ports);

    Jack.Client.Get_Ports
      (Client     => Jack_Client,
       Port_Flags => Jack.Client.Port_Flags_t'
        (Jack.Client.Port_Is_Physical => True,
         Jack.Client.Port_Is_Output   => True,
         others                       => False),
       Ports      => Output_Ports);

    --
    -- The client now connects it's own input port to the first physical
    -- "output" port it found. An "output" in JACK terminology is something
    -- like a microphone input.
    --

    Jack.Client.Connect
      (Client           => Jack_Client,
       Source_Port      => Jack.Client.Port_Name_Sets.First_Element (Output_Ports),
       Destination_Port => Jack.Port.Name (Port_Input),
       Failed           => Failed);
    if Failed then
      raise Program_Error with "could not connect input port";
    end if;

    --
    -- Connect the client output port to the first available physical "input".
    -- In JACK terminology, an "input" is something like a soundcard output.
    --

    Jack.Client.Connect
      (Client           => Jack_Client,
       Source_Port      => Jack.Port.Name (Port_Output),
       Destination_Port => Jack.Client.Port_Name_Sets.First_Element (Input_Ports),
       Failed           => Failed);
    if Failed then
      raise Program_Error with "could not connect output port";
    end if;

    --
    -- Do 30 seconds of processing.
    --

    delay 30.0;
  end Init_Jack;

  procedure Run is
  begin
    Init_Jack;
  end Run;

end Simple_Client;
