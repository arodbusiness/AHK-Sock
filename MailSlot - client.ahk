;Mailslot Client - the client for a mailslot is the process that SENDS to a mailslot that has been created
;by a mailslot server process
;Error checking pretty minimal - see Mailslot server script for better checking & more details of 
;Windows error messages

;Sending to a mailslot is essentially the same as writing to a file

;SlotName, below, must exactly match the mailslot name created by the server EXCEPT
;if the client is running on a different computer the dot must be replaced by the name
;of the SERVER script's computer e.g. \\LabComputer1\mailslot\TestSlot1
;A 'star' * can be used in place of computer name to broadcast to all computers on domain.

SlotName = \\.\mailslot\TestSlot1

mailslotHandle := DllCall("CreateFile"
	,Str , SlotName		;of form \\HostName\pipe\PipeName
	,UInt, 0x40000000	;Generic_Write access
	,UInt, 3		;0 = no sharing; 3 = read & write sharing
	,UInt, 0		;Security Attributes (a pointer; 0 here actually means NULL)
	,UInt, 3		;Creation Disposition (3 = OPEN_EXISTING)
	,UInt, 0		;Flags And Attributes
	,UInt, 0)		;Template File (no template used; 0 means NULL; ignored for existing file, in any case)

if errorlevel
	MsgBox An error occured in AHK call to DLL CreateFile
else
	If (a_lasterror <> 0) ;error from DLL itself - see Server script for more info on errors
	{
		MsgBox	Last error: %a_lasterror%
		exitapp
	}


;This script just sends a couple of messages, then exits

message = This is a test messasge sent to a mailslot. The last character is a star. *
SendToMailslot(mailslothandle, message)

message =
(
This is
a multiline
message
that was sent
without waiting
for the first one
to be read.
)
SendToMailslot(mailslothandle, message)

return

SendToMailslot(slotHandle, StringToWrite)
{
	DllCall("WriteFile"
	, uint, slotHandle
	, str, StringToWrite
	, uint, StrLen(StringToWrite)+1  ;+1 so terminating NULL is included
	, uintp,0, uint,0)

	If errorlevel
    		MsgBox WriteFile failed.`nError level: %ErrorLevel%`nLast error: %A_LastError%
}