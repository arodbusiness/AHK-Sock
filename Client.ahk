	#NoEnv
    ;We'll need to allow more than one instance to test it on the same machine
    #SingleInstance Force
    
    ;Needed if AHKsock isn't in one of your lib folders
    #Include %A_ScriptDir%\AHKsock.ahk
    
    ;Set up an error handler (this is optional)
    AHKsock_ErrorHandler("AHKsockErrors")
    
    ;Set up an OnExit routine
    OnExit, GuiClose
    
    ;Set default value to invalid handle
    iPeerSocket := -1
    
    ;Set up the GUI
    Gui, +Resize +OwnDialogs
    Gui, Add, Edit, r10 w300 vtxtDialog ReadOnly hwndhtxtDialog
    

    Gui, Add, Edit, xm w250 vtxtInput hwndhtxtInput ReadOnly gtxtInput Limit65535
    Gui, Add, Button, x+5 w45 hp vbtnSend hwndhbtnSend Default Disabled gbtnSend, Send
    Gui, Add, Text, xm w300 vlblStatus hwndhlblStatus, Not connected...
    Gui, +MinSize
    Gui, Show
    
	Port := 23
	
	
    ;Ask for peer address
    InputBox, sName, Peer hostname or IP address, Please enter your peer's hostname or IP address:,,, 100,,,,, 107.77.237.128
    If ErrorLevel
        ExitApp
    
    ;Try to connect
    GuiControl,, lblStatus, Trying to connect to %sName%... ;Update status
    If (i := AHKsock_Connect(sName, Port, "Peer"))
        OutputDebug, % "AHKsock_Connect() failed with return value = " i " and ErrorLevel = " ErrorLevel
    

Return

GuiSize:
    Anchor(htxtDialog, "wh")
    Anchor(htxtInput, "wy")
    Anchor(hbtnSend, "xy")
    Anchor(hlblStatus, "wy", 1)
Return

GuiEscape:
GuiClose:
    
    ;So that we don't go back to listening on disconnect
    bExiting := True
    
    /*! If the GUI is closed, this function will be called twice:
        - Once non-critically when the GUI event fires (GuiEscape or GuiClose) (graceful shutdown will occur), and
        - Once more critically during the OnExit sub (after the previous GUI event calls ExitApp)
        
        But if the application is exited using the Exit item in the tray's menu, graceful shutdown will be impossible
        because AHKsock_Close() will only be called once critically during the OnExit sub.
    */
    AHKsock_Close()
ExitApp

txtInput:
    
    /*! This GUI event will be called everytime the text in the Edit control changes. The idea is to send a notification to
    the peer when the user starts/stops typing. We do this using the typing update frame. See the StreamProcessor function
    for more information on frames.
    */
    
    ;Check if we're connected
    If (iPeerSocket = -1)
        Return
    
    ;Get text entered, if any
    GuiControlGet, sText,, txtInput
    
    ;Create a boolean value
    bTypingUpdate := sText <> ""
    
    ;Send a typing update frame only if the boolean value is different from the last one we sent. We do this check so that
    ;we don't drown the peer with updates everytime the user types an additional character in the Edit control box.
    If (bTypingUpdate = bLastTypingUpdate)
        Return
    
    /*! It is more appropriate to use AHKsock_ForceSend here because the thread is not critical, and because the amount of
    data to send is very small. We thus don't have to worry about the function taking time to return.
    */
    
    ;Send a typing update frame: high-word is 1, low-word is a boolean value (False = 0, True = 1)
    VarSetCapacity(iFrame, 4, 0), NumPut((1 << 16) + bTypingUpdate, iFrame, 0, "UInt")
    If (i := AHKsock_ForceSend(iPeerSocket, &iFrame, 4)) {
        OutputDebug, % "AHKsock_ForceSend failed with return value = " i " and error code = " ErrorLevel " at line " A_LineNumber
        ExitApp
    }
    
    ;Remember the boolean value of the typing update frame we just sent
    bLastTypingUpdate := sText ? 1 : 0
    
Return

btnSend:
    
    /*! This GUI event will be called everytime the user clicks on the Send button, or when he presses the Enter key after
    typing text in the Edit control. The logic is rather straight-forward. We first send a string length frame to tell the
    peer how long the string coming will be, and we then send the string. See the StreamProcessor function for more
    information on frames.
    */
    
    ;Check if we're connected
    If (iPeerSocket = -1)
        Return
    
    ;Get the text to send
    GuiControlGet, sText,, txtInput
    
    ;Make sure we even have something to send
    If Not sText
        Return
    
    ;Get text length
    iTextLength := StrLen(sText) * 2
    
    ;First send a string length frame: high-word is 2, low-word is the length of the string
    VarSetCapacity(iFrame, 4, 0), NumPut((2 << 16) + iTextLength, iFrame, 0, "UInt")
    If (i := AHKsock_ForceSend(iPeerSocket, &iFrame, 4)) {
        OutputDebug, % "AHKsock_ForceSend failed with return value = " i " and error code = " ErrorLevel " at line " A_LineNumber
        ExitApp
    }
    
    ;Send the actual string now, excluding the null terminator
    If (i := AHKsock_ForceSend(iPeerSocket, &sText, iTextLength)) {
        OutputDebug, % "AHKsock_ForceSend failed with return value = " i " and error code = " ErrorLevel " at line " A_LineNumber
        ExitApp
    }
    
    ;Data was sent. Add it to the dialog.
    AddDialog(&sText)
    
    ;Clear the Edit control and give focus
    GuiControl,, txtInput
    GuiControl, Focus, txtInput
    
Return

AddDialog(ptrText, bYou = True) {
    Global htxtDialog
    
    ;Append the interlocutor
    sAppend := bYou ? "You > " : "Peer > "
    InsertText(htxtDialog, &sAppend)
    
    ;Append the new text
    InsertText(htxtDialog, ptrText)
    
    ;Append a new line
    sAppend := "`r`n"
    InsertText(htxtDialog, &sAppend)
    
    ;Scroll to bottom
    SendMessage, 0x0115, 7, 0,, ahk_id %htxtDialog% ;WM_VSCROLL
}

Peer(sEvent, iSocket = 0, sName = 0, sAddr = 0, sPort = 0, ByRef bData = 0, bDataLength = 0) {
    Global iPeerSocket, bExiting
    
    If (sEvent = "CONNECTED") {
        ;Remember the socket
        iPeerSocket := iSocket
        
        ;Allow input and set focus
        GuiControl, Enable, btnSend
        GuiControl, -ReadOnly, txtInput
        GuiControl, Focus, txtInput
        
        ;Update status
        GuiControl,, lblStatus, Connected to %sName%!
        
    } Else If (sEvent = "RECEIVED") {
        
        ;OutputDebug, % "Received " bDataLength " bytes" ;FOR DEBUGGING PURPOSES ONLY
        ;OutputDebug, % "bData = " MCode_Bin2Hex(&bData, bDataLength) ;FOR DEBUGGING PURPOSES ONLY
        
        ;Send to the stream processor
        StreamProcessor(bData, bDataLength)
    }
}

StreamProcessor(ByRef bNewData = 0, bNewDataLength = -1) {
    Static bPastData, bPastDataLength := 0
    
    ;Check if we're just erasing any previous data we had
    If (bNewDataLength = -1) {
        If bPastDataLength { ;Check if there's data to erase
            VarSetCapacity(bPastData, 0)
            bPastDataLength := 0
        }
        
        ;OutputDebug, % "SP - Deleting any previous data" ;FOR DEBUGGING PURPOSES ONLY
        Return
    }
    
    ;Check if we have any past data to prepend before entering the processing loop
    If bPastDataLength {
        
        ;OutputDebug, % "SP - Prepending previous data of " bPastDataLength " bytes" ;FOR DEBUGGING PURPOSES ONLY
        
        ;The length of the data to process will be the length of both old and new data combined
        bDataLength := bPastDataLength + bNewDataLength
        
        ;Prep the variable which will hold both the previous data and the new data
        VarSetCapacity(bData, bDataLength)
        
        ;Copy the old data into the beginning of bData, and then the new data right after
        CopyBinData(&bPastData, &bData, bPastDataLength)
        CopyBinData(&bNewData, &bData + bPastDataLength, bNewDataLength)
        
        ;We can now delete any old data we had because we're about to process it
        VarSetCapacity(bPastData, 0) ;Clear the variable to free some memory since it won't be used
        bPastDataLength := 0 ;Reset the value
        
        ;Now bData contains the newly received data as well as any previous data that could not be fully processed before
        
        ;Set the data pointer to the new data we just created
        bDataPointer := &bData
        
        /*! The advantage of using a data pointer is so that the code that follows after can work regardless of whether
        the data to process is in bNewData (if we had nothing to prepend), or in bData (if we had to create it to
        prepend some past data). The variable bDataLength holds the length of the data to which bDataPointer points.
        */        
        
    ;Set the data pointer to the newly arrived data
    } Else bDataPointer := &bNewData, bDataLength := bNewDataLength
    
    ;Now bDataPointer points to the data to process, whether it be only the new data that arrived, or old and new data which
    ;have been concatenated. The length of the data to which bDataPointer points is in bDataLength.
    
    ;Check if we have at least one frame to process
    If (bDataLength < 4) {
        
        ;OutputDebug, % "SP - Less than a frame to process. Saving " bDataLength " bytes and leaving" ;FOR DEBUGGING PURPOSES ONLY
        
        ;We only have part of one frame. Keep what we have and leave.
        VarSetCapacity(bPastData, bPastDataLength := bDataLength)
        CopyBinData(bDataPointer, &bPastData, bDataLength)
        Return
    }
    
    ;Start processing the stream of frames
    iOffset := 0
    While (iOffset + 4 <= bDataLength) {
        
        ;Get frame type and data
        iFrame     := NumGet(bDataPointer+0, iOffset, "UInt")
        iFrameType := iFrame >> 16 ;High-word
        iFrameData := iFrame & 0xFFFF ;Low-word        
        
        /*! The type of the frame is in the high-word, and the data of the frame is in the low-word:
            - Typing update frames have a high-word of 1, and a boolean value in the low-word indicating typing status
            - String length frames have a high-word of 2, and the length of the string that follows in the low-word
        */
        
        ;Check what kind of frame it is
        If (iFrameType = 1) { ;It's a typing update frame
            GuiControl,, lblStatus, % iFrameData ? "Peer is typing..." : "Connected!" ;Update
            iOffset += 4 ;Increase the offset to be at the beginning of the next frame
        } Else If (iFrameType = 2) { ;It's a string length frame
            
            ;Check if the whole string is present
            ;We do + 4 because the string starts right after the frame we're currently processing
            If Not (iFrameData <= bDataLength - (iOffset + 4)) {
                
                ;OutputDebug, % "SP - Part of string missing. Saving " (bDataLength - iOffset) " bytes and leaving" ;FOR DEBUGGING PURPOSES ONLY
                
                ;We only have part of a string. Keep what we have (including the string length frame) and leave.
                ;We include the string length frame so that this loop will be able to handle it the next time
                ;it processes the same data but with more of the string at the end
                VarSetCapacity(bPastData, bPastDataLength := bDataLength - iOffset)
                CopyBinData(bDataPointer + iOffset, &bPastData, bDataLength - iOffset)
                Return
            }
            
            ;Increase the offset to where the string starts (right after the frame we're processing)
            iOffset += 4
            
            ;Get the string and add to dialog
            VarSetCapacity(sTextData, iFrameData + 2, 0)
            CopyBinData(bDataPointer + iOffset, &sTextData, iFrameData)
            
            ;Add the string to the dialog textbox
            AddDialog(&sTextData, False)
            
            ;Move the offset by the length of the string to be at the beginning of the next frame
            iOffset += iFrameData
        }
    }

    ;Check if there are bytes part of an incomplete frame at the end of the stream that could not be processed
    If (n := (bDataLength - iOffset)) And (n < 4) {
        
        ;OutputDebug, % "SP - Less than a frame left at the end of the stream. Saving " n " bytes and leaving" ;FOR DEBUGGING PURPOSES ONLY
        
        ;We only have part of one frame. Keep what we have and leave.
        VarSetCapacity(bPastData, bPastDataLength := n)
        CopyBinData(bDataPointer + iOffset, &bPastData, n)
        Return
    }
}

AHKsockErrors(iError, iSocket) {
    OutputDebug, % "Error " iError " with error code = " ErrorLevel ((iSocket <> -1) ? " on socket " iSocket "." : ".") 
}

CopyBinData(ptrSource, ptrDestination, iLength) {
    If iLength ;Only do it if there's anything to copy
        DllCall("RtlMoveMemory", "Ptr", ptrDestination, "Ptr", ptrSource, "UInt", iLength)
}


InsertText(hEdit, ptrText, iPos = -1) {
    
    If (iPos = -1) {
        SendMessage, 0x000E, 0, 0,, ahk_id %hEdit% ;WM_GETTEXTLENGTH
        iPos := ErrorLevel
    }
    
    SendMessage, 0x00B1, iPos, iPos,, ahk_id %hEdit% ;EM_SETSEL
    SendMessage, 0x00C2, False, ptrText,, ahk_id %hEdit% ;EM_REPLACESEL
}

;Anchor by Titan, adapted by TheGood
;http://www.autohotkey.com/forum/viewtopic.php?p=377395#377395
Anchor(i, a = "", r = false) {
	static c, cs = 12, cx = 255, cl = 0, g, gs = 8, gl = 0, gpi, gw, gh, z = 0, k = 0xffff, ptr
	If z = 0
		VarSetCapacity(g, gs * 99, 0), VarSetCapacity(c, cs * cx, 0), ptr := A_PtrSize ? "Ptr" : "UInt", z := true
	If (!WinExist("ahk_id" . i)) {
		GuiControlGet, t, Hwnd, %i%
		If ErrorLevel = 0
			i := t
		Else ControlGet, i, Hwnd, , %i%
	}
	VarSetCapacity(gi, 68, 0), DllCall("GetWindowInfo", "UInt", gp := DllCall("GetParent", "UInt", i), ptr, &gi)
		, giw := NumGet(gi, 28, "Int") - NumGet(gi, 20, "Int"), gih := NumGet(gi, 32, "Int") - NumGet(gi, 24, "Int")
	If (gp != gpi) {
		gpi := gp
		Loop, %gl%
			If (NumGet(g, cb := gs * (A_Index - 1)) == gp, "UInt") {
				gw := NumGet(g, cb + 4, "Short"), gh := NumGet(g, cb + 6, "Short"), gf := 1
				Break
			}
		If (!gf)
			NumPut(gp, g, gl, "UInt"), NumPut(gw := giw, g, gl + 4, "Short"), NumPut(gh := gih, g, gl + 6, "Short"), gl += gs
	}
	ControlGetPos, dx, dy, dw, dh, , ahk_id %i%
	Loop, %cl%
		If (NumGet(c, cb := cs * (A_Index - 1), "UInt") == i) {
			If a =
			{
				cf = 1
				Break
			}
			giw -= gw, gih -= gh, as := 1, dx := NumGet(c, cb + 4, "Short"), dy := NumGet(c, cb + 6, "Short")
				, cw := dw, dw := NumGet(c, cb + 8, "Short"), ch := dh, dh := NumGet(c, cb + 10, "Short")
			Loop, Parse, a, xywh
				If A_Index > 1
					av := SubStr(a, as, 1), as += 1 + StrLen(A_LoopField)
						, d%av% += (InStr("yh", av) ? gih : giw) * (A_LoopField + 0 ? A_LoopField : 1)
			DllCall("SetWindowPos", "UInt", i, "UInt", 0, "Int", dx, "Int", dy
				, "Int", InStr(a, "w") ? dw : cw, "Int", InStr(a, "h") ? dh : ch, "Int", 4)
			If r != 0
				DllCall("RedrawWindow", "UInt", i, "UInt", 0, "UInt", 0, "UInt", 0x0101) ; RDW_UPDATENOW | RDW_INVALIDATE
			Return
		}
	If cf != 1
		cb := cl, cl += cs
	bx := NumGet(gi, 48, "UInt"), by := NumGet(gi, 16, "Int") - NumGet(gi, 8, "Int") - gih - NumGet(gi, 52, "UInt")
	If cf = 1
		dw -= giw - gw, dh -= gih - gh
	NumPut(i, c, cb, "UInt"), NumPut(dx - bx, c, cb + 4, "Short"), NumPut(dy - by, c, cb + 6, "Short")
		, NumPut(dw, c, cb + 8, "Short"), NumPut(dh, c, cb + 10, "Short")
	Return, true
}






1::
	Tooltip, Do the Thing
	Sleep 100
	
	
	
	sText := "abc"
	iTextLength := StrLen(sText) * 2
    
    ;First send a string length frame: high-word is 2, low-word is the length of the string
    VarSetCapacity(iFrame, 4, 0), NumPut((2 << 16) + iTextLength, iFrame, 0, "UInt")
    If (i := AHKsock_ForceSend(iPeerSocket, &iFrame, 4)) {
        OutputDebug, % "AHKsock_ForceSend failed with return value = " i " and error code = " ErrorLevel " at line " A_LineNumber
        ExitApp
    }
    
    ;Send the actual string now, excluding the null terminator
    If (i := AHKsock_ForceSend(iPeerSocket, &sText, iTextLength)) {
        OutputDebug, % "AHKsock_ForceSend failed with return value = " i " and error code = " ErrorLevel " at line " A_LineNumber
        ExitApp
    }
	Tooltip, Thing is Done
	Sleep 1000
	Tooltip
	
	
return
