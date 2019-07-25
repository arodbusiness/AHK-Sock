	#NoEnv
    #SingleInstance Force
    #Include AHKsock.ahk
    
	
	global oVoice := ComObjCreate("SAPI.SpVoice")
	Voices := oVoice.GetVoices
	oVoice.Voice := Voices.Item(1)
	
	
    ;Set up an error handler (this is optional)
    AHKsock_ErrorHandler("AHKsockErrors")
    
    ;Set up an OnExit routine
    OnExit, GuiClose
    
    ;Set default value to invalid handle
    iPeerSocket := -1
    
    ;Set up the GUI
    Gui, +Resize +OwnDialogs
    Gui, Add, Edit, r10 w300 vtxtDialog ReadOnly hwndhtxtDialog
    
    /*! Limit the edit control text length to 65535 because that's the maximum length we can express with the stream
    processing we use here, because the string's length must fit in the low-word of a 32-bit integer. See the comment block
    in the StreamProcessor function for more details
    */
    Gui, Add, Text, xm w300 vlblStatus hwndhlblStatus, Server Starting...
    Gui, +MinSize
    Gui, Show

	MyExternalIP=0.0.0.0
	TmpFile=%WinDir%\TEMP\IPAddress.TMP
	UrlDownloadToFile,http://www.whatismyip.org/,%TmpFile%
	FileRead,MyExternalIP,%TmpFile%
	FileDelete,%TmpFile%
	MyExternalIP := RegexMatch(MyExternalIP, "my-ip-address"">(.+?)</a", Match)
	MyExternalIP := Match1

	
	Port := 23
	
	
	
	If (i := AHKsock_GetAddrInfo(A_ComputerName, sIPList, 1)) {
        OutputDebug, % "AHKsock_GetAddrInfo failed.`nReturn value = " i ".`nErrorLevel = " ErrorLevel
        
    }
	else
	{
		OutputDebug, % "AHKsock_GetAddrInfo aquired the following IPs.`n" sIPList
	}
	
	global sName := sIPList
	
	
	;global sName := "192.168.1.105"
	;global sName := MyExternalIP
	
	
	If (i := AHKsock_Listen(Port, "Peer")) {
		OutputDebug, % "AHKsock_Listen() failed with return value = " i " and ErrorLevel = " ErrorLevel
		ExitApp
	}
	else
	{
		OutputDebug, % "Listening for peers..."
        GuiControl,, lblStatus, Waiting for peers on %sName% ;Update status
        oVoice.Speak("Server Started")
	}

    bSameMachine := 0
Return

GuiSize:
    Anchor(htxtDialog, "wh")
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
    Global iPeerSocket, bExiting, bSameMachine, bCantListen
    Static iIgnoreDisconnect
    
    If (sEvent = "ACCEPTED") {
        OutputDebug, % "A client with IP " sAddr " connected!"
        
		/*
        If (iPeerSocket <> -1) {
            OutputDebug, % "We already have a peer! Disconnecting..."
            AHKsock_Close(iSocket) ;Close the socket
            iIgnoreDisconnect += 1 ;So that we don't react when this peer disconnects
            Return
        }
        */

        ;Remember the socket
        iPeerSocket := iSocket
        
        ;Stop listening (see comment block in CONNECTED event)
        AHKsock_Listen(Port)
        
        
        ;Update status
        GuiControl,, lblStatus, Connected to %sAddr%!
		oVoice.Speak("A peer connected")
        
    } If (sEvent = "CONNECTED") {
        
        ;Check if the connection attempt was successful
        If (iSocket = -1) {
            OutputDebug, % "AHKsock_Connect() failed."
            
            ;Check if we are not currently listening, and if we already tried to listen and failed.
            If bCantListen
                ExitApp
            
            OutputDebug, % "Listening for peers..."
            GuiControl,, lblStatus, Waiting for peers on %sName% ;Update status
            oVoice.Speak("Waiting for Peers")
			
            ;If the connection attempt was on this computer, we can start listening now since the connect attempt is
            ;over and we thus run no risk of ending up connected to ourselves. 
            If bSameMachine {
                If (i := AHKsock_Listen(Port, "Peer")) {
                    OutputDebug, % "AHKsock_Listen() failed with return value = " i " and ErrorLevel = " ErrorLevel
                    ExitApp
                }
            }
            
            ;The connect attempt failed, but we are now listening for clients. We can leave now.
            Return
            
        } Else OutputDebug, % "AHKsock_Connect() successfully connected on IP " sAddr "."
        
        ;We now have an established connection with a peer
        
        ;This is the same fail-safe as in the ACCEPTED event (see comment block there)
        If (iPeerSocket <> -1) {
            OutputDebug, % "We already have a peer! Disconnecting..."
            AHKsock_Close(iSocket) ;Close the socket
            iIgnoreDisconnect += 1 ;So that we don't react when this peer disconnects
            Return
        }
        
        ;Remember the socket
        iPeerSocket := iSocket
        
        
        
        ;Update status
        GuiControl,, lblStatus, Connected to %sName%!
        
    } Else If (sEvent = "DISCONNECTED") {
        
        ;Check if we're supposed to ignore this event
        If iIgnoreDisconnect {
            iIgnoreDisconnect -= 1
            Return
        }
        
        ;Reset variable
        iPeerSocket := -1
        
        ;Delete any past data the stream processor had stored
        StreamProcessor()
        
        ;We should go back to listening (unless we're in the process of leaving)
        If Not bExiting {
            
            OutputDebug, % "A peer disconnected!"
            
            GuiControl,, lblStatus, Waiting for peers on %sName% ;Update status
            oVoice.Speak("A peer disconnected")
			If (i := AHKsock_Listen(Port, "Peer")) {
                OutputDebug, % "AHKsock_Listen() failed with return value = " i " and ErrorLevel = " ErrorLevel
                ExitApp
            }

        } Else OutputDebug, % "The peer disconnected! Exiting..."
        
    } Else If (sEvent = "RECEIVED") {
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
        
        ;Check what kind of frame it is
        If (iFrameType = 1) { ;It's a typing update frame
            GuiControl,, lblStatus, % iFrameData ? "Peer is typing..." : "Connected!" ;Update
            iOffset += 4 ;Increase the offset to be at the beginning of the next frame
        } Else If (iFrameType = 2) { ;It's a string length frame
            
            ;Check if the whole string is present
            ;We do + 4 because the string starts right after the frame we're currently processing
            If Not (iFrameData <= bDataLength - (iOffset + 4)) {
                
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
			
			Temp := DllCall("MulDiv", int, &sTextData, int, 1, int, 1, str)
			Tooltip % Temp
			;oVoice.Speak(Temp)
			
        }
    }

    ;Check if there are bytes part of an incomplete frame at the end of the stream that could not be processed
    If (n := (bDataLength - iOffset)) And (n < 4) {
        
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