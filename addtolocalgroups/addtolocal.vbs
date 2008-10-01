On Error Resume Next
'================================================================================
' * Adds users or groups from a domain to a workstation's local groups *
'
'Example usage
'addLocalToLocal "LocalGroup","Administrators"
'addToLocal "DomainGroup","Administrators","DOMAINNAME"
'================================================================================
'Clean up objects
Set oGroup = nothing
Set oWshNet = nothing
Set oUser = nothing
WScript.Quit
'
' Add domain users or groups to local groups
'
Sub addToLocal(netUser,lGroup,Domain)
sDomain = Domain
sUser = netUser
Set oUser = GetObject("WinNT://" & sDomain & "/" & sUser)
Set oWshNet = CreateObject("Wscript.Network")
sComputerName = oWshNet.ComputerName
'checkForErr
sGroup = lGroup
Set oGroup = GetObject("WinNT://" & sComputerName & "/" & sGroup)
'checkForErr
oGroup.add(oUser.ADsPath)
End Sub
'
' Add local accounts to local groups
'
Sub addLocalToLocal(netUser,lGroup)
Set oWshNet = CreateObject("Wscript.Network")
sComputerName = oWshNet.ComputerName
'checkForErr
sUser = netUser
Set oUser = GetObject("WinNT://" & sComputerName & "/" & sUser)
sGroup = lGroup
Set oGroup = GetObject("WinNT://" & sComputerName & "/" & sGroup)
'checkForErr
oGroup.add(oUser.ADsPath)
End Sub
'
' Check if an error has occured
'
Sub checkForErr
If err.number <> 0 Then
  Wscript.Echo("An error has occured." & Err.Number)
  WScript.Quit
End If
End Sub
'===============================================================================