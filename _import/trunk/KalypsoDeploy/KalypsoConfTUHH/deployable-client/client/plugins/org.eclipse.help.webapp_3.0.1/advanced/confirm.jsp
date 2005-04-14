<%--
 Copyright (c) 2000, 2004 IBM Corporation and others.
 All rights reserved. This program and the accompanying materials 
 are made available under the terms of the Common Public License v1.0
 which accompanies this distribution, and is available at
 http://www.eclipse.org/legal/cpl-v10.html
 
 Contributors:
     IBM Corporation - initial API and implementation
--%>
<%@ include file="header.jsp"%>

<% 
	RequestData data = new RequestData(application, request, response);
	WebappPreferences prefs = data.getPrefs();
	String okText="";
	String cancelText="";
	if("yesno".equalsIgnoreCase(request.getParameter("buttons"))){
		okText=ServletResources.getString("yes", request);
		cancelText=ServletResources.getString("no", request);
	}else{
		okText=ServletResources.getString("OK", request);
		cancelText=ServletResources.getString("Cancel", request);
	}
%>


<html>
<head>
<title><%=ServletResources.getString(request.getParameter("title"), request)%></title>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
<meta http-equiv="Pragma" content="no-cache">
<meta http-equiv="Expires" content="-1">


<style type="text/css">
<%@ include file="list.css"%>
</style>

<style type="text/css">
HTML, BODY {
	width:100%;
	height:100%;
	margin:0px;
	padding:0px;
	border:0px;
}

BODY {
	background-color: <%=prefs.getToolbarBackground()%>;
}

TABLE {
	width:auto;
}

TD, TR{
	margin:0px;
	padding:0px;
	border:0px;
}

BUTTON {
	font:<%=prefs.getViewFont()%>;
}

</style>

<script language="JavaScript">

function onloadHandler() {
<%
if(!data.isMozilla() || "1.3".compareTo(data.getMozillaVersion()) <=0){
// buttons are not resized immediately on mozilla before 1.3
%>
	sizeButtons();
<%}%>
	document.getElementById("<%=request.getParameter("initialFocus")%>").focus();
}

function sizeButtons() {
	var minWidth=60;

	if(document.getElementById("ok").offsetWidth < minWidth){
		document.getElementById("ok").style.width = minWidth+"px";
	}
	if(document.getElementById("cancel").offsetWidth < minWidth){
		document.getElementById("cancel").style.width = minWidth+"px";
	}
}

function confirmed(){
	try{
<% if ("true".equalsIgnoreCase(request.getParameter("dontaskagain")) ){ %>
		if(document.getElementById("dontask").checked){
			window.opener.<%=request.getParameter("dontaskagainCallback")%>;
		}
<% } %>
		window.opener.<%=request.getParameter("confirmCallback")%>;
	} catch(e) {}
 	window.close();
	return false;
}

</script>

</head>

<body dir="<%=direction%>" onload="onloadHandler()">
<form onsubmit="confirmed();return false;">
<div style="overflow:auto;height:220px;width:100%;">
	<div style="padding:10px;">
	<span style="font-weight:bold;"><%=ServletResources.getString(request.getParameter("header"), request)%></span>
	<br><br>
	<%=ServletResources.getString(request.getParameter("message"), request)%>
	</div>
</div>
<% if ("true".equalsIgnoreCase(request.getParameter("dontaskagain")) ){ %>
<div style="height:30px;">
	<div style="padding-left:10px;padding-right:10px;">
	<input class='check'
			type="checkbox" 
			id='dontask' 
			name='dontask'>
			<label for="dontask"
				accesskey="<%=ServletResources.getAccessKey("dontask", request)%>">
				<%=ServletResources.getLabel("dontask", request)%>
			</label>
	</div>				
</div>
<% } %>
<div style="height:50px;">
	<table valign="bottom" align="<%=isRTL?"left":"right"%>" style="background:<%=prefs.getToolbarBackground()%>">
		<tr id="buttonsTable" valign="bottom"><td valign="bottom" align="<%=isRTL?"left":"right"%>">
  			<table cellspacing=10 cellpading=0 border=0 style="background:transparent;">
				<tr>
					<td>
						<button type="submit" id="ok"><%=okText%></button>
					</td>
					<td>
					  	<button type="reset" onclick="window.close()" id="cancel"><%=cancelText%></button>
					</td>
				</tr>
  			</table>
		</td></tr>
	</table>
</div>
</form>
</body>
</html>
