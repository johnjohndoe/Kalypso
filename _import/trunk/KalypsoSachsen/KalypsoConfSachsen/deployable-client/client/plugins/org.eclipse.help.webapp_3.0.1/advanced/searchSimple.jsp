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
	SearchData data = new SearchData(application, request, response);
	WebappPreferences prefs = data.getPrefs();
%>


<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8">

<title><%=ServletResources.getString("Search", request)%></title>
     
<style type="text/css">
/* need this one for Mozilla */
HTML { 
	width:100%;
	height:100%;
	margin:0px;
	padding:0px;
	border:0px;
 }

BODY {
	background:<%=prefs.getToolbarBackground()%>;
	border:0px;
	text:white;
	height:100%;
}

TABLE {
	font: <%=prefs.getToolbarFont()%>;
	background:<%=prefs.getToolbarBackground()%>;
	margin: 0px;
	padding: 0px;
	height:100%;
}

FORM {
	background:<%=prefs.getToolbarBackground()%>;
	height:100%;
	margin:0px;
}

INPUT {
	font: <%=prefs.getToolbarFont()%>;
	margin:0px;
	padding:0px;
}


#searchTable {
	padding-<%=isRTL?"right":"left"%>:5;
}

#searchWord {	
	padding-left:4px;
	padding-right:4px;
	border:1px solid ThreeDShadow;
}

#go {
	background:ThreeDShadow;
	color:Window;
	font-weight:bold;
	border:1px solid ThreeDShadow;
}


#advanced {
	text-decoration:underline; 
	text-align:<%=isRTL?"left":"right"%>;
	color:#0066FF; 
	cursor:hand;
	margin-<%=isRTL?"right":"left"%>:4px;
	border:0px;
}

<%
	if (data.isIE()) {
%>
#go {
	padding-<%=isRTL?"right":"left"%>:1px;
}
<%
	}
%>
</style>

<script language="JavaScript">
var isIE = navigator.userAgent.indexOf('MSIE') != -1;
var isMozilla = navigator.userAgent.toLowerCase().indexOf('mozilla') != -1 && parseInt(navigator.appVersion.substring(0,1)) >= 5;

<%
	String[] selectedBooks = data.getSelectedTocs();
%>
// create list of books initilize selectedBooks variable used by advances search
var selectedBooks = new Array(<%=selectedBooks.length%>);
<%
for (int i=0; i<selectedBooks.length; i++) 
{
%>
	selectedBooks[<%=i%>] = "<%=UrlUtil.JavaScriptEncode(selectedBooks[i])%>";
<%
}
%>

var advancedDialog;
var w = 400;
var h = 300;

function saveSelectedBooks(books)
{
	selectedBooks = new Array(books.length);
	for (var i=0; i<selectedBooks.length; i++){
		selectedBooks[i] = new String(books[i]);
	}
}

function openAdvanced()
{
	window.location="javascript://needModal";
	advancedDialog = window.open("advanced.jsp?searchWord="+encodeURIComponent(document.getElementById("searchWord").value), "advancedDialog", "resizeable=no,height="+h+",width="+w );
	advancedDialog.focus(); 
}

function closeAdvanced()
{
	try {
		if (advancedDialog)
			advancedDialog.close();
	}
	catch(e) {}
}

/**
 * This function can be called from this page or from
 * the advanced search page. When called from the advanced
 * search page, a query is passed.
 */
function doSearch(query)
{
	if (!query || query == "")
	{
		var form = document.forms["searchForm"];
		var searchWord = form.searchWord.value;
		var maxHits = form.maxHits.value;
		if (!searchWord || searchWord == "")
			return;
		query ="searchWord="+encodeURIComponent(searchWord)+"&maxHits="+maxHits;
	}
		
	/******** HARD CODED VIEW NAME *********/
	// do some tests to ensure the results are available
	if (parent.HelpFrame && 
		parent.HelpFrame.NavFrame && 
		parent.HelpFrame.NavFrame.showView &&
		parent.HelpFrame.NavFrame.ViewsFrame && 
		parent.HelpFrame.NavFrame.ViewsFrame.search && 
		parent.HelpFrame.NavFrame.ViewsFrame.search.searchViewFrame) 
	{
		parent.HelpFrame.NavFrame.showView("search");
		var searchView = parent.HelpFrame.NavFrame.ViewsFrame.search.searchViewFrame;
		searchView.location.replace("searchView.jsp?"+query);
	}
}

function fixHeights()
{
	if (!isIE) return;
	
	var h = document.getElementById("searchWord").offsetHeight;
	document.getElementById("go").style.height = h;
}

function onloadHandler(e)
{
	var form = document.forms["searchForm"];
	form.searchWord.value = '<%=UrlUtil.JavaScriptEncode(data.getSearchWord())%>';
	fixHeights();
}

</script>

</head>

<body dir="<%=direction%>" onload="onloadHandler()"  onunload="closeAdvanced()">

	<form  name="searchForm"   onsubmit="doSearch()">
		<table id="searchTable" align="<%=isRTL?"right":"left"%>" valign="middle" cellspacing="0" cellpadding="0" border="0">
			<tr nowrap  valign="middle">
				<td <%=isRTL?"nowrap":""%>>
					<label id="searchLabel" for="searchWord" accesskey="<%=ServletResources.getAccessKey("Search", request)%>">
					&nbsp;<%=ServletResources.getLabel("Search", request)%>:
					</label>
				</td>
				<td>
					<input type="text" id="searchWord" name="searchWord" value='' size="20" maxlength="256" alt='<%=ServletResources.getString("SearchExpression", request)%>' title='<%=ServletResources.getString("SearchExpression", request)%>'>
				</td>
				<td >
					&nbsp;<input type="button" onclick="this.blur();doSearch()" value='<%=ServletResources.getString("GO", request)%>' id="go" alt='<%=ServletResources.getString("GO", request)%>' title='<%=ServletResources.getString("GO", request)%>'>
					<input type="hidden" name="maxHits" value="500" >
				</td>
				<td nowrap>
					<a id="advanced" href="javascript:openAdvanced();" alt='<%=ServletResources.getString("Advanced", request)%>' title='<%=ServletResources.getString("Advanced", request)%>' onmouseover="window.status='<%=ServletResources.getString("Advanced", request)%>'; return true;" onmouseout="window.status='';"><%=ServletResources.getString("Advanced", request)%></a>&nbsp;
				</td>
			</tr>

		</table>
	</form>		

</body>
</html>

