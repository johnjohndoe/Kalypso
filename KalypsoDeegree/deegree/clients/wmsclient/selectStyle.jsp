<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">
<%@ page language="java" %>
<%@ page import="java.net.URL" %>
<%@ page import="java.util.ArrayList" %>
<%@ page import="java.util.HashMap" %>
<%@ page import="java.util.Iterator" %>
<%@ page import="org.deegree.services.wms.capabilities.Layer" %>
<%@ page import="org.deegree.services.wms.capabilities.Style" %>
<%@ page import="org.deegree_impl.tools.NetWorker" %>
<%
	HashMap availableStyles = (HashMap)request.getAttribute( "AVAILABLESTYLES" );
	ArrayList layers = (ArrayList)request.getAttribute( "LAYERS" );
	HashMap layerStyle = (HashMap)request.getAttribute( "LAYERSTYLE" );
%>
<html>
	<html>
	<head>
		<title>lat/lon WMS-Client</title>
		<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
		<style type="text/css"></style>
		<LINK REL="stylesheet" HREF="deegree.css"/>
		<script src="client.js"  language="JavaScript" type="text/javascript">
			<!--
			enableExternalCapture();															
			//-->
		</script>
		<script language="JavaScript">
			<!--
			symbols = new Array( ); 
			styleAbstracts = new Array( ); 
			styleAbstracts[0] = new Array( ); 
		
			function buildImageList() {		
				symbols[0] = new Array();		
				<%	
					Iterator iterator = availableStyles.keySet().iterator();
					while ( iterator.hasNext() ) {
						String s = (String)iterator.next();
						out.println( "img = new Image();" );
						out.println( "img.src = '" + availableStyles.get( s ) + "'" );
						out.println( "symbols[0]['" + s + "']=img;"  );
					}
					for (int i = 0; i < layers.size(); i++) {
 	     				Layer layer = (Layer)layers.get(i);
						Style[] styles = layer.getStyles();
						for ( int j = 0; j < styles.length; j++) {
							out.print( "styleAbstracts[0]['" + styles[j].getName() + "'] = '" + styles[j].getAbstract() + "';" );
						}
					}
				%>			
			}			
			
			function changeSymbol(select ) {
				list = select;
				var idx = list.selectedIndex;
				var s = list.options[idx].text;
				if ( s == 'default' ) {
					s = s + ':' + list.options[idx].value;
				}
				document.images[list.options[idx].value].src = symbols[0][s].src;
				document.forms[0].STYLEABSTRACT.value = styleAbstracts[0][s];
			}
			
			function submitToOpener() {
				opener.clearList();
				<%
					for (int i = layers.size()-1; i >= 0; i-- ) {
						Layer layer = (Layer)layers.get(i);
						out.println( "name =  '" + layer.getName() + "';" );
						out.println( "list =  document.forms[0]." + layer.getName().replace(':','_') + ";" );
				%>		
					var idx = list.selectedIndex;
					var s = list.options[idx].text;
					if ( s.substr(0,8) == 'default:' ) {
						s = 'default';
					}		
					opener.addNamedLayer( name, name+'|'+s );
					
				<%
					}
				%>
				opener.refreshMap();
				this.close();
			}
			
			-->
		</script>
	</head>
	<body bgcolor="#EEEEEE" onload="buildImageList(); ">
		
		<form action="">
			<table width="500" border="0" cellpadding="0" cellspacing="0">
				<tr>
					<td>
					<table align="center" border="0" width="500" cellpadding="0" cellspacing="0" >
						<tr  bgcolor="#de6339">
							<th>layer name</th>
							<th>available styles</th>
							<th>selected style</th>
						</tr>
<%	
	for (int i = 0; i < layers.size(); i++) {
 	     Layer layer = (Layer)layers.get(i);
		Style[] styles = layer.getStyles();
%>
					<tr>
						<td height="40" bgcolor="#de6339"><b><% out.print( "&nbsp;" + layer.getName() ); %></b></td>
						<td <% if (i % 2 == 1 ) out.print( " bgcolor='#de6339' " ); else  out.print( " bgcolor='#FFBB00' " );%>>
							&nbsp;<select name="<% out.print( layer.getName().replace(':','_') ); %>" size="1" onchange="changeSymbol(this);">
<%
    // list available styles
	for ( int j = 0; j < styles.length; j++) {
		out.print( "<option value='" + layer.getName() + "'" );
		String s = (String)layerStyle.get( layer.getName() ); 
		if ( s.equals( "default" ) ) {
			s = s + ":" + layer.getName();
		}
		if ( styles[j].getName().equals( s ) ) {
			out.print( " selected " );
		}
		out.println(  " />" + styles[j].getName() );
	}
	// paint selected style
	try {
		String s = (String)layerStyle.get( layer.getName() ); 
		if ( s.equals( "default" ) ) {
			s = s + ":" + layer.getName();
		}
		String src = "./legend/default.png";
		if ( layer.getStyleResource( s ) != null ) {
			URL url = layer.getStyleResource( s ).getLegendURL()[0].getOnlineResource();
			src = NetWorker.url2String( url );
		}
		out.print ("<td align='center' " );
		if (i % 2 == 1 ) out.print( " bgcolor='#de6339' " ); else  out.print( " bgcolor='#FFBB00' " );
		out.print (" width=\"40\"><img name='" + layer.getName()  + "' src=\"" + src + "\" alt=\"" + s +"\" /></td>" );
	} catch(Exception ee) {
		System.out.println(ee);
	}
%>					
							</select>
					</tr>
<%
	}
%>
					</table>
					</td>
				</tr>
				<tr>
					<td>
						style abstract:<br/>
						<textarea name="STYLEABSTRACT" cols="55" rows="5"></textarea>
					</td>
				</tr>
			</table>
			<br/>
			<a href="javascript:submitToOpener()"><img src="images/take_styles.gif" alt="setlayer" name="up" border="0"/></a>
			&nbsp;&nbsp;
			<a href="javascript:close()"><img src="images/cancel.gif" alt="setlayer" name="up" border="0"/></a>
		</form>
	</body>
</html>
