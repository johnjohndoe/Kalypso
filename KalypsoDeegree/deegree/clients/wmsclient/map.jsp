<%@ page contentType="text/html"%>
<%@ page import="java.util.ArrayList" %>
<%@ page import="java.net.URL" %>
<%@ page import="org.deegree_impl.clients.wmsclient.model.Constants" %>
<%@ page import="org.deegree_impl.clients.wmsclient.configuration.WMSClientConfiguration" %>
<%@ page import="org.deegree_impl.clients.wmsclient.configuration.MapSize" %>
<%@ page import="org.deegree_impl.clients.wmsclient.configuration.MapOperation" %>
<%@ page import="org.deegree_impl.clients.wmsclient.configuration.Project" %>
<%@ page import="org.deegree_impl.clients.wmsclient.tools.ClientHelper" %>
<%@ page import="org.deegree.services.wms.protocol.WMSGetMapRequest" %>
<%@ page import="org.deegree.services.wms.protocol.WMSFeatureInfoRequest" %>
<%@ page import="org.deegree.services.wms.capabilities.WMSCapabilities" %>
<%@ page import="org.deegree.services.wms.capabilities.Layer" %>
<%@ page import="org.deegree_impl.tools.NetWorker" %>

<html>
	<head>
		<title>deegree WMS-Client</title>
		<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
		<style type="text/css"></style>
		<LINK REL="stylesheet" HREF="deegree.css"/>
		<script src="client.js" type="text/javascript">
			<!--
			enableExternalCapture();
			//-->
		</script>
	</head>
<%
	    WMSClientConfiguration cconfig = 
            (WMSClientConfiguration)request.getAttribute( Constants.WMSCLIENTCONFIGURATION );
        WMSCapabilities capa = cconfig.getWMSCapabilities()[0];
        WMSFeatureInfoRequest fir = (WMSFeatureInfoRequest)request.getAttribute( Constants.WMSGETFEATUREINFOREQUEST );
        String url_ = ClientHelper.getGetMapURL( capa );
        if (fir != null) {
                out.print( "<body onload=\"doFiReq('"+url_+fir.getRequestParameter()+"')\" bgcolor=\"#EEEEEE\">" );
        } else {
                out.print( "<body bgcolor=\"#EEEEEE\">" );
        }
 %>	
	<script language="JavaScript">
		<!--
		 function doFiReq( fiReq ) {
			    myFiWindow = window.open( fiReq ,"Fenster1","width=550,height=400,left=0,top=0,scrollbars=yes");
			    myFiWindow.focus();
			 }
		
		function setAction(action) {
			document.mapform.action = 'control?action=' + action ;
		}
		
		function gotoProject(request) {
		     while ( document.mapform.LAYERLIST.length > 0 ) {
		    		document.mapform.LAYERLIST.options[document.mapform.LAYERLIST.length-1] = null;
		    }
			document.mapform.WMSGETMAPREQUEST.value = request;
			document.mapform.action = 'control?action=REFRESH';
			document.mapform.submit();
		}
		
		function refreshMap() {
			selectLayers();
			document.mapform.action = 'control?action=REFRESH';
			document.mapform.submit();
		}
		
		function selectStyle() {
			stWindow = window.open( 'control?action=SELECTSTYLE&LAYERLIST=' + getLayerListAsString() ,
														"Fenster1","width=550,height=400,left=0,top=0,scrollbars=yes,dependent=yes," +
														"top=100,left=100");
		    stWindow.focus();		
		}
				

	-->
	</script>
	
<%
MapSize selectedMapSize = cconfig.getSelectedMapSize();
MapOperation[] mapOperations = cconfig.getOfferedMapOperations();
MapOperation so = cconfig.getSelectedMapOperation();
%>
		<FORM onSubmit="selectLayers();" name="mapform" action="control?action=<%out.print( so.getName() );%>" METHOD="POST"  ENCTYPE="application/x-www-form-urlencoded">

		<table align="center">
			<tr>
				<td bgcolor="#de6339" valign="top">
					<table border="0" width="100%">
					    <tr>
					     	<td align="right" valign="top"><a class="menu" href="http://www.lat-lon.de">lat/lon</a></td>
					    </tr>
					    <tr>
					     	<td align="right" valign="top"><a class="menu" href="http:/deegree.sourceforge.net">deegree</a></td>
					    </tr>
					    <tr><td>&nbsp;</td></tr>
					    <tr><td>&nbsp;</td></tr>
					    <tr><td><h2>Projects:</h2></td></tr>
<%
						Project[] projects = cconfig.getOfferedProjects();
						for (int i = 0; i < projects.length; i++) {
						    String s = projects[i].getInitialWMSGetMapRequest().getRequestParameter();
							out.print( "<tr><td><a href=\"javascript:gotoProject('" + s + "')\"><b>" );
							out.print( projects[i].getName() );
							out.print( "</b></a></td></tr>" );
						}
%>					    
				    </table>
				</td>
				<td>
					<h1>deegr<b class="orange">ee</b> WMS Client</h1>
					<center><img src="images/bar.gif" height="6" width="100%" border="0"></center>
					<table width="900">							
						<tr>
							<td >&nbsp;</td>
						</tr>
					    <tr>
					        <!-- map -->
					    	<td width="<% out.print( selectedMapSize.getWidth() + 80); %>" align="left">
								<table border="0" cellspacing="0" cellpadding="1">
									<tr valign="middle" align="center">
										<td colspan="3">
											<font face="arial,helvetica,sans serif" size="1" color="black">
			<%
				// will be set to true if the client offeres panning in the following loop
			    boolean hasPan = false;
				for ( int i = 0; i < mapOperations.length; i++ ) {
				    if ( !mapOperations[i].getName().equals( "PAN" ) ) {
						out.print( "<input type=\"Radio\" name=\"MAPMODE\" value=\"" + mapOperations[i].getName() + "\" " );
						out.print( " onclick=\"setAction('" + mapOperations[i].getName() + "');\" ");
						if ( mapOperations[i].isSelected() ) {
							out.print( "checked " );
						}
						out.print( "/>" + mapOperations[i].getName() );
					} else {
						// indicates that client offeres panning
						hasPan = true;
					}
				}
			%>									
											</font>
										</td>
									</tr>
									<tr valign="middle" align="center">
										<td>&nbsp;</td>
										<td>
			<%
											if ( hasPan ) {
											out.print( "<input type=\"image\" name=\"NORTH\" src=\"images/north.gif\" border=\"0\" " );
											out.print( " title='move the map to north' onclick=\"setAction('PAN');\" />" );
											}
			%>
										</td>
										<td>&nbsp;</td>
									</tr>
									<tr valign="middle" align="center">
										<td>
			<%
											if ( hasPan ) {							
												out.print( "<input type=\"image\" name=\"WEST\" src=\"images/west.gif\" border=\"0\" " );
												out.print( " title='move the map to west' onclick=\"setAction('PAN');\" />" );
											}
			%>
										</td>
										<td>
											<input type="image" name="CLICK" alt="CLICK"
			<%
				capa = cconfig.getWMSCapabilities()[0];
				String tmp = ClientHelper.getGetMapURL( capa );
				WMSGetMapRequest gmr = (WMSGetMapRequest)request.getAttribute( Constants.WMSGETMAPREQUEST );
				tmp = tmp + gmr.getRequestParameter(  );
			%>								
													src="<%out.print( tmp );%>"
													width="<%out.print( selectedMapSize.getWidth()  );%>"
													height="<%out.print( selectedMapSize.getHeight()  );%>"
													border="0" title="click to perform selected action"/>
										</td>
										<td>
			<%
											if ( hasPan ) {
												out.print( "<input type=\"image\" name=\"EAST\" src=\"images/east.gif\" border=\"0\" " );
												out.print( " title='move the map to east' onclick=\"setAction('PAN');\" />" );
											}
			%>
										</td>
									</tr>
									<tr valign="middle" align="center">
										<td>&nbsp;</td>
										<td>
			<%
											if ( hasPan ) {
												out.print( "<input type=\"image\" name=\"SOUTH\" src=\"images/south.gif\" border=\"0\" " );
 												out.print( " title='move the map to south' onclick=\"setAction('PAN');\" />" );
											}
			%>
										</td>
										<td>&nbsp;</td>
									</tr>
								</table>
							</td>
						    <td width="30">&nbsp;</td>
							<td valign="top" align="left">
<%
		    // returns a list of available layers from the submitted wms capabilities
		    
			Layer[] availableLayers = ClientHelper.getLayers( capa );
			org.deegree.services.wms.protocol.WMSGetMapRequest. Layer[] sL = gmr.getLayers();
			ArrayList selectedLayers = new ArrayList();
			for (int i = 0; i < sL.length;i++) {
				selectedLayers.add( sL[i].getName().trim() );
			}			
%>							
								<table>
									<tr>
										<!-- legend -->
										<td valign="top" height="280">
											<h2>Legend:</h2>
											<table>
<%
											for (int i = sL.length-1; i >= 0 ; i--) {
%>
												<tr>
<%
try {
													Layer lay = capa.getCapability().getLayer( sL[i].getName() );
													String st = sL[i].getStyleName();
													if ( st.equals( "default" ) ) {
														st = st + ":" + sL[i].getName();
													}
													URL url = lay.getStyleResource( st ).getLegendURL()[0].getOnlineResource();
												    String s = NetWorker.url2String( url );
													out.print ("<td width=\"40\"><img src=\"" + s + "\" alt=\"" + sL[i].getStyleName()+ ":" + sL[i].getName() +"\" /></td>" );
													out.print ("<td valign=\"middle\">" + lay.getTitle()  + "</td>" );
} catch(Exception ee) {
System.out.println(ee);
}
%>
												</tr>
<%
											}
%>										
											</table>
										</td>
									</tr>
								</table><BR>
								<table>
									<tr>
										<td>
											<table border="0" cellpadding="0" cellspacing="3">
												<tr>
													<td>
														<h2>Available Layers:</h2>
														<select width="250" name="AVAILABLELAYERS" size="1">
														<%
						 								    // fill list of available layers
															for (int i = 0; i < availableLayers.length; i++) {
																if ( availableLayers[i].getName() != null && 
																	 !availableLayers[i].getName().trim().equals("") ) {
																	out.print ("<option value=\"" + availableLayers[i].getName() + "\"" );
																	if ( selectedLayers.contains( availableLayers[i].getName() ) ) {
																		out.print (" selected " );
																	}
																	out.print ("  />" );
																	out.print ( availableLayers[i].getTitle() );
																} else {
																	out.print ("<option value=\"NODATA\" />-----" +availableLayers[i].getTitle() + "-----" );
																}
														    }
														%>											
														</select>													
													</td>
													<td align="left" valign="bottom">
														<a href="javascript:addLayer()"><img src="images/addLayer.gif" alt="add Layer" border="0"
																							       title="add layer to list"/></a><br/>
													</td>
												</tr>
												<tr>
													<td>
															<h2>Selected Layers:</h2>
															<select  width="250" name="LAYERLIST" multiple size="10" >
<%
													for (int i = sL.length-1; i >= 0; i--) {
														Layer lay = capa.getCapability().getLayer( sL[i].getName() );
														out.print ("<option value=\"" + sL[i].getName() + "|" + sL[i].getStyleName() +  "\" />" );
														out.print ( sL[i].getName() );
													}
%>
															</select>		
													</td>
													<td>
														<table cellpadding="0" cellspacing="0">
															<tr>
																<td>&nbsp;</td>
															</tr>
															<tr valign="top">
																<td align="left">
																	<a href="javascript:removeFromList()"><img src="images/removeLayer.gif" alt="" name="up" border="0"
																										title="remove selected layer from the list"/></a>
																</td>
															</tr>															
															<tr>
																<td>&nbsp;</td>
															</tr>
															<tr>
																<td align="center" valign="bottom">
																	<a href="javascript:move( 'up' )"><img src="images/up.gif" alt="" name="up" border="0"
																									 title="move selected layer up" /></a>
																</td>
															</tr>
															<tr>
																<td align="center" valign="top">
																	<a href="javascript:move( 'down' )"><img src="images/down.gif" name="down" border="0" 
																									    title="move selected layer down" /></a>
																</td>
															</tr>
															<tr>
																<td>&nbsp;</td>
															</tr>
															<tr valign="top">
																<td align="left">
																	<a href="javascript:selectStyle()"><img src="images/set_style.gif" alt="setlayer" name="up" border="0" 
																									title="define style for selected layers"/></a>
																</td>
															</tr>
														</table>
													</td>
												</tr>
											</table>										    																						
										</td>
									</tr>				
								</table>
							</td>
						</tr>
					</table>
					<br/>
					<center>
						<img src="images/bar.gif" height="6" width="100%" border="0"><br/>
						(c) deegree WMS 1.1.1 
						<script LANGUAGE="JavaScript">
							var now = new Date();
							document.write( now );
					    </script>
					</center>
				</td>
			</tr>
		</table>
		<p>
		<!-- wms specific parameters -->
		<input type="hidden" name="WMSGETMAPREQUEST" value="<%
			out.print( gmr.getRequestParameter() );
		%>">
	</form>
	<!-- eof form -->
	</body>
</html>