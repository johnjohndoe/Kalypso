/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR  
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de

 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.clients.wcasclient.control;

import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.deegree.enterprise.control.FormEvent;
import org.deegree.enterprise.control.RPCMember;
import org.deegree.enterprise.control.RPCParameter;
import org.deegree.enterprise.control.RPCStruct;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Point;
import org.deegree.services.wms.protocol.WMSGetMapRequest;
import org.deegree.xml.ElementList;
import org.deegree.xml.XMLTools;
import org.deegree_impl.clients.wcasclient.CatalogClientException;
import org.deegree_impl.clients.wcasclient.Constants;
import org.deegree_impl.clients.wcasclient.configuration.CSWClientConfiguration;
import org.deegree_impl.clients.wcasclient.model.Selection;
import org.deegree_impl.clients.wmsclient.configuration.WMSClientConfiguration;
import org.deegree_impl.enterprise.control.AbstractListener;
import org.deegree_impl.enterprise.control.RPCWebEvent;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.services.wms.protocol.WMSProtocolFactory;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.NetWorker;
import org.deegree_impl.tools.StringExtend;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Listener for initianlizing the map view.
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public class MapViewListener extends AbstractListener
{

  /**
   * 
   * 
   * @param e
   */
  public void actionPerformed( FormEvent e )
  {
    Debug.debugMethodBegin();

    RPCWebEvent webEvent = (RPCWebEvent)e;
    String msg = null;
    try
    {
      // validate if the current event is a valid MapViewListener event
      validateRequest( webEvent );
      // check if the user owns the required rights
      msg = proofAuthorization( webEvent );
    }
    catch( Exception ee )
    {
      gotoErrorPage( "Invalid Request: " + StringExtend.stackTraceToString( ee.getStackTrace() ) );
      Debug.debugMethodEnd();
      return;
    }

    if( !msg.equals( "" ) )
    {
      // user doesn't have the authorization to download the ordered
      // datasets
      this.setNextPage( this.getAlternativeNextPage() );
      this.getRequest().setAttribute( Constants.MESSAGE, msg );
    }
    else
    {
      // get all WMS GetMap request resulting from the requested IDs/layers
      HashMap gmr = getWMSGetMapRequests( webEvent );
      if( gmr != null )
      {
        markAsSelected( webEvent );
        // remove result from a previous search (full metadata description)
        // from the session
        HttpSession session = ( (HttpServletRequest)this.getRequest() ).getSession( true );
        session.removeAttribute( Constants.SESSION_METADATA );

        this.getRequest().setAttribute(
            org.deegree_impl.clients.wmsclient.model.Constants.WMSCLIENTCONFIGURATION,
            CSWClientConfiguration.getInstance().getWMSClientConfiguration() );
        Iterator iterator = gmr.keySet().iterator();
        Object o = iterator.next();
        WMSGetMapRequest mapReq = (WMSGetMapRequest)gmr.get( o );

        this.getRequest().setAttribute(
            org.deegree_impl.clients.wmsclient.model.Constants.WMSGETMAPREQUEST, mapReq );
      }

    }

    Debug.debugMethodEnd();
  }

  /**
   * creates all WMS GetMap request resulting from the requested IDs/layers
   */
  private HashMap getWMSGetMapRequests( RPCWebEvent webEvent )
  {
    Debug.debugMethodBegin();

    // get associations between catalogs and IDs/layers
    HashMap gmr = new HashMap();
    String catalog = "";
    HashMap tmp = null;
    try
    {
      GM_Envelope bbox = calcBoundingBox( webEvent );
      HashMap cat_Ids = getCatalogIdAssociations( webEvent );
      Iterator iterator = cat_Ids.keySet().iterator();
      while( iterator.hasNext() )
      {
        catalog = (String)iterator.next();
        ArrayList list = (ArrayList)cat_Ids.get( catalog );
        String[] ids = (String[])list.toArray( new String[list.size()] );
        tmp = getWMSGetMapCalls( catalog, ids, bbox );
        gmr.putAll( tmp );
        catalog = "";
      }

    }
    catch( CatalogClientException cce )
    {
      gotoErrorPage( "Couldn't get GetMap request for '" + catalog + "': "
          + StringExtend.stackTraceToString( cce.getStackTrace() ) );
    }
    catch( org.deegree.services.InconsistentRequestException ire )
    {
      gotoErrorPage( "Couldn't create GetMap request: \n" + tmp + "\n"
          + StringExtend.stackTraceToString( ire.getStackTrace() ) );
    }
    catch( org.deegree.xml.XMLParsingException xpe )
    {
      gotoErrorPage( "Couldn't create GetMap request: \n" + tmp + "\n"
          + StringExtend.stackTraceToString( xpe.getStackTrace() ) );
    }
    catch( java.net.MalformedURLException mue )
    {
      gotoErrorPage( "Couldn't create GetMap request: \n" + tmp + "\n"
          + StringExtend.stackTraceToString( mue.getStackTrace() ) );
    }
    catch( Exception ex )
    {
      gotoErrorPage( "Error: " + StringExtend.stackTraceToString( ex.getStackTrace() ) );
    }

    Debug.debugMethodEnd();
    return gmr;
  }

  /**
   * validates if the passed event contains a valid request
   */
  private void validateRequest( RPCWebEvent event ) throws CatalogClientException
  {
    Debug.debugMethodBegin();
    Debug.debugMethodEnd();
  }

  /**
   * checks if the current user is authorized to to download the ordered
   * datasets
   */
  private String proofAuthorization( RPCWebEvent event ) throws CatalogClientException
  {
    Debug.debugMethodBegin();

    RPCParameter[] params = event.getRPCMethodCall().getParameters();

    StringBuffer sb = sb = new StringBuffer();
    for( int i = 0; i < params.length; i++ )
    {
      RPCStruct struct = (RPCStruct)params[i].getValue();
      RPCMember mem = struct.getMember( Constants.RPC_ID );
      String fileIdentifier = (String)mem.getValue();
      String msg = isAuthorizied( fileIdentifier );
      if( msg != null )
      {
        // collect NoAuthorization messages
        sb.append( "<BR/>" + msg );
      }
    }

    Debug.debugMethodEnd();
    return sb.toString();
  }

  /**
   * returns the associations between catalogs and the IDs/layers that are
   * served by them. for each catalog the returned HashMap contains an ArrayList
   * containing the associated IDs
   */
  private HashMap getCatalogIdAssociations( RPCWebEvent event ) throws CatalogClientException
  {
    Debug.debugMethodBegin();

    RPCParameter[] params = event.getRPCMethodCall().getParameters();
    // get/create GetMap-requests for each passed layer/id
    HashMap cat_Ids = new HashMap();
    for( int i = 0; i < params.length; i++ )
    {
      RPCStruct struct = (RPCStruct)params[i].getValue();
      RPCMember mem = struct.getMember( Constants.RPC_CATALOG );
      String catalog = (String)mem.getValue();
      mem = struct.getMember( Constants.RPC_ID );
      String id = (String)mem.getValue();

      // get IDs associated with the current catalog and add the
      // current ID to the list. If necessary, create a new list
      ArrayList list = (ArrayList)cat_Ids.get( catalog );
      if( list == null )
      {
        list = new ArrayList();
      }
      list.add( id );
      cat_Ids.put( catalog, list );

    }

    Debug.debugMethodEnd();
    return cat_Ids;
  }

  /**
   * validates if the current user iss allowed to display the selected datasets
   * with a WMS
   * 
   * @return
   */
  protected String isAuthorizied( String id ) throws CatalogClientException
  {
    // returning <tt>null</tt> indicates that an authorization exists
    return null;
  }

  /**
   * returns the addresses to be used to perform a GetMap request for the passed
   * layers (IDs). The method assumes that all passed IDs/layers are served by
   * one WMS.
   * 
   * @param catalog
   *          catalog where the required ISO 19119 data are stored
   * @param ids
   *          IDs/layers to find the serving WMS
   * @param bbox
   *          bbox of the resulting WMS GetMap request(s)
   */
  private HashMap getWMSGetMapCalls( String catalog, String[] ids, GM_Envelope bbox )
      throws CatalogClientException
  {
    Debug.debugMethodBegin();

    String href = null;
    String version = null;
    try
    {
      String casReq = ISO19119RequestFactory.createRequest( "Full", "WMS", ids, "GetMap" );
      URL url = CSWClientConfiguration.getInstance().getCatalogServerAddress( catalog );

      NetWorker nw = new NetWorker( url, casReq );
      Reader reader = new InputStreamReader( nw.getInputStream() );
      Document doc = XMLTools.parse( reader );
      NodeList nl = doc.getElementsByTagName( "serviceTypeVersion" );
      // get service version; if not available use 1.1.0 as default
      if( nl.getLength() > 0 )
      {
        version = XMLTools.getStringValue( nl.item( 0 ) );
      }
      else
      {
        version = "1.1.0";
      }
      nl = doc.getElementsByTagName( "operationMetadata" );
      // loop over all supported operation
      for( int i = 0; i < nl.getLength(); i++ )
      {
        String s1 = XMLTools.getRequiredStringValue( "operationName", null, nl.item( i ) );
        String s2 = XMLTools.getRequiredStringValue( "nameNameSpace", null, nl.item( i ) );
        // is required operation?
        if( s1.equals( "GetMap" ) && s2.equals( "OGC" ) )
        {
          ElementList ell = XMLTools.getChildElementsByName( "DCP", null, nl.item( i ) );
          // get web address/accesspoint for GetMap operation
          for( int k = 0; k < ell.getLength(); k++ )
          {
            String s3 = XMLTools.getAttrValue( "type", ell.item( k ) );
            if( s3.equalsIgnoreCase( "GET" ) || s3.equalsIgnoreCase( "HTTPGET" ) )
            {
              Node node = ell.item( k ).getElementsByTagName( "linkage" ).item( k );
              href = XMLTools.getAttrValue( "href", node );
              // leaf the sub loop
              break;
            }
          }
          // leaf the main loop
          if( href != null )
            break;
        }
      }

    }
    catch( Exception e )
    {
      throw new CatalogClientException( "", e );
    }

    WMSClientConfiguration wmsConf = CSWClientConfiguration.getInstance()
        .getWMSClientConfiguration();

    HashMap map = new HashMap();
    map.put( "REQUEST", "GetMap" );
    map.put( "SERVICE", "WMS" );
    map.put( "VERSION", version );
    map.put( "BBOX", bbox.getMin().getX() + "," + bbox.getMin().getY() + "," + bbox.getMax().getX()
        + "," + bbox.getMax().getY() );

    StringBuffer sb = new StringBuffer( 1000 );
    for( int i = 0; i < ids.length; i++ )
    {
      sb.append( ids[i] );
      if( i < ids.length - 1 )
      {
        sb.append( "," );
      }
    }
    map.put( "LAYERS", sb.toString() );
    map.put( "STYLES", "" );
    map.put( "FORMAT", "image/jpeg" );
    map.put( "TRANSPARENT", "false" );
    map.put( "EXCEPTIONS", "application/vnd.ogc.se_inimage" );
    map.put( "BGCOLOR", "0xffffff" );
    map.put( "WIDTH", "" + wmsConf.getSelectedMapSize().getWidth() );
    map.put( "HEIGHT", "" + wmsConf.getSelectedMapSize().getHeight() );
    map.put( "SRS", wmsConf.getInitialGetMapRequest().getSrs() );

    HashMap gmr = new HashMap();
    try
    {
      gmr.put( href, WMSProtocolFactory.createGetMapRequest( "id" + 1, map ) );
    }
    catch( Exception e )
    {
      throw new CatalogClientException( "", e );
    }

    Debug.debugMethodEnd();

    return gmr;
  }

  /**
   * marks all metadatasets (threir IDs) contained in the passed webEvent as
   * selected
   */
  private void markAsSelected( RPCWebEvent webEvent )
  {
    Debug.debugMethodBegin();

    // remove result from a previous search (full metadata description)
    // from the session
    HttpSession session = ( (HttpServletRequest)this.getRequest() ).getSession( true );
    Selection selection = (Selection)session.getAttribute( Constants.SESSION_SELECTION );

    if( selection != null )
    {
      // unselect all entries to avoid mixing current selected entries
      // with entries that have been selected before
      selection.unselectAll();

      RPCParameter[] params = webEvent.getRPCMethodCall().getParameters();

      for( int i = 0; i < params.length; i++ )
      {
        RPCStruct struct = (RPCStruct)params[i].getValue();
        RPCMember mem = struct.getMember( Constants.RPC_CATALOG );
        String catalog = (String)mem.getValue();
        mem = struct.getMember( Constants.RPC_ID );
        String fileIdentifier = (String)mem.getValue();
        selection.setEntryToSelected( fileIdentifier, catalog, true );
      }

      session.setAttribute( Constants.SESSION_SELECTION, selection );

    }

    Debug.debugMethodEnd();
  }

  /**
   * calculates the common bounding box of all submitted boxes and returns it in
   * the coordinate reference system of the dataset
   */
  private GM_Envelope calcBoundingBox( RPCWebEvent event ) throws Exception
  {
    Debug.debugMethodBegin();

    double minx = 9E99;
    double miny = 9E99;
    double maxx = -9E99;
    double maxy = -9E99;

    RPCParameter[] params = event.getRPCMethodCall().getParameters();
    // get/create GetMap-requests for each passed layer/id
    for( int i = 0; i < params.length; i++ )
    {
      RPCStruct struct = (RPCStruct)params[i].getValue();
      RPCMember mem = struct.getMember( Constants.RPC_BBOX );
      struct = (RPCStruct)mem.getValue();
      Double mnx = (Double)struct.getMember( Constants.RPC_BBOXMINX ).getValue();
      Double mny = (Double)struct.getMember( Constants.RPC_BBOXMINY ).getValue();
      Double mxx = (Double)struct.getMember( Constants.RPC_BBOXMAXX ).getValue();
      Double mxy = (Double)struct.getMember( Constants.RPC_BBOXMAXY ).getValue();

      if( mnx.doubleValue() < minx )
      {
        minx = mnx.doubleValue();
      }

      if( mny.doubleValue() < miny )
      {
        miny = mny.doubleValue();
      }

      if( mxx.doubleValue() > maxx )
      {
        maxx = mxx.doubleValue();
      }

      if( mxy.doubleValue() > maxy )
      {
        maxy = mxy.doubleValue();
      }
    }

    if( minx > maxx )
    {
      double tmp = minx;
      minx = maxx;
      maxx = tmp;
    }

    if( miny > maxy )
    {
      double tmp = miny;
      miny = maxy;
      maxy = tmp;
    }

    GM_Point min = GeometryFactory.createGM_Point( minx, miny, null );
    GM_Point max = GeometryFactory.createGM_Point( maxx, maxy, null );

    // create bounding box with coordinates in the datasets reference system
    GM_Envelope bbox = GeometryFactory.createGM_Envelope( min.getX(), min.getY(), max.getX(), max
        .getY() );

    Debug.debugMethodEnd();
    return bbox;
  }

}