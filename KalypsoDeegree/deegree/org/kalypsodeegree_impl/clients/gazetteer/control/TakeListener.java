// $Header:
// /cvsroot/deegree/deegree/org/deegree_impl/clients/gazetteer/control/TakeListener.java,v
// 1.7 2004/07/09 07:16:19 poth Exp $
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
package org.deegree_impl.clients.gazetteer.control;

import java.io.InputStreamReader;
import java.net.URL;

import org.deegree.enterprise.control.FormEvent;
import org.deegree.enterprise.control.RPCMethodCall;
import org.deegree.enterprise.control.RPCParameter;
import org.deegree.enterprise.control.RPCStruct;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureCollection;
import org.deegree_impl.clients.gazetteer.Constants;
import org.deegree_impl.clients.gazetteer.GazetteerClientException;
import org.deegree_impl.clients.gazetteer.configuration.GazetteerClientConfiguration;
import org.deegree_impl.enterprise.control.AbstractListener;
import org.deegree_impl.enterprise.control.RPCWebEvent;
import org.deegree_impl.model.feature.GMLFeatureAdapter;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.NetWorker;

/**
 * Listener to take the passed gazetteer term, to access its properties and pass
 * them to the target web resource (nextPage)
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author last edited by: $Author$
 * 
 * @version 1.0. $Revision$, $Date$
 * 
 * @since 1.1
 */
public final class TakeListener extends AbstractListener
{

  /**
   * inherited from <tt>AbstractListener</tt> performes the request
   * 
   * @param e
   */
  public void actionPerformed( FormEvent e )
  {

    RPCWebEvent rpcEvent = (RPCWebEvent)e;
    RPCMethodCall mc = rpcEvent.getRPCMethodCall();

    try
    {
      validateRequest( mc );
    }
    catch( Exception ex )
    {
      gotoErrorPage( "Invalid Request: " + ex.toString() );
      Debug.debugMethodEnd();
      return;
    }

    String request = null;
    try
    {
      request = createRequest( mc );
    }
    catch( Exception ex )
    {
      gotoErrorPage( "Couldn't create Gazetteer Request: " + ex.toString() );
      Debug.debugMethodEnd();
      return;
    }

    Feature feature = null;
    try
    {
      feature = performRequest( mc, request );
    }
    catch( Exception ex )
    {
      gotoErrorPage( "Couldn't perform Gazetteer Request: " + ex.toString() );
      Debug.debugMethodEnd();
      return;
    }

    this.getRequest().setAttribute( Constants.TERM, feature );
    RPCParameter[] params = mc.getParameters();
    RPCStruct struct = (RPCStruct)params[0].getValue();
    this.getRequest().setAttribute( Constants.TARGETTYPE,
        struct.getMember( Constants.TARGETTYPE ).getValue() );

  }

  /**
   * validates the request to be performed.
   * 
   * @param mc
   *          object containing the request to be performed
   */
  protected void validateRequest( RPCMethodCall mc ) throws GazetteerClientException
  {
    RPCParameter[] params = mc.getParameters();
    if( params == null || params.length != 1 )
    {
      throw new GazetteerClientException( "one rpc parameter containing a struct "
          + "with requiered parameters must be set" );
    }
    RPCStruct struct = (RPCStruct)params[0].getValue();
    if( struct.getMember( Constants.TARGETTYPE ) == null )
    {
      throw new GazetteerClientException( "parameter 'targetType' must be set." );
    }
    if( struct.getMember( Constants.GAZETTEER ) == null )
    {
      throw new GazetteerClientException( "parameter 'gazetteer' must be set." );
    }
    if( struct.getMember( Constants.IDENTIFIER ) == null )
    {
      throw new GazetteerClientException( "parameter 'identifier' must be set." );
    }

  }

  /**
   * creates a Gazetteer/WFS GetFeature request from the parameters contained in
   * the passed <tt>RPCMethodeCall</tt>.
   * 
   * @param mc
   * @return Gazetteer/WFS GetFeature request
   */
  protected String createRequest( RPCMethodCall mc ) throws GazetteerClientException
  {
    Debug.debugMethodBegin();

    RPCParameter[] params = mc.getParameters();
    RPCStruct struct = (RPCStruct)params[0].getValue();

    StringBuffer sb = new StringBuffer( 1000 );
    sb.append( "<wfs-g:GetFeature outputFormat='GML2' " );
    sb.append( "xmlns:wfs-g='http://www.opengis.net/wfs-g' " );
    sb.append( "xmlns:ogc='http://www.opengis.net/ogc' " );
    sb.append( "xmlns:gml='http://www.opengis.net/gml' " );
    sb.append( "xmlns:wfs='http://www.opengis.net/wfs' " );
    sb.append( "version='1.0.0' service='WFS'>" );
    String s = (String)struct.getMember( Constants.TARGETTYPE ).getValue();
    sb.append( "<wfs:Query typeName='" ).append( s ).append( "'>" );
    sb.append( "<wfs:PropertyName>identifier</wfs:PropertyName>" );
    sb.append( "<wfs:PropertyName>geographicIdentifier</wfs:PropertyName>" );
    sb.append( "<wfs:PropertyName>geographicExtent</wfs:PropertyName>" );
    sb.append( "<ogc:Filter>" );
    sb.append( "<ogc:PropertyIsEqualTo>" );
    sb.append( "<ogc:PropertyName>identifier</ogc:PropertyName>" );
    s = (String)struct.getMember( Constants.IDENTIFIER ).getValue();
    sb.append( "<ogc:Literal>" ).append( s ).append( "</ogc:Literal>" );
    sb.append( "</ogc:PropertyIsEqualTo>" );
    sb.append( "</ogc:Filter>" );
    sb.append( "</wfs:Query>" );
    sb.append( "</wfs-g:GetFeature>" );

    Debug.debugMethodEnd();
    System.out.println( sb );
    return sb.toString();
  }

  /**
   * performs a GetFeature request against a WFS-G and returns the result
   * encapsulated in <tt>GetTermResultSet</tt>
   * 
   * @param mc
   * @param request
   *          request to perform
   * @return one feature as result to the passed request
   * @throws GazetteerClientException
   */
  protected Feature performRequest( RPCMethodCall mc, String request )
      throws GazetteerClientException
  {
    Debug.debugMethodBegin();

    RPCParameter[] params = mc.getParameters();
    RPCStruct struct = (RPCStruct)params[0].getValue();

    GazetteerClientConfiguration conf = GazetteerClientConfiguration.getInstance();

    String gaze = (String)struct.getMember( Constants.GAZETTEER ).getValue();
    URL url = conf.getGazetteerAddress( gaze );
    NetWorker nw = new NetWorker( "ISO-8859-1", url, request );
    InputStreamReader isr = null;
    try
    {
      isr = new InputStreamReader( nw.getInputStream(), "UTF-8" );
    }
    catch( Exception e )
    {
      throw new GazetteerClientException( "Couldn't open stream from WFS-G", e );
    }

    FeatureCollection fc = null;
    try
    {
      fc = GMLFeatureAdapter.wrap( isr );
    }
    catch( Exception e )
    {
      throw new GazetteerClientException( "Couldn't open stream from WFS-G", e );
    }

    Debug.debugMethodEnd();
    if( fc == null || fc.getSize() == 0 )
    {
      return null;
    }
    return fc.getFeature( 0 );

  }

}
/*******************************************************************************
 * Changes to this class. What the people have been up to: $Log:
 * TakeListener.java,v $ Revision 1.7 2004/07/09 07:16:19 poth no message
 * 
 * Revision 1.6 2004/03/29 10:36:39 poth no message
 * 
 * Revision 1.5 2004/03/26 16:42:18 poth no message
 * 
 * Revision 1.4 2004/03/26 11:19:28 poth no message
 * 
 * Revision 1.3 2004/03/16 15:19:47 poth no message
 * 
 * Revision 1.2 2004/03/16 08:07:23 poth no message
 * 
 * Revision 1.1 2004/03/15 07:38:05 poth no message
 * 
 * 
 *  
 ******************************************************************************/