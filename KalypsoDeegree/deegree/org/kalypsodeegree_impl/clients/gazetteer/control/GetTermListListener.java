// $Header:
// /cvsroot/deegree/deegree/org/deegree_impl/clients/gazetteer/control/GetTermListListener.java,v
// 1.3 2004/03/26 16:42:18 poth Exp $
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

import org.deegree.enterprise.control.RPCMethodCall;
import org.deegree.enterprise.control.RPCParameter;
import org.deegree.enterprise.control.RPCStruct;
import org.deegree_impl.clients.gazetteer.Constants;
import org.deegree_impl.clients.gazetteer.GazetteerClientException;
import org.deegree_impl.tools.Debug;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author last edited by: $Author$
 * 
 * @version 1.0. $Revision$, $Date$
 * 
 * @since 1.1
 */
public class GetTermListListener extends GetRelatedTermsListener
{

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
    if( struct.getMember( Constants.GEOGRAPHICIDENTIFIER ) == null )
    {
      throw new GazetteerClientException( "parameter 'geomgraphicIdentifier' must be set." );
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
    sb.append( "<ogc:Filter>" );
    s = (String)struct.getMember( Constants.GEOGRAPHICIDENTIFIER ).getValue();
    try
    {
      Integer.parseInt( s );
      sb.append( "<ogc:PropertyIsEqualTo>" );
      sb.append( "<ogc:PropertyName>geographicIdentifier</ogc:PropertyName>" );
      sb.append( "<ogc:Literal>" ).append( s ).append( "</ogc:Literal>" );
      sb.append( "</ogc:PropertyIsEqualTo>" );
    }
    catch( Exception e )
    {
      sb.append( "<ogc:PropertyIsLike wildCard='*' singleChar='?' escape='/'>" );
      sb.append( "<ogc:PropertyName>geographicIdentifier</ogc:PropertyName>" );
      sb.append( "<ogc:Literal>*" ).append( s ).append( "*</ogc:Literal>" );
      sb.append( "</ogc:PropertyIsLike>" );
    }
    sb.append( "</ogc:Filter>" );
    sb.append( "</wfs:Query>" );
    sb.append( "</wfs-g:GetFeature>" );

    Debug.debugMethodEnd();
    return sb.toString();
  }

}
/*******************************************************************************
 * Changes to this class. What the people have been up to: $Log:
 * GetTermListListener.java,v $ Revision 1.3 2004/03/26 16:42:18 poth no message
 * 
 * Revision 1.2 2004/03/26 11:19:28 poth no message
 * 
 * Revision 1.1 2004/03/16 15:19:47 poth no message
 *  
 ******************************************************************************/