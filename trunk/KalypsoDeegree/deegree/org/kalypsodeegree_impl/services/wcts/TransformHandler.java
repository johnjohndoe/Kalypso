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
package org.deegree_impl.services.wcts;

import java.util.StringTokenizer;

import org.deegree.gml.GMLGeometry;
import org.deegree.services.Handler;
import org.deegree.services.OGCWebServiceEvent;
import org.deegree.services.OGCWebServiceException;
import org.deegree.services.wcts.protocol.TransformRequest;
import org.deegree.services.wcts.protocol.TransformResponse;
import org.deegree_impl.gml.GML_Transformer;
import org.deegree_impl.services.OGCWebServiceEvent_Impl;
import org.deegree_impl.services.OGCWebServiceException_Impl;
import org.deegree_impl.services.wcts.protocol.WCTS_ProtocolFactory;
import org.deegree_impl.tools.Debug;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * This class handles the Transform-request and creates the response which
 * contains the transformed data. If this failed, an exception is included in
 * the response.
 * 
 * <p>
 * --------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 2002-07-31
 */
class TransformHandler implements Handler
{
  /**
   * handles a request against an OGC web service
   */
  public void handleRequest( OGCWebServiceEvent request )
  {
    Debug.debugMethodBegin( this, "handleRequest" );

    TransformResponse response = null;
    TransformRequest req = (TransformRequest)request.getRequest();

    if( !req.getVersion().equals( "1.0.0" ) )
    {
      try
      {
        OGCWebServiceException exc = new OGCWebServiceException_Impl( getClass().toString(),
            "wrong version: " + req.getVersion() );

        // responseobjekt erzeugen
        response = WCTS_ProtocolFactory.createTransformResponse( req, exc, null );
      }
      catch( Exception ex )
      {
        System.out.println( ex );
      }
    }
    else
    {
      GML_Transformer gmltransformer = null;
      GMLGeometry result = null;

      try
      {
        CS_CoordinateSystem s_srs_cs = ( (TransformRequest)request.getRequest() ).getSourceCRS();
        CS_CoordinateSystem t_srs_cs = ( (TransformRequest)request.getRequest() )
            .getDestinationCRS();

        String s_srs = s_srs_cs.getName();
        String t_srs = t_srs_cs.getName();

        String source_srs_gml = completeSrs( s_srs );

        GMLGeometry[] geometries = ( (TransformRequest)request.getRequest() ).getGeometries();
        GMLGeometry[] result_array = new GMLGeometry[geometries.length];

        for( int i = 0; i < geometries.length; i++ )
        {
          geometries[i].setSrs( source_srs_gml );
          gmltransformer = new GML_Transformer();
          result = gmltransformer.transform( geometries[i], t_srs );
          result_array[i] = result;
        }

        response = WCTS_ProtocolFactory.createTransformResponse( req, null, result_array );
      }
      catch( Exception ex )
      {
        OGCWebServiceException exc = new OGCWebServiceException_Impl( getClass().toString(),
            "transformation failed: " + ex );

        // responseobjekt erzeugen
        response = WCTS_ProtocolFactory.createTransformResponse( req, exc, null );
      }
    }

    OGCWebServiceEvent ogcResp = new OGCWebServiceEvent_Impl( this, response, "-" );

    request.getDestination().write( ogcResp );
  }

  /**
   * handles the response of an OGC web service
   */
  public void handleResponse( OGCWebServiceEvent response )
  {}

  /**
   * returns true if the handler is interested in a event
   */
  public boolean isInterested( OGCWebServiceEvent event )
  {
    return true;
  }

  /**
   * registers a Handler so this Handler is able to act as a proxy to the
   * registered handler
   */
  public void registerHandler( Handler handler )
  {}

  /**
   * @see registerHandler
   */
  public void removeHandler( Handler handler )
  {}

  /**
   * completes SRS to GML-compatible SRS
   */
  private String completeSrs( String ts )
  {
    ts = ts.replace( ' ', ':' );

    StringTokenizer a = new StringTokenizer( ts, ":" );
    return "http://www.opengis.net/gml/srs/" + a.nextToken().toLowerCase() + ".xml#"
        + a.nextToken();
  }
}