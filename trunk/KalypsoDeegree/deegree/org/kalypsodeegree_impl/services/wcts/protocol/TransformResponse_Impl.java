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
package org.deegree_impl.services.wcts.protocol;

import java.util.ArrayList;

import org.deegree.gml.GMLGeometry;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.wcts.protocol.TransformResponse;
import org.deegree.xml.XMLTools;
import org.deegree_impl.gml.GMLGeometry_Impl;
import org.deegree_impl.services.OGCWebServiceResponse_Impl;
import org.deegree_impl.tools.Debug;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * <p>
 * The TransformResponse-schema describes the format of the answer of the
 * transform-request that is fired by the coordinate-transformation-web-service.
 * the root-element (TransformResponse) inludes in the case of a error-free
 * request-handling the transformed data. If am error occures during the
 * processing, an Exception is included in the the TransformResponse-element.
 * The format of the returned transformed data is defined in the request. In
 * addition to the GML-coded geometries a coding as &quot;Well Known Text&quot;
 * (WKT) is possible. A WKT-geometry is transported as String.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 2002-07-19
 */
public class TransformResponse_Impl extends OGCWebServiceResponse_Impl implements TransformResponse
{
  private ArrayList data = null;

  /**
   * Creates a new TransformResponse_Impl object.
   * 
   * @param request
   * @param exception
   * @param data
   */
  TransformResponse_Impl( OGCWebServiceRequest request, Document exception, GMLGeometry[] data )
  {
    super( request, exception );
    Debug.debugMethodBegin( "TransformResponse_Impl",
        "TransformResponse_Impl(OGCWebServiceRequest, Document, GMLGeometry)" );

    this.data = new ArrayList();
    setGeometries( data );

    Debug.debugMethodEnd();
  }

  /**
   * gets the Data. The Format of the data is defined in the request.
   */
  public GMLGeometry[] getGeometries()
  {
    Debug.debugMethodBegin( "TransformResponse_Impl", "getGeometries" );

    GMLGeometry[] geo = (GMLGeometry[])data.toArray( new GMLGeometry[data.size()] );
    Debug.debugMethodEnd();
    return geo;
  }

  /**
   * @see #getGeometries()
   */
  public void addGeometry( GMLGeometry data )
  {
    Debug.debugMethodBegin( "TransformResponse_Impl", "addGeometry" );
    this.data.add( data );
    Debug.debugMethodEnd();
  }

  /**
   * @see #getGeometries()
   */
  public void setGeometries( GMLGeometry[] data )
  {
    Debug.debugMethodBegin( "TransformResponse_Impl", "setGeometries" );
    this.data.clear();

    if( data != null )
    {
      for( int i = 0; i < data.length; i++ )
      {
        this.data.add( data[i] );
      }
    }

    Debug.debugMethodEnd();
  }

  /**
   * creates the TransformResponse
   */
  public Document exportAsXML() throws Exception
  {
    Debug.debugMethodBegin( "TransformResponse_Impl", "exportAsXML" );

    Document doc = XMLTools.create();
    Element elem = doc.createElement( "TransformResponse" );
    doc.appendChild( elem );

    for( int i = 0; i < data.size(); i++ )
    {
      Node node = ( (GMLGeometry_Impl)data.get( i ) ).getAsElement();
      XMLTools.insertNodeInto( node, elem );
    }

    Document exDoc = this.getException();

    if( exDoc != null )
    {
      Element exElem = (Element)exDoc.getElementsByTagName( "Exception" ).item( 0 );
      XMLTools.insertNodeInto( exElem, elem );
    }

    Debug.debugMethodEnd();
    return doc;
  }

  /**
   * parses geometries to nodes private Node toDOMNode(GMLGeometry data) throws
   * Exception { Debug.debugMethodBegin("TransformResponse_Impl", "toDOMNode");
   * 
   * String stringi = data.toString(); Node node = XMLTools.parse(stringi);
   * 
   * Debug.debugMethodEnd(); return node; }
   */
}