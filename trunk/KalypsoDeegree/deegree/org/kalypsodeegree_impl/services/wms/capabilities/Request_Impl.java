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
package org.deegree_impl.services.wms.capabilities;

import java.util.HashMap;

import org.deegree.services.wms.capabilities.Operation;
import org.deegree.services.wms.capabilities.Request;
import org.deegree.xml.Marshallable;

/**
 * The interface <Request>describes the acces to the available WMS operations.
 * GetMap and GetCapabilities are mandatory. GetFeatureInfo, DescribeLayer,
 * GetLegendGraphic, getStyles and PutStyles are optional.
 * 
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$
 */
class Request_Impl implements Request, Marshallable
{
  private HashMap operations = null;

  /**
   * default constructor
   */
  Request_Impl()
  {
    operations = new HashMap();
  }

  /**
   * default constructor
   */
  Request_Impl( Operation[] operations )
  {
    this();

    for( int i = 0; i < operations.length; i++ )
    {
      addOperation( operations[i] );
    }
  }

  /**
   * returns the operation identified by the submitted key. If the WMS doesn't
   * support the desired operation <tt>null</tt> will be returned.
   */
  public Operation getOperation( int operationKey )
  {
    String s = Operation_Impl.getName( operationKey );
    return getOperation( s );
  }

  /**
   * returns the operation identified by the submitted operation name. If the
   * WMS doesn't support the desired operation <tt>null</tt> will be returned.
   */
  public Operation getOperation( String operationName )
  {
    return (Operation)operations.get( operationName );
  }

  /**
   * adds a operation to the request
   */
  public void addOperation( Operation operation )
  {
    operations.put( operation.getOperationName(), operation );
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = null;
    ret = "operations = " + operations + "\n";
    return ret;
  }

  /**
   * Returns an XML representation of this object.
   */
  public String exportAsXML()
  {
    StringBuffer sb = new StringBuffer();

    sb.append( "<Request>" );

    if( getOperation( Operation.GETCAPABILITIES ) != null )
    {
      Marshallable operation = (Marshallable)getOperation( Operation.GETCAPABILITIES );
      sb.append( operation.exportAsXML() );
    }

    if( getOperation( Operation.GETMAP ) != null )
    {
      Marshallable operation = (Marshallable)getOperation( Operation.GETMAP );
      sb.append( operation.exportAsXML() );
    }

    if( getOperation( Operation.GETFEATUREINFO ) != null )
    {
      Marshallable operation = (Marshallable)getOperation( Operation.GETFEATUREINFO );
      sb.append( operation.exportAsXML() );
    }

    if( getOperation( Operation.DESCRIBELAYER ) != null )
    {
      Marshallable operation = (Marshallable)getOperation( Operation.DESCRIBELAYER );
      sb.append( operation.exportAsXML() );
    }

    if( getOperation( Operation.GETLEGENDGRAPHIC ) != null )
    {
      Marshallable operation = (Marshallable)getOperation( Operation.GETLEGENDGRAPHIC );
      sb.append( operation.exportAsXML() );
    }

    if( getOperation( Operation.GETSTYLES ) != null )
    {
      Marshallable operation = (Marshallable)getOperation( Operation.GETSTYLES );
      sb.append( operation.exportAsXML() );
    }

    if( getOperation( Operation.PUTSTYLES ) != null )
    {
      Marshallable operation = (Marshallable)getOperation( Operation.PUTSTYLES );
      sb.append( operation.exportAsXML() );
    }

    sb.append( "</Request>" );

    return sb.toString();
  }
}