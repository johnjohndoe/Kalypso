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

import java.net.URL;

import org.deegree.services.OGCWebService;
import org.deegree.services.wcs.protocol.WCSGetCoverageRequest;
import org.deegree.services.wfs.protocol.WFSQuery;
import org.deegree.services.wms.capabilities.DataSource;
import org.deegree.services.wms.capabilities.ScaleHint;
import org.deegree.services.wms.protocol.WMSGetMapRequest;
import org.deegree.xml.Marshallable;
import org.deegree.xml.XMLTools;
import org.deegree_impl.tools.NetWorker;

/**
 * name of the data source where the WMS can find the data of a layer. the
 * filterServiceClassName element identifies the filter servive that's
 * responsible for accessing the data.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$
 */
public class DataSource_Impl implements DataSource, Marshallable
{
  private OGCWebService ows = null;

  private URL capabilitiesURL;

  private ScaleHint scaleHint = null;

  private String geometryProperty = null;

  private String name = null;

  private WCSGetCoverageRequest getCoverage = null;

  private WFSQuery query = null;

  private WMSGetMapRequest getMap = null;

  private int type = 0;

  /**
   * Creates a new DataSource_Impl object.
   * 
   * @param name
   * @param type
   * @param ows
   * @param scaleHint
   */
  private DataSource_Impl( String name, int type, String geometryProperty, OGCWebService ows,
      URL capabilitiesURL, ScaleHint scaleHint )
  {
    this.scaleHint = scaleHint;
    this.name = name;
    this.type = type;
    this.geometryProperty = geometryProperty;
    this.ows = ows;
    this.capabilitiesURL = capabilitiesURL;
  }

  /**
   * Creates a new DataSource_Impl object.
   * 
   * @param name
   *          name of the featuretype to access
   * @param type
   *          type of the data source (LOCALWFS, REMOTEWFS)
   * @param ows
   *          <tt>OGCWebService</tt> instance for accessing the data source
   * @param scaleHint
   * @param query
   *          filter condition
   */
  DataSource_Impl( String name, int type, String geometryProperty, OGCWebService ows,
      URL capabilitiesURL, ScaleHint scaleHint, WFSQuery query )
  {
    this( name, type, geometryProperty, ows, capabilitiesURL, scaleHint );
    this.query = query;
  }

  /**
   * Creates a new DataSource_Impl object. Because only a REMOTEWMS can handle a
   * <tt>WMSGetMapRequest</tt> no type must be submitted.
   * 
   * @param name
   *          name of the featuretype to access
   * @param ows
   *          <tt>OGCWebService</tt> instance for accessing the data source
   * @param scaleHint
   * @param getMap
   *          filter condition
   */
  DataSource_Impl( String name, String geometryProperty, OGCWebService ows, URL capabilitiesURL,
      ScaleHint scaleHint, WMSGetMapRequest getMap )
  {
    this( name, DataSource.REMOTEWMS, geometryProperty, ows, capabilitiesURL, scaleHint );
    this.getMap = getMap;
  }

  /**
   * Creates a new DataSource_Impl object.
   * 
   * @param name
   *          name of the featuretype to access
   * @param type
   *          type of the data source (REMOTEWCS, LOCALWCS)
   * @param ows
   *          <tt>OGCWebService</tt> instance for accessing the data source
   * @param scaleHint
   * @param getCoverage
   *          filter condition
   */
  DataSource_Impl( String name, int type, String geometryProperty, OGCWebService ows,
      URL capabilitiesURL, ScaleHint scaleHint, WCSGetCoverageRequest getCoverage )
  {
    this( name, type, geometryProperty, ows, capabilitiesURL, scaleHint );
    this.getCoverage = getCoverage;
  }

  /**
   * returns the scale interval the data source is valid
   */
  public ScaleHint getScaleHint()
  {
    return scaleHint;
  }

  /**
   * returns the name of the data source. The method may returns <tt>null</tt>
   * if so no name is defined and a online resource or WFS filter have shall be
   * returned.
   */
  public String getName()
  {
    return name;
  }

  /**
   * returns an instance of the <tt>OGCWebService</tt> that represents the
   * datasource. Notice: if more than one layer uses data that are offered by
   * the same OWS the deegree WMS shall just use one instance for accessing the
   * OWS
   *  
   */
  public OGCWebService getOGCWebService()
  {
    return ows;
  }

  /**
   * returns the WFS Query that describes the access/filtering to the data
   * source.
   */
  public WFSQuery getQuery()
  {
    return query;
  }

  /**
   * returns an instance of a <tt>WCSGetCoverageRequest</tt> encapsulating the
   * filter conditions against a remote WCS. The request object contains:
   * VERSION, LAYER, FORMAT, VENDORSPECIFICPARAMETERS
   * 
   * @return filter conditions
   *  
   */
  public WCSGetCoverageRequest getGetCoverageRequest()
  {
    return getCoverage;
  }

  /**
   * returns an instance of a <tt>WMSGetMapRequest</tt> encapsulating the
   * filter conditions against a remote WMS. The request object contains:
   * WMTVER, LAYERS, STYLES, FORMAT, TRANSPARENT, VENDORSPECIFICPARAMETERS
   * 
   * @return filter conditions
   *  
   */
  public WMSGetMapRequest getGetMapRequest()
  {
    return getMap;
  }

  /**
   * returns the type of the data source. possible values are:
   * <ul>
   * <li>LOCALWFS</li>
   * <li>LOCALWCS</li>
   * <li>REMOTEWFS</li>
   * <li>REMOTEWCS</li>
   * <li>REMOTEWMS</li>
   * </ul>
   * the values are defined as constants in <tt>DataSource</tt>
   * 
   * @return
   */
  public int getType()
  {
    return type;
  }

  /**
   * Returns the name of the geometry property in case the datasource is of type
   * LOCALWFS / REMOTEWFS.
   * <p>
   * 
   * @return name of the geometry property
   */
  public String getGeometryProperty()
  {
    return geometryProperty;
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = getClass().getName() + ":\n";
    ret += ( "scaleHint = " + scaleHint + "\n" );
    ret += ( "name = " + name + "\n" );
    ret += ( "type = " + type + "\n" );
    return ret;
  }

  /**
   * Returns an XML representation of this object.
   */
  public String exportAsXML()
  {
    StringBuffer sb = new StringBuffer();

    sb.append( "<DataSource>" ).append( "<Name>" ).append( XMLTools.validateCDATA( name ) ).append(
        "</Name>" ).append( "<GeometryProperty>" ).append(
        XMLTools.validateCDATA( geometryProperty ) ).append( "</GeometryProperty>" ).append(
        "<Type>" );

    switch( type )
    {
    case LOCALWCS:
    {
      sb.append( "LOCALWCS" );
      break;
    }
    case LOCALWFS:
    {
      sb.append( "LOCALWFS" );
      break;
    }
    case REMOTEWMS:
    {
      sb.append( "REMOTEWMS" );
      break;
    }
    case REMOTEWCS:
    {
      sb.append( "REMOTEWCS" );
      break;
    }
    case REMOTEWFS:
    {
      sb.append( "REMOTEWFS" );
      break;
    }
    }

    sb.append( "</Type>" );

    sb.append( "<OWSCapabilities>" ).append( "<OnlineResource " ).append(
        "xmlns:xlink=\"http://www.w3.org/1999/xlink\" " ).append(
        "xlink:type=\"simple\" xlink:href=\"" ).append( NetWorker.url2String( capabilitiesURL ) )
        .append( "\"/>" ).append( "</OWSCapabilities>" );

    if( scaleHint != null )
    {
      sb.append( ( (Marshallable)scaleHint ).exportAsXML() );
    }

    sb.append( "</DataSource>" );

    return sb.toString();
  }
}