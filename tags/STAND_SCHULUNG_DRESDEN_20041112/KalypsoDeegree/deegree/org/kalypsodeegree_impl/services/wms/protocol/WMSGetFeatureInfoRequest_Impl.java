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
package org.deegree_impl.services.wms.protocol;

import java.awt.Point;
import java.util.ArrayList;
import java.util.HashMap;

import org.deegree.graphics.sld.StyledLayerDescriptor;
import org.deegree.services.WebServiceException;
import org.deegree.services.wms.protocol.WMSGetFeatureInfoRequest;
import org.deegree.services.wms.protocol.WMSGetMapRequest;
import org.deegree.xml.Marshallable;
import org.deegree_impl.services.OGCWebServiceRequest_Impl;

/**
 * 
 * 
 * <p>
 * --------------------------------------------------------
 * </p>
 * 
 * @author Katharina Lupp <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @version $Revision$ $Date$
 */
public class WMSGetFeatureInfoRequest_Impl extends OGCWebServiceRequest_Impl implements
    WMSGetFeatureInfoRequest, Marshallable
{
  private ArrayList queryLayers = null;

  private Point clickPoint = null;

  private String exceptions = null;

  private String infoFormat = null;

  private StyledLayerDescriptor sld = null;

  private WMSGetMapRequest getMapRequestCopy = null;

  private int featureCount = 1;

  /**
   * Creates a new WMSFeatureInfoRequest_Impl object.
   * 
   * @param version
   * @param id
   * @param queryLayers
   * @param getMapRequestCopy
   * @param infoFormat
   * @param featureCount
   * @param clickPoint
   * @param exceptions
   * @param sld
   * @param vendorSpecificParameter
   */
  WMSGetFeatureInfoRequest_Impl( String version, String id, String[] queryLayers,
      WMSGetMapRequest getMapRequestCopy, String infoFormat, int featureCount, Point clickPoint,
      String exceptions, StyledLayerDescriptor sld, HashMap vendorSpecificParameter )
  {
    super( "FeatureInfo", "WMS", version, id, vendorSpecificParameter );
    this.queryLayers = new ArrayList();
    setQueryLayers( queryLayers );
    setGetMapRequestCopy( getMapRequestCopy );
    setGetMapRequestCopy( getMapRequestCopy );
    setFeatureCount( featureCount );
    setClickPoint( clickPoint );
    setExceptions( exceptions );
    setStyledLayerDescriptor( sld );
    setInfoFormat( infoFormat );
  }

  /**
   * <map request copy> is not a name/value pair like the other parameters.
   * Instead, most of the GetMap request parameters that generated the original
   * map are repeated. Two are omitted because GetFeatureInfo provides its own
   * values: VERSION and REQUEST. The remainder of the GetMap request shall be
   * embedded contiguously in the GetFeatureInfo request.
   */
  public WMSGetMapRequest getGetMapRequestCopy()
  {
    return getMapRequestCopy;
  }

  /**
   * sets the <GetMapRequestCopy>
   */
  public void setGetMapRequestCopy( WMSGetMapRequest getMapRequestCopy )
  {
    this.getMapRequestCopy = getMapRequestCopy;
  }

  /**
   * The required QUERY_LAYERS parameter states the map layer(s) from which
   * feature information is desired to be retrieved. Its value is a comma-
   * separated list of one or more map layers that are returned as an array.
   * This parameter shall contain at least one layer name, but may contain fewer
   * layers than the original GetMap request.
   * <p>
   * </p>
   * If any layer in this list is not contained in the Capabilities XML of the
   * WMS, the results are undefined and the WMS shall produce an exception
   * response.
   */
  public String[] getQueryLayers()
  {
    return (String[])queryLayers.toArray( new String[queryLayers.size()] );
  }

  /**
   * adds the <QueryLayers>
   */
  public void addQueryLayers( String queryLayers )
  {
    this.queryLayers.add( queryLayers );
  }

  /**
   * sets the <QueryLayers>
   */
  public void setQueryLayers( String[] queryLayers )
  {
    this.queryLayers.clear();

    if( queryLayers != null )
    {
      for( int i = 0; i < queryLayers.length; i++ )
      {
        this.queryLayers.add( queryLayers[i] );
      }
    }
  }

  /**
   * The optional INFO_FORMAT indicates what format to use when returning the
   * feature information. Supported values for a GetFeatureInfo request on a WMS
   * instance are listed as MIME types in one or more <Format>elements inside
   * the <Request><FeatureInfo>element of its Capabilities XML. The entire MIME
   * type string in <Format>is used as the value of the INFO_FORMAT parameter.
   * In an HTTP environment, the MIME type shall be set on the returned object
   * using the Content-type entity header.
   * <p>
   * </p>
   * <b>EXAMPLE: </b> <tt> The parameter INFO_FORMAT=application/vnd.ogc.gml
   * requests that the feature information be formatted in Geography Markup
   * Language (GML).</tt>
   */
  public String getInfoFormat()
  {
    return infoFormat;
  }

  /**
   * sets the <InfoFormat>
   */
  public void setInfoFormat( String infoFormat )
  {
    this.infoFormat = infoFormat;
  }

  /**
   * The optional FEATURE_COUNT parameter states the maximum number of features
   * for which feature information should be returned. Its value is a positive
   * integer greater than zero. The default value is 1 if this parameter is
   * omitted.
   */
  public int getFeatureCount()
  {
    return featureCount;
  }

  /**
   * sets the <FeatureCount>
   */
  public void setFeatureCount( int featureCount )
  {
    this.featureCount = featureCount;
  }

  /**
   * The required X and Y parameters indicate a point of interest on the map. X
   * and Y identify a single point within the borders of the WIDTH and HEIGHT
   * parameters of the embedded GetMap request. The origin is set to (0,0)
   * centered in the pixel at the upper left corner; X increases to the right
   * and Y increases downward. X and Y are retruned as java.awt.Point
   * class/datastructure.
   */
  public Point getClickPoint()
  {
    return clickPoint;
  }

  /**
   * sets the <ClickPoint>
   */
  public void setClickPoint( Point clickPoint )
  {
    this.clickPoint = clickPoint;
  }

  /**
   * The optional EXCEPTIONS parameter states the manner in which errors are to
   * be reported to the client. The default value is application/vnd.ogc.se_xml
   * if this parameter is absent from the request. At present, not other values
   * are defined for the WMS GetFeatureInfo request.
   */
  public String getExceptions()
  {
    return exceptions;
  }

  /**
   * sets the <Exception>
   */
  public void setExceptions( String exceptions )
  {
    this.exceptions = exceptions;
  }

  /**
   * returns the SLD the request is made of. This implies that a 'simple' HTTP
   * GET-Request will be transformed into a valid SLD. This is mandatory within
   * a JaGo WMS.
   * <p>
   * </p>
   * This mean even if a GetMap request is send using the HTTP GET method, an
   * implementing class has to map the request to a SLD data sructure.
   */
  public StyledLayerDescriptor getStyledLayerDescriptor()
  {
    return sld;
  }

  /**
   * sets the SLD the request is made of. This implies that a 'simple' HTTP
   * GET-Request or a part of it will be transformed into a valid SLD. For
   * convenience it is asumed that the SLD names just a single layer to generate
   * display elements of.
   */
  public void setStyledLayerDescriptor( StyledLayerDescriptor sld )
  {
    this.sld = sld;
  }

  /**
   *  
   */
  public String exportAsXML()
  {
    StringBuffer sb = new StringBuffer();
    sb.append( "<GetFeatureInfo>" ).append( "<X>" ).append( clickPoint.x );
    sb.append( "</X><Y>" ).append( clickPoint.y ).append( "</Y>" );
    sb.append( "<Info_Format>" ).append( infoFormat ).append( "</Info_Format>" );
    for( int i = 0; i < queryLayers.size(); i++ )
    {
      String layer = (String)queryLayers.get( i );
      sb.append( "<NamedLayer><Name>" ).append( layer );
      sb.append( "</Name></NamedLayer>" );
    }
    sb.append( ( (Marshallable)getMapRequestCopy ).exportAsXML() );
    sb.append( "</GetFeatureInfo>" );
    return sb.toString();
  }

  public String toString()
  {
    return exportAsXML();
  }

  /**
   * returns the parameter of a HTTP GET request.
   *  
   */
  public String getRequestParameter() throws WebServiceException
  {
    // indicates if the request parameters are decoded as SLD. deegree won't
    // perform SLD requests through HTTP GET
    if( ( getMapRequestCopy.getBoundingBox() == null ) || ( queryLayers.size() == 0 ) )
    {
      throw new WebServiceException( "Request can't be expressed as HTTP GET request " );
    }

    StringBuffer sb = new StringBuffer( "service=WMS" );

    if( getVersion().compareTo( "1.0.0" ) <= 0 )
    {
      sb.append( "&VERSION=" + getVersion() + "&REQUEST=feature_info" );
      sb.append( "&TRANSPARENT=" + getMapRequestCopy.getTransparency() );
    }
    else
    {
      sb.append( "&VERSION=" + getVersion() + "&REQUEST=GetFeatureInfo" );
      sb.append( "&TRANSPARENCY=" + getMapRequestCopy.getTransparency() );
    }

    sb.append( "&WIDTH=" + getMapRequestCopy.getWidth() );
    sb.append( "&HEIGHT=" + getMapRequestCopy.getHeight() );
    sb.append( "&FORMAT=" + getMapRequestCopy.getFormat() );
    sb.append( "&EXCEPTIONS=" + getExceptions() );
    sb.append( "&BGCOLOR=" + getMapRequestCopy.getBGColorAsHex() );
    sb.append( "&SRS=" + getMapRequestCopy.getSrs() );
    sb.append( "&BBOX=" + getMapRequestCopy.getBoundingBox().getMin().getX() );
    sb.append( "," + getMapRequestCopy.getBoundingBox().getMin().getY() );
    sb.append( "," + getMapRequestCopy.getBoundingBox().getMax().getX() );
    sb.append( "," + getMapRequestCopy.getBoundingBox().getMax().getY() );

    WMSGetMapRequest.Layer[] layers = getMapRequestCopy.getLayers();
    String l = "";
    String s = "";

    for( int i = 0; i < layers.length; i++ )
    {
      l += ( layers[i].getName() + "," );
      s += ( layers[i].getStyleName() + "," );
    }

    l = l.substring( 0, l.length() - 1 );
    s = s.substring( 0, s.length() - 1 );
    sb.append( "&LAYERS=" + l );
    sb.append( "&STYLES=" + s );

    double[] elevation = getMapRequestCopy.getElevation();

    if( elevation != null )
    {
      s = "";

      for( int i = 0; i < elevation.length; i++ )
      {
        s += ( elevation[i] + "/" );
      }

      if( elevation.length > 0 )
      {
        s = s.substring( 0, s.length() - 1 );
        sb.append( "&Elevation=" + s );
      }
    }

    String time = getMapRequestCopy.getTime();

    if( ( time != null ) && ( time.length() > 0 ) )
    {
      sb.append( "&TIME=" + time );
    }

    String[] qlayers = getQueryLayers();
    String ql = "";

    for( int i = 0; i < qlayers.length; i++ )
    {
      ql += ( qlayers[i] + "," );
    }

    ql = ql.substring( 0, ql.length() - 1 );
    sb.append( "&QUERY_LAYERS=" + ql );
    sb.append( "&FEATURE_COUNT=" + getFeatureCount() );
    sb.append( "&INFO_FORMAT=" + getInfoFormat() );
    sb.append( "&X=" + clickPoint.x );
    sb.append( "&Y=" + clickPoint.y );

    return sb.toString();
  }

}