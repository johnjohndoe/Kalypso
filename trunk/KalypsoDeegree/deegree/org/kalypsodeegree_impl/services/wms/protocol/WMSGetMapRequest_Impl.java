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

import java.awt.Color;
import java.net.URL;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.StringTokenizer;

import org.deegree.graphics.sld.StyledLayerDescriptor;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.services.WebServiceException;
import org.deegree.services.wms.protocol.WMSGetMapRequest;
import org.deegree.xml.Marshallable;
import org.deegree_impl.services.OGCWebServiceRequest_Impl;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.MimeTypeMapper;
import org.deegree_impl.tools.NetWorker;
import org.deegree_impl.tools.StringExtend;

/**
 * This interface describes the access to the parameters of a GeMap request. It
 * is excpected that there are two kinds of request. The first is the 'normal'
 * HTTP GET request with name-value-pair enconding and the second is a HTTP POST
 * request containing a SLD.
 * <p>
 * </p>
 * Even it is possible to access the values of a HTTP GET request throught their
 * bean accessor methods the request shall be mapped to a SLD data structure
 * that is accessible using the <tt>getSLD()</tt>.
 * <p>
 * --------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 2002-03-01
 */
public class WMSGetMapRequest_Impl extends OGCWebServiceRequest_Impl implements WMSGetMapRequest,
    Marshallable
{
  private ArrayList elevation = null;

  private ArrayList layers = null;

  private ArrayList sampleDimension = null;

  private Color bGColor = null;

  private GM_Envelope boundingBox = null;

  private String exceptions = null;

  private String format = null;

  private String srs = null;

  private String time = null;

  private StyledLayerDescriptor sld = null;

  private URL sLD_URL = null;

  private URL wFS_URL = null;

  private boolean transparency = false;

  private int height = 0;

  private int width = 0;

  /**
   * Creates a new WMSGetMapRequest_Impl object.
   * 
   * @param version
   * @param layers
   * @param elevation
   * @param sampleDimension
   * @param format
   * @param width
   * @param height
   * @param srs
   * @param boundingBox
   * @param transparency
   * @param bGColor
   * @param exceptions
   * @param time
   * @param sLD_URL
   * @param wFS_URL
   * @param sld
   * @param id
   * @param vendorSpecificParameter
   */
  WMSGetMapRequest_Impl( String version, String id, Layer[] layers, double[] elevation,
      String[] sampleDimension, String format, int width, int height, String srs,
      GM_Envelope boundingBox, boolean transparency, Color bGColor, String exceptions, String time,
      URL sldURL, StyledLayerDescriptor sld, HashMap vendorSpecificParameter )
  {
    super( "GetMap", "WMS", version, id, vendorSpecificParameter );

    this.layers = new ArrayList();
    this.elevation = new ArrayList();
    this.sampleDimension = new ArrayList();

    setLayers( layers );
    setStyledLayerDescriptor( sld );
    setElevation( elevation );
    setSampleDimension( sampleDimension );
    setFormat( format );
    setWidth( width );
    setHeight( height );
    setSrs( srs );
    setBoundingBox( boundingBox );
    setTransparency( transparency );
    setBGColor( bGColor );
    setExceptions( exceptions );
    setTime( time );
    setSLD_URL( sldURL );
    // setWFS_URL( wFS_URL );
  }

  /**
   * The FORMAT parameter specifies the output format of the response to an
   * operation.
   * <p>
   * </p>
   * An OGC Web Service may offer only a subset of the formats known for that
   * type of operation, but the server shall advertise in its Capabilities XML
   * those formats it does support and shall accept requests for any format it
   * advertises. A Service Instance may optionally offer a new format not
   * previously offered by other instances, with the recognition that clients
   * are not required to accept or process an unknown format. If a request
   * contains a Format not offered by a particular server, the server shall
   * throw a Service Exception (with code "InvalidFormat").
   * 
   * @return the output format
   */
  public String getFormat()
  {
    return format;
  }

  /**
   * sets the format
   * 
   * @param format
   *          the requested output-format
   */
  public void setFormat( String format )
  {
    this.format = format;
  }

  /**
   * The required LAYERS parameter lists the map layer(s) to be returned by this
   * GetMap request. The value of the LAYERS parameter is a comma-separated list
   * of one or more valid layer names. Allowed layer names are the character
   * data content of any <Layer><Name>element in the Capabilities XML.
   * <p>
   * </p>
   * A WMS shall render the requested layers by drawing the leftmost in the list
   * bottommost, the next one over that, and so on.
   * <p>
   * </p>
   * Each layer is associated to a style. Styles are also is encoded as a comma-
   * seperated list within the GetMap request.
   * <p>
   * </p>
   * The required STYLES parameter lists the style in which each layer is to be
   * rendered. There is a one-to-one correspondence between the values in the
   * LAYERS parameter and the values in the STYLES parameter. Because of this
   * layer-style combinations are returned coupled within an array of Layer-
   * objects. Each map in the list of LAYERS is drawn using the corresponding
   * style in the same position in the list of STYLES. Each style Name shall be
   * one that was defined in the <Name>element of a <Style>element that is
   * either directly contained within, or inherited by, the associated <Layer>
   * element in Capabilities XML.
   * 
   * @return The required LAYERS
   */
  public Layer[] getLayers()
  {
    return (Layer[])layers.toArray( new Layer[layers.size()] );
  }

  /**
   * adds the <Layer>
   */
  public void addLayers( Layer layers )
  {
    this.layers.add( layers );
  }

  /**
   * sets the <Layer>
   * 
   * @param layers
   *          a set of layer
   */
  public void setLayers( Layer[] layers )
  {
    this.layers.clear();

    if( layers != null )
    {
      for( int i = 0; i < layers.length; i++ )
      {
        this.layers.add( layers[i] );
      }
    }
  }

  /**
   * The required SRS parameter states which Spatial Reference System applies to
   * the values in the BBOX parameter. The value of the SRS parameter shall be
   * one of the values defined in the character data section of an <SRS>element
   * defined or inherited by the requested layer. The same SRS applies to all
   * layers in a single request.
   * <p>
   * </p>
   * If the WMS server has declared SRS=NONE for a Layer, as discussed in the
   * Basic Service Elements section, then the Layer does not have a well-defined
   * spatial reference system and should not be shown in conjunction with other
   * layers. The Client shall specify SRS=NONE (case-insensitive) in the GetMap
   * request and the Server may issue a Service Exception otherwise.
   * 
   * @return the spatial reference system
   */
  public String getSrs()
  {
    return srs;
  }

  /**
   * sets the srs
   * 
   * @param srs
   *          the spatial reference system
   */
  public void setSrs( String srs )
  {
    this.srs = srs;
  }

  /**
   * The required BBOX parameter allows a Client to request a particular
   * Bounding Box. Bounding Boxes are defined in the Basic Service Elements
   * section. The value of the BBOX parameter in a GetMap request is a list of
   * comma-separated numbers of the form "minx,miny,maxx,maxy".
   * <p>
   * </p>
   * If the WMS server has declared that a Layer is not subsettable then the
   * Client shall specify exactly the declared Bounding Box values in the GetMap
   * request and the Server may issue a Service Exception otherwise.
   */
  public GM_Envelope getBoundingBox()
  {
    return boundingBox;
  }

  /**
   * sets the <BoundingBox>
   */
  public void setBoundingBox( GM_Envelope boundingBox )
  {
    this.boundingBox = boundingBox;
  }

  /**
   * WIDTH specifies the number of pixels to be used between the minimum and
   * maximum X values (inclusive) in the BBOX parameter. The returned picture,
   * regardless of its return format, shall have exactly the specified width and
   * height in pixels. In the case where the aspect ratio of the BBOX and the
   * ratio width/height are different, the WMS shall stretch the returned map so
   * that the resulting pixels could themselves be rendered in the aspect ratio
   * of the BBOX. In other words, it should be possible using this definition to
   * request a map for a device whose output pixels are themselves non-square,
   * or to stretch a map into an image area of a different aspect ratio.
   */
  public int getWidth()
  {
    return width;
  }

  /**
   * sets width
   */
  public void setWidth( int width )
  {
    this.width = width;
  }

  /**
   * HEIGHT specifies the number of pixels between the minimum and maximum Y
   * values. The returned picture, regardless of its return format, shall have
   * exactly the specified width and height in pixels. In the case where the
   * aspect ratio of the BBOX and the ratio width/height are different, the WMS
   * shall stretch the returned map so that the resulting pixels could
   * themselves be rendered in the aspect ratio of the BBOX. In other words, it
   * should be possible using this definition to request a map for a device
   * whose output pixels are themselves non-square, or to stretch a map into an
   * image area of a different aspect ratio.
   */
  public int getHeight()
  {
    return height;
  }

  /**
   * sets height
   */
  public void setHeight( int height )
  {
    this.height = height;
  }

  /**
   * The optional TRANSPARENT parameter specifies whether the map background is
   * to be made transparent or not. TRANSPARENT can take on two values, "TRUE"
   * or "FALSE". The default value is FALSE if this parameter is absent from the
   * request.
   * <p>
   * </p>
   * The ability to return pictures drawn with transparent pixels allows results
   * of different Map requests to be overlaid, producing a composite map. It is
   * strongly recommended that every WMS offer a format that provides
   * transparency for layers which could sensibly be overlaid above others.
   */
  public boolean getTransparency()
  {
    return transparency;
  }

  /**
   * sets <Transparency>
   */
  public void setTransparency( boolean transparency )
  {
    this.transparency = transparency;
  }

  /**
   * The optional BGCOLOR parameter specifies the color to be used as the
   * background of the map. The general format of BGCOLOR is a hexadecimal
   * encoding of an RGB value where two hexadecimal characters are used for each
   * of Red, Green, and Blue color values. The values can range between 00 and
   * FF for each (0 and 255, base 10). The format is 0xRRGGBB; either upper or
   * lower case characters are allowed for RR, GG, and BB values. The "0x"
   * prefix shall have a lower case 'x'. The default value is 0xFFFFFF
   * (corresponding to the color white) if this parameter is absent from the
   * request.
   */
  public Color getBGColor()
  {
    return bGColor;
  }

  /**
   * transforms the color of the request from java.awt.Color to the hexadecimal
   * representation as in an OGC conform WMS-GetMap request (e.g. white ==
   * "0xffffff").
   * 
   * @return the color as hexadecimal representation
   */
  public String getBGColorAsHex()
  {
    String r = Integer.toHexString( bGColor.getRed() );
    if( r.length() < 2 )
      r = "0" + r;
    String g = Integer.toHexString( bGColor.getGreen() );
    if( g.length() < 2 )
      g = "0" + g;
    String b = Integer.toHexString( bGColor.getBlue() );
    if( b.length() < 2 )
      b = "0" + b;

    return "0x" + r + g + b;
  }

  /**
   * sets <BGColor>
   * 
   * @param the
   *          requested color
   */
  public void setBGColor( Color bGColor )
  {
    this.bGColor = bGColor;
  }

  /**
   * The optional EXCEPTIONS parameter states the manner in which errors are to
   * be reported to the client. The default value is application/vnd.ogc.se_xml
   * if this parameter is absent from the request.
   * <p>
   * </p>
   * A Web Map Service shall offer one or more of the following exception
   * reporting formats by listing them in separate <Format>elements inside the
   * <Exceptions>element of its Capabilities XML. The entire MIME type string in
   * <Format>is used as the value of the EXCEPTIONS parameter. The first of
   * these formats is required to be offered by every WMS; the others are
   * optional.
   */
  public String getExceptions()
  {
    return exceptions;
  }

  /**
   * sets <Exceptions>
   */
  public void setExceptions( String exceptions )
  {
    this.exceptions = exceptions;
  }

  /**
   * This specification is based on [ISO 8601:1988(E)]; it extends ISO 8601 in
   * the following ways:
   * <UL>
   * <li>It defines a syntax for expressing the start, end and periodicity of a
   * data collection.
   * <li>It defines terms to represent the 7 days of the week.
   * <li>It allows years before 0001 AD.
   * <li>It allows times in the distant geologic past (thousands, millions or
   * billions of years before present).
   * </UL>
   */
  public String getTime()
  {
    return time;
  }

  /**
   * sets the <Time>
   */
  public void setTime( String time )
  {
    this.time = time;
  }

  /**
   * Some geospatial information may be available at multiple elevations. An OWS
   * may announce available elevations in its Capabilities XML, and some
   * operations include a parameter for requesting a particular elevation. A
   * single elevation value is an integer or real number whose units are
   * declared by naming an EPSG datum. When providing elevation information,
   * Servers should declare a default value in Capabilities XML unless there is
   * compelling reason to behave otherwise, and Servers shall respond with the
   * default value if one has been declared and the Client request does not
   * include a value.
   */
  public double[] getElevation()
  {
    Debug.debugMethodBegin( "WMSGetMapRequest", "getElevation" );

    Double[] d = (Double[])elevation.toArray( new Double[elevation.size()] );
    double[] dd = new double[d.length];

    for( int i = 0; i < d.length; i++ )
    {
      dd[i] = d[i].doubleValue();
    }

    Debug.debugMethodEnd();
    return dd;
  }

  /**
   * adds the <Elevation>
   */
  public void addElevation( double elevation )
  {
    this.elevation.add( new Double( elevation ) );
  }

  /**
   * sets the <Elevation>
   */
  public void setElevation( double[] elevation )
  {
    this.elevation.clear();

    if( elevation != null )
    {
      for( int i = 0; i < elevation.length; i++ )
      {
        this.elevation.add( new Double( elevation[i] ) );
      }
    }
  }

  /**
   * Some geospatial information may be available at other dimensions (for
   * example, satellite images in different wavelength bands). The dimensions
   * other than the four space-time dimensions are referred to as "sample
   * dimensions". An OWS may announce available sample dimensions in its
   * Capabilities XML, and some operations include a mechanism for including
   * dimensional parameters. Each sample dimension has a Name and one or more
   * valid values.
   */
  public String[] getSampleDimension()
  {
    return (String[])sampleDimension.toArray( new String[sampleDimension.size()] );
  }

  /**
   * adds the <SampleDimension>
   */
  public void addSampleDimension( String sampleDimension )
  {
    this.sampleDimension.add( sampleDimension );
  }

  /**
   * sets the <SampleDimension>
   */
  public void setSampleDimension( String[] sampleDimension )
  {
    this.sampleDimension.clear();

    if( sampleDimension != null )
    {
      for( int i = 0; i < sampleDimension.length; i++ )
      {
        this.sampleDimension.add( sampleDimension[i] );
      }
    }
  }

  /**
   * URL of Styled Layer Descriptor (as defined in SLD Specification). This
   * parameter is optional. If no sld URL is defined <tt>null</tt> will be
   * returned.
   */
  public URL getSLD_URL()
  {
    return sLD_URL;
  }

  /**
   * sets <SLD_URL>
   */
  public void setSLD_URL( URL sLD_URL )
  {
    this.sLD_URL = sLD_URL;
  }

  /**
   * URL of Web Feature Service providing features to be symbolized using SLD.
   * This parameter is optional. If no WFS URL is defined <tt>null</tt> will
   * be returned.
   */
  public URL getWFS_URL()
  {
    return wFS_URL;
  }

  /**
   * sets <WFS_URL>
   */
  public void setWFS_URL( URL wFS_URL )
  {
    this.wFS_URL = wFS_URL;
  }

  /**
   * returns the SLD the request is made of. This implies that a 'simple' HTTP
   * GET-Request will be transformed into a valid SLD. This is mandatory within
   * a JaGo WMS.
   * <p>
   * </p>
   * This mean even if a GetMap request is send using the HTTP GET method, an
   * implementing class has to map the request to a SLD data structure.
   */
  public StyledLayerDescriptor getStyledLayerDescriptor()
  {
    return sld;
  }

  /**
   * sets the SLD the request is made of. This implies that a 'simple' HTTP
   * GET-Request will be transformed into a valid SLD. This is mandatory within
   * a JaGo WMS.
   * <p>
   * </p>
   * This mean even if a GetMap request is send using the HTTP GET method, an
   * implementing class has to map the request to a SLD data sructure.
   */
  public void setStyledLayerDescriptor( StyledLayerDescriptor sld )
  {
    this.sld = sld;
  }

  /**
   * returns the parameter of a HTTP GET request.
   *  
   */
  public String getRequestParameter() throws WebServiceException
  {
    Debug.debugMethodBegin( "WMSGetMapRequest", "getRequestParameter" );

    // indicates if the request parameters are decoded as SLD. deegree won't
    // perform SLD requests through HTTP GET
    if( boundingBox == null )
    {
      throw new WebServiceException( "Request can't be expressed as HTTP GET request " );
    }

    StringBuffer sb = new StringBuffer( "SERVICE=WMS" );

    if( getVersion().compareTo( "1.0.0" ) <= 0 )
    {
      sb.append( "&VERSION=" + getVersion() + "&REQUEST=map" );
      String f = StringExtend.replace( getFormat(), "image/", "", false );
      try
      {
        sb.append( "&FORMAT=" + URLEncoder.encode( f, "ISO-8859-1" ) );
      }
      catch( Exception e )
      {}
    }
    else
    {
      sb.append( "&VERSION=" + getVersion() + "&REQUEST=GetMap" );
      try
      {
        sb.append( "&FORMAT=" + URLEncoder.encode( getFormat(), "ISO-8859-1" ) );
      }
      catch( Exception e )
      {}
    }

    sb.append( "&TRANSPARENT=" + getTransparency() );
    sb.append( "&WIDTH=" + getWidth() );
    sb.append( "&HEIGHT=" + getHeight() );
    sb.append( "&EXCEPTIONS=" + getExceptions() );
    sb.append( "&BGCOLOR=" + getBGColorAsHex() );
    sb.append( "&BBOX=" + boundingBox.getMin().getX() );
    sb.append( "," + boundingBox.getMin().getY() );
    sb.append( "," + boundingBox.getMax().getX() );
    sb.append( "," + boundingBox.getMax().getY() );

    Layer[] layers = getLayers();
    String l = "";
    String s = "";

    if( sLD_URL == null )
    {
      for( int i = 0; i < layers.length; i++ )
      {
        try
        {
          l += ( URLEncoder.encode( layers[i].getName(), "ISO-8859-1" ) + "," );
          s += ( URLEncoder.encode( layers[i].getStyleName(), "ISO-8859-1" ) + "," );
        }
        catch( Exception e )
        {
          throw new WebServiceException( e.toString() );
        }
      }

      l = l.substring( 0, l.length() - 1 );
      s = s.substring( 0, s.length() - 1 );
      sb.append( "&LAYERS=" + l );
      sb.append( "&STYLES=" + s );
    }
    else if( sLD_URL != null )
    {
      sb.append( "&SLD=" + NetWorker.url2String( sLD_URL ) );
    }
    else if( sld != null )
    {
      String tmp = ( (Marshallable)sld ).exportAsXML();
      try
      {
        tmp = URLEncoder.encode( tmp, "ISO-8859-1" );
      }
      catch( Exception e )
      {
        throw new WebServiceException( e.toString() );
      }
      sb.append( "&SLD_BODY=" + tmp );
    }

    sb.append( "&SRS=" + getSrs() );

    double[] elevation = getElevation();

    if( elevation != null && elevation.length > 0 )
    {
      s = "";

      for( int i = 0; i < elevation.length; i++ )
      {
        s += ( elevation[i] + "/" );
      }

      s = s.substring( 0, s.length() - 1 );
      sb.append( "&ELEVATION=" + s );
    }

    if( ( time != null ) && ( time.length() > 0 ) )
    {
      sb.append( "&TIME=" + time );
    }

    if( vendorSpecificParameter != null )
    {
      Iterator iterator = vendorSpecificParameter.keySet().iterator();
      while( iterator.hasNext() )
      {
        String key = (String)iterator.next();
        String value = (String)vendorSpecificParameter.get( key );
        try
        {
          value = URLEncoder.encode( value, "ISO-8859-1" );
        }
        catch( Exception e )
        {}
        sb.append( "&" + key + "=" + value );
      }
    }

    Debug.debugMethodEnd();
    return sb.toString();
  }

  /**
   * exports the request in its XML representation. May look like this:
   * 
   * <pre>
   *
   *
   */
  public String exportAsXML()
  {
    Debug.debugMethodBegin( this, "exportAsXML" );

    StringBuffer sb = new StringBuffer( 5000 );

    sb.append( "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" );

    // REQUEST (hard-coded)
    sb.append( "<!DOCTYPE GetMap SYSTEM \"http://some.site.com/wms/GetMap.xsd\">" );
    sb.append( "<ogc:GetMap xmlns:ogc=\"http://www.opengis.net/ogc\" "
        + "xmlns:gml=\"http://www.opengis.net/gml\" "
        + "env:encodingStyle=\"http://www.w3.org/2001/09/soap-encoding\" " );

    // VERSION
    sb.append( "version=\"" + getVersion() + "\" " );
    sb.append( "service=\"WMS\">" );

    if( getStyledLayerDescriptor() != null )
    {
      //sb.append( ((Marshallable)getStyledLayerDescriptor()).exportAsXML() );
    }

    for( int i = 0; i < layers.size(); i++ )
    {
      org.deegree.services.wms.protocol.WMSGetMapRequest.Layer layer = (org.deegree.services.wms.protocol.WMSGetMapRequest.Layer)layers
          .get( i );
      sb.append( "<NamedLayer><Name>" ).append( layer.getName() );
      sb.append( "</Name><NamedStyle><Name>" ).append( layer.getStyleName() );
      sb.append( "</Name></NamedStyle></NamedLayer>" );
    }

    // SRS
    StringTokenizer st = new StringTokenizer( getSrs(), ":" );
    String praefix = st.nextToken();
    String suffix = st.nextToken();

    // BBOX
    sb.append( "<BoundingBox srsName=\"http://www.opengis.net/gml/srs/" + praefix.toLowerCase()
        + ".xml#" + suffix + "\">" );
    sb.append( "<gml:coordinates cs=\",\" decimal=\".\" ts=\" \">" );
    sb.append( getBoundingBox().getMin().getX() + "," + getBoundingBox().getMin().getY() );
    sb.append( " " + getBoundingBox().getMax().getX() + "," + getBoundingBox().getMax().getX() );
    sb.append( "</gml:coordinates>" );
    sb.append( "</BoundingBox>" );

    // TIME
    if( getTime() != null )
    {
      sb.append( "<Time>" + getTime() + "</Time>" );
    }

    // ELEVATION
    if( ( getElevation() != null ) & ( getElevation().length > 0 ) )
    {
      double[] elevationarr = getElevation();
      sb.append( "<Elevation>" );

      for( int i = 0; i < ( elevationarr.length - 1 ); i++ )
      {
        sb.append( elevationarr[i] + "/" );
      }

      sb.append( ( elevationarr.length - 1 ) + "</Elevation>" );
    }

    // OUTPUT
    sb.append( "<Output>" );
    sb.append( "<Format>" );
    sb.append( MimeTypeMapper.toMimeType( getFormat() ) );
    sb.append( "</Format>" );

    // TRANSPARENT
    sb.append( "<Transparent>" + getTransparency() + "</Transparent>" );

    // SIZE = WIDTH + HEIGHT
    sb.append( "<Size>" );
    sb.append( "<Width>" + getWidth() + "</Width>" );
    sb.append( "<Height>" + getHeight() + "</Height>" );
    sb.append( "</Size>" );

    // BGCOLOR
    sb.append( "<BGColor>" + getBGColorAsHex() + "</BGColor>" );

    sb.append( "</Output>" );

    // SAMPLE DIMENSION
    if( ( getSampleDimension() != null ) & ( getSampleDimension().length > 0 ) )
    {
      String[] sdarr = getSampleDimension();
      sb.append( "<Dimension>" );

      for( int i = 0; i < ( sdarr.length - 1 ); i++ )
      {
        sb.append( sdarr[i] + "," );
      }

      sb.append( sdarr.length - 1 + "</Dimension>" );
    }

    // EXCEPTIONS
    sb.append( "<Exceptions>" + MimeTypeMapper.toMimeType( getExceptions() ) );
    sb.append( "</Exceptions>" );

    sb.append( "</ogc:GetMap>" );
    Debug.debugMethodEnd();

    return sb.toString();
  }

  public String toString()
  {
    String s = "" + this.getClass().getName();
    try
    {
      s = getRequestParameter();
    }
    catch( Exception e )
    {}
    return s;
  }

  /**
   * creates a Layer object beacuse of the inner class construct.
   * 
   * @param name
   *          the name of the layer
   * @param style
   *          the corresponding style of the layer
   * @return Layer a layer object constaining name and style
   */
  public static Layer createLayer( String name, String style )
  {
    return new Layer_Impl( name, style );
  }

  /**
   * A Layer object. It contains the name of the layer and the corresponding
   * style.
   * 
   * @version $Revision$
   * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
   */
  private static class Layer_Impl implements Layer
  {
    private String name = null;

    private String styleName = null;

    /**
     * default constructor
     */
    public Layer_Impl()
    {}

    /**
     * constructor initializing the class with the <Layer>
     */
    public Layer_Impl( String name, String styleName )
    {
      setName( name );
      setStyleName( styleName );
    }

    /**
     * returns the <Name>
     */
    public String getName()
    {
      return name;
    }

    /**
     * sets the <Name>
     */
    public void setName( String name )
    {
      this.name = name;
    }

    /**
     * returns the <StyleName>
     */
    public String getStyleName()
    {
      return styleName;
    }

    /**
     * sets the <StyleName>
     */
    public void setStyleName( String styleName )
    {
      this.styleName = styleName;
    }

  }
}