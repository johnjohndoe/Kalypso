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
package org.deegree.services.wms.protocol;

import java.awt.Color;
import java.net.URL;

import org.deegree.graphics.sld.StyledLayerDescriptor;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.services.OGCWebServiceRequest;

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
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 2002-03-01
 */
public interface WMSGetMapRequest extends OGCWebServiceRequest
{
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
   */
  String getFormat();

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
   */
  Layer[] getLayers();

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
   */
  String getSrs();

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
  GM_Envelope getBoundingBox();

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
  int getWidth();

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
  int getHeight();

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
  boolean getTransparency();

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
  Color getBGColor();

  /**
   * transforms the color of the request from java.awt.Color to the hexadecimal
   * representation as in an OGC conform WMS-GetMap request (e.g. white ==
   * "0xffffff").
   * 
   * @return the color as hexadecimal representation
   */
  String getBGColorAsHex();

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
  String getExceptions();

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
  String getTime();

  /**
   * 
   * 
   * @return
   */
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
  double[] getElevation();

  /**
   * 
   * 
   * @return
   */
  /**
   * Some geospatial information may be available at other dimensions (for
   * example, satellite images in different wavelength bands). The dimensions
   * other than the four space-time dimensions are referred to as "sample
   * dimensions". An OWS may announce available sample dimensions in its
   * Capabilities XML, and some operations include a mechanism for including
   * dimensional parameters. Each sample dimension has a Name and one or more
   * valid values.
   */
  String[] getSampleDimension();

  /**
   * URL of Styled Layer Descriptor (as defined in SLD Specification). This
   * parameter is optional. If no sld URL is defined <tt>null</tt> will be
   * returned.
   */
  URL getSLD_URL();

  /**
   * URL of Web Feature Service providing features to be symbolized using SLD.
   * This parameter is optional. If no WFS URL is defined <tt>null</tt> will
   * be returned.
   */
  URL getWFS_URL();

  /**
   * returns the SLD the request is made of. This implies that a 'simple' HTTP
   * GET-Request will be transformed into a valid SLD. This is mandatory within
   * a JaGo WMS.
   * <p>
   * </p>
   * This mean even if a GetMap request is send using the HTTP GET method, an
   * implementing class has to map the request to a SLD data sructure.
   */
  StyledLayerDescriptor getStyledLayerDescriptor();

  /**
   * 
   * 
   * @version $Revision$
   * @author $author$
   */
  public interface Layer
  {
    /**
     * 
     * 
     * @return
     */
    String getName();

    /**
     * 
     * 
     * @return
     */
    String getStyleName();
  }
}