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

import java.awt.Point;

import org.deegree.graphics.sld.StyledLayerDescriptor;
import org.deegree.services.OGCWebServiceRequest;

/**
 * 
 * 
 * @version $Revision$
 * @author $author$
 */
public interface WMSGetFeatureInfoRequest extends OGCWebServiceRequest
{
  /**
   * <map request copy> is not a name/value pair like the other parameters.
   * Instead, most of the GetMap request parameters that generated the original
   * map are repeated. Two are omitted because GetFeatureInfo provides its own
   * values: VERSION and REQUEST. The remainder of the GetMap request shall be
   * embedded contiguously in the GetFeatureInfo request.
   */
  WMSGetMapRequest getGetMapRequestCopy();

  /**
   * 
   * 
   * @return
   */
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
  String[] getQueryLayers();

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
  String getInfoFormat();

  /**
   * The optional FEATURE_COUNT parameter states the maximum number of features
   * for which feature information should be returned. Its value is a positive
   * integer greater than zero. The default value is 1 if this parameter is
   * omitted.
   */
  int getFeatureCount();

  /**
   * The required X and Y parameters indicate a point of interest on the map. X
   * and Y identify a single point within the borders of the WIDTH and HEIGHT
   * parameters of the embedded GetMap request. The origin is set to (0,0)
   * centered in the pixel at the upper left corner; X increases to the right
   * and Y increases downward. X and Y are retruned as java.awt.Point
   * class/datastructure.
   */
  Point getClickPoint();

  /**
   * The optional EXCEPTIONS parameter states the manner in which errors are to
   * be reported to the client. The default value is application/vnd.ogc.se_xml
   * if this parameter is absent from the request. At present, not other values
   * are defined for the WMS GetFeatureInfo request.
   */
  String getExceptions();

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
}