/*----------------    FILE HEADER  ------------------------------------------
 
 This file is part of deegree (Java Framework for Geospatial Solutions).
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
package org.deegree.services.wcs.protocol;

import org.deegree.model.geometry.GM_Envelope;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.RangeParamList;

/**
 * defines the encapsulating of the WCS GetCoverage operation
 * <p>
 * method comments are taken from OGC WCS specification 0.7
 * <p>
 * ------------------------------------------------------------------------
 * </p>
 * 
 * @author Andreas Poth
 * @version 31.10.2002
 */
public interface WCSGetCoverageRequest extends OGCWebServiceRequest
{

  /**
   * The LAYER parameter requests a single coverage layer, identified in the
   * Capabilities XML by a LayerID element.
   */
  String getLayer();

  //    //ETJ removed
  //    /**
  //     * The RANGE parameter requests one or more of the range set components
  //     * defined on the selected coverage layer in the Capabilities XML. Its value
  //     * must match one or more RangeID elements under the selected coverage
  // layer.
  //     * <p>
  //     * If the selected coverage layer has only one range set defined, this
  //     * parameter is optional.
  //     */
  //    String[] getRange();

  /**
   * The SRS (Spatial Reference System) parameter is specified in the Basic
   * Service Elements section.
   * <p>
   * GetCoverage requests must use this parameter to specify the coordinate
   * reference system in which the query (BBOX) is expressed. The values of this
   * request parameter must be one of those defined in a SRS or QuerySRS/SRS
   * element under the requested coverage layer.
   */
  String getSRS();

  /**
   * This parameter specifies the coordinate system in which the coverage
   * response should be referenced. This parameter is optional; its value
   * defaults to that of SRS. Thus, omitting it requests a coverage response
   * referenced in the same coordinate reference system as the query (like WMS
   * and WFS).
   * <p>
   * The value of this request parameter must be one of those defined in a SRS
   * or ResponseSRS/SRS element defined under the requested coverage layer
   */
  String getResponseSRS();

  /**
   * GetCoverage queries must express spatial constraints in at least 2-D (XY)
   * coordinate space, using a BBOX parameter with comma-separated numbers
   * (minx, miny, maxx, maxy), expressed in the spatial reference system given
   * in the SRS parameter.
   * <p>
   * For any part of the coverage domain that is partly or entirely contained in
   * the Bounding Box defined by BBOX, the server should return coverage data in
   * the appropriate format.
   * <p>
   * In addition, if the Capabilities XML expresses the Layers domain as a 3-D
   * bounding box in a 3D coordinate reference system, then GetCoverage queries
   * may use an extended BBOX syntax (BBOX=minx,miny,maxx,maxy,minz,maxz), as
   * described in the Basic Services Model. These extra numbers are optional:
   * omitting minz,maxz requests the coverage layers default elevation (if
   * defined).
   */
  GM_Envelope getBoundingBox();

  /**
   * RangeParamList contains all the range parameter, including the special
   * ranges "time" and "elevation".
   * 
   * @return the RangeParamList
   * 
   * @author ETj
   */
  RangeParamList getRangeList();

  // ETj removed
  //    /**
  //     * If the Capabilities XML defines an ElevationExtent on the queried
  // coverage
  //     * layer, GetCoverage queries may use an ELEVATION parameter to constrain
  // the
  //     * request in elevation thus supplementing a 2D XY BBOX.<p>
  //     * The syntax for expressing Elevation constraints is specified in WMS 1.1
  //     * Annex C.
  //     */
  //    double[][] getElevation();

  // ETj removed
  //    /**
  //     * If the Capabilities XML defines a TemporalExtent on the queried coverage
  //     * layer, GetCoverage queries may use a TIME parameter to constrain the
  //     * request in time, thus supplementing a 2D XY or 3D XYZ BBOX.<p>
  //     * The syntax for expressing Time constraints and date / time values is
  //     * specified in WMS 1.1 Annexes B and C
  //     */
  //    TimeExtent getTime();

  // ETj removed
  //    /**
  //     * If one of the selected range set components consists of a compound
  // observable,
  //     * GetCoverage queries may constrain the request along the parameter(s) of
  //     * the compound observation. This is a variable parameter name, formed by
  //     * concatenating one of the selected range set components (given in the
  //     * RANGE parameter) with one of the RangeAxis names defined on that range
  // set
  //     * component, with a colon (:) in between.<p>
  //     * For example, in Capabilities XML a given coverage layer might define a
  //     * range set named radiance as a compound observable reported by wavelength
  //     * intervals. A GetCoverage request might limit the request to visible
  //     * wavelengths by specifying.<p>
  //     * <pre>
  //     * RANGE=radiance, and
  //     * RADIANCE:WAVELENGTH=650/700, 500/560, 430/500
  //     * </pre>
  //     * (assuming radiance is a compound variable tied to a RangeAxis called
  //     * wavelength that uses nanometers).<p>
  //     * If the selected coverage layer has only one range set defined, the RANGE
  //     * parameter is optional, as well as the corresponding parameter prefix. In
  //     * the previous example, if radiance is in fact the only range set component
  //     * defined on the selected coverage layer, then the constraint can be
  // expressed
  //     * simply as: <p>
  //     * WAVELENGTH=650/700, 500/560, 430/500
  //     */
  //    String[][] getParam(String range);

  /**
   * GetCoverage queries for gridded coverages may request coverage replies with
   * a specific grid size. The parameters WIDTH, HEIGHT, DEPTH define the grid
   * size (number of gridpoints or cells) along the three axes of the grid.
   * <p>
   * Either these parameters or RESX, RESY, RESZ are normally required for grid
   * coverage layers. However, if the Capabilities XML reports only the
   * Interpolation method None for the queried coverage layer, then GetCoverage
   * queries must be for the full native resolution of the data: they may not
   * use RESX, RESY, RESZ or WIDTH, HEIGHT, DEPTH to change the coverage
   * resolution. In this case, BBOX alone is used for subsetting.
   * <p>
   * If the coverage layer has elevation extents expressed separately from its
   * BoundingBox, then GetCoverage queries may use ELEVATION= as described
   * previously to request a particular extent and resolution.
   */
  int getWidth();

  /**
   * @see getWidth
   */
  int getHeight();

  /**
   * @see getWidth
   */
  int getDepth();

  /**
   * GetCoverage queries for gridded coverages may request coverage replies in
   * specific grid resolution. The parameters RESX and RESY define the grid-cell
   * size along the first and second axes of the coordinate reference system
   * given in SRS or RESPONSE_SRS.
   * <p>
   * If the RESPONSE_SRS is a 3D spatial reference system, then the additional
   * RESZ parameter may be used to specify the desired resolution along the
   * third axis of that spatial reference system.
   * <p>
   * Either these parameters or WIDTH, HEIGHT, DEPTH are normally required for
   * grid coverage layers. However, if the Capabilities XML reports only the
   * Interpolation method None for the queried coverage layer, then GetCoverage
   * queries must be for the full native resolution of the data: they may not
   * use RESX, RESY, RESZ or WIDTH, HEIGHT, DEPTH to change the coverage
   * resolution. In this case, BBOX alone is used for subsetting.
   * <p>
   * If the coveage layer has elevation extents expressed separately from its
   * BoundingBox, then GetCoverage queries may use ELEVATION= as described
   * previously to request a particular extent and resolution.
   */
  double getResX();

  /**
   * @see getResX
   */
  double getResY();

  /**
   * @see getResX
   */
  double getResZ();

  /**
   * The value of this parameter must be one of those listed in the Format
   * element of the desired coverage layer in Capabilities XML. The entire MIME
   * type string in the MIMEType sub-element is used as the value of the FORMAT
   * parameter. In an HTTP environment, the MIME type must be set on the
   * returned object using the Content-type entity header
   */
  String getFormat();

  /**
   * specifies what type of interpolation to use for resampling coverage values
   * over the spatial domain. This must be one of those listed for this coverage
   * layer in Capabilities XML. Options are "linear", "bilinear", "bicubic",
   * "lost area, "barycentric, "piecewise constant", and "none".
   */
  String getInterpolationMethod();

  /**
   * A Web Coverage Service must offer the exception reporting format
   * application/vnd.ogc.se_xml by listing it in a <Exceptions><Format>element
   * in its Capabilities XML. The entire MIME type string in <Format>is used as
   * the value of the EXCEPTIONS parameter.
   * <p>
   * Errors are reported using Service Exception XML, as specified in WMS 1.1
   * Section 6.7 and Annex A.3. This is the default exception format if none is
   * specified in the request.
   */
  String getExceptions();

}

