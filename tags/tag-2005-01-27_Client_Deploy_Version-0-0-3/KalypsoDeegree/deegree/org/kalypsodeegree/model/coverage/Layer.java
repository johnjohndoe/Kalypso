/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

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

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 
 history:
  
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
     
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
---------------------------------------------------------------------------------------------------*/
package org.deegree.model.coverage;

import java.net.URL;

import org.deegree.model.geometry.GM_Envelope;

/**
 * The Layer(Type), is meant to be shared with the Web Map Service and Web
 * Feature Service and defines access basic to common elements.
 * 
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @author Andreas Poth
 * @version $Revision$ $Date$
 *          <p>
 */
public interface Layer
{

  /**
   * The required LayerID is a unique identifier (not used for any other
   * coverage layer). Using the GetCoverage or DescribeCoverage operation,
   * clients request coverages from that Coverage Layer (or a description of the
   * Coverage Layer) by using this LayerID value in the LAYERS parameter (for
   * requests expressed as key-value pairs) or in the LayerID element (for XML
   * requests).
   */
  String getLayerID();

  /**
   * The required Title element contains a human-readable string for p
   * resentation in a menu.
   */
  String getTitle();

  /**
   * returns a narrative description of the map layer
   */
  String getAbstract();

  /**
   * returns a list that contains keywords to aid in catalog searches
   */
  String[] getKeywordList();

  /**
   * The optional MetadataURL element is recommended for access to detailed,
   * standardized metadata about the data underneath a particular layer. The
   * type attribute indicates the standard to which the metadata complies. Two
   * types are defined at present: 'TC211' = ISO TC211 19115; 'FGDC' = FGDC
   * Content Standard for Digital Geospatial Metadata
   */
  URL[] getMetadataURLs();

  /**
   * The required LatLonBoundingBox element, has attributes indicating the edges
   * of an enclosing rectangle in longitude/latitude decimal degrees.
   * LatLonBoundingBox must be supplied regardless of what SRS the map server
   * may support for coverage requests and responses. However, it may be
   * approximate if EPSG:4326 (WGS84 geographic) is not a supported request SRS.
   * Its chief purpose is to populate registries for geographic search
   */
  GM_Envelope getLatLonBoundingBox();

  /**
   * For each Coverage Layer, the Capabilities XML description specifies (i) the
   * native Spatial Reference System (SRS) of the data; (ii) the SRSs in which
   * it understands incoming GetCoverage requests; and (iii) the SRSs in which
   * it can produce coverages in response to GetCoverage requests
   * <p>
   * Every CoverageLayer must have either one or more SRS elements, or both a
   * QuerySRS and a ResponseSRS element (which contain one or more SRS
   * elements). A server may choose to detail query and response SRSs
   * separately, or just advertise SRSs in which it can both accept requests and
   * deliver coverage responses
   * <p>
   * The content of the SRS element may be any of the EPSG: or AUTO: coordinate
   * systems defined in the Web Map Service Implementation Specification; it may
   * be undefined (�OGC:none�); or it may be an embedded �swath� referencing
   * system (�OGC:swath�) that lets the client reconstruct ground coordinates
   * from tie-points or sensor metadata
   */
  String[] getCRS();

  /**
   * The QuerySRS element states the SRS(s) in which GetCoverage requests may be
   * expressed against that coverage layer. One of these should be the coverage
   * layer�s native SRS.
   * <p>
   * Requests expressed in the special SRS codes �OGC:none� or �OGC:swath� refer
   * to subsets defined according the layer�s pixel row/column coordinate system
   * (for imagery) or its internal / local coordinate system (for non-gridded
   * data).
   */
  String[] getQueryCRS();

  /**
   * The ResponseSRS element states the SRS(s) in which coverage replies to
   * GetCoverage requests may be expressed. One of these should be the Layer�s
   * native SRS. Coverages served in the special SRS codes �OGC:none� or
   * �OGC:swath� are expressed in pixel row/column coordinate system (for
   * imagery) or an internal / local coordinate system (for non-gridded data).
   * Coverage replies with the SRS �OGC:swath� may embed sensor-model or
   * tie-point information to let a thick client georeference the returned
   * coverage
   */
  String[] getResponseCRS();

  /**
   * The optional NativeSRS element states the native SRS of a Coverage Layer.
   * (This, along with the native resolution stated in the BoundingBox,
   * facilitates client access to unretouched coverage values.
   */
  String getNativeCRS();

  /**
   * The required SupportedFormatList element advertises the output format(s) in
   * which coverages may be requested from this Coverage Layer (e.g., GeoTIFF,
   * HDF-EOS, NITF, DTED, etc.). Both a format name and a MIME type identify
   * these formats. Several OGC-specific types have been defined to distinguish
   * various types of XML documents; these are listed in WMS 1.1.
   */
  Format[] getSupportedFormatList();
}