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
package org.deegree.services.wms.capabilities;

import org.deegree.graphics.sld.UserStyle;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.services.capabilities.MetadataURL;

/**
 * Each available map is advertised by a <Layer>element in the Capabilities XML.
 * A single parent Layer encloses any number of additional layers, which may be
 * hierarchically nested as desired. Some properties defined in a parent layer
 * are inherited by the children it encloses. These inherited properties may be
 * either redefined or added to by the child.
 * <p>
 * </p>
 * A Map Server shall include at least one <Layer>element for each map layer
 * offered. If desired, layers may be repeated in different categories when
 * relevant. No controlled vocabulary has been defined, so at present Layer and
 * Style Names, Titles and Keywords are arbitrary.
 * <p>
 * </p>
 * The <Layer>element can enclose child elements providing metadata about the
 * Layer.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 2002-03-01
 */
public interface Layer
{
  /**
   * If, and only if, a layer has a <Name>, then it is a map layer that can be
   * requested by using that Name in the LAYERS parameter of a GetMap request.
   * If the layer has a Title but no Name, then that layer is only a category
   * title for all the layers nested within. A Map Server that advertises a
   * Layer containing a Name element shall be able to accept that Name as the
   * value of LAYERS argument in a GetMap request and return the corresponding
   * map. A Client shall not attempt to request a layer that has a Title but no
   * Name.
   */
  String getName();

  /**
   * A <Title>is required for all layers; it is a human-readable string for
   * presentation in a menu. The Title is not inherited by child Layers.
   */
  String getTitle();

  /**
   * Abstract is a narrative description of the map layer. The Abstract elements
   * are not inherited by child Layers.
   */
  String getAbstract();

  /**
   * KeywordList contains zero or more Keywords to aid in catalog searches. The
   * KeywordList elements are not inherited by child Layers.
   */
  String[] getKeywordList();

  /**
   * Every Layer is available in one or more spatial reference systems Every
   * Layer shall have at least one <SRS>element that is either stated explicitly
   * or inherited from a parent Layer . The root <Layer>element shall include a
   * sequence of zero or more SRS elements listing all SRSes that are common to
   * all subsidiary layers. Use a single SRS element with empty content (like
   * so: "<SRS></SRS>") if there is no common SRS. Layers may optionally add
   * to the global SRS list, or to the list inherited from a parent layer. Any
   * duplication shall be ignored by clients. When a Layer is available in
   * several Spatial Reference Systems, there are two ways to encode the list of
   * SRS values. The first of these is new in this version of the specification,
   * the second is deprecated but still included for backwards compatibility.
   * <p>
   * </p>
   * 1. Optional, recommended: Multiple single-valued <SRS>elements: a list of
   * SRS values is represented as a sequence of <SRS>elements, each of which
   * contains only a single SRS name. Example: <SRS>EPSG:1234 </SRS>
   * <SRS>EPSG:5678 </SRS>.
   * <p>
   * </p>
   * 2. Deprecated: Single list-valued <SRS>element: a list of SRS values is
   * represented asa whitespace-separated list of SRS names contained within a
   * single <SRS>element. Example: <SRS>EPSG:1234 EPSG:5678 </SRS>.
   */
  String[] getSrs();

  /**
   * return s true if the submitted srs (name) is supported by the layer
   */
  boolean isSrsSupported( String srs );

  /**
   * Every Layer shall have exactly one <LatLonBoundingBox>element that is
   * either stated explicitly or inherited from a parent Layer.
   * LatLonBoundingBox states the minimum bounding rectangle of the map data in
   * the EPSG:4326 geographic coordinate system. The LatLonBoundingBox
   * attributes minx, miny, maxx, maxy indicate the edges of an enclosing
   * rectangle in decimal degrees. LatLonBoundingBox shall be supplied
   * regardless of what SRS the map server may support, but it may be
   * approximate if EPSG:4326 is not supported. Its purpose is to facilitate
   * geographic searches without requiring coordinate transformations by the
   * search engine.
   */
  GM_Envelope getLatLonBoundingBox();

  /**
   * Layers may have zero or more <BoundingBox>elements that are either stated
   * explicitly or inherited from a parent Layer. Each BoundingBox states the
   * bounding rectangle of the map data in a particular spatial reference
   * system; the attribute SRS indicates which SRS applies. If the data area is
   * shaped irregularly then the BoundingBox gives the minimum enclosing
   * rectangle. The attributes minx, miny, maxx, maxy indicate the edges of the
   * bounding box in units of the specified SRS. Optional resx and resy
   * attributes indicate the spatial resolution of the data in those same units.
   * <p>
   * </p>
   * A Layer may have multiple BoundingBox element, but each one shall state a
   * different SRS. A Layer inherits any BoundingBox values defined by its
   * parents. A BoundingBox inherited from the parent Layer for a particular SRS
   * is replaced by any declaration for the same SRS in the child Layer. A
   * BoundingBox in the child for a new SRS not already declared by the parent
   * is added to the list of bounding boxes for the child Layer. A single Layer
   * element shall not contain more than one BoundingBox for the same SRS.
   */
  LayerBoundingBox[] getBoundingBox();

  /**
   * Dimension declarations are inherited from parent Layers. Any new Dimension
   * declarations in the child are added to the list inherited from the parent.
   * A child shall not redefine a Dimension with the same name attribute as one
   * that was inherited.
   */
  Dimension[] getDimension();

  /**
   * Extent declarations are inherited from parent Layers. Any Extent
   * declarations in the child with the same name attribute as one inherited
   * from the parent replaces the value declared by the parent. A Layer shall
   * not declare an Extent unless a Dimension with the same name has been
   * declared or inherited earlier in the Capabilities XML.
   */
  Extent[] getExtent();

  /**
   * The optional <Attribution>element provides a way to identify the source of
   * the map data used in a Layer or collection of Layers. Attribution encloses
   * several optional elements: <OnlineResource>states the data provider's URL;
   * <Title>is a human-readable string naming the data provider; <LogoURL>is the
   * URL of a logo image. Client applications may choose to display one or more
   * of these items. A <Format>element in LogoURL indicates the MIME type of the
   * logo image, and the attributes width and height state the size of the image
   * in pixels.
   */
  Attribution getAttribution();

  /**
   * The authority attribute of the Identifier element corresponds to the name
   * attribute of a separate <AuthorityURL>element. AuthorityURL encloses an
   * <OnlineResource>element which states the URL of a document defining the
   * meaning of the Identifier values.
   */
  AuthorityURL[] getAuthorityURL();

  /**
   * A Map Server may use zero or more <Identifier>elements to list ID numbers
   * or labels defined by a particular Authority. The text content of the
   * Identifier element is the ID value.
   */
  Identifier[] getIdentifier();

  /**
   * A Map Server should use one or more <MetadataURL>elements to offer
   * detailed, standardized metadata about the data underneath a particular
   * layer. The type attribute indicates the standard to which the metadata
   * complies. Two types are defined at present: the value 'TC211' refers to
   * [ISO 19115]; the value 'FGDC' refers to [FGDC-STD-001-1988]. The
   * MetadataURL element shall not be used to reference metadata in a
   * non-standardized metadata format; see DataURL instead. The enclosed
   * <Format>element indicates the file format MIME type of the metadata record.
   */
  MetadataURL[] getMetadataURL();

  /**
   * A Map Server may use DataURL to offer more information about the data
   * represented by a particular layer. While the semantics are not
   * well-defined, as long as the results of an HTTP GET request against the
   * DataURL are properly MIME-typed, Viewer Clients and Cascading Map Servers
   * can make use of this. Use <MetadataURL>instead for a precisely defined
   * reference to standardized metadata records.
   * 
   * @return
   */
  DataURL[] getDataURL();

  /**
   * A Map Server may use a <FeatureListURL>element to point to a list of the
   * features represented in a Layer.
   * 
   * @return
   */
  FeatureListURL[] getFeatureListURL();

  /**
   * returns a list of style that can be used form rendering the layer.
   */
  Style[] getStyles();

  /**
   * returns the <tt>UserStyle</tt> (SLD) representation of the style
   * identified by the submitted name.
   * 
   * @param name
   *          of the requested style
   * @return SLD - UserStyle
   */
  UserStyle getStyle( String name );

  /**
   * returns the <tt>Style</tt> identified by the submitted name.
   * 
   * @param name
   *          of the requested style
   * @return Style
   *  
   */
  Style getStyleResource( String name );

  /**
   * Layers may include a <ScaleHint>element that suggests minimum and maximum
   * scales for which it is appropriate to display this layer. Because WMS
   * output is destined for output devices of arbitrary size and resolution, the
   * usual definition of scale as the ratio of map size to real-world size is
   * not appropriate here. The following definition of Scale Hint is
   * recommended. Consider a hypothetical map with a given Bounding Box, width
   * and height. The central pixel of that map (or the pixel just to the
   * northwest of center) will have some size, which can be expressed as the
   * ground distance in meters of the southwest to northeast diagonal of that
   * pixel. The two values in ScaleHint are the minimum and maximum recommended
   * values of that diagonal. It is recognized that this definition is not
   * geodetically precise, but at the same time the hope is that by including it
   * conventions will develop that can be later specified more clearly.
   */
  ScaleHint getScaleHint();

  /**
   * returns a list of layers the are enclosed by this layer.
   * 
   * @return
   */
  Layer[] getLayer();

  /**
   * source where the WMS can find the data of a layer.
   * 
   * @return
   */
  DataSource[] getDataSource();

  /**
   * source where the WMS can find the data of a layer that matches the
   * submitted scale. If no <tt>DataSource</tt> is defined that matches the
   * scale,</tt> null</tt> will be returned;
   * 
   * @return
   */
  DataSource getDataSource( double scale );

  /**
   * returns the parent layer of this layer. If the method returns <tt>null</tt>
   * the current layer is the root layer. In addition with the <tt>getLayer</tt>
   * method this enables a program to traverse the layer tree in both
   * directions.
   */
  Layer getParent();

  /**
   * returns true if the layer is queryable. That means it can be targeted by a
   * GetFeatureInfo request.
   */
  boolean isQueryable();

  /**
   * returns '0' if the layer is provided directly form the deegree WMS. other
   * it returns the number of cascaded WMS servers the is passed through
   */
  int getCascaded();

  /**
   * returns false if map data represents vector features that probably do not
   * completely fill space.
   */
  boolean isOpaque();

  /**
   * returns false if the WMS can map a subset of the full bounding box.
   */
  boolean hasNoSubsets();

  /**
   * returns '0' if the WMS can resize map to arbitrary width. nonzero: map has
   * a fixed width that cannot be changed by the WMS.
   */
  int getFixedWidth();

  /**
   * returns '0' if the WMS can resize map to arbitrary height. nonzero: map has
   * a fixed height that cannot be changed by the WMS.
   */
  int getFixedHeight();
}