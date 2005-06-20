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
package org.kalypsodeegree.graphics.sld;

/**
 * StyledLayerDescriptor: This is a sequence of styled layers, represented at the first level by Layer and UserLayer
 * elements. A "version" attribute has been added to allow the formatting of static-file
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public interface StyledLayerDescriptor
{

  /**
   * 
   * @return the Layers as Array
   */
  Layer[] getLayers();

  /**
   * Sets Layers
   * 
   * @param layers
   *          the Layers as Array
   */
  void setLayers( Layer[] layers );

  /**
   * Adds a Layer
   * 
   * @param Layer
   *          a Layer to add
   */
  void addLayer( Layer Layer );

  /**
   * Removes a Layer
   * 
   * @param Layer
   *          a Layer to remvoe
   */
  void removeLayer( Layer Layer );

  /**
   * A UserLayer can contain one or more UserStyles. A UserLayer may direct the WMS to a specified WFS source of feature
   * data. Multiple feature types can be included in a UserLayer, since this is semantically equivalent to a Layer. All
   * feature types of a UserLayer come from the same WFS. The WFS can be named explicitly with the "wfs" attribute or it
   * can be implied by context.
   * 
   * @return the UserLayers as Array
   */
  UserLayer[] getUserLayers();

  /**
   * A NamedLayer uses the "name" attribute to identify a layer known to the WMS and can contain zero or more styles,
   * either NamedStyles or UserStyles. In the absence of any styles the default style for the layer is used.
   * 
   * @return the NamedLayers as Array
   */
  NamedLayer[] getNamedLayers();

  /**
   * The version attribute gives the SLD version of an SLD document, to facilitate backward compatibility with static
   * documents stored in various different versions of the SLD spec. The string has the format x.y.z, the same as in
   * other OpenGIS Web Server specs. For example, an SLD document stored according to this spec would have the version
   * string 0.7.2.
   * 
   * @return the version of the SLD as String
   */
  String getVersion();

  /**
   * Sets the Version.
   * 
   * @param version
   *          the version of the SLD
   */
  void setVersion( String version );

  /** Finds a user style by its name */
  public UserStyle findUserStyle( final String name );
}