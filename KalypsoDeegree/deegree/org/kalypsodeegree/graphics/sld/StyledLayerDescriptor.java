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
package org.deegree.graphics.sld;

/**
 * StyledLayerDescriptor: This is a sequence of styled layers, represented at
 * the first level by Layer and UserLayer elements. A "version" attribute has
 * been added to allow the formatting of static-file
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
   * A UserLayer can contain one or more UserStyles. A UserLayer may direct the
   * WMS to a specified WFS source of feature data. Multiple feature types can
   * be included in a UserLayer, since this is semantically equivalent to a
   * Layer. All feature types of a UserLayer come from the same WFS. The WFS can
   * be named explicitly with the "wfs" attribute or it can be implied by
   * context.
   * 
   * @return the UserLayers as Array
   */
  UserLayer[] getUserLayers();

  /**
   * A NamedLayer uses the "name" attribute to identify a layer known to the WMS
   * and can contain zero or more styles, either NamedStyles or UserStyles. In
   * the absence of any styles the default style for the layer is used.
   * 
   * @return the NamedLayers as Array
   */
  NamedLayer[] getNamedLayers();

  /**
   * The version attribute gives the SLD version of an SLD document, to
   * facilitate backward compatibility with static documents stored in various
   * different versions of the SLD spec. The string has the format x.y.z, the
   * same as in other OpenGIS Web Server specs. For example, an SLD document
   * stored according to this spec would have the version string 0.7.2.
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
}