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
 * A user-defined allows map styling to be defined externally from a system and
 * to be passed around in an interoperable format.
 * <p>
 * </p>
 * A UserStyle is at the same semantic level as a NamedStyle used in the context
 * of a WMS. In a sense, a named style can be thought of as a reference to a
 * hidden UserStyle that is stored inside of a map server.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public interface UserStyle extends Style
{

  /**
   * The Title is a human-readable short description for the style that might be
   * displayed in a GUI pick list.
   * 
   * @return the title of the User-Style
   */
  String getTitle();

  /**
   * Sets the title.
   * 
   * @param title
   *          the title of the User-Style
   */
  void setTitle( String title );

  /**
   * the Abstract is a more exact description that may be a few paragraphs long.
   * 
   * @return the abstract of the User-Style
   */
  String getAbstract();

  /**
   * Sets the Abstract.
   * 
   * @param abstract_
   *          the abstract of the User-Style
   */
  void setAbstract( String abstract_ );

  /**
   * The IsDefault element identifies whether a style is the default style of a
   * layer, for use in SLD library mode when rendering or for storing inside of
   * a map server. The default value is <tt>false</tt>.
   * 
   * @return true if the style ist the default style
   */
  boolean isDefault();

  /**
   * sets the <Default>
   * 
   * @param default_
   */
  void setDefault( boolean default_ );

  /**
   * A UserStyle can contain one or more FeatureTypeStyles which allow the
   * rendering of features of specific types.
   * <p>
   * </p>
   * The FeatureTypeStyle defines the styling that is to be applied to a single
   * feature type of a layer.
   * <p>
   * </p>
   * The FeatureTypeStyle element identifies that explicit separation in SLD
   * between the handling of layers and the handling of features of specific
   * feature types. The layer concept is unique to WMS and SLD, but features are
   * used more generally, such as in WFS and GML, so this explicit separation is
   * important.
   * 
   * @return the FeatureTypeStyles of a User-Style
   */
  FeatureTypeStyle[] getFeatureTypeStyles();

  /**
   * Sets FeatureTypeStyles
   * 
   * @param featureTypeStyles
   *          the FeatureTypeStyles of a User-Style
   */
  void setFeatureTypeStyles( FeatureTypeStyle[] featureTypeStyles );

  /**
   * Adds a <FeatureTypeStyle>
   * 
   * @param featureTypeStyle
   *          a FeatureTypeStyle to add
   */
  void addFeatureTypeStyle( FeatureTypeStyle featureTypeStyle );

  /**
   * Removes a <FeatureTypeStyle>
   * 
   * @param featureTypeStyle
   *          a FeatureTypeStyle to remove
   */
  void removeFeatureTypeStyle( FeatureTypeStyle featureTypeStyle );
}