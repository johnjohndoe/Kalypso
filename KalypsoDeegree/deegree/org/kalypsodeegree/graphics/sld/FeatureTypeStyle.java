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
package org.deegree.graphics.sld;

/**
 * The FeatureTypeStyle defines the styling that is to be applied to a single
 * feature type of a layer). This element may also be externally re-used outside
 * of the scope of WMSes and layers.
 * <p>
 * </p>
 * The FeatureTypeStyle element identifies that explicit separation in SLD
 * between the handling of layers and the handling of features of specific
 * feature types. The layer concept is unique to WMS and SLD, but features are
 * used more generally, such as in WFS and GML, so this explicit separation is
 * important.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public interface FeatureTypeStyle
{

  /**
   * The Name element does not have an explicit use at present, though it
   * conceivably might be used to reference a feature style in some
   * feature-style library.
   * 
   * @return name
   */
  String getName();

  /**
   * The Name element does not have an explicit use at present, though it
   * conceivably might be used to reference a feature style in some
   * feature-style library. Sets the <Name>
   * 
   * @param name
   *          the name
   */
  void setName( String name );

  /**
   * human-readable information about the style
   * 
   * @return the title of the FeatureTypeStyle
   */
  String getTitle();

  /**
   * sets the <Title>
   * 
   * @param title
   *          the title of the FeatureTypeStyle
   */
  void setTitle( String title );

  /**
   * human-readable information about the style
   * 
   * @return an abstract of the FeatureTypeStyle
   */
  String getAbstract();

  /**
   * human-readable information about the style
   * 
   * @param abstract_
   *          an abstract of the FeatureTypeStyle
   */
  void setAbstract( String abstract_ );

  /**
   * returns the name of the affected feature type
   * 
   * @return the name of the FeatureTypeStyle as String
   */
  String getFeatureTypeName();

  /**
   * sets the name of the affected feature type
   * 
   * @param featureTypeName
   *          the name of the FeatureTypeStyle
   */
  void setFeatureTypeName( String featureTypeName );

  /**
   * The SemanticTypeIdentifier is experimental and is intended to be used to
   * identify what the feature style is suitable to be used for using community-
   * controlled name(s). For example, a single style may be suitable to use with
   * many different feature types. The syntax of the SemanticTypeIdentifier
   * string is undefined, but the strings generic:line, generic:polygon,
   * generic:point, generic:text, generic:raster, and generic:any are reserved
   * to indicate that a FeatureTypeStyle may be used with any feature type with
   * the corresponding default geometry type (i.e., no feature properties are
   * referenced in the feature-type style).
   * 
   * @return the SemanticTypeIdentifiers from the FeatureTypeStyle as
   *         String-Array
   */
  String[] getSemanticTypeIdentifier();

  /**
   * Sets the SemanticTypeIdentifiers.
   * 
   * @param semanticTypeIdentifiers
   *          SemanticTypeIdentifiers for the FeatureTypeStyle
   */
  void setSemanticTypeIdentifier( String[] semanticTypeIdentifiers );

  /**
   * Adds a SemanticTypeIdentifier.
   * 
   * @param semanticTypeIdentifier
   *          SemanticTypeIdentifier to add
   */
  void addSemanticTypeIdentifier( String semanticTypeIdentifier );

  /**
   * Removes a SemanticTypeIdentifier.
   * 
   * @param semanticTypeIdentifier
   *          SemanticTypeIdentifier to remove
   */
  void removeSemanticTypeIdentifier( String semanticTypeIdentifier );

  /**
   * Rules are used to group rendering instructions by feature-property
   * conditions and map scales. Rule definitions are placed immediately inside
   * of feature-style definitions.
   * 
   * @return the rules of the FeatureTypeStyle as Array
   */
  Rule[] getRules();

  /**
   * Sets the Rules.
   * 
   * @param rules
   *          the rules of the FeatureTypeStyle as Array
   */
  void setRules( Rule[] rules );

  /**
   * Adds a Rule.
   * 
   * @param rule
   *          a rule
   */
  void addRule( Rule rule );

  /**
   * Removes a Rule.
   * 
   * @param rule
   *          a rule
   */
  void removeRule( Rule rule );
}