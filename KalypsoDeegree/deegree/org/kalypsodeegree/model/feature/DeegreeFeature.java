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
package org.kalypsodeegree.model.feature;

import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;

/**
 * Features are, according to the Abstract Specification, digital representations of real world entities. Feature
 * Identity thus refers to mechanisms to identify such representations: not to identify the real world entities that are
 * the subject of a representation. Thus two different representations of a real world entity (say the Mississippi
 * River) will be two different features with distinct identities. Real world identification systems, such as title
 * numbers, while possibly forming a sound basis for an implementation of a feature identity mechanism, are not of
 * themselves such a mechanism.
 * 
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
public interface DeegreeFeature
{

  /**
   * returns the id of the Feature. the id has to be a name space that must be unique for each feature. use the adress
   * of the datasource in addition to a number for example .
   */
  String getId();

  /**
   * returns the FeatureType of this Feature
   */
  FeatureType getFeatureType();

  /**
   * returns the properties of the feature as array of Objects
   */
  Object[] getProperties();

  /**
   * returns the property of the feature that matches the submitted name
   */
  Object getProperty( String name );

  /**
   * returns the property of the feature that matches the submitted index
   */
  Object getProperty( int index );

  /**
   * returns all geometry properties of the feature. If no geometry could be found an <tt>GM_Object[]</tt> with zero
   * length will be returned.
   */
  GM_Object[] getGeometryProperties();

  /**
   * Returns the default geometry of the <tt>Feature</tt>.
   * 
   * @return default geometry or null, if the <tt>Feature</tt> has none
   */
  GM_Object getDefaultGeometryProperty();

  /**
   * sets the value for the submitted property. if no property with the submitted exists the property will be added
   */
  void setProperty( FeatureProperty property );

  /**
   * returns the envelope / boundingbox of the feature
   */
  GM_Envelope getEnvelope();

}