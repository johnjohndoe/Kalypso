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

import org.deegree.filterencoding.Filter;

/**
 * A FeatureTypeConstraint element is used to identify a feature type by
 * well-known name, using the FeatureTypeName element.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public interface FeatureTypeConstraint
{

  /**
   * returns the name of the feature type
   * 
   * @return the name of the feature type
   */
  String getFeatureTypeName();

  /**
   * sets the name of the feature type
   * 
   * @param featureTypeName
   *          the name of the feature type
   */
  void setFeatureTypeName( String featureTypeName );

  /**
   * returns a feature-filter as defined in WFS specifications.
   * 
   * @return the filter of the FeatureTypeConstraints
   */
  Filter getFilter();

  /**
   * sets a feature-filter as defined in WFS specifications.
   * 
   * @param filter
   *          the filter of the FeatureTypeConstraints
   */
  void setFilter( Filter filter );

  /**
   * returns the extent for filtering the feature type
   * 
   * @return the extent for filtering the feature type
   */
  Extent[] getExtents();

  /**
   * sets the extent for filtering the feature type
   * 
   * @param extents
   *          extents for filtering the feature type
   */
  void setExtents( Extent[] extents );

  /**
   * adds an extent
   * 
   * @param extent
   *          an extent to add
   */
  void addExtent( Extent extent );

  /**
   * removes an extent
   * 
   * @param extent
   *          an extent to remove
   */
  void removeExtent( Extent extent );
}