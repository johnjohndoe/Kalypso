/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.model.feature.visitors;

import java.util.ArrayList;
import java.util.Collection;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;

/**
 * <p>
 * Simply collects all visited features
 * </p>
 * 
 * @author belger
 */
public class CollectorVisitor implements FeatureVisitor
{
  private final Collection<Feature> m_results = new ArrayList<Feature>();

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public synchronized boolean visit( final Feature f )
  {
    m_results.add( f );
    return true;
  }

  /**
   * Returns alle visited features.
   * <p>
   * IMPORTANT: this method has been synchronized since toggel between radio buttons cause a
   * ArrayIndexOutOfBoundsException in the first line.
   * </p>
   * 
   * @param reset
   *          if true, resets the inner result set, so next call to getResults results in empty array.
   */
  public synchronized Feature[] getResults( final boolean reset )
  {
    final Feature[] features = m_results.toArray( new Feature[0] );
    if( reset )
      m_results.clear();
    return features;
  }
}
