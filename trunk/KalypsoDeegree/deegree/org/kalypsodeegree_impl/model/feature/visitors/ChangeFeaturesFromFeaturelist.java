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

import java.util.Map;
import java.util.Properties;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Ändert die Features einer {@link org.kalypsodeegree.model.feature.FeatureList}anhand der besuchten Features. Dazu
 * wird für jedes besuchte {@link org.kalypsodeegree.model.feature.Feature}ein passendes aus der Liste anhand einer
 * vorgegebenen zuordnung gesucht. Dann werden die Daten des besuchten Features via
 * {@link org.kalypsodeegree_impl.model.feature.FeatureHelper#copyProperties(Feature, Feature, Properties)}übertragen.
 * 
 * @author bce
 */
public class ChangeFeaturesFromFeaturelist implements FeatureVisitor
{
  private final Properties m_propertyMap;

  private final String m_sourceID;

  private Map m_index;

  public ChangeFeaturesFromFeaturelist( final FeatureList list, final Properties propertyMap, final String sourceID,
      final String targetID )
  {
    m_propertyMap = propertyMap;
    m_sourceID = sourceID;

    // index anhand der targetid erstellen
    final IndexFeaturesVisitor indexer = new IndexFeaturesVisitor( targetID );
    list.accept( indexer );
    m_index = indexer.getIndex();
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    final Object index = f.getProperty( m_sourceID);
    final Feature targetFeature = (Feature)m_index.get( index );
    if( targetFeature != null )
    {
      try
      {
        FeatureHelper.copyProperties( f, targetFeature, m_propertyMap );
      }
      catch( Exception e )
      {
        // TODO error handling
        e.printStackTrace();
      }
    }

    return true;
  }
}