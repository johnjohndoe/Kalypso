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

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Fügt neue Features in eine {@link org.kalypsodeegree.model.feature.FeatureList}ein. Dabei wird für jedes besuchte
 * Feature ein neues (Default) Feature erzeugt in welches Werte aus dem besuchten Feature via
 * {@link org.kalypsodeegree_impl.model.feature.FeatureHelper#copyProperties(Feature, Feature, Properties)}übertragen
 * werden.
 * 
 * @author bce
 */
public class AddFeaturesToFeaturelist implements FeatureVisitor
{
  private final FeatureList m_list;

  private final Properties m_propertyMap;

  private final IFeatureType m_featureType;

  private final String m_fromID;

  private final Map m_idHash;

  private final String m_fid;

  private Map m_fidHash;

  private static final String REPLACE_COUNT = "${count}";

  private final String m_handleExisting;

  /**
   * @param fid
   *          Anhand dieses Pattern werden die neuen FeatureIds erzeugt. Es wird ein Pattern-Replace mit folgenden
   *          Variablen erzeugt:
   *          <ul>
   *          <li>fromID</li>
   *          <li>toID</li>
   *          <li>fID</li>
   *          <li>count</li>
   *          </ul>
   */
  public AddFeaturesToFeaturelist( final FeatureList list, final Properties propertyMap, final IFeatureType featureType, final String fromID, final String toID, final String handleExisting, final String fid )
  {
    m_list = list;
    m_featureType = featureType;
    m_propertyMap = propertyMap;
    m_fromID = fromID;
    m_handleExisting = handleExisting;
    m_fid = fid;

    // create index for toID
    final IndexFeaturesVisitor visitor = new IndexFeaturesVisitor( toID );
    m_list.accept( visitor );
    m_idHash = visitor.getIndex();

    final IndexFeaturesVisitor fidVisitor = new IndexFeaturesVisitor( null );
    m_list.accept( fidVisitor );
    m_fidHash = fidVisitor.getIndex();
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    final Object fromID = f.getProperty( m_fromID );

    final Feature existingFeature = (Feature) m_idHash.get( fromID );
    final String fid = createID( existingFeature, fromID );

    final Feature newFeature;
    if( existingFeature == null || "overwrite".equals( m_handleExisting ) )
      newFeature = FeatureFactory.createFeature( m_list.getParentFeature(), m_list.getParentFeatureTypeProperty(), fid, m_featureType, true );
    else if( "change".equals( m_handleExisting ) )
      newFeature = existingFeature;
    else if( "nothing".equals( m_handleExisting ) )
      return true;
    else
      throw new IllegalArgumentException( "Argument 'handleExisting' must be one of 'change', 'overwrite' or 'existing', but is: " + m_handleExisting );

    try
    {
      FeatureHelper.copyProperties( f, newFeature, m_propertyMap );

      if( newFeature != existingFeature )
        m_list.add( newFeature );

      // den fid-hash aktuell halten
      m_fidHash.put( newFeature.getId(), newFeature );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      // TODO: error handling
    }

    return true;
  }

  private String createID( final Feature feature, final Object fromID )
  {
    final String oldFid = feature == null ? "<none>" : feature.getId();

    String fidhelp = m_fid;
    fidhelp = fidhelp.replaceAll( "\\Q${fromID}\\E", fromID == null ? "<null>" : fromID.toString() );
    // fidhelp = fidhelp.replaceAll( "\\Q${toID}\\E" , toID );
    fidhelp = fidhelp.replaceAll( "\\Q${fID}\\E", oldFid );
    if( fidhelp.indexOf( REPLACE_COUNT ) == -1 )
      fidhelp += REPLACE_COUNT;

    int count = -1;
    while( true )
    {
      final String replace = count == -1 ? "" : ("" + count);
      final String fid = fidhelp.replaceAll( "\\Q" + REPLACE_COUNT + "\\E", replace );

      if( m_fidHash.get( fid ) == null )
        return fid;

      count++;
    }
  }
}
