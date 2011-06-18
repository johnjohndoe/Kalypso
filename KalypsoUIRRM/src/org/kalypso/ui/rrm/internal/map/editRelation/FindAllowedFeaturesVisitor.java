/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ui.rrm.internal.map.editRelation;

import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree_impl.model.sort.SplitSort;

/**
 * @author Gernot Belger
 */
public class FindAllowedFeaturesVisitor implements FeatureVisitor
{
  private final FeatureList m_sourceFeatures = new SplitSort( null, null );

  private final FeatureList m_targetFeatures = new SplitSort( null, null );

  private final IEditRelationType m_relation;

  public FindAllowedFeaturesVisitor( final IEditRelationType relation )
  {
    m_relation = relation;
  }

  @SuppressWarnings("unchecked")
  @Override
  public boolean visit( final Feature f )
  {
    final IFeatureType featureType = f.getFeatureType();

    final IFeatureType sourceType = m_relation.getSourceType();
    final IFeatureType targetType = m_relation.getTargetType();

    if( GMLSchemaUtilities.substitutes( featureType, sourceType.getQName() ) )
      m_sourceFeatures.add( f );

    if( GMLSchemaUtilities.substitutes( featureType, targetType.getQName() ) )
      m_targetFeatures.add( f );

    return true;
  }

  public FeatureList getSourceFeatures( )
  {
    return m_sourceFeatures;
  }

  public FeatureList getTargetFeatures( )
  {
    return m_targetFeatures;
  }
}