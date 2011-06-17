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

import java.util.ArrayList;
import java.util.List;

import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * class FindExistingHeavyRelationsFeatureVisitor created by
 * 
 * @author doemming (13.05.2005)
 */
public class FindExistingHeavyRelationsFeatureVisitor implements FeatureVisitor
{
  private final HeavyRelationType m_relation;

  private final List<Feature[]> m_results = new ArrayList<Feature[]>();

  private final GMLWorkspace m_workspace;

  /**
   * @param relation
   *          relationtype to query
   * @param workspace
   *          workspace to query
   */
  public FindExistingHeavyRelationsFeatureVisitor( final GMLWorkspace workspace, final HeavyRelationType relation )
  {
    m_workspace = workspace;
    m_relation = relation;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  @Override
  public boolean visit( final Feature srcFE )
  {
    if( !m_relation.getSrcFT().equals( srcFE.getFeatureType() ) )
      return false;
    final IRelationType link1Name = m_relation.getLink1();
    final IRelationType link2Name = m_relation.getLink2();
    final Feature[] props1 = m_workspace.resolveLinks( srcFE, link1Name );
    for( final Feature feature1 : props1 )
    {
      if( feature1.getFeatureType().equals( m_relation.getBodyFT() ) )
      {
        final Feature[] props2 = m_workspace.resolveLinks( feature1, link2Name );
        for( final Feature feature2 : props2 )
        {
          if( feature2.getFeatureType().equals( m_relation.getDestFT() ) )
          {
            m_results.add( new Feature[] { srcFE, feature1, feature2 } );
          }
        }
      }
    }
    return !m_results.isEmpty();
  }

  public Feature[] getBodyFeatureFor( final Feature destFE )
  {
    final List<Feature> result = new ArrayList<Feature>();
    for( final Feature[] f : m_results )
    {
      if( f[2] == destFE )
        result.add( f[1] );
    }
    return result.toArray( new Feature[result.size()] );
  }

  public boolean relationExistsTo( final Feature f2 )
  {
    for( final Object element : m_results )
    {
      final Feature[] f = (Feature[]) element;
      if( f[2] == f2 )
        return true;
    }
    return false;
  }
}