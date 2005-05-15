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

package org.kalypsodeegree.model.feature;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.kalypso.ogc.gml.map.widgets.editrelation.HeavyRelationType;

/**
 * 
 * class FindExistingHeavyRelationsFeatureVisitor
 * 
 * created by
 * 
 * @author doemming (13.05.2005)
 */
public class FindExistingHeavyRelationsFeatureVisitor implements FeatureVisitor
{
  private final HeavyRelationType m_relation;

  private final List m_results = new ArrayList();

  private final GMLWorkspace m_workspace;

  /**
   * @param relation
   *          relationtype to query
   * @param workspace
   *          workspace to query
   */
  public FindExistingHeavyRelationsFeatureVisitor( GMLWorkspace workspace,
      HeavyRelationType relation )
  {
    m_workspace = workspace;
    m_relation = relation;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( Feature srcFE )
  {
    if( srcFE.getFeatureType() != m_relation.getSrcFT() )
      return false;
    final String link1Name = m_relation.getLink1().getName();
    final String link2Name = m_relation.getLink2().getName();
    final Feature[] props1 = m_workspace.resolveLinks( srcFE, link1Name );
    for( int i = 0; i < props1.length; i++ )
    {
      final Feature feature1 = props1[i];
      if( feature1.getFeatureType() == m_relation.getBodyFT() )
      {
        final Feature[] props2 = m_workspace.resolveLinks( feature1, link2Name );
        for( int j = 0; j < props2.length; j++ )
        {
          final Feature feature2 = props2[j];
          if( feature2.getFeatureType() == m_relation.getDestFT() )
          {
            m_results.add( new Feature[]
            {
                srcFE,
                feature1,
                feature2 } );
          }
        }
      }
    }
    return !m_results.isEmpty();
  }

  public Feature[] getBodyFeatureFor( Feature destFE )
  {
    List result = new ArrayList();
    for( Iterator iter = m_results.iterator(); iter.hasNext(); )
    {
      Feature[] f = (Feature[])iter.next();
      if( f[2] == destFE )
        result.add( f[1] );
    }
    return (Feature[])result.toArray( new Feature[result.size()] );
  }

  public boolean relationExistsTo( Feature f2 )
  {
    for( Iterator iter = m_results.iterator(); iter.hasNext(); )
    {
      Feature[] f = (Feature[])iter.next();
      if( f[2] == f2 )
        return true;
    }
    return false;
  }
}