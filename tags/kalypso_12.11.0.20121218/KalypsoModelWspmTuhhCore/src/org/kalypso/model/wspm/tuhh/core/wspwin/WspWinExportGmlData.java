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
package org.kalypso.model.wspm.tuhh.core.wspwin;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.core.gml.WspmReach;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.IFeatureRelation;

/**
 * @author Gernot Belger
 */
public class WspWinExportGmlData extends WspWinExportData
{
  private final Collection<WspmWaterBody> m_waterBodies = new HashSet<>();

  private final Map<WspmWaterBody, Set<TuhhReach>> m_reaches = new HashMap<>();

  public void setSelection( final IStructuredSelection selection )
  {
    // TODO: search for water bodies or reaches
    final List< ? > list = selection.toList();
    addElements( list );
  }

  private void addElements( final List< ? > list )
  {
    for( final Object element : list )
      addElement( element );
  }

  private void addElement( final Object element )
  {
    if( element instanceof WspmWaterBody )
    {
      final WspmWaterBody waterBody = (WspmWaterBody) element;
      m_waterBodies.add( waterBody );
      /* If water body is selected, all it's reaches get exported */
      final IFeatureBindingCollection<WspmReach> reaches = waterBody.getReaches();
      addElements( reaches );
    }
    else if( element instanceof TuhhReach )
    {
      final TuhhReach reach = (TuhhReach) element;
      final WspmWaterBody waterBody = reach.getWaterBody();
      if( !m_reaches.containsKey( waterBody ) )
        m_reaches.put( waterBody, new HashSet<TuhhReach>() );
      final Set<TuhhReach> reaches = m_reaches.get( waterBody );
      m_waterBodies.add( reach.getWaterBody() );
      reaches.add( reach );
    }
    else if( element instanceof IFeatureRelation )
    {
      final IFeatureRelation property = (IFeatureRelation) element;
      final IRelationType relation = property.getPropertyType();
      final Feature parentFeature = property.getOwner();
      if( parentFeature != null && relation != null && relation.isList() )
      {
        final List< ? > list = (List< ? >) parentFeature.getProperty( relation );
        addElements( list );
      }
    }
  }

  public WspmWaterBody[] getWaterBodies( )
  {
    return m_waterBodies.toArray( new WspmWaterBody[m_waterBodies.size()] );
  }

  public TuhhReach[] getReaches( final WspmWaterBody waterBody )
  {
    if( !m_reaches.containsKey( waterBody ) )
      return new TuhhReach[0];

    final Set<TuhhReach> reaches = m_reaches.get( waterBody );
    return reaches.toArray( new TuhhReach[reaches.size()] );
  }
}