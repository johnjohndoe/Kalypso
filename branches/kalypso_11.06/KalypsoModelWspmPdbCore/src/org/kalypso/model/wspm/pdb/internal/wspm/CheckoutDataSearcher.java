/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.pdb.internal.wspm;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.db.utils.ByStationComparator;

/**
 * @author Gernot Belger
 */
public class CheckoutDataSearcher
{
  private final Set<CrossSection> m_crossSections = new TreeSet<CrossSection>( new ByStationComparator() );

  private final Set<Event> m_events = new HashSet<Event>();

  public void search( final IStructuredSelection selection )
  {
    final List< ? > list = selection.toList();
    addElements( list );
  }

  private void addElements( final Collection< ? > elements )
  {
    for( final Object element : elements )
    {
      addElementAsCrossSection( element );
      addElementAsWaterLevel( element );
    }
  }

  private void addElementAsCrossSection( final Object element )
  {
    if( element instanceof CrossSection )
      m_crossSections.add( (CrossSection) element );
    else if( element instanceof State )
    {
      final State state = (State) element;
      addElements( state.getCrossSections() );
    }
    else if( element instanceof WaterBody )
    {
      final WaterBody waterBody = (WaterBody) element;
      addElements( waterBody.getCrossSections() );
    }
  }

  private void addElementAsWaterLevel( final Object element )
  {
    if( element instanceof Event )
      m_events.add( (Event) element );
    else if( element instanceof WaterBody )
      addElements( ((WaterBody) element).getEvents() );
  }

  public CrossSection[] getCrossSections( )
  {
    return m_crossSections.toArray( new CrossSection[m_crossSections.size()] );
  }

  public Event[] getEvents( )
  {
    return m_events.toArray( new Event[m_events.size()] );
  }
}