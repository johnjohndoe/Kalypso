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
package org.kalypso.model.wspm.pdb.ui.internal.wspm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemeVisitor;

/**
 * @author Gernot Belger
 */
public class FindReachThemesVisitor implements IKalypsoThemeVisitor
{
  private final Map<String, IKalypsoFeatureTheme> m_reaches = new HashMap<>();

  @Override
  public boolean visit( final IKalypsoTheme theme )
  {
    final String reachGmlId = theme.getProperty( PdbWspmProject.PROPERTY_THEME_REACH, null );
    if( reachGmlId != null && theme instanceof IKalypsoFeatureTheme )
      m_reaches.put( reachGmlId, (IKalypsoFeatureTheme) theme );

    return true;
  }

  public boolean hasReachTheme( final String reachGmlID )
  {
    return m_reaches.containsKey( reachGmlID );
  }

  public IKalypsoFeatureTheme[] getThemes( )
  {
    final Collection<IKalypsoFeatureTheme> values = m_reaches.values();
    return values.toArray( new IKalypsoFeatureTheme[values.size()] );
  }

  public IKalypsoFeatureTheme[] getThemes( final Object[] elements )
  {
    final Collection<IKalypsoFeatureTheme> themes = new ArrayList<>( elements.length );

    for( final Object element : elements )
    {
      if( element instanceof TuhhReach )
      {
        final TuhhReach reach = (TuhhReach) element;
        final String id = reach.getId();
        if( m_reaches.containsKey( id ) )
          themes.add( m_reaches.get( id ) );
      }
    }

    return themes.toArray( new IKalypsoFeatureTheme[themes.size()] );
  }
}