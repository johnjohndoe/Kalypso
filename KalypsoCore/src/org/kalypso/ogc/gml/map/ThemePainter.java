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
package org.kalypso.ogc.gml.map;

import java.awt.Graphics2D;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemeVisitor;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * @author Gernot Belger
 */
public class ThemePainter
{
  private final GeoTransform m_projection;

  private final double m_scale;

  private final GM_Envelope m_boundingBox;

  private final IKalypsoTheme[] m_themes;

  public ThemePainter( final IMapModell model, final GeoTransform projection, final GM_Envelope boundingBox, final double scale )
  {
    m_projection = projection;
    m_boundingBox = boundingBox;
    m_scale = scale;
    m_themes = getVisibleThemes( model );
  }

  /**
   * @see org.kalypso.ogc.gml.map.IPainter#paint(java.awt.Graphics2D)
   */
  public void paintThemes( final Graphics2D gr, final boolean selected, final IProgressMonitor monitor ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, "zeichne Themen", m_themes.length );

    for( int i = m_themes.length - 1; i >= 0; i-- )
    {
      final IKalypsoTheme theme = m_themes[i];

      final SubMonitor childProgress = progress.newChild( 1 );
      theme.paint( gr, m_projection, m_scale, m_boundingBox, selected, childProgress );
      childProgress.done();
    }
  }

  private IKalypsoTheme[] getVisibleThemes( final IMapModell model )
  {
    final List<IKalypsoTheme> visibleThemes = new ArrayList<IKalypsoTheme>();
    final IKalypsoThemeVisitor visitor = new IKalypsoThemeVisitor()
    {
      public boolean visit( final IKalypsoTheme theme )
      {
        if( theme instanceof IMapModell )
        {
          if( theme.isVisible() )
          {
            // dont add it to the themes, we paint them ourselfes (the cascading modells paint method would also paint
            // all is children)
            return true;
          }

          // dont descent
          return false;
        }

        if( theme.isVisible() )
          visibleThemes.add( theme );

        // if it is not a map modell we dont recurse
        return false;
      }
    };

    model.accept( visitor, IKalypsoThemeVisitor.DEPTH_INFINITE );
    return visibleThemes.toArray( new IKalypsoTheme[visibleThemes.size()] );
  }
}
