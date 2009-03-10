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
package org.kalypso.ogc.gml.map.layer;

import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.IKalypsoThemeListener;
import org.kalypso.ogc.gml.KalypsoThemeAdapter;
import org.kalypso.ogc.gml.map.IMapLayer;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * Abstract implementation of {@link IMapLayer}.<br>
 * Contains the standard reusable code (standard getters etc.).
 *
 * @author Gernot Belger
 */
public abstract class AbstractMapLayer implements IMapLayer
{
  private final IKalypsoThemeListener m_themeListener = new KalypsoThemeAdapter()
  {
    /**
     * @see org.kalypso.ogc.gml.KalypsoThemeAdapter#repaintRequested(org.kalypso.ogc.gml.AbstractKalypsoTheme,
     *      org.kalypsodeegree.model.geometry.GM_Envelope)
     */
    @Override
    public void repaintRequested( final IKalypsoTheme source, final GM_Envelope invalidExtent )
    {
      handeRepaintRequested( invalidExtent );
    }
  };

  private final IMapPanel m_panel;

  private final IKalypsoTheme m_theme;

  public AbstractMapLayer( final IMapPanel panel, final IKalypsoTheme theme )
  {
    m_panel = panel;
    m_theme = theme;

    theme.addKalypsoThemeListener( m_themeListener );
  }

  /**
   * If overridden, this method must be called in a super-call.
   *
   * @see org.kalypso.ogc.gml.map.IMapLayer#dispose()
   */
  @Override
  public void dispose( )
  {
    getTheme().removeKalypsoThemeListener( m_themeListener );
  }

  public String getLabel( )
  {
    return m_theme.getLabel();
  }

  /**
   * @see org.kalypso.ogc.gml.map.tiles.IMapLayer#getTheme()
   */
  protected final IKalypsoTheme getTheme( )
  {
    return m_theme;
  }

  /**
   * @see org.kalypso.ogc.gml.map.tiles.IMapLayer#getMapPanel()
   */
  @Override
  public final IMapPanel getMapPanel( )
  {
    return m_panel;
  }

  protected final void handeRepaintRequested( final GM_Envelope extent )
  {
    invalidate( extent );

    getMapPanel().invalidateMap();
  }

  /** Called, when the theme request a repaint for the given extent. */
  protected abstract void invalidate( final GM_Envelope invalidExtent );
}
