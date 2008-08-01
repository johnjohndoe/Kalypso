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
package org.kalypso.ogc.gml.mapmodel;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.ogc.gml.IKalypsoTheme;

public final class ActivateThemeJob extends Job
{
  public static final String NO_THEME = "NO_THEME"; //$NON-NLS-1$

  protected final IKalypsoThemeVisitor m_themeVisitor = new IKalypsoThemeVisitor()
  {
    public boolean visit( final IKalypsoTheme theme )
    {
      if( theme.isLoaded() )
        maybeActivateTheme( theme );

      return true;
    }
  };

  protected final IMapModellListener m_modellListener = new MapModellAdapter()
  {
    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeAdded(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypso.ogc.gml.IKalypsoTheme)
     */
    @Override
    public void themeAdded( IMapModell source, IKalypsoTheme theme )
    {
      maybeActivateTheme( theme );
    }

    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#contextChanged(org.kalypso.ogc.gml.mapmodel.IMapModell)
     */
    @Override
    public void themeContextChanged( final IMapModell source, final IKalypsoTheme theme )
    {
      maybeActivateTheme( theme );
    }
  };

  private final String m_themeContext;

  private final IMapModell m_mapModell;

  public ActivateThemeJob( final IMapModell mapModell, final String name, final String themeContext )
  {
    super( name );

    Assert.isNotNull( mapModell );

    m_themeContext = themeContext;
    m_mapModell = mapModell;
  }

  protected void maybeActivateTheme( final IKalypsoTheme themeToActivate )
  {
    try
    {
      final String themeContext = themeToActivate.getTypeContext();
      if( m_themeContext != null && m_themeContext.equals( themeContext ) )
      {
        m_mapModell.activateTheme( themeToActivate );
      }
    }
    catch( final Throwable t )
    {
      t.printStackTrace();
    }
  }

  @Override
  protected IStatus run( final IProgressMonitor monitor )
  {
    if( m_mapModell != null )
      m_mapModell.addMapModelListener( m_modellListener );
    // TODO and what if it IS null? NPE below!

    if( m_themeContext != null )
    {
      final IKalypsoTheme activeTheme = m_mapModell == null ? null : m_mapModell.getActiveTheme();
      if( activeTheme != null && m_themeContext.equals( NO_THEME ) )
      {
        m_mapModell.activateTheme( null );
      }
      else
      {
        m_mapModell.accept( m_themeVisitor, IKalypsoThemeVisitor.DEPTH_INFINITE );
      }
    }

    return Status.OK_STATUS;
  }

  public void dispose( )
  {
    if( m_mapModell != null )
      m_mapModell.removeMapModelListener( m_modellListener );
  }
}