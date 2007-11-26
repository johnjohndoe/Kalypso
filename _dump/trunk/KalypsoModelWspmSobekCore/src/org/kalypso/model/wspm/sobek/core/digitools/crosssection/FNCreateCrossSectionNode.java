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
package org.kalypso.model.wspm.sobek.core.digitools.crosssection;

import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Point;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.model.wspm.sobek.core.SobekModelMember;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekModelMember;
import org.kalypso.model.wspm.sobek.core.utils.FNGmlUtils;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author kuch
 */
public class FNCreateCrossSectionNode extends AbstractWidget
{
  final static private java.awt.Cursor CURSOR_CROSSHAIR = java.awt.Cursor.getPredefinedCursor( Cursor.CROSSHAIR_CURSOR );

  final static private java.awt.Cursor CURSOR_DEFAULT = java.awt.Cursor.getPredefinedCursor( Cursor.DEFAULT_CURSOR );

  protected FNSnapPainterCreateProfileNode m_snapPainter = null;

  private Point m_currentPoint;

  public FNCreateCrossSectionNode( )
  {
    super( "Create a new Flow Network geometry", "Create a new Flow Network geometry" );

    new UIJob( "loading hydraulic model workspcae" )
    {
      @Override
      public IStatus runInUIThread( final IProgressMonitor monitor )
      {
        m_snapPainter = new FNSnapPainterCreateProfileNode( SobekModelMember.getModel() );
        return Status.OK_STATUS;
      }
    }.schedule();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final MapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    getMapPanel().setCursor( CURSOR_CROSSHAIR );
    getMapPanel().setMessage( "select profile by drawing a rectangle with help of left mouse button..." );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#moved(java.awt.Point)
   */
  @Override
  public void moved( final Point p )
  {
    m_currentPoint = p;

    /* Repaint. */
    final MapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaint();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    if( m_snapPainter != null && m_currentPoint != null )
    {
      final Point point = m_snapPainter.paint( g, getMapPanel(), m_currentPoint );
      if( point != null )
        m_currentPoint = point;
    }
  }

  /**
   * rectangle includes an profile?!?
   */
  @Override
  public void leftReleased( final Point p )
  {
    final GM_Point point = MapUtilities.transform( getMapPanel(), p );
    final GM_Point snapPoint = m_snapPainter.getSnapPoint( getMapPanel(), point );
    if( snapPoint != null )
    {
      final IBranch branch = m_snapPainter.getLastSnappedBranch();
      final Feature crossSection = m_snapPainter.getLastSnappedCrossSection();

      performFinish( branch, crossSection, snapPoint );
    }
  }

  private void performFinish( final IBranch branch, final Feature crossSection, final GM_Point snapPoint )
  {
    try
    {
      final ISobekModelMember model = SobekModelMember.getModel();

      FNGmlUtils.createProfileNode( model, branch, snapPoint, crossSection );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    super.finish();
  }
}
