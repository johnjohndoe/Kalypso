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
package org.kalypso.model.wspm.sobek.core.digitools.branch;

import java.awt.Graphics;
import java.awt.Point;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.model.wspm.sobek.core.SobekModelMember;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekModelMember;
import org.kalypso.model.wspm.sobek.core.pub.ISnapPainter;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.map.widgets.builders.IGeometryBuilder;
import org.kalypso.ogc.gml.map.widgets.builders.LineGeometryBuilder;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author kuch
 */
public class FNExtendBranch extends AbstractWidget
{
  private IGeometryBuilder m_geoBuilder = null;

  protected ISnapPainter m_snapPainter = null;

  private GM_Point m_pos;

  private Point m_currentPoint;

  public FNExtendBranch( )
  {
    super( "Create a new Flow Network geometry", "Create a new Flow Network geometry" );

    new UIJob( "loading hydraulic model workspcae" )
    {

      @Override
      public IStatus runInUIThread( final IProgressMonitor monitor )
      {

        m_snapPainter = new FNSnapPainterExtendBranches( SobekModelMember.getModel( null ) );
        return Status.OK_STATUS;
      }
    }.schedule();

    reinit();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final MapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    reinit();
  }

  /**
   * Returns false if this widget is not correctly initialized.
   */
  private boolean check( )
  {
    if( m_geoBuilder == null )
      return false;

    return true;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#doubleClickedLeft(java.awt.Point)
   */
  @Override
  public void doubleClickedLeft( final Point p )
  {
    if( !check() )
      return;

    ISobekModelMember model = SobekModelMember.getModel( null );

    if( model == null )
      throw new IllegalStateException( "Flow Network Model can't be null" );

    try
    {
      final GM_Curve curve = (GM_Curve) m_geoBuilder.finish();
      if( curve == null )
        return;

      /* If the object is finished (!=null) create the feature. */
      final FNBranchExpander expander = new FNBranchExpander( model, getMapPanel(), curve );
      expander.finish();

      reset();

      m_snapPainter = new FNSnapPainterExtendBranches( model );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      reset();
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
   */
  @Override
  public void leftClicked( final Point p )
  {
    if( !check() )
      reinit();

    m_pos = MapUtilities.transform( getMapPanel(), p );

    try
    {
      /*
       * Only adds points. The return value should be always null, because we specified no rule regarding the amount of
       * points.
       */
      final GM_Point point = m_snapPainter.getSnapPoint( getMapPanel(), m_pos );
      if( point == null )
        m_geoBuilder.addPoint( m_pos );
      else
        m_geoBuilder.addPoint( point );

    }
    catch( final Exception e )
    {
      e.printStackTrace();
      reset();
    }
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
    if( m_snapPainter != null )
    {
      final Point point = m_snapPainter.paint( g, getMapPanel(), m_currentPoint );
      if( point != null )
        m_currentPoint = point;
    }

    if( m_geoBuilder != null )
      m_geoBuilder.paint( g, getMapPanel().getProjection(), m_currentPoint );
  }

  private final void reinit( )
  {
    final MapPanel mapPanel = getMapPanel();
    if( mapPanel != null )
    {
      final IMapModell mapModell = mapPanel.getMapModell();
      m_geoBuilder = new LineGeometryBuilder( 0, mapModell.getCoordinatesSystem() );
    }
  }

  /**
   * Resets the widget, must be reinitialized before it can be used again.
   * 
   * @see #init(Feature, CommandableWorkspace)
   */
  private void reset( )
  {
    m_geoBuilder = null;
  }

}
