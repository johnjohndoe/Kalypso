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
package org.kalypso.model.wspm.tuhh.ui.chart;

import java.awt.geom.Point2D;

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.ui.view.ILayerStyleProvider;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

import de.openali.odysseus.chart.framework.model.figure.impl.EmptyRectangleFigure;
import de.openali.odysseus.chart.framework.model.figure.impl.PolylineFigure;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;

/**
 * @author kimwerner
 */
public class PointMarkerLayer extends AbstractProfilLayer
{

  private final int m_offset;

  private final boolean m_close;

  public PointMarkerLayer( final IProfil profil, final String targetRangeProperty, final ILayerStyleProvider styleProvider, final int offset, final boolean close )
  {
    super( profil, targetRangeProperty, styleProvider );

    m_offset = offset;

    m_close = close;

  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.chart.AbstractProfilLayer#executeDrop(org.eclipse.swt.graphics.Point,
   *      de.openali.odysseus.chart.framework.model.layer.EditInfo)
   */
 
  @Override
  public void executeDrop( Point point, EditInfo dragStartData )
  {
    Integer pos = dragStartData.m_data instanceof Integer ? (Integer) (dragStartData.m_data) : -1;
    if( pos > -1 )
    {
      final IProfil profil = getProfil();
      final IRecord profilPoint = profil.getPoint( pos );
      final IProfilPointMarker[] deviders = profil.getPointMarkerFor( profilPoint );
      for( final IProfilPointMarker devider : deviders )
      {
        if( devider.getId().getId().equals( getTargetComponent().getId() ) )
        {
          final IRecord newPoint = ProfilUtil.findNearestPoint( profil, toNumeric( point ).getX() );
          if( newPoint != profilPoint )
          {
            devider.setPoint( newPoint );
            profil.setActivePoint( newPoint );
          }
        }
      }
    }
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.chart.AbstractProfilLayer#getHoverRect(org.kalypso.observation.result.IRecord)
   */
  @Override
  public Rectangle getHoverRect( IRecord profilPoint )
  {
    if( getTargetComponent() == null )
      return null;
    final IProfilPointMarker[] deviders = getProfil().getPointMarkerFor( profilPoint );
    for( final IProfilPointMarker devider : deviders )
    {
      if( devider.getId().getId().equals( getTargetComponent().getId() ) )
      {
        final Rectangle rect = new Rectangle( toScreen( profilPoint ).x - 5, m_offset, 10, getTargetAxis().getScreenHeight() - m_offset );
        return rect;
      }
    }
    return null;
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IChartLayer#paint(org.eclipse.swt.graphics.GC)
   */
  @Override
  public void paint( GC gc )
  {
    final IProfil profil = getProfil();
    final IComponent target = getTargetComponent();

    if( profil == null || target == null )
      return;
    final IProfilPointMarker[] deviders = profil.getPointMarkerFor( target.getId() );
    final int len = deviders.length;

    final int baseLine = getCoordinateMapper().getTargetAxis().getScreenHeight();
    final PolylineFigure pf = new PolylineFigure();
    pf.setStyle( getLineStyle() );
    for( int i = 0; i < len; i++ )
    {
      final int x = toScreen( deviders[i].getPoint() ).x;
      final Point p1 = new Point( x, m_offset );
      final Point p2 = new Point( x, baseLine );
      pf.setPoints( new Point[] { p1, p2 } );
      pf.paint( gc );
    }
    if( m_close )
    {
      final int x1 = toScreen( deviders[0].getPoint() ).x;
      final int x2 = toScreen( deviders[len - 1].getPoint() ).x;
      final Point p1 = new Point( x1, m_offset );
      final Point p2 = new Point( x2, m_offset );
      pf.setPoints( new Point[] { p1, p2 } );
      pf.paint( gc );
    }
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IEditableChartLayer#drag(org.eclipse.swt.graphics.Point,
   *      de.openali.odysseus.chart.framework.model.layer.EditInfo)
   */
  @Override
  public EditInfo drag( Point newPos, EditInfo dragStartData )
  {
    final IProfil profil = getProfil();
    final IRecord point = ProfilUtil.findNearestPoint( profil, toNumeric( newPos ).getX() );
    final Point p = toScreen( point );

    final EmptyRectangleFigure hoverFigure = new EmptyRectangleFigure();
    hoverFigure.setStyle( getLineStyle_hover() );
    hoverFigure.setRectangle( new Rectangle( p.x - 5, m_offset, 10, getTargetAxis().getScreenHeight() ) );

    return new EditInfo( this, null, hoverFigure, dragStartData.m_data, getTooltipInfo( toNumeric( newPos ) ), newPos );

  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.chart.AbstractProfilLayer#getTooltipInfo(java.awt.geom.Point2D)
   */
  @Override
  public String getTooltipInfo( Point2D point )
  {
    try
    {
      return String.format( "%-12s %10.4f [m]%n%-12s", new Object[] { getDomainComponent().getName(), point.getX(), getTargetComponent().getName() } );
    }
    catch( RuntimeException e )
    {
      return e.getLocalizedMessage();
    }
  }

}
