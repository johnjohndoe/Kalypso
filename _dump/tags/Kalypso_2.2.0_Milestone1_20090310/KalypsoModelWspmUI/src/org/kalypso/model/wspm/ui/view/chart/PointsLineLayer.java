/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.ui.view.chart;

import java.awt.geom.Point2D;

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.contribs.eclipse.swt.graphics.RectangleUtils;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.ui.view.ILayerStyleProvider;
import org.kalypso.observation.result.IRecord;

import de.openali.odysseus.chart.framework.model.figure.IPaintable;
import de.openali.odysseus.chart.framework.model.figure.impl.PointFigure;
import de.openali.odysseus.chart.framework.model.figure.impl.PolylineFigure;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.layer.ILegendEntry;
import de.openali.odysseus.chart.framework.model.layer.impl.LegendEntry;
import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;

/**
 * @author kimwerner
 */
public class PointsLineLayer extends AbstractProfilLayer
{

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#onProfilChanged(org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint,
   *      org.kalypso.model.wspm.core.profil.IProfilChange[])
   */
  @Override
  public void onProfilChanged( ProfilChangeHint hint, IProfilChange[] changes )
  {
    if( hint.isPointsChanged() || hint.isPointValuesChanged() )
      getEventHandler().fireLayerContentChanged( this );
  }

  public PointsLineLayer( final IProfil profil, final String targetRangeProperty, final ILayerStyleProvider styleProvider )
  {
    super( profil, targetRangeProperty, styleProvider );
    setData( IProfilChartLayer.VIEW_DATA_KEY, IProfilChartLayer.ALLOW_VERTICAL_EDITING );

  }

  /**
   * @see de.openali.odysseus.chart.ext.base.layer.AbstractChartLayer#getLegendEntries()
   */
  @Override
  public synchronized ILegendEntry[] getLegendEntries( )
  {
    LegendEntry le = new LegendEntry( this, toString() )
    {
      @Override
      public void paintSymbol( GC gc, Point size )
      {

        final Rectangle clipping = gc.getClipping();

        final PolylineFigure figure = new PolylineFigure();
        figure.setStyle( getLineStyle() );
        final Point[] path = new Point[6];
        path[0] = new Point( 0, clipping.width / 2 );
        path[1] = new Point( clipping.width / 5, clipping.height / 2 );
        path[2] = new Point( clipping.width / 5 * 2, clipping.height / 4 );
        path[3] = new Point( clipping.width / 5 * 3, clipping.height / 4 * 3 );
        path[4] = new Point( clipping.width / 5 * 4, clipping.height / 2 );
        path[5] = new Point( clipping.width, clipping.height / 2 );
        figure.setPoints( path );
        figure.paint( gc );
      }
    };

    return new ILegendEntry[] { le };

  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.chart.AbstractProfilLayer#executeDrop(org.eclipse.swt.graphics.Point,
   *      de.openali.odysseus.chart.framework.model.layer.EditInfo)
   */

  @Override
  public void executeDrop( Point point, EditInfo dragStartData )
  {
    final Point newPoint = verifyPos( dragStartData.m_pos, point );
    Integer pos = dragStartData.m_data instanceof Integer ? (Integer) (dragStartData.m_data) : -1;
    if( pos > -1 )
    {
      final IProfil profil = getProfil();
      final IRecord profilPoint = profil.getPoint( pos );
      final Integer hoehe = profil.indexOfProperty( getTargetComponent() );
      final Integer breite = profil.indexOfProperty( getDomainComponent() );
      final ICoordinateMapper cm = getCoordinateMapper();
      final Double x = cm.getDomainAxis().screenToNumeric( newPoint.x ).doubleValue();
      final Double y = cm.getTargetAxis().screenToNumeric( newPoint.y ).doubleValue();
      profilPoint.setValue( breite, x );
      profilPoint.setValue( hoehe, y );
      profil.setActivePoint( profilPoint );
      getEventHandler().fireLayerContentChanged( this );
    }
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IChartLayer#paint(org.eclipse.swt.graphics.GC)
   */
  @Override
  public void paint( GC gc )
  {
    final IProfil profil = getProfil();

    if( profil == null )
      return;
    final IRecord[] profilPoints = profil.getPoints();
    final int len = profilPoints.length;
    final Point[] points = new Point[len];

    final int active = profil.indexOfPoint( profil.getActivePoint() );
    for( int i = 0; i < len; i++ )
    {
      final Point p = toScreen( profilPoints[i] );
      if( p == null )
        //TODO: user message
        return;
      points[i] = p;
    }

    final PolylineFigure pf = new PolylineFigure();

    pf.setStyle( getLineStyle() );
    pf.setPoints( points );
    pf.paint( gc );

    if( active < len - 1 )
    {
      pf.setStyle( getLineStyle_active() );
      pf.setPoints( new Point[] { points[active], points[active + 1] } );
      pf.paint( gc );
    }

    PointFigure pf2 = new PointFigure();

    pf2.setStyle( getPointStyle() );
    pf2.setPoints( points );
    pf2.paint( gc );
    if( active < len - 1 )
    {
      pf2.setStyle( getPointStyle_active() );
      pf2.setPoints( new Point[] { points[active] } );
      pf2.paint( gc );
    }
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#drag(org.eclipse.swt.graphics.Point,
   *      de.openali.odysseus.chart.framework.model.layer.EditInfo)
   */
  @Override
  public EditInfo drag( Point newPos, EditInfo dragStartData )
  {

    final Point newPoint = verifyPos( dragStartData.m_pos, newPos );
    final Integer index = (Integer) dragStartData.m_data;
    final IRecord[] profilPoints = getProfil().getPoints();
    final Point next = index == profilPoints.length - 1 ? newPoint : toScreen( profilPoints[index + 1] );
    final Point previous = index == 0 ? newPoint : toScreen( profilPoints[index - 1] );

    final PolylineFigure lineFigure = new PolylineFigure();
    lineFigure.setPoints( new Point[] { previous, newPoint, next } );
    lineFigure.setStyle( getLineStyle_hover() );

    final PointFigure pointFigure = new PointFigure();

    pointFigure.setStyle( getPointStyle_hover() );
    pointFigure.setPoints( new Point[] { newPoint } );

    final IPaintable dragFigure = new IPaintable()
    {

      @Override
      public void paint( GC gc )
      {
        lineFigure.paint( gc );
        pointFigure.paint( gc );

      }
    };

    final Point2D point = toNumeric( newPoint );
    return new EditInfo( this, null, dragFigure, dragStartData.m_data, String.format( TOOLTIP_FORMAT, new Object[] { getDomainComponent().getName(), point.getX(), getTargetComponent().getName(),
        point.getY(), getTargetComponent().getUnit() } ), dragStartData.m_pos );

  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#getHoverRect(org.kalypso.observation.result.IRecord)
   */

  @Override
  public Rectangle getHoverRect( IRecord profilPoint )
  {
    final ICoordinateMapper cm = getCoordinateMapper();
    return cm == null ? null : RectangleUtils.buffer( toScreen( profilPoint ) );
  }

  private final Point verifyPos( final Point oldPos, final Point newPos )
  {
    final Object o = getData( IProfilChartLayer.VIEW_DATA_KEY );
    if( o != null )

      try
      {
        final int i = Integer.valueOf( o.toString() );
        if( (i & 2) == 0 )
        {
          newPos.y = oldPos.y;
        }
        if( (i & 1) == 0 )
        {
          newPos.x = oldPos.x;
        }
      }

      catch( NumberFormatException e )
      {
        return oldPos;
      }
    return newPos;
  }

}
