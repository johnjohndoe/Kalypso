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
package org.kalypso.model.wspm.tuhh.ui.chart;

import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.jface.resource.ColorRegistry;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyRemove;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.view.IProfilView;
import org.kalypso.model.wspm.ui.view.ProfilViewData;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilChartLayer;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;

import de.belger.swtchart.EditInfo;

public class BewuchsLayer extends AbstractProfilChartLayer implements IProfilChartLayer
{
  private final IProfilEventManager m_pem;

  private final Color m_color;

  private static String TOOLTIP_FORMAT = " AX: %.4f %n AY: %.4f %n DP: %.4f";
  

  public BewuchsLayer(final ProfilChartView pcv)
  {
    super(IWspmTuhhConstants.LAYER_BEWUCHS, pcv, pcv.getDomainRange(), pcv.getValueRangeLeft(),"Bewuchs");

    m_pem = pcv.getProfilEventManager();
    final ColorRegistry cr = pcv.getColorRegistry();
    if (!cr.getKeySet().contains(IWspmTuhhConstants.LAYER_BEWUCHS ))
      cr.put( IWspmTuhhConstants.LAYER_BEWUCHS, new RGB( 0, 255, 0 ) );
    m_color = cr.get( IWspmTuhhConstants.LAYER_BEWUCHS );

  }

  @Override
  public IProfilView createLayerPanel( IProfilEventManager pem, ProfilViewData viewData )
  {
    return null;
  }

  public void removeYourself( )
  {
    final IProfilChange[] changes = new IProfilChange[3];
    changes[0] = new PointPropertyRemove( m_pem.getProfil(), IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX );
    changes[1] = new PointPropertyRemove( m_pem.getProfil(), IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AY );
    changes[2] = new PointPropertyRemove( m_pem.getProfil(), IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_DP );
    final ProfilOperation operation = new ProfilOperation( "Bewuchs entfernen", m_pem, changes, true );
    new ProfilOperationJob( operation ).schedule();
  }

  public Rectangle2D getBounds( )
  {
    try
    {
      final IProfilPoint p = m_pem.getProfil().getPoints().getFirst();
      final double x = p.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_BREITE );
      final double y = p.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_HOEHE );
      final Point2D p2 = new Point2D.Double( x, y );
      return new Rectangle2D.Double( p2.getX(), p2.getY(), 0, 0 );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return new Rectangle2D.Double( 0, 0, 0, 0 );

    }

  }

  private Point2D[] getPoints( )
  {
    try
    {
      final List<IProfilPoint> ppoints = m_pem.getProfil().getPoints();
      final Point2D[] points = new Point2D[ppoints.size()];
      int i = 0;
      for( final IProfilPoint p : ppoints )
      {
        final double x = p.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_BREITE );
        final double y = p.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_HOEHE );
        points[i++] = new Point2D.Double( x, y );
      }
      return points;
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      return new Point2D[0];
    }
  }

  @Override
  public String toString( )
  {
    return "Bewuchs";
  }

  @Override
  public EditInfo getHoverInfo( final Point mousePos )
  {
    final Point2D[] points = getPoints();
    for( int i = 0; i < points.length - 1; i++ )
    {
      final IProfilPoint pp = m_pem.getProfil().getPoints().get( i );
      try
      {
        final double ax = pp.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX );
        final double ay = pp.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AY );
        final double dp = pp.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_DP );
        if( ax * ay * dp != 0 )
        {

          final Point pl = logical2screen( points[i] );
          final double xl = points[i].getX();
          final double yl = points[i].getY();
          final double xr = points[i + 1].getX();
          final double yr = points[i + 1].getY();
          final Point2D pm = new Point2D.Double( xl + (xr - xl) / 2, yl + (yr - yl) / 2 );
          final Point p = logical2screen( pm );
          final Rectangle hover = new Rectangle( p.x - 10, p.y - 20, 20, 20 );// RectangleUtils.buffer( p );
          if( hover.contains( mousePos ) & ((p.x - pl.x) > 12) )
            return new EditInfo( this, hover, i, String.format( TOOLTIP_FORMAT, new Object[] { ax, ay, dp } ) );
        }
      }
      catch( final Exception e1 )
      {
        // Do Nothing
        e1.printStackTrace();
      }

    }
    return null;
  }

  @Override
  public void paintDrag( GCWrapper gc, Point editing, Object hoverData )
  {
    // do nothing

  }

  @Override
  public void paintLegend( GCWrapper gc )
  {
    drawIcon( gc, gc.getClipping() );
  }

  public void paint( GCWrapper gc )
  {
    final LinkedList<IProfilPoint> points = m_pem.getProfil().getPoints();
    if( points.isEmpty() )
      return;
    Point2D p2dL = null;
    boolean hasValue = false;
    for( IProfilPoint point : points )
    {

      final Point2D p2dR = ProfilUtil.getPoint2D( point, IWspmTuhhConstants.POINT_PROPERTY_HOEHE );
      if( (p2dL != null) && hasValue )
      {
        final double xl = p2dL.getX();
        final double yl = p2dL.getY();
        final double xr = p2dR.getX();
        final double yr = p2dR.getY();
        final Point p = logical2screen( new Point2D.Double( xl + (xr - xl) / 2, yl + (yr - yl) / 2 ) );
        if( (p.x - logical2screen( p2dL ).x) > 12 )
          drawIcon( gc, new Rectangle( p.x - 10, p.y - 20, 20, 20 ) );
      }
      hasValue = point.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX ) * point.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AY )
          * point.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_DP ) != 0.0;
      p2dL = p2dR;
    }
  }

  private void drawIcon( final GCWrapper gc, final Rectangle clipping )
  {

    final int left = clipping.x + 2;
    final int top = clipping.y + 2;
    final int right = clipping.x + clipping.width - 4;
    final int bottom = clipping.y + clipping.width - 4;
    final int midx = (left + right) / 2;
    final int midy = (top + bottom) / 2;
    gc.setLineWidth( 2 );
    gc.setForeground( m_color );
    gc.setLineStyle( SWT.LINE_SOLID );

    gc.drawLine( midx - 3, bottom, midx + 3, bottom );
    gc.drawLine( midx, bottom, midx, midy );
    gc.drawOval( left + 2, top, right - left - 4, bottom - midy + 4 );
  }

  /**
   * @see com.bce.profil.ui.view.chart.layer.AbstractProfilChartLayer#editProfil(org.eclipse.swt.graphics.Point,
   *      java.lang.Object)
   */
  @Override
  protected void editProfil( Point point, Object data )
  {
  }

  /**
   * @see com.bce.eind.core.profil.IProfilListener#onProfilChanged(com.bce.eind.core.profil.changes.ProfilChangeHint,
   *      com.bce.eind.core.profil.IProfilChange[])
   */
  @Override
  public void onProfilChanged( ProfilChangeHint hint, IProfilChange[] changes )
  {
  }
}
