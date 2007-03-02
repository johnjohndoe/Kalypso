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
package org.kalypso.model.wspm.tuhh.ui.chart.overlay;

import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.Arrays;
import java.util.List;

import org.eclipse.jface.resource.ColorRegistry;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypso.contribs.eclipse.swt.graphics.RectangleUtils;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilChartLayer;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;
import org.kalypso.model.wspm.ui.view.chart.AbstractPolyLineLayer.EditData;

import de.belger.swtchart.EditInfo;
import de.belger.swtchart.axis.AxisRange;

/**
 * @author kimwerner
 */
public class ProfilOverlayLayer extends AbstractProfilChartLayer
{

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilChartLayer#alwaysAllowsEditing()
   */
  @Override
  public boolean alwaysAllowsEditing( )
  {
    return true;
  }

  private List<Point2D> m_points;

  private final Color m_color;

  public ProfilOverlayLayer( ProfilChartView chartView )
  {
    this( chartView, ProfilUtil.getPoints2D( chartView.getProfil(), IWspmConstants.POINT_PROPERTY_HOEHE ) );
  }

  public ProfilOverlayLayer( ProfilChartView chartView, Point2D[] points )
  {
    super( IWspmOverlayConstants.LAYER_OVERLAY, chartView, chartView.getDomainRange(), chartView.getValueRangeLeft(), "Zeichenfl‰che" );
    m_points = Arrays.asList( points );
    final ColorRegistry cr = chartView.getColorRegistry();
    if( !cr.getKeySet().contains( IWspmOverlayConstants.LAYER_OVERLAY ) )
      cr.put( IWspmOverlayConstants.LAYER_OVERLAY, new RGB( 255, 255, 0 ) );
    m_color = cr.get( IWspmOverlayConstants.LAYER_OVERLAY );
  }

  protected void drawEditLine( final GCWrapper gc, final Point editing, final int index )
  {
    if( 0 <= index && index < m_points.size() )
    {
      final int lineWidthBuffer = gc.getLineWidth();
      final int lineStyleBuffer = gc.getLineStyle();
      final Color foregroundBuffer = gc.getForeground();
      gc.setLineStyle( SWT.LINE_DASH );
      gc.setLineWidth( 1 );
      gc.setForeground( m_color );

      // final Point p = logical2screen( m_points.get( index ) );
      final AxisRange valueRange = getValueRange();
      final int bottom = valueRange.getScreenFrom() + valueRange.getGapSpace();
      int top = valueRange.getScreenTo() + valueRange.getGapSpace();
      gc.drawLine( editing.x, bottom, editing.x, top );
      gc.setLineWidth( lineWidthBuffer );
      gc.setLineStyle( lineStyleBuffer );
      gc.setForeground( foregroundBuffer );
    }
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilChartLayer#editProfil(org.eclipse.swt.graphics.Point,
   *      java.lang.Object)
   */
  @Override
  protected void editProfil( Point point, Object data )
  {
    final EditData editData = (EditData) data;
    final Point2D point2D = m_points.get( editData.getIndex() );
    final Point2D logPoint = screen2logical( point );
    final double hoehe = logPoint.getY();
    point2D.setLocation( logPoint.getX(),hoehe );
    getProfilChartView().getChart().repaint();
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#getBounds()
   */
  public Rectangle2D getBounds( )
  {
    Rectangle2D bounds = new Rectangle2D.Double( m_points.get( 0 ).getX(), m_points.get( 0 ).getY(), 0, 0 );
    for( final Point2D point : m_points )
      bounds.add( point );
    return bounds;
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilChartLayer#getHoverInfo(org.eclipse.swt.graphics.Point)
   */
  @Override
  public EditInfo getHoverInfo( Point point )
  {
    int index = 0;
    for( final Point2D point2D : m_points )
    {
      final Point p = logical2screen( point2D );
      final Rectangle hover = RectangleUtils.buffer( p );
      if( hover.contains( point ) )
      {
        return new EditInfo( this, hover, new EditData( index, "" ), "HoverInfo" );
      }
      index++;
    }
    return null;
  }

  public List getPoints( )
  {
    return m_points;
  }

  public Point2D[] setPoints( final Point2D[] points )
  {
    final Point2D[] oldPoints = m_points.toArray( new Point2D[0] );
    m_points = Arrays.asList( points );
    return oldPoints;
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paint(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  public void paint( GCWrapper gc )
  {
    if( m_points.size() < 2 )
      return;
    Point leftP = null;
    final int lineWidthBuffer = gc.getLineWidth();
    final int lineStyleBuffer = gc.getLineStyle();
    final Color foregroundBuffer = gc.getForeground();
    gc.setLineWidth( 3 );
    gc.setLineStyle( SWT.LINE_SOLID );
    gc.setForeground( m_color );
    for( final Point2D point : m_points )
    {
      if( leftP == null )
      {
        leftP = logical2screen( point );
      }
      else
      {
        final Point rightP = logical2screen( point );
        gc.drawOval( leftP.x - 2, leftP.y - 2, 4, 4 );
        gc.drawLine( leftP.x, leftP.y, rightP.x, rightP.y );
        leftP = rightP;
      }
    }
    gc.drawOval( leftP.x - 2, leftP.y - 2, 4, 4 );
    gc.setLineWidth( lineWidthBuffer );
    gc.setLineStyle( lineStyleBuffer );
    gc.setForeground( foregroundBuffer );
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilChartLayer#paintDrag(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper,
   *      org.eclipse.swt.graphics.Point, java.lang.Object)
   */
  @Override
  public void paintDrag( GCWrapper gc, Point editing, Object hoverData )
  {
    final EditData editData = (EditData) hoverData;
    final int index = editData.getIndex();
    drawEditLine( gc, editing, index - 1 );
    drawEditLine( gc, editing, index + 1 );

    final Rectangle editRect = RectangleUtils.buffer( editing );
    gc.drawFocus( editRect.x, editRect.y, editRect.width, editRect.height );

  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer#removeYourself()
   */
  public void removeYourself( )
  {
    // do nothing
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    return getLabel();
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilChartLayer#getZOrder()
   */
  @Override
  public int getZOrder( )
  {
    return 10;
  }
}
