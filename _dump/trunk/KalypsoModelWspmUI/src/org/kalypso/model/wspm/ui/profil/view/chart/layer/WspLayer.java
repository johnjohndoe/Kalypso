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
package org.kalypso.model.wspm.ui.profil.view.chart.layer;

import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.graphics.Region;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.result.IStationResult;
import org.kalypso.model.wspm.ui.profil.view.IProfilView;
import org.kalypso.model.wspm.ui.profil.view.ProfilViewData;
import org.kalypso.model.wspm.ui.profil.view.chart.ProfilChartView;
import org.kalypso.model.wspm.ui.profil.view.panel.WspPanel;

import de.belger.swtchart.EditInfo;
import de.belger.swtchart.axis.AxisRange;

/**
 * Zeigt eine Konstante WSP Linie im Profil
 * 
 * @author Gernot Belger
 */
public class WspLayer extends AbstractProfilChartLayer implements IProfilChartLayer, IStationResult
{
  private final Color m_color;

  private final IProfil m_profil;

  private final IStationResult m_result;

  private double m_height;

  public WspLayer( final ProfilChartView pvp, final AxisRange domainRange, final AxisRange valueRange, final Color color, final IStationResult result )
  {
    super( pvp, domainRange, valueRange );

    m_profil = pvp.getProfil();
    m_color = color;
    m_result = result;
    final Number componentValue = result.getComponentValue( "urn:ogc:gml:dict:kalypso:model:wspm:components#LengthSectionWaterlevel" );
    m_height = componentValue == null ? Double.NaN : componentValue.doubleValue();
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#getBounds()
   */
  public Rectangle2D getBounds( )
  {
    return new Rectangle2D.Double( Double.NaN, m_height, Double.NaN, 0.0 );
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paint(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  public void paint( final GCWrapper gc )
  {
    final Rectangle clipping = gc.getClipping();

    final Point2D point = new Point2D.Double( 0.0, m_height );
    final Point location = logical2screen( point );

    final Region clipreg = new Region();
    final int[] points = getPoints();

    clipreg.add( points );
    clipreg.intersect( clipping );

    final Rectangle toprect = new Rectangle( clipping.x, location.y - 100000, clipping.width, 100000 );
    clipreg.subtract( toprect );

    // if not fill
    if( true )
    {
      final int linesize = 2;
      final Rectangle bottomrect = new Rectangle( clipping.x, location.y + linesize, clipping.width, 10000 );
      clipreg.subtract( bottomrect );
    }

    gc.setClipping( clipreg );

    final Color oldColor = gc.getBackground();
    gc.setBackground( m_color );

    gc.fillRectangle( clipping );

    gc.setBackground( oldColor );
    gc.setClipping( clipping );
  }

  private int[] getPoints( )
  {
    // ermittelt das Polygon oberhalb der gel‰ndelinie
    final List<IProfilPoint> ppoints = m_profil.getPoints();
    final List<Point> points = new ArrayList<Point>( (ppoints.size() + 2) * 2 );
    for( int i = 0; i < ppoints.size(); i++ )
    {
      final IProfilPoint p = ppoints.get( i );

      try
      {
        final double x = p.getValueFor( POINT_PROPERTY.BREITE );
        final double y = p.getValueFor( POINT_PROPERTY.HOEHE );

        final Point point = logical2screen( new Point2D.Double( x, y ) );

        if( i == 0 )
          points.add( new Point( point.x, -1000 ) );

        points.add( point );

        if( i == ppoints.size() - 1 )
          points.add( new Point( point.x, -1000 ) );
      }
      catch( Exception e )
      {
        // should never happen
      }
    }

    final int[] ps = new int[points.size() * 2];
    int count = 0;
    for( int i = 0; i < points.size(); i++ )
    {
      final Point p = points.get( i );
      ps[count++] = p.x;
      ps[count++] = p.y;
    }

    return ps;
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paintLegend(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  @Override
  public void paintLegend( final GCWrapper gc )
  {
    final Rectangle clipping = gc.getClipping();
    final int mid = clipping.y + clipping.width / 2;

    gc.setForeground( m_color );
    gc.drawLine( clipping.x, mid, clipping.x + clipping.width, mid );
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    return m_result.getName();
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#getHoverInfo(org.eclipse.swt.graphics.Point)
   */
  @Override
  public EditInfo getHoverInfo( Point point )
  {
    return null;
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paintDrag(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper,
   *      org.eclipse.swt.graphics.Point, java.lang.Object)
   */
  @Override
  public void paintDrag( final GCWrapper gc, final Point editing, final Object hoverData )
  {
  }

  @Override
  public IProfilView createLayerPanel( final IProfilEventManager pem, final ProfilViewData viewData )
  {
    return new WspPanel( pem, viewData, m_result );
  }

  public void removeYourself( )
  {
    throw new UnsupportedOperationException();
  }

  public IStationResult getResult( )
  {
    return m_result;
  }

  /**
   * @see org.kalypso.model.wspm.core.result.IStationResult#getName()
   */
  public String getName( )
  {
    return m_result.getName();
  }

  /**
   * @see org.kalypso.model.wspm.core.result.IStationResult#getComponentIds()
   */
  public String[] getComponentIds( )
  {
    return m_result.getComponentIds();
  }

  /**
   * @see org.kalypso.model.wspm.core.result.IStationResult#getComponentName(java.lang.String)
   */
  public String getComponentName( final String componentId )
  {
    return m_result.getComponentName( componentId );
  }

  /**
   * @see org.kalypso.model.wspm.core.result.IStationResult#getComponentValue(java.lang.String)
   */
  public Number getComponentValue( final String componentId )
  {
    return m_result.getComponentValue( componentId );
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
