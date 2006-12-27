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
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilConstants;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfil.PROFIL_PROPERTY;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.changes.ActiveObjectEdit;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyRemove;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.ui.chart.AbstractPolyLineLayer.EditData;
import org.kalypso.model.wspm.tuhh.ui.panel.RauheitenPanel;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.view.IProfilView;
import org.kalypso.model.wspm.ui.view.ProfilViewData;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;

import de.belger.swtchart.EditInfo;
import de.belger.swtchart.axis.AxisRange;
import de.belger.swtchart.util.LogicalRange;

/**
 * @author Gernot Belger
 */
public class RauheitLayer extends AbstractProfilChartLayer implements IProfilChartLayer
{
  private final Color m_color;

  // private final IProfil m_profil;

  private final Color m_fillColor;

  private IProfilEventManager m_pem;

  public RauheitLayer( final ProfilChartView pvp, final AxisRange domainRange, final AxisRange valueRange, final Color color, final Color fillColor )
  {
    super( pvp, domainRange, valueRange, false );

    m_pem = pvp.getProfilEventManager();
    m_color = color;
    m_fillColor = fillColor;

  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#getBounds()
   */
  public Rectangle2D getBounds( )
  {
    final List<IProfilPoint> points = m_pem.getProfil().getPoints();

    Rectangle2D bounds = null;

    try
    {
      for( final IProfilPoint p : points )
      {
        final double x = p.getValueFor( POINT_PROPERTY.BREITE );

        final double rauheit = p.getValueFor( POINT_PROPERTY.RAUHEIT );
        final Rectangle2D area = new Rectangle2D.Double( x, rauheit, 0, 0 );

        if( bounds == null )
          bounds = area;
        else
          bounds.add( area );
      }
    }
    catch( final Exception e )
    {
      // should never happen
      e.printStackTrace();
    }

    bounds.add( bounds.getX(), bounds.getMinY() * 0.9 );
    return bounds;
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paint(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  public void paint( final GCWrapper gc )
  {
    final Color background = gc.getBackground();
    final IProfil profil = getProfil();
    if( profil == null )
      return;
    IProfilDevider[] deviders = profil.getDevider( new String[] { IProfilConstants.DEVIDER_TYP_TRENNFLAECHE, IProfilConstants.DEVIDER_TYP_DURCHSTROEMTE } );
    List<IProfilPoint> points = new ArrayList<IProfilPoint>();
    double[] values;
    if( getViewData().useDeviderValue() && deviders.length == 4 )
    {
      values = new double[4];
      int i = 0;
      for( IProfilDevider dev : deviders )
      {
        final Double value = (Double) dev.getValueFor( IProfilPoint.POINT_PROPERTY.RAUHEIT );
        try
        {
          values[i++] = value == null ? dev.getPoint().getValueFor( IProfilPoint.POINT_PROPERTY.RAUHEIT ) : value;
        }
        catch( ProfilDataException e )
        {
          values[i++] = 0.0;
        }
        points.add( dev.getPoint() );
      }
    }
    else
    {
      points = getProfil().getPoints();
      try
      {
        values = ProfilUtil.getValuesFor( getProfil(), POINT_PROPERTY.RAUHEIT );
      }
      catch( ProfilDataException e )
      {
        values = new double[points.size()];
      }
    }

    IProfilPoint lastP = null;
    int i = 0;
    for( final Iterator<IProfilPoint> pIt = points.iterator(); pIt.hasNext(); )
    {
      final IProfilPoint p = pIt.next();

      if( lastP != null )
      {
        try
        {
          final double x1 = lastP.getValueFor( POINT_PROPERTY.BREITE );
          final double x2 = p.getValueFor( POINT_PROPERTY.BREITE );
          // final double y1 = 0;
          // final double y2 = lastP.getValueFor( POINT_PROPERTY.RAUHEIT );
          final Rectangle box = logical2screen( new Rectangle2D.Double( x1, 0.0, x2 - x1, values[i++] ) );
          box.width += 1;
          fillRectangle( gc, box );
        }
        catch( final ProfilDataException e )
        {
          // sollte nie passieren
        }
      }
      lastP = p;

    }
    gc.setBackground( background );
  }

  private void fillRectangle( final GCWrapper gc, final Rectangle box )
  {
    // gc.setAlpha( 50 );
    gc.setForeground( m_color );
    gc.setBackground( m_fillColor );
    gc.fillRectangle( box );
    // gc.drawRectangle( box );
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#getHoverInfo(org.eclipse.swt.graphics.Point)
   */
  @Override
  public EditInfo getHoverInfo( final Point point )
  {
    final IProfil profil = getProfil();
    final Point2D[] points = ProfilUtil.getPoints2D( profil, POINT_PROPERTY.RAUHEIT );
    if( points == null || points.length < 2 )
      return null;
    Rectangle hover = null;
    final int baseLine = logical2screen( new Point2D.Double( 0, 0 ) ).y;
    for( int i = 0; i < points.length - 1; i++ )
    {
      final Point lp = logical2screen( points[i] );
      final Point rp = logical2screen( points[i + 1] );

      hover = new Rectangle( lp.x, lp.y, rp.x - lp.x, baseLine - lp.y );
      if( hover.contains( point ) )
        return new EditInfo( this, new Rectangle( lp.x, lp.y, 0, 0 ), new EditData( i, POINT_PROPERTY.RAUHEIT ), String.format( "%.4f[" + profil.getProperty( PROFIL_PROPERTY.RAUHEIT_TYP ).toString()
            + "]", points[i].getY() ) );
    }
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

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    final Object rw = m_pem.getProfil().getProperty( PROFIL_PROPERTY.RAUHEIT_TYP );
    if (IProfilConstants.RAUHEIT_TYP_KS.equals( rw))return "Rauheit Typ ks";
    if (IProfilConstants.RAUHEIT_TYP_KST.equals( rw))return "Rauheit Typ kst";
    return "Rauheit Typ unbekannt";
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paintLegend(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  @Override
  public void paintLegend( final GCWrapper gc )
  {
    final Rectangle clipping = gc.getClipping();
    fillRectangle( gc, clipping );
  }

  @Override
  public IProfilView createLayerPanel( final IProfilEventManager pem, final ProfilViewData viewData )
  {
    return new RauheitenPanel( pem, viewData );
  }

  /**
   * @see IProfilChartLayer#removeYourself()
   */
  public void removeYourself( )
  {
    final IProfilChange change = new PointPropertyRemove( m_pem.getProfil(), POINT_PROPERTY.RAUHEIT );

    final ProfilOperation operation = new ProfilOperation( "Datensatz entfernen: " + toString(), m_pem, change, true );
    new ProfilOperationJob( operation ).schedule();
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.chart.layer.AbstractProfilChartLayer#editProfil(org.eclipse.swt.graphics.Point,
   *      java.lang.Object)
   */
  @Override
  protected void editProfil( Point point, Object data )
  {
    // no operation available
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilListener#onProfilChanged(org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint,
   *      org.kalypso.model.wspm.core.profil.IProfilChange[])
   */
  @Override
  public void onProfilChanged( ProfilChangeHint hint, IProfilChange[] changes )
  {

    if( !hint.isPointValuesChanged() )
      return;
    final AxisRange valueRange = getValueRange();
    final double maxProfilValue = ProfilUtil.getMaxValueFor( getProfil(), POINT_PROPERTY.RAUHEIT );
    final double minProfilValue = ProfilUtil.getMinValueFor( getProfil(), POINT_PROPERTY.RAUHEIT );
    if( Math.abs( maxProfilValue - valueRange.getLogicalTo() ) > 0.1 || minProfilValue < valueRange.getLogicalFrom() )
      valueRange.setLogicalRange( new LogicalRange( minProfilValue * 0.9, maxProfilValue ) );
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.chart.layer.AbstractProfilChartLayer#setActivePoint(java.lang.Object)
   */
  @Override
  public void setActivePoint( Object data )
  {
    if( data instanceof EditData )
    {
      final EditData editData = (EditData) data;
      final IProfilPoint activePoint = getProfil().getPoints().get( editData.getIndex() );
      final ProfilOperation operation = new ProfilOperation( "", getProfilEventManager(), new ActiveObjectEdit( getProfil(), activePoint, null ), true );
      final IStatus status = operation.execute( new NullProgressMonitor(), null );
      operation.dispose();
      if( !status.isOK() )
        KalypsoModelWspmUIPlugin.getDefault().getLog().log( status );
    }
  }
}
