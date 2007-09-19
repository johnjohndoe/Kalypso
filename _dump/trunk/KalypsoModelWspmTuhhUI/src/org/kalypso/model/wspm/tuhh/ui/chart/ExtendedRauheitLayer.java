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
import java.awt.geom.Rectangle2D;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.changes.ActiveObjectEdit;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyRemove;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.panel.RauheitenPanel;
import org.kalypso.model.wspm.ui.profil.operation.NullProgressProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.view.IProfilView;
import org.kalypso.model.wspm.ui.view.ProfilViewData;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;
import org.kalypso.model.wspm.ui.view.chart.AbstractPolyLineLayer.EditData;

import de.belger.swtchart.EditInfo;
import de.belger.swtchart.axis.AxisRange;
import de.belger.swtchart.util.LogicalRange;

/**
 * @author kimwerner
 */
public class ExtendedRauheitLayer extends AbstractRauheitLayer
{

  final String m_rauheit;

  public ExtendedRauheitLayer( final ProfilChartView pcv, final String layerId, final String label )
  {
    super( pcv, layerId, label, new RGB( 220, 220, 220 ) );
    m_rauheit = layerId == IWspmTuhhConstants.LAYER_RAUHEIT_KST ? IWspmConstants.POINT_PROPERTY_RAUHEIT_KST : IWspmConstants.POINT_PROPERTY_RAUHEIT_KS;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.chart.AbstractRauheitLayer#getBounds()
   */

  public Rectangle2D getBounds( )
  {

    final List<IProfilPoint> points = m_pem.getProfil().getPoints();
    Rectangle2D bounds = null;
    for( final IProfilPoint p : points )
    {
      final double x = p.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_BREITE );

      final double rauheit = p.getValueFor( m_rauheit );
      final Rectangle2D area = new Rectangle2D.Double( x, rauheit, 0, 0 );

      if( bounds == null )
        bounds = area;
      else
        bounds.add( area );
    }
    bounds.add( bounds.getX(), bounds.getMinY() * 0.9 );
    return bounds;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.chart.AbstractRauheitLayer#paint(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */

  public void paint( GCWrapper gc )
  {

    final Color background = gc.getBackground();
    final IProfil profil = getProfil();
    if( profil == null )
      return;
    final LinkedList<IProfilPoint> points = profil.getPoints();
    IProfilPoint lastP = null;
    for( final IProfilPoint point : points )
    {
      if( lastP != null )
      {
        final double x1 = lastP.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_BREITE );
        final double x2 = point.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_BREITE );
        final double y2 = lastP.getValueFor( m_rauheit );
        final Rectangle box = logical2screen( new Rectangle2D.Double( x1, 0.0, x2 - x1, y2 ) );
        box.width += 1;
        fillRectangle( gc, box );
      }
      lastP = point;
    }
    gc.setBackground( background );
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
      NullProgressProfilOperation.execute( m_pem, new ActiveObjectEdit( getProfil(), activePoint, null ) );
    }
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilListener#onProfilChanged(org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint,
   *      org.kalypso.model.wspm.core.profil.IProfilChange[])
   */
  @Override
  public void onProfilChanged( ProfilChangeHint hint, IProfilChange[] changes )
  {

    if( hint.isPointValuesChanged() )
    {
      final AxisRange valueRange = getValueRange();
      final double maxProfilValue = ProfilUtil.getMaxValueFor( getProfil(), m_rauheit );
      final double minProfilValue = ProfilUtil.getMinValueFor( getProfil(), m_rauheit );
      if( Math.abs( maxProfilValue - valueRange.getLogicalTo() ) > 0.1 || minProfilValue < valueRange.getLogicalFrom() )
        valueRange.setLogicalRange( new LogicalRange( minProfilValue * 0.9, maxProfilValue ) );
    }
    if( hint.isMarkerMoved() && getViewData().isVisible( m_rauheit ) )
    {
      updateRauheit();
    }
  }

  @Override
  public IProfilView createLayerPanel( final IProfilEventManager pem, final ProfilViewData viewData )
  {
    return new RauheitenPanel( pem, viewData );
  }

  private final void updateRauheit( )
  {
// TODO:Kim überschreiben der Rauheitsspalte
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.chart.AbstractRauheitLayer#getHoverInfo(org.eclipse.swt.graphics.Point)
   */
  @Override
  public EditInfo getHoverInfo( Point point )
  {

    final IProfil profil = getProfil();
    final Point2D[] points = ProfilUtil.getPoints2D( profil, m_rauheit );
    if( points == null || points.length < 2 )
      return null;
    Rectangle hover = null;
    for( int i = 0; i < points.length - 1; i++ )
    {
      final Point lp = logical2screen( points[i] );
      final Point rp = logical2screen( points[i + 1] );
      if( lp.y == 0 )
        return null;
      hover = new Rectangle( lp.x, lp.y, rp.x - lp.x, getValueRange().getScreenFrom() - lp.y );
      if( hover.contains( point ) )
      {
        final String text = m_rauheit == IWspmConstants.POINT_PROPERTY_RAUHEIT_KST ? "kst" : "ks";
        return new EditInfo( this, new Rectangle( lp.x, lp.y, 0, 0 ), new EditData( i, m_rauheit ), String.format( "%.4f[" + text + "]", points[i].getY() ) );
      }
    }
    return null;
  }

  /**
   * @see IProfilChartLayer#removeYourself()
   */
  public void removeYourself( )
  {
    final IProfilChange change = new PointPropertyRemove( m_pem.getProfil(), m_rauheit );
    final ProfilOperation operation = new ProfilOperation( "Datensatz entfernen: " + toString(), m_pem, change, true );
    new ProfilOperationJob( operation ).schedule();
  }

}
