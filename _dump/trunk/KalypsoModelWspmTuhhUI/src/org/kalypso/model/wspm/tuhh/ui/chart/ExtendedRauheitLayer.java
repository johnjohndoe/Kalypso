/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraï¿½e 22
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

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
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
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

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
    final IRecord[] points = m_profile.getPoints();
    Rectangle2D bounds = null;
    final int iBreite = m_profile.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    final int iRauheit = m_profile.indexOfProperty( m_rauheit );

    for( final IRecord p : points )
    {
      final Object x = p.getValue( iBreite );
      final Object rauheit = p.getValue( iRauheit );
      if( x == null || rauheit == null )
        continue;
      final Rectangle2D area = new Rectangle2D.Double( (Double) x, (Double) rauheit, 0, 0 );

      if( bounds == null )
        bounds = area;
      else
        bounds.add( area );
    }

    if( bounds == null )
      return null;

    bounds.add( bounds.getX(), bounds.getMinY() * 0.9 );
    return bounds;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.chart.AbstractRauheitLayer#paint(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */

  public void paint( final GCWrapper gc )
  {
    final Color background = gc.getBackground();
    try
    {
      final IProfil profil = getProfil();
      if( profil == null )
        return;
      final IRecord[] points = profil.getPoints();
      IRecord lastP = null;
      final int iBreite = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );
      final int iRauheit = profil.indexOfProperty( m_rauheit );
      if( iRauheit == -1 )
        return;

      for( final IRecord point : points )
      {
        if( lastP != null )
        {
          final Object x1 = lastP.getValue( iBreite );
          final Object x2 = point.getValue( iBreite );
          final Object y2 = lastP.getValue( iRauheit );
          if( x1 != null && x2 != null && y2 != null )
          {
            final Rectangle box = logical2screen( new Rectangle2D.Double( (Double) x1, 0.0, (Double) x2 - (Double) x1, (Double) y2 ) );
            box.width += 1;
            fillRectangle( gc, box );
          }
        }
        lastP = point;
      }
    }
    finally
    {
      gc.setBackground( background );
    }
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.chart.layer.AbstractProfilChartLayer#setActivePoint(java.lang.Object)
   */
  @Override
  public void setActivePoint( final Object data )
  {
    if( data instanceof EditData )
    {
      final EditData editData = (EditData) data;
      final IRecord activePoint = getProfil().getPoints()[editData.getIndex()];
      NullProgressProfilOperation.execute( m_profile, new ActiveObjectEdit( getProfil(), activePoint, null ) );
    }
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilListener#onProfilChanged(org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint,
   *      org.kalypso.model.wspm.core.profil.IProfilChange[])
   */
  @Override
  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {

    if( hint.isPointValuesChanged() )
    {
      final AxisRange valueRange = getValueRange();
      final IComponent rauheit = getProfil().hasPointProperty( m_rauheit );
      final Double maxProfilValue = ProfilUtil.getMaxValueFor( getProfil(), rauheit );
      final Double minProfilValue = ProfilUtil.getMinValueFor( getProfil(), rauheit );
      if( (maxProfilValue != null && minProfilValue != null) && (Math.abs( maxProfilValue - valueRange.getLogicalTo() ) > 0.1 || minProfilValue < valueRange.getLogicalFrom()) )
        valueRange.setLogicalRange( new LogicalRange( minProfilValue * 0.9, maxProfilValue ) );
    }
    if( hint.isMarkerMoved() && getViewData().isVisible( m_rauheit ) )
      updateRauheit();
  }

  @Override
  public IProfilView createLayerPanel( final IProfil profile, final ProfilViewData viewData )
  {
    return new RauheitenPanel( profile, viewData );
  }

  private final void updateRauheit( )
  {
    // TODO:Kim überschreiben der Rauheitsspalte
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.chart.AbstractRauheitLayer#getHoverInfo(org.eclipse.swt.graphics.Point)
   */
  @Override
  public EditInfo getHoverInfo( final Point point )
  {

    final IProfil profil = getProfil();
    final IComponent rauheit = profil.hasPointProperty( m_rauheit );
    final Point2D[] points = ProfilUtil.getPoints2D( profil, rauheit );
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
        return new EditInfo( this, new Rectangle( lp.x, lp.y, 0, 0 ), new EditData( i, rauheit ), String.format( "%.4f[" + text + "]", points[i].getY() ) );
      }
    }
    return null;
  }

  /**
   * @see IProfilChartLayer#removeYourself()
   */
  public void removeYourself( )
  {
    final IProfilChange change = new PointPropertyRemove( m_profile, m_profile.hasPointProperty( m_rauheit ) );
    final ProfilOperation operation = new ProfilOperation( "Datensatz entfernen: " + toString(), m_profile, change, true );
    new ProfilOperationJob( operation ).schedule();
  }

}
