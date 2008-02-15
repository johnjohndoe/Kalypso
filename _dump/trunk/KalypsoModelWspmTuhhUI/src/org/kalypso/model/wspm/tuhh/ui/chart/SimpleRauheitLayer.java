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

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.util.ProfilObsHelper;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.panel.RauheitenPanel;
import org.kalypso.model.wspm.ui.view.IProfilView;
import org.kalypso.model.wspm.ui.view.ProfilViewData;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;
import org.kalypso.model.wspm.ui.view.chart.AbstractPolyLineLayer.EditData;
import org.kalypso.observation.result.IRecord;

import de.belger.swtchart.EditInfo;
import de.belger.swtchart.axis.AxisRange;
import de.belger.swtchart.util.LogicalRange;

/**
 * @author kimwerner
 */
public class SimpleRauheitLayer extends AbstractRauheitLayer
{
  final String m_rauheit;

  public SimpleRauheitLayer( final ProfilChartView pcv, final String layerId, final String label )
  {
    super( pcv, layerId, label, new RGB( 220, 220, 220 ) );
    m_rauheit = layerId == IWspmTuhhConstants.LAYER_RAUHEIT_KST ? IWspmConstants.POINT_PROPERTY_RAUHEIT_KST : IWspmConstants.POINT_PROPERTY_RAUHEIT_KS;

  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.chart.AbstractRauheitLayer#getBounds()
   */

  public Rectangle2D getBounds( )
  {
    final IProfil profil = getProfil();
    if( profil == null )
      return MINIMAL_RECT;
    final Rectangle2D bounds = MINIMAL_RECT;
    final IProfilPointMarker[] deviders1 = profil.getPointMarkerFor( ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) );
    final IProfilPointMarker[] deviders2 = profil.getPointMarkerFor( ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
    for( final IProfilPointMarker dev : deviders1 )
    {
      final Double value = (Double) dev.getValue();
      final double y = value == null ? 0.0 : value;
      final IRecord point = dev.getPoint();
      final Double breite = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BREITE ) );
      final double x = breite == null ? 0.0 : breite;
      final Rectangle2D area = new Rectangle2D.Double( x, y, 0, 0 );
      bounds.add( area );
    }
    for( final IProfilPointMarker dev : deviders2 )
    {
      final Double value = (Double) dev.getValue();
      final double y = value == null ? 0.0 : value;
      final IRecord point = dev.getPoint();
      final Double breite = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BREITE ) );
      final double x = breite == null ? 0.0 : breite;
      final Rectangle2D area = new Rectangle2D.Double( x, y, 0, 0 );
      bounds.add( area );
    }

    bounds.add( bounds.getX(), bounds.getMinY() * 0.9 );
    return bounds;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilListener#onProfilChanged(org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint,
   *      org.kalypso.model.wspm.core.profil.IProfilChange[])
   */

  @Override
  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {

    if( !hint.isMarkerDataChanged() )
      return;
    final AxisRange valueRange = getValueRange();

    final IProfilPointMarker[] deviders1 = getProfil().getPointMarkerFor( ProfilObsHelper.getPropertyFromId( getProfil(), IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) );
    final IProfilPointMarker[] deviders2 = getProfil().getPointMarkerFor( ProfilObsHelper.getPropertyFromId( getProfil(), IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );

    double maxProfilValue = 0.0;
    double minProfilValue = 0.0;
    for( final IProfilPointMarker dev : deviders1 )
    {
      final Double value = (Double) dev.getValue();
      final double y = value == null ? 0.0 : value;
      minProfilValue = Math.min( minProfilValue, y );
      maxProfilValue = Math.max( maxProfilValue, y );
    }
    for( final IProfilPointMarker dev : deviders2 )
    {
      final Double value = (Double) dev.getValue();
      final double y = value == null ? 0.0 : value;
      minProfilValue = Math.min( minProfilValue, y );
      maxProfilValue = Math.max( maxProfilValue, y );
    }

    if( Math.abs( maxProfilValue - valueRange.getLogicalTo() ) > 0.1 || minProfilValue < valueRange.getLogicalFrom() )
      valueRange.setLogicalRange( new LogicalRange( minProfilValue * 0.9, maxProfilValue ) );
  }

  @Override
  public IProfilView createLayerPanel( final IProfil profile, final ProfilViewData viewData )
  {
    return new RauheitenPanel( profile, viewData );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.chart.AbstractRauheitLayer#paint(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */

  private void getRauheiten( final Double[] values, final Double[] breite )
  {
    final IProfilPointMarker[] durchstroemte = getProfil().getPointMarkerFor( ProfilObsHelper.getPropertyFromId( getProfil(), IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) );
    final IProfilPointMarker[] trennflaechen = getProfil().getPointMarkerFor( ProfilObsHelper.getPropertyFromId( getProfil(), IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );

    if( durchstroemte.length > 1 && trennflaechen.length > 1 )
    {

      values[0] = (Double) durchstroemte[0].getValue();
      if( values[0] == null )
      {
        durchstroemte[0].setValue( 0.0 );
        values[0] = 0.0;
      }

      breite[0] = (Double) durchstroemte[0].getPoint().getValue( ProfilObsHelper.getPropertyFromId( durchstroemte[0].getPoint(), IWspmConstants.POINT_PROPERTY_BREITE ) );

      values[1] = (Double) trennflaechen[0].getValue();
      if( values[1] == null )
      {
        trennflaechen[0].setValue( 0.0 );
        values[1] = 0.0;
      }
      breite[1] = (Double) trennflaechen[0].getPoint().getValue( ProfilObsHelper.getPropertyFromId( trennflaechen[0].getPoint(), IWspmConstants.POINT_PROPERTY_BREITE ) );

      values[2] = (Double) trennflaechen[trennflaechen.length - 1].getValue();
      if( values[2] == null )
      {
        trennflaechen[trennflaechen.length - 1].setValue( 0.0 );
        values[2] = 0.0;
      }

      breite[2] = (Double) trennflaechen[trennflaechen.length - 1].getPoint().getValue( ProfilObsHelper.getPropertyFromId( trennflaechen[trennflaechen.length - 1].getPoint(), IWspmConstants.POINT_PROPERTY_BREITE ) );
      values[3] = (Double) durchstroemte[durchstroemte.length - 1].getValue();
      if( values[3] == null )
      {
        durchstroemte[durchstroemte.length - 1].setValue( 0.0 );
        values[3] = 0.0;
      }
      breite[3] = (Double) durchstroemte[durchstroemte.length - 1].getPoint().getValue( ProfilObsHelper.getPropertyFromId( durchstroemte[durchstroemte.length - 1].getPoint(), IWspmConstants.POINT_PROPERTY_BREITE ) );
    }
  }

  public void paint( final GCWrapper gc )
  {
    final Color background = gc.getBackground();
    final Double[] values = new Double[4];
    final Double[] breite = new Double[4];
    getRauheiten( values, breite );
// final IProfil profil = getProfil();
// if( profil == null )
// return;
// IProfilPointMarker[] durchstroemte = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
// IProfilPointMarker[] trennflaechen = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
//
// Double[] values;
// IProfilPoint[] points;
// if( durchstroemte.length > 1 && trennflaechen.length > 1 )
// {
// values = new Double[4];
// points = new IProfilPoint[4];
// values[0] = (Double) durchstroemte[0].getValueFor( IWspmTuhhConstants.POINTMARKER_PROPERTY_RAUHEIT );
// if( values[0] == null )
// {
// durchstroemte[0].setValueFor( IWspmTuhhConstants.POINTMARKER_PROPERTY_RAUHEIT, 0.0 );
// values[0] = 0.0;
// }
// points[0] = durchstroemte[0].getPoint();
//
// values[1] = (Double) trennflaechen[0].getValueFor( IWspmTuhhConstants.POINTMARKER_PROPERTY_RAUHEIT );
// if( values[1] == null )
// {
// trennflaechen[0].setValueFor( IWspmTuhhConstants.POINTMARKER_PROPERTY_RAUHEIT, 0.0 );
// values[1] = 0.0;
// }
// points[1] = trennflaechen[0].getPoint();
//
// values[2] = (Double) trennflaechen[trennflaechen.length - 1].getValueFor(
// IWspmTuhhConstants.POINTMARKER_PROPERTY_RAUHEIT );
// if( values[2] == null )
// {
// trennflaechen[trennflaechen.length - 1].setValueFor( IWspmTuhhConstants.POINTMARKER_PROPERTY_RAUHEIT, 0.0 );
// values[2] = 0.0;
// }
// points[2] = trennflaechen[trennflaechen.length - 1].getPoint();
// values[3] = (Double) durchstroemte[durchstroemte.length - 1].getValueFor(
// IWspmTuhhConstants.POINTMARKER_PROPERTY_RAUHEIT );
// if( values[3] == null )
// {
// durchstroemte[durchstroemte.length - 1].setValueFor( IWspmTuhhConstants.POINTMARKER_PROPERTY_RAUHEIT, 0.0 );
// values[3] = 0.0;
// }
// points[3] = durchstroemte[durchstroemte.length - 1].getPoint();
    for( int i = 0; i < 3; i++ )
    {
// final double x1 = points[i].getValueFor( IWspmTuhhConstants.POINT_PROPERTY_BREITE );
// final double x2 = points[i + 1].getValueFor( IWspmTuhhConstants.POINT_PROPERTY_BREITE );
// final Rectangle box = logical2screen( new Rectangle2D.Double( x1, 0.0, x2 - x1, values[i] ) );
      final Rectangle box = logical2screen( new Rectangle2D.Double( breite[i], 0.0, breite[i + 1] - breite[i], values[i] ) );
      box.width += 1;
      fillRectangle( gc, box );
// }
    }
    gc.setBackground( background );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.chart.AbstractRauheitLayer#getHoverInfo(org.eclipse.swt.graphics.Point)
   */
  @Override
  public EditInfo getHoverInfo( final Point point )
  {
    final Double[] values = new Double[4];
    final Double[] breite = new Double[4];
    getRauheiten( values, breite );
    Rectangle hover = null;
    for( int i = 0; i < 3; i++ )
    {
      final Point lp = logical2screen( new Point2D.Double( breite[i], values[i] ) );
      final Point rp = logical2screen( new Point2D.Double( breite[i + 1], values[i + 1] ) );
      hover = new Rectangle( lp.x, lp.y, rp.x - lp.x, getValueRange().getScreenFrom() - lp.y );
      if( hover.contains( point ) )
      {
        final EditData editData = new EditData( i, ProfilObsHelper.getPropertyFromId( getProfil(), m_rauheit ) );
        final String text = String.format( "%.4f[" + m_rauheit == IWspmConstants.POINT_PROPERTY_RAUHEIT_KST ? "kst" : "ks" + "]", values[i] );

        return new EditInfo( this, new Rectangle( lp.x, lp.y, 0, 0 ), editData, text );
      }
    }
    return null;
  }

  /**
   * @see IProfilChartLayer#removeYourself()
   */
  public void removeYourself( )
  {
// final IProfilChange change = new PointPropertyRemove( m_pem.getProfil(), IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT );
// final ProfilOperation operation = new ProfilOperation( "Datensatz entfernen: " + toString(), m_pem, change, true );
// new ProfilOperationJob( operation ).schedule();
  }

}
