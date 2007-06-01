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

import java.awt.geom.Rectangle2D;

import org.eclipse.jface.resource.ColorRegistry;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilChartLayer;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;

import de.belger.swtchart.EditInfo;
import de.belger.swtchart.axis.AxisRange;
import de.belger.swtchart.util.LogicalRange;

/**
 * @author Gernot Belger
 */
public abstract class AbstractRauheitLayer extends AbstractProfilChartLayer implements IProfilChartLayer
{
  private final Color m_color;

  private final Color m_fillColor;

  protected IProfilEventManager m_pem;

  public AbstractRauheitLayer( final ProfilChartView pcv ,final String layerId,final String label)

  {
    super( layerId, pcv, pcv.getDomainRange(), pcv.getValueRangeRight(), label, false );
    m_pem = pcv.getProfilEventManager();
    final ColorRegistry cr = pcv.getColorRegistry();
    if( !cr.getKeySet().contains( IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT ) )
    {
      cr.put(IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT , new RGB( 220, 220, 220 ) );
      cr.put( IWspmTuhhConstants.LAYER_RAUHEIT_COLOR_BACKGROUND, new RGB( 220, 220, 220 ) );
    }
    m_color = cr.get( IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT);
    m_fillColor = pcv.getColorRegistry().get( IWspmTuhhConstants.LAYER_RAUHEIT_COLOR_BACKGROUND );

  }

 
  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilChartLayer#editProfil(org.eclipse.swt.graphics.Point,
   *      java.lang.Object)
   */
  @Override
  protected void editProfil( Point point, Object data )
  {
    // no editing

  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#getBounds()
   */
  public abstract Rectangle2D getBounds( );

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    final Object rw = m_pem.getProfil().getProperty( IWspmTuhhConstants.RAUHEIT_TYP );
    if( IWspmTuhhConstants.RAUHEIT_TYP_KS.equals( rw ) )
      return "Rauheit Typ ks";
    if( IWspmTuhhConstants.RAUHEIT_TYP_KST.equals( rw ) )
      return "Rauheit Typ kst";
    return "Rauheit Typ unbekannt";
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paint(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  public abstract void paint( final GCWrapper gc );

  protected void fillRectangle( final GCWrapper gc, final Rectangle box )
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
  public abstract EditInfo getHoverInfo( final Point point );

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paintLegend(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  @Override
  public void paintLegend( final GCWrapper gc )
  {
    final Rectangle clipping = gc.getClipping();
    fillRectangle( gc, clipping );
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
    final double maxProfilValue = ProfilUtil.getMaxValueFor( getProfil(), IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT );
    final double minProfilValue = ProfilUtil.getMinValueFor( getProfil(), IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT );
    if( Math.abs( maxProfilValue - valueRange.getLogicalTo() ) > 0.1 || minProfilValue < valueRange.getLogicalFrom() )
      valueRange.setLogicalRange( new LogicalRange( minProfilValue * 0.9, maxProfilValue ) );
  }

}