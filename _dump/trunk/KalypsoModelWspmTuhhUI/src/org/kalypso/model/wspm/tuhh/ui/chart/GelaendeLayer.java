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
package org.kalypso.model.wspm.tuhh.ui.chart;

import org.eclipse.jface.resource.ColorRegistry;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.changes.PointAdd;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyEdit;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.util.ProfilObsHelper;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.panel.GelaendePanel;
import org.kalypso.model.wspm.ui.view.IProfilView;
import org.kalypso.model.wspm.ui.view.ProfilViewData;
import org.kalypso.model.wspm.ui.view.chart.AbstractPolyLineLayer;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;
import org.kalypso.observation.result.IRecord;

import de.belger.swtchart.axis.AxisRange;
import de.belger.swtchart.util.LogicalRange;

/**
 * @author kimwerner
 */
public class GelaendeLayer extends AbstractPolyLineLayer
{
  public GelaendeLayer( final ProfilChartView pcv )
  {
    super( IWspmTuhhConstants.LAYER_GELAENDE, "Gel�nde", pcv, pcv.getDomainRange(), pcv.getValueRangeLeft(), ProfilObsHelper.getPropertyFromId( pcv.getProfil(), new String[] { IWspmConstants.POINT_PROPERTY_HOEHE } ), true, true, true );
    setColors( setColor( pcv.getColorRegistry() ) );
  }

  private final Color[] setColor( final ColorRegistry cr )
  {
    if( !cr.getKeySet().contains( IWspmTuhhConstants.LAYER_GELAENDE ) )
      cr.put( IWspmTuhhConstants.LAYER_GELAENDE, new RGB( 255, 150, 0 ) );
    return new Color[] { cr.get( IWspmTuhhConstants.LAYER_GELAENDE ) };
  }

  @Override
  public IProfilView createLayerPanel( final IProfilEventManager pem, final ProfilViewData viewData )
  {
    return new GelaendePanel( pem, viewData );
  }

  @Override
  public IRecord[] getPoints( )
  {
    return getProfil().getPoints();
  }

  /**
   * @see com.bce.eind.core.profil.IProfilListener#onProfilChanged(com.bce.eind.core.profil.changes.ProfilChangeHint,
   *      com.bce.eind.core.profil.IProfilChange[])
   */
  @Override
  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
    // FIXME - insert new point at start or end of profile - chart view resized() event must be generated

    if( !(hint.isPointValuesChanged() || hint.isPointsChanged()) )
      return;
    final AxisRange domainRange = getDomainRange();
    final AxisRange valueRange = getValueRange();

    final double left = domainRange.getLogicalFrom();
    final double right = domainRange.getLogicalTo();
    final double top = valueRange.getLogicalTo();
    final double bottom = valueRange.getLogicalFrom();
    for( final IProfilChange change : changes )
      if( change instanceof PointPropertyEdit || change instanceof PointAdd )
        for( final IRecord point : (IRecord[]) change.getObjects() )
          try
          {
            final double breite = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BREITE ) );
            final double hoehe = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_HOEHE ) );

            if( breite > right || breite < left || hoehe > top || hoehe < bottom )
            {
              valueRange.setLogicalRange( new LogicalRange( Math.min( hoehe, bottom ), Math.max( hoehe, top ) ) );
              domainRange.setLogicalRange( new LogicalRange( Math.min( breite, left ), Math.max( breite, right ) ) );
            }
          }
          catch( final Exception e )
          {
            return;
          }
  }

  @Override
  public void paintLegend( final GCWrapper gc )
  {
    final Rectangle clipping = gc.getClipping();

    final int left = clipping.x;
    final int top = clipping.y;
    final int right = clipping.x + clipping.width;
    final int bottom = clipping.y + clipping.width;
    final int midx = (left + right) / 2;
    final int midy = (top + bottom) / 2;

    drawStationline( gc, midx, midy, midx, bottom );
    gc.setLineWidth( 1 );
    gc.setLineStyle( SWT.LINE_SOLID );
    gc.setForeground( m_colors[0] );
    gc.drawOval( midx - 2, midy - 2, 4, 4 );
    gc.drawLine( left, top, midx, midy );
    gc.drawLine( midx, midy, right, midy );
  }

  /**
   * @see IProfilChartLayer#removeYourself()
   */
  public void removeYourself( )
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public String toString( )
  {
    return "Gel�ndeh�he";
  }
}
