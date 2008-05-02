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

import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;

/**
 * @author kimwerner
 */
public class TrapezBuildingLayer extends AbstractBuildingLayer
{
  public TrapezBuildingLayer( final ProfilChartView pcv )
  {
    super( IWspmTuhhConstants.LAYER_TRAPEZ, "Trapez-Durchla�", pcv );
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#getBounds()
   */
  @Override
  public Rectangle2D getBounds( )
  {
    try
    {
      final double[] trapezArray = createTrapez();
      return new Rectangle2D.Double( trapezArray[6], trapezArray[1], trapezArray[4] - trapezArray[6], trapezArray[5] - trapezArray[3] );
    }
    catch( final Exception e )
    {
      return new Rectangle2D.Double( Double.NaN, Double.NaN, Double.NaN, Double.NaN );
    }
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paintLegend(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  @Override
  public void paintLegend( final GCWrapper gc )
  {
    final Rectangle clipping = gc.getClipping();

    final Color background = gc.getBackground();

    gc.setBackground( getColor() );
    final int[] trapez = { clipping.x + 6, clipping.height - 2, clipping.width - 6, clipping.height - 2, clipping.width - 2, clipping.y + 2, clipping.x + 2, clipping.y + 2 };
    gc.fillPolygon( trapez );
    gc.drawPolygon( trapez );
    gc.setBackground( background );
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paint(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  @Override
  public void paint( final GCWrapper gc )
  {

    final double[] trapezArray = createTrapez();

    if( trapezArray == null )
      return;
    final Color background = gc.getBackground();
    gc.setBackground( getColor() );

    final Rectangle2D trapezRect = new Rectangle2D.Double( trapezArray[0], trapezArray[1], trapezArray[2] - trapezArray[0], trapezArray[5] - trapezArray[1] );
    final Rectangle trapezScreen = logical2screen( trapezRect );
    final Point2D topLeft = new Point2D.Double( trapezArray[6], trapezArray[7] );
    final Point2D topRight = new Point2D.Double( trapezArray[4], trapezArray[5] );
    final Point topLeftScreen = logical2screen( topLeft );
    final Point topRightScreen = logical2screen( topRight );
    final int[] trapezScreenArray = new int[8];
    trapezScreenArray[0] = trapezScreen.x;
    trapezScreenArray[1] = trapezScreen.y;

    trapezScreenArray[2] = trapezScreen.x + trapezScreen.width;
    trapezScreenArray[3] = trapezScreen.y;

    trapezScreenArray[4] = topRightScreen.x;
    trapezScreenArray[5] = topRightScreen.y;

    trapezScreenArray[6] = topLeftScreen.x;
    trapezScreenArray[7] = topLeftScreen.y;

    gc.fillPolygon( trapezScreenArray );

    gc.setBackground( background );
  }

  private double[] createTrapez( )
  {
    final IProfileObject building = getBuilding();
    final Double bezX = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X, building );
    final Double bezY = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y, building );
    final Double lang = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BREITE, building );
    final Double hoch = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_HOEHE, building );
    final Double delta = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_STEIGUNG, building );
    if( bezX.isNaN() || bezY.isNaN() || lang.isNaN() || hoch.isNaN() || delta.isNaN() )
      return null;

    final Double dx = hoch * delta;

    final double[] trapezArray = new double[8];
    trapezArray[0] = bezX - (lang / 2);
    trapezArray[1] = bezY;

    trapezArray[2] = bezX + (lang / 2);
    trapezArray[3] = bezY;

    trapezArray[4] = bezX + (lang / 2 + dx);
    trapezArray[5] = bezY + hoch;

    trapezArray[6] = bezX - (lang / 2 + dx);
    trapezArray[7] = bezY + hoch;

    return trapezArray;
  }

  @Override
  public String toString( )
  {
    return "Trapez";
  }

  /**
   * @see com.bce.profil.ui.view.chart.layer.AbstractProfilChartLayer#editProfil(org.eclipse.swt.graphics.Point,
   *      java.lang.Object)
   */
  @Override
  protected void editProfil( final Point point, final Object data )
  {

  }

  /**
   * @see com.bce.eind.core.profil.IProfilListener#onProfilChanged(com.bce.eind.core.profil.changes.ProfilChangeHint,
   *      com.bce.eind.core.profil.IProfilChange[])
   */
  @Override
  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {

  }

}
