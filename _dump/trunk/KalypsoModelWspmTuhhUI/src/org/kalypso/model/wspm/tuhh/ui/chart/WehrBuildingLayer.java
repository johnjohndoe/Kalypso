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

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilConstants;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.changes.BuildingSet;
import org.kalypso.model.wspm.core.profil.changes.DeviderRemove;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.ui.panel.WehrPanel;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.view.IProfilView;
import org.kalypso.model.wspm.ui.view.ProfilViewData;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;

import de.belger.swtchart.axis.AxisRange;

/**
 * @author kimwerner
 */
public class WehrBuildingLayer extends AbstractPolyLineLayer
{
  @Override
  public List<IProfilPoint> getPoints( )
  {
    return ProfilUtil.getInnerPoints( getProfil(), IProfilConstants.DEVIDER_TYP_TRENNFLAECHE );
  }

  public WehrBuildingLayer( final ProfilChartView pvp, final AxisRange domainRange, final AxisRange valueRange, final List<Color> colors, final Color selectedcolor, final Color stationColor, final Color editColor )
  {
    super( pvp, domainRange, valueRange, colors, selectedcolor, stationColor, editColor, Arrays.asList( POINT_PROPERTY.OBERKANTEWEHR ), false, false, false );
  }

  @Override
  public String toString( )
  {
    return "Wehr";
  }

  @Override
  public void paintLegend( GCWrapper gc )
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
    gc.setForeground( m_colors.get( 0 ) );
    gc.drawOval( midx - 2, midy - 2, 4, 4 );
    gc.drawLine( left, top, midx, midy );
    gc.drawLine( midx, midy, right, midy );

  }

  @Override
  public IProfilView createLayerPanel( final IProfilEventManager pem, final ProfilViewData viewData )
  {
    return new WehrPanel( pem, viewData );
  }

  public void removeYourself( )
  {
    final IProfilEventManager pem = getProfilEventManager();
    final IProfilDevider[] deviders = pem.getProfil().getDevider( IProfilConstants.DEVIDER_TYP_WEHR );
    final IProfilChange[] changes = new IProfilChange[deviders.length + 1];
    changes[0] = new BuildingSet( pem.getProfil(), null );
    for( int i = 0; i < deviders.length; i++ )
    {
      changes[i + 1] = new DeviderRemove( pem.getProfil(), deviders[i] );
    }
    final ProfilOperation operation = new ProfilOperation( "Wehr entfernen", pem, changes, true );
    new ProfilOperationJob( operation ).schedule();
  }

  /**
   * @see com.bce.eind.core.profil.IProfilListener#onProfilChanged(com.bce.eind.core.profil.changes.ProfilChangeHint,
   *      com.bce.eind.core.profil.IProfilChange[])
   */
  @Override
  public void onProfilChanged( ProfilChangeHint hint, IProfilChange[] changes )
  {
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.chart.layer.AbstractPolyLineLayer#isPointVisible(org.kalypso.model.wspm.core.profil.IProfilPoint)
   */
  @Override
  protected boolean isPointVisible( IProfilPoint point )
  {
    final IProfil profil = getProfil();
    final LinkedList points = profil.getPoints();
    final int i = points.indexOf( point );
    final IProfilDevider[] deviders = profil.getDevider( IProfilConstants.DEVIDER_TYP_TRENNFLAECHE );
    if( i < points.indexOf( deviders[0].getPoint() ) )
    {
      return false;
    }
    if( i > points.indexOf( deviders[deviders.length - 1].getPoint() ) )
    {
      return false;
    }
    return true;
  }
}
