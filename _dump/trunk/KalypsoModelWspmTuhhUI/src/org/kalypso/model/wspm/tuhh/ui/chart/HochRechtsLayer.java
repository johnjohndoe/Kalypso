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

import java.awt.geom.Rectangle2D;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyRemove;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.view.IProfilView;
import org.kalypso.model.wspm.ui.view.ProfilViewData;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilChartLayer;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;

import de.openali.odysseus.chart.framework.model.layer.EditInfo;

public class HochRechtsLayer extends AbstractProfilChartLayer implements IProfilChartLayer
{
  private final IProfil m_profile;

  private final Color m_color;

  public HochRechtsLayer( final ProfilChartView pcv )
  {
    super( IWspmTuhhConstants.LAYER_GEOKOORDINATEN, pcv, pcv.getDomainRange(), pcv.getValueRangeLeft(), "Geokoordinaten", false );

    m_profile = pcv.getProfil();
    m_color = pcv.getColorRegistry().get( IWspmTuhhConstants.LAYER_GELAENDE );
  }

  @Override
  public IProfilView createLayerPanel( final IProfil profile, final ProfilViewData viewData )
  {
    return null;
  }

  public void removeYourself( )
  {
    final IProfilChange[] changes = new IProfilChange[2];
    changes[0] = new PointPropertyRemove( m_profile, m_profile.hasPointProperty(  IWspmTuhhConstants.POINT_PROPERTY_HOCHWERT ) );
    changes[1] = new PointPropertyRemove( m_profile, m_profile.hasPointProperty(  IWspmTuhhConstants.POINT_PROPERTY_RECHTSWERT ) );

    final ProfilOperation operation = new ProfilOperation( "Datensatz entfernen: " + toString(), m_profile, changes, true );
    new ProfilOperationJob( operation ).schedule();
  }

  public Rectangle2D getBounds( )
  {
    return null;//IChartLayer.MINIMAL_RECT;
  }

  @Override
  public String toString( )
  {
    return "Geokoordinaten RW/HW";
  }

  @Override
  public EditInfo getHoverInfo( final Point point )
  {
    return null;
  }

  @Override
  public void paintDrag( final GC gc, final Point editing, final Object hoverData )
  {
  }

  @Override
  public void paintLegend( final GC gc )
  {
    final Rectangle clipping = gc.getClipping();

    final int left = clipping.x + 3;
    final int top = clipping.y + 3;
    final int right = clipping.x + clipping.width - 6;
    final int bottom = clipping.y + clipping.width - 6;
    final int midx = (left + right) / 2;
    final int midy = (top + bottom) / 2;
    gc.setLineWidth( 1 );
    gc.setForeground( m_color );
    gc.setLineStyle( SWT.LINE_SOLID );

    gc.drawLine( left, top, left, bottom + 2 );
    gc.drawLine( left - 2, bottom, right, bottom );

    gc.drawOval( midx, midy, 3, 3 );
  }

  public boolean isNotPainting( )
  {
    return true;
  }

 

  

  /**
   * @see com.bce.eind.core.profil.IProfilListener#onProfilChanged(com.bce.eind.core.profil.changes.ProfilChangeHint,
   *      com.bce.eind.core.profil.IProfilChange[])
   */
  @Override
  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilChartLayer#editProfil(org.eclipse.swt.graphics.Point, java.lang.Object)
   */
  @Override
  protected void editProfil( Point point, Object data )
  {
    // TODO Auto-generated method stub
    
  }
}
