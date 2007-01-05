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

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyRemove;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.view.IProfilView;
import org.kalypso.model.wspm.ui.view.ProfilViewData;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilChartLayer;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;

import de.belger.swtchart.EditInfo;
import de.belger.swtchart.axis.AxisRange;

public class HochRechtsLayer extends AbstractProfilChartLayer implements IProfilChartLayer
{
  private IProfilEventManager m_pem;

  private Color m_color;

  public HochRechtsLayer( final ProfilChartView  pvp, final AxisRange domainRange, final AxisRange valueRange, final Color color )
  {
    super( pvp,domainRange, valueRange, false );

    m_pem = pvp.getProfilEventManager() ;
    m_color = color;
  }

  @Override
  public IProfilView createLayerPanel( IProfilEventManager pem, ProfilViewData viewData )
  {
    return null;
  }

  public void removeYourself( )
  {
    final IProfilChange[] changes = new IProfilChange[2];
    changes[0] = new PointPropertyRemove( m_pem.getProfil(), POINT_PROPERTY.HOCHWERT );
    changes[1] = new PointPropertyRemove( m_pem.getProfil(), POINT_PROPERTY.RECHTSWERT );

    final ProfilOperation operation = new ProfilOperation( "Datensatz entfernen: " + toString(), m_pem, changes, true );
    new ProfilOperationJob( operation ).schedule();
  }

  public Rectangle2D getBounds( )
  {
    try
    {
      final IProfilPoint p = m_pem.getProfil().getPoints().getFirst();
      final double x = p.getValueFor( POINT_PROPERTY.BREITE );
      final double y = p.getValueFor( POINT_PROPERTY.HOEHE );
      final Point2D p2 = new Point2D.Double( x, y );
      return new Rectangle2D.Double( p2.getX(), p2.getY(), 0, 0 );
    }
    catch( ProfilDataException e )
    {
      e.printStackTrace();
      return new Rectangle2D.Double( 0, 0, 0, 0 );

    }

  }

  @Override
  public String toString( )
  {
    return "Geokoordinaten RW/HW";
  }

  @Override
  public EditInfo getHoverInfo( Point point )
  {
    return null;
  }

  @Override
  public void paintDrag( GCWrapper gc, Point editing, Object hoverData )
  {
  }

  @Override
  public void paintLegend( GCWrapper gc )
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
  

  @Override
  public boolean isNotPainting( )
  {
    return true;
  }

  /**
   * @see com.bce.profil.ui.view.chart.layer.AbstractProfilChartLayer#editProfil(org.eclipse.swt.graphics.Point, java.lang.Object)
   */
  @Override
  protected void editProfil( Point point, Object data )
  {
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paint(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  public void paint( GCWrapper gc )
  {
  }

  /**
   * @see com.bce.eind.core.profil.IProfilListener#onProfilChanged(com.bce.eind.core.profil.changes.ProfilChangeHint, com.bce.eind.core.profil.IProfilChange[])
   */
  @Override
  public void onProfilChanged( ProfilChangeHint hint, IProfilChange[] changes )
  {
  }
}
