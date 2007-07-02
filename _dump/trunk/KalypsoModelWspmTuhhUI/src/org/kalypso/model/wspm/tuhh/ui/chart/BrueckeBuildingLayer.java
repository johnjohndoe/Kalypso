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
import java.awt.geom.Point2D.Double;
import java.util.List;

import org.eclipse.jface.resource.ColorRegistry;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyRemove;
import org.kalypso.model.wspm.core.profil.changes.ProfileObjectSet;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.panel.BuildingPanel;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.view.IProfilView;
import org.kalypso.model.wspm.ui.view.ProfilViewData;
import org.kalypso.model.wspm.ui.view.chart.AbstractPolyLineLayer;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;


/**
 * @author kimwerner
 */
public class BrueckeBuildingLayer extends AbstractPolyLineLayer
{
  @Override
  public List<IProfilPoint> getPoints( )
  {
    return getProfil().getPoints();
  }

  public BrueckeBuildingLayer( final ProfilChartView pcv )

  {
    super(IWspmTuhhConstants.LAYER_BRUECKE,"Brücke", pcv, pcv.getDomainRange(), pcv.getValueRangeLeft(),  new String[] { IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE,
        IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE }, false, false, false );
    setColors( setColor(pcv.getColorRegistry()));
   
  }

  @Override
  public String toString( )
  {
    return "Brücke";
  }
  private final Color[] setColor(final ColorRegistry cr)
  {
    
   
    if( !cr.getKeySet().contains( IWspmTuhhConstants.LAYER_BRUECKE+"_COLOR_TOP") )
    {
      cr.put( IWspmTuhhConstants.LAYER_BRUECKE+"_COLOR_TOP" , new RGB(0, 128, 0 ));
    }
    if( !cr.getKeySet().contains( IWspmTuhhConstants.LAYER_BRUECKE+"_COLOR_BOTTOM") )
    {
      cr.put( IWspmTuhhConstants.LAYER_BRUECKE+"_COLOR_BOTTOM", new RGB(0, 128,179 ));
    }
    return new Color[]{cr.get(  IWspmTuhhConstants.LAYER_BRUECKE+"_COLOR_TOP"),cr.get( IWspmTuhhConstants.LAYER_BRUECKE+"_COLOR_BOTTOM")};
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
    gc.setForeground( m_colors[0] );
    gc.drawOval( midx - 2, midy - 2, 4, 4 );
    gc.drawLine( left, top, midx, midy );
    gc.drawLine( midx, midy, right, midy );

  }

  protected Double convertPoint( IProfilPoint p, int lineNr )
  {
    {
      final double x = p.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_BREITE );
      double y = 0.0;
      switch( lineNr )
      {
        case 0:
        {
          y = p.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE );
          break;
        }
        case 1:
        {
          y = p.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE );
          break;
        }
      }
      return new Point2D.Double( x, y );
    }
  }

  @Override
  public IProfilView createLayerPanel( final IProfilEventManager pem, final ProfilViewData viewData )
  {
    return new BuildingPanel( pem, viewData );
  }

  public void removeYourself( )
  {
    final IProfilEventManager pem = getProfilEventManager();
    final IProfil profile = pem.getProfil();
    final IProfilChange[] changes = new IProfilChange[3];
    changes[0] = new ProfileObjectSet(profile , (IProfileObject)null );
    changes[1] = new PointPropertyRemove(profile, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE );
    changes[2] = new PointPropertyRemove(profile, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE );
    final ProfilOperation operation = new ProfilOperation( " entfernen", pem, changes, true );
    new ProfilOperationJob( operation ).schedule();
  }
}
