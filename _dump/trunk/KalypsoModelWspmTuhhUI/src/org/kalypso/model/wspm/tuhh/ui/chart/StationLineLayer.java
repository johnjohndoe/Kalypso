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

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.ui.view.chart.ComponentLayer;
import org.kalypso.observation.result.IRecord;

import de.openali.odysseus.chart.framework.model.figure.impl.PolylineFigure;
import de.openali.odysseus.chart.framework.model.layer.ILegendEntry;
import de.openali.odysseus.chart.framework.model.layer.impl.LegendEntry;
import de.openali.odysseus.chart.framework.model.mapper.IAxis;

/**
 * @author kimwerner
 */
public class StationLineLayer extends ComponentLayer
{

  public StationLineLayer( final IProfil profil, final String targetRangeProperty )
  {
    super( profil, targetRangeProperty );
  }

  /**
   * @see de.openali.odysseus.chart.ext.base.layer.AbstractChartLayer#getLegendEntries()
   */
  @Override
  public synchronized ILegendEntry[] getLegendEntries( )
  {
    LegendEntry le = new LegendEntry( this, toString() )
    {
      @Override
      public void paintSymbol( GC gc, Point size )
      {
        drawLine( gc, gc.getClipping() );
      }
    };
    return new ILegendEntry[] { le };
  }

  @Override
  public String getId( )
  {
    return super.getId() + "_STATIONLINE";
  }

  @Override
  public String getTitle( )
  {
    return "stationlines";
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IChartLayer#paint(org.eclipse.swt.graphics.GC)
   */
  @Override
  public void paint( GC gc )
  {
    final IProfil profil = getProfil();

    if( profil == null )
      return;
    final IRecord[] profilPoints = profil.getPoints();

    IAxis targetAxis = getCoordinateMapper().getTargetAxis();
    final int baseLine = targetAxis.numericToScreen( targetAxis.getNumericRange().getMin() );
    for( IRecord profilPoint : profilPoints )
    {
      final Point point = toScreen( profilPoint );
      if( point == null )
        continue;
      drawLine( gc, new Rectangle( point.x, point.y, 0, baseLine ) );
    }
  }

  protected void drawLine( GC gc, final Rectangle clipping )
  {
    final PolylineFigure pf = new PolylineFigure();
    pf.setStyle( getLineStyle_hover() );
    pf.setPoints( new Point[] { new Point( clipping.x + clipping.width / 2, clipping.y + clipping.height ), new Point( clipping.x + clipping.width / 2, clipping.y ) } );
    pf.paint( gc );
  }

}
