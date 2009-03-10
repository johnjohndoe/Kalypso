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

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.ui.view.ILayerStyleProvider;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer;
import org.kalypso.observation.result.IRecord;

import de.openali.odysseus.chart.framework.model.figure.impl.FullRectangleFigure;
import de.openali.odysseus.chart.framework.model.mapper.IAxis;
import de.openali.odysseus.chart.framework.model.style.IPointStyle;
import de.openali.odysseus.chart.framework.model.style.impl.AreaStyle;
import de.openali.odysseus.chart.framework.model.style.impl.ColorFill;

/**
 * @author kimwerner
 */
public class RoughnessLayer extends AbstractProfilLayer
{

  public RoughnessLayer( final IProfil profil, final String targetRangeProperty, final ILayerStyleProvider styleProvider )
  {
    super( profil, targetRangeProperty, styleProvider );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.chart.AbstractProfilLayer#getHoverRect(org.kalypso.observation.result.IRecord)
   */
  @Override
  public Rectangle getHoverRect( IRecord profilPoint )
  {
    // TODO get HoverInfo
    return null;
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IChartLayer#paint(org.eclipse.swt.graphics.GC)
   */
  @Override
  public void paint( GC gc )
  {
    if( getTargetComponent() == null )
      return;

    final IProfil profil = getProfil();

    if( profil == null )
      return;
    final IRecord[] profilPoints = profil.getPoints();
    final int len = profilPoints.length;

    final int baseLine = getTargetAxis().getScreenHeight();
    final FullRectangleFigure fr = new FullRectangleFigure();

    IPointStyle ps = getPointStyle();
    AreaStyle as = new AreaStyle( new ColorFill( ps.getInlineColor() ), ps.getAlpha(), ps.getStroke(), ps.isVisible() );
    fr.setStyle( as );
    final IAxis dom = getDomainAxis();
    final IAxis tar = getTargetAxis();
    final int index = profil.indexOfProperty( getTargetComponent() );
    final int breite = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    for( int i = 0; i < len - 1; i++ )
    {
      final Double dX1 = (Double) profilPoints[i].getValue( breite );
      final Double dY1 = (Double) profilPoints[i].getValue( index );
      final Double dX2 = (Double) profilPoints[i + 1].getValue( breite );
      if( dX1 == null || dX2 == null || dY1 == null )
        continue;
      final int x1 = dom.numericToScreen( dX1 );
      final int y1 = tar.numericToScreen( dY1 );
      final int x2 = dom.numericToScreen( dX2 );
      fr.setRectangle( new Rectangle( x1, y1, Math.abs( x2 - x1 ), Math.abs( baseLine - y1 ) ) );
      fr.paint( gc );
    }
  }

}
