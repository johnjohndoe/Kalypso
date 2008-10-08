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

import org.eclipse.swt.graphics.Point;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.ui.view.ILayerStyleProvider;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;
import org.kalypso.model.wspm.ui.view.chart.PointsLineLayer;

import de.openali.odysseus.chart.framework.model.layer.EditInfo;

/**
 * @author kimwerner
 *
 */
public class CrossSectionLayer extends PointsLineLayer
{

  public CrossSectionLayer( IProfil profil, String targetRangeProperty, ILayerStyleProvider styleProvider )
  {
    super( profil, targetRangeProperty, styleProvider );
  }
  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilTheme#drag(org.eclipse.swt.graphics.Point,
   *      de.openali.odysseus.chart.framework.model.layer.EditInfo)
   */
  @Override
  public EditInfo drag( Point newPos, EditInfo dragStartData )
  {

    final Object o = getData( IProfilChartLayer.VIEW_DATA_KEY );
    if( o != null )

      try
      {
        final int i = Integer.valueOf( o.toString() );
        if( (i & 2) == 0 )
        {
          newPos.y = dragStartData.m_pos.y;
        }
        if( (i & 1) == 0 )
        {
          newPos.x = dragStartData.m_pos.x;
        }
      }
      catch( NumberFormatException e )
      {
        return super.drag( newPos, dragStartData );
      }
    return super.drag( newPos, dragStartData );
  }

}
