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

import org.eclipse.jface.resource.ColorRegistry;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.ui.view.ILayerStyleProvider;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;

/**
 * @author kimwerner
 */
public abstract class AbstractRauheitLayer extends AbstractProfilLayer
{
  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer#removeYourself()
   */
  public void removeYourself( )
  {
    // override this method
    
  }

 // private final Color m_color;

  protected IProfil m_profile;

  public AbstractRauheitLayer( final IProfil profil, final String targetRangeProperty, final ILayerStyleProvider styleProvider )

  {
    
    
    super( profil, targetRangeProperty, styleProvider );
    m_profile = profil;
//    final ColorRegistry cr = pcv.getColorRegistry();
//    if( !cr.getKeySet().contains( layerId ) )
//    {
//      cr.put( layerId, color );
//    }
//    m_color = cr.get( layerId );

  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilChartLayer#editProfil(org.eclipse.swt.graphics.Point,
   *      java.lang.Object)
   */
//
//  @Override
//  protected void editProfil( final Point point, final Object data )
//  {
//    // no editing
//  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    return "Rauheit";
  }

  protected void fillRectangle( final GC gc, final Rectangle box )
  {
    gc.setForeground( m_color );
    gc.setBackground( m_color );
    gc.fillRectangle( box );
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paintLegend(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  @Override
  public void paintLegend( final GC gc )
  {
    final Rectangle clipping = gc.getClipping();
    fillRectangle( gc, clipping );
  }

}