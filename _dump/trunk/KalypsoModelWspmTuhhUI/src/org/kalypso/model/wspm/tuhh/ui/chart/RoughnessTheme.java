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

import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.panel.RauheitenPanel;
import org.kalypso.model.wspm.ui.view.IProfilView;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilTheme;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;

import de.openali.odysseus.chart.framework.model.data.IDataRange;
import de.openali.odysseus.chart.framework.model.mapper.IAxis;
import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;

/**
 * @author kimwerner
 */
public class RoughnessTheme extends AbstractProfilTheme

{

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilTheme#getTargetRange(de.openali.odysseus.chart.framework.model.mapper.IAxis)
   */
  @Override
  public IDataRange<Number> getTargetRange()
  {
    // TODO Auto-generated method stub
    return super.getTargetRange( );
  }

  public RoughnessTheme( final IProfilChartLayer[] chartLayers, final ICoordinateMapper cm )
  {
    super( IWspmTuhhConstants.LAYER_RAUHEIT, "Rauheiten", chartLayers, cm );

  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer#createLayerPanel(org.kalypso.model.wspm.core.profil.IProfil)
   */
  @Override
  public IProfilView createLayerPanel( )
  {
    return new RauheitenPanel( getProfil() );
  }
  
}
