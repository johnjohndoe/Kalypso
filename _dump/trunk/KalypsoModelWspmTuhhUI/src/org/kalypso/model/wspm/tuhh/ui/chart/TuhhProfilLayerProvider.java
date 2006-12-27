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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.resource.ColorRegistry;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.result.IStationResult;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;
import org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;
import org.kalypso.model.wspm.ui.view.chart.color.IProfilColorSet;

import de.belger.swtchart.axis.AxisRange;

/**
 * @author kimwerner
 */
public class TuhhProfilLayerProvider implements IProfilLayerProvider
{
  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider#createLayer()
   */
  public IProfilChartLayer[] createLayer( final ProfilChartView view, final IProfil profil, final IStationResult[] results, final AxisRange domainRange, final AxisRange valueRangeLeft, final AxisRange valueRangeRight, final ColorRegistry colorRegistry )
  {
    final List<IProfilChartLayer> layers = new ArrayList<IProfilChartLayer>();
    
    layers.add( new GelaendeLayer( view, domainRange, valueRangeLeft, colorRegistry.get( IProfilColorSet.COLOUR_GELAENDE ), colorRegistry.get( IProfilColorSet.COLOUR_GELAENDE_MARKED ), colorRegistry.get( IProfilColorSet.COLOUR_STATIONS ), colorRegistry.get( IProfilColorSet.COLOUR_AXIS_FOREGROUND ) ) );
    final IProfilChartLayer buildingLayer = BuildingLayerFactory.createLayer( view, domainRange, valueRangeLeft, colorRegistry );
    if( buildingLayer != null )
      layers.add( buildingLayer );
    layers.add( new TrennerLayer( view, domainRange, valueRangeLeft, colorRegistry ) );
    final List lst = profil.getPointProperties( false );
    if( lst.contains( POINT_PROPERTY.HOCHWERT ) )
      layers.add( new HochRechtsLayer( view, domainRange, valueRangeLeft, colorRegistry.get( IProfilColorSet.COLOUR_AXIS_FOREGROUND ) ) );
    if( lst.contains( POINT_PROPERTY.BEWUCHS_AX ) )
      layers.add( new BewuchsLayer( view, domainRange, valueRangeLeft, colorRegistry.get( IProfilColorSet.COLOUR_BEWUCHS ) ) );
    if( lst.contains( POINT_PROPERTY.RAUHEIT ) )
      layers.add( new RauheitLayer( view, domainRange, valueRangeRight, colorRegistry.get( IProfilColorSet.COLOUR_AXIS_FOREGROUND ), colorRegistry.get( IProfilColorSet.COLOUR_RAUHEIT ) ) );

    // Wasserpiegel
    for( final IStationResult result : results )
    {
      // only if we have got a wsp for this profile
      if( result.getComponentValue( "urn:ogc:gml:dict:kalypso:model:wspm:components#LengthSectionWaterlevel" ) != null )
        layers.add( new WspLayer( view, domainRange, valueRangeLeft, colorRegistry.get( IProfilColorSet.COLOUR_WSP ), result ) );
    }

    return layers.toArray( new IProfilChartLayer[layers.size()] );
  }

}
