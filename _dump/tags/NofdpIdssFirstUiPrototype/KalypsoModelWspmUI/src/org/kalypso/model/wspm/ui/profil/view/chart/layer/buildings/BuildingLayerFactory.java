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
package org.kalypso.model.wspm.ui.profil.view.chart.layer.buildings;

import java.util.Arrays;

import org.eclipse.jface.resource.ColorRegistry;
import org.eclipse.swt.graphics.Color;
import org.kalypso.model.wspm.core.profil.IProfilBuilding;
import org.kalypso.model.wspm.ui.profil.view.chart.IProfilColorSet;
import org.kalypso.model.wspm.ui.profil.view.chart.ProfilChartView;
import org.kalypso.model.wspm.ui.profil.view.chart.layer.IProfilChartLayer;
import org.kalypso.model.wspm.ui.profil.view.chart.layer.buildings.building.BrueckeBuildingLayer;
import org.kalypso.model.wspm.ui.profil.view.chart.layer.buildings.building.WehrBuildingLayer;
import org.kalypso.model.wspm.ui.profil.view.chart.layer.buildings.durchlass.EiBuildingLayer;
import org.kalypso.model.wspm.ui.profil.view.chart.layer.buildings.durchlass.KreisBuildingLayer;
import org.kalypso.model.wspm.ui.profil.view.chart.layer.buildings.durchlass.MaulBuildingLayer;
import org.kalypso.model.wspm.ui.profil.view.chart.layer.buildings.durchlass.TrapezBuildingLayer;

import de.belger.swtchart.axis.AxisRange;

public class BuildingLayerFactory
{

  public static IProfilChartLayer createLayer( final ProfilChartView chartView, final AxisRange domainRange, final AxisRange valueRange, final ColorRegistry colorRegistry )
  {
    final Color colorDurchlass = colorRegistry.get( IProfilColorSet.COLOUR_DURCHLASS );
    final Color colorWehr = colorRegistry.get( IProfilColorSet.COLOUR_WEHR );
    final Color colorOKBruecke = colorRegistry.get( IProfilColorSet.COLOUR_BRUECKE );
    final Color colorUKBruecke = colorRegistry.get( IProfilColorSet.COLOUR_WEHR );
    final Color colorForeground = colorRegistry.get( IProfilColorSet.COLOUR_AXIS_FOREGROUND );
    final IProfilBuilding building = chartView.getProfil().getBuilding();
    if( building == null )
      return null;
    switch( building.getTyp() )
    {
      case KREIS:
        return new KreisBuildingLayer( chartView, domainRange, valueRange, colorDurchlass );
      case EI:
        return new EiBuildingLayer( chartView, domainRange, valueRange, colorDurchlass );
      case MAUL:
        return new MaulBuildingLayer( chartView, domainRange, valueRange, colorDurchlass );
      case TRAPEZ:
        return new TrapezBuildingLayer( chartView, domainRange, valueRange, colorDurchlass );
      case WEHR:
        return new WehrBuildingLayer( chartView, domainRange, valueRange, Arrays.asList( colorWehr ), colorWehr, colorWehr, colorForeground );
      case BRUECKE:
        return new BrueckeBuildingLayer( chartView, domainRange, valueRange, Arrays.asList( colorOKBruecke, colorUKBruecke ), colorOKBruecke, colorOKBruecke, colorForeground );
      default:
        return null;
    }

  }
}
