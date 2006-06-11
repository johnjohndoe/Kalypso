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
