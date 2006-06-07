package org.kalypso.model.wspm.ui.profil.view.chart;

import org.eclipse.jface.resource.ColorRegistry;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;

/** Erzeugt eine {@link org.eclipse.jface.resource.ColorRegistry} mit einer Standardfarbeelegung für den Profileditor */
public class DefaultProfilColorRegistryFactory
{
  private DefaultProfilColorRegistryFactory( )
  {
    // wird nicht instantiiert
  }

  /**
   * Erzeugt die Registry, diese muss vom aufrufenden zerstört (dispose) werden.
   * 
   * @param display
   */
  public static ColorRegistry createColorRegistry( final Display display )
  {
    final ColorRegistry registry = new ColorRegistry( display, true );

    registry.put( IProfilColorSet.COLOUR_GELAENDE, new RGB( 200, 50, 0 ) );
    registry.put( IProfilColorSet.COLOUR_GELAENDE_MARKED, new RGB( 255, 150, 0 ) );
    registry.put( IProfilColorSet.COLOUR_AXIS_FOREGROUND, new RGB( 0, 0, 0 ) );
    registry.put( IProfilColorSet.COLOUR_AXIS_BACKGROUND, new RGB( 255, 255, 255 ) );
    registry.put( IProfilColorSet.COLOUR_STATIONS, new RGB( 128, 128, 128 ) );
    registry.put( IProfilColorSet.COLOUR_TRENNFLAECHEN, new RGB( 0, 180, 0 ) );
    registry.put( IProfilColorSet.COLOUR_BORDVOLLPUNKTE, new RGB( 200, 50, 0 ) );
    registry.put( IProfilColorSet.COLOUR_DURCHSTROEMTE_BEREICHE, new RGB( 0, 0, 255 ) );
    registry.put( IProfilColorSet.COLOUR_BRUECKE, new RGB( 0, 128, 0 ) );
    registry.put( IProfilColorSet.COLOUR_DURCHLASS, new RGB( 220, 220, 0 ) );
    registry.put( IProfilColorSet.COLOUR_RAUHEIT, new RGB( 220, 220, 220 ) );
    registry.put( IProfilColorSet.COLOUR_BEWUCHS, new RGB( 0, 255, 0 ) );
    registry.put( IProfilColorSet.COLOUR_WSP, new RGB( 0, 128, 255 ) );
    
    return registry;
  }
}
