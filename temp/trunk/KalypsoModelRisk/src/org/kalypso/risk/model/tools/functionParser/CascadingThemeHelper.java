package org.kalypso.risk.model.tools.functionParser;

import org.kalypso.ogc.gml.CascadingKalypsoTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.IMapModell;

public class CascadingThemeHelper
{
  /**
   * Finds Cascadimg theme with the given name from the map model. Searches only for instances of CascadingKalypsoTheme 
   * @param mapModell
   * @param themeName
   * @return IKalypsoTheme
   */
  public static final CascadingKalypsoTheme getNamedCascadingTheme( final IMapModell mapModell, final String themeName )
  {
    final IKalypsoTheme[] allThemes = mapModell.getAllThemes();
    for( final IKalypsoTheme kalypsoTheme : allThemes )
    {
      if( kalypsoTheme instanceof CascadingKalypsoTheme && kalypsoTheme.getName().equals( themeName ) )
        return (CascadingKalypsoTheme) kalypsoTheme;
    }
    return null;
  }
}
