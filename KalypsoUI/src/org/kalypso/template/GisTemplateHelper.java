package org.kalypso.template;

import org.eclipse.core.resources.IProject;
import org.kalypso.ogc.gml.PoolableKalypsoFeatureTheme;
import org.kalypso.template.gistableview.GistableviewType.LayerType;

/**
 * Hilfsklasse, um aus den Binding-Klassen 'echte' Objekte zu erzeugen und umgekehrt
 * 
 * @author Belger
 */
public class GisTemplateHelper
{
  private GisTemplateHelper()
  {
    // never instantiate this class
  }

  public static PoolableKalypsoFeatureTheme createThemeFromTemplate( final LayerType layerType, final IProject project )
  {
    final String source = layerType.getHref();
    final String type = layerType.getLinktype();
    
    return new PoolableKalypsoFeatureTheme( "<no name>", type, source, project );
  }

}
