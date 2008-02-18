package org.kalypso.afgui.scenarios;

import java.net.URL;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.IKalypsoThemeFactory;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.template.types.LayerType;

/**
 * Can create a {@link ScenarioFeatureTheme}
 * 
 * @author kurzbach
 */
public class KalypsoAFGUIThemeFactory implements IKalypsoThemeFactory
{
  public IKalypsoTheme createTheme( final String linkType, final LayerType layerType, final URL context, final IMapModell mapModell, final IFeatureSelectionManager selectionManager ) throws CoreException
  {
    if( "scenario".equals( linkType ) )
      return new ScenarioFeatureTheme( layerType, context, selectionManager, mapModell, null, true );
    return null;
  }

  public String[] handledLinkTypes( )
  {
    return new String[] { "scenario" };
  }

}
