package org.kalypso.afgui.scenarios;

import java.net.URL;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.commons.i18n.I10nString;
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
  public IKalypsoTheme createTheme( final String linkType, final I10nString layerName, final LayerType layerType, final URL context, final IMapModell mapModell, final IFeatureSelectionManager selectionManager ) throws CoreException
  {
    if( "scenario".equals( linkType ) )
      return new ScenarioFeatureTheme( layerName, layerType, context, selectionManager, mapModell, null, true );
    return null;
  }

  public String[] handledLinkTypes( )
  {
    return new String[] { "scenario" };
  }

}
