package org.kalypso.ogc.gml;

import java.net.URL;

import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.template.types.LayerType;

public interface IKalypsoThemeFactory
{
  public IKalypsoTheme createTheme( final String linkType, final LayerType layerType, final URL context, final IMapModell mapModell, final IFeatureSelectionManager selectionManager );

  public String[] handledLinkTypes( );
}
