package org.kalypso.ogc.gml;

import java.net.URL;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.commons.i18n.I10nString;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.template.types.StyledLayerType;

/**
 * A factory that creates different types of themes depending on a link type.
 * 
 * @author Stefan Kurzbach
 */
public interface IKalypsoThemeFactory
{
  /**
   * Creates a theme. TODO: this method signature needs to be reworked, way too many arguments
   */
  public IKalypsoTheme createTheme( final I10nString layerName, final StyledLayerType layerType, final URL context, final IMapModell mapModell, final IFeatureSelectionManager selectionManager ) throws CoreException;
}
