package org.kalypso.google.earth.export.interfaces;

import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IFile;

public interface IGoogleEarthAdapter
{
  // TODO we really want to provide IFiles?!? hmmm... String handling is not even better...
  /**
   * Point geometries can define placemarkers, this means, which image will be displayed for an point TODO add name,
   * description (additional informations to these point markers)
   */
  public Map<QName, IFile> getPlaceMarkers( );

  // TODO add additional placemarkers, perhaps for measures and their details (images, weblinks, aso)...
}
