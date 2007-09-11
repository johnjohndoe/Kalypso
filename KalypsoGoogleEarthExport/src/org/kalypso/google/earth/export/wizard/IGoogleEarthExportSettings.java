/**
 *
 */
package org.kalypso.google.earth.export.wizard;

import java.io.File;

/**
 * @author kuch
 */
public interface IGoogleEarthExportSettings
{
  public String getExportName( );

  public String getExportDescription( );

  public File getExportFile( );
}
