/**
 *
 */
package org.kalypso.google.earth.export.constants;

import java.io.File;

/**
 * @author kuch
 */
public interface IGoogleEarthExportSettings
{
  public static final String CONST_TARGET_FILE = "googleEarthExportTargetFile";

  public String getExportDescription( );

  public File getExportFile( );

  public String getExportName( );
}
