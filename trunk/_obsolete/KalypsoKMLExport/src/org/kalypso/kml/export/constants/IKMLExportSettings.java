/**
 *
 */
package org.kalypso.kml.export.constants;

import java.io.File;

/**
 * @author Dirk Kuch
 */
public interface IKMLExportSettings
{
  public static final String CONST_TARGET_FILE = "kmlExportTargetFile"; //$NON-NLS-1$

  public String getExportDescription( );

  public File getExportFile( );

  public String getExportName( );
}
