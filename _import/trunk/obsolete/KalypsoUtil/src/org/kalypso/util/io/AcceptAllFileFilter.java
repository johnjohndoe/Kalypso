package org.kalypso.util.io;

import java.io.File;
import java.io.FileFilter;

/**
 * Ein FileFilter dass alle Dateien erlaubt.
 * 
 * @author schlienger
 */
public class AcceptAllFileFilter implements FileFilter
{
  /**
   * @see java.io.FileFilter#accept(java.io.File)
   */
  public boolean accept( File arg0 )
  {
    return true;
  }
}
