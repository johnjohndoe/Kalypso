package org.kalypso.java.io;

import java.io.File;

/**
 * Ein FileVisitor führt eine Operation auf einer Datei durch.
 * 
 * @author belger
 */
public interface FileVisitor
{
  /**
   * 'Besucht' eine Datei.
   * 
   * @return ob, bei einem recursiven Durchgang, die Rekursion weiterlaufen darf
   *         oder nicht
   */
  public boolean visit( final File file );
}