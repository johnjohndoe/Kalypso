package org.kalypso.java.io;

import java.io.File;

/**
 * Löscht alle Dateien aus einem Dateibaum (sourceDir), welche in einem anderen (targetDir) nicht existieren
 * 
 * @author belger
 */
public class DeleteObsoleteFilesVisitor implements FileVisitor
{
  private final File m_targetDir;
  private final File m_sourceDir;

  public DeleteObsoleteFilesVisitor( final File sourceDir, final File targetDir )
  {
    m_sourceDir = sourceDir;
    m_targetDir = targetDir;}

  /**
   * @see org.kalypso.java.io.FileVisitor#visit(java.io.File)
   */
  public boolean visit( final File file )
  {
    final String relativePathTo = FileUtilities.getRelativePathTo( m_sourceDir, file );
    final File targetFile = new File( m_targetDir, relativePathTo );
    
    if( !targetFile.exists() )
    {
      FileUtilities.deleteRecursive( file );
      return false;
    }

    return true;
  }

}
