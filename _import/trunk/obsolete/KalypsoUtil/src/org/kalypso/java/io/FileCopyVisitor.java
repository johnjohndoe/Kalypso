package org.kalypso.java.io;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FileUtils;

/**
 * Copiert eine Datei und zwar relativ zu den angegebenen 'fromDir' und 'toDir'
 * 
 * @author belger
 */
public class FileCopyVisitor implements FileVisitor
{
  private final File m_toDir;
  private final File m_fromDir;
  private final boolean m_overwriteIfNewer;
  private final String m_excludeDirWithFile;

  /**
   * @param overwriteIfNewer Die Zieldatei selbst dann überschreiben, wenn sie neuer ist
   */
  public FileCopyVisitor( final File fromDir, final File toDir, final boolean overwriteIfNewer )
  {
    this( fromDir, toDir, overwriteIfNewer, null );
  }
  
  public FileCopyVisitor( final File fromDir, final File toDir, final boolean overwriteIfNewer, final String excludeDirWithFile )
  {
    m_fromDir = fromDir;
    m_toDir = toDir;
    m_overwriteIfNewer = overwriteIfNewer;
    m_excludeDirWithFile = excludeDirWithFile;
  }

  /**
   * @see org.kalypso.java.io.FileVisitor#visit(java.io.File)
   */
  public boolean visit( final File file )
  {
    final String relativePathTo = FileUtilities.getRelativePathTo( m_fromDir, file );
    if( relativePathTo != null )
    {
      final File targetFile = new File( m_toDir, relativePathTo );
      
      // falls es ein Verzeichnis ist und das Auschlussfile enthält, hier abbrechen
      if( m_excludeDirWithFile != null && file.isDirectory() )
      {
        final File excludeFile = new File( file, m_excludeDirWithFile );
        if( excludeFile.exists() )
          return false;
      }
      
      if( file.isDirectory() )
        targetFile.mkdir();
      
      if( file.isFile() )
      {
        // falls die Zieldatei neuer ist und das überschreiben neuerer verboten wurde
        // einfach abbrechen
        if( targetFile.exists() )
        {
          // falls neuer überschreiben oder nicht?
          final long targetLastModified = targetFile.lastModified();
          final long lastModified = file.lastModified();
          if( !m_overwriteIfNewer && targetLastModified > lastModified )
            return false;

          // falls die Dateien wirklich gleich sind, nichts tun
          if( targetLastModified == lastModified  && targetFile.length() == file.length() )
            return false;
        }

        // sonst kopieren
        try
        {
          FileUtils.copyFile( file, targetFile );
        }
        catch( IOException e )
        {
          e.printStackTrace();
        }
        
        return false;
      }
    }
    
    return true;
  }

}
