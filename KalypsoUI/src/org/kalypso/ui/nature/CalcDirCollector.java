package org.kalypso.ui.nature;

import java.io.File;
import java.util.Collection;
import java.util.LinkedList;

import org.kalypso.java.io.FileVisitor;

/**
 * Collect CalcCases in directory structure
 * 
 * @author Belger
 */
public class CalcDirCollector implements FileVisitor
{
  private Collection m_calcDirs = new LinkedList();

  /**
   * @see org.kalypso.java.io.FileVisitor#visit(java.io.File)
   */
  public boolean visit( final File file )
  {
    if( file.isDirectory() )
    {
      final File controlFile = new File( file, ModelNature.CONTROL_NAME );
      if( controlFile.exists() )
      {
        m_calcDirs.add( file );
        return false;
      }
    }
    
    return true;
  }

  public File[] getCalcDirs()
  {
    return (File[])m_calcDirs.toArray( new File[m_calcDirs.size()] );
  }
}