package org.kalypso.ui.nature;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;

import org.apache.commons.io.CopyUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * @author belger
 */
public class CopyResourceToFileVisitor implements IResourceVisitor
{
  private final IContainer m_baseresource;
  private final File m_targetDir;

  public CopyResourceToFileVisitor( final IContainer baseresource, final File targetDir )
  {
    m_baseresource = baseresource;
    m_targetDir = targetDir;
  }

  /**
   * @see org.eclipse.core.resources.IResourceVisitor#visit(org.eclipse.core.resources.IResource)
   */
  public boolean visit( final IResource resource ) throws CoreException
  {
    // nur Dateien werden kopiert
    if( resource instanceof IFile )
    {
      final IFile inputfile = (IFile)resource;
      final IPath inputpath = resource.getFullPath();
      
      // Error msg?
      final IPath basePath = m_baseresource.getFullPath();
      if( !basePath.isPrefixOf( inputpath ) )
        return true;

      final String basepath = basePath.toString();
      final String resourcepath = inputpath.toString();
      
      final String relativepath = resourcepath.substring( basepath.length() + 1 );
      
      final File targetpath = new File( m_targetDir, relativepath );
      targetpath.getParentFile().mkdirs();
      
      // daten kopieren
      try
      {
        final OutputStreamWriter writer = new OutputStreamWriter( new FileOutputStream( targetpath ), inputfile.getCharset( ) );
        final Reader r = new InputStreamReader( inputfile.getContents(), inputfile.getCharset() );

        // bean erzeugen
        CopyUtils.copy( r, writer );
        r.close();
        writer.close();
      }
      catch( final CoreException e )
      {
        throw e;
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        throw new CoreException( KalypsoGisPlugin.createErrorStatus( "Fehler beim Kopieren einer Datei", e ) );
      }
    }
    
    return true;
  }

}
