package org.kalypso.util.synchronize;

import java.io.File;

import org.eclipse.core.internal.resources.Resource;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.java.io.DeleteObsoleteFilesVisitor;
import org.kalypso.java.io.FileCopyVisitor;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.ui.nature.ModelNature;

/**
 * @author belger
 */
public class ModelSynchronizer
{
  private final File m_serverRoot;
  private final IProject m_resourceRoot;

  public ModelSynchronizer( final IProject root, final File serverRoot )
  {
    m_resourceRoot = root;
    m_serverRoot = serverRoot;
  }

  public void updateLocal() throws CoreException
  {
    if( !m_resourceRoot.exists() )
    {
      m_resourceRoot.create( new NullProgressMonitor( ) );
      m_resourceRoot.open( new NullProgressMonitor( ) );
    }
    
    // server -> local
    // Hack, weil getLocation für IProject's nicht funktioniert!
    // hoffentlich leitet alles von Resource ab
    final File resourceFile = ((Resource)m_resourceRoot).getLocalManager().locationFor( m_resourceRoot ).toFile();
    copy( m_serverRoot, resourceFile );
    
    // local refreshen
    m_resourceRoot.refreshLocal( IResource.DEPTH_INFINITE, new NullProgressMonitor() );
  }

  private void copy( final File from, final File to )
  {
    final FileCopyVisitor copyVisitor = new FileCopyVisitor( from, to, true, ModelNature.CONTROL_NAME );
    FileUtilities.accept( from, copyVisitor );
    
    final DeleteObsoleteFilesVisitor deleteVisitor = new DeleteObsoleteFilesVisitor( to, from );
    FileUtilities.accept( to, deleteVisitor );
  }

}
