package org.kalypso.util.synchronize;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.internal.resources.Resource;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.java.io.DeleteObsoleteFilesVisitor;
import org.kalypso.java.io.FileCopyVisitor;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.nature.ModelNature;

/**
 * @author belger
 */
public class ModelSynchronizer
{
  private final File m_serverRoot;

  private final IProject m_resourceRoot;

  private final File m_resourceRootFile;

  public ModelSynchronizer( final IProject root, final File serverRoot )
  {
    m_resourceRoot = root;
    // Hack, weil getLocation f�r IProject's nicht funktioniert!
    // hoffentlich leitet alles von Resource ab
    m_resourceRootFile = ( (Resource)m_resourceRoot ).getLocalManager()
        .locationFor( m_resourceRoot ).toFile();

    m_serverRoot = serverRoot;
  }

  public void updateLocal() throws CoreException
  {
    if( !m_resourceRoot.exists() )
    {
      m_resourceRoot.create( new NullProgressMonitor() );
      m_resourceRoot.open( new NullProgressMonitor() );
    }

    // server -> local
    synchronizeProject( m_serverRoot, m_resourceRootFile );

    // local refreshen
    m_resourceRoot.refreshLocal( IResource.DEPTH_INFINITE, new NullProgressMonitor() );
  }

  private void synchronizeProject( final File from, final File to )
  {
    final FileCopyVisitor copyVisitor = new FileCopyVisitor( from, to, true,
        ModelNature.CONTROL_NAME );
    FileUtilities.accept( from, copyVisitor );

    final DeleteObsoleteFilesVisitor deleteVisitor = new DeleteObsoleteFilesVisitor( to, from );
    FileUtilities.accept( to, deleteVisitor );
  }

  private void copyAll( final File from, final File to )
  {
    final FileCopyVisitor copyVisitor = new FileCopyVisitor( from, to, true );
    FileUtilities.accept( from, copyVisitor );
  }

  /**
   * Schreibt ein einzelnes Verzeichnis innerhalb des lokalen Projekts zur�ck
   * zum server Das Verzeichnis darf Serverseitig noch nicht existieren
   * 
   * @throws CoreException
   */
  public void commitFolder( final IFolder folder ) throws CoreException
  {
    final String projectRelativePath = folder.getProjectRelativePath().toString();

    final File serverDir = new File( m_serverRoot, projectRelativePath );

    if( serverDir.exists() )
      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Das Verzeichnis existiert bereits auf dem Server: " + projectRelativePath, null ) );

    final File localDir = new File( m_resourceRootFile, projectRelativePath );
    copyAll( localDir, serverDir );
  }

  public File getServerRoot()
  {
    return m_serverRoot;
  }

  /**
   * L�dt einen Remote Folder vom Server und legt in local ab �berschreibt, ist
   * lokal bereits etwas vorhanden, gibts ne Fehlermeldung
   * 
   * @throws CoreException
   */
  public void getFolder( final File dir, final String localName ) throws CoreException
  {
    final String relativePath = FileUtilities.getRelativePathTo( m_serverRoot, dir );
    final IFile file = m_resourceRoot.getFile( localName );
    if( file.exists() )
      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Verzeichnis exisitert lokal bereits: " + relativePath, null ) );

    final File localDir = new File( m_resourceRootFile, localName );
    copyAll( dir, localDir );

    file.getParent().refreshLocal( IResource.DEPTH_INFINITE, new NullProgressMonitor() );
  }

  public void commitProject() throws CoreException
  {
    if( !m_serverRoot.exists() )
      m_serverRoot.mkdir();

    // server -> local
    final File lockFile = new File( m_serverRoot, ".lock" );
    if( lockFile.exists() )
    {
      String user = "<unbekannt>";
      try
      {
        user = FileUtils.readFileToString( lockFile, "UTF-8" );
      }
      catch( final IOException e )
      {
        e.printStackTrace();
      }

      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Lock-Datei existiert f�r Benutzer: " + user, null ) );
    }

    try
    {
      final String user = System.getProperties().getProperty( "user.name", "<unbekannt>" );
      FileUtils.writeStringToFile( lockFile, user, "UTF-8" );
      synchronizeProject( m_resourceRootFile, m_serverRoot );
    }
    catch( IOException e )
    {
      e.printStackTrace();

      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Konnte lock Datei nicht erzeugen", e ) );
    }
    finally
    {
      lockFile.delete();
    }
  }

}