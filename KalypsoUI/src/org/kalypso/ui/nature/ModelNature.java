package org.kalypso.ui.nature;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import javax.xml.bind.JAXBException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.util.transformation.CalculationCaseTransformation;
import org.kalypso.util.transformation.TransformationException;
import org.kalypso.util.transformation.TransformationFactory;
import org.kalypso.xml.util.ObjectFactory;
import org.kalypso.xml.util.TransformationConfig;
import org.kalypso.xml.util.TransformationType;

/**
 * 
 * @author belger
 */
public class ModelNature implements IProjectNature
{
  public static final String CALCTYPE = "calcType";
  
  public static final String ID = "org.kalypso.ui.ModelNature";
  
  private IProject m_project;

  private final Properties m_metadata = new Properties();

  private static final String METADATA_FILE = ".metadata";

  private static final String CALCULATION_FILE = ".calculation";

  /**
   * @see org.eclipse.core.resources.IProjectNature#configure()
   */
  public void configure() throws CoreException
  {
    try
    {
      final IFile file = m_project.getFile( new Path( METADATA_FILE ) );
      m_metadata.load( file.getContents() );
    }
    catch( final IOException e )
    {
      throw new CoreException( new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0,
          "Error loading Metadata", e ) );
    }
  }

  /**
   * @see org.eclipse.core.resources.IProjectNature#deconfigure()
   */
  public void deconfigure() throws CoreException
  {
    try
    {
      final IFile file = m_project.getFile( new Path( METADATA_FILE ) );

      final ByteArrayOutputStream bos = new ByteArrayOutputStream();
      m_metadata.store( bos, "Modell-Projekt Metadata Information" );
      bos.close();

      final ByteArrayInputStream bis = new ByteArrayInputStream( bos.toByteArray() );

      if( file.exists() )
        file.setContents( bis, false, true, new NullProgressMonitor() );
      else
        file.create( bis, false, new NullProgressMonitor() );

      bis.close();
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see org.eclipse.core.resources.IProjectNature#getProject()
   */
  public IProject getProject()
  {
    return m_project;
  }

  /**
   * @see org.eclipse.core.resources.IProjectNature#setProject(org.eclipse.core.resources.IProject)
   */
  public void setProject( final IProject project )
  {
    m_project = project;
  }

  public static String checkCanCreateCalculationCase( final IPath path )
  {
    final IWorkspaceRoot resourceRoot = ResourcesPlugin.getWorkspace().getRoot();
    final IResource resource = resourceRoot.findMember( path );

    if( resource == null || resource == resourceRoot )
      return "Rechenfall muss innerhalb eines Projektordners angelegt werden";

    if( resource instanceof IFolder )
    {
      final IFolder folder = (IFolder)resource;
      if( isCalcCalseFolder( folder ) )
        return "Rechenfall darf nicht innerhalb eines anderen Rechenfalls angelegt werden";

      return checkCanCreateCalculationCase( folder.getParent().getFullPath() );
    }
    else if( resource instanceof IProject )
    {
      final IProject project = (IProject)resource;
      try
      {
        project.isNatureEnabled( ModelNature.ID );
        return null;
      }
      catch( CoreException e )
      {
        e.printStackTrace();

        return e.getMessage();
      }
    }

    return "???";
  }
  
  public static boolean isCalcCalseFolder( final IFolder folder )
  {
    final IResource calcFile = folder.findMember( CALCULATION_FILE );
    return ( calcFile != null && calcFile.exists() && calcFile instanceof IFile );
  }

  public static void createCalculationCaseInFolder( final IFolder folder,
      final IProgressMonitor monitor ) throws Exception
  {
    monitor.beginTask( "Rechenfall erzeugen...", 1000 );
    
    // maybe check requirements? (Project is of this nature an folder is not
    // contained in other CalcCase)

    final IFile calcFile = folder.getFile( CALCULATION_FILE );

    final ByteArrayInputStream bis = new ByteArrayInputStream( new byte[] {} );
    calcFile.create( bis, false, new SubProgressMonitor( monitor, 500 ) );
    bis.close();

    tranformModelData( folder, new SubProgressMonitor(monitor, 500 ) );
  }

  private static void tranformModelData( final IFolder targetFolder, final IProgressMonitor monitor )
      throws JAXBException, CoreException, IOException, TransformationException
  {
    // load Transformer config
    final IFile tranformerConfigFile = (IFile)targetFolder.getProject().findMember(
        "modellTyp/calcCaseConfig.xml" );

    final InputStream contents = tranformerConfigFile.getContents();
    final TransformationConfig trans = (TransformationConfig)new ObjectFactory()
        .createUnmarshaller().unmarshal( contents );
    contents.close();

    final List transList = trans.getTransformation();
    for( Iterator iter = transList.iterator(); iter.hasNext(); )
    {
      final TransformationType element = (TransformationType)iter.next();
      final CalculationCaseTransformation ccTrans = TransformationFactory
          .createTransformation( element );

      ccTrans.transform( targetFolder, monitor );
    }
  }

  public String getCalcType()
  {
    return getMetadata().getProperty( ModelNature.CALCTYPE );
  }

  private Properties getMetadata()
  {
    return m_metadata;
  }

}