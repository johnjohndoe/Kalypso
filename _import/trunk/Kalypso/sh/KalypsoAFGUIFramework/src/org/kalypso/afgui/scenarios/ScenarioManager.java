package org.kalypso.afgui.scenarios;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.jwsdp.JaxbUtilities;

/**
 * This implementation of {@link IScenarioManager} persists the scenario model data in the project workspace.
 * Information about the scenarios is kept in the project .metadata folder.
 * 
 * @author Stefan Kurzbach
 */
public class ScenarioManager implements IScenarioManager
{
  public static final String METADATA_FOLDER = ".metadata";

  public static final String METADATA_FILENAME = "scenarios.xml";

  private static final Logger logger = Logger.getLogger( ScenarioManager.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.afgui/debug" ) );

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  private static final JAXBContext JC = JaxbUtilities.createQuiet( org.kalypso.afgui.scenarios.ObjectFactory.class );

  private final List<IScenarioManagerListener> m_listeners = new ArrayList<IScenarioManagerListener>();

  ProjectScenarios m_projectScenarios;

  private final IProject m_project;

  private final IFile m_metaDataFile;

  private Scenario m_currentScenario;

  /**
   * Initializes the {@link ScenarioManager} on the given project
   * 
   * @param project
   *          the project, must not be <code>null</code>
   * @exception CoreException
   *              if this method fails. Reasons include:
   *              <ul>
   *              <li> The metadata folder is not accessible.</li>
   *              <li> There is a problem loading the database.</li>
   */
  public ScenarioManager( final IProject project ) throws CoreException
  {
    final IFolder folder = project.getFolder( METADATA_FOLDER );
    final IFile metadataFile = folder.getFile( METADATA_FILENAME );
    m_project = project;
    m_metaDataFile = metadataFile;
    final IWorkspaceRunnable action = new IWorkspaceRunnable()
    {
      public void run( final IProgressMonitor monitor ) throws CoreException
      {
        try
        {
          project.refreshLocal( IResource.DEPTH_INFINITE, null );
          if( !folder.exists() )
          {
            folder.create( false, true, null );
          }
          if( !metadataFile.exists() )
          {
            m_projectScenarios = new org.kalypso.afgui.scenarios.ObjectFactory().createProjectScenarios();
            persist( null );
          }
          else
          {
            m_projectScenarios = loadModel( metadataFile.getRawLocationURI().toURL() );
          }
        }
        catch( final Throwable e )
        {
          // either JAXBException or MalformedURLException or CoreException
          IStatus status = StatusUtilities.statusFromThrowable( e );
          KalypsoAFGUIFrameworkPlugin.getDefault().getLog().log( status );
          throw new CoreException( status );
        }
      }
    };
    project.getWorkspace().run( action, null );
  }

  /**
   * Loads the {@link ProjectScenarios} from a file at the given location
   */
  ProjectScenarios loadModel( final URL url ) throws JAXBException
  {
    return (ProjectScenarios) JC.createUnmarshaller().unmarshal( url );
  }

  /**
   * @see org.kalypso.afgui.scenarios.IScenarioManager#getCurrentScenario()
   */
  public Scenario getCurrentScenario( )
  {
    return m_currentScenario;
  }

  /**
   * @see org.kalypso.afgui.scenarios.IScenarioManager#setCurrentScenario(org.kalypso.scenarios.Scenario)
   */
  public void setCurrentScenario( final Scenario scenario )
  {
    m_currentScenario = scenario;
  }

  /**
   * @see org.kalypso.afgui.scenarios.IScenarioManager#deriveScenario(java.lang.String, org.kalypso.scenarios.Scenario)
   */
  public Scenario deriveScenario( final String id, final Scenario parentScenario ) throws CoreException
  {
    final Scenario newScenario = new Scenario();
    newScenario.setURI( parentScenario.getURI() + "/" + id );
    newScenario.setName( id );
    newScenario.setParentScenario( parentScenario );
    ScenarioList derivedScenarios = parentScenario.getDerivedScenarios();
    if( derivedScenarios == null )
    {
      derivedScenarios = new ScenarioList();
      parentScenario.setDerivedScenarios( derivedScenarios );
    }
    derivedScenarios.getScenarios().add( newScenario );

    final IFolder newFolder = m_project.getFolder( getProjectPath( newScenario ) );
    newFolder.create( false, true, null );
    persist( null );
    fireWorkflowDBChange();
    return newScenario;
  }

  /**
   * @see org.kalypso.afgui.scenarios.IScenarioManager#getScenario(java.lang.String)
   */
  public Scenario getScenario( final String id )
  {
    Scenario result = null;
    for( final Scenario scenario : getRootScenarios() )
    {
      result = findScenario( scenario, id );
      if( result != null )
      {
        return result;
      }
    }
    return result;
  }

  /**
   * Returns a scenario with the given id in the context of the parentScenario or null if no such scenario exists
   */
  private Scenario findScenario( final Scenario parentScenario, final String id )
  {
    Scenario result = null;
    final ScenarioList derivedScenarios = parentScenario.getDerivedScenarios();
    if( parentScenario.getURI().equals( id ) )
    {
      result = parentScenario;
    }
    else if( derivedScenarios != null )
    {
      for( final Scenario derivedScenario : derivedScenarios.getScenarios() )
      {
        if( derivedScenario.getURI().equals( id ) )
        {
          result = derivedScenario;
        }
        else
        {
          result = findScenario( derivedScenario, id );
        }
        if( result != null )
        {
          return result;
        }
      }
    }
    return result;
  }

  /**
   * @see org.kalypso.afgui.scenarios.IScenarioManager#addScenarioManagerListener(org.kalypso.afgui.scenarios.IScenarioManagerListener)
   */
  public void addScenarioManagerListener( final IScenarioManagerListener l )
  {
    if( l == null )
    {
      return;
    }
    else
    {
      if( m_listeners.contains( l ) )
      {
        return;
      }
      else
      {
        m_listeners.add( l );
      }
    }
  }

  /**
   * @see org.kalypso.afgui.scenarios.IScenarioManager#removeScenarioManagerListener(org.kalypso.afgui.scenarios.IScenarioManagerListener)
   */
  public void removeScenarioManagerListener( final IScenarioManagerListener l )
  {
    if( l == null )
    {
      return;
    }
    else
    {
      m_listeners.remove( l );
    }
  }

  /**
   * @see org.kalypso.afgui.scenarios.IScenarioManager#dispose()
   */
  public void dispose( )
  {
    m_listeners.clear();
  }

  /**
   * @see org.kalypso.afgui.scenarios.IScenarioManager#persist(org.eclipse.core.runtime.IProgressMonitor)
   */
  public void persist( IProgressMonitor monitor ) throws CoreException
  {
    if( monitor == null )
    {
      monitor = new NullProgressMonitor();
    }

    ByteArrayInputStream bis = null;
    try
    {
      monitor.beginTask( "Szenarios speichern.", 5000 );
      final ByteArrayOutputStream bos = new ByteArrayOutputStream();
      JC.createMarshaller().marshal( m_projectScenarios, bos );
      monitor.worked( 2000 );
      bis = new ByteArrayInputStream( bos.toByteArray() );
      bos.close();
      m_metaDataFile.refreshLocal( IResource.DEPTH_ONE, new SubProgressMonitor( monitor, 1000 ) );
      if( m_metaDataFile.exists() )
      {
        m_metaDataFile.setContents( bis, false, true, new SubProgressMonitor( monitor, 2000 ) );
      }
      else
      {
        m_metaDataFile.create( bis, false, new SubProgressMonitor( monitor, 2000 ) );
      }
    }
    catch( final Exception e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }
    finally
    {
      IOUtils.closeQuietly( bis );
      monitor.done();
    }
  }

  /**
   * @see org.kalypso.afgui.scenarios.IScenarioManager#getRootScenarios()
   */
  public List<Scenario> getRootScenarios( )
  {
    return m_projectScenarios.getScenarios();
  }

  /**
   * Constructs a path for the scenario relative to the project location.
   */
  public IPath getProjectPath( final Scenario scenario )
  {
    if( scenario.getParentScenario() != null )
      return getProjectPath( scenario.getParentScenario() ).append( scenario.getName() );
    else
      return new Path( scenario.getName() );
  }

  void fireWorkflowDBChange( )
  {
    for( final IScenarioManagerListener l : m_listeners )
    {
      l.scenariosChanged();
    }
  }
}
