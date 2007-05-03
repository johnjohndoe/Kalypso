package org.kalypso.afgui.scenarios;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import javax.xml.bind.JAXBContext;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilitites;
import org.kalypso.jwsdp.JaxbUtilities;

import de.renew.workflow.base.IWorkflowSystem;
import de.renew.workflow.base.Workflow;
import de.renew.workflow.base.WorkflowSystem;

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

  public static final String SCENARIO_BASE_URI = "scenario://${project}/${scenarioPath}";

  private static final Logger logger = Logger.getLogger( ScenarioManager.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.afgui/debug" ) );

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  private static final JAXBContext JC = JaxbUtilities.createQuiet( org.kalypso.afgui.scenarios.ObjectFactory.class, de.renew.workflow.cases.ObjectFactory.class );

  private final List<IScenarioManagerListener> m_listeners = new ArrayList<IScenarioManagerListener>();

  private ProjectScenarios m_projectScenarios;

  private final IProject m_project;

  private final IFile m_metaDataFile;

  private Scenario m_currentScenario;

  private IWorkflowSystem m_currentWorkflow;

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
  public ScenarioManager( final IProject project )
  {
    final IFolder folder = project.getFolder( METADATA_FOLDER );
    final IFile metadataFile = folder.getFile( METADATA_FILENAME );
    m_project = project;
    m_metaDataFile = metadataFile;
    final IWorkspaceRunnable runnable = new IWorkspaceRunnable()
    {
      public void run( IProgressMonitor monitor ) throws CoreException
      {
        if( monitor == null )
        {
          monitor = new NullProgressMonitor();
        }
        try
        {
          monitor.beginTask( "Szenarien laden", 40 );

          project.refreshLocal( IResource.DEPTH_INFINITE, new SubProgressMonitor( monitor, 10 ) );
          if( !folder.exists() )
          {
            folder.create( false, true, new SubProgressMonitor( monitor, 10 ) );
          }
          if( !metadataFile.exists() )
          {
            m_projectScenarios = new org.kalypso.afgui.scenarios.ObjectFactory().createProjectScenarios();
            createBaseScenario( "Basis" );
            monitor.worked( 10 );
          }
          else
          {
            m_projectScenarios = loadModel( metadataFile );
            monitor.worked( 10 );
          }
          m_currentWorkflow = new WorkflowSystem( m_project );
          monitor.worked( 10 );
        }
        finally
        {
          monitor.done();
        }
      }

      /**
       * Loads the {@link ProjectScenarios} from a file at the given location
       */
      private ProjectScenarios loadModel( final IFile file ) throws CoreException
      {
        try
        {
          final URL url = file.getRawLocationURI().toURL();
          return (ProjectScenarios) JC.createUnmarshaller().unmarshal( url );
        }
        catch( final Throwable e )
        {
          final IStatus status = StatusUtilities.statusFromThrowable( e );
          throw new CoreException( status );
        }
      }
    };

    final ICoreRunnableWithProgress runnable2 = new ICoreRunnableWithProgress()
    {
      public IStatus execute( IProgressMonitor monitor ) throws CoreException
      {
        try
        {
          project.getWorkspace().run( runnable, m_project, IWorkspace.AVOID_UPDATE, monitor );
        }
        catch( Throwable t )
        {
          throw new CoreException( StatusUtilities.statusFromThrowable( t ) );
        }
        return Status.OK_STATUS;
      }
    };
    ProgressUtilitites.busyCursorWhile( runnable2, "Problem beim Laden der Szenarienbeschreibung" );
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
   * @see org.kalypso.afgui.scenarios.IScenarioManager#createBaseScenario(java.lang.String)
   */
  public Scenario createBaseScenario( final String name ) throws CoreException
  {
    final Scenario newScenario = new Scenario();
    final String uri = SCENARIO_BASE_URI.replaceFirst( Pattern.quote( "${project}" ), m_project.getName() ).replaceFirst( Pattern.quote( "${scenarioPath}" ), name );
    newScenario.setURI( uri );
    newScenario.setName( name );
    m_projectScenarios.getScenarios().add( newScenario );

    persist( null );
    fireScenarioAdded( newScenario );
    return newScenario;
  }

  /**
   * @see org.kalypso.afgui.scenarios.IScenarioManager#deriveScenario(java.lang.String,
   *      org.kalypso.afgui.scenarios.Scenario)
   */
  public Scenario deriveScenario( final String name, final Scenario parentScenario ) throws CoreException
  {
    final Scenario newScenario = new Scenario();
    newScenario.setURI( parentScenario.getURI() + "/" + name );
    newScenario.setName( name );
    newScenario.setParentScenario( parentScenario );

    ScenarioList derivedScenarios = parentScenario.getDerivedScenarios();
    if( derivedScenarios == null )
    {
      derivedScenarios = new ScenarioList();
      parentScenario.setDerivedScenarios( derivedScenarios );
    }
    derivedScenarios.getScenarios().add( newScenario );

    persist( null );
    fireScenarioAdded( newScenario );
    return newScenario;
  }

  public void removeScenario( final Scenario scenario, IProgressMonitor monitor ) throws CoreException
  {
    if( monitor == null )
    {
      monitor = new NullProgressMonitor();
    }
    try
    {
      monitor.beginTask( "Szenario löschen", 100 );
      final ScenarioList derivedScenarios = scenario.getDerivedScenarios();
      // only remove if no derived scenarios
      if( derivedScenarios != null && !derivedScenarios.getScenarios().isEmpty() )
      {
        throw new CoreException( StatusUtilities.createErrorStatus( "Das Szenario enthält abgeleitete Szenarien und kann nicht gelöscht werden." ) );
      }
      final Scenario parentScenario = scenario.getParentScenario();
      if( parentScenario == null )
      {
        // base scenario
        m_projectScenarios.getScenarios().remove( scenario );
      }
      else
      {
        parentScenario.getDerivedScenarios().getScenarios().remove( scenario );
      }
      monitor.worked( 5 );
      persist( new SubProgressMonitor( monitor, 15 ) );
    }
    finally
    {
      monitor.done();
    }
    fireScenarioRemoved( scenario );
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

  void fireScenarioAdded( final Scenario scenario )
  {
    for( final IScenarioManagerListener l : m_listeners )
    {
      l.scenarioAdded( scenario );
    }
  }

  private void fireScenarioRemoved( final Scenario scenario )
  {
    for( final IScenarioManagerListener l : m_listeners )
    {
      l.scenarioRemoved( scenario );
    }
  }

  /**
   * @see org.kalypso.afgui.scenarios.IScenarioManager#getCurrentWorkflow()
   */
  public Workflow getCurrentWorkflow( )
  {
    if( m_currentWorkflow == null )
    {
      return null;
    }
    else
    {
      return m_currentWorkflow.getCurrentWorkflow();
    }
  }
}
