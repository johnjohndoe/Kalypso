package org.kalypso.afgui.db;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Platform;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.jwsdp.JaxbUtilities;
import org.kalypso.scenarios.ProjectScenarios;
import org.kalypso.scenarios.Scenario;
import org.kalypso.scenarios.ScenarioList;

/**
 * @author Patrice Congo, Stefan Kurzbach
 */
public class WorkflowDB implements IWorkflowDB
{
  private static final Logger logger = Logger.getLogger( WorkflowDB.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.afgui/debug" ) );

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  private static final JAXBContext JC = JaxbUtilities.createQuiet( org.kalypso.scenarios.ObjectFactory.class );

  private final List<IWorkflowDBChangeListerner> m_listeners = new ArrayList<IWorkflowDBChangeListerner>();

  private ProjectScenarios m_projectScenarios;

  private IFile m_file;

  /**
   *
   */
  public WorkflowDB( final IFile file ) throws JAXBException
  {
    try
    {
      loadModel( file.getRawLocationURI().toURL() );
      this.m_file = file;
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
    }
  }

  private void loadModel( final URL url ) throws JAXBException
  {
    m_projectScenarios = (ProjectScenarios) JC.createUnmarshaller().unmarshal( url );
    fireWorkflowDBChange();
  }

  /**
   * @see org.kalypso.afgui.db.IWorkflowDB#createWorkflowData(java.lang.String, org.kalypso.scenarios.Scenario)
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
    persist();
    fireWorkflowDBChange();
    return newScenario;
  }

  /**
   * @see org.kalypso.afgui.db.IWorkflowDB#getWorkflowDataById(java.lang.String)
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

  public Scenario findScenario( final Scenario parentScenario, final String id )
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
   * @see org.kalypso.afgui.db.IWorkflowDB#addWorkflowDBChangeListener(org.kalypso.afgui.db.IWorkflowDBChangeListerner)
   */
  public void addWorkflowDBChangeListener( IWorkflowDBChangeListerner l )
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
   * @see org.kalypso.afgui.db.IWorkflowDB#removeWorkflowDBChangeListener(org.kalypso.afgui.db.IWorkflowDBChangeListerner)
   */
  public void removeWorkflowDBChangeListener( IWorkflowDBChangeListerner l )
  {
    if( l == null )
    {
      return;
    }
    else
    {
      if( m_listeners.contains( l ) )
      {
        m_listeners.remove( l );
      }
    }
  }

  /**
   * @see org.kalypso.afgui.db.IWorkflowDB#dispose()
   */
  public void dispose( )
  {
    m_listeners.clear();
  }

  private void fireWorkflowDBChange( )
  {
    for( IWorkflowDBChangeListerner l : m_listeners )
    {
      l.workflowDBChanged();
    }
  }

  public void persist( ) throws CoreException
  {
    ByteArrayInputStream bis = null;
    try
    {
      final ByteArrayOutputStream bos = new ByteArrayOutputStream();
      JC.createMarshaller().marshal( m_projectScenarios, bos );
      bis = new ByteArrayInputStream( bos.toByteArray() );
      bos.close();
      if( m_file.exists() )
      {
        try
        {
          m_file.refreshLocal( IResource.DEPTH_ONE, null );
        }
        catch( final Exception e )
        {
        }
        m_file.setContents( bis, false, true, null );
      }
      else
      {
        m_file.create( bis, false, null );
      }
    }
    catch( final Exception e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }
    finally
    {
      IOUtils.closeQuietly( bis );
    }
  }

  /**
   * @see org.kalypso.afgui.db.IWorkflowDB#getRootScenario()
   */
  public List<Scenario> getRootScenarios( )
  {
    return m_projectScenarios.getScenarios();
  }
}
