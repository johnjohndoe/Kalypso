package org.kalypso.afgui.scenarios;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.ScenarioHandlingProjectNature;
import org.kalypso.afgui.i18n.Messages;
import org.kalypso.commons.bind.JaxbUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;

import de.renew.workflow.cases.Case;
import de.renew.workflow.connector.cases.AbstractCaseManager;
import de.renew.workflow.connector.cases.ICase;

/**
 * This implementation of {@link ICaseManager} persists the scenario model data in the project workspace. Information
 * about the scenarios is kept in the project .metadata folder.
 * 
 * @author Stefan Kurzbach
 */
public class ScenarioManager extends AbstractCaseManager<IScenario> implements IScenarioManager
{
  private static final Logger logger = Logger.getLogger( ScenarioManager.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.afgui/debug" ) ); //$NON-NLS-1$

  static
  {
    if( !log )
    {
      logger.setUseParentHandlers( false );
    }
  }

  /**
   * Initializes the {@link ScenarioManager} on the given project
   * 
   * @param project
   *          the project, must not be <code>null</code>
   * @exception CoreException
   *              if this method fails. Reasons include:
   *              <ul>
   *              <li>The metadata folder is not accessible.</li>
   *              <li>There is a problem loading the database.</li>
   */
  public ScenarioManager( final IProject project ) throws CoreException
  {
    super( project, JaxbUtilities.createQuiet( org.kalypso.afgui.scenarios.ObjectFactory.class, de.renew.workflow.cases.ObjectFactory.class ) );
  }

  /**
   * @see de.renew.workflow.connector.context.SimpleCaseManager#createCase(java.lang.String)
   */
  @Override
  public IScenario createCase( final String name )
  {
    final Scenario newScenario = new org.kalypso.afgui.scenarios.ObjectFactory().createScenario();
    newScenario.setName( name );

    final IScenario scenario = new ScenarioHandler( newScenario, m_project );
    internalAddCase( scenario );
    persist( null );
    fireCaseAdded( scenario );

    return scenario;
  }

  /**
   * @see org.kalypso.afgui.scenarios.IScenarioManager#deriveScenario(java.lang.String,
   *      org.kalypso.afgui.scenarios.Scenario)
   */
  public IScenario deriveScenario( final String name, final IScenario parentScenario )
  {
    final org.kalypso.afgui.scenarios.ObjectFactory of = new org.kalypso.afgui.scenarios.ObjectFactory();
    final Scenario newScenario = of.createScenario();
    try
    {
      newScenario.setURI( parentScenario.getURI() + "/" + URLEncoder.encode( name, "UTF-8" ) ); //$NON-NLS-1$ //$NON-NLS-2$
    }
    catch( final UnsupportedEncodingException e )
    {
      KalypsoAFGUIFrameworkPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }
    newScenario.setName( name );
    newScenario.setParentScenario( parentScenario.getScenario() );

    ScenarioList derivedScenarios = parentScenario.getScenario().getDerivedScenarios();
    if( derivedScenarios == null )
    {
      derivedScenarios = of.createScenarioList();
      parentScenario.getScenario().setDerivedScenarios( derivedScenarios );
    }
    derivedScenarios.getScenarios().add( newScenario );

    persist( null );

    final ScenarioHandler scenario = new ScenarioHandler( newScenario, m_project );

    fireCaseAdded( scenario );
    return scenario;
  }

  /**
   * TODO handling of sub scenarios
   */
  public IScenario cloneScenario( final String name, final IScenario toClone ) throws CoreException
  {
    final IScenario parentScenario = toClone.getParentScenario();
    try
    {
      final IProjectNature nature = toClone.getProject().getNature( ScenarioHandlingProjectNature.ID );
      final ScenarioHandlingProjectNature scenarioNature = (ScenarioHandlingProjectNature) nature;
      final IDerivedScenarioCopyFilter filter = scenarioNature.getDerivedScenarioCopyFilter();
      scenarioNature.setDerivedScenarioCopyFilter( new IDerivedScenarioCopyFilter()
      {
        @Override
        public boolean shouldBeCopied( final IResource resource )
        {
          return false;
        }
      } );

      final IScenario derived = deriveScenario( name, parentScenario );
      /** problem - project nature copies project files -> take data from toClone and not from parent!!! */

      final IFolder destinationFolder = derived.getFolder();

      // TODO works with subscenarios - i don't believe?!?
      final IFolder srcFolder = toClone.getFolder();
      final IResource[] members = srcFolder.members( false );
      for( final IResource resource : members )
      {
        if( resource.getName().equals( destinationFolder.getName() ) )
        {
          // ignore scenario folder and .* resources
          continue;
        }
        else
        {
          resource.copy( destinationFolder.getFullPath().append( resource.getName() ), false, null );
        }
      }

      persist( null );

      new UIJob( "" ) //$NON-NLS-1$
      {
        @Override
        public IStatus runInUIThread( final IProgressMonitor monitor )
        {
          scenarioNature.setDerivedScenarioCopyFilter( filter );

          return Status.OK_STATUS;
        }
      }.schedule( 250 );

      return derived;
    }
    catch( final Exception e )
    {
      KalypsoAFGUIFrameworkPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );

      throw new CoreException( StatusUtilities.createErrorStatus( Messages.getString("org.kalypso.afgui.scenarios.ScenarioManager.3"), e ) ); //$NON-NLS-1$
    }

  }

  /**
   *
   */
  @Override
  public void removeCase( final IScenario scenario, IProgressMonitor monitor ) throws CoreException
  {
    if( monitor == null )
    {
      monitor = new NullProgressMonitor();
    }
    try
    {
      monitor.beginTask( Messages.getString( "org.kalypso.afgui.scenarios.ScenarioManager.4" ), 100 ); //$NON-NLS-1$
      final IScenarioList derivedScenarios = scenario.getDerivedScenarios();
      // only remove if no derived scenarios
      if( derivedScenarios != null && !derivedScenarios.getScenarios().isEmpty() )
      {
        throw new CoreException( StatusUtilities.createErrorStatus( Messages.getString( "org.kalypso.afgui.scenarios.ScenarioManager.5" ) ) ); //$NON-NLS-1$
      }
      final IScenario parentScenario = scenario.getParentScenario();
      if( parentScenario == null )
      {
        // base scenario
        internalRemoveCase( scenario );
      }
      else
      {
        parentScenario.getDerivedScenarios().getScenarios().remove( scenario );
      }
      monitor.worked( 5 );
      persist( null );
    }
    finally
    {
      monitor.done();
    }
    fireCaseRemoved( scenario );
  }

  /**
   * @see org.kalypso.afgui.scenarios.IScenarioManager#getScenario(java.lang.String)
   */
  public IScenario getCase( final String id )
  {
    IScenario result = null;
    for( final IScenario scenario : getCases() )
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
   * @see de.renew.workflow.connector.context.ICaseManager#getCases()
   */
  public List<IScenario> getCases( )
  {
    // FIXME

    final List<IScenario> resultList = new ArrayList<IScenario>();
    final List<ICase> internalCases = internalGetCases();
    for( final ICase caze : internalCases )
    {
      final Case myCaze = caze.getCase();
      if( myCaze instanceof Scenario )
      {
        resultList.add( new ScenarioHandler( (Scenario) myCaze, caze.getProject() ) );
      }
    }

    return resultList;
  }

  /**
   * Returns a scenario with the given id in the context of the parentScenario or null if no such scenario exists
   */
  private IScenario findScenario( final IScenario parentScenario, final String id )
  {
    IScenario result = null;
    final IScenarioList derivedScenarios = parentScenario.getDerivedScenarios();
    if( parentScenario.getURI().equals( id ) )
    {
      result = parentScenario;
    }
    else if( derivedScenarios != null )
    {
      for( final IScenario derivedScenario : derivedScenarios.getScenarios() )
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
}
