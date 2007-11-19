package org.kalypso.afgui.scenarios;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.jwsdp.JaxbUtilities;

import de.renew.workflow.cases.Case;
import de.renew.workflow.connector.cases.AbstractCaseManager;

/**
 * This implementation of {@link ICaseManager} persists the scenario model data in the project workspace. Information
 * about the scenarios is kept in the project .metadata folder.
 * 
 * @author Stefan Kurzbach
 */
public class ScenarioManager extends AbstractCaseManager<Scenario> implements IScenarioManager
{
  private static final Logger logger = Logger.getLogger( ScenarioManager.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.afgui/debug" ) ); //$NON-NLS-1$

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  /**
   * Initializes the {@link ScenarioManager} on the given project
   * 
   * @param project
   *            the project, must not be <code>null</code>
   * @exception CoreException
   *                if this method fails. Reasons include:
   *                <ul>
   *                <li> The metadata folder is not accessible.</li>
   *                <li> There is a problem loading the database.</li>
   */
  public ScenarioManager( final IProject project ) throws CoreException
  {
    super( project, JaxbUtilities.createQuiet( org.kalypso.afgui.scenarios.ObjectFactory.class, de.renew.workflow.cases.ObjectFactory.class ) );
  }

  /**
   * @see de.renew.workflow.connector.context.SimpleCaseManager#createCase(java.lang.String)
   */
  @Override
  public Scenario createCase( final String name )
  {
    final Scenario newScenario = new org.kalypso.afgui.scenarios.ObjectFactory().createScenario();
    final String uri = CASE_BASE_URI.replaceFirst( Pattern.quote( "${project}" ), m_project.getName() ).replaceFirst( Pattern.quote( "${casePath}" ), name ); //$NON-NLS-1$ //$NON-NLS-2$
    newScenario.setURI( uri );
    newScenario.setName( name );
    internalAddCase( newScenario );

    persist( null );
    fireCaseAdded( newScenario );
    return newScenario;
  }

  /**
   * @see org.kalypso.afgui.scenarios.IScenarioManager#deriveScenario(java.lang.String,
   *      org.kalypso.afgui.scenarios.Scenario)
   */
  public Scenario deriveScenario( final String name, final Scenario parentScenario )
  {
    final org.kalypso.afgui.scenarios.ObjectFactory of = new org.kalypso.afgui.scenarios.ObjectFactory();
    final Scenario newScenario = of.createScenario();
    newScenario.setURI( parentScenario.getURI() + "/" + name ); //$NON-NLS-1$
    newScenario.setName( name );
    newScenario.setParentScenario( parentScenario );

    ScenarioList derivedScenarios = parentScenario.getDerivedScenarios();
    if( derivedScenarios == null )
    {
      derivedScenarios = of.createScenarioList();
      parentScenario.setDerivedScenarios( derivedScenarios );
    }
    derivedScenarios.getScenarios().add( newScenario );

    persist( null );
    fireCaseAdded( newScenario );
    return newScenario;
  }

  /**
   * 
   */
  @Override
  public void removeCase( final Scenario scenario, IProgressMonitor monitor ) throws CoreException
  {
    if( monitor == null )
    {
      monitor = new NullProgressMonitor();
    }
    try
    {
      monitor.beginTask( Messages.getString( "ScenarioManager.4" ), 100 ); //$NON-NLS-1$
      final ScenarioList derivedScenarios = scenario.getDerivedScenarios();
      // only remove if no derived scenarios
      if( derivedScenarios != null && !derivedScenarios.getScenarios().isEmpty() )
      {
        throw new CoreException( StatusUtilities.createErrorStatus( Messages.getString( "ScenarioManager.5" ) ) ); //$NON-NLS-1$
      }
      final Scenario parentScenario = scenario.getParentScenario();
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
  public Scenario getCase( final String id )
  {
    Scenario result = null;
    for( final Scenario scenario : getCases() )
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
  public List<Scenario> getCases( )
  {
    final List<Scenario> resultList = new ArrayList<Scenario>();
    final List<Case> internalCases = internalGetCases();
    for( final Case caze : internalCases )
    {
      resultList.add( (Scenario) caze );
    }
    return resultList;
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
}
