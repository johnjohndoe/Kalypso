package de.renew.workflow.connector.cases;

import java.util.Map;
import java.util.TreeMap;
import java.util.logging.Logger;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.AbstractSourceProvider;

import de.renew.workflow.cases.Case;
import de.renew.workflow.connector.context.ActiveWorkContext;
import de.renew.workflow.connector.context.IActiveContextChangeListener;
import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

public class CaseHandlingSourceProvider<T extends Case, D extends Object> extends AbstractSourceProvider implements ICaseHandlingSourceProvider
{
  private static final Logger LOGGER = Logger.getLogger( CaseHandlingSourceProvider.class.getName() );

  static
  {
    final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.kalypso1d2d.pjt/debug" ) );
    if( !log )
    {
      LOGGER.setUseParentHandlers( false );
    }
  }

  private static final String[] PROVIDED_SOURCE_NAMES = new String[] { ACTIVE_CASE_FOLDER_NAME, ACTIVE_CASE_DATA_PROVIDER_NAME, ACTIVE_CASE_URI_NAME };

  protected ActiveWorkContext<T> m_activeWorkContext;

  /** data provider for the current szenario */
  private final ICaseDataProvider<D> m_dataProvider;

  private final IActiveContextChangeListener<T> workContextChangeListener = new IActiveContextChangeListener<T>()
  {
    @SuppressWarnings("synthetic-access")
    public void activeContextChanged( final CaseHandlingProjectNature newProject, T scenario )
    {
      m_dataProvider.setCurrent( getSzenarioFolder() );

      fireSourceChanged( 0, getCurrentState() );
    }
  };

  public CaseHandlingSourceProvider( final ActiveWorkContext<T> context, final ICaseDataProvider<D> dataProvider )
  {
    m_activeWorkContext = context;
    m_dataProvider = dataProvider;
    m_activeWorkContext.addActiveContextChangeListener( workContextChangeListener );
  }

  /**
   * @see org.eclipse.ui.ISourceProvider#dispose()
   */
  public void dispose( )
  {
    m_activeWorkContext.removeActiveContextChangeListener( workContextChangeListener );
  }

  /**
   * @see org.eclipse.ui.ISourceProvider#getCurrentState()
   */
  @SuppressWarnings("unchecked")
  public Map getCurrentState( )
  {
    final Map currentState = new TreeMap();
    currentState.put( ACTIVE_CASE_FOLDER_NAME, getSzenarioFolder() );
    currentState.put( ACTIVE_CASE_DATA_PROVIDER_NAME, getDataProvider() );
    currentState.put( ACTIVE_CASE_URI_NAME, getSzenarioUri() );
    return currentState;
  }

  /**
   * @see org.eclipse.ui.ISourceProvider#getProvidedSourceNames()
   */
  public String[] getProvidedSourceNames( )
  {
    return PROVIDED_SOURCE_NAMES;
  }

  private String getSzenarioUri( )
  {
    final T currentCase = m_activeWorkContext.getCurrentCase();
    if( currentCase == null )
      return null;
    else
      return currentCase.getURI();
  }

  private IContainer getSzenarioFolder( )
  {
    final T currentCase = m_activeWorkContext.getCurrentCase();
    final CaseHandlingProjectNature currentProject = m_activeWorkContext.getCurrentProject();
    if( currentProject == null || currentCase == null )
      return null;
    else
    {
      // TODO: is this really up to date? We allways assume that the scenarioFolder is a IFolder
      // TODO: comment why we need that
      final IPath projectPath = currentProject.getRelativeProjectPath( currentCase );
      if( projectPath.isEmpty() )
        return currentProject.getProject();
      return currentProject.getProject().getFolder( projectPath );
    }
  }

  private ICaseDataProvider<D> getDataProvider( )
  {
    return m_dataProvider;
  }
}
