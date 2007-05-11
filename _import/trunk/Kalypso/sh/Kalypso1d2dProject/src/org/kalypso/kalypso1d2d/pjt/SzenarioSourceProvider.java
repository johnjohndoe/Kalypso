package org.kalypso.kalypso1d2d.pjt;

import java.util.Map;
import java.util.TreeMap;
import java.util.logging.Logger;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.AbstractSourceProvider;
import org.kalypso.afgui.scenarios.Scenario;
import org.kalypso.kalypso1d2d.pjt.views.SzenarioDataProvider;

import de.renew.workflow.base.ISzenarioSourceProvider;
import de.renew.workflow.cases.ICaseDataProvider;
import de.renew.workflow.connector.context.ActiveWorkContext;
import de.renew.workflow.connector.context.CaseHandlingProjectNature;
import de.renew.workflow.connector.context.IActiveContextChangeListener;

public class SzenarioSourceProvider extends AbstractSourceProvider implements ISzenarioSourceProvider
{
  private static final Logger LOGGER = Logger.getLogger( SzenarioSourceProvider.class.getName() );

  static
  {
    final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.kalypso1d2d.pjt/debug" ) );
    if( !log )
      LOGGER.setUseParentHandlers( false );
  }

  private static final String[] PROVIDED_SOURCE_NAMES = new String[] { ACTIVE_SZENARIO_FOLDER_NAME, ACTIVE_SZENARIO_DATA_PROVIDER_NAME };

  protected ActiveWorkContext<Scenario> activeWorkContext;

  /** data provider for the current szenario */
  private final SzenarioDataProvider m_dataProvider = new SzenarioDataProvider();

  private IActiveContextChangeListener<Scenario> workContextChangeListener = new IActiveContextChangeListener<Scenario>()
  {
    @SuppressWarnings("synthetic-access")
    public void activeContextChanged( final CaseHandlingProjectNature<Scenario> newProject, Scenario scenario )
    {
      m_dataProvider.setCurrent( getSzenarioFolder() );

      fireSourceChanged( 0, getCurrentState() );
    }
  };

  public SzenarioSourceProvider( final ActiveWorkContext<Scenario> context )
  {
    activeWorkContext = context;
    activeWorkContext.addActiveContextChangeListener( workContextChangeListener );
    m_dataProvider.setCurrent( getSzenarioFolder() );
  }

  /**
   * @see org.eclipse.ui.ISourceProvider#dispose()
   */
  public void dispose( )
  {
    activeWorkContext.removeActiveContextChangeListener( workContextChangeListener );
  }

  /**
   * @see org.eclipse.ui.ISourceProvider#getCurrentState()
   */
  @SuppressWarnings("unchecked")
  public Map getCurrentState( )
  {
    final Map currentState = new TreeMap();
    currentState.put( ACTIVE_SZENARIO_FOLDER_NAME, getSzenarioFolder() );
    currentState.put( ACTIVE_SZENARIO_DATA_PROVIDER_NAME, getDataProvider() );
    return currentState;
  }

  /**
   * @see org.eclipse.ui.ISourceProvider#getProvidedSourceNames()
   */
  public String[] getProvidedSourceNames( )
  {
    return PROVIDED_SOURCE_NAMES;
  }

  public static IFolder findModelContext( final IFolder szenarioFolder, final String modelFile )
  {
    if( szenarioFolder == null )
      return null;

    if( szenarioFolder.getFile( modelFile ).exists() )
    {
      return szenarioFolder;
    }
    final IContainer parent = szenarioFolder.getParent();
    if( parent.getType() != IResource.PROJECT )
      return findModelContext( (IFolder) parent, modelFile );
    else
      return null;

  }

  private IFolder getSzenarioFolder( )
  {
    return activeWorkContext.getCurrentCaseFolder();
  }

  private ICaseDataProvider getDataProvider( )
  {
    return m_dataProvider;
  }
}
