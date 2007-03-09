package org.kalypso.kalypso1d2d.pjt;

import java.util.Map;
import java.util.TreeMap;
import java.util.logging.Logger;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.AbstractSourceProvider;
import org.kalypso.afgui.scenarios.Scenario;
import org.kalypso.kalypso1d2d.pjt.views.ISzenarioDataProvider;

public class SzenarioSourceProvider extends AbstractSourceProvider
{
  public static final String ACTIVE_SZENARIO_DATA_PROVIDER_NAME = "activeSzenarioDataProvider";

  public static final String ACTIVE_SZENARIO_FOLDER_NAME = "activeSimulationModelBaseFolder";

  private static final Logger LOGGER = Logger.getLogger( SzenarioSourceProvider.class.getName() );

  static
  {
    final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.kalypso1d2d.pjt/debug" ) );
    if( !log )
      LOGGER.setUseParentHandlers( false );
  }

  private static final String[] PROVIDED_SOURCE_NAMES = new String[] { ACTIVE_SZENARIO_FOLDER_NAME, ACTIVE_SZENARIO_DATA_PROVIDER_NAME };

  protected ActiveWorkContext activeWorkContext;

  private IActiveContextChangeListener workContextChangeListener = new IActiveContextChangeListener()
  {

    @SuppressWarnings("synthetic-access")
    public void activeContextChanged( final IProject newProject, Scenario scenario )
    {
      fireSourceChanged( 0, getCurrentState() );
    }

  };

  public SzenarioSourceProvider( final ActiveWorkContext context )
  {
    activeWorkContext = context;
    activeWorkContext.addActiveContextChangeListener( workContextChangeListener );
  }

  /**
   * @see org.eclipse.ui.ISourceProvider#dispose()
   */
  public void dispose( )
  {
    activeWorkContext = null;
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
    return activeWorkContext.getCurrentScenarioFolder();
  }

  private ISzenarioDataProvider getDataProvider( )
  {
    return activeWorkContext.getSzenarioDataProvider();
  }
}
