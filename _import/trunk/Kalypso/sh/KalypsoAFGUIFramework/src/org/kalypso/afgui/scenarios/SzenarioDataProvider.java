/**
 *
 */
package org.kalypso.afgui.scenarios;

import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.ObjectUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.command.ICommand;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.util.pool.IPoolListener;
import org.kalypso.core.util.pool.IPoolableObjectType;
import org.kalypso.core.util.pool.KeyInfo;
import org.kalypso.core.util.pool.PoolableObjectType;
import org.kalypso.core.util.pool.ResourcePool;
import org.kalypso.kalypsosimulationmodel.core.ICommandPoster;
import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.model.feature.GMLWorkspace;

import de.renew.workflow.connector.cases.ICaseDataProvider;

/**
 * Objects of this class are responsible for loading the gml-workspaces for the current selected simulation model and
 * provide them to the commands.
 * <p>
 * This is preliminary, because at the moment we assume that there is only one simulation model per project which is
 * always at the same place.
 * </p>
 * 
 * @author Gernot Belger
 */
public class SzenarioDataProvider implements ICaseDataProvider<IModel>, ICommandPoster
{
  private static final class KeyPoolListener implements IPoolListener
  {
    private final IPoolableObjectType m_key;

    private final List<IScenarioDataListener> m_controller;

    private final Class< ? > m_modelClass;

    public KeyPoolListener( final IPoolableObjectType key, final List<IScenarioDataListener> controller, final Class< ? > modelClass )
    {
      m_key = key;
      m_controller = controller;
      m_modelClass = modelClass;
    }

    public IPoolableObjectType getKey( )
    {
      return m_key;
    }

    /**
     * @see org.kalypso.util.pool.IPoolListener#dirtyChanged(org.kalypso.util.pool.IPoolableObjectType, boolean)
     */
    public void dirtyChanged( final IPoolableObjectType key, final boolean isDirty )
    {
    }

    /**
     * @see org.kalypso.util.pool.IPoolListener#isDisposed()
     */
    public boolean isDisposed( )
    {
      return false;
    }

    /**
     * @see org.kalypso.util.pool.IPoolListener#objectInvalid(org.kalypso.util.pool.IPoolableObjectType,
     *      java.lang.Object)
     */
    public void objectInvalid( final IPoolableObjectType key, final Object oldValue )
    {
    }

    /**
     * @see org.kalypso.util.pool.IPoolListener#objectLoaded(org.kalypso.util.pool.IPoolableObjectType,
     *      java.lang.Object, org.eclipse.core.runtime.IStatus)
     */
    public void objectLoaded( final IPoolableObjectType key, final Object newValue, final IStatus status )
    {
      if( newValue instanceof GMLWorkspace )
      {
        final GMLWorkspace workspace = (GMLWorkspace) newValue;

        // Adapting directly to IModel is dangerous because the mapping is not unique
        // (for example, 1d2d adapter factory as well as risk adapter factory are registered to adapt Feature to IModel)
        // TODO remove mappings to IModel from the factories
        final IModel model = (IModel) workspace.getRootFeature().getAdapter( m_modelClass );
        if( model != null )
        {
          fireModelLoaded( model, status );
        }

        // notify user once about messages during loading
        final IWorkbench workbench = PlatformUI.getWorkbench();
        if( workbench != null && !workbench.isClosing() && !status.isOK() )
        {
          final Display display = workbench.getDisplay();
          display.asyncExec( new Runnable()
          {

            public void run( )
            {
              final Shell activeShell = display.getActiveShell();
              ErrorDialog.openError( activeShell, "GML wurde geladen.", "Meldungen:", status );
            }
          } );
        }
      }
    }

    private void fireModelLoaded( final IModel model, final IStatus status )
    {
      // REMARK: copy current listeners into array to avoid ConcurrentModificationException
      final IScenarioDataListener[] listeners = m_controller.toArray( new IScenarioDataListener[m_controller.size()] );
      for( final IScenarioDataListener listener : listeners )
        listener.modelLoaded( model, status );
    }
  }

  /**
   * Maps the (adapted) feature-wrapper-classes onto the corresponding pool key.
   * <p>
   * At the moment this works, because each gml-file corresponds to exactly one (different) wraper class.
   */
  protected final Map<Class< ? extends IModel>, KeyPoolListener> m_keyMap = new HashMap<Class< ? extends IModel>, KeyPoolListener>();

  private final List<IScenarioDataListener> m_controller = new ArrayList<IScenarioDataListener>();

  private IContainer m_scenarioFolder = null;

  private String m_dataSetScope;

  /**
   * Returns the current data set scope
   */
  public String getDataSetScope( )
  {
    return m_dataSetScope;
  }

  public void addScenarioDataListener( final IScenarioDataListener listener )
  {
    m_controller.add( listener );
  }

  public void removeScenarioDataListener( final IScenarioDataListener listener )
  {
    m_controller.remove( listener );
  }

  /**
   * @see de.renew.workflow.connector.cases.ICaseDataProvider#setCurrent(org.eclipse.core.resources.IContainer)
   */
  public void setCurrent( final IContainer scenarioFolder )
  {
    /* Nothing to do if scenario folder stays the same */
    if( ObjectUtils.equals( m_scenarioFolder, scenarioFolder ) )
      return;

    /* Release current models && reset state */
    reset();

    if( scenarioFolder != null )
    {
      final IProject project = scenarioFolder.getProject();
      final ProjectScope projectScope = new ProjectScope( project );
      final IEclipsePreferences afguiNode = projectScope.getNode( "org.kalypso.afgui" );
      m_dataSetScope = afguiNode == null ? null : afguiNode.get( "dataSetScope", null );
    }
    m_scenarioFolder = scenarioFolder;

    fireScenarioDataFolderChanged( scenarioFolder );

    if( m_scenarioFolder == null || m_dataSetScope == null )
      return;

    final String dataSetScope = m_dataSetScope;
    final Job job = new Job( "Initalisiere Daten für das aktuelle Szenario." )
    {
      /**
       * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
       */
      @Override
      protected IStatus run( IProgressMonitor monitor )
      {
        try
        {
          // TODO: do not do this! If something is out of sync, thats a bug!
          scenarioFolder.refreshLocal( IResource.DEPTH_INFINITE, new NullProgressMonitor() );
        }
        catch( final Throwable th )
        {
          th.printStackTrace();
        }

        final List<IStatus> statusList = new ArrayList<IStatus>();
        final Map<String, IScenarioDatum> locationMap = ScenarioDataExtension.getScenarioDataMap( dataSetScope );
        if( locationMap != null )
        {
          synchronized( m_keyMap )
          {
            for( final IScenarioDatum entry : locationMap.values() )
            {
              Class< ? extends IModel> wrapperClass;
              try
              {
                wrapperClass = entry.getModelClass();
                final String gmlLocation = entry.getModelPath();
                resetKeyForProject( (IFolder) scenarioFolder, wrapperClass, gmlLocation );
              }
              catch( final CoreException e )
              {
                statusList.add( e.getStatus() );
              }
            }
          }
        }
        return StatusUtilities.createStatus( statusList, "Beim Initialisieren der Szenariodaten sind Probleme aufgetreten." );
      }
    };

    job.setRule( scenarioFolder.getProject() );
    job.schedule();
  }

  /**
   * Releases all models and clears all cached data.
   */
  private void reset( )
  {
    m_scenarioFolder = null;
    m_dataSetScope = null;

    KeyPoolListener[] keys;
    synchronized( m_keyMap )
    {
      final Collection<KeyPoolListener> values = m_keyMap.values();
      keys = values.toArray( new KeyPoolListener[values.size()] );
      m_keyMap.clear();
    }

    final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();
    for( final KeyPoolListener key : keys )
    {
      if( key != null )
      {
        pool.removePoolListener( key );
      }
    }
  }

  private void fireScenarioDataFolderChanged( final IContainer szenarioFolder )
  {
    for( final IScenarioDataListener listener : m_controller )
    {
      listener.scenarioChanged( szenarioFolder );
    }
  }

  /**
   * TODO: probably that does not belong here, move to ScenarioHelper instead? This method will try to find a model file
   * in this scenario and its parent scenarios. Needs refactoring!
   */
  public static IFolder findModelContext( final IFolder szenarioFolder, final String modelFile )
  {
    if( szenarioFolder == null )
      return null;

    if( szenarioFolder.getFile( new Path( modelFile ) ).exists() )
      return szenarioFolder;

    final IContainer parent = szenarioFolder.getParent();
    if( parent.getType() != IResource.PROJECT )
      return findModelContext( (IFolder) parent, modelFile );

    return null;
  }

  /**
   * Reloads all models.
   * 
   * @see de.renew.workflow.connector.cases.ICaseDataProvider#reloadModel()
   */
  public void reloadModel( )
  {
    final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();

    final KeyPoolListener[] keys;
    synchronized( m_keyMap )
    {
      final Collection<KeyPoolListener> values = m_keyMap.values();
      keys = values.toArray( new KeyPoolListener[values.size()] );
    }

    for( final KeyPoolListener listener : keys )
    {
      final KeyInfo keyInfo = pool.getInfoForKey( listener.getKey() );
      keyInfo.reload();
    }
  }

  /**
   * Resets the pool-key for the given folder.
   * 
   * @param szenarioFolder
   *          If <code>null</code>, just releases the existing key.
   */
  /* protected */void resetKeyForProject( final IFolder szenarioFolder, final Class< ? extends IModel> wrapperClass, final String gmlLocation )
  {
    final IPoolableObjectType newKey = keyForLocation( szenarioFolder, gmlLocation );

    /* If nothing changed, return */
    final KeyPoolListener oldListener = m_keyMap.get( wrapperClass );
    final IPoolableObjectType oldKey = oldListener == null ? null : oldListener.getKey();
    if( ObjectUtils.equals( oldKey, newKey ) )
      return;

    final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();

    if( oldKey != null )
    {
      pool.removePoolListener( oldListener );
    }

    if( newKey == null )
    {
      m_keyMap.put( wrapperClass, null );
    }
    else
    {
      final KeyPoolListener newListener = new KeyPoolListener( newKey, m_controller, wrapperClass );
      m_keyMap.put( wrapperClass, newListener );
      pool.addPoolListener( newListener, newKey );
    }
  }

  private IPoolableObjectType keyForLocation( final IFolder szenarioFolder, final String gmlLocation )
  {
    if( szenarioFolder == null )
      return null;

    try
    {
      final URL szenarioURL = ResourceUtilities.createURL( szenarioFolder );
      return new PoolableObjectType( "gml", gmlLocation, szenarioURL );
    }
    catch( final MalformedURLException e )
    {
      // should never happen
      e.printStackTrace();
    }

    return null;
  }

  /**
   * @see de.renew.workflow.connector.cases.ICaseDataProvider#getModel(java.lang.Class) Returns the feature wrapper
   *      corresponding to the given key. The class must be one of the known classes by this data provider.
   *      <p>
   *      This method blocks until the gml is loaded, which may take some time!
   *      </p>
   *      .
   */
  @SuppressWarnings("unchecked")
  public <T extends IModel> T getModel( final Class<T> modelClass ) throws CoreException
  {
    final CommandableWorkspace workspace = getCommandableWorkSpace( modelClass );
    return (T) workspace.getRootFeature().getAdapter( modelClass );
  }

  public void postCommand( final Class< ? extends IModel> wrapperClass, final ICommand command ) throws InvocationTargetException
  {
    try
    {
      final CommandableWorkspace modelWorkspace = getCommandableWorkSpace( wrapperClass );
      modelWorkspace.postCommand( command );
    }
    catch( final Exception e )
    {
      throw new InvocationTargetException( e );
    }
  }

  /**
   * @see de.renew.workflow.connector.cases.ICaseDataProvider#isDirty()
   */
  public boolean isDirty( )
  {
    synchronized( m_keyMap )
    {
      for( final Class< ? extends IModel> modelClass : m_keyMap.keySet() )
      {
        if( isDirty( modelClass ) )
          return true;
      }
    }
    return false;
  }

  /**
   * @see de.renew.workflow.connector.cases.ICaseDataProvider#isDirty(java.lang.Class)
   */
  public boolean isDirty( final Class< ? extends IModel> modelClass )
  {
    final KeyPoolListener keyPoolListener;
    synchronized( m_keyMap )
    {
      keyPoolListener = m_keyMap.get( modelClass );
    }

    if( keyPoolListener == null )
      return false;

    final IPoolableObjectType key = keyPoolListener.getKey();
    if( key == null )
      // TODO throw (core/other) exception?
      return false;

    final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();
    final KeyInfo infoForKey = pool.getInfoForKey( key );
    if( infoForKey == null )
      // TODO throw (core/other) exception?
      return false;

    return infoForKey.isDirty();
  }

  /**
   * @see de.renew.workflow.connector.cases.ICaseDataProvider#saveModel(java.lang.Class,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  public void saveModel( final Class< ? extends IModel> modelClass, final IProgressMonitor monitor ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString( "SzenarioDataProvider.14" ) + modelClass.getSimpleName() + Messages.getString( "SzenarioDataProvider.15" ), 110 ); //$NON-NLS-1$ //$NON-NLS-2$

    final KeyPoolListener keyPoolListener;
    synchronized( m_keyMap )
    {
      keyPoolListener = m_keyMap.get( modelClass );
    }

    try
    {
      if( keyPoolListener == null )
        throw new IllegalArgumentException( "Unknown model: " + modelClass.getName() );

      final IPoolableObjectType key = keyPoolListener.getKey();
      if( key != null )
      {
        final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();
        final KeyInfo infoForKey = pool.getInfoForKey( key );
        progress.worked( 10 );
        if( infoForKey.isDirty() )
        {
          infoForKey.saveObject( progress.newChild( 100 ) );
        }
      }
    }
    catch( final LoaderException e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }
    finally
    {
      progress.done();
    }
  }

  /**
   * @see de.renew.workflow.connector.cases.ICaseDataProvider#saveModel(org.eclipse.core.runtime.IProgressMonitor)
   */
  public void saveModel( final IProgressMonitor monitor ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString( "SzenarioDataProvider.16" ), m_keyMap.size() * 100 ); //$NON-NLS-1$
    try
    {
      for( final Class< ? extends IModel> modelClass : m_keyMap.keySet() )
      {
        saveModel( modelClass, progress.newChild( 100 ) );
      }
    }
    finally
    {
      progress.done();
    }
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.ICommandPoster#getCommandableWorkSpace(java.lang.Class)
   */
  public CommandableWorkspace getCommandableWorkSpace( final Class< ? extends IModel> wrapperClass ) throws IllegalArgumentException, CoreException
  {
    final Map<String, IScenarioDatum> locationMap = ScenarioDataExtension.getScenarioDataMap( m_dataSetScope );
    final String classKey = wrapperClass.getName();

    if( locationMap == null || !locationMap.containsKey( classKey ) )
      throw new IllegalArgumentException( Messages.getString( "SzenarioDataProvider.13" ) + wrapperClass ); //$NON-NLS-1$

    final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();

    final KeyPoolListener keyPoolListener;
    synchronized( m_keyMap )
    {
      keyPoolListener = m_keyMap.get( wrapperClass );
    }

    if( keyPoolListener == null )
      return null;

    final IPoolableObjectType key = keyPoolListener.getKey();
    if( key == null )
      return null;

    return (CommandableWorkspace) pool.getObject( key );
  }

  /**
   * Returns the current scenario's base folder
   */
  public IContainer getScenarioFolder( )
  {
    return m_scenarioFolder;
  }
}
