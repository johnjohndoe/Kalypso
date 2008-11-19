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
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.util.pool.IPoolListener;
import org.kalypso.core.util.pool.IPoolableObjectType;
import org.kalypso.core.util.pool.KeyInfo;
import org.kalypso.core.util.pool.PoolableObjectType;
import org.kalypso.core.util.pool.ResourcePool;
import org.kalypso.kalypsosimulationmodel.core.ICommandPoster;
import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
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

    private final Class< ? extends IModel> m_modelClass;

    public KeyPoolListener( final IPoolableObjectType key, final List<IScenarioDataListener> controller, final Class< ? extends IModel> modelClass )
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

        final IModel model = adaptModel( m_modelClass, workspace );
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
   * At the moment this works, because each gml-file corresponds to exactly one (different) wrapper class.
   */
  protected final Map<String, KeyPoolListener> m_keyMap = new HashMap<String, KeyPoolListener>();

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
      protected IStatus run( final IProgressMonitor monitor )
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
              try
              {
                final String id = entry.getID();
                final Class< ? extends IModel> wrapperClass = entry.getModelClass();
                final String gmlLocation = entry.getModelPath();
                resetKeyForProject( (IFolder) scenarioFolder, id, wrapperClass, gmlLocation );
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

    final ResourcePool pool = KalypsoCorePlugin.getDefault().getPool();
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
    final ResourcePool pool = KalypsoCorePlugin.getDefault().getPool();

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
  /* protected */void resetKeyForProject( final IFolder szenarioFolder, final String id, final Class< ? extends IModel> wrapperClass, final String gmlLocation )
  {
    final IPoolableObjectType newKey = keyForLocation( szenarioFolder, gmlLocation );

    /* If nothing changed, return */
    final KeyPoolListener oldListener = m_keyMap.get( wrapperClass );
    final IPoolableObjectType oldKey = oldListener == null ? null : oldListener.getKey();
    if( ObjectUtils.equals( oldKey, newKey ) )
      return;

    final ResourcePool pool = KalypsoCorePlugin.getDefault().getPool();

    if( oldKey != null )
    {
      pool.removePoolListener( oldListener );
    }

    if( newKey == null )
    {
      m_keyMap.put( id, null );
    }
    else
    {
      final KeyPoolListener newListener = new KeyPoolListener( newKey, m_controller, wrapperClass );
      m_keyMap.put( id, newListener );
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
   */
  @SuppressWarnings("deprecation")
  @Deprecated
  public <T extends IModel> T getModel( final Class<T> modelClass ) throws CoreException
  {
    final CommandableWorkspace workspace = getCommandableWorkSpace( modelClass.getName() );
    return adaptModel( modelClass, workspace );
  }

  /**
   * @see de.renew.workflow.connector.cases.ICaseDataProvider#getModel(java.lang.String)
   */
  public <D extends IModel> D getModel( final String id, final Class<D> modelClass ) throws CoreException
  {
    final CommandableWorkspace workspace = getCommandableWorkSpace( id );
    return adaptModel( modelClass, workspace );
  }

  @SuppressWarnings("unchecked")
  protected static <T> T adaptModel( final Class<T> modelClass, final GMLWorkspace workspace )
  {
    final Feature rootFeature = workspace.getRootFeature();
    if( modelClass.isAssignableFrom( rootFeature.getClass() ) )
      return (T) rootFeature;

    return (T) rootFeature.getAdapter( modelClass );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.ICommandPoster#postCommand(java.lang.Class,
   *      org.kalypso.commons.command.ICommand)
   */
  @SuppressWarnings("deprecation")
  @Deprecated
  public void postCommand( final Class< ? extends IModel> wrapperClass, final ICommand command ) throws InvocationTargetException
  {
    postCommand( wrapperClass.getName(), command );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.ICommandPoster#postCommand(java.lang.String,
   *      org.kalypso.commons.command.ICommand)
   */
  public void postCommand( final String id, final ICommand command ) throws InvocationTargetException
  {
    try
    {
      final CommandableWorkspace modelWorkspace = getCommandableWorkSpace( id );
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
      for( final String modelClass : m_keyMap.keySet() )
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
  @SuppressWarnings("deprecation")
  @Deprecated
  public boolean isDirty( final Class< ? extends IModel> modelClass )
  {
    return isDirty( modelClass.getName() );
  }

  /**
   * @see de.renew.workflow.connector.cases.ICaseDataProvider#isDirty(java.lang.String)
   */
  @Override
  public boolean isDirty( final String id )
  {
    final KeyPoolListener keyPoolListener;
    synchronized( m_keyMap )
    {
      keyPoolListener = m_keyMap.get( id );
    }

    if( keyPoolListener == null )
      return false;

    final IPoolableObjectType key = keyPoolListener.getKey();
    if( key == null )
      // TODO throw (core/other) exception?
      return false;

    final ResourcePool pool = KalypsoCorePlugin.getDefault().getPool();
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
  @SuppressWarnings("deprecation")
  @Deprecated
  public void saveModel( final Class< ? extends IModel> modelClass, final IProgressMonitor monitor ) throws CoreException
  {
    saveModel( modelClass.getName(), monitor );
  }

  /**
   * @see de.renew.workflow.connector.cases.ICaseDataProvider#saveModel(java.lang.String,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  public void saveModel( final String id, final IProgressMonitor monitor ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString( "SzenarioDataProvider.14" ) + id + Messages.getString( "SzenarioDataProvider.15" ), 110 ); //$NON-NLS-1$ //$NON-NLS-2$

    final KeyPoolListener keyPoolListener;
    synchronized( m_keyMap )
    {
      keyPoolListener = m_keyMap.get( id );
    }

    try
    {
      if( keyPoolListener == null )
        throw new IllegalArgumentException( "Unknown model: " + id );

      final IPoolableObjectType key = keyPoolListener.getKey();
      if( key != null )
      {
        final ResourcePool pool = KalypsoCorePlugin.getDefault().getPool();
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
      for( final String id : m_keyMap.keySet() )
        saveModel( id, progress.newChild( 100 ) );
    }
    finally
    {
      progress.done();
    }
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.ICommandPoster#getCommandableWorkSpace(java.lang.Class)
   */
  @Override
  @Deprecated
  public CommandableWorkspace getCommandableWorkSpace( final Class< ? extends IModel> wrapperClass ) throws IllegalArgumentException, CoreException
  {
    return getCommandableWorkSpace( wrapperClass.getName() );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.ICommandPoster#getCommandableWorkSpace(java.lang.Class)
   */
  public CommandableWorkspace getCommandableWorkSpace( final String id ) throws IllegalArgumentException, CoreException
  {
    final Map<String, IScenarioDatum> locationMap = ScenarioDataExtension.getScenarioDataMap( m_dataSetScope );

    if( locationMap == null || !locationMap.containsKey( id ) )
      throw new IllegalArgumentException( Messages.getString( "SzenarioDataProvider.13" ) + id ); //$NON-NLS-1$

    final ResourcePool pool = KalypsoCorePlugin.getDefault().getPool();

    final KeyPoolListener keyPoolListener;
    synchronized( m_keyMap )
    {
      keyPoolListener = m_keyMap.get( id );
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
