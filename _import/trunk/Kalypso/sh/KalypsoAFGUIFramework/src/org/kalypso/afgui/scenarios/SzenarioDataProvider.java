/**
 *
 */
package org.kalypso.afgui.scenarios;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
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
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.commons.command.ICommand;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsosimulationmodel.core.ICommandPoster;
import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.KeyInfo;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;
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
          fireModelLoaded( model );
      }
    }

    private void fireModelLoaded( final IModel model )
    {
      for( final IScenarioDataListener listener : m_controller )
      {
        listener.modelLoaded( model );
      }
    }
  }

  /**
   * Maps the (adapted) feature-wrapper-classes onto the corresponding pool key.
   * <p>
   * At the moment this works, because each gml-file corresponds to exactly one (different) wraper class.
   */
  private final Map<Class< ? extends IModel>, KeyPoolListener> m_keyMap = new HashMap<Class< ? extends IModel>, KeyPoolListener>();

  private final List<IScenarioDataListener> m_controller = new ArrayList<IScenarioDataListener>();

  private String m_dataSetScope;

  public void addScenarioDataListener( final IScenarioDataListener listener )
  {
    m_controller.add( listener );
  }

  public void removeScenarioDataListener( final IScenarioDataListener listener )
  {
    m_controller.remove( listener );
  }

  public void setCurrent( final IContainer szenarioFolder )
  {
    if( szenarioFolder != null )
    {
      final IProject project = szenarioFolder.getProject();
      final ProjectScope projectScope = new ProjectScope( project );
      m_dataSetScope = projectScope.getNode( "org.kalypso.afgui" ).get( "dataSetScope", "" );
    }
    else
    {
      m_dataSetScope = null;
    }

    fireScenarioDataFolderChanged( szenarioFolder );

    final Job job = new Job( "" )
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
          if( szenarioFolder != null )
            szenarioFolder.refreshLocal( IFolder.DEPTH_INFINITE, new NullProgressMonitor() );
        }
        catch( final Throwable th )
        {
          th.printStackTrace();
        }

        // TODO: this causes an exception no startup!
        final Map<Class< ? extends IModel>, String> locationMap = ScenarioDataExtension.getLocationMap( m_dataSetScope );
        if( locationMap != null )
        {
          for( final Map.Entry<Class< ? extends IModel>, String> entry : locationMap.entrySet() )
          {
            final Class< ? extends IModel> wrapperClass = entry.getKey();
            final String gmlLocation = entry.getValue();

            resetKeyForProject( (IFolder) szenarioFolder, wrapperClass, gmlLocation );
          }
        }
        return Status.OK_STATUS;
      }
    };
    if( szenarioFolder != null )
      job.setRule( szenarioFolder.getProject() );
    job.schedule();
  }

  private void fireScenarioDataFolderChanged( final IContainer szenarioFolder )
  {
    for( final IScenarioDataListener listener : m_controller )
    {
      listener.scenarioChanged( (IFolder) szenarioFolder );
    }
  }

  public static IFolder findModelContext( final IFolder szenarioFolder, final String modelFile )
  {
    if( szenarioFolder == null )
      return null;

    if( szenarioFolder.getFile( new Path( modelFile ) ).exists() )
    {
      return szenarioFolder;
    }
    final IContainer parent = szenarioFolder.getParent();
    if( parent.getType() != IResource.PROJECT )
      return findModelContext( (IFolder) parent, modelFile );
    else
      return null;

  }

  public void reloadModel( )
  {
    final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();
    for( final KeyPoolListener listener : m_keyMap.values() )
    {
      final KeyInfo keyInfo = pool.getInfoForKey( listener.getKey() );
      keyInfo.reload();
    }
  }

  /**
   * Resets the pool-key for the given folder.
   * 
   * @param szenarioFolder
   *            If <code>null</code>, just releases the existing key.
   */
  protected synchronized void resetKeyForProject( final IFolder szenarioFolder, final Class< ? extends IModel> wrapperClass, final String gmlLocation )
  {
    final IPoolableObjectType newKey = keyForLocation( szenarioFolder, gmlLocation );

    /* If nothing changed, return */
    final KeyPoolListener oldListener = m_keyMap.get( wrapperClass );
    final IPoolableObjectType oldKey = oldListener == null ? null : oldListener.getKey();
    if( ObjectUtils.equals( oldKey, newKey ) )
      return;

    final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();

    if( oldKey != null )
      pool.removePoolListener( oldListener );

    if( newKey == null )
      m_keyMap.put( wrapperClass, null );
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
      // final URL context = UrlResolverSingleton.resolveUrl( szenarioURL, gmlLocation );
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
   * Returns the feature wrapper corresponding to the given key. The class must be one of the known classes by this data
   * provider.
   * <p>
   * This method block until the gml is loaded, which may take some time
   * </p>.
   */
  @SuppressWarnings("unchecked")
  public <T extends IModel> T getModel( final Class<T> modelClass ) throws CoreException
  {
    final CommandableWorkspace workspace = getModelWorkspace( modelClass );
    return (T) workspace.getRootFeature().getAdapter( modelClass );
  }

  public void postCommand( final Class< ? extends IModel> wrapperClass, final ICommand command ) throws Exception
  {
    final CommandableWorkspace modelWorkspace = getModelWorkspace( wrapperClass );
    modelWorkspace.postCommand( command );
  }

  private synchronized CommandableWorkspace getModelWorkspace( final Class< ? extends IModel> wrapperClass ) throws IllegalArgumentException, CoreException
  {
    if( !ScenarioDataExtension.getLocationMap( m_dataSetScope ).containsKey( wrapperClass ) )
      throw new IllegalArgumentException( Messages.getString( "SzenarioDataProvider.13" ) + wrapperClass ); //$NON-NLS-1$

    final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();

    final KeyPoolListener keyPoolListener = m_keyMap.get( wrapperClass );
    if( keyPoolListener == null )
      return null;

    final IPoolableObjectType key = keyPoolListener.getKey();
    if( key == null )
      return null;

    final CommandableWorkspace workspace = (CommandableWorkspace) pool.getObject( key );
    return workspace;
  }

  public boolean isDirty( )
  {
    for( final Class< ? extends IModel> modelClass : m_keyMap.keySet() )
    {
      if( isDirty( modelClass ) )
        return true;
    }
    return false;
  }

  /**
   * @see de.renew.workflow.cases.ICaseDataProvider#isDirty(java.lang.Class)
   */
  public boolean isDirty( final Class< ? extends IModel> modelClass )
  {
    final KeyPoolListener keyPoolListener = m_keyMap.get( modelClass );
    if( keyPoolListener == null )
    {
      return false;
    }
    final IPoolableObjectType key = keyPoolListener.getKey();
    if( key == null )
    {
      // TODO throw (core/other) exception?
      return false;
    }

    final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();
    final KeyInfo infoForKey = pool.getInfoForKey( key );
    if( infoForKey == null )
    {
      // TODO throw (core/other) exception?
      return false;
    }

    return infoForKey.isDirty();
  }

  /**
   * @see de.renew.workflow.cases.ICaseDataProvider#saveModel(java.lang.Class,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  public void saveModel( final Class< ? extends IModel> modelClass, IProgressMonitor monitor ) throws CoreException
  {
    if( monitor == null )
    {
      monitor = new NullProgressMonitor();
    }
    try
    {
      monitor.beginTask( Messages.getString( "SzenarioDataProvider.14" ) + modelClass.getSimpleName() + Messages.getString( "SzenarioDataProvider.15" ), 110 ); //$NON-NLS-1$ //$NON-NLS-2$

      final KeyPoolListener keyPoolListener = m_keyMap.get( modelClass );
      if( keyPoolListener == null )
        throw new IllegalArgumentException( "Unknown model: " + modelClass.getName() );

      final IPoolableObjectType key = keyPoolListener.getKey();
      if( key != null )
      {
        final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();
        final KeyInfo infoForKey = pool.getInfoForKey( key );
        monitor.worked( 10 );
        if( infoForKey.isDirty() )
        {
          infoForKey.saveObject( new SubProgressMonitor( monitor, 100 ) );
        }
      }
    }
    catch( final LoaderException e )
    {
      monitor.done();
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }
    finally
    {
      monitor.done();
    }
  }

  public void saveModel( IProgressMonitor monitor ) throws CoreException
  {
    if( monitor == null )
    {
      monitor = new NullProgressMonitor();
    }
    try
    {
      monitor.beginTask( Messages.getString( "SzenarioDataProvider.16" ), m_keyMap.size() * 100 ); //$NON-NLS-1$
      for( final Class< ? extends IModel> modelClass : m_keyMap.keySet() )
      {
        saveModel( modelClass, new SubProgressMonitor( monitor, 100 ) );
      }
    }
    finally
    {
      monitor.done();
    }
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.ICommandPoster#getCommandableWorkSpace(java.lang.Class)
   */
  public CommandableWorkspace getCommandableWorkSpace( final Class< ? extends IModel> wrapperClass ) throws IllegalArgumentException, CoreException
  {
    return getModelWorkspace( wrapperClass );
  }
}
