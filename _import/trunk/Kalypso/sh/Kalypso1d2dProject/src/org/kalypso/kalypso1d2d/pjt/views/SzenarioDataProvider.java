/**
 *
 */
package org.kalypso.kalypso1d2d.pjt.views;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang.ObjectUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
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
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModelGroup;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IPseudoOPerationalModel;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IStaticModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsosimulationmodel.core.ICommandPoster;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.KeyInfo;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

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
public class SzenarioDataProvider implements ICaseDataProvider<IFeatureWrapper2>, ICommandPoster
{
  /**
   * Maps the (adapted) feature-wrapper-classes onto the (szenario-relative) path of its gml-file.
   * <p>
   * At the moment this works, because each gml-file corresponds to exactly one (different) wraper class.
   */
  private static final Map<Class< ? extends IFeatureWrapper2>, String> LOCATION_MAP = new HashMap<Class< ? extends IFeatureWrapper2>, String>();

  private static final String MODELS_FOLDER = "models";  //$NON-NLS-1$

  static
  {
    LOCATION_MAP.put( IFEDiscretisationModel1d2d.class, MODELS_FOLDER + "/discretisation.gml" );
    LOCATION_MAP.put( ITerrainModel.class, MODELS_FOLDER + "/terrain.gml" );
    LOCATION_MAP.put( IFlowRelationshipModel.class, MODELS_FOLDER + "/flowrelations.gml" );
    LOCATION_MAP.put( IPseudoOPerationalModel.class, MODELS_FOLDER + "/operational.gml" );
    LOCATION_MAP.put( IControlModelGroup.class, MODELS_FOLDER + "/control.gml" );
    LOCATION_MAP.put( IStaticModel1D2D.class, MODELS_FOLDER + "/static_model.gml" );
    LOCATION_MAP.put( IRoughnessClsCollection.class, "project:/.metadata/roughness.gml" );
    LOCATION_MAP.put( IScenarioResultMeta.class, MODELS_FOLDER + "/scenarioResultMeta.gml" );
  }

  private static final class KeyPoolListener implements IPoolListener
  {
    private final IPoolableObjectType m_key;

    private final SzenarioController m_controller;

    public KeyPoolListener( final IPoolableObjectType key, final SzenarioController controller )
    {
      m_key = key;
      m_controller = controller;
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
      // TODO Auto-generated method stub

    }

    /**
     * @see org.kalypso.util.pool.IPoolListener#isDisposed()
     */
    public boolean isDisposed( )
    {
      // TODO Auto-generated method stub
      return false;
    }

    /**
     * @see org.kalypso.util.pool.IPoolListener#objectInvalid(org.kalypso.util.pool.IPoolableObjectType,
     *      java.lang.Object)
     */
    public void objectInvalid( final IPoolableObjectType key, final Object oldValue )
    {
      // TODO Auto-generated method stub

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
        final IModel model = (IModel) workspace.getRootFeature().getAdapter( IModel.class );
        if( model != null )
          m_controller.modelLoaded( model );
      }
    }
  }

  /**
   * Maps the (adapted) feature-wrapper-classes onto the corresponding pool key.
   * <p>
   * At the moment this works, because each gml-file corresponds to exactly one (different) wraper class.
   */
  private final Map<Class< ? extends IFeatureWrapper2>, KeyPoolListener> m_keyMap = new HashMap<Class< ? extends IFeatureWrapper2>, KeyPoolListener>();

  private final SzenarioController m_controller = new SzenarioController();

  public void setCurrent( final IContainer szenarioFolder )
  {
    m_controller.szenarioChanged( (IFolder)szenarioFolder );

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

        for( final Map.Entry<Class< ? extends IFeatureWrapper2>, String> entry : LOCATION_MAP.entrySet() )
        {
          final Class< ? extends IFeatureWrapper2> wrapperClass = entry.getKey();
          final String gmlLocation = entry.getValue();

          resetKeyForProject( (IFolder) szenarioFolder, wrapperClass, gmlLocation );
        }
        return Status.OK_STATUS;
      }
    };
    if( szenarioFolder != null )
      job.setRule( szenarioFolder.getProject() );
    job.schedule();
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
      if( keyInfo.isDirty() )
      {
        try
        {
          keyInfo.onLoaderObjectInvalid( keyInfo.getObject(), false );
          keyInfo.setDirty( false );
        }
        catch( final Exception e )
        {
          e.printStackTrace();
        }
      }
    }
  }

  /**
   * Resets the pool-key for the given folder.
   * 
   * @param szenarioFolder
   *            If <code>null</code>, just releases the existing key.
   */
  protected synchronized void resetKeyForProject( final IFolder szenarioFolder, final Class< ? extends IFeatureWrapper2> wrapperClass, final String gmlLocation )
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
      final KeyPoolListener newListener = new KeyPoolListener( newKey, m_controller );
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
  @SuppressWarnings("unchecked")  //$NON-NLS-1$
  public <T extends IFeatureWrapper2> T getModel( final Class<T> modelClass ) throws CoreException
  {
    final CommandableWorkspace workspace = getModelWorkspace( modelClass );
    return (T) workspace.getRootFeature().getAdapter( modelClass );
  }

  public void postCommand( final Class< ? extends IFeatureWrapper2> wrapperClass, final ICommand command ) throws Exception
  {
    final CommandableWorkspace modelWorkspace = getModelWorkspace( wrapperClass );
    modelWorkspace.postCommand( command );
  }

  private CommandableWorkspace getModelWorkspace( final Class< ? extends IFeatureWrapper2> wrapperClass ) throws IllegalArgumentException, CoreException
  {
    if( !LOCATION_MAP.containsKey( wrapperClass ) )
      throw new IllegalArgumentException( Messages.getString("SzenarioDataProvider.13") + wrapperClass ); //$NON-NLS-1$

    final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();

    final KeyPoolListener keyPoolListener = m_keyMap.get( wrapperClass );
    final IPoolableObjectType key = keyPoolListener.getKey();
    if( key == null )
      return null;

    final CommandableWorkspace workspace = (CommandableWorkspace) pool.getObject( key );
    return workspace;
  }

  public boolean isDirty( )
  {
    for( final Class< ? extends IFeatureWrapper2> modelClass : m_keyMap.keySet() )
    {
      if( isDirty( modelClass ) )
        return true;
    }
    return false;
  }

  /**
   * @see de.renew.workflow.cases.ICaseDataProvider#isDirty(java.lang.Class)
   */
  public boolean isDirty( final Class< ? extends IFeatureWrapper2> modelClass )
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
  public void saveModel( final Class< ? extends IFeatureWrapper2> modelClass, IProgressMonitor monitor ) throws CoreException
  {
    if( monitor == null )
    {
      monitor = new NullProgressMonitor();
    }
    try
    {
      monitor.beginTask( Messages.getString("SzenarioDataProvider.14") + modelClass.getSimpleName() + Messages.getString("SzenarioDataProvider.15"), 110 ); //$NON-NLS-1$ //$NON-NLS-2$

      final KeyPoolListener keyPoolListener = m_keyMap.get( modelClass );
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
      monitor.beginTask( Messages.getString("SzenarioDataProvider.16"), m_keyMap.size() * 100 ); //$NON-NLS-1$
      for( final Class< ? extends IFeatureWrapper2> modelClass : m_keyMap.keySet() )
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
  public CommandableWorkspace getCommandableWorkSpace( final Class< ? extends IFeatureWrapper2> wrapperClass ) throws IllegalArgumentException, CoreException
  {
    return getModelWorkspace( wrapperClass );
  }
}
