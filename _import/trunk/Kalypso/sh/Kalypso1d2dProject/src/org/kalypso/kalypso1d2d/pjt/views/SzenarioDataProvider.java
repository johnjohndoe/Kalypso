/**
 * 
 */
package org.kalypso.kalypso1d2d.pjt.views;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang.ObjectUtils;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.afgui.model.IWorkflowData;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper;

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
public class SzenarioDataProvider implements IPoolListener, ISzenarioDataProvider
{
  /**
   * Maps the (adapted) feature-wrapper-classes onto the (szenario-relative) path of its gml-file.
   * <p>
   * At the moment this works, because each gml-file corresponds to exactly one (different) wraper class.
   */
  private static Map<Class, String> LOCATION_MAP = new HashMap<Class, String>();

  static
  {
    // TODO: at the moment, IFeatureWrapper.class is the placeholder for the simulation-model; needs to bee changed when
    // simulation model gets its own wrapper.
    LOCATION_MAP.put( IFeatureWrapper.class, "simulation.gml" );
    LOCATION_MAP.put( IFEDiscretisationModel1d2d.class, "discretisation.gml" );
    LOCATION_MAP.put( ITerrainModel.class, "terrain.gml" );
    // TODO: add other model types here
  }

  /**
   * Maps the (adapted) feature-wrapper-classes onto the corresponding pool key.
   * <p>
   * At the moment this works, because each gml-file corresponds to exactly one (different) wraper class.
   */
  private Map<Class, IPoolableObjectType> m_keyMap = new HashMap<Class, IPoolableObjectType>();

  private static final String MODELS_FOLDER = "models";

  public synchronized void setCurrent( final IProject project, @SuppressWarnings("unused")
  final IWorkflowData data )
  {
    final IFolder szenarioFolder = project == null ? null : project.getFolder( "szenario" );

    for( final Map.Entry<Class, String> entry : LOCATION_MAP.entrySet() )
    {
      final Class wrapperClass = entry.getKey();
      final String gmlLocation = entry.getValue();

      resetKeyForProject( szenarioFolder, wrapperClass, gmlLocation );
    }
  }

  /**
   * Resets the pool-key for the given folder.
   * 
   * @param szenarioFolder
   *          If <code>null</code>, just releases the existing key.
   */
  private synchronized void resetKeyForProject( final IFolder szenarioFolder, final Class wrapperClass, final String gmlLocation )
  {
    IPoolableObjectType newKey = null;

    if( szenarioFolder != null )
    {
      try
      {
        // TODO: change context later to folder of selected szenario/simulation
        final IFolder folder = szenarioFolder.getFolder( MODELS_FOLDER );
        final URL context = ResourceUtilities.createURL( folder );

        newKey = new PoolableObjectType( "gml", gmlLocation, context );
      }
      catch( final MalformedURLException e )
      {
        // should never happen
        e.printStackTrace();
      }
    }

    /* If nothing changed, return */
    final IPoolableObjectType oldKey = m_keyMap.get( wrapperClass );
    if( ObjectUtils.equals( oldKey, newKey ) )
      return;

    final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();

    if( oldKey != null )
      pool.removePoolListener( this );

    m_keyMap.put( wrapperClass, newKey );

    if( newKey != null )
      pool.addPoolListener( this, newKey );
  }

  /**
   * Returns the feature wrapper corresponding to the given key. The class must be one of the known classes by this data
   * provider.
   * <p>
   * This method block until the gml is loaded, which may take some time
   * </p>.
   */
  public IFeatureWrapper getModel( final Class wrapperClass ) throws CoreException
  {
    if( !LOCATION_MAP.containsKey( wrapperClass ) )
      throw new IllegalArgumentException( "No gml-file defined for wrapper class: " + wrapperClass );

    final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();

    final IPoolableObjectType key = m_keyMap.get( wrapperClass );
    if( key == null )
      return null;

    final GMLWorkspace workspace = (GMLWorkspace) pool.getObject( key );
    return (IFeatureWrapper) workspace.getRootFeature().getAdapter( wrapperClass );
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#dirtyChanged(org.kalypso.util.pool.IPoolableObjectType, boolean)
   */
  public void dirtyChanged( final IPoolableObjectType key, final boolean isDirty )
  {
    // i dont mind
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#isDisposed()
   */
  public boolean isDisposed( )
  {
    // we are never disposed?
    return false;
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectInvalid(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object)
   */
  public void objectInvalid( final IPoolableObjectType key, final Object oldValue )
  {
    // ignore for now
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectLoaded(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object,
   *      org.eclipse.core.runtime.IStatus)
   */
  public void objectLoaded( final IPoolableObjectType key, final Object newValue, final IStatus status )
  {
    // we ignore it, if we access the object, we class pool.getObject() instead
  }
}
