package org.kalypso.loader.impl;

import java.util.Properties;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.loader.AbstractLoader;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;

/**
 * Loads a single layer from a gml
 * 
 * @author bce
 */
public class GmlLoader extends AbstractLoader implements IPoolListener
{
  private final ResourcePool m_gmlArrayPool = KalypsoGisPlugin.getDefault().getPool(
      KalypsoFeatureLayer[].class );

  public GmlLoader()
  {
    m_gmlArrayPool.addPoolListener( this );
  }

  public void dispose()
  {
    m_gmlArrayPool.removePoolListener( this );
  }

  /**
   * @see org.kalypso.loader.AbstractLoader#loadIntern(java.util.Properties, org.eclipse.core.resources.IProject, org.eclipse.core.runtime.IProgressMonitor)
   */
  public Object loadIntern( final Properties source, final IProject project,
      final IProgressMonitor monitor ) throws LoaderException
  {
    try
    {
      final String name = source.getProperty( "LAYER", "" );

      // erst mal gml laden
      final PoolableObjectType key = new PoolableObjectType( "gmlarray", source, project );
      final KalypsoFeatureLayer[] layers = (KalypsoFeatureLayer[])m_gmlArrayPool.getObject( key,
          monitor );

      for( int i = 0; i < layers.length; i++ )
      {
        final KalypsoFeatureLayer layer = layers[i];
        if( name.equals( layer.getName() ) )
          return layer;
      }

      throw new LoaderException( "Layer not found: " + source );
    }

    catch( Exception e )
    {
      e.printStackTrace();
      throw new LoaderException( e );
    }
  }

  /**
   * @see org.kalypso.loader.ILoader#save(java.util.Properties,
   *      java.lang.Object)
   */
  public void save( Properties source, Object data ) throws LoaderException
  {
    // TODO: support it
    throw new LoaderException( "Operation not supported" );
  }

  /**
   * @see org.kalypso.loader.ILoader#getDescription()
   */
  public String getDescription()
  {
    return null;
  }

  public void onObjectInvalid( final Object oldValue, final boolean bCannotReload )
      throws Exception
  {
    final KalypsoFeatureLayer[] layers = (KalypsoFeatureLayer[])oldValue;
    for( int i = 0; i < layers.length; i++ )
    {
      if( hasObject( layers[i] ) )
      {
        release( layers[i] );
        fireLoaderObjectInvalid( layers[i], bCannotReload );
      }
    }
  }
}