package org.kalypso.loader.impl;

import java.util.Properties;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
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
    super.dispose();
    
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
      final KalypsoFeatureLayer[] layers = getLayerArray( source, project, monitor );

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

  private KalypsoFeatureLayer[] getLayerArray( final Properties source, final IProject project, final IProgressMonitor monitor ) throws Exception
  {
    final PoolableObjectType key = new PoolableObjectType( "gmlarray", source, project );
    final KalypsoFeatureLayer[] layers = (KalypsoFeatureLayer[])m_gmlArrayPool.getObject( key,
        monitor );
    return layers;
  }

  /**
   * @see org.kalypso.loader.ILoader#getDescription()
   */
  public String getDescription()
  {
    return "GML Layer";
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

  /**
   * @see org.kalypso.loader.AbstractLoader#save(java.util.Properties, org.eclipse.core.resources.IProject, org.eclipse.core.runtime.IProgressMonitor, java.lang.Object)
   */
  public void save( final Properties source, final IProject project, final IProgressMonitor monitor, final Object data )
      throws LoaderException
  {
    try
    {
      monitor.beginTask( "Thema speichern", 2000 );
      
      // das (bereits gepoolte objekt holen)
      final KalypsoFeatureLayer[] layerArray = getLayerArray( source, project, new SubProgressMonitor( monitor, 1000 ) );
      m_gmlArrayPool.saveObject( layerArray, new SubProgressMonitor( monitor, 1000 ) );
    }
    catch( Exception e )
    {
      throw new LoaderException( e );
    }
  }
  
  
  
}