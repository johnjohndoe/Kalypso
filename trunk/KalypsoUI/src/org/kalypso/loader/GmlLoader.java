package org.kalypso.loader;

import java.util.Properties;

import org.deegree.graphics.Layer;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.util.loader.ILoader;
import org.kalypso.util.loader.LoaderException;
import org.kalypso.util.pool.PoolableObjectType;

/**
 * Loads a single layer from a gml
 * 
 * @author bce
 */
public class GmlLoader implements ILoader
{
  /**
   * @see org.kalypso.util.loader.ILoader#load(java.util.Properties, java.lang.Object)
   */
  public Object load( final Properties source, Object helper ) throws LoaderException
  {
    try
    {
      final String name = source.getProperty( "LAYER", "" );
      
      // erst mal gml laden
      final Layer[] layers=(Layer[])KalypsoGisPlugin.getDefault().getPool( Layer[].class ).borrowObject( new PoolableObjectType("gmlarray",source, helper) );
      for( int i = 0; i < layers.length; i++ )
      {
        final Layer layer = layers[i];
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
   * @see org.kalypso.util.loader.ILoader#save(java.util.Properties, java.lang.Object)
   */
  public void save( Properties source, Object data ) throws LoaderException
  {
    // TODO: support it
    throw new LoaderException( "Operation not supported" );
  }

  /**
   * @see org.kalypso.util.loader.ILoader#getDescription()
   */
  public String getDescription()
  {
    return null;
  }

  /**
   * @see org.kalypso.util.loader.ILoader#setSource(java.util.Properties)
   */
  public void setSource( Properties source )
  {
    //
  }

  /**
   * @see org.kalypso.util.loader.ILoader#getSource()
   */
  public Properties getSource()
  {
    return null;
  }

  /**
   * @see org.kalypso.util.loader.ILoader#createControl(java.lang.Object)
   */
  public Object createControl( Object argument ) 
  {
    return null;
  }
}
