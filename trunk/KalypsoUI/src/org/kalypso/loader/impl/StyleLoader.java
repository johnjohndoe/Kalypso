package org.kalypso.loader.impl;

import java.util.Properties;

import org.deegree.graphics.sld.NamedLayer;
import org.deegree.graphics.sld.Style;
import org.deegree.graphics.sld.StyledLayerDescriptor;
import org.deegree.graphics.sld.UserLayer;
import org.deegree.graphics.sld.UserStyle;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.loader.AbstractLoader;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;

/**
 * Loads a single userstyle from a sld
 * 
 * @author bce
 */
public class StyleLoader extends AbstractLoader implements IPoolListener
{
  private final ResourcePool m_sldPool = KalypsoGisPlugin.getDefault().getPool(
      StyledLayerDescriptor.class );

  public StyleLoader()
  {
    m_sldPool.addPoolListener( this );
  }

  /**
   * @see org.kalypso.loader.AbstractLoader#dispose()
   */
  public void dispose()
  {
    super.dispose();

    m_sldPool.removePoolListener( this );
  }

  /**
   * @see org.kalypso.loader.AbstractLoader#loadIntern(java.util.Properties,
   *      org.eclipse.core.resources.IProject,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  protected final Object loadIntern( final Properties source, final IProject project,
      final IProgressMonitor monitor ) throws LoaderException
  {
    try
    {
      final String name = source.getProperty( "STYLE", "" );

      final StyledLayerDescriptor sld = (StyledLayerDescriptor)m_sldPool.getObject(
          new PoolableObjectType( "sld", source, project ), monitor );
      // TODO: move to StyleLoader

      final NamedLayer[] namedLayers = sld.getNamedLayers();
      for( int i = 0; i < namedLayers.length; i++ )
      {
        final Style[] styles = namedLayers[i].getStyles();
        for( int n = 0; n < styles.length; n++ )
        {
          final Style style = styles[n];
          if( style instanceof UserStyle && name.equals( style.getName() ) )
            return new KalypsoUserStyle( (UserStyle)style );
        }
      }

      final UserLayer[] userLayers = sld.getUserLayers();
      for( int i = 0; i < userLayers.length; i++ )
      {
        final Style[] styles = userLayers[i].getStyles();
        for( int n = 0; n < styles.length; n++ )
        {
          final Style style = styles[n];
          if( style instanceof UserStyle && name.equals( style.getName() ) )
            return new KalypsoUserStyle( (UserStyle)style );
        }
      }

      throw new LoaderException( "Style not found: " + source );
    }

    catch( final Exception e )
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