package org.kalypso.loader;

import java.util.Properties;

import org.deegree.graphics.sld.NamedLayer;
import org.deegree.graphics.sld.Style;
import org.deegree.graphics.sld.StyledLayerDescriptor;
import org.deegree.graphics.sld.UserLayer;
import org.deegree.graphics.sld.UserStyle;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.util.loader.ILoader;
import org.kalypso.util.loader.LoaderException;
import org.kalypso.util.pool.PoolableObjectType;

/**
 * Loads a single userstyle from a sld
 * 
 * @author bce
 */
public class StyleLoader implements ILoader
{
  /**
   * @see org.kalypso.util.loader.ILoader#load(java.util.Properties,
   *      java.lang.Object)
   */
  public Object load( final Properties source, Object helper ) throws LoaderException
  {
    try
    {
      final String name = source.getProperty( "STYLE", "" );

      // erst mal gml laden
      final StyledLayerDescriptor sld = (StyledLayerDescriptor)KalypsoGisPlugin.getDefault()
          .getPool( StyledLayerDescriptor.class ).borrowObject( new PoolableObjectType( "sld", source, helper ) );
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
   * @see org.kalypso.util.loader.ILoader#save(java.util.Properties,
   *      java.lang.Object)
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