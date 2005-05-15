package org.kalypsodeegree_impl.gml.schema.virtual;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;

/**
 * virtual featuretypes are not defined in schema
 * 
 * @author doemming
 */
public class VirtualFeatureTypeRegistry
{
  private final static VirtualFeatureTypeRegistry m_instance;

  private final List m_register = new ArrayList();

  // load default types
  static
  {
    m_instance = new VirtualFeatureTypeRegistry();
    try
    {
      // na-modell:
      m_instance.register( new VirtualAssociationFeatureTypePropertyHandler() );
      // risiko:
      m_instance.register( new VirtualRasterFeatureTypePropertyHandler() );
      // 2d:
      m_instance.register( new VirtualVelocityFeatureTypePropertyHandler() );
      m_instance.register( new VirtualIsoFeatureTypePropertyHandler() );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

  }

  private VirtualFeatureTypeRegistry()
  {
  // singelton constructor
  }

  public void register( VirtualFeatureTypePropertyHandler property )
  {
    m_register.add( property );
  }

  public void unregister( VirtualFeatureTypePropertyHandler property )
  {
    m_register.remove( property );
  }

  public FeatureTypeProperty[] getVirtualFeatureTypePropertiesFor( final FeatureType ft )
  {
    final List result = new ArrayList();
    for( Iterator iter = m_register.iterator(); iter.hasNext(); )
    {
      final VirtualFeatureTypePropertyHandler vftp = (VirtualFeatureTypePropertyHandler)iter.next();
      if( vftp.isDekoratorOf( ft ) )
      {
        final FeatureTypeProperty[] properties = vftp.createVirtualFeatureTypeProperties( ft );
        for( int i = 0; i < properties.length; i++ )
        {
          result.add( properties[i] );
        }
      }
    }
    return (FeatureTypeProperty[])result.toArray( new FeatureTypeProperty[result.size()] );
  }

  public FeatureTypeProperty[] getVirtualFeatureTypePropertiesFor( final FeatureTypeProperty ftp )
  {
    final List result = new ArrayList();
    for( Iterator iter = m_register.iterator(); iter.hasNext(); )
    {
      final VirtualFeatureTypePropertyHandler vftp = (VirtualFeatureTypePropertyHandler)iter.next();
      if( vftp.isDekoratorOf( ftp ) )
      {
        final FeatureTypeProperty[] properties = vftp.createVirtualFeatureTypeProperties( ftp );
        for( int i = 0; i < properties.length; i++ )
        {
          result.add( properties[i] );
        }
      }
    }
    return (FeatureTypeProperty[])result.toArray( new FeatureTypeProperty[result.size()] );
  }

  public static VirtualFeatureTypeRegistry getInstance()
  {
    return m_instance;
  }
}