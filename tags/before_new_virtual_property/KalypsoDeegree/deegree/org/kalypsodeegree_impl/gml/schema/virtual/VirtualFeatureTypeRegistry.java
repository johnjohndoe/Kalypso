package org.kalypsodeegree_impl.gml.schema.virtual;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;

/**
 * virtual featuretypes are not defined in schema
 * 
 * @author doemming
 */
public class VirtualFeatureTypeRegistry
{
  private final static VirtualFeatureTypeRegistry m_instance;

  private final List<VirtualFeatureTypePropertyHandler> m_register = new ArrayList<VirtualFeatureTypePropertyHandler>();

  // load default types
  static
  {
    m_instance = new VirtualFeatureTypeRegistry();
    try
    {
      // common: show ids
//      m_instance.register( new VirtualIdFeatureTypePropertyHandler() );
      // common / na-modell: generate relations as geometry
      m_instance.register( new VirtualAssociationFeatureTypePropertyHandler() );
      // 2d: generate velocity as arrow
//      m_instance.register( new VirtualVelocityFeatureTypePropertyHandler() );
//      m_instance.register( new VirtualIsoFeatureTypePropertyHandler() );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

  }

  private VirtualFeatureTypeRegistry( )
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

  public VirtualFeatureTypeProperty[] getVirtualFeatureTypePropertiesFor( final IFeatureType ft )
  {
    final List<VirtualFeatureTypeProperty> result = new ArrayList<VirtualFeatureTypeProperty>();
    for( Iterator iter = m_register.iterator(); iter.hasNext(); )
    {
      final VirtualFeatureTypePropertyHandler vftp = (VirtualFeatureTypePropertyHandler) iter.next();
      if( vftp.isDekoratorOf( ft ) )
      {
        final VirtualFeatureTypeProperty[] properties = vftp.createVirtualFeatureTypeProperties( ft );
        for( int i = 0; i < properties.length; i++ )
          result.add( properties[i] );
      }
    }
    return result.toArray( new VirtualFeatureTypeProperty[result.size()] );
  }

  public VirtualFeatureTypeProperty[] getVirtualFeatureTypePropertiesFor( final IPropertyType ftp )
  {
    final List<VirtualFeatureTypeProperty> result = new ArrayList<VirtualFeatureTypeProperty>();
    for( Iterator iter = m_register.iterator(); iter.hasNext(); )
    {
      final VirtualFeatureTypePropertyHandler vftp = (VirtualFeatureTypePropertyHandler) iter.next();
      if( vftp.isDekoratorOf( ftp ) )
      {
        final VirtualFeatureTypeProperty[] properties = vftp.createVirtualFeatureTypeProperties( ftp );
        for( int i = 0; i < properties.length; i++ )
        {
          result.add( properties[i] );
        }
      }
    }
    return result.toArray( new VirtualFeatureTypeProperty[result.size()] );
  }

  public static VirtualFeatureTypeRegistry getInstance( )
  {
    return m_instance;
  }
}