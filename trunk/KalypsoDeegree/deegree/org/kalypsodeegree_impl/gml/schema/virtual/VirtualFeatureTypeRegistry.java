package org.kalypsodeegree_impl.gml.schema.virtual;

import java.util.ArrayList;
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
      // common / na-modell: generate relations as geometry
      m_instance.register( new VirtualAssociationFeatureTypePropertyHandler() );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

  }

  private VirtualFeatureTypeRegistry( )
  {
    // singelton constructor
  }

  public void register( final VirtualFeatureTypePropertyHandler property )
  {
    m_register.add( property );
  }

  public void unregister( final VirtualFeatureTypePropertyHandler property )
  {
    m_register.remove( property );
  }

  public VirtualFeatureTypeProperty[] getVirtualFeatureTypePropertiesFor( final IFeatureType ft )
  {
    final List<VirtualFeatureTypeProperty> result = new ArrayList<VirtualFeatureTypeProperty>();
    for( final Object element : m_register )
    {
      final VirtualFeatureTypePropertyHandler vftp = (VirtualFeatureTypePropertyHandler) element;
      if( vftp.isDekoratorOf( ft ) )
      {
        final VirtualFeatureTypeProperty[] properties = vftp.createVirtualFeatureTypeProperties( ft );
        for( final VirtualFeatureTypeProperty element2 : properties )
          result.add( element2 );
      }
    }
    return result.toArray( new VirtualFeatureTypeProperty[result.size()] );
  }

  public VirtualFeatureTypeProperty[] getVirtualFeatureTypePropertiesFor( final IPropertyType ftp )
  {
    final List<VirtualFeatureTypeProperty> result = new ArrayList<VirtualFeatureTypeProperty>();
    for( final Object element : m_register )
    {
      final VirtualFeatureTypePropertyHandler vftp = (VirtualFeatureTypePropertyHandler) element;
      if( vftp.isDekoratorOf( ftp ) )
      {
        final VirtualFeatureTypeProperty[] properties = vftp.createVirtualFeatureTypeProperties( ftp );
        for( final VirtualFeatureTypeProperty element2 : properties )
        {
          result.add( element2 );
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