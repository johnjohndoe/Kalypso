package org.kalypso.ogc.sensor;

import java.util.Arrays;

import org.eclipse.ui.views.properties.IPropertyDescriptor;
import org.eclipse.ui.views.properties.IPropertySource;
import org.eclipse.ui.views.properties.TextPropertyDescriptor;

/**
 * PropertySource for the Metadata of an IObservation.
 * 
 * @author schlienger
 */
public class ObservationPropertySource implements IPropertySource
{
  private final IObservation m_observation;
  private IPropertyDescriptor[] m_descriptors = null;

  public ObservationPropertySource( final IObservation obs )
  {
    m_observation = obs;
  }

  /**
   * @see org.eclipse.ui.views.properties.IPropertySource#getEditableValue()
   */
  public Object getEditableValue()
  {
    return m_observation.getName();
  }

  /**
   * @see org.eclipse.ui.views.properties.IPropertySource#getPropertyDescriptors()
   */
  public IPropertyDescriptor[] getPropertyDescriptors()
  {
    if( m_descriptors == null )
    {
      Metadata md = m_observation.getMetadata();
      
      m_descriptors = new IPropertyDescriptor[ md.size() ];
      
      Object[] keys = md.keySet().toArray();
      Arrays.sort( keys );
      
      for( int i = 0; i < keys.length; i++ )
      {
        String key = keys[i].toString();
        m_descriptors[i] = new TextPropertyDescriptor( key , key );
      }
    }
    
    return m_descriptors;
  }

  /**
   * @see org.eclipse.ui.views.properties.IPropertySource#getPropertyValue(java.lang.Object)
   */
  public Object getPropertyValue( Object id )
  {
    return m_observation.getMetadata().get( id );
  }

  /**
   * @see org.eclipse.ui.views.properties.IPropertySource#isPropertySet(java.lang.Object)
   */
  public boolean isPropertySet( Object id )
  {
    // not relevant
    return true;
  }

  /**
   * @see org.eclipse.ui.views.properties.IPropertySource#resetPropertyValue(java.lang.Object)
   */
  public void resetPropertyValue( Object id )
  {
    // not relevant
  }

  /**
   * @see org.eclipse.ui.views.properties.IPropertySource#setPropertyValue(java.lang.Object, java.lang.Object)
   */
  public void setPropertyValue( Object id, Object value )
  {
    // not relevant
  }
}
