package org.kalypso.ogc.sensor;

import org.eclipse.ui.views.properties.IPropertySource;
import org.eclipse.ui.views.properties.IPropertySourceProvider;
import org.kalypso.ogc.sensor.IObservation;

/**
 * Provides an ObservationPropertySource.
 * 
 * @author schlienger
 */
public class ObservationPropertySourceProvider implements IPropertySourceProvider
{
  /**
   * @see org.eclipse.ui.views.properties.IPropertySourceProvider#getPropertySource(java.lang.Object)
   */
  public IPropertySource getPropertySource( Object object )
  {
    if( object instanceof IObservation )
      return new ObservationPropertySource( (IObservation)object );
    
    return null;
  }
}
