package org.kalypso.ogc.sensor.view.propertySource;

import org.eclipse.ui.views.properties.IPropertySource;
import org.eclipse.ui.views.properties.IPropertySourceProvider;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.util.adapter.IAdaptable;

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
    if( object instanceof IAdaptable )
    {
      IObservation obs = (IObservation)((IAdaptable)object).getAdapter( IObservation.class );
      if( obs == null )
        return null;
      
      return new ObservationPropertySource( obs );
    }
    
    return null;
  }
}
