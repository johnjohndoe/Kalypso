package org.kalypso.ogc.sensor.view.test;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.view.ObservationCache;
import org.kalypso.util.adapter.IAdaptable;

import junit.framework.TestCase;

/**
 * @author schlienger
 */
public class ObservationCacheTest extends TestCase
{
  public void testGetObservationFor()
  {
    final IObservation obs = ObservationCache.getObservationFor( new FooAdaptable() );
    
    assertNotNull( obs );
    
    System.out.println( obs );
  }

  private static class FooAdaptable implements IAdaptable
  {
    /**
     * @see org.kalypso.util.adapter.IAdaptable#getAdapter(java.lang.Class)
     */
    public Object getAdapter( Class anotherClass )
    {
      if( anotherClass == IObservation.class )
        return new SimpleObservation(  );
      
      return null;
    }
  }
}
