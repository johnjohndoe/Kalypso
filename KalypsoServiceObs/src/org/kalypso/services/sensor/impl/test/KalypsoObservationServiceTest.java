package org.kalypso.services.sensor.impl.test;

import java.io.File;

import org.kalypso.java.lang.reflect.ClassUtilities.ClassUtilityException;
import org.kalypso.repository.RepositoryException;
import org.kalypso.services.sensor.impl.KalypsoObservationService;

import junit.framework.TestCase;

/**
 * @author schlienger
 */
public class KalypsoObservationServiceTest extends TestCase
{
  public void testKalypsoObservationService() throws RepositoryException, ClassUtilityException
  {
    KalypsoObservationService serv = new KalypsoObservationService();
    
    //File file = new File("//Pc242/KalypsoServer/data/mirrored/SomeObservations");
    //System.out.println( file.exists() );
  }
}
