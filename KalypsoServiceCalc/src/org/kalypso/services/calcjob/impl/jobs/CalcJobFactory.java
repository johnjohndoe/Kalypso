package org.kalypso.services.calcjob.impl.jobs;

import java.io.IOException;
import java.net.URL;
import java.util.Properties;

import org.kalypso.services.calcjob.CalcJob;
import org.kalypso.services.calcjob.CalcJobServiceException;

/**
 * @author belger
 */
public class CalcJobFactory
{
  private static final Properties m_jobTypes = new Properties();

  public CalcJobFactory()
  {
    try
    {
      m_jobTypes.load( CalcJobFactory.class.getResourceAsStream( "jobtypes.properties" ) );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }
  }

  public String[] getSupportedTypes()
  {
    return (String[])m_jobTypes.keySet().toArray( new String[0] );
  }

  public CalcJob createJob( final String id, final String description, final URL[] arguments, final String typeID ) throws CalcJobServiceException
  {
    try
    {
      final String className = m_jobTypes.getProperty( typeID );

      final CalcJob job = (CalcJob)Class.forName( className ).newInstance();

      job.init( id, description, arguments, typeID );

      return job;
    }
    catch( final Exception e )
    {
      throw new CalcJobServiceException( "Konnte Job nicht erzeugen für Typ: " + typeID, e );
    }
  }
}