package org.kalypso.services.calculation.job.impl;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Properties;

import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.service.CalcJobDataBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;

/**
 * @author belger
 */
public class CalcJobFactory
{
  private static final Properties m_jobTypes = new Properties();

  public CalcJobFactory( final File typeFile )
  {
    try
    {
      m_jobTypes.load( new FileInputStream( typeFile ) );
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

  public ICalcJob createJob( final String id, final String typeID, final String description, final CalcJobDataBean[] input ) throws CalcJobServiceException
  {
    try
    {
      final String className = m_jobTypes.getProperty( typeID );

      final ICalcJob job = (ICalcJob)Class.forName( className ).newInstance();

      job.init( id, typeID, description, input );

      return job;
    }
    catch( final Exception e )
    {
      throw new CalcJobServiceException( "Konnte Job nicht erzeugen für Typ: " + typeID, e );
    }
  }
}