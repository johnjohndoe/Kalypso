package org.kalypso.optimize;

import java.io.IOException;
import java.io.Writer;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.List;
import java.util.TreeMap;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.optimize.errorfunctions.ErrorFunctionFactory;
import org.kalypso.optimize.errorfunctions.IErrorFunktion;
import org.kalypso.optimizer.AutoCalibration;
import org.kalypso.optimizer.Parameter;

/**
 * 
 * this class handles the comunication between optimizer routine (SCE) and
 * calcjob routine
 * 
 * @author doemming
 */
public class SceIOHandler
{
  private static boolean DEBUG = false;

  private static Pattern PARAMETER_COUNT = Pattern.compile( "^.+\\&Parameter: n =.*?(\\d+).*?\\r$" );

  private static Pattern PARAMETER = Pattern.compile( "^.*?(\\d+\\.\\d+.(E|)(-|\\+|)\\d*).*?\\r$" );

  private final static int STATUS_READ_PARAMETER_COUNT = 1;

  private final static int STATUS_READ_PARAMETER = 2;

  private final static int STATUS_CALCULATE_AND_EVALUATE = 3;

  private final Parameter[] m_parameterConf;

  private final IOpmizingJob m_job;

  private final IErrorFunktion m_errorFunction;

  private final List m_parameter = new ArrayList();

  private final Logger m_logger;

  private int m_parameterCount = 0;

  private int status = STATUS_READ_PARAMETER_COUNT;

  private int m_calculationCounter = 0;

  private double m_bestEvaluation = -1;

  public SceIOHandler( Logger logger, AutoCalibration calibration, IOpmizingJob job )
      throws MalformedURLException, SensorException
  {
    m_logger = logger;
    //    calibration.getPegel().getFile()
    TreeMap measuersTS = job.getMeasuredTimeSeries();
    m_errorFunction = ErrorFunctionFactory.createErrorFunktion( measuersTS, calibration );
    final List parameterList = calibration.getParameterlist().getParameter();
    final Parameter[] parameters = (Parameter[])parameterList.toArray( new Parameter[parameterList
        .size()] );
    m_parameterConf = parameters;
    m_job = job;

  }

  public void readLine( String line )
  {
    switch( status )
    {
    case STATUS_READ_PARAMETER_COUNT:
    {
      Matcher m = PARAMETER_COUNT.matcher( line );
      if( m.matches() )
      {
        m_parameterCount = Integer.parseInt( m.group( 1 ) );
        status = STATUS_READ_PARAMETER;
        m_parameter.clear();
      }
    }
      break;
    case STATUS_READ_PARAMETER:
    {
      Matcher m = PARAMETER.matcher( line );
      if( m.matches() )
      {
        String group = m.group( 1 );
        double sceValue = Double.parseDouble( group );
        double realValue = scaleSCEtoRealValue( sceValue, m_parameter.size() );
        m_parameter.add( new Double( realValue ) );
        if( m_parameter.size() == m_parameterCount )
          status = STATUS_CALCULATE_AND_EVALUATE;
      }
    }
      break;
    default:
      break;
    }
  }

  private double scaleSCEtoRealValue( double sceValue, int index )
  {
    double result = m_parameterConf[index].getLowerBound() + sceValue
        * ( m_parameterConf[index].getUpperBound() - m_parameterConf[index].getLowerBound() );
    return result;
  }

  public void handle( Writer inputWriter ) throws IOException
  {
    switch( status )
    {
    case STATUS_CALCULATE_AND_EVALUATE:
    {
      String answer;
      try
      {
        recalculate();
        double evaluation = evaluate();
        if( m_calculationCounter == 0 || evaluation < m_bestEvaluation )
        {
          m_job.setBestEvaluation( true );
          m_bestEvaluation = evaluation;
        }
        else
        {
          m_job.setBestEvaluation( false );
        }
        m_calculationCounter++;
        answer = Double.toString( evaluation ) + "\n";
        m_logger.info( "evaluation of " + m_calculationCounter + ". calculation is "
            + evaluation + " (best is " + m_bestEvaluation + ")" );
      }
      catch( Exception e )
      {
        answer = Double.toString( m_bestEvaluation + 999d );
        e.printStackTrace();
      }
      inputWriter.write( answer );
      inputWriter.flush();
      if( DEBUG )
        System.out.print( "SCE < " + answer );
      status = STATUS_READ_PARAMETER_COUNT;
    }
      break;
    default:
      break;
    }
  }

  private double evaluate() throws MalformedURLException, SensorException
  {
    TreeMap calcedTS = m_job.getCalcedTimeSeries();
    return m_errorFunction.calculateError( calcedTS );
  }

  private void recalculate() throws Exception
  {
    if( DEBUG )
      System.out.println( "\n\nrecalculation ..." );
    double[] newValues = new double[m_parameter.size()];
    for( int i = 0; i < newValues.length; i++ )
    {
      newValues[i] = ( (Double)m_parameter.get( i ) ).doubleValue();
    }
    m_job.optimize( m_parameterConf, newValues );
    m_job.calculate();
  }

  public void handleStreams( StringBuffer outBuffer, StringBuffer errBuffer, Writer inputWriter )
      throws IOException
  {
    String[] out = getLines( outBuffer );
    String[] err = getLines( errBuffer );
    if( DEBUG )
      printlines( "OUT: ", out );
    if( DEBUG )
      printlines( "ERR: ", err );
    for( int i = 0; i < out.length; i++ )
    {
      readLine( out[i] );
    }
    handle( inputWriter );
  }

  private void printlines( String prefix, String[] lines )
  {
    for( int i = 0; i < lines.length; i++ )
    {
      System.out.print( prefix + lines[i] );
    }
  }

  public String[] getLines( StringBuffer buffer )
  {
    final String out = buffer.toString();
    final int lastBR = out.lastIndexOf( '\n' );
    if( lastBR > -1 )
    {
      buffer.delete( 0, lastBR );

      final String linesComplete = out.substring( 0, lastBR );
      if( linesComplete.length() > 0 )
      {
        return linesComplete.split( "\n" );
      }
    }
    return new String[0];
  }
}