/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.optimize;

import java.io.IOException;
import java.io.Writer;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.SortedMap;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.optimize.errorfunctions.ErrorFunctionFactory;
import org.kalypso.optimize.errorfunctions.IErrorFunktion;
import org.kalypso.optimizer.AutoCalibration;
import org.kalypso.optimizer.Parameter;

/**
 * this class handles the comunication between optimizer routine (SCE) and calcjob routine
 * 
 * @author doemming
 */
public class SceIOHandler
{
  private static Pattern PARAMETER_COUNT = Pattern.compile( "^.+\\&Parameter: n =.*?(\\d+).*?\\r$" );

  private static Pattern PARAMETER = Pattern.compile( "^.*?(\\d+\\.\\d+.(E|)(-|\\+|)\\d*).*?\\r$" );

  private final static int STATUS_READ_PARAMETER_COUNT = 1;

  private final static int STATUS_READ_PARAMETER = 2;

  private final static int STATUS_CALCULATE_AND_EVALUATE = 3;

  private final Parameter[] m_parameterConf;

  private final IOptimizingJob m_job;

  private final IErrorFunktion m_errorFunction;

  private final List<Double> m_parameter = new ArrayList<Double>();

  private final Logger m_logger;

  private int m_parameterCount = 0;

  private int status = STATUS_READ_PARAMETER_COUNT;

  private int m_calculationCounter = 0;

  private double m_bestEvaluation = -1;

  public SceIOHandler( final Logger logger, final AutoCalibration calibration, final IOptimizingJob job ) throws MalformedURLException, SensorException
  {
    m_logger = logger;
    // calibration.getPegel().getFile()
    final SortedMap<Date, Double> measuersTS = job.getMeasuredTimeSeries();
    m_errorFunction = ErrorFunctionFactory.createErrorFunktion( measuersTS, calibration );
    final List<Parameter> parameterList = calibration.getParameterlist().getParameter();
    final Parameter[] parameters = parameterList.toArray( new Parameter[parameterList.size()] );
    m_parameterConf = parameters;
    m_job = job;
  }

  public void readLine( final String line )
  {
    switch( status )
    {
      case STATUS_READ_PARAMETER_COUNT:
      {
        final Matcher m = PARAMETER_COUNT.matcher( line );
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
        final Matcher m = PARAMETER.matcher( line );
        if( m.matches() )
        {
          final String group = m.group( 1 );
          final double sceValue = Double.parseDouble( group );
          final double realValue = scaleSCEtoRealValue( sceValue, m_parameter.size() );
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

  private double scaleSCEtoRealValue( final double sceValue, final int index )
  {
    final double result = m_parameterConf[index].getLowerBound() + sceValue * (m_parameterConf[index].getUpperBound() - m_parameterConf[index].getLowerBound());
    return result;
  }

  public void handle( final Writer inputWriter ) throws IOException
  {
    switch( status )
    {
      case STATUS_CALCULATE_AND_EVALUATE:
      {
        String answer;
        try
        {
          recalculate();
          final double evaluation = evaluate();
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
          m_logger.info( "evaluation of " + m_calculationCounter + ". calculation is " + evaluation + " (best is " + m_bestEvaluation + ")" );
        }
        catch( final Exception e )
        {
          m_calculationCounter++;
          m_job.setBestEvaluation( false );
          answer = Double.toString( m_bestEvaluation + 999d );
          e.printStackTrace();
        }
        inputWriter.write( answer );
        inputWriter.write( '\n' );
        inputWriter.flush();
        KalypsoOptimizeDebug.DEBUG.printf( "SCE < %s", answer );
        status = STATUS_READ_PARAMETER_COUNT;
      }
      break;
      default:
        break;
    }
  }

  private double evaluate( ) throws MalformedURLException, SensorException
  {
    final SortedMap<Date, Double> calcedTS = m_job.getCalcedTimeSeries();
    return m_errorFunction.calculateError( calcedTS );
  }

  private void recalculate( ) throws Exception
  {
    KalypsoOptimizeDebug.DEBUG.printf( "%n%nrecalculation step %d...", m_calculationCounter );
    final double[] newValues = new double[m_parameter.size()];
    for( int i = 0; i < newValues.length; i++ )
    {
      newValues[i] = (m_parameter.get( i )).doubleValue();
    }
    m_job.optimize( m_parameterConf, newValues );
    m_job.calculate();
  }

  public void handleStreams( final StringBuffer outBuffer, final StringBuffer errBuffer, final Writer inputWriter ) throws IOException
  {
    // REMARK: do not inline: getLines() deletes read data from the buffer
    final String[] outLines = getLines( outBuffer );
    final String[] errLines = getLines( errBuffer );

    printlines( "OUT: ", outLines );
    printlines( "ERR: ", errLines );

    for( final String element : outLines )
      readLine( element );

    handle( inputWriter );
  }

  private void printlines( final String prefix, final String[] lines )
  {
    for( final String line : lines )
      KalypsoOptimizeDebug.DEBUG.printf( "%s%s", prefix, line );
  }

  public String[] getLines( final StringBuffer buffer )
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

  public int getStep( )
  {
    return m_calculationCounter;
  }
}