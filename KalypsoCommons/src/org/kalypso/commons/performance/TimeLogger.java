/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.commons.performance;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeConstants;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.Duration;

/**
 * A small helper class wich shows usthe duraction of some code.
 * <p>
 * Usage:
 * </p>
 * <p>
 * The class remebers its time of construction
 * </p>
 * <p>
 * Call #showElapsedTime( String ) to print a message, indicating how long it took since construction
 * </p>.
 * 
 * @author Belger
 */
public class TimeLogger
{
  private final static DatatypeConstants.Field[] FIELDS_HOURS = { DatatypeConstants.HOURS, DatatypeConstants.MINUTES, DatatypeConstants.SECONDS };
  
  private DatatypeFactory m_factory = null;
  
  private final long m_constructionTime;

  private long m_lastCall;

  private Duration m_currentInterimDuration;

  private Duration m_currentTotalDuration;

  public TimeLogger( )
  {
    this( null );
  }

  /**
   * Create a time logger and start measuring. Prints the given message.
   */
  public TimeLogger( final String message )
  {
    System.out.println( message );
    
    m_constructionTime = System.currentTimeMillis();
    try
    {
      m_factory = DatatypeFactory.newInstance();
    }
    catch( final DatatypeConfigurationException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    
    m_lastCall = m_constructionTime;
  }

  public void takeInterimTime( )
  {
    final long current = System.currentTimeMillis();

    final long sinceLastCall = current - m_lastCall;
    final long total = current - m_constructionTime;

    m_currentInterimDuration = m_factory.newDuration( sinceLastCall );
    m_currentTotalDuration = m_factory.newDuration( total );
    
    m_lastCall = current;
  }

  public void printCurrentInterim( final String message )
  {
    printDuration( message, m_currentInterimDuration, FIELDS_HOURS );
  }

  public void printCurrentTotal( final String message )
  {
    printDuration( message, m_currentTotalDuration, FIELDS_HOURS);
  }
  
  /**
   *  @deprecated Use {@link #takeInterimTime()} and instead.
   */
  public void showElapsedTime( final String message )
  {
    final long current = System.currentTimeMillis();

    final long sinceLastCall = current - m_lastCall;
    final long total = current - m_constructionTime;
    System.out.print( message );
    System.out.print( ": Total " );
    System.out.print( total );
    System.out.print( " millis / SinceLast " );
    System.out.print( sinceLastCall );
    System.out.println( " millis." );

    m_lastCall = current;
  }

  private static void printDuration( final String message, final Duration duration, final DatatypeConstants.Field[] fields )
  {
    System.out.print( message );

    for( final DatatypeConstants.Field field : fields )
    {
      if( duration.isSet( field ) )
      {
        System.out.print( duration.getField( field ) );
        System.out.print( " " );
        System.out.print( field.toString() );
        System.out.print( ", " );
      }
    }
    System.out.println();
  }
  
}
