/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.ogc.sensor.filter.test;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.net.URL;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import junit.framework.TestCase;

import org.apache.commons.io.IOUtils;
import org.kalypso.contribs.java.xml.XMLUtilities;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.zml.filters.IntervallFilter;
import org.kalypso.zml.filters.ObjectFactory;
import org.kalypso.zml.filters.ZmlFilter;
import org.w3._1999.xlinkext.SimpleLinkType;

public class IntervallFilterTest extends TestCase
{
  private static final SimpleDateFormat XML_DATETIME_FORMAT = new SimpleDateFormat( "yyyy-MM-dd'T'HH:mm:ss" );

  private ObjectFactory m_fac = new ObjectFactory();

  public void testIntervallFilter()
  {
    Writer writer = null;
    try
    {
      final URL resource = getClass().getResource( "Niederschlag.zml" );

      final ObjectFactory fac = new ObjectFactory();

      final ZmlFilter zmlFilter = zmlFilterFromUrl( resource );

      final IntervallFilter intervallFilter = fac.createIntervallFilter();
      intervallFilter.setAmount( 10 );
      intervallFilter.setCalendarField( "MINUTE" );
      intervallFilter.setMode( "sum" );
      intervallFilter.setDefaultStatus( 4 );
      intervallFilter.setDefaultValue( 12.9 );
      intervallFilter.setFilter( zmlFilter );
      writer = new StringWriter();
      final Marshaller marshaller = fac.createMarshaller();
      marshaller.marshal( intervallFilter, writer );
      writer.close();

      final String string = XMLUtilities.removeXMLHeader( writer.toString() );
      final String filterInline = XMLUtilities.prepareInLine( string );
      final URL zmlURL = new URL( resource.toExternalForm() + "?" + filterInline );

      /* final IObservation obs = */ZmlFactory.parseXML( resource, "Tageswerte" );
      /* final IObservation observation = */ZmlFactory.parseXML( zmlURL, "id" );

      //      System.out.println( ObservationUtilities.dump( obs.getValues( null ), "," ) );
      //      System.out.println();

      //      final Date from = XML_DATETIME_FORMAT.parse( "2005-02-16T17:00:00" );
      //      final Date to = XML_DATETIME_FORMAT.parse( "2005-02-16T18:36:00" );
      //      String dump = ObservationUtilities.dump( observation
      //          .getValues( new ObservationRequest( new DateRange( from, to ) ) ), "," );
      //      System.out.println( dump );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }

  private ZmlFilter zmlFilterFromUrl( final URL resource ) throws JAXBException
  {
    final org.w3._1999.xlinkext.ObjectFactory linkFac = new org.w3._1999.xlinkext.ObjectFactory();
    final SimpleLinkType xlink = linkFac.createSimpleLinkType();
    final String href = resource.toExternalForm();
    xlink.setHref( href );

    final ZmlFilter zmlFilter = m_fac.createZmlFilter();
    zmlFilter.setZml( xlink );

    return zmlFilter;
  }

  public void testTageswert() throws SensorException, JAXBException, IOException, ParseException
  {
    final URL tagesWertZmlURL = getClass().getResource( "resources/tageswerte_barby.zml" );

    final ZmlFilter zmlFilter = zmlFilterFromUrl( tagesWertZmlURL );

    final IntervallFilter intervallFilter = m_fac.createIntervallFilter();
    intervallFilter.setAmount( 1 );
    intervallFilter.setCalendarField( "HOUR_OF_DAY" );
    intervallFilter.setMode( "sum" );
    intervallFilter.setDefaultStatus( 2 );
    intervallFilter.setDefaultValue( 0.0 );
    intervallFilter.setFilter( zmlFilter );

    final StringWriter writer = new StringWriter();
    final Marshaller marshaller = m_fac.createMarshaller();
    marshaller.marshal( intervallFilter, writer );
    writer.close();

    final String string = XMLUtilities.removeXMLHeader( writer.toString() );
    final String filterInline = XMLUtilities.prepareInLine( string );
    final URL zmlURL = new URL( tagesWertZmlURL.toExternalForm() + "?" + filterInline );
    final IObservation observation = ZmlFactory.parseXML( zmlURL, "id" );

    // Dump source Observation
    //  final IObservation obs = ZmlFactory.parseXML( tagesWertZmlURL, "Tageswerte" );
    //  System.out.println( obs.getName() );
    //    System.out.println( ObservationUtilities.dump( obs.getValues( null ), "," ) );
    //    System.out.println();

    final Date from = XML_DATETIME_FORMAT.parse( "2006-07-20T17:00:00+02:00" );
    final Date to = XML_DATETIME_FORMAT.parse( "2006-07-25T17:00:00+02:00" );

    final ITuppleModel obsValues = observation.getValues( new ObservationRequest( new DateRange( from, to ) ) );
    //    System.out.println( ObservationUtilities.dump( obsValues, "," ) );

    final IAxis valueAxis = ObservationUtilities.findAxisByClass( obsValues.getAxisList(), Double.class );
    assertNotNull( "There must be a value axis", valueAxis );

    final Double firstValue = (Double)obsValues.getElement( 0, valueAxis );
    assertNotNull( "The first element must be non null", firstValue );

    // test for the bug
    // BUGFIX:
    // Bug description: the intervall filter cut the first value, when its interval
    // started before the first wanted value. -> Tageswertproblem
    assertTrue( "The first value must be greater than 0.0", firstValue.doubleValue() > 0.1 );
  }

  public void testLeer() throws SensorException, JAXBException, IOException, ParseException
  {
    final URL tagesWertZmlURL = getClass().getResource( "resources/leer.zml" );

    final ZmlFilter zmlFilter = zmlFilterFromUrl( tagesWertZmlURL );

    final IntervallFilter intervallFilter = m_fac.createIntervallFilter();
    intervallFilter.setAmount( 1 );
    intervallFilter.setCalendarField( "HOUR_OF_DAY" );
    intervallFilter.setMode( "sum" );
    intervallFilter.setDefaultStatus( 2 );
    intervallFilter.setDefaultValue( 0.0 );
    intervallFilter.setFilter( zmlFilter );

    final StringWriter writer = new StringWriter();
    final Marshaller marshaller = m_fac.createMarshaller();
    marshaller.marshal( intervallFilter, writer );
    writer.close();

    final String string = XMLUtilities.removeXMLHeader( writer.toString() );
    final String filterInline = XMLUtilities.prepareInLine( string );
    final URL zmlURL = new URL( tagesWertZmlURL.toExternalForm() + "?" + filterInline );
    final IObservation observation = ZmlFactory.parseXML( zmlURL, "id" );

    // Dump source Observation
    //    final IObservation obs = ZmlFactory.parseXML( tagesWertZmlURL, "Tageswerte" );
    //    System.out.println( obs.getName() );
    //    System.out.println( ObservationUtilities.dump( obs.getValues( null ), "," ) );
    //    System.out.println();

    final Date from = XML_DATETIME_FORMAT.parse( "2006-07-20T17:00:00+02:00" );
    final Date to = XML_DATETIME_FORMAT.parse( "2006-07-25T17:00:00+02:00" );

    final ITuppleModel obsValues = observation.getValues( new ObservationRequest( new DateRange( from, to ) ) );

    // Dump Result
    //    System.out.println( ObservationUtilities.dump( obsValues, "," ) );

    final IAxis valueAxis = ObservationUtilities.findAxisByClass( obsValues.getAxisList(), Double.class );
    assertNotNull( "There must be a value axis", valueAxis );

    final Double firstValue = (Double)obsValues.getElement( 0, valueAxis );
    assertNotNull( "The first element must be non null", firstValue );
  }

}
