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
package org.kalypso.ogc.sensor.zml.test;

import java.io.StringReader;
import java.io.StringWriter;
import java.net.URL;
import java.text.ParseException;
import java.text.SimpleDateFormat;

import javax.xml.bind.JAXBException;

import junit.framework.TestCase;

import org.kalypso.commons.factory.FactoryException;
import org.kalypso.contribs.java.util.DoubleComparator;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.zml.Observation;
import org.xml.sax.InputSource;

/**
 * @author schlienger
 */
public class ZmlTest extends TestCase
{
  private final SimpleDateFormat df = new SimpleDateFormat( "dd.MM.yyyy" );

  private final DoubleComparator dc = new DoubleComparator( 0 );

  // contains the list of ZMLs to test
  private final static String[] ZMLs = { "resources/beispiel.zml", "resources/inline_ex.zml" };

  public void testZmls( ) throws SensorException, ParseException
  {
    for( int i = 0; i < ZMLs.length; i++ )
    {
      System.out.println( "Testing: " + ZMLs[i] );

      final String obsID = ZMLs[i];
      final URL zmlURL = getClass().getResource( ZMLs[i] );

      final IObservation obs = ZmlFactory.parseXML( zmlURL, obsID );

      _testGetName( obs );
      _testGetTarget( obs );
      _testGetAxisList( obs );
      _testGetIdentifier( obs, obsID );
      _testGetMetadataList( obs );
      _testGetValues( obs );
      _testIsEditable( obs );
      _testSetValues( obs );
    }
  }

  private void _testGetName( final IObservation obs )
  {
    assertTrue( obs.getName().equals( "Eine Test-Observation" ) );
  }

  private void _testIsEditable( final IObservation obs )
  {
    assertTrue( obs.isEditable() );
  }

  private void _testGetIdentifier( final IObservation obs, final String obsID )
  {
    assertTrue( obs.getIdentifier().equals( obsID ) );
  }

  private void _testGetTarget( final IObservation obs )
  {
    assertNull( obs.getTarget() );
  }

  private void _testGetMetadataList( final IObservation obs )
  {
    final MetadataList mdl = obs.getMetadataList();
    assertNotNull( mdl );

    assertTrue( mdl.getProperty( "Pegelnullpunkt" ).equals( "10" ) );

    assertTrue( mdl.getProperty( "Alarmstufe 1" ).equals( "4.3" ) );
  }

  private void _testGetAxisList( final IObservation obs )
  {
    final IAxis[] axes = obs.getAxisList();
    assertNotNull( axes );

    assertTrue( axes.length == 3 );
  }

  private void _testGetValues( final IObservation obs ) throws SensorException, ParseException
  {
    final ITuppleModel values = obs.getValues( null );
    assertNotNull( values );

    assertEquals( values.getCount(), 21 );

    final IAxis[] axes = values.getAxisList();

    final IAxis dateAxis = ObservationUtilities.findAxisByName( axes, "Datum" );
    assertNotNull( dateAxis );

    final IAxis vAxis1 = ObservationUtilities.findAxisByName( axes, "Pegel1" );
    assertNotNull( vAxis1 );

    final IAxis vAxis2 = ObservationUtilities.findAxisByName( axes, "Pegel2" );
    assertNotNull( vAxis2 );

    assertEquals( values.getElement( 0, dateAxis ), df.parse( "01.01.2004" ) );
    assertTrue( dc.compare( (Number) values.getElement( 0, vAxis1 ), Double.valueOf( "1.0" ) ) == 0 );
    assertTrue( dc.compare( (Number) values.getElement( 0, vAxis2 ), new Double( 11 ) ) == 0 );

    assertEquals( values.getElement( 20, dateAxis ), df.parse( "21.01.2004" ) );
    assertTrue( dc.compare( (Number) values.getElement( 20, vAxis1 ), Double.valueOf( "16.6" ) ) == 0 );
    assertTrue( dc.compare( (Number) values.getElement( 20, vAxis2 ), Double.valueOf( "18.5" ) ) == 0 );
  }

  private void _testSetValues( final IObservation obs ) throws SensorException, ParseException
  {
    final IAxis[] axes = obs.getAxisList();

    final IAxis dateAxis = ObservationUtilities.findAxisByName( axes, "Datum" );
    assertNotNull( dateAxis );

    final IAxis vAxis1 = ObservationUtilities.findAxisByName( axes, "Pegel1" );
    assertNotNull( vAxis1 );

    final IAxis vAxis2 = ObservationUtilities.findAxisByName( axes, "Pegel2" );
    assertNotNull( vAxis2 );

    final SimpleTuppleModel m = new SimpleTuppleModel( obs.getAxisList() );

    final Object[] t1 = new Object[3];
    t1[m.getPositionFor( dateAxis )] = df.parse( "20.01.2004" );
    t1[m.getPositionFor( vAxis1 )] = new Double( 44 );
    t1[m.getPositionFor( vAxis2 )] = new Double( 11 );
    m.addTupple( t1 );

    final Object[] t2 = new Object[3];
    t2[m.getPositionFor( dateAxis )] = df.parse( "21.01.2004" );
    t2[m.getPositionFor( vAxis1 )] = new Double( 55 );
    t2[m.getPositionFor( vAxis2 )] = new Double( 22 );
    m.addTupple( t2 );

    final Object[] t3 = new Object[3];
    t3[m.getPositionFor( dateAxis )] = df.parse( "22.01.2004" );
    t3[m.getPositionFor( vAxis1 )] = new Double( 66 );
    t3[m.getPositionFor( vAxis2 )] = new Double( 33 );
    m.addTupple( t3 );

    obs.setValues( m );

    final ITuppleModel values = obs.getValues( null );
    assertNotNull( values );

    assertEquals( values.getCount(), 22 );

    int i = 19;
    assertEquals( values.getElement( i, dateAxis ), df.parse( "20.01.2004" ) );
    assertTrue( dc.compare( (Number) values.getElement( i, vAxis1 ), Double.valueOf( "44" ) ) == 0 );
    assertTrue( dc.compare( (Number) values.getElement( i, vAxis2 ), Double.valueOf( "11" ) ) == 0 );

    i = 20;
    assertEquals( values.getElement( i, dateAxis ), df.parse( "21.01.2004" ) );
    assertTrue( dc.compare( (Number) values.getElement( i, vAxis1 ), Double.valueOf( "55" ) ) == 0 );
    assertTrue( dc.compare( (Number) values.getElement( i, vAxis2 ), Double.valueOf( "22" ) ) == 0 );

    i = 21;
    assertEquals( values.getElement( i, dateAxis ), df.parse( "22.01.2004" ) );
    assertTrue( dc.compare( (Number) values.getElement( i, vAxis1 ), Double.valueOf( "66" ) ) == 0 );
    assertTrue( dc.compare( (Number) values.getElement( i, vAxis2 ), Double.valueOf( "33" ) ) == 0 );
  }

  /**
   * Tests the new mechanism ('data'-Element) for storing Metadata stuff
   */
  public void testMetadataEx( ) throws SensorException, FactoryException, JAXBException
  {
    final URL zmlURL = getClass().getResource( "resources/beispiel-metadata.zml" );

    final IObservation obs = ZmlFactory.parseXML( zmlURL, "beispiel-metadata.zml" );

    final Observation xml = ZmlFactory.createXML( obs, null );
    final StringWriter writer = new StringWriter();
    ZmlFactory.getMarshaller().marshal( xml, writer );

    final String xmlStr = writer.toString();

    System.out.println( xmlStr );

    final IObservation obs2 = ZmlFactory.parseXML( new InputSource( new StringReader( xmlStr ) ), "fake-id", null );
    final Observation xml2 = ZmlFactory.createXML( obs2, null );
    final StringWriter writer2 = new StringWriter();
    ZmlFactory.getMarshaller().marshal( xml2, writer2 );
    System.out.println( writer2.toString() );
  }
}