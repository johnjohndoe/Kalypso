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
package org.kalypso.ogc.sensor.filter.test;

import java.io.StringWriter;
import java.io.Writer;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;

import junit.framework.TestCase;

import org.apache.commons.io.IOUtils;
import org.kalypso.contribs.java.xml.XMLUtilities;
import org.kalypso.jwsdp.JaxbUtilities;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.zml.filters.IntervallFilterType;
import org.kalypso.zml.filters.ObjectFactory;
import org.kalypso.zml.filters.ZmlFilterType;
import org.w3._1999.xlinkext.SimpleLinkType;

public class IntervallFilterTest extends TestCase
{
  public void testIntervallFilter( )
  {
    final SimpleDateFormat XML_DATETIME_FORMAT = new SimpleDateFormat( "yyyy-MM-dd'T'HH:mm:ss" );

    Writer writer = null;
    try
    {
      final URL resource = getClass().getResource( "Niederschlag.zml" );

      final org.w3._1999.xlinkext.ObjectFactory linkFac = new org.w3._1999.xlinkext.ObjectFactory();
      final SimpleLinkType xlink = linkFac.createSimpleLinkType();
      final String href = resource.toExternalForm();
      xlink.setHref( href );

      final ObjectFactory fac = new ObjectFactory();
      final JAXBContext jc = JaxbUtilities.createQuiet( ObjectFactory.class );
      final ZmlFilterType zmlFilter = fac.createZmlFilterType();
      zmlFilter.setZml( xlink );

      final IntervallFilterType intervallFilter = fac.createIntervallFilterType();
      intervallFilter.setAmount( 10 );
      intervallFilter.setCalendarField( "MINUTE" );
      intervallFilter.setMode( "sum" );
      intervallFilter.setDefaultStatus( 4 );
      intervallFilter.setDefaultValue( 12.9 );
      intervallFilter.setFilter( fac.createZmlFilter( zmlFilter ) );
      writer = new StringWriter();
      final Marshaller marshaller = JaxbUtilities.createMarshaller( jc );
      marshaller.marshal( intervallFilter, writer );
      writer.close();
      final String string = XMLUtilities.removeXMLHeader( writer.toString() );
      final String filterInline = XMLUtilities.prepareInLine( string );
      final URL zmlURL = new URL( href + "?" + filterInline );
      final IObservation observation = ZmlFactory.parseXML( zmlURL, "id" );

      // ZML geht von
      // "2005-02-16T16:50:00"
      // bis
      // "2005-02-23T17:00:00"
      final Date from = XML_DATETIME_FORMAT.parse( "2005-02-16T17:00:00" );
      final Date to = XML_DATETIME_FORMAT.parse( "2005-02-16T18:36:00" );
      // final Date from = XML_DATETIME_FORMAT.parse( "2005-02-23T16:00:00" );
      // final Date to = XML_DATETIME_FORMAT.parse( "2005-02-23T18:00:00" );
      String dump = ObservationUtilities.dump( observation.getValues( new ObservationRequest( new DateRange( from, to ) ) ), "," );
      System.out.println( dump );
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
}
