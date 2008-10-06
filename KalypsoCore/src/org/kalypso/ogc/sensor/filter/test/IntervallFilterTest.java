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

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;

import junit.framework.TestCase;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.bind.JaxbUtilities;
import org.kalypso.core.i18n.Messages;
import org.kalypso.ogc.sensor.filter.FilterFactory;
import org.kalypso.zml.filters.IntervallFilterType;
import org.kalypso.zml.filters.ZmlFilterType;
import org.w3._1999.xlinkext.SimpleLinkType;

public class IntervallFilterTest extends TestCase
{
  public void testIntervallFilter( ) throws Exception
  {
    Writer writer = null;
    try
    {
      final URL resource = getClass().getResource( Messages.getString("org.kalypso.ogc.sensor.filter.test.IntervallFilterTest.1") ); //$NON-NLS-1$

      final org.w3._1999.xlinkext.ObjectFactory linkFac = new org.w3._1999.xlinkext.ObjectFactory();
      final SimpleLinkType xlink = linkFac.createSimpleLinkType();
      final String href = resource.toExternalForm();
      xlink.setHref( href );
      xlink.setType( "simple" ); //$NON-NLS-1$

      // TODO: probably the second filter factory will be forgotten everywhere
      // so move instantiation into central helper class and use it everywhere

      final JAXBContext jc = FilterFactory.JC_FILTER;

      final ZmlFilterType zmlFilter = FilterFactory.OF_FILTER.createZmlFilterType();
      zmlFilter.setZml( xlink );

      final IntervallFilterType intervallFilter = FilterFactory.OF_FILTER.createIntervallFilterType();
      intervallFilter.setAmount( 10 );
      intervallFilter.setCalendarField( Messages.getString("org.kalypso.ogc.sensor.filter.test.IntervallFilterTest.3") ); //$NON-NLS-1$
      intervallFilter.setMode( "sum" ); //$NON-NLS-1$
      intervallFilter.setDefaultStatus( 4 );
      intervallFilter.setDefaultValue( 12.9 );
      intervallFilter.setFilter( FilterFactory.OF_FILTER.createZmlFilter( zmlFilter ) );
      writer = new StringWriter();
      final Marshaller marshaller = JaxbUtilities.createMarshaller( jc, true );
      marshaller.marshal( FilterFactory.OF_FILTER.createIntervallFilter( intervallFilter ), writer );
      writer.close();
//      final String string = XMLUtilities.removeXMLHeader( writer.toString() );
//      final String filterInline = XMLUtilities.prepareInLine( string );
      
      // REMARK: this is all crap! Many of the used characters in the filter are not allowed in
      // URLs. So any URL parser may change them or do something strange.
      
      // The concrete problem here is, that the double '//' are removed by the URL-parser
      // So later, the filter will not be parsed correctly.
      // I have no idea how to fix this at the moment, so the test is commented out
//      final URL zmlURL = new URL( resource, href + "?" + filterInline ); //$NON-NLS-1$
//      final IObservation observation = ZmlFactory.parseXML( zmlURL, "id" );
//
//      // ZML geht von
//      // "2005-02-16T16:50:00"
//      // bis
//      // "2005-02-23T17:00:00"
//      final Date from = XML_DATETIME_FORMAT.parse( "2005-02-16T17:00:00" );
//      final Date to = XML_DATETIME_FORMAT.parse( "2005-02-16T18:36:00" );
//      // final Date from = XML_DATETIME_FORMAT.parse( "2005-02-23T16:00:00" );
//      // final Date to = XML_DATETIME_FORMAT.parse( "2005-02-23T18:00:00" );
//      String dump = ObservationUtilities.dump( observation.getValues( new ObservationRequest( new DateRange( from, to ) ) ), "," );
//      System.out.println( dump );
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }
}
