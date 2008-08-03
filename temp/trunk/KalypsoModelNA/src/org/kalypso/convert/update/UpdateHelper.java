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
package org.kalypso.convert.update;

import java.io.StringWriter;
import java.io.Writer;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.bind.JaxbUtilities;
import org.kalypso.contribs.java.xml.XMLUtilities;
import org.kalypso.ogc.sensor.filter.FilterFactory;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.zml.filters.InterpolationFilterType;
import org.kalypso.zml.filters.IntervallFilterType;

/**
 * @author doemming
 */
public class UpdateHelper
{
  public static String createInterpolationFilter( int amountHours, String defaultValue, boolean forceFill ) throws JAXBException
  {
    StringWriter writer = null;
    try
    {
      final InterpolationFilterType interpolationFilter = FilterFactory.OF_FILTER.createInterpolationFilterType();
      interpolationFilter.setAmount( amountHours );
      interpolationFilter.setCalendarField( "HOUR_OF_DAY" );
      interpolationFilter.setDefaultValue( defaultValue );
      interpolationFilter.setDefaultStatus( KalypsoStati.BIT_CHECK );
      interpolationFilter.setForceFill( forceFill );
      Marshaller marshaller = JaxbUtilities.createMarshaller( FilterFactory.JC_FILTER );
      writer = new StringWriter( 0 );
      JAXBElement<InterpolationFilterType> interpolationFilterElement = FilterFactory.OF_FILTER.createInterpolationFilter( interpolationFilter );
      marshaller.marshal( interpolationFilterElement, writer );
      final String result = writer.toString();
      return XMLUtilities.prepareInLine( result );
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }

  public static String createIntervallFilter( int amount, String calendarField, String mode ) throws JAXBException
  {
    Writer writer = null;
    try
    {
      final IntervallFilterType intervallFilter = FilterFactory.OF_FILTER.createIntervallFilterType();
      intervallFilter.setAmount( amount );
      intervallFilter.setCalendarField( calendarField );
      intervallFilter.setMode( mode );
      writer = new StringWriter();
      final Marshaller marshaller = JaxbUtilities.createMarshaller( FilterFactory.JC_FILTER );
      final JAXBElement<IntervallFilterType> intervallFilterElement = FilterFactory.OF_FILTER.createIntervallFilter( intervallFilter );
      marshaller.marshal( intervallFilterElement, writer );

      final String string = XMLUtilities.removeXMLHeader( writer.toString() );
      final String filterInline = XMLUtilities.prepareInLine( string );
      return filterInline;
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }
}