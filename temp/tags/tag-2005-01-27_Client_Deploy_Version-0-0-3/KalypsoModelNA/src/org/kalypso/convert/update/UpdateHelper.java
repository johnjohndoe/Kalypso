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

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.zml.filters.InterpolationFilter;
import org.kalypso.zml.filters.ObjectFactory;
import org.kalypso.zml.filters.WqFilter;

/**
 * @author doemming
 */
public class UpdateHelper
{
  //  private final static String baseInterpolation =
  // "?<filter><interpolationFilter xmlns=\"filters.zml.kalypso.org\" "
  //      + "calendarField=\"HOUR_OF_DAY\" "
  //      + "amount=\"3\" "
  //      + "forceFill=\"true\" "
  //      + "defaultValue=\"0.0\"/></filter>";

  public static String createInterpolationFilter( int amountHours, double defaultValue,
      boolean forceFill ) throws JAXBException
  {
    final ObjectFactory of = new ObjectFactory();
    InterpolationFilter interpolationFilter = of.createInterpolationFilter();
    interpolationFilter.setAmount( amountHours );
    interpolationFilter.setCalendarField( "HOUR_OF_DAY" );
    interpolationFilter.setDefaultValue( defaultValue );
    interpolationFilter.setDefaultStatus( KalypsoStati.BIT_CHECK );
    interpolationFilter.setForceFill( forceFill );
    Marshaller marshaller = of.createMarshaller();
    StringWriter writer = new StringWriter( 0 );
    marshaller.marshal( interpolationFilter, writer );
    final String result = writer.toString();
    return prepareInLineFilter( result );
  }

  private static String prepareInLineFilter( String xmlString )
  {
    String result = xmlString.replaceAll( "\n", "" );
    result = removeXMLHeader( result );
    return "<filter>" + result + "</filter>";
  }

  private static String removeXMLHeader( String xmlString )
  {
    return xmlString.replaceFirst( "<\\?.+?\\?>", "" );
  }

  public static String createWQFilter()
  {
    try
    {
      ObjectFactory of = new ObjectFactory();
      WqFilter wqFilter = of.createWqFilter();
      wqFilter.setType( "Q" );
      Marshaller marshaller = of.createMarshaller();
      StringWriter writer = new StringWriter( 0 );
      marshaller.marshal( wqFilter, writer );
      String result = writer.toString();
      return prepareInLineFilter( result );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }
}