package org.kalypso.convert.update;

import java.io.StringWriter;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.zml.filters.InterpolationFilter;
import org.kalypso.zml.filters.ObjectFactory;

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
    ObjectFactory of = new ObjectFactory();
    InterpolationFilter interpolationFilter = of.createInterpolationFilter();
    interpolationFilter.setAmount( amountHours );
    interpolationFilter.setCalendarField( "HOUR_OF_DAY" );
    interpolationFilter.setDefaultValue( defaultValue );
    interpolationFilter.setDefaultStatus( KalypsoStati.BIT_CHECK );
    interpolationFilter.setForceFill( forceFill );
    Marshaller marshaller = of.createMarshaller();
    StringWriter writer = new StringWriter( 0 );
    marshaller.marshal( interpolationFilter, writer );
    String result = writer.toString();
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
}