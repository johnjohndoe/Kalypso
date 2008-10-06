/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ogc.sensor.timeseries.envelope;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.util.Calendar;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.factory.FactoryException;
import org.kalypso.contribs.java.xml.XMLUtilities;
import org.kalypso.jwsdp.JaxbUtilities;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.zml.Observation;
import org.kalypso.zml.filters.ObjectFactory;
import org.kalypso.zml.filters.TranProLinFilterType;

/**
 * @author doemming
 */
public class TranProLinFilterUtilities
{
  public static void transformAndWrite( final IObservation baseObservation, final Calendar dateBegin, final Calendar dateEnd, final double operandBegin, final double operandEnd, final String operator, final String axisTypes, int statusToMerge, final File resultFile, String sufix ) throws SensorException, JAXBException, FactoryException, UnsupportedEncodingException, FileNotFoundException
  {
    if( resultFile == null )
      return; // nothing to do
    final ObjectFactory fac = new ObjectFactory();
    final JAXBContext jc = JaxbUtilities.createQuiet( ObjectFactory.class );
    final TranProLinFilterType filter = fac.createTranProLinFilterType();
    
    filter.setDateBegin( dateBegin );
    filter.setDateEnd( dateEnd );
    filter.setOperandBegin( operandBegin );
    filter.setOperandEnd( operandEnd );
    filter.setOperator( operator );
    filter.setStatusToMerge( statusToMerge );
    filter.setAxisTypes( axisTypes );
    final StringWriter stringWriter = new StringWriter();
    
    final Marshaller marshaller = JaxbUtilities.createMarshaller(jc);
    marshaller.marshal( filter, stringWriter );
    final String string = XMLUtilities.removeXMLHeader( stringWriter.toString() );
    final String filterInline = XMLUtilities.prepareInLine( string );
    final IObservation resultObservation = ZmlFactory.decorateObservation( baseObservation, filterInline, null );
    // write ZML
    final IRequest request = new ObservationRequest( dateBegin.getTime(), dateEnd.getTime() );
    final Observation observationType = ZmlFactory.createXML( resultObservation, request );
    String name = observationType.getName();
    if( name == null )
      name = "";
    observationType.setName( name + sufix );
    final Marshaller m = ZmlFactory.getMarshaller();
    m.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
    FileOutputStream stream = null;
    OutputStreamWriter writer = null;
    try
    {
      stream = new FileOutputStream( resultFile );
      writer = new OutputStreamWriter( stream, "UTF-8" );
      m.marshal( observationType, writer );
    }
    finally
    {
      IOUtils.closeQuietly( writer );
      IOUtils.closeQuietly( stream );
    }
  }
}
