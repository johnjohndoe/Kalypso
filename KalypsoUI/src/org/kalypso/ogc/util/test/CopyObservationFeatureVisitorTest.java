/*
 * --------------- Kalypso-Header
 * --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal
 * engineering Denickestr. 22 21073 Hamburg, Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany
 * http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ogc.util.test;

import java.net.MalformedURLException;
import java.net.URL;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.util.CopyObservationFeatureVisitor;
import org.kalypso.util.UrlResolver;

import junit.framework.TestCase;

/**
 * CopyObservationFeatureVisitorTest
 * <p>
 * 
 * this is not actually a test on the CopyObservationFeatureVisitor, but it
 * calls the ZmlFactory in the same way as the CopyObservationFeatureVisitor
 * 
 * created by
 * 
 * @author doemming (14.06.2005)
 *  
 */
public class CopyObservationFeatureVisitorTest extends TestCase
{

  public void testObservationWithComplexFilter() throws Exception
  {
    try
    {
      final String ref = "?"// 
          + "<filter>" // 
          + "  <nOperationFilter operator=\"+\" xmlns=\"filters.zml.kalypso.org\"> " //
          + "    <operationFilter operand=\"0.19914285714285712\" operator=\"*\"> " //
          + "      <zmlFilter> <zml ns1:href=\"Ombrometer_plauen.zml\" xmlns:ns1=\"http://www.w3.org/1999/xlink\"/> </zmlFilter>" //
          + "    </operationFilter>" // 
          + "    <operationFilter operand=\"0.24771428571428575\" operator=\"*\">" //
          + "      <zmlFilter> <zml ns2:href=\"Ombrometer_schleiz.zml\" xmlns:ns2=\"http://www.w3.org/1999/xlink\"/> </zmlFilter> " //
          + "    </operationFilter> " // 
          + "    <operationFilter operand=\"0.5533333333333332\" operator=\"*\"> " // 
          + "      <zmlFilter> <zml ns3:href=\"Ombrometer_gera.zml\" xmlns:ns3=\"http://www.w3.org/1999/xlink\"/> </zmlFilter>" //
          + "    </operationFilter>" // 
          + "  </nOperationFilter>" // 
          + "</filter>" //
          + "<from>1995-08-30T17:00:00</from>" //
          + "<to>1995-09-07T17:00:00</to>";
      // @marc: hier meine Überlegungen:
      //     Problem: wie sollte ein filter aussehen, der in einer href (URL)
      // codiert ist ?
      // <br>
      //      der context muss irgendwie drin sein, da im filter relative pfade
      // verwendet werden können. <br>
      //
      //      bisher:
      // file://DIRa/DIRb/zeitreihe.zml?<from>...</from><to>...</to>
      // kein Problem, der filter wird einfach auf die existierende URL
      // angewendet.
      //
      // nun ist die zeitreihe aber innerhalb des filters und die url wird nur
      // als context gebraucht, z.B.:
      // file://DIRa/DIRb/context.gml?<filer>...<filter>
      // die zeitreihe ergibt sich durch auswertung des filters, da die
      // Datenquelle tief innerhalb des filters ist, die URL muss dazu nicht als
      // stream geparst werden.
      //
      // und wie soll es aussehen wenn beides zusammenkommt ?
      // z.B.:
      // file://DIRa/DIRb/context.gml?<filer>...<filter><from>...</from><to>...</to>
      //      
      // hm, woran erkennt das programm nun, ob es die URL als stream einlesen
      // soll (Zeitreihe aus der URL),
      // oder ob es die url nur als context verwenden soll (Zeitreihe aus
      // filter)?
      // vielleicht sollte erst der Teil nach dem ? in der URL eingelesen werden
      // und dann anhand dem Typ der Filter entschieden werden, ob die URL als
      // Context oder Observation verwendet wird.
      
      
      // Marc@Andreas: es scheint jetzt zu funktionieren! Habe das Konzept von
      // Context im Filter und ZmlFactory Zeug eingeführt.
      // Damit das funktionniert muss der Zml-Link so aussehen:
      // file:context://foo/bar/script.foo?<filter>blablabla</filter>...
      final String strc = getClass().getResource( "contextFake.txt" )
          .toExternalForm();
      final URL context = new URL( strc.replaceAll( "file:", "file:context:" ) );
      final URL sourceURL = new UrlResolver().resolveURL( context, ref );
      final IObservation observation = ZmlFactory.parseXML( sourceURL, "id" );
      assertNotNull( observation );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw e;
    }
  }
}
