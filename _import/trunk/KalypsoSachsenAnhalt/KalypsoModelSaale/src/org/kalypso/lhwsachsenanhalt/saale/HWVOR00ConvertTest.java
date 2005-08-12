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
package org.kalypso.lhwsachsenanhalt.saale;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.Writer;
import java.text.ParseException;

import junit.framework.TestCase;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;

public class HWVOR00ConvertTest extends TestCase
{
  /** test readin of .vor file and writing into a csv file. Content is not checked */
  public void testhwvor002zml() throws ParseException, IOException
  {
    final InputStream is = HWVOR00ConvertTest.class.getResourceAsStream( "resources/test/P_dat.vor" );
    final Reader reader = new BufferedReader( new InputStreamReader( is ) );

    final IObservation[] observations = HWVOR00Converter.toZML( TimeserieConstants.TYPE_RAINFALL, reader );
    reader.close();

    final File file = File.createTempFile( "zmlWriterTest", "csv" );
    file.deleteOnExit();

    final Writer out = new BufferedWriter( new FileWriter( "C:\\test.txt" ) );
    final HWVOR00Converter conv = new HWVOR00Converter();
    conv.addObservation( observations[0], "date", "N" );
    conv.addObservation( observations[1], "date", "N" );
    conv.toHWVOR00( out );
    out.close();
    file.delete();
  }
}
