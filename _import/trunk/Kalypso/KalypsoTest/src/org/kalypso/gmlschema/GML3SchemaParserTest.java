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
package org.kalypso.gmlschema;

import java.io.File;
import java.net.URL;

import junit.framework.TestCase;

import org.kalypso.KalypsoTest;
import org.kalypso.gmlschema.basics.GMLSchemaLabelProvider;
import org.kalypso.gmlschema.basics.GMLSchemaTreeContentProvider;
import org.kalypso.gmlschema.basics.GmlTreePrintVisitor;
import org.kalypso.gmlschema.basics.ITreeContentProviderVisitor;

/**
 * @author doemming
 */
public class GML3SchemaParserTest extends TestCase
{
  /**
   * @see junit.framework.TestCase#setUp()
   */
  @Override
  protected void setUp( ) throws Exception
  {
    KalypsoTest.init();
  }

  public void testParseGML3Schema( ) throws Exception
  {
    try
    {
      // final URL gml3SchemaArchiveURL = new URL( "http://schemas.opengis.net/gml/3.1.1/base/gml.xsd" );
      // final GMLSchema gmlSchema = GMLSchemaCatalog.getSchema( "http://www.opengis.net/gml" );
      // gmlSchema.getGMLVersion();

      // final GMLSchema gmlSchema = GMLSchemaCatalog.getSchema( "http://www.opengis.net/swe" );
      // final GMLSchema gmlSchema = GMLSchemaCatalog.getSchema( "http://www.opengis.net/om" );
      final URL schemaLocationURL = (new File( "C:/eclipse3.1_workspace/KalypsoModelEindim/src/org/kalypso/model/wspm/schemata/wspm.xsd" )).toURL();
      final URL testResourceURL = getClass().getResource( "resources/GML3_wspm/schematree.txt" );

      GMLSchemaTest.loadAndTestSchema( schemaLocationURL, testResourceURL, false );

      // final GMLSchema gmlSchema = GMLSchemaCatalog.getSchema( "org.kalypso.model.wspm" );
      // gmlSchema.getGMLVersion();
      //
      // final StringBuffer buffer = new StringBuffer();
      // final ITreeContentProviderVisitor visitor = new GmlTreePrintVisitor( new GMLSchemaLabelProvider(), buffer );
      // final GMLSchemaTreeContentProvider provider = new GMLSchemaTreeContentProvider( gmlSchema, true );
      // provider.accept( gmlSchema, visitor, 0 );
      // System.out.println( buffer.toString() );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw e;
    }
  }
}
