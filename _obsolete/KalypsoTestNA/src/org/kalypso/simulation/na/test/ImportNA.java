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
package org.kalypso.simulation.na.test;

import java.io.File;
import java.io.FileWriter;

import junit.framework.TestCase;

import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.NAModellConverter;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;

/**
 * ImportNA
 * <p>
 * created by
 * 
 * @author doemming (24.05.2005)
 */
public class ImportNA extends TestCase
{
  public void testImport( ) throws Exception
  {
    final File asciiBaseDir = new File( "C:\\TMP\\na" );
    final File gmlBaseDir = new File( "C:\\TMP\\import" );
    if( !gmlBaseDir.exists() )
      gmlBaseDir.mkdirs();
    importParameter( gmlBaseDir, asciiBaseDir );
//    NAModellConverter.completeascii2gml(gmlBaseDir, asciiBaseDir );
  }

  public void importParameter( File gmlBaseDir, File asciiBaseDir ) throws Exception
  {
    final File parameterGmlFile = new File( gmlBaseDir, "parameter.gml" );
    final NAConfiguration ascii2GmlConfiguration = NAConfiguration.getAscii2GmlConfiguration( asciiBaseDir, gmlBaseDir );
    final Feature parameterRootFeature = NAModellConverter.parameterAsciiToFeature( ascii2GmlConfiguration );
    final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
    final GMLSchema paraGmlSchema = schemaCatalog.getSchema( NaModelConstants.NS_NAPARAMETER, (String)null );
    final GMLWorkspace paraWorkspace = new GMLWorkspace_Impl( paraGmlSchema, paraGmlSchema.getAllFeatureTypes(), parameterRootFeature, null, null, " project:/.model/schema/parameter.xsd", null );
    GmlSerializer.serializeWorkspace( new FileWriter( parameterGmlFile ), paraWorkspace );
    System.out.println( "Die parameter.gml Datei befindet sich unter: " + parameterGmlFile.getPath() );
  }
}
