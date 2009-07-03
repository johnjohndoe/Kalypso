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
package org.kalypso.convert.namodel.hydrotope;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import junit.framework.TestCase;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.convert.namodel.hydrotope.LanduseImportOperation.InputDescriptor;
import org.kalypso.convert.namodel.schema.binding.LanduseCollection;
import org.kalypso.convert.namodel.schema.binding.LanduseCollection.ImportType;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * Although a unit test, no real testing stuff. Just for developement of hydrotope stuff.
 *
 * @author Gernot Belger
 */
public class HydrotopeMain extends TestCase
{
  public void testImportLanduseFromShape( ) throws Exception
  {
    final File shapeFile = new File( "C:\\Projekte\\plc0822907\\work\\HydrotopVerschneidung\\BeispieldatenKollau\\Gernot\\landnutzung.shp" );
    final InputDescriptor inputDescriptor = new ShapeInputDescriptor( shapeFile, "ALB_NR", "VERSIEGKOR", "ENTWART" );

    final File landuseTemplateFile = new File( "C:\\Eclipse\\runtime-kalypsomodel.product\\DemoModell\\landuse.gml" );
    final GMLWorkspace landuseWorkspace = GmlSerializer.createGMLWorkspace( landuseTemplateFile, null );

    final File landuseClassesFile = new File( "C:\\Eclipse\\runtime-kalypsomodel.product\\DemoModell\\parameter.gml" );
    final GMLWorkspace landuseClassesWorkspace = GmlSerializer.createGMLWorkspace( landuseClassesFile, null );
    final LanduseCollection output = (LanduseCollection) landuseWorkspace.getRootFeature();

    final Map<String, String> landuseClasses = new HashMap<String, String>();
    final List< ? > landuseClassesFeatures = (List< ? >) landuseClassesWorkspace.getRootFeature().getProperty( new QName( NaModelConstants.NS_NAPARAMETER, "landuseMember" ) );
    for( final Object object : landuseClassesFeatures )
    {
      final Feature f = (Feature) object;
      final String name = f.getName();
      final String id = f.getId();

      landuseClasses.put( name, id );
    }

    // call importer
    final LanduseImportOperation op = new LanduseImportOperation( inputDescriptor, output, landuseClasses, ImportType.CLEAR_OUTPUT );
    op.execute( new NullProgressMonitor() );

    final File outputFile = new File( "C:\\temp\\landuse.gml.gz" );
    GmlSerializer.serializeWorkspace( outputFile, landuseWorkspace, "UTF-8" );
  }

}
