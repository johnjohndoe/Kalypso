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

package org.kalypso.raster;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;

import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage2;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * Methods for reading and writing raster data
 * 
 * @author Nadja Peiler (14.06.2005)
 */
public class RasterDataModel
{
  /**
   * creates a rectifiedGridCoverage object of a gml object
   * 
   * @param gmlURL
   *          (Schema: RasterDataModel.xsd)
   * @return RectifiedGridCoverage2
   * @throws Exception
   */
  public RectifiedGridCoverage2 getRectifiedGridCoverage( final URL gmlURL ) throws Exception
  {
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( gmlURL, null );
    return RasterDataModel.getRectifiedGridCoverage( workspace );
  }

  /**
   * creates a rectifiedGridCoverage object of a gml workspace
   * 
   * @param workspace
   * @return RectifiedGridCoverage
   */
  public static RectifiedGridCoverage2 getRectifiedGridCoverage( final GMLWorkspace workspace )
  {
    final Feature rootFeature = workspace.getRootFeature();
    return new RectifiedGridCoverage2( rootFeature );
  }

  /**
   * creates gml workspace of a rectifiedGridCoverage object and serializes it to a file
   * 
   * @param rasterDataGML_OutputFile
   * @param grid
   * @throws InvocationTargetException,
   *           IOException, GmlSerializeException
   */
  public void exportToGML( final File rasterDataGML_OutputFile, final RectifiedGridCoverage2 grid ) throws InvocationTargetException, IOException, GmlSerializeException
  {
    // load schema
    final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
    final GMLSchema schema = schemaCatalog.getSchema( NS.GML3, "3.1" );

    final Feature rootFeature = grid.getFeature();

    // create workspace
    final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( schema, rootFeature, rasterDataGML_OutputFile.toURL(), null, null );

    // serialize Workspace
    final FileWriter fw = new FileWriter( rasterDataGML_OutputFile );
    GmlSerializer.serializeWorkspace( fw, workspace );
    fw.close();
  }

}