package org.kalypso.risk.test;

/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;

import javax.xml.namespace.QName;

import junit.framework.TestCase;
import ogc31.www.opengis.net.gml.FileType;
import ogc31.www.opengis.net.gml.RangeSetType;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.ogc31.KalypsoOGC31JAXBcontext;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.grid.ConvertAscii2Binary;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.grid.processes.raster2vector.Raster2Lines;
import org.kalypso.grid.processes.raster2vector.Raster2LinesWalkingStrategy;
import org.kalypso.grid.processes.raster2vector.SegmentCollector;
import org.kalypso.grid.processes.raster2vector.collector.CollectorDataProvider;
import org.kalypso.grid.processes.raster2vector.collector.LineStringCollector;
import org.kalypso.grid.processes.raster2vector.collector.PolygonCollector;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypsodeegree.KalypsoDeegreeDebug;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.gml.binding.commons.CoverageCollection;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridCoverage;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;
import org.kalypsodeegree_impl.io.shpapi.ShapeConst;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * JUnit Test Case for converting a grid into line and polygon shape.<br>
 * This test extracts demo input data (grid) from resources and converts them into shape files. <br>
 * <br>
 * Run this test as plug-in test.
 * 
 * @author Thomas Jung
 */
public class TestGrid2Shp extends TestCase
{

  private static final GeometryFactory GF = new GeometryFactory();

  public void testRiskModel( ) throws Exception
  {
    // unzip test data into workspace
    final IWorkspace workspace = ResourcesPlugin.getWorkspace();
    final IProject project = workspace.getRoot().getProject( "GridConvertingTest" );
    project.create( new NullProgressMonitor() );

    final URL zipLocation = getClass().getResource( "resources/grids.zip" );
    ZipUtilities.unzip( zipLocation, project, new NullProgressMonitor() );

    // run test model
    final IFolder importDataFolder = project.getFolder( "grids" );

    final IGeoGrid raster = getTestGrid( importDataFolder );

    final boolean bSimple = false;
    final boolean bIsolines = false;

    final double[] grenzen = new double[] { 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.4, 1.5 };

    final SegmentCollector collector = bIsolines ? (SegmentCollector) new LineStringCollector( GF, grenzen, bSimple ) : new PolygonCollector( GF, grenzen, bSimple );
    final Raster2Lines r2l = new Raster2Lines( collector, grenzen );

    new Raster2LinesWalkingStrategy().walk( raster, r2l, null, new NullProgressMonitor() );

    final CollectorDataProvider[] data = collector.getData();

    // Shape schreiben
    if( collector instanceof PolygonCollector )
      writePolygonShape( data, importDataFolder );
    else
      writeLineShape( data, importDataFolder );

  }

  private void writeLineShape( final CollectorDataProvider[] data, final IFolder importDataFolder ) throws Exception, GmlSerializeException
  {
    /* Create feature type which describes what data the shape file contains */
    final ITypeRegistry<IMarshallingTypeHandler> typeRegistry = MarshallingTypeRegistrySingleton.getTypeRegistry();

    final IMarshallingTypeHandler doubleTypeHandler = typeRegistry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "double" ) );
    final IMarshallingTypeHandler stringTypeHandler = typeRegistry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "string" ) );
    final IMarshallingTypeHandler lineTypeHandler = typeRegistry.getTypeHandlerForTypeName( GeometryUtilities.QN_LINE_STRING_PROPERTY );

    final QName shapeTypeQName = new QName( "anyNS", "shapeType" );

    final IValuePropertyType doubleType = GMLSchemaFactory.createValuePropertyType( shapeTypeQName, new QName( "anyNS", "id" ), doubleTypeHandler, 1, 1, false );
    final IValuePropertyType stringType = GMLSchemaFactory.createValuePropertyType( shapeTypeQName, new QName( "anyNS", "name" ), stringTypeHandler, 1, 1, false );
    final IValuePropertyType lineType = GMLSchemaFactory.createValuePropertyType( shapeTypeQName, new QName( "anyNS", "aGeometry" ), lineTypeHandler, 1, 1, false );

    final IPropertyType[] properties = new IPropertyType[] { lineType, doubleType, stringType };
    final IFeatureType shapeFT = GMLSchemaFactory.createFeatureType( shapeTypeQName, properties );

    /* Create the shape root feature, we need it to create the children. */
    final Feature shapeRootFeature = ShapeSerializer.createWorkspaceRootFeature( shapeFT, ShapeConst.SHAPE_TYPE_POLYLINEZ );
    final GMLWorkspace shapeWorkspace = shapeRootFeature.getWorkspace();
    final IRelationType shapeParentRelation = (IRelationType) shapeRootFeature.getFeatureType().getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );

    for( int i = 0; i < data.length; i++ )
    {
      final GM_Curve line = (GM_Curve) data[i].getGeometry();
      final Double id = data[i].getId();
      final String[] name = data[i].getName();

      final Object[] shapeData = new Object[] { line, id, name[0] };
      final Feature feature = FeatureFactory.createFeature( shapeRootFeature, shapeParentRelation, "FeatureID" + i, shapeFT, shapeData );
      shapeWorkspace.addFeatureAsComposition( shapeRootFeature, shapeParentRelation, -1, feature );
    }

    final String shapeBase = importDataFolder.getLocation() + "export_isoline";
    ShapeSerializer.serialize( shapeWorkspace, shapeBase, null );
  }

  private void writePolygonShape( final CollectorDataProvider[] data, final IFolder importDataFolder ) throws Exception, GmlSerializeException
  {
    /* Create feature type which describes what data the shape file contains */
    final ITypeRegistry<IMarshallingTypeHandler> typeRegistry = MarshallingTypeRegistrySingleton.getTypeRegistry();

    final IMarshallingTypeHandler doubleTypeHandler = typeRegistry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "double" ) );
    final IMarshallingTypeHandler stringTypeHandler = typeRegistry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "string" ) );
    final IMarshallingTypeHandler polygonTypeHandler = typeRegistry.getTypeHandlerForTypeName( GeometryUtilities.QN_POLYGON_PROPERTY );

    final QName shapeTypeQName = new QName( "anyNS", "shapeType" );

    final IValuePropertyType doubleTypeId = GMLSchemaFactory.createValuePropertyType( shapeTypeQName, new QName( "anyNS", "id" ), doubleTypeHandler, 1, 1, false );
    final IValuePropertyType doubleTypeFrom = GMLSchemaFactory.createValuePropertyType( shapeTypeQName, new QName( "anyNS", "from" ), doubleTypeHandler, 1, 1, false );
    final IValuePropertyType doubleTypeTo = GMLSchemaFactory.createValuePropertyType( shapeTypeQName, new QName( "anyNS", "to" ), doubleTypeHandler, 1, 1, false );
    final IValuePropertyType stringTypeRange = GMLSchemaFactory.createValuePropertyType( shapeTypeQName, new QName( "anyNS", "range" ), stringTypeHandler, 1, 1, false );
    final IValuePropertyType polygonType = GMLSchemaFactory.createValuePropertyType( shapeTypeQName, new QName( "anyNS", "geometry" ), polygonTypeHandler, 1, 1, false );

    final IPropertyType[] properties = new IPropertyType[] { polygonType, doubleTypeId, doubleTypeFrom, doubleTypeTo, stringTypeRange };
    final IFeatureType shapeFT = GMLSchemaFactory.createFeatureType( shapeTypeQName, properties );

    /* Create the shape root feature, we need it to create the children. */
    final Feature shapeRootFeature = ShapeSerializer.createWorkspaceRootFeature( shapeFT, ShapeConst.SHAPE_TYPE_POLYGONZ );
    final GMLWorkspace shapeWorkspace = shapeRootFeature.getWorkspace();
    final IRelationType shapeParentRelation = (IRelationType) shapeRootFeature.getFeatureType().getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );

    for( int i = 0; i < data.length; i++ )
    {
      final GM_Surface< ? > line = (GM_Surface< ? >) data[i].getGeometry();
      final Double id = data[i].getId();
      final Double[] borders = data[i].getBorders();
      final Double from = borders[0];
      final Double to = borders[1];
      final String[] name = data[i].getName();

      final Object[] shapeData = new Object[] { line, id, from, to, name[0] };
      final Feature feature = FeatureFactory.createFeature( shapeRootFeature, shapeParentRelation, "FeatureID" + i, shapeFT, shapeData );
      shapeWorkspace.addFeatureAsComposition( shapeRootFeature, shapeParentRelation, -1, feature );
    }

    final String shapeBase = importDataFolder.getLocation() + "export_polygon";
    ShapeSerializer.serialize( shapeWorkspace, shapeBase, null );
  }

  private IGeoGrid getTestGrid( final IFolder importDataFolder ) throws Exception, MalformedURLException, IOException
  {
    // load models
    KalypsoDeegreeDebug.OPERATION.printf( "%s", "importiere Grid...\n" );

    final IFile covCollFile = importDataFolder.getFile( new Path( "covColl.gml" ) ); //$NON-NLS-1$
    final GMLWorkspace covCollWorkspace = GmlSerializer.createGMLWorkspace( ResourceUtilities.createURL( covCollFile ), null );

    final IFile file = importDataFolder.getFile( "wsp_hq100.asc" );

    final String binFileName = file.getName() + ".bin";
    final String dstFileName = binFileName;
    final IFile dstRasterIFile = importDataFolder.getFile( dstFileName );
    final File dstRasterFile = dstRasterIFile.getRawLocation().toFile();
    final RectifiedGridDomain gridDomain = importAsBinaryRaster( file.getLocation().toFile(), dstRasterFile, "EPSG:31467", new NullProgressMonitor() );
    if( gridDomain != Status.OK_STATUS )
      KalypsoDeegreeDebug.OPERATION.printf( "%s", "Import fehlgeschlagen...\n" );
    else
      KalypsoDeegreeDebug.OPERATION.printf( "%s", "Import erfolgreich...\n" );

    final IFeatureType ft = covCollWorkspace.getGMLSchema().getFeatureType( RectifiedGridCoverage.QNAME );
    final Feature rootFeature = covCollWorkspace.getRootFeature();
    ICoverageCollection covColl = (ICoverageCollection) rootFeature.getAdapter( ICoverageCollection.class );

    final IRelationType parentRelation = (IRelationType) covColl.getFeature().getFeatureType().getProperty( CoverageCollection.QNAME_PROP_COVERAGE_MEMBER );
    final Feature coverageFeature = covCollWorkspace.createFeature( covColl.getFeature(), parentRelation, ft );
    final RectifiedGridCoverage coverage = new RectifiedGridCoverage( coverageFeature );
    covColl.add( coverage );

    final FileType rangeSetFile = KalypsoOGC31JAXBcontext.GML3_FAC.createFileType();
    rangeSetFile.setFileName( binFileName ); //$NON-NLS-1$
    rangeSetFile.setMimeType( "image/bin" ); //$NON-NLS-1$
    final RangeSetType rangeSet = KalypsoOGC31JAXBcontext.GML3_FAC.createRangeSetType();
    rangeSet.setFile( rangeSetFile );

    covColl.add( coverage );
    coverage.setRangeSet( rangeSet );
    coverage.setGridDomain( gridDomain );
    coverage.setName( binFileName );
    coverage.setDescription( "ASCII-Import" );

    KalypsoDeegreeDebug.OPERATION.printf( "%s", "importiere Grid...\n" );

    return GeoGridUtilities.toGrid( coverage );

  }

  private static RectifiedGridDomain importAsBinaryRaster( final File srcFile, final File dstFile, final String sourceCRS, final IProgressMonitor monitor ) throws IOException
  {
    final ConvertAscii2Binary ascii2Binary = new ConvertAscii2Binary( srcFile.toURL(), dstFile, 2, sourceCRS );
    ascii2Binary.doConvert( monitor );
    final RectifiedGridDomain gridDomain = ascii2Binary.getGridDomain();
    return gridDomain;
  }

}
