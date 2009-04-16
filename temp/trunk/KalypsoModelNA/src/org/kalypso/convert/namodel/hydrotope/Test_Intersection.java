/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
import java.util.Date;
import java.util.List;

import junit.framework.TestCase;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.convert.namodel.FeatureListGeometryIntersector;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.convert.namodel.schema.binding.Geology;
import org.kalypso.convert.namodel.schema.binding.GeologyCollection;
import org.kalypso.convert.namodel.schema.binding.Landuse;
import org.kalypso.convert.namodel.schema.binding.LanduseCollection;
import org.kalypso.convert.namodel.schema.binding.SoilType;
import org.kalypso.convert.namodel.schema.binding.SoilTypeCollection;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.MultiPolygon;

/**
 * @author Dejan Antanaskovic
 */
public class Test_Intersection extends TestCase
{
  public void test( ) throws Exception
  {
    final File catchmentGML = new File( "D:\\eclipse_runtime_Connector\\01-Kollau-NA-PlanerClient\\modell.gml" );
    final File landuseGML = new File( "D:\\eclipse_runtime_Connector\\01-Kollau-NA-PlanerClient\\landuse.gml" );
    final File pedologyGML = new File( "D:\\eclipse_runtime_Connector\\01-Kollau-NA-PlanerClient\\pedologie.gml" );
    final File geologyGML = new File( "D:\\eclipse_runtime_Connector\\01-Kollau-NA-PlanerClient\\geologie.gml" );

    final File template = new File( "D:\\eclipse_runtime_Connector\\01-Kollau-NA-PlanerClient\\hydrotop.gml" );
// final File template = new File( "D:\\eclipse_runtime_Connector\\01-Kollau-NA-PlanerClient\\template.gml" );
    final File outputGML = new File( "D:\\eclipse_runtime_Connector\\01-Kollau-NA-PlanerClient\\__test_output_" + new Date().getTime() + ".gml" );
    if( outputGML.exists() )
      outputGML.delete();
    outputGML.createNewFile();

    final GMLWorkspace catchmentWS = GmlSerializer.createGMLWorkspace( catchmentGML, null );
    final GMLWorkspace landuseWS = GmlSerializer.createGMLWorkspace( landuseGML, null );
    final GMLWorkspace pedologyWS = GmlSerializer.createGMLWorkspace( pedologyGML, null );
    final GMLWorkspace geologyWS = GmlSerializer.createGMLWorkspace( geologyGML, null );

    final GMLWorkspace outputWS = GmlSerializer.createGMLWorkspace( template, null );

    final Feature catchmentCollection = (Feature) catchmentWS.getRootFeature().getProperty( NaModelConstants.CATCHMENT_COLLECTION_MEMBER_PROP );
    final FeatureList catchmentFeatureList = (FeatureList) catchmentCollection.getProperty( NaModelConstants.CATCHMENT_MEMBER_PROP );
    final FeatureList landuseFeatureList = (FeatureList) landuseWS.getRootFeature().getProperty( LanduseCollection.QNAME_PROP_LANDUSEMEMBER );
    final FeatureList soilTypesFeatureList = (FeatureList) pedologyWS.getRootFeature().getProperty( SoilTypeCollection.QNAME_PROP_SOILTYPEMEMBER );
    final FeatureList geologiesFeatureList = (FeatureList) geologyWS.getRootFeature().getProperty( GeologyCollection.QNAME_PROP_GEOLOGYMEMBER );

    final FeatureList hydrotopFeatureList = (FeatureList) outputWS.getRootFeature().getProperty( NaModelConstants.HYDRO_MEMBER );
    final IFeatureType typeHydrotop = outputWS.getGMLSchema().getFeatureType( NaModelConstants.HYDRO_ELEMENT_FT );

    final FeatureListGeometryIntersector geometryIntersector = new FeatureListGeometryIntersector();
    geometryIntersector.addFeatureList( catchmentFeatureList );
    geometryIntersector.addFeatureList( soilTypesFeatureList );
    geometryIntersector.addFeatureList( geologiesFeatureList );
    geometryIntersector.addFeatureList( landuseFeatureList );
    final List<MultiPolygon> intersectionList = geometryIntersector.intersect( new NullProgressMonitor() );

    hydrotopFeatureList.clear();
    for( final Geometry geometry : intersectionList )
    {
      final Feature feature = outputWS.createFeature( null, null, typeHydrotop );
      final GM_Envelope envelope = JTSAdapter.wrap( geometry.getInteriorPoint().getEnvelopeInternal() );
      final GM_Point point = (GM_Point) JTSAdapter.wrap( geometry.getInteriorPoint() );

      final List<Object> catchmentList = catchmentFeatureList.query( envelope, null );
      if( catchmentList.size() == 0 )
        continue;
      else
      {
        boolean catchmentFound = false;
        for( final Object object : catchmentList )
          if( ((Feature) object).getDefaultGeometryPropertyValue().contains( point ) )
          {
            catchmentFound = true;
            break;
          }
        if( !catchmentFound )
          continue;
      }

      final List<Landuse> landuseList = landuseFeatureList.query( envelope, null );
      if( landuseList.size() > 0 )
      {
        Landuse landuse = null;
        for( final Landuse l : landuseList )
          if( l.getDefaultGeometryPropertyValue().contains( point ) )
          {
            landuse = l;
            break;
          }
        if( landuse == null )
          continue;
        final Object landuseClassLink = landuse.getLanduse();
        String value = "";
        if( landuseClassLink instanceof XLinkedFeature_Impl )
          value = ((XLinkedFeature_Impl) landuseClassLink).getFeatureId();
        else
          value = landuseClassLink.toString().substring( landuseClassLink.toString().indexOf( "#" ) + 1 );
        feature.setProperty( NaModelConstants.HYDRO_PROP_LANDUSE_NAME, value );
        feature.setProperty( NaModelConstants.HYDRO_PROP_DAINAGETYPE, landuse.getDrainageType() );
        feature.setProperty( NaModelConstants.HYDRO_PROP_SEAL_CORR_FACTOR, landuse.getCorrSealing() );
      }
      else
        continue;

      final List<SoilType> soilTypesList = soilTypesFeatureList.query( envelope, null );
      if( soilTypesList.size() > 0 )
      {
        SoilType soilType = null;
        for( final SoilType s : soilTypesList )
          if( s.getDefaultGeometryPropertyValue().contains( point ) )
          {
            soilType = s;
            break;
          }
        if( soilType == null )
          continue;
        final Object soiltypeClassLink = soilType.getSoilType();
        String value = "";
        if( soiltypeClassLink instanceof XLinkedFeature_Impl )
          value = ((XLinkedFeature_Impl) soiltypeClassLink).getFeatureId();
        else
          value = soiltypeClassLink.toString().substring( soiltypeClassLink.toString().indexOf( "#" ) + 1 );
        feature.setProperty( NaModelConstants.HYDRO_PROP_SOILTYPE, value );
      }
      else
        continue;

      final List<Geology> geologyList = geologiesFeatureList.query( envelope, null );
      if( geologyList.size() > 0 )
      {
        Geology geology = null;
        for( final Geology g : geologyList )
          if( g.getDefaultGeometryPropertyValue().contains( point ) )
          {
            geology = g;
            break;
          }
        if( geology == null )
          continue;
        feature.setProperty( NaModelConstants.HYDRO_PROP_MAXPERCOLATIONSRATE, geology.getMaxPerkulationsRate() );
        feature.setProperty( NaModelConstants.HYDRO_PROP_INFLOWRATEGW, geology.getGWFactor() );
      }
      else
        continue;

      feature.setProperty( NaModelConstants.HYDRO_PROP_GEOM, JTSAdapter.wrap( geometry ) );
      feature.setProperty( NaModelConstants.HYDRO_PROP_AREA, geometry.getArea() );

      hydrotopFeatureList.add( feature );
    }

    GmlSerializer.serializeWorkspace( outputGML, outputWS, "UTF-8" );
  }

}
