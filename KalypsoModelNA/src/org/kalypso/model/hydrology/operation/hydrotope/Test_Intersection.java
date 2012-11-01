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
package org.kalypso.model.hydrology.operation.hydrotope;

import java.io.File;
import java.util.Date;
import java.util.List;

import junit.framework.TestCase;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.junit.Ignore;
import org.kalypso.model.hydrology.binding.Geology;
import org.kalypso.model.hydrology.binding.GeologyCollection;
import org.kalypso.model.hydrology.binding.HydrotopeCollection;
import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.model.hydrology.binding.Landuse;
import org.kalypso.model.hydrology.binding.LanduseCollection;
import org.kalypso.model.hydrology.binding.OverlayCollection;
import org.kalypso.model.hydrology.binding.SoilType;
import org.kalypso.model.hydrology.binding.SoilTypeCollection;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.IXLinkedFeature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Polygon;

/**
 * @author Dejan Antanaskovic
 */
@Ignore
public class Test_Intersection extends TestCase
{
  public void test( ) throws Exception
  {
    final File catchmentGML = new File( "P:\\FE_Projekte\\2009_PlanerClient\\03_Modelle\\031_Kollau_Modelle\\Testing_02032010\\Gesamtmodell\\01_PLC_Kollau_NA_Gesamt_\\modell.gml" ); //$NON-NLS-1$
    final File landuseGML = new File( "P:\\FE_Projekte\\2009_PlanerClient\\03_Modelle\\031_Kollau_Modelle\\Testing_02032010\\Gesamtmodell\\01_PLC_Kollau_NA_Gesamt_\\landuse.gml" ); //$NON-NLS-1$
    final File pedologyGML = new File( "P:\\FE_Projekte\\2009_PlanerClient\\03_Modelle\\031_Kollau_Modelle\\Testing_02032010\\Gesamtmodell\\01_PLC_Kollau_NA_Gesamt_\\pedologie.gml" ); //$NON-NLS-1$
    final File geologyGML = new File( "P:\\FE_Projekte\\2009_PlanerClient\\03_Modelle\\031_Kollau_Modelle\\Testing_02032010\\Gesamtmodell\\01_PLC_Kollau_NA_Gesamt_\\geologie.gml" ); //$NON-NLS-1$
    final File overlayGML = new File( "P:\\FE_Projekte\\2009_PlanerClient\\03_Modelle\\031_Kollau_Modelle\\Testing_02032010\\Gesamtmodell\\01_PLC_Kollau_NA_Gesamt_\\overlay.gml" ); //$NON-NLS-1$

    final File template = new File( "P:\\FE_Projekte\\2009_PlanerClient\\03_Modelle\\031_Kollau_Modelle\\Testing_02032010\\Gesamtmodell\\01_PLC_Kollau_NA_Gesamt_\\hydrotop.gml" ); //$NON-NLS-1$
    final File outputGML = new File( "d:\\temp\\__test_output_" + new Date().getTime() + ".gml" ); //$NON-NLS-1$ //$NON-NLS-2$
    if( outputGML.exists() )
      outputGML.delete();

    final GMLWorkspace catchmentWS = GmlSerializer.createGMLWorkspace( catchmentGML, null );
    final GMLWorkspace landuseWS = GmlSerializer.createGMLWorkspace( landuseGML, null );
    final GMLWorkspace pedologyWS = GmlSerializer.createGMLWorkspace( pedologyGML, null );
    final GMLWorkspace geologyWS = GmlSerializer.createGMLWorkspace( geologyGML, null );
    final GMLWorkspace overlayWS = GmlSerializer.createGMLWorkspace( overlayGML, null );

    final GMLWorkspace outputWS = GmlSerializer.createGMLWorkspace( template, null );

    final NaModell naModel = (NaModell)catchmentWS.getRootFeature();

    final LanduseCollection landuseRoot = (LanduseCollection)landuseWS.getRootFeature();
    final SoilTypeCollection pedology = (SoilTypeCollection)pedologyWS.getRootFeature();
    final GeologyCollection geologyRoot = (GeologyCollection)geologyWS.getRootFeature();
    final OverlayCollection overlay = (OverlayCollection)overlayWS.getRootFeature();

    final HydrotopeCollection naHydrotopes = (HydrotopeCollection)outputWS.getRootFeature();

    final HydrotopeInputIndexer indexer = new HydrotopeInputIndexer( "indexer" ); //$NON-NLS-1$
    indexer.addInput( new CatchmentHydrotopeInput( naModel ) );
    indexer.addInput( new LanduseHydrotopeInput( landuseRoot ) );
    indexer.addInput( new PedologyHydrotopeInput( pedology ) );
    indexer.addInput( new GeologyHydrotopeInput( geologyRoot ) );
    indexer.addInput( new OverlayHydrotopeInput( overlay, naModel ) );

    indexer.execute( new NullProgressMonitor() );
    final IHydrotopeInput[] input = indexer.getIndices();

    final FeatureListGeometryIntersector geometryIntersector = new FeatureListGeometryIntersector( input, "test" ); //$NON-NLS-1$

    geometryIntersector.execute( new NullProgressMonitor() );

    final IFeatureBindingCollection<IHydrotope> hydrotopes = naHydrotopes.getHydrotopes();
    final IFeatureBindingCollection<Catchment> catchments = naModel.getCatchments();

    final List<Polygon> intersectionList = geometryIntersector.getResult();
    for( final Geometry geometry : intersectionList )
    {
      final IHydrotope hydrotop = hydrotopes.addNew( IHydrotope.FEATURE_HYDROTOPE );
      final GM_Envelope envelope = JTSAdapter.wrap( geometry.getInteriorPoint().getEnvelopeInternal(), KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );
      final GM_Point point = (GM_Point)JTSAdapter.wrap( geometry.getInteriorPoint() );

      final List<Catchment> catchmentList = catchments.query( envelope );
      Catchment catchment = null;
      if( catchmentList.size() == 0 )
        continue;
      else
      {
        for( final Catchment object : catchmentList )
        {
          if( object.getDefaultGeometryPropertyValue().contains( point ) )
          {
            catchment = object;
            break;
          }
        }
        if( catchment == null )
          continue;
      }

      final List<Landuse> landuseList = landuseRoot.getLanduses().query( envelope );
      if( landuseList.size() > 0 )
      {
        Landuse landuse = null;
        for( final Landuse l : landuseList )
        {
          if( l.getDefaultGeometryPropertyValue().contains( point ) )
          {
            landuse = l;
            break;
          }
        }
        if( landuse == null )
          continue;
        final Object landuseClassLink = landuse.getLanduse();
        String value = ""; //$NON-NLS-1$
        if( landuseClassLink instanceof IXLinkedFeature )
          value = ((IXLinkedFeature)landuseClassLink).getFeatureId();
        else
          value = landuseClassLink.toString().substring( landuseClassLink.toString().indexOf( "#" ) + 1 ); //$NON-NLS-1$
        hydrotop.setLanduse( value );
        final double corrSealing = landuse.getCorrSealing() * catchment.getCorrSealing();
        hydrotop.setCorrSealing( corrSealing );
      }
      else
        continue;

      final List<SoilType> soilTypesList = pedology.getSoilTypes().query( envelope );
      if( soilTypesList.size() > 0 )
      {
        SoilType soilType = null;
        for( final SoilType s : soilTypesList )
        {
          if( s.getDefaultGeometryPropertyValue().contains( point ) )
          {
            soilType = s;
            break;
          }
        }

        if( soilType == null )
          continue;

        final Object soiltypeClassLink = soilType.getSoilType();
        String value = ""; //$NON-NLS-1$
        if( soiltypeClassLink instanceof IXLinkedFeature )
          value = ((IXLinkedFeature)soiltypeClassLink).getFeatureId();
        else
          value = soiltypeClassLink.toString().substring( soiltypeClassLink.toString().indexOf( "#" ) + 1 ); //$NON-NLS-1$

        hydrotop.setSoilType( value );
      }
      else
        continue;

      final List<Geology> geologyList = geologyRoot.getGeologies().query( envelope );
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
        hydrotop.setMaxPerkolationRate( geology.getMaxPercolationRate() );
        hydrotop.setGWFactor( geology.getGWFactor() );
      }
      else
        continue;

      final MultiPolygon multiPolygon = JTSAdapter.jtsFactory.createMultiPolygon( new Polygon[] { (Polygon)geometry } );
      hydrotop.setGeometry( (GM_MultiSurface)JTSAdapter.wrap( multiPolygon ) );
    }

    GmlSerializer.serializeWorkspace( outputGML, outputWS, "UTF-8" ); //$NON-NLS-1$
  }

}
