/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.updatemodel.kollau;

import java.io.File;
import java.io.FileWriter;
import java.io.OutputStreamWriter;
import java.net.URL;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import junit.framework.TestCase;

import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.convert.namodel.timeseries.NAZMLGenerator;
import org.kalypso.convert.update.WeisseElsterConstants;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Surface;

/**
 * @author huebsch here are moethodes used for preparing the kollau modell
 */
public class UpdateModellKollau extends TestCase
{
  // private final URL m_modellURL;

  public final static String PSI_PROGNOSE_SUFFIX = ".P1_MW";

  public final static String KollauPREFIX_LINK_N_LOKAL = "Niederschlag/Ombrometer_";// ...+.zml

  public final static String KollauPREFIX_LINK_N_REPSITORY = "project:/.model/Zeitreihen/Niederschlag/Ombrometer_";// ...+.zml

  public final static String KollauPREFIX_LINK_GEBN_LOKAL = "Niederschlag/";// ...+.zml

  public final static String KollauPREFIX_LINK_GEBN_REPOSITORY = "project:/.model/Zeitreihen/Niederschlag/";

  public void testUpdateKollau( ) throws Exception
  {
    final UpdateModellKollau modell2 = new UpdateModellKollau();
    modell2.updateIt();
  }

  public UpdateModellKollau( final URL modellURL ) throws Exception
  {
    // m_modellURL = modellURL;
  }

  public UpdateModellKollau( ) throws Exception
  {
    // m_modellURL = getClass().getResource( "resources/modell.gml" );

  }

  public void updateIt( ) throws Exception
  {
    final URL inputModel = getClass().getResource( "resources/modell.gml" );
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( inputModel, null );
    final Feature naModelFe = workspace.getRootFeature();

    // Catchments...
    // final IFeatureType catchmentFT = workspace.getFeatureType( "Catchment" );
    // final Feature[] catchmentFEs = workspace.getFeatures( catchmentFT );
    // updateCatchments( catchmentFEs );
    // updateGebNiederschlagZR( catchmentFEs );
    // updateLZNiederschlagZR( catchmentFEs );
    // updateNiederschlagZR( catchmentFEs );
    // updateGeometries( naModelFe, "D:\\Kalypso_NA\\9-Programmtest\\AdvancedTestModel\\GIS" );
    // Nodes
    // final IFeatureType nodeFT = workspace.getFeatureType( "Node" );
    // final Feature[] nodeFEs = workspace.getFeatures( nodeFT );
    // updateNodes( nodeFEs );
    final IFeatureType RHBchannelFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.STORAGE_CHANNEL_ELEMENT_FT );
    final Feature[] rhbChannelFEs = workspace.getFeatures( RHBchannelFT );
    updateRHBCHannels( rhbChannelFEs );

    final File tmpDir = new File( "C:\\tmp\\ModellUpdate" );
    final File file = File.createTempFile( "modellUpdate", ".gml", tmpDir );
    final OutputStreamWriter writer = new FileWriter( file );
    GmlSerializer.serializeWorkspace( writer, workspace );
    writer.close();
    System.out.println( " updated model is written to " + file.getCanonicalPath() );
  }

  private void updateRHBCHannels( final Feature[] features ) throws SensorException
  {
    for( final Feature feature : features )
    {
      final Double sv = ((Double) feature.getProperty( NaModelConstants.STORAGE_CHANNEL_SV_PROP )) * 1000000;
      final Double vmax = ((Double) feature.getProperty( NaModelConstants.STORAGE_CHANNEL_VMAX_PROP )) * 1000000;
      final Double vmin = ((Double) feature.getProperty( NaModelConstants.STORAGE_CHANNEL_VMIN_PROP )) * 1000000;
      final IObservation observation = (IObservation) feature.getProperty( NaModelConstants.STORAGE_CHANNEL_HVVSQD_PROP );

      final IAxis[] axisList = observation.getAxisList();
      final IAxis waterTableAxis = ObservationUtilities.findAxisByType( axisList, TimeserieConstants.TYPE_NORMNULL );
      final IAxis volumeAxis = ObservationUtilities.findAxisByType( axisList, TimeserieConstants.TYPE_VOLUME );
      final IAxis dischargeAxis = ObservationUtilities.findAxisByType( axisList, TimeserieConstants.TYPE_RUNOFF );
      final ITuppleModel values = observation.getValues( null );
      final int count = values.getCount();
      final Object[][] newValues = new Object[count][axisList.length];
      for( int row = 0; row < count; row++ )
      {
        final Double w = (Double) values.getElement( row, waterTableAxis );
        final Double v = ((Double) values.getElement( row, volumeAxis )) * 1000000;
        final Double q = (Double) values.getElement( row, dischargeAxis );
        newValues[row][0] = w;
        newValues[row][1] = v;
        newValues[row][2] = q;
      }

      feature.setProperty( NaModelConstants.STORAGE_CHANNEL_SV_PROP, sv );
      feature.setProperty( NaModelConstants.STORAGE_CHANNEL_VMAX_PROP, vmax );
      feature.setProperty( NaModelConstants.STORAGE_CHANNEL_VMIN_PROP, vmin );
      final ITuppleModel model = new SimpleTuppleModel( axisList, newValues );
      final SimpleObservation newObservation = new SimpleObservation( null, null, null, true, null, new MetadataList(), axisList, model );
      feature.setProperty( NaModelConstants.STORAGE_CHANNEL_HVVSQD_PROP, newObservation );
    }
  }

  private static void updateCatchments( final Feature[] features ) throws Exception
  {

    for( final Feature feature : features )
    {

    }
  }

  // int asciiID = Integer.parseInt( (String)idProp.getValue() );
  private static void updateNiederschlagZR( final Feature[] features ) throws Exception
  {

    for( final Feature feature : features )
    {
      // Niederschlag Lokale Zeitreihen setzen
      final Object idObj = feature.getProperty( "inum" ); // inum is not used any more in the schema
      final int id = Integer.parseInt( idObj.toString() );
      // Station wasserwerk.kz
      if( (id >= 100 && id <= 106) || (id >= 200 && id <= 202) || id == 408 || id == 410 || id == 504 )
      {
        final TimeseriesLinkType linkNiederschlagZR = NAZMLGenerator.generateobsLink( KollauPREFIX_LINK_N_LOKAL + "wasserwerk.zml" );
        setTSLink( feature, "niederschlagZR", linkNiederschlagZR );
        final TimeseriesLinkType linkNiederschlagZRRepositoryVorhersage = NAZMLGenerator.generateobsLink( KollauPREFIX_LINK_N_REPSITORY + "wasserwerk.zml" );
        setTSLink( feature, "niederschlagZRRepositoryVorhersage", linkNiederschlagZRRepositoryVorhersage );
        final TimeseriesLinkType linkNiederschlagZRRepository = NAZMLGenerator.generateobsLink( KollauPREFIX_LINK_N_REPSITORY + "wasserwerk.zml" );
        setTSLink( feature, "niederschlagZRRepository", linkNiederschlagZRRepository );
      }
      else if( id == 601 || (id >= 603 && id <= 608) || (id >= 619 && id <= 620) )
      {
        final TimeseriesLinkType linkNiederschlagZR = NAZMLGenerator.generateobsLink( KollauPREFIX_LINK_N_LOKAL + "Desy.zml" );
        setTSLink( feature, "niederschlagZR", linkNiederschlagZR );
        final TimeseriesLinkType linkNiederschlagZRRepositoryVorhersage = NAZMLGenerator.generateobsLink( KollauPREFIX_LINK_N_REPSITORY + "Desy.zml" );
        setTSLink( feature, "niederschlagZRRepositoryVorhersage", linkNiederschlagZRRepositoryVorhersage );
        final TimeseriesLinkType linkNiederschlagZRRepository = NAZMLGenerator.generateobsLink( KollauPREFIX_LINK_N_REPSITORY + "Desy.zml" );
        setTSLink( feature, "niederschlagZRRepository", linkNiederschlagZRRepository );
      }

      else if( (id >= 400 && id <= 407) || id == 503 || id == 501 || (id >= 609 && id <= 618) )
      {
        final TimeseriesLinkType linkNiederschlagZR = NAZMLGenerator.generateobsLink( KollauPREFIX_LINK_N_LOKAL + "DB.zml" );
        setTSLink( feature, "niederschlagZR", linkNiederschlagZR );
        final TimeseriesLinkType linkNiederschlagZRRepositoryVorhersage = NAZMLGenerator.generateobsLink( KollauPREFIX_LINK_N_REPSITORY + "DB.zml" );
        setTSLink( feature, "niederschlagZRRepositoryVorhersage", linkNiederschlagZRRepositoryVorhersage );
        final TimeseriesLinkType linkNiederschlagZRRepository = NAZMLGenerator.generateobsLink( KollauPREFIX_LINK_N_REPSITORY + "DB.zml" );
        setTSLink( feature, "niederschlagZRRepository", linkNiederschlagZRRepository );
      }

      else if( id == 107 || id == 301 || id == 409 || id == 500 || id == 505 || (id >= 621 && id <= 623) || (id >= 700 && id <= 704) || (id >= 708 && id <= 730) || id == 900 )
      {
        final TimeseriesLinkType linkNiederschlagZR = NAZMLGenerator.generateobsLink( KollauPREFIX_LINK_N_LOKAL + "Bauhof.zml" );
        setTSLink( feature, "niederschlagZR", linkNiederschlagZR );
        final TimeseriesLinkType linkNiederschlagZRRepositoryVorhersage = NAZMLGenerator.generateobsLink( KollauPREFIX_LINK_N_REPSITORY + "Bauhof.zml" );
        setTSLink( feature, "niederschlagZRRepositoryVorhersage", linkNiederschlagZRRepositoryVorhersage );
        final TimeseriesLinkType linkNiederschlagZRRepository = NAZMLGenerator.generateobsLink( KollauPREFIX_LINK_N_REPSITORY + "Bauhof.zml" );
        setTSLink( feature, "niederschlagZRRepository", linkNiederschlagZRRepository );
      }
    }
  }

  private static void updateGeometries( final Feature modelFeature, final String shapeDir ) throws GmlSerializeException
  {
    // load ShapeFile
    final String cSystem = "EPSG:31467";

    final GMLWorkspace catchmentWorkspace = ShapeSerializer.deserialize( shapeDir + "\\Subcatchments", cSystem );
    final List catchmentFeatures = (List) catchmentWorkspace.getRootFeature().getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );

    final GMLWorkspace channelWorkspace = ShapeSerializer.deserialize( shapeDir + "\\Strand", cSystem );
    final List channelFeatures = (List) channelWorkspace.getRootFeature().getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );

    final GMLWorkspace nodeWorkspace = ShapeSerializer.deserialize( shapeDir + "\\Node", cSystem );
    final List nodeFeatures = (List) nodeWorkspace.getRootFeature().getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );

    // insertGeometries

    System.out.println( "inserting geometries: catchments" );
    final Feature catchmentCollection = (Feature) modelFeature.getProperty( NaModelConstants.CATCHMENT_COLLECTION_MEMBER_PROP );
    final List catchmentList = (List) catchmentCollection.getProperty( NaModelConstants.CATCHMENT_MEMBER_PROP );
    copyProperties( catchmentFeatures, "GEOM", "SUBC_NR", (Feature[]) catchmentList.toArray( new Feature[catchmentList.size()] ), "Ort", "inum" );

    System.out.println( "inserting geometries: channels" );
    final Feature channelCollection = (Feature) modelFeature.getProperty( NaModelConstants.CHANNEL_COLLECTION_MEMBER_PROP );
    final List channelList = (List) channelCollection.getProperty( NaModelConstants.CHANNEL_MEMBER_PROP );
    copyProperties( channelFeatures, "GEOM", "STRAND_NR", (Feature[]) channelList.toArray( new Feature[channelList.size()] ), "Ort", "inum" );

    System.out.println( "inserting geometries: nodes" );
    final Feature nodeCollection = (Feature) modelFeature.getProperty( NaModelConstants.NODE_COLLECTION_MEMBER_PROP );
    final List nodeList = (List) nodeCollection.getProperty( NaModelConstants.NODE_MEMBER_PROP );
    copyProperties( nodeFeatures, "GEOM", "NODE_NR", (Feature[]) nodeList.toArray( new Feature[nodeList.size()] ), "Ort", "num" );

  }

  private static void copyProperties( final List catchmentFeatures, final String orgGeomPropName, final String orgIdPropName, final Feature[] destFE, final String destGeomPropName, final String destIdPropName )
  {
    final HashMap<String, Feature> orgHash = new HashMap<String, Feature>();
    for( final Iterator iter = catchmentFeatures.iterator(); iter.hasNext(); )
    {
      final Feature f = (Feature) iter.next();
      final String id = f.getProperty( orgIdPropName ).toString();
      orgHash.put( id, f );
    }
    for( final Feature destFeature : destFE )
    {
      final String id = destFeature.getProperty( destIdPropName ).toString();
      // System.out.println("processing id=" + id);
      final Feature orgFeature = orgHash.get( id );
      if( orgFeature != null )
      {
        final Object value = orgFeature.getProperty( orgGeomPropName );
        if( value == null )
          System.out.println( "copyvalue is null: id=" + id );
        // FeatureProperty fProp = FeatureFactory.createFeatureProperty( destGeomPropName, value );
        destFeature.setProperty( destGeomPropName, value );
        final Object GEOMProperty = destFeature.getProperty( NaModelConstants.CATCHMENT_GEOM_PROP );
        if( GEOMProperty instanceof GM_Surface )
        {

          final Long area = new Long( (long) ((GM_Surface) value).getArea() );
          // FeatureProperty fpArea = FeatureFactory.createFeatureProperty( "flaech", area );
          destFeature.setProperty( NaModelConstants.NA_MODEL_FLAECH_PROP, area );
        }

      }
      else
        System.out.println( "not found in shapeFile: id=" + id );
    }
  }

  private static void updateGebNiederschlagZR( final Feature[] features ) throws Exception
  {

    for( final Feature feature : features )
    {
      // Niederschlag Lokale Gebietsniederschlagszeitreihen setzen
      final Object idObj = feature.getProperty( "inum" );
      final int id = Integer.parseInt( idObj.toString() );
      final TimeseriesLinkType linkNiederschlagZR = NAZMLGenerator.generateobsLink( KollauPREFIX_LINK_GEBN_LOKAL + "Niederschlag_Catchment" + id + ".zml" );
      setTSLink( feature, "niederschlagZR", linkNiederschlagZR );
      final TimeseriesLinkType linkNiederschlagZRRepository = NAZMLGenerator.generateobsLink( KollauPREFIX_LINK_GEBN_REPOSITORY + "Niederschlag_Catchment" + id + ".zml" );
      setTSLink( feature, "niederschlagZRRepository", linkNiederschlagZRRepository );
      setTSLink( feature, "niederschlagZRRepositoryVorhersage", linkNiederschlagZRRepository );
    }
  }

  private static void updateLZNiederschlagZR( final Feature[] features ) throws Exception
  {

    for( final Feature feature : features )
    {
      // Niederschlag Lokale Langzeitniederschlagszeitreihen setzen
      final TimeseriesLinkType linkNiederschlagZR = NAZMLGenerator.generateobsLink( KollauPREFIX_LINK_N_LOKAL + "Fuhlsbuettel.zml" );
      setTSLink( feature, "niederschlagZR", linkNiederschlagZR );
      final TimeseriesLinkType linkNiederschlagZRRepositoryVorhersage = NAZMLGenerator.generateobsLink( KollauPREFIX_LINK_N_REPSITORY + "Fuhlsbuettel.zml" );
      setTSLink( feature, "niederschlagZRRepositoryVorhersage", linkNiederschlagZRRepositoryVorhersage );
      final TimeseriesLinkType linkNiederschlagZRRepository = NAZMLGenerator.generateobsLink( KollauPREFIX_LINK_N_REPSITORY + "Fuhlsbuettel.zml" );
      setTSLink( feature, "niederschlagZRRepository", linkNiederschlagZRRepository );
    }
  }

  private static void updateNodes( final Feature[] features ) throws Exception
  {
    // lokale ZR werden fuer alle gesetzt.
    for( final Feature fe : features )
    {
      // pegel lokal
      final TimeseriesLinkType linkPegel = NAZMLGenerator.generateobsLink( WeisseElsterConstants.PREFIX_LINK_WQ_PEGEL_LOKAL + fe.getId() + ".zml" );
      setTSLink( fe, "pegelZR", linkPegel );
      // berechnet
      final TimeseriesLinkType linkBerechnet = NAZMLGenerator.generateobsLink( WeisseElsterConstants.PREFIX_LINK_WQ_BERECHNET_LOKAL + fe.getId() + ".zml" );
      setTSLink( fe, "qberechnetZR", linkBerechnet );
      setTSLink( fe, "pegelBerechnetZRRepository", null );
      setTSLink( fe, "zuflussZR", null );
      setTSLink( fe, "pegelZRRepository", null );
      setTSLink( fe, "zuflussZRRepository", null );
      setTSLink( fe, "zuflussZRRepositoryVorhersage", null );
      // FeatureProperty nameProp = FeatureFactory.createFeatureProperty( "name", null );
      fe.setProperty( NaModelConstants.GML_FEATURE_NAME_PROP, null );
    }
  }

  private static void setTSLink( final Feature fe, final String propName, final TimeseriesLinkType tsLink )
  {
    // fe.setProperty( FeatureFactory.createFeatureProperty( propName, tsLink ) );
    fe.setProperty( propName, tsLink );
  }

}