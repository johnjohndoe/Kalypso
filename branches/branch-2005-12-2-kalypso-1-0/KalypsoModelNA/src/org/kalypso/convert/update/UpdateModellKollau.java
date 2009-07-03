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
package org.kalypso.convert.update;

import java.io.File;
import java.io.FileWriter;
import java.io.OutputStreamWriter;
import java.net.URL;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.kalypso.contribs.java.io.FileUtilities;
import org.kalypso.contribs.java.net.IUrlCatalog;
import org.kalypso.contribs.java.net.MultiUrlCatalog;
import org.kalypso.convert.namodel.schema.UrlCatalogNA;
import org.kalypso.convert.namodel.timeseries.NAZMLGenerator;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.ogc.gml.typehandler.DiagramTypeHandler;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;
import org.kalypso.zml.obslink.TimeseriesLink;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.extension.ITypeRegistry;
import org.kalypsodeegree_impl.extension.MarshallingTypeRegistrySingleton;
import org.kalypsodeegree_impl.gml.schema.GMLSchemaCatalog;
import org.kalypsodeegree_impl.gml.schema.schemata.DeegreeUrlCatalog;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * here are moethodes used for preparing the kollau modell
 * 
 * @author huebsch
 */
public class UpdateModellKollau
{
  private final URL m_modellURL;

  public final static String PSI_PROGNOSE_SUFFIX = ".P1_MW";

  public final static String KollauPREFIX_LINK_N_LOKAL = "Niederschlag/Ombrometer_";//...+.zml

  public final static String KollauPREFIX_LINK_N_REPSITORY = "project:/.model/Zeitreihen/Niederschlag/Ombrometer_";//...+.zml

  public final static String KollauPREFIX_LINK_GEBN_LOKAL = "Niederschlag/";//...+.zml

  public final static String KollauPREFIX_LINK_GEBN_REPOSITORY = "project:/.model/Zeitreihen/Niederschlag/";

  public static void main( String[] args )
  {
    final IUrlCatalog catalog = new MultiUrlCatalog( new IUrlCatalog[]
    { new DeegreeUrlCatalog(), new UrlCatalogNA() } );
    GMLSchemaCatalog.init( catalog, FileUtilities.createNewTempDir( "schemaCache" ) );
    try
    {
      final ITypeRegistry registry = MarshallingTypeRegistrySingleton.getTypeRegistry();
      registry.registerTypeHandler( new ObservationLinkHandler() );
      registry.registerTypeHandler( new DiagramTypeHandler() );
      UpdateModellKollau modell2 = new UpdateModellKollau();
      modell2.updateIt();
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  public UpdateModellKollau( URL modellURL ) throws Exception
  {
    m_modellURL = modellURL;
  }

  public UpdateModellKollau() throws Exception
  {
    m_modellURL = getClass().getResource( "resources/modell.gml" );
  }

  public void updateIt() throws Exception
  {
    //    URL schemaURL = KalypsoNADefaultSchema.getDefaultNaModellSchemaURL();
    GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( m_modellURL );
    final Feature naModelFe = workspace.getRootFeature();

    // Catchments...
    final FeatureType catchmentFT = workspace.getFeatureType( "Catchment" );
    final Feature[] catchmentFEs = workspace.getFeatures( catchmentFT );
    //    updateCatchments( catchmentFEs );
    //    updateGebNiederschlagZR( catchmentFEs );
    //    updateLZNiederschlagZR( catchmentFEs );
    //    updateNiederschlagZR( catchmentFEs );
    updateGeometries( naModelFe, "D:\\Kalypso_NA\\9-Programmtest\\AdvancedTestModel\\GIS" );
    // Nodes
    final FeatureType nodeFT = workspace.getFeatureType( "Node" );
    final Feature[] nodeFEs = workspace.getFeatures( nodeFT );
    //    updateNodes( nodeFEs );

    File tmpDir = new File( "C:\\temp\\ModellUpdate" );
    File file = File.createTempFile( "modellUpdate", ".gml", tmpDir );
    final OutputStreamWriter writer = new FileWriter( file );
    GmlSerializer.serializeWorkspace( writer, workspace );
    writer.close();
    System.out.println( " updated model is written to " + file.getCanonicalPath() );
  }

  private static void updateCatchments( Feature[] features ) throws Exception
  {
    for( int i = 0; i < features.length; i++ )
    {
      //final Feature feature = features[i];
    }
  }

  //  int asciiID = Integer.parseInt( (String)idProp.getValue() );
  private static void updateNiederschlagZR( Feature[] features ) throws Exception
  {
    for( int i = 0; i < features.length; i++ )
    {
      final Feature feature = features[i];

      // Niederschlag Lokale Zeitreihen setzen
      Object idObj = feature.getProperty( "inum" );
      int id = Integer.parseInt( idObj.toString() );
      //     Station wasserwerk.kz
      if( ( id >= 100 && id <= 106 ) || ( id >= 200 && id <= 202 ) || id == 408 || id == 410 || id == 504 )
      {
        TimeseriesLink linkNiederschlagZR = NAZMLGenerator.generateobsLink( KollauPREFIX_LINK_N_LOKAL
            + "wasserwerk.zml" );
        setTSLink( feature, "niederschlagZR", linkNiederschlagZR );
        TimeseriesLink linkNiederschlagZRRepositoryVorhersage = NAZMLGenerator
            .generateobsLink( KollauPREFIX_LINK_N_REPSITORY + "wasserwerk.zml" );
        setTSLink( feature, "niederschlagZRRepositoryVorhersage", linkNiederschlagZRRepositoryVorhersage );
        TimeseriesLink linkNiederschlagZRRepository = NAZMLGenerator.generateobsLink( KollauPREFIX_LINK_N_REPSITORY
            + "wasserwerk.zml" );
        setTSLink( feature, "niederschlagZRRepository", linkNiederschlagZRRepository );
      }
      else if( id == 601 || ( id >= 603 && id <= 608 ) || ( id >= 619 && id <= 620 ) )
      {
        TimeseriesLink linkNiederschlagZR = NAZMLGenerator.generateobsLink( KollauPREFIX_LINK_N_LOKAL + "Desy.zml" );
        setTSLink( feature, "niederschlagZR", linkNiederschlagZR );
        TimeseriesLink linkNiederschlagZRRepositoryVorhersage = NAZMLGenerator
            .generateobsLink( KollauPREFIX_LINK_N_REPSITORY + "Desy.zml" );
        setTSLink( feature, "niederschlagZRRepositoryVorhersage", linkNiederschlagZRRepositoryVorhersage );
        TimeseriesLink linkNiederschlagZRRepository = NAZMLGenerator.generateobsLink( KollauPREFIX_LINK_N_REPSITORY
            + "Desy.zml" );
        setTSLink( feature, "niederschlagZRRepository", linkNiederschlagZRRepository );
      }

      else if( ( id >= 400 && id <= 407 ) || id == 503 || id == 501 || ( id >= 609 && id <= 618 ) )
      {
        TimeseriesLink linkNiederschlagZR = NAZMLGenerator.generateobsLink( KollauPREFIX_LINK_N_LOKAL + "DB.zml" );
        setTSLink( feature, "niederschlagZR", linkNiederschlagZR );
        TimeseriesLink linkNiederschlagZRRepositoryVorhersage = NAZMLGenerator
            .generateobsLink( KollauPREFIX_LINK_N_REPSITORY + "DB.zml" );
        setTSLink( feature, "niederschlagZRRepositoryVorhersage", linkNiederschlagZRRepositoryVorhersage );
        TimeseriesLink linkNiederschlagZRRepository = NAZMLGenerator.generateobsLink( KollauPREFIX_LINK_N_REPSITORY
            + "DB.zml" );
        setTSLink( feature, "niederschlagZRRepository", linkNiederschlagZRRepository );
      }

      else if( id == 107 || id == 301 || id == 409 || id == 500 || id == 505 || ( id >= 621 && id <= 623 )
          || ( id >= 700 && id <= 704 ) || ( id >= 708 && id <= 730 ) || id == 900 )
      {
        TimeseriesLink linkNiederschlagZR = NAZMLGenerator.generateobsLink( KollauPREFIX_LINK_N_LOKAL + "Bauhof.zml" );
        setTSLink( feature, "niederschlagZR", linkNiederschlagZR );
        TimeseriesLink linkNiederschlagZRRepositoryVorhersage = NAZMLGenerator
            .generateobsLink( KollauPREFIX_LINK_N_REPSITORY + "Bauhof.zml" );
        setTSLink( feature, "niederschlagZRRepositoryVorhersage", linkNiederschlagZRRepositoryVorhersage );
        TimeseriesLink linkNiederschlagZRRepository = NAZMLGenerator.generateobsLink( KollauPREFIX_LINK_N_REPSITORY
            + "Bauhof.zml" );
        setTSLink( feature, "niederschlagZRRepository", linkNiederschlagZRRepository );
      }
    }
  }

  private static void updateGeometries( Feature modelFeature, String shapeDir ) throws GmlSerializeException
  {
    // load ShapeFile
    ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
    CS_CoordinateSystem cSystem = org.kalypsodeegree_impl.model.cs.Adapters.getDefault().export(
        csFac.getCSByName( "EPSG:31467" ) );

    final GMLWorkspace catchmentWorkspace = ShapeSerializer.deserialize( shapeDir + "\\Subcatchments", cSystem );
    final List catchmentFeatures = (List)catchmentWorkspace.getRootFeature().getProperty(
        ShapeSerializer.PROPERTY_FEATURE_MEMBER );

    final GMLWorkspace channelWorkspace = ShapeSerializer.deserialize( shapeDir + "\\Strand", cSystem );
    final List channelFeatures = (List)channelWorkspace.getRootFeature().getProperty(
        ShapeSerializer.PROPERTY_FEATURE_MEMBER );

    final GMLWorkspace nodeWorkspace = ShapeSerializer.deserialize( shapeDir + "\\Node", cSystem );
    final List nodeFeatures = (List)nodeWorkspace.getRootFeature()
        .getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );

    // insertGeometries

    System.out.println( "inserting geometries: catchments" );
    Feature catchmentCollection = (Feature)modelFeature.getProperty( "CatchmentCollectionMember" );
    List catchmentList = (List)catchmentCollection.getProperty( "catchmentMember" );
    copyProperties( catchmentFeatures, "GEOM", "SUBC_NR", (Feature[])catchmentList.toArray( new Feature[catchmentList
        .size()] ), "Ort", "inum" );

    System.out.println( "inserting geometries: channels" );
    Feature channelCollection = (Feature)modelFeature.getProperty( "ChannelCollectionMember" );
    List channelList = (List)channelCollection.getProperty( "channelMember" );
    copyProperties( channelFeatures, "GEOM", "STRAND_NR", (Feature[])channelList.toArray( new Feature[channelList
        .size()] ), "Ort", "inum" );

    System.out.println( "inserting geometries: nodes" );
    Feature nodeCollection = (Feature)modelFeature.getProperty( "NodeCollectionMember" );
    List nodeList = (List)nodeCollection.getProperty( "nodeMember" );
    copyProperties( nodeFeatures, "GEOM", "NODE_NR", (Feature[])nodeList.toArray( new Feature[nodeList.size()] ),
        "Ort", "num" );

  }

  private static void copyProperties( final List catchmentFeatures, String orgGeomPropName, String orgIdPropName,
      Feature[] destFE, String destGeomPropName, String destIdPropName )
  {
    HashMap orgHash = new HashMap();
    for( Iterator iter = catchmentFeatures.iterator(); iter.hasNext(); )
    {
      final Feature f = (Feature)iter.next();
      String id = f.getProperty( orgIdPropName ).toString();
      orgHash.put( id, f );
    }
    for( int i = 0; i < destFE.length; i++ )
    {
      Feature destFeature = destFE[i];
      String id = destFeature.getProperty( destIdPropName ).toString();
      //            System.out.println("processing id=" + id);
      Feature orgFeature = (Feature)orgHash.get( id );
      if( orgFeature != null )
      {
        Object value = orgFeature.getProperty( orgGeomPropName );
        if( value == null )
          System.out.println( "copyvalue is null: id=" + id );
        FeatureProperty fProp = FeatureFactory.createFeatureProperty( destGeomPropName, value );
        destFeature.setProperty( fProp );
        Object GEOMProperty = destFeature.getProperty( "Ort" );
        if( GEOMProperty instanceof GM_Surface )
        {

          Long area = new Long( (long)( (GM_Surface)value ).getArea() );
          FeatureProperty fpArea = FeatureFactory.createFeatureProperty( "flaech", area );
          destFeature.setProperty( fpArea );
        }

      }
      else
        System.out.println( "not found in shapeFile: id=" + id );
    }
  }

  private static void updateGebNiederschlagZR( Feature[] features ) throws Exception
  {
    for( int i = 0; i < features.length; i++ )
    {
      final Feature feature = features[i];

      // Niederschlag Lokale Gebietsniederschlagszeitreihen setzen
      Object idObj = feature.getProperty( "inum" );
      int id = Integer.parseInt( idObj.toString() );
      TimeseriesLink linkNiederschlagZR = NAZMLGenerator.generateobsLink( KollauPREFIX_LINK_GEBN_LOKAL
          + "Niederschlag_Catchment" + id + ".zml" );
      setTSLink( feature, "niederschlagZR", linkNiederschlagZR );
      TimeseriesLink linkNiederschlagZRRepository = NAZMLGenerator.generateobsLink( KollauPREFIX_LINK_GEBN_REPOSITORY
          + "Niederschlag_Catchment" + id + ".zml" );
      setTSLink( feature, "niederschlagZRRepository", linkNiederschlagZRRepository );
      setTSLink( feature, "niederschlagZRRepositoryVorhersage", linkNiederschlagZRRepository );
    }
  }

  private static void updateLZNiederschlagZR( Feature[] features ) throws Exception
  {
    for( int i = 0; i < features.length; i++ )
    {
      final Feature feature = features[i];

      // Niederschlag Lokale Langzeitniederschlagszeitreihen setzen
      TimeseriesLink linkNiederschlagZR = NAZMLGenerator.generateobsLink( KollauPREFIX_LINK_N_LOKAL
          + "Fuhlsbuettel.zml" );
      setTSLink( feature, "niederschlagZR", linkNiederschlagZR );
      TimeseriesLink linkNiederschlagZRRepositoryVorhersage = NAZMLGenerator
          .generateobsLink( KollauPREFIX_LINK_N_REPSITORY + "Fuhlsbuettel.zml" );
      setTSLink( feature, "niederschlagZRRepositoryVorhersage", linkNiederschlagZRRepositoryVorhersage );
      TimeseriesLink linkNiederschlagZRRepository = NAZMLGenerator.generateobsLink( KollauPREFIX_LINK_N_REPSITORY
          + "Fuhlsbuettel.zml" );
      setTSLink( feature, "niederschlagZRRepository", linkNiederschlagZRRepository );
    }
  }

  private static void updateNodes( Feature[] features ) throws Exception
  {
    // lokale ZR werden fuer alle gesetzt.
    for( int i = 0; i < features.length; i++ )
    {
      final Feature fe = features[i];
      // pegel lokal
      TimeseriesLink linkPegel = NAZMLGenerator.generateobsLink( WeisseElsterConstants.PREFIX_LINK_WQ_PEGEL_LOKAL
          + fe.getId() + ".zml" );
      setTSLink( fe, "pegelZR", linkPegel );
      // berechnet
      TimeseriesLink linkBerechnet = NAZMLGenerator
          .generateobsLink( WeisseElsterConstants.PREFIX_LINK_WQ_BERECHNET_LOKAL + fe.getId() + ".zml" );
      setTSLink( fe, "qberechnetZR", linkBerechnet );
      setTSLink( fe, "pegelBerechnetZRRepository", null );
      setTSLink( fe, "zuflussZR", null );
      setTSLink( fe, "pegelZRRepository", null );
      setTSLink( fe, "zuflussZRRepository", null );
      setTSLink( fe, "zuflussZRRepositoryVorhersage", null );
      FeatureProperty nameProp = FeatureFactory.createFeatureProperty( "name", null );
      fe.setProperty( nameProp );
    }
  }

  private static void setTSLink( Feature fe, String propName, TimeseriesLink tsLink )
  {
    fe.setProperty( FeatureFactory.createFeatureProperty( propName, tsLink ) );
  }
}