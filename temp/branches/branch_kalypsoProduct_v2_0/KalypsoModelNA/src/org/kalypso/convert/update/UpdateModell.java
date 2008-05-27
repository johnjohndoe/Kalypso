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

import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.convert.namodel.timeseries.NAZMLGenerator;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author doemming here are moethodes used for preparing the modell
 */
public class UpdateModell
{
  private final URL m_modellURL;

  public final static String PSI_PROGNOSE_SUFFIX = ".P1_MW";

  private static final double DEFAULT_Q = 0;

  private static final double DEFAULT_W = 0;

  public static void main( String[] args )
  {
    try
    {
      final ITypeRegistry<IMarshallingTypeHandler> registry = MarshallingTypeRegistrySingleton.getTypeRegistry();
      registry.registerTypeHandler( new ObservationLinkHandler() );
      UpdateModell modell2 = new UpdateModell();
      modell2.updateIt();
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  public UpdateModell( URL modellURL ) throws Exception
  {
    m_modellURL = modellURL;
  }

  public UpdateModell( ) throws Exception
  {
    m_modellURL = getClass().getResource( "resources/WE_modell.gml" );
  }

  public void updateIt( ) throws Exception
  {
    GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( m_modellURL, null );

    // Catchments...
    final IFeatureType catchmentFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.CATCHMENT_ELEMENT_FT );
    final Feature[] catchmentFEs = workspace.getFeatures( catchmentFT );
    updateCatchments( catchmentFEs );
    // Nodes
    final IFeatureType nodeFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_ELEMENT_FT );
    final Feature[] nodeFEs = workspace.getFeatures( nodeFT );
    updateNodes( nodeFEs );

    updatePegel( workspace );

    // updateZuflussNamen( workspace );
    File tmpDir = new File( "C:\\TMP" );
    File file = File.createTempFile( "modellUpdate", ".gml", tmpDir );
    final OutputStreamWriter writer = new FileWriter( file );
    GmlSerializer.serializeWorkspace( writer, workspace );
    writer.close();
    System.out.println( " updated model is written to " + file.getCanonicalPath() );
  }

  private static void updateCatchments( Feature[] features ) throws Exception
  {
    String intervallFilter = "?" + UpdateHelper.createIntervallFilter( 1, "HOUR", "sum" );
    // String intervallFilter = "?" + UpdateHelper.createIntervallFilter( 15, "MINUTE", "sum" );
    for( int i = 0; i < features.length; i++ )
    {
      final Feature feature = features[i];
      // messung
      TimeseriesLinkType linkMessung = NAZMLGenerator.generateobsLink( WeisseElsterConstants.PREFIX_LINK_GebietsNiederschlagModell + feature.getId() + intervallFilter );
      setTSLink( feature, "niederschlagZRRepository", linkMessung );
      // vorhersage
      TimeseriesLinkType linkVorhersage = NAZMLGenerator.generateobsLink( WeisseElsterConstants.PREFIX_LINK_NIEDERSCHLAGVORHERSAGE + feature.getId() + ".zml" + intervallFilter );
      setTSLink( feature, "niederschlagZRRepositoryVorhersage", linkVorhersage );
      // berechnung
      TimeseriesLinkType linkBerechnung = NAZMLGenerator.generateobsLink( WeisseElsterConstants.PREFIX_LINK_N_LOKAL + feature.getId() + ".zml" );
      setTSLink( feature, "niederschlagZR", linkBerechnung );
    }
  }

  private final static int POS_NAME = 0;

  private final static int POS_FID = 1;

  private final static int POS_ZML = 2;

  private final static int POS_PSI_ID = 3;

  // "P":Pegel "Z":Zufluss "-":nicht verfuegbar
  private final static int POS_TYPE = 4;

  // <Wasserstand_gemessenEingang>
  // <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink"
  // xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum"
  // valueaxis="Wert" ns1:actuate="onRequest"
  // ns1:href="project:/.model/zeitreihen/W_leer.zml?&lt;filter&gt;&lt;interpolationFilter
  // xmlns=&quot;filters.zml.kalypso.org&quot;
  // calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;3&quot;
  // forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot;
  // defaultStatus=&quot;2&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
  // </Wasserstand_gemessenEingang>

  private final static String[][] m_pegel = {
  /*
   * [1] Pegel-Name [2] FeatureID [3] zml [4] PSI-ID
   */
  // 1
      new String[] { "Bad Elster", "Node1800", null, // TODO
          "576391", "P", },
      // 2
      new String[] { "Adorf", "Node1700", "q_adorf.zml", "576400", "P" },
      // 3
      new String[] { "Oelsnitz", "Node1600", "q_oelsnitz.zml", "576410", "P" },
      // 4
      new String[] { "Strassberg", "Node1401", "q_strassberg.zml", "576421", "P" },
      // 5
      new String[] { "Elsterberg", "Node1300", "q_elsterberg.zml", "576440", "P" },
      // 6
      new String[] { "Rodewisch", "Node4200", null, // TODO
          "577211", "P" },
      // 7
      new String[] { "Mylau", "Node4100", "q_mylau.zml", "577220", "P" },
      // 8
      new String[] { "Greiz", "Node1220", "q_greiz.zml", "576470", // TODO ist nicht in PSI enthalten
          "P" },
      // 9
      new String[] { "Weida", "Node2002", "q_weida.zml", "577320", // TODO ist nicht in PSI enthlten
          "P" },
      // 10
      new String[] { "Gera-Langenberg", "Node1210", "q_gera.zml", "576520", "P" },
      // 11
      new String[] { "Zeitz", "Node1110", "q_zeitz.zml", "576610", // TODO evt nicht in PSI-fake vorhanden
          "P" },
      // 12
      new String[] { "Kleindalzig", "Node1020", "q_kleindalzig.zml", "576631", "P" },
      // 13
      new String[] { "Albrechtshain", "Node3201", null, // TODO
          "578090", // TODO nicht in PSI enthalten
          "P" },
      // 14
      new String[] { "Leipzig-Thekla", "Node3100", "q_leipzig-thekla.zml", "578110", "P" },
      // 15
      new String[] { "Oberthau", "Node1001", "q_oberthau.zml", "576900", // evt nicht in PSI-fake enthalten
          "P" },
      // 16
      new String[] { "Neukirchen", "Node7300", "q_neukirchen.zml", "577501", "P" },
      // 17
      new String[] { "Goessnitz", "Node7200", "q_goessnitz.zml", "577510", "P" },
      // Z1
      new String[] { "Droeda", "Node6100", "q_droeda.zml", "577050", // TODO nicht in PSI enthalten
          "Z" },
      // Z2
      new String[] { "Magwitz", "Node1500", null, // TODO
          "576420", "Z" },
      // Z3
      new String[] { "Poehl", "Node1301", null, // TODO
          "577110", "Z" },
      // Z4
      new String[] { "Boehlen", "Node7100", "q_boehlen.zml", "577571", // TODO nicht in PSI enthalten
          "Z" },
      // Z5
      new String[] { "Koberbach", "Node7310", "q_koberbach.zml", "", "Z" }, };

  private static void updatePegel( GMLWorkspace workspace ) throws Exception
  {
    for( int i = 0; i < m_pegel.length; i++ )
    {
      final String[] pegelContext = m_pegel[i];
      final String fId = pegelContext[POS_FID];
      final String name = pegelContext[POS_NAME];
      final String psiID = pegelContext[POS_PSI_ID];
      final String zml = pegelContext[POS_ZML];
      final String type = pegelContext[POS_TYPE];
      boolean available = true;
      if( type.indexOf( "-" ) > -1 )
        available = false;

      final Feature fe = workspace.getFeature( fId );
      fe.setProperty( NaModelConstants.GML_FEATURE_NAME_PROP, name );
      if( type.indexOf( "Z" ) > -1 )
      {
        // zuflussRep
        String interpolationFilter = "?" + UpdateHelper.createInterpolationFilter( 1, DEFAULT_Q, true );
        TimeseriesLinkType zuflussRep = NAZMLGenerator.generateobsLink( "project:/.model/zeitreihen/Q_leer.zml" + interpolationFilter );
        setTSLink( fe, "zuflussZRRepository", zuflussRep );

        // if( available )
        // {
        // TimeseriesLink zuflussRep = NAZMLGenerator
        // .generateobsLink( WeisseElsterConstants.PREFIX_LINK_WQ_Zufluss_Rep +
        // psiID
        // + interpolationFilter );
        // setTSLink( fe, "zuflussZRRepository", zuflussRep );
        // }
        // else
        // {
        // TimeseriesLink zuflussRep = NAZMLGenerator
        // .generateobsLink(
        // WeisseElsterConstants.ALTERNATIV_PREFIX_LINK_WQ_Zufluss_Rep
        // + fId
        // + interpolationFilter );
        // setTSLink( fe, "zuflussZRRepository", zuflussRep );
        // }

        // zuflussRepVorhersage
        TimeseriesLinkType zuflussRepVorhersage = NAZMLGenerator.generateobsLink( "project:/.model/zeitreihen/Q_leer.zml" + interpolationFilter );
        setTSLink( fe, "zuflussZRRepositoryVorhersage", zuflussRepVorhersage );

        // if( available )
        // {
        // TimeseriesLink zuflussRepVorhersage = NAZMLGenerator
        // .generateobsLink(
        // WeisseElsterConstants.PREFIX_LINK_WQ_Zufluss_Rep_Vorhersage + psiID
        // + interpolationFilter );
        // setTSLink( fe, "zuflussZRRepositoryVorhersage", zuflussRepVorhersage
        // );
        // }
        // else
        // {
        // TimeseriesLink zuflussRepVorhersage = NAZMLGenerator
        // .generateobsLink(
        // WeisseElsterConstants.ALTERNATIV_PREFIX_LINK_WQ_Zufluss_Rep + fId
        // + interpolationFilter );
        // setTSLink( fe, "zuflussZRRepositoryVorhersage", zuflussRepVorhersage
        // );
        // }

        // zufluss lokal
        TimeseriesLinkType linkZufluss = NAZMLGenerator.generateobsLink( WeisseElsterConstants.PREFIX_LINK_WQ_ZUFLUSS_LOKAL + fe.getId() + ".zml" );
        setTSLink( fe, "zuflussZR", linkZufluss );
      }
      // pegelRep
      if( type.indexOf( "P" ) > -1 )
      {
        String interpolationFilter = "?" + UpdateHelper.createInterpolationFilter( 1, DEFAULT_W, true );
        if( available )
        {
          TimeseriesLinkType pegelRep = NAZMLGenerator.generateobsLink( WeisseElsterConstants.PREFIX_LINK_WQ_Pegel_Rep + psiID + interpolationFilter );
          setTSLink( fe, "pegelZRRepository", pegelRep );
        }
        else if( zml != null )
        {
          TimeseriesLinkType pegelRep = NAZMLGenerator.generateobsLink( WeisseElsterConstants.ALTERNATIV_PREFIX_LINK_WQ_Pegel_Rep + zml + interpolationFilter );
          setTSLink( fe, "pegelZRRepository", pegelRep );
        }
        // pegelBerechnetZRRepository (Ablage)
        TimeseriesLinkType pegelBerechnetRep = NAZMLGenerator.generateobsLink( WeisseElsterConstants.PREFIX_LINK_WQ_Pegel_Rep + psiID + PSI_PROGNOSE_SUFFIX );
        setTSLink( fe, "pegelBerechnetZRAblage", pegelBerechnetRep );
      }
    }
  }

  private static void updateNodes( Feature[] features ) throws Exception
  {
    // lokale ZR werden fuer alle gesetzt.
    for( int i = 0; i < features.length; i++ )
    {
      final Feature fe = features[i];
      // pegel lokal
      TimeseriesLinkType linkPegel = NAZMLGenerator.generateobsLink( WeisseElsterConstants.PREFIX_LINK_WQ_PEGEL_LOKAL + fe.getId() + ".zml" );
      setTSLink( fe, "pegelZR", linkPegel );
      // berechnet
      TimeseriesLinkType linkBerechnet = NAZMLGenerator.generateobsLink( WeisseElsterConstants.PREFIX_LINK_WQ_BERECHNET_LOKAL + fe.getId() + ".zml" );
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

  private static void setTSLink( Feature fe, String propName, TimeseriesLinkType tsLink )
  {
    // fe.setProperty( FeatureFactory.createFeatureProperty( propName, tsLink ) );
    fe.setProperty( propName, tsLink );
  }
  // TODO
  // <temperaturZRRepository/>
  // <temperaturZRRepositoryVorhersage/>
  // <temperaturZR/>
}