package org.kalypso.convert.update;

import java.io.File;
import java.io.FileWriter;
import java.io.Writer;
import java.net.URL;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree_impl.extension.ITypeRegistry;
import org.deegree_impl.extension.TypeRegistrySingleton;
import org.deegree_impl.model.feature.FeatureFactory;
import org.kalypso.convert.WeisseElsterConstants;
import org.kalypso.convert.namodel.NAZMLGenerator;
import org.kalypso.convert.namodel.schema.KalypsoNADefaultSchema;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;
import org.kalypso.zml.obslink.TimeseriesLink;

/**
 * @author doemming
 * 
 * here are moethodes used for preparing the modell
 */
public class UpdateModell
{
  private final URL m_modellURL;

  public UpdateModell( URL modellURL ) throws Exception
  {
    m_modellURL = modellURL;
  }

  public void updateIt() throws Exception
  {
    URL schemaURL = KalypsoNADefaultSchema.getInstance().getDefaultNaModellSchemaURL();
    GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( m_modellURL, schemaURL );

    // Catchments...
    final FeatureType catchmentFT = workspace.getFeatureType( "Catchment" );
    final Feature[] catchmentFEs = workspace.getFeatures( catchmentFT );
    updateCatchments( catchmentFEs );
    // Nodes
    final FeatureType nodeFT = workspace.getFeatureType( "Node" );
    final Feature[] nodeFEs = workspace.getFeatures( nodeFT );
    updateNodes( nodeFEs );        
    
    updatePegel(workspace);

    //    updateZuflussNamen( workspace );
    File file = File.createTempFile( "modellUpdate", ".gml" );
    Writer writer = new FileWriter( file );
    GmlSerializer.serializeWorkspace( writer, workspace );
    writer.close();
    System.out.println( " updated model is written to " + file.getCanonicalPath() );
  }

  public static void main( String[] args )
  {
    try
    {
      final ITypeRegistry registry = TypeRegistrySingleton.getTypeRegistry();
      registry.registerTypeHandler( new ObservationLinkHandler() );
      File modell = new File( "C:\\TMP\\modell.gml" );
      URL modellURL = modell.toURL();
      UpdateModell modell2 = new UpdateModell( modellURL );
      modell2.updateIt();
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }


  private static void updateCatchments( Feature[] features ) throws Exception
  {
    for( int i = 0; i < features.length; i++ )
    {
      final Feature feature = features[i];
      // messung
      TimeseriesLink linkMessung = NAZMLGenerator.generateobsLink(
          WeisseElsterConstants.PREFIX_LINK_GebietsNiederschlagModell + feature.getId(),
          NAZMLGenerator.NA_LINK_N );
      setTSLink( feature, "niederschlagZRRepository", linkMessung );
      // vorhersage
      TimeseriesLink linkVorhersage = NAZMLGenerator.generateobsLink(
          WeisseElsterConstants.PREFIX_LINK_NIEDERSCHLAGVORHERSAGE + feature.getId() + ".zml",
          NAZMLGenerator.NA_LINK_N );
      setTSLink( feature, "niederschlagZRRepositoryVorhersage", linkVorhersage );
      // berechnung
      TimeseriesLink linkBerechnung = NAZMLGenerator.generateobsLink(
          WeisseElsterConstants.PREFIX_LINK_N_LOKAL + feature.getId() + ".zml",
          NAZMLGenerator.NA_LINK_N );
      setTSLink( feature, "niederschlagZR", linkBerechnung );
    }
  }

  private final static int POS_NAME = 0;

  private final static int POS_FID = 1;

  private final static int POS_ZML = 2;

  private final static int POS_PSI_ID = 3;

  // "P":Pegel "Z":Zufluss "-":nicht verfuegbar
  private final static int POS_TYPE = 4;

  private final static String[][] m_pegel =
  {
      // kaputte pegel und noch nicht verfuegbare pegel sind kommentiert
      //  new String[]{"Bad Elster","Node1800","adorf.zml"},
      /*
       * [1] Pegel-Name [2] FeatureID [3] zml [4] PSI-ID
       */
      // 1
      new String[]
      {
          "Bad Elster",
          "Node1800",
          null, // TODO
          "576391",
          "-P", },
      // 2
      new String[]
      {
          "Adorf",
          "Node1700",
          "q_adorf.zml",
          "576400",
          "P" },
      //3
      new String[]
      {
          "Oelsnitz",
          "Node1600",
          "q_oelsnitz.zml",
          "576410",
          "P" },
      //4
      new String[]
      {
          "Strassberg",
          "Node1401",
          "q_strassberg.zml",
          "576421",
          "P" },
      //5
      new String[]
      {
          "Elsterberg",
          "Node1300",
          "q_elsterberg.zml",
          "576440",
          "P" },
      //6
      new String[]
      {
          "Rodewisch",
          "Node4200",
          null, // TODO
          "577211",
          "-P" },
      //7
      new String[]
      {
          "Mylau",
          "Node4100",
          "q_mylau.zml",
          "577211",
          "P" },
      //8
      new String[]
      {
          "Greiz",
          "Node1220",
          "q_greiz.zml",
          "576470", // TODO ist nicht in PSI enthalten
          "-P" },
      //9
      new String[]
      {
          "Weida",
          "Node2002",
          "q_weida.zml",
          "577320", // TODO ist nicht in PSI enthlten
          "-P" },
      //10
      new String[]
      {
          "Gera-Langenberg",
          "Node1210",
          "q_gera.zml",
          "576520",
          "P" },
      //11
      new String[]
      {
          "Zeitz",
          "Node1110",
          "q_zeitz.zml",
          null, // TODO keine PSI-ID
          "-P" },
      //12
      new String[]
      {
          "Kleindalzig",
          "Node1020",
          "q_kleindalzig.zml",
          "576631",
          "P" },
      //13
      new String[]
      {
          "Albrechtshain",
          "Node3201",
          null, // TODO
          "578090", // TODO nicht in PSI enthalten
          "-P" },
      //14
      new String[]
      {
          "Leipzig-Thekla",
          "Node3100",
          "q_leipzig-thekla.zml",
          "578110",
          "P" },
      //15
      new String[]
      {
          "Oberthau",
          "Node1001",
          "q_oberthau.zml",
          null, // keine PSI-ID
          "-P" },
      //16
      new String[]
      {
          "Neukirchen",
          "Node7300",
          "q_neukirchen.zml",
          "577501",
          "P" },
      //17
      new String[]
      {
          "Goessnitz",
          "Node7200",
          "q_goessnitz.zml",
          "577510",
          "P" },
      // Z1
      new String[]
      {
          "Droeda",
          "Node6100",
          "q_droeda.zml",
          "577050", // TODO nicht in PSI enthalten
          "-Z" },
      //Z2
      new String[]
      {
          "Magwitz",
          "Node1500",
          null, // TODO
          "576420",
          "-Z" },
      //Z3
      new String[]
      {
          "Poehl",
          "Node1301",
          null, // TODO
          "577110",
          "-Z" },
      //Z4
      new String[]
      {
          "Boehlen",
          "Node7100",
          "q_boehlen.zml",
          "577571", // TODO nicht in PSI enthalten
          "-Z" },
      //Z5 ist auch 16 Koberbach
      // TODO darf nicht gleichzeitig zufluss und pegel sein
      new String[]
      {
          "Neukirchen",
          "Node7300",
          "q_neukirchen.zml",
          "577501",
          "Z" }, };

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
      fe.setProperty( FeatureFactory.createFeatureProperty( "name", name ) );
      //zuflussRep
      if( type.indexOf( "Z" ) > -1 )
      {
        if( available )
        {
          TimeseriesLink zuflussRep = NAZMLGenerator.generateobsLink(
              WeisseElsterConstants.PREFIX_LINK_WQ_Zufluss_Rep + psiID,
              NAZMLGenerator.NA_LINK_WQ );
          setTSLink( fe, "zuflussZRRepository", zuflussRep );
        }
        else
        {
          TimeseriesLink zuflussRep = NAZMLGenerator.generateobsLink(
              WeisseElsterConstants.ALTERNATIV_PREFIX_LINK_WQ_Zufluss_Rep + fId,
              NAZMLGenerator.NA_LINK_WQ );
          setTSLink( fe, "zuflussZRRepository", zuflussRep );
        }
      }
      //zuflussRepVorhersage
      if( type.indexOf( "Z" ) > -1 )
      {
        if( available )
        {
          TimeseriesLink zuflussRepVorhersage = NAZMLGenerator.generateobsLink(
              WeisseElsterConstants.PREFIX_LINK_WQ_Zufluss_Rep_Vorhersage + psiID,
              NAZMLGenerator.NA_LINK_WQ );
          setTSLink( fe, "zuflussZRRepositoryVorhersage", zuflussRepVorhersage );
        }
        else
        {
          TimeseriesLink zuflussRepVorhersage = NAZMLGenerator.generateobsLink(
              WeisseElsterConstants.ALTERNATIV_PREFIX_LINK_WQ_Zufluss_Rep + fId,
              NAZMLGenerator.NA_LINK_WQ );
          setTSLink( fe, "zuflussZRRepositoryVorhersage", zuflussRepVorhersage );
        }
          // zufluss lokal
          TimeseriesLink linkZufluss = NAZMLGenerator.generateobsLink(
              WeisseElsterConstants.PREFIX_LINK_WQ_ZUFLUSS_LOKAL + fe.getId() + ".zml",
              NAZMLGenerator.NA_LINK_WQ );
          setTSLink( fe, "zuflussZR", linkZufluss );
      }
      //pegelRep
      if( type.indexOf( "P" ) > -1)        
      {
        if(available)
        {
        TimeseriesLink pegelRep = NAZMLGenerator
            .generateobsLink( WeisseElsterConstants.PREFIX_LINK_WQ_Pegel_Rep + psiID,
                NAZMLGenerator.NA_LINK_WQ );
        setTSLink( fe, "pegelZRRepository", pegelRep );
        }
        else if(zml!=null)
        {
          TimeseriesLink pegelRep = NAZMLGenerator.generateobsLink(
              WeisseElsterConstants.ALTERNATIV_PREFIX_LINK_WQ_Pegel_Rep + zml,
              NAZMLGenerator.NA_LINK_WQ );
          setTSLink( fe, "pegelZRRepository", pegelRep );
          }
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
      TimeseriesLink linkPegel = NAZMLGenerator.generateobsLink(
          WeisseElsterConstants.PREFIX_LINK_WQ_PEGEL_LOKAL + fe.getId() + ".zml",
          NAZMLGenerator.NA_LINK_WQ );
      setTSLink( fe, "pegelZR", linkPegel );
      // berechnet
      TimeseriesLink linkBerechnet = NAZMLGenerator.generateobsLink(
          WeisseElsterConstants.PREFIX_LINK_WQ_BERECHNET_LOKAL + fe.getId() + ".zml",
          NAZMLGenerator.NA_LINK_WQ );
      setTSLink( fe, "qberechnetZR", linkBerechnet );
      
      setTSLink( fe, "zuflussZR", null );
      setTSLink( fe, "pegelZRRepository",null);
      setTSLink( fe, "zuflussZRRepository",null);
      setTSLink( fe, "zuflussZRRepositoryVorhersage",null);
      FeatureProperty nameProp=FeatureFactory.createFeatureProperty("name",null);
      fe.setProperty(nameProp);
      //      final TimeseriesLink messPegel =
      // (TimeseriesLink)feature.getProperty( "pegelZRRepository" );
      //      if( messPegel != null )
      //      {
      //        messPegel.setHref( WeisseElsterConstants.PREFIX_LINK_FLUSSPEGEL +
      // feature.getId() );
      //        if( m_availablePegel.indexOf( feature.getId() ) < 0 )
      //          feature.setProperty( null );
      //      }
      //      TimeseriesLink zuflussPegel = (TimeseriesLink)feature.getProperty(
      // "zuflussZRRepository" );
      //      if( zuflussPegel != null )
      //        zuflussPegel.setHref( WeisseElsterConstants.PREFIX_LINK_ZUFLUSSPEGEL +
      // feature.getId() );
    }
  }

  private static void setTSLink( Feature fe, String propName, TimeseriesLink tsLink )
  {
    fe.setProperty( FeatureFactory.createFeatureProperty( propName, tsLink ) );
  }
  // TODO
  //    <temperaturZRRepository/>
  //    <temperaturZRRepositoryVorhersage/>
  //    <temperaturZR/>
}