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

  private UpdateModell() throws Exception
  {
    final ITypeRegistry registry = TypeRegistrySingleton.getTypeRegistry();
    registry.registerTypeHandler( new ObservationLinkHandler() );

    File modell = new File( "C:\\TMP\\modell.gml" );
    URL modellURL = modell.toURL();
    URL schemaURL = KalypsoNADefaultSchema.getInstance().getDefaultNaModellSchemaURL();
    GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( modellURL, schemaURL );
    updateRepositoryLinks( workspace );
    File file = File.createTempFile( "modellUpdateTSLink", ".gml" );
    Writer writer = new FileWriter( file );
    GmlSerializer.serializeWorkspace( writer, workspace );
    writer.close();
    System.out.println( " updated model is written to " + file.getCanonicalPath() );
  }

  public static void main( String[] args )
  {
    try
    {
      new UpdateModell();
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  public static void updateRepositoryLinks( GMLWorkspace workspace )
  {
    // Catchments...
    final FeatureType catchmentFT = workspace.getFeatureType( "Catchment" );
    final Feature[] catchmentFEs = workspace.getFeatures( catchmentFT );
    updateCatchments( catchmentFEs );
    updateCatchments( catchmentFEs );

    final FeatureType nodeFT = workspace.getFeatureType( "Node" );
    final Feature[] nodeFEs = workspace.getFeatures( nodeFT );
    updateNodes( nodeFEs );
  }

  private static void updateCatchments( Feature[] features )
  {
    for( int i = 0; i < features.length; i++ )
    {
      final Feature feature = features[i];
      TimeseriesLink tsLink = (TimeseriesLink)feature.getProperty( "niederschlagZRRepository" );
      tsLink
          .setHref( WeisseElsterConstants.PREFIX_LINK_GebietsNiederschlagModell + feature.getId() );
      // TODO niederschlagZRRepositoryVorhersage
      // erstmal auf gemessen gesetzt
      FeatureProperty property = FeatureFactory.createFeatureProperty(
          "niederschlagZRRepositoryVorhersage", tsLink );
      feature.setProperty( property );
    }
  }

  private final static String m_availablePegel = "Node1600 Node1302 Node1401 Node1300";

  private static void updateNodes( Feature[] features )
  {
    for( int i = 0; i < features.length; i++ )
    {
      //    <pegelZRRepository/>
      //    "kalypso-ocs:WeisseElster://Pegel/Pegel_Node1600.zml"
      final Feature feature = features[i];
      TimeseriesLink messPegel = (TimeseriesLink)feature.getProperty( "pegelZRRepository" );
      if( messPegel != null )
      {
        messPegel.setHref( WeisseElsterConstants.PREFIX_LINK_FLUSSPEGEL + feature.getId() );
        if( m_availablePegel.indexOf( feature.getId() ) < 0 )
          feature.setProperty( null );
      }
      TimeseriesLink zuflussPegel = (TimeseriesLink)feature.getProperty( "zuflussZRRepository" );
      if( zuflussPegel != null )
        zuflussPegel.setHref( WeisseElsterConstants.PREFIX_LINK_ZUFLUSSPEGEL + feature.getId() );
    }
  }

  // TODO
  //    <temperaturZRRepository/>
  //    <temperaturZRRepositoryVorhersage/>
  //    <temperaturZR/>
}