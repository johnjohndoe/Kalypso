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
    GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( modellURL,
        schemaURL );
    updateRepositoryLink( workspace );
    File file = File.createTempFile( "model", ".gml" );
    Writer writer = new FileWriter( file );
    GmlSerializer.serializeWorkspace( writer, workspace );
    writer.close();
    System.out.println( " model is written to " + file.getCanonicalPath() );
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

  public static void updateRepositoryLink( GMLWorkspace workspace )
  {
    final FeatureType featureType = workspace.getFeatureType( "Catchment" );
    final Feature[] features = workspace.getFeatures( featureType );
    for( int i = 0; i < features.length; i++ )
    {
      final Feature feature = features[i];
      TimeseriesLink tsLink = (TimeseriesLink)feature.getProperty("niederschlagZRRepository");
      tsLink.setHref( WeisseElsterConstants.PREFIX_LINK_GebietsNiederschlagModell
          + feature.getId());      
    }
  }
}