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

    public UpdateModell(URL modellURL) throws Exception
    {
        m_modellURL = modellURL;
    }

    public void updateIt() throws Exception
    {
        URL schemaURL = KalypsoNADefaultSchema.getInstance()
                .getDefaultNaModellSchemaURL();
        GMLWorkspace workspace = GmlSerializer.createGMLWorkspace(m_modellURL,
                schemaURL);
        updateRepositoryLinks(workspace);
        File file = File.createTempFile("modellUpdate", ".gml");
        Writer writer = new FileWriter(file);
        GmlSerializer.serializeWorkspace(writer, workspace);
        writer.close();
        System.out.println(" updated model is written to "
                + file.getCanonicalPath());
    }

    public static void main(String[] args)
    {
        try
        {
            final ITypeRegistry registry = TypeRegistrySingleton
                    .getTypeRegistry();
            registry.registerTypeHandler(new ObservationLinkHandler());
            File modell = new File("C:\\TMP\\modell.gml");
            URL modellURL = modell.toURL();
            UpdateModell modell2 = new UpdateModell(modellURL);
            modell2.updateIt();
        } catch (Exception e)
        {
            e.printStackTrace();
        }
    }

    public static void updateRepositoryLinks(GMLWorkspace workspace)
    {
        // Catchments...
        final FeatureType catchmentFT = workspace.getFeatureType("Catchment");
        final Feature[] catchmentFEs = workspace.getFeatures(catchmentFT);
        updateCatchments(catchmentFEs);
        updateCatchments(catchmentFEs);
        updatePegel(workspace);
        final FeatureType nodeFT = workspace.getFeatureType("Node");
        final Feature[] nodeFEs = workspace.getFeatures(nodeFT);
        updateNodes(nodeFEs);
    }

    private static void updateCatchments(Feature[] features)
    {
        for (int i = 0; i < features.length; i++)
        {
            final Feature feature = features[i];
            TimeseriesLink tsLink = (TimeseriesLink) feature
                    .getProperty("niederschlagZRRepository");
            tsLink
                    .setHref(WeisseElsterConstants.PREFIX_LINK_GebietsNiederschlagModell
                            + feature.getId());
            // TODO niederschlagZRRepositoryVorhersage
            // erstmal auf gemessen gesetzt
            FeatureProperty property = FeatureFactory.createFeatureProperty(
                    "niederschlagZRRepositoryVorhersage", tsLink);
            feature.setProperty(property);
        }
    }
    
    private final static String[][] m_zufluss=
    {
    new String[]{"Neukirchen","Node7300"},
    new String[]{"Poehl","Node1301"},
    new String[]{"Droeda","Node6100"},
    new String[]{"Boehlen","Node7100"},
    new String[]{"Magwitz","Node1500"}
    };
    
    private static void updateZuflussNamen(GMLWorkspace workspace)
    {
        for (int i = 0; i < m_zufluss.length; i++)
        {
            final String[] zuflussContext = m_zufluss[i];
            final String zuflussID = zuflussContext[1];
            final Feature feature = workspace.getFeature(zuflussID);
            final FeatureProperty nameProp = FeatureFactory.createFeatureProperty(
                    "name", zuflussContext[0]);
            feature.setProperty(nameProp);
        }
    }
    private final static String[][] m_pegel =
    {
    // kaputte pegel und noch nicht verfuegbare pegel sind kommentiert
            //  new String[]{"Bad Elster","Node1800","adorf.zml"},
            new String[]
            { "Adorf", "Node1700", "adorf.zml" }, new String[]
            { "Oelsnitz", "Node1600", "oelsnitz.zml" }, new String[]
            { "Strassberg", "Node1401", "strassberg.zml" }, new String[]
            { "Elsterberg", "Node1300", "elsterberg.zml" },
            //  new String[]{"Rodewisch","Node4200","rodewisch.zml"},
            new String[]
            { "Mylau", "Node4100", "mylau.zml" }, new String[]
            { "Greiz", "Node1220", "greiz.zml" }, new String[]
            { "Weida", "Node2002", "weida.zml" }, new String[]
            { "Gera", "Node1210", "gera.zml" },
            //  new String[]{"Zeitz","Node1110","zeitz.zml"},
            new String[]
            { "Kleindalzig", "Node1020", "kleindalzig.zml" },
            //  new String[]{"Albrechtshain","Node3201","albrechtshain.zml"},
            new String[]
            { "Leipzig-Thekla", "Node3100", "leipzig-thekla.zml" },
            new String[]
            { "Oberthau", "Node1001", "oberthau.zml" }, new String[]
            { "Neukirchen", "Node7300", "neukirchen.zml" }, new String[]
            { "Goessnitz", "Node7200", "goessnitz.zml" } };

    private static void updatePegel(GMLWorkspace workspace)
    {
        for (int i = 0; i < m_pegel.length; i++)
        {
            final String[] pegelContext = m_pegel[i];
            final String nodeID = pegelContext[1];
            final Feature feature = workspace.getFeature(nodeID);
            final FeatureProperty nameProp = FeatureFactory.createFeatureProperty(
                    "name", pegelContext[0]);
            feature.setProperty(nameProp);
            try
            {
                TimeseriesLink link = NAZMLGenerator.generateobsLink(
                        WeisseElsterConstants.PREFIX_LINK_FLUSSPEGEL
                                + pegelContext[2],
                        NAZMLGenerator.NA_PEGEL_MESSUNG);
                FeatureProperty linkProp = FeatureFactory
                        .createFeatureProperty("pegelZRRepository", link);
                feature.setProperty(linkProp);
            } catch (Exception e)
            {
                e.printStackTrace();
            }

        }

    }

    private static void updateNodes(Feature[] features)
    {
        for (int i = 0; i < features.length; i++)
        {
            final Feature feature = features[i];
            //      final TimeseriesLink messPegel =
            // (TimeseriesLink)feature.getProperty( "pegelZRRepository" );
            //      if( messPegel != null )
            //      {
            //        messPegel.setHref( WeisseElsterConstants.PREFIX_LINK_FLUSSPEGEL +
            // feature.getId() );
            //        if( m_availablePegel.indexOf( feature.getId() ) < 0 )
            //          feature.setProperty( null );
            //      }
            TimeseriesLink zuflussPegel = (TimeseriesLink) feature
                    .getProperty("zuflussZRRepository");
            if (zuflussPegel != null)
                zuflussPegel
                        .setHref(WeisseElsterConstants.PREFIX_LINK_ZUFLUSSPEGEL
                                + feature.getId());
        }
    }

    // TODO
    //    <temperaturZRRepository/>
    //    <temperaturZRRepositoryVorhersage/>
    //    <temperaturZR/>
}