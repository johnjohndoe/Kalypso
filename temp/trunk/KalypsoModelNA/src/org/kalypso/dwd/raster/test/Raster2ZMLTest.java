package org.kalypso.dwd.raster.test;

import java.io.File;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.Reader;
import java.net.URL;

import junit.framework.TestCase;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree_impl.extension.ITypeRegistry;
import org.deegree_impl.extension.TypeRegistrySingleton;
import org.kalypso.convert.namodel.schema.KalypsoNADefaultSchema;
import org.kalypso.dwd.raster.Raster2ZML;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;

/**
 * @author doemming
 */
public class Raster2ZMLTest extends TestCase
{
    public void testLoadRaster()
    {
        try
        {
            final ITypeRegistry registry = TypeRegistrySingleton
                    .getTypeRegistry();
            registry.registerTypeHandler(new ObservationLinkHandler());

            final File modell = new File("/tmp/modell.gml");
            final URL modellURL = modell.toURL();
            final URL schemaURL = KalypsoNADefaultSchema.getInstance()
                    .getDefaultNaModellSchemaURL();
            final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace(
                    modellURL, schemaURL);
            final Feature[] features = workspace.getFeatures(workspace
                    .getFeatureType("Catchment"));

            final Raster2ZML raster2ZML = new Raster2ZML(new File("/tmp/"));
            final Reader r1 = new InputStreamReader(getClass()
                    .getResourceAsStream("testraster/lm_2004_10_22_00"));
            //                    .getResourceAsStream("testraster/lm_2003_07_07_00.txt"));
            final LineNumberReader r2 = new LineNumberReader(r1);
            final Reader r3 = new InputStreamReader(getClass()
                    .getResourceAsStream("testraster/lm_inv_slug"));
            final LineNumberReader r4 = new LineNumberReader(r3);
            raster2ZML.loadRaster(r2);
            raster2ZML.loadRaster(r4);
            r1.close();
            r2.close();
            r3.close();
            r4.close();
            raster2ZML.createGeoRaster();
            raster2ZML.createZML(features);
        } catch (Exception e)
        {
            e.printStackTrace();
            fail("could not convert Raster to ZML");
        }
    }

}