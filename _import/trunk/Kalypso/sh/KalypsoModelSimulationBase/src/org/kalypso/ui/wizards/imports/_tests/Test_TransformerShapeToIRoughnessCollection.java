package org.kalypso.ui.wizards.imports._tests;

import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URL;

import javax.xml.namespace.QName;

import junit.framework.TestCase;

import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.wizards.imports.roughness.DataContainer;
import org.kalypso.ui.wizards.imports.roughness.Transformer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

public class Test_TransformerShapeToIRoughnessCollection extends TestCase
{

  public final void testConvertShp2Gml( ) throws MalformedURLException, InvocationTargetException
  {
    final QName m_RootFeatureQName = KalypsoModelSimulationBaseConsts.SIM_BASE_F_ROUGHNESS_POLYGON_COLLECTION;
    URL outputFileURL = new URL( "file:D:/Eclipse/TESTS_RESULTS/rauheitstest.gml" );
    GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( m_RootFeatureQName, outputFileURL, GmlSerializer.DEFAULT_FACTORY );
    DataContainer data = new DataContainer();
//    data.setWorkspace( workspace );

    data.setInputFile( "D:/Eclipse/Test/Roughness/rauheitstest.shp" );
    data.setShapeProperty( "RAUHEITSKL" );

    Transformer t = new Transformer( data );
    t.execute( null );
  }

}
