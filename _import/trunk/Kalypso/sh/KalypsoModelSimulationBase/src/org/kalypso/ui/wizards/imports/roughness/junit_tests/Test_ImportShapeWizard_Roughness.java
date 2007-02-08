package org.kalypso.ui.wizards.imports.roughness.junit_tests;

import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URL;

import javax.xml.namespace.QName;

import junit.framework.TestCase;

import org.eclipse.jface.wizard.WizardDialog;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.wizards.imports.roughness.ImportWizard;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

public class Test_ImportShapeWizard_Roughness extends TestCase
{

  public final void testImportWizard( ) throws MalformedURLException, InvocationTargetException
  {
    final QName m_RootFeatureQName = KalypsoModelSimulationBaseConsts.SIM_BASE_F_ROUGHNESS_POLYGON_COLLECTION;
    URL outputFileURL = new URL( "file:D:/Eclipse/TESTS_RESULTS/rauheitstest.gml" );
    GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( m_RootFeatureQName, outputFileURL, GmlSerializer.DEFAULT_FACTORY );
    
    ImportWizard wizard = new ImportWizard( workspace );

    // Instantiates the wizard container with the wizard and opens it
    WizardDialog dialog = new WizardDialog( null, wizard );
    dialog.create();
    dialog.open();
  }

}
