package org.kalypso.ui.calcwizard.bericht;

import java.util.List;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureList;
import org.kalypso.eclipse.jface.wizard.ArrayChooserPage;
import org.kalypso.java.util.Arrays;
import org.kalypso.services.proxy.DocBean;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.metadoc.ExportBerichtWizard;
import org.kalypso.ui.metadoc.IExportableDocument;
import org.kalypso.ui.wizard.feature.ChooseFeaturePage;

/**
 * @author belger
 */
public class ExportWizardBerichtWizard extends ExportBerichtWizard
{
  private final FeatureList m_features;
  private final String m_nameProperty;
  private ChooseFeaturePage m_chooseFeaturePage;
  private final List m_checkedFeatures;
  private Feature[] m_choosenFeatures;
  private IBerichtExporter[] m_choosenExporter;
  private final IBerichtExporter[] m_exporter;
  private ArrayChooserPage m_chooseFromListPage;
  

  public ExportWizardBerichtWizard( final FeatureList features, final List checkedFeatures, final String nameProperty, final IExportableDocument document2export, final DocBean doc, final IBerichtExporter[] exporter )
  {
    super( document2export, doc );
    
    m_features = features;
    m_checkedFeatures = checkedFeatures;
    m_nameProperty = nameProperty;
    m_exporter = exporter;
  }
  
  /**
   * @see org.kalypso.ui.metadoc.ExportBerichtWizard#addPages()
   */
  public void addPages()
  {
    m_chooseFeaturePage = new ChooseFeaturePage( m_features, null, m_checkedFeatures.toArray(), m_nameProperty, "chooseFeatures", "Für diese Vorhersagepegel werden die Berichte erzeugt:", ImageProvider.IMAGE_UTIL_BERICHT_WIZ );
    m_chooseFromListPage = new ArrayChooserPage( m_exporter, "arrayChooser", "Diese Berichtsarten werden erzeugt:", ImageProvider.IMAGE_UTIL_BERICHT_WIZ ); 
    
    addPage( m_chooseFeaturePage );
    addPage( m_chooseFromListPage );
    
    super.addPages();
  }
  
  /**
   * @see org.kalypso.ui.metadoc.ExportBerichtWizard#performFinish()
   */
  public boolean performFinish()
  {
    if( !super.performFinish() )
      return false;
    
    m_choosenFeatures = m_chooseFeaturePage.getSelected();
    m_choosenExporter = (IBerichtExporter[])Arrays.castArray( m_chooseFromListPage.getChoosen(), new IBerichtExporter[0] );
    
    return true;
  }
  
  public Feature[] getChoosenFeatures()
  {
    return m_choosenFeatures;
  }

  public IBerichtExporter[] getChoosenExporter()
  {
    return m_choosenExporter;
  }
}
