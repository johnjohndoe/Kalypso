package org.kalypso.ui.calcwizard.bericht;

import java.util.List;

import org.deegree.model.feature.FeatureList;
import org.kalypso.services.proxy.DocBean;
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
  private final List m_selectedFeatures;

  public ExportWizardBerichtWizard( final FeatureList features, final List selectedFeatures, final String nameProperty, final IExportableDocument document2export, final DocBean doc )
  {
    super( document2export, doc );
    
    m_features = features;
    m_selectedFeatures = selectedFeatures;
    m_nameProperty = nameProperty;
  }
  
  /**
   * @see org.kalypso.ui.metadoc.ExportBerichtWizard#addPages()
   */
  public void addPages()
  {
    m_chooseFeaturePage = new ChooseFeaturePage( m_features, m_selectedFeatures, m_nameProperty, "chooseFeatures"  );
    
    // was wird exportiert?
    
    addPage( m_chooseFeaturePage );
    // addit
    
    super.addPages();
  }
}
