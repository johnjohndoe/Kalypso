package org.kalypso.ui.wizard.feature;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureList;
import org.deegree.model.feature.FeatureTypeProperty;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.java.util.Arrays;
import org.kalypso.ogc.gml.featureview.modfier.StringModifier;
import org.kalypso.ogc.gml.util.FeatureLabelProvider;

/**
 * @author belger
 */
public class ChooseFeaturePage extends WizardPage
{
  private final FeatureList m_features;
  private final FeatureTypeProperty m_ftp;
  private CheckboxTableViewer m_viewer;

  public ChooseFeaturePage( final FeatureList features, final String nameProperty, final String pageName )
  {
    super( pageName );
    m_features = features;
    
    if( m_features != null && m_features.size() != 0 )
    {
      final Feature f = (Feature)m_features.get( 0 );
      m_ftp = f.getFeatureType().getProperty( nameProperty );
    }
    else
      m_ftp = null;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    m_viewer = CheckboxTableViewer.newCheckList( parent, SWT.BORDER );
    m_viewer.setContentProvider( new ArrayContentProvider() );
    m_viewer.setLabelProvider( new FeatureLabelProvider( new StringModifier( m_ftp ) ) );
    m_viewer.setInput( m_features );

    setControl( m_viewer.getControl() );
  }

  public Feature[] getSelected()
  {
    final Object[] checkedElements = m_viewer.getCheckedElements();
    return (Feature[])Arrays.castArray( checkedElements, new Feature[checkedElements.length] );
  }
}
