package org.kalypso.ui.wizard.feature;

import java.util.List;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureList;
import org.deegree.model.feature.FeatureTypeProperty;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.StructuredSelection;
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
  private final List m_selectedFeatures;
  private final Object[] m_checkedFeatures;

  public ChooseFeaturePage( final FeatureList features, final List selectedFeatures, final Object[] checkedFeatures, final String nameProperty, final String pageName, final String title, final ImageDescriptor imageDescriptor )
  {
    super( pageName, title, imageDescriptor );
    m_features = features;
    m_selectedFeatures = selectedFeatures;
    m_checkedFeatures = checkedFeatures;
    
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
    if( m_selectedFeatures != null )
      m_viewer.setSelection( new StructuredSelection( m_selectedFeatures ) );
    if( m_checkedFeatures != null )
      m_viewer.setCheckedElements( m_checkedFeatures );

    setControl( m_viewer.getControl() );
  }

  public Feature[] getSelected()
  {
    final Object[] checkedElements = m_viewer.getCheckedElements();
    return (Feature[])Arrays.castArray( checkedElements, new Feature[checkedElements.length] );
  }
}
