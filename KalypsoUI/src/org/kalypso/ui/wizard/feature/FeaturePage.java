package org.kalypso.ui.wizard.feature;

import java.net.URL;
import java.util.Collection;

import org.deegree.model.feature.Feature;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.kalypso.ogc.gml.featureview.FeatureComposite;

/**
 * Wizard-Page zur Eingabe der Steuerparameter
 * 
 * @author belger
 */
public class FeaturePage extends WizardPage
{
  private FeatureComposite m_featureComposite;

  private boolean m_overrideCanFlipToNextPage;

  private Feature m_feature;

  public FeaturePage( final String pagename, final String title, final ImageDescriptor image,
      final boolean overrideCanFlipToNextPage, final Feature feature )
  {
    super( pagename, title, image );

    m_overrideCanFlipToNextPage = overrideCanFlipToNextPage;
    m_feature = feature;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Group group = new Group( parent, SWT.NONE );
    group.setLayout( new GridLayout() );
    group.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    group.setText( getTitle() );

    m_featureComposite = new FeatureComposite( null, new URL[] {} );
    m_featureComposite.setFeature( m_feature );
    final Control control = m_featureComposite.createControl( group, SWT.NONE );
    control.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    
    setControl( group );
  }

  public void setFeature( final Feature feature )
  {
    m_feature = feature;
    m_featureComposite.setFeature( feature );
    m_featureComposite.updateControl();
  }

  /**
   * @see org.eclipse.jface.wizard.WizardPage#canFlipToNextPage()
   */
  public boolean canFlipToNextPage()
  {
    // TODO: wenn feature-edit valid ist
    if( m_overrideCanFlipToNextPage )
      return isPageComplete();

    return super.canFlipToNextPage();
  }

  public void collectChanges( final Collection changes )
  {
    m_featureComposite.collectChanges( changes );
  }
}