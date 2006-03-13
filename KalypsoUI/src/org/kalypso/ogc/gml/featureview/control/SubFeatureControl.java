package org.kalypso.ogc.gml.featureview.control;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypso.ogc.gml.featureview.FeatureComposite;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.IFeatureControl;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.template.featureview.FeatureviewType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author belger
 */
public class SubFeatureControl extends AbstractFeatureControl
{
  private IFeatureControl m_fc;

  private final FeatureviewType[] m_views;

  private final IFeatureSelectionManager m_selectionManager;

  public SubFeatureControl( final GMLWorkspace workspace, final IPropertyType ftp, final IFeatureSelectionManager selectionManager, final FeatureviewType[] views )
  {
    super( workspace, ftp );
    m_selectionManager = selectionManager;
    m_views = views;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#createControl(org.eclipse.swt.widgets.Composite, int)
   */
  public Control createControl( final Composite parent, int style )
  {
    final Feature feature = getFeature();
    final GMLWorkspace workspace = getWorkspace();
    final Object property = feature.getProperty( getFeatureTypeProperty().getName() );
    if( property instanceof Feature )
      m_fc = new FeatureComposite( workspace, (Feature)property, m_selectionManager, m_views );
    else
      m_fc = new ButtonFeatureControl( workspace, feature, getFeatureTypeProperty(), m_selectionManager );

    m_fc.addChangeListener( new IFeatureChangeListener()
    {
      public void featureChanged( final FeatureChange change )
      {
        fireFeatureChange( change );
      }

      public void openFeatureRequested( final Feature featureToOpen, final IPropertyType ftpToOpen )
      {
        fireOpenFeatureRequested( featureToOpen, ftpToOpen );
      }
    } );

    final Control control = m_fc.createControl( parent, SWT.NONE );
    return control;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#dispose()
   */
  public void dispose()
  {
    m_fc.dispose();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#updateControl()
   */
  public void updateControl()
  {
    m_fc.updateControl();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#isValid()
   */
  public boolean isValid()
  {
    return m_fc.isValid();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#addModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void addModifyListener( ModifyListener l )
  {
    m_fc.addModifyListener( l );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#removeModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void removeModifyListener( ModifyListener l )
  {
    m_fc.removeModifyListener( l );
  }

}