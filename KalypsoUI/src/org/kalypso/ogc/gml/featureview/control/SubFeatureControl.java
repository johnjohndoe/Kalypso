package org.kalypso.ogc.gml.featureview.control;

import java.util.Collection;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.ogc.gml.featureview.FeatureComposite;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.IFeatureControl;
import org.kalypso.template.featureview.FeatureviewType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author belger
 */
public class SubFeatureControl extends AbstractFeatureControl
{
  private IFeatureControl m_fc;

  private final FeatureviewType[] m_views;

  public SubFeatureControl( final GMLWorkspace workspace, final FeatureTypeProperty ftp, final FeatureviewType[] views )
  {
    super( workspace, ftp );
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
      m_fc = new FeatureComposite( workspace, (Feature)property, m_views );
    else
      m_fc = new ButtonFeatureControl( workspace, feature, getFeatureTypeProperty() );
    //    final ScrolledComposite scrolledComposite = new ScrolledComposite( parent, SWT.H_SCROLL
    //        | SWT.V_SCROLL );

    // don't forget this line!
    //    scrolledComposite.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    //    final Control control = m_fc.createControl( scrolledComposite, SWT.NONE );
    final Control control = m_fc.createControl( parent, SWT.NONE );
    //    control.setSize( control.computeSize( SWT.DEFAULT, SWT.DEFAULT ) );
    //    scrolledComposite.setContent( control );

    return control;
    //    return scrolledComposite;
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
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#collectChanges(java.util.Collection)
   */
  public void collectChanges( Collection c )
  {
    m_fc.collectChanges( c );
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

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#addChangeListener(org.kalypso.ogc.gml.featureview.IFeatureChangeListener)
   */
  public void addChangeListener( IFeatureChangeListener l )
  {
    m_fc.addChangeListener( l );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#removeChangeListener(org.kalypso.ogc.gml.featureview.IFeatureChangeListener)
   */
  public void removeChangeListener( IFeatureChangeListener l )
  {
    m_fc.removeChangeListener( l );
  }

}