package org.kalypso.ogc.gml.featureview.control;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.maker.IFeatureviewFactory;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class SubFeatureControl extends AbstractFeatureControl
{
  private IFeatureControl m_fc;

  private final IFeatureSelectionManager m_selectionManager;

  private final FormToolkit m_formToolkit;

  private final IFeatureviewFactory m_featureviewFactory;

  public SubFeatureControl( final IPropertyType ftp, final IFeatureSelectionManager selectionManager, final FormToolkit formToolkit, final IFeatureviewFactory featureviewFactory )
  {
    super( ftp );
    
    m_selectionManager = selectionManager;
    m_formToolkit = formToolkit;
    m_featureviewFactory = featureviewFactory;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#createControl(org.eclipse.swt.widgets.Composite, int)
   */
  public Control createControl( final Composite parent, int style )
  {
    final Feature feature = getFeature();
    final Object property = feature.getProperty( getFeatureTypeProperty() );
    if( property instanceof Feature )
    {
      final FeatureComposite fc = new FeatureComposite( (Feature) property, m_selectionManager, m_featureviewFactory );

      /* Set the toolkit to the FeatureComposite. The check for null is perfomrmed in FeatureComposite. */
      fc.setFormToolkit( m_formToolkit );

      m_fc = fc;
    }
    else
    {
      m_fc = new ButtonFeatureControl( feature, getFeatureTypeProperty() );
    }

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
  @Override
  public void dispose( )
  {
    m_fc.dispose();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#updateControl()
   */
  public void updateControl( )
  {
    m_fc.updateControl();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#isValid()
   */
  public boolean isValid( )
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

  /** Returns the used feature control. */
  public IFeatureControl getFeatureControl( )
  {
    return m_fc;
  }

}