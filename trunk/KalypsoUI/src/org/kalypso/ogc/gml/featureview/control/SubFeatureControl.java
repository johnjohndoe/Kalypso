package org.kalypso.ogc.gml.featureview.control;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.Assert;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.maker.IFeatureviewFactory;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author Gernot Belger
 */
public class SubFeatureControl extends AbstractFeatureControl
{
  private IFeatureControl m_fc;

  private final IFeatureSelectionManager m_selectionManager;

  private final FormToolkit m_formToolkit;

  private final boolean m_showOk;

  private final IFeatureviewFactory m_featureviewFactory;

  private QName m_selector;

  public SubFeatureControl( final IPropertyType ftp, final IFeatureSelectionManager selectionManager, final FormToolkit formToolkit, final boolean showOk, final IFeatureviewFactory featureviewFactory )
  {
    this( ftp, selectionManager, formToolkit, showOk, featureviewFactory, null );
  }

  public SubFeatureControl( final IPropertyType ftp, final IFeatureSelectionManager selectionManager, final FormToolkit formToolkit, final boolean showOk, final IFeatureviewFactory featureviewFactory, final String selector )
  {
    super( ftp );
    m_selectionManager = selectionManager;
    m_formToolkit = formToolkit;
    m_showOk = showOk;
    m_featureviewFactory = featureviewFactory;
    if( selector != null && ftp != null )
      // TODO: NO! Please ALWAYS use full qnames inside the .gft!! The namespace of the property is not always the
      // namespace of the feature!!
      // You can even define the selector attribute to be of type QNAME! You dont have to parse anything yourself!
      // BEST HERE: use gml-xpath like the enabledOperation attribute of the ControlType.
      // Search for 'evaluateOperation' in the FeatureComposite class for an example on how to use it.
      m_selector = new QName( ftp.getQName().getNamespaceURI(), selector );
    else
      m_selector = null;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#createControl(org.eclipse.swt.widgets.Composite, int)
   */
  public Control createControl( final Composite parent, final int style )
  {
    try
    {
      final Feature featureToSet = findFeatuereToSet();

      /* crerate the control */
      if( featureToSet == null )
      {
        // TODO: If selector is present, just create an empty control

        m_fc = new ButtonFeatureControl( getFeature(), getFeatureTypeProperty() );
      }
      else
      {
        final FeatureComposite fc = new FeatureComposite( featureToSet, m_selectionManager, m_featureviewFactory );
        fc.setFormToolkit( m_formToolkit );
        fc.setShowOk( m_showOk );

        m_fc = fc;
      }
    }
    catch( final Throwable t )
    {
      // TODO: Create text feature control with error message!
    }

    m_fc.addChangeListener( new IFeatureChangeListener()
    {
      public void featureChanged( final FeatureChange[] changes )
      {
        fireFeatureChange( changes );
      }

      public void openFeatureRequested( final Feature featureToOpen, final IPropertyType ftpToOpen )
      {
        fireOpenFeatureRequested( featureToOpen, ftpToOpen );
      }
    } );

    return m_fc.createControl( parent, SWT.NONE );
  }

  private Feature findFeatuereToSet( )
  {
    final Feature feature = getFeature();
    final IPropertyType ftp = getFeatureTypeProperty();
    final IRelationType rt = (IRelationType) ftp;

    // find feature to set to the sub-FeatureControl
    final Feature featureToSet;
    if( m_selector == null )
    {
      Assert.isTrue( !rt.isList() );

      final Object property = feature.getProperty( rt );
      featureToSet = FeatureHelper.getFeature( feature.getWorkspace(), property );
    }
    else
    {
// try
// {
      final Object link = feature.getProperty( m_selector );
      // TODO: also handle external links
      // Use a HELPER method for that, dont just put it here! (see FeatureHelper.getFeature)
      featureToSet = FeatureHelper.getFeature( feature.getWorkspace(), link );
// }
// catch( final Exception e )
// {
// // Do not do this!
// // This could be a property of the Combo-Feature-Control to select the first element, if nothing is selected
// beforehand
//          
// f = ((Feature) ((List) property).get( 0 ));
// // final String xpath = feature.getWorkspace().getContext() + "#" + f.getId();
// // final XLinkedFeature_Impl linkedFeature = new XLinkedFeature_Impl( feature, f.getParentRelation(),
// // f.getFeatureType(), xpath, "", "", "", "", "" );
// // System.out.println( "WARNING: control.gml, xlink to active model broken, first model used as default;
// // temp xlink
// // set
// // to " + xpath );
// // TODO: also handle external links
// // Use a HELPER method for that, dont just put it here! (see FeatureHelper.getFeature)
// feature.setProperty( m_selector, f.getId() );
// }
    }
    return featureToSet;
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
    // TODO: this is not always enough
    // Better: cdestroy m_fc and recreate it from scratch
    // In order to to this, m_fc must be put into an extra Composite (when createComposite is called)

    final Feature findFeatureToSet = findFeatuereToSet();
    m_fc.setFeature( findFeatureToSet );
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
  public void addModifyListener( final ModifyListener l )
  {
    m_fc.addModifyListener( l );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#removeModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void removeModifyListener( final ModifyListener l )
  {
    m_fc.removeModifyListener( l );
  }

  /** Returns the used feature control. */
  public IFeatureControl getFeatureControl( )
  {
    return m_fc;
  }

}