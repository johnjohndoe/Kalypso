package org.kalypso.ogc.gml.featureview.control;

import java.util.List;

import javax.xml.namespace.QName;

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
    final Feature feature = getFeature();
    final IPropertyType ftp = getFeatureTypeProperty();
    final Object property = feature.getProperty( ftp );
    if( property instanceof Feature )
    {
      final FeatureComposite fc = new FeatureComposite( (Feature) property, m_selectionManager, m_featureviewFactory );

      /* Set the toolkit to the FeatureComposite. The check for null is perfomrmed in FeatureComposite. */
      fc.setFormToolkit( m_formToolkit );
      fc.setShowOk( m_showOk );

      m_fc = fc;
    }
    else
    {
      if( m_selector != null && ftp instanceof IRelationType && ftp.isList() )
      {
        // TODO: This is not so good!
        // The SubFeatureControl gets tweaked and shows a subfeature instead of the specified one...
        // Better would be to implement an extra feature control that shows a combo-box and a specified feature out of a
        // list.

        Feature f = null;
        try
        {
          final Object link = feature.getProperty( m_selector );
          // TODO: also handle external links
          // Use a HELPER method for that, dont just put it here! (see FeatureHelper.getFeature)
          f = FeatureHelper.getFeature( feature.getWorkspace(), link );
        }
        catch( final Exception e )
        {
          f = ((Feature) ((List) property).get( 0 ));
// final String xpath = feature.getWorkspace().getContext() + "#" + f.getId();
// final XLinkedFeature_Impl linkedFeature = new XLinkedFeature_Impl( feature, f.getParentRelation(),
// f.getFeatureType(), xpath, "", "", "", "", "" );
// System.out.println( "WARNING: control.gml, xlink to active model broken, first model used as default; temp xlink set
// to " + xpath );
          // TODO: also handle external links
          // Use a HELPER method for that, dont just put it here! (see FeatureHelper.getFeature)
          feature.setProperty( m_selector, f.getId() );
        }
        final FeatureComposite fc = new FeatureComposite( f, m_selectionManager, m_featureviewFactory );
        fc.setFormToolkit( m_formToolkit );
        fc.setShowOk( m_showOk );

        m_fc = fc;
      }
      else
        m_fc = new ButtonFeatureControl( feature, ftp );
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
    // TODO: that does not work!
    // SubFeature does not get updated... see the timeserie for example
    // Better: make an own SubFeatureCoontrol implementation with an own
    // combo box instead which contains a featureComposite wich
    // gets recreated completely if something changes
    final Feature feature = getFeature();
    final IPropertyType ftp = getFeatureTypeProperty();
    if( m_selector != null && ftp instanceof IRelationType && ftp.isList() )
    {
      final Object link = feature.getProperty( m_selector );
      // TODO: also handle external links
      // Use a HELPER method for that, dont just put it here! (see FeatureHelper.getFeature)
      final Feature f = FeatureHelper.getFeature( feature.getWorkspace(), link );
      m_fc.setFeature( f );
    }
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