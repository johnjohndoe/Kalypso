/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.featureview.control;

import java.lang.reflect.Method;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBElement;
import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.util.SafeRunnable;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.adapter.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.AnnotationUtilities;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.maker.IFeatureviewFactory;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.template.featureview.Button;
import org.kalypso.template.featureview.Checkbox;
import org.kalypso.template.featureview.Combo;
import org.kalypso.template.featureview.CompositeType;
import org.kalypso.template.featureview.ControlType;
import org.kalypso.template.featureview.FeatureviewType;
import org.kalypso.template.featureview.GridDataType;
import org.kalypso.template.featureview.LabelType;
import org.kalypso.template.featureview.LayoutDataType;
import org.kalypso.template.featureview.LayoutType;
import org.kalypso.template.featureview.PropertyControlType;
import org.kalypso.template.featureview.Radiobutton;
import org.kalypso.template.featureview.SubcompositeType;
import org.kalypso.template.featureview.Table;
import org.kalypso.template.featureview.Text;
import org.kalypso.template.featureview.TupleResult;
import org.kalypso.template.featureview.ValidatorLabelType;
import org.kalypso.template.featureview.Combo.Entry;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.swt.SWTUtilities;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class FeatureComposite extends AbstractFeatureControl implements IFeatureChangeListener, ModifyListener
{
  /** Used for the compability-hack. Is it possible to get this from the binding classes? */
  private static String FEATUREVIEW_NAMESPACE = "featureview.template.kalypso.org"; //$NON-NLS-1$

  private final Collection<IFeatureControl> m_featureControls = new ArrayList<IFeatureControl>();

  private final Collection<Control> m_swtControls = new ArrayList<Control>();

  private final Collection<ModifyListener> m_modifyListeners = new ArrayList<ModifyListener>( 5 );

  private Control m_control = null;

  private final IFeatureSelectionManager m_selectionManager;

  private FormToolkit m_formToolkit = null;

  private final IFeatureviewFactory m_featureviewFactory;

  /**
   * Constructs the FeatureComposite.
   * 
   * @param feature
   *          If you want to add a feature directly at instantiation time, provide it here, otherwise leave it null.
   * @param selectionManager
   *          A selection manager, which provides functionality for adding and removing a feature from an selection and
   *          it handels the registration of listerners and so on. It has to implement IFeatureSelectionManager. You can
   *          get a default one for the features here <strong>KalypsoCorePlugin.getDefault().getSelectionManager()</strong>.
   * @param featureviewFactory
   *          A factory which delivers feature-view-templates (e.g. FeatureviewHelper).
   */
  public FeatureComposite( final Feature feature, final IFeatureSelectionManager selectionManager, final IFeatureviewFactory featureviewFactory )
  {
    super( feature, null );

    m_selectionManager = selectionManager;
    m_featureviewFactory = featureviewFactory;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#updateControl()
   */
  public void updateControl( )
  {
    for( final Iterator iter = m_featureControls.iterator(); iter.hasNext(); )
    {
      final IFeatureControl fc = (IFeatureControl) iter.next();
      fc.updateControl();
    }
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#dispose()
   */
  @Override
  public void dispose( )
  {
    disposeControl();

    m_modifyListeners.clear();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#isValid()
   */
  public boolean isValid( )
  {
    for( final Iterator iter = m_featureControls.iterator(); iter.hasNext(); )
    {
      final IFeatureControl fc = (IFeatureControl) iter.next();

      if( !fc.isValid() )
        return false;
    }

    return true;
  }

  public Control createControl( final Composite parent, final int style, final IFeatureType ft )
  {
    final FeatureviewType view = m_featureviewFactory.get( ft, getFeature() );

    m_control = createControl( parent, style, view );

    /* If a toolkit is set, use it. */
    if( m_formToolkit != null )
      m_formToolkit.adapt( m_control, true, true );

    return m_control;
  }

  public Control createControl( final Composite parent, final int style )
  {
    return createControl( parent, style, getFeature().getFeatureType() );
  }

  public Control createControl( final Composite parent, final int style, final ControlType controlType )
  {
    final Control control = createControlFromControlType( parent, style, controlType );

    m_swtControls.add( control );

    control.setVisible( controlType.isVisible() );
    control.setEnabled( controlType.isEnabled() );

    // einen bereits gesetzten Tooltip nicht überschreiben
    if( control.getToolTipText() == null )
      control.setToolTipText( controlType.getTooltip() );

    final JAXBElement< ? extends LayoutDataType> jaxLayoutData = controlType.getLayoutData();
    if( jaxLayoutData != null )
    {
      final LayoutDataType layoutData = jaxLayoutData.getValue();
      control.setLayoutData( createLayoutData( layoutData ) );
    }
    else
      control.setLayoutData( new GridData() );

    return control;
  }

  private Control createControlFromControlType( final Composite parent, final int style, final ControlType controlType )
  {
    final Feature feature = getFeature();
    if( controlType instanceof CompositeType )
    {
      final CompositeType compositeType = (CompositeType) controlType;
      final Composite composite = createCompositeFromCompositeType( parent, style, compositeType );

      // Layout setzen
      final LayoutType layoutType = compositeType.getLayout().getValue();
      if( layoutType != null )
        composite.setLayout( createLayout( layoutType ) );

      /* If a toolkit is set, use it. */
      if( m_formToolkit != null )
      {
        m_formToolkit.adapt( composite, true, true );
        // m_formToolkit.paintBordersFor( composite );
      }

      for( final JAXBElement< ? extends ControlType> element : compositeType.getControl() )
      {
        createControl( composite, SWT.NONE, element.getValue() );
      }

      return composite;
    }

    final IPropertyType ftp;
    if( controlType instanceof PropertyControlType )
      ftp = getProperty( feature, (PropertyControlType) controlType );
    else
      ftp = null;

    // control erzeugen!
    if( controlType instanceof LabelType )
    {
      final LabelType labelType = (LabelType) controlType;
      final Label label = new Label( parent, SWTUtilities.createStyleFromString( labelType.getStyle() ) );
      label.setText( labelType.getText() );

      final QName property = labelType.getProperty();
      if( property != null )
        applyAnnotation( label, property, feature );

      /* If a toolkit is set, use it. */
      if( m_formToolkit != null )
        m_formToolkit.adapt( label, true, true );

      return label;
    }
    else if( controlType instanceof ValidatorLabelType )
    {
      final ValidatorLabelType validatorLabelType = (ValidatorLabelType) controlType;

      final ValidatorFeatureControl vfc = new ValidatorFeatureControl( feature, ftp, true );

      final Control control = vfc.createControl( parent, SWTUtilities.createStyleFromString( validatorLabelType.getStyle() ) );

      addFeatureControl( vfc );

      /* If a toolkit is set, use it. */
      if( m_formToolkit != null )
        m_formToolkit.adapt( control, true, true );

      return control;
    }
    else if( controlType instanceof Text )
    {
      final Text editorType = (Text) controlType;

      final IValuePropertyType vpt = (IValuePropertyType) ftp;
      final TextFeatureControl tfc = new TextFeatureControl( feature, vpt );

      final Control control = tfc.createControl( parent, SWTUtilities.createStyleFromString( editorType.getStyle() ) );
      tfc.setEditable( editorType.isEditable() );

      addFeatureControl( tfc );

      return control;
    }
    else if( controlType instanceof Checkbox )
    {
      final Checkbox checkboxType = (Checkbox) controlType;

      final IValuePropertyType vpt = (IValuePropertyType) ftp;
      final CheckboxFeatureControl cfc = new CheckboxFeatureControl( feature, vpt );

      final Control control = cfc.createControl( parent, SWTUtilities.createStyleFromString( checkboxType.getStyle() ) );
      cfc.setEnabled( checkboxType.isEditable() );

      addFeatureControl( cfc );

      return control;
    }
    else if( controlType instanceof Button )
    {
      final Button buttonType = (Button) controlType;

      final ButtonFeatureControl bfc = new ButtonFeatureControl( feature, ftp );

      final Control control = bfc.createControl( parent, SWTUtilities.createStyleFromString( buttonType.getStyle() ) );

      addFeatureControl( bfc );

      return control;
    }
    else if( controlType instanceof TupleResult )
    {
      final TupleResult editorType = (TupleResult) controlType;

      final IValuePropertyType vpt = (IValuePropertyType) ftp;
      final TupleResultFeatureControl tfc = new TupleResultFeatureControl( feature, vpt );

      final Control control = tfc.createControl( parent, SWTUtilities.createStyleFromString( editorType.getStyle() ) );

      addFeatureControl( tfc );

      return control;
    }
    else if( controlType instanceof Combo )
    {
      final Combo comboType = (Combo) controlType;

      final List<Entry> entryList = comboType.getEntry();
      final Map<Object, String> comboEntries = new HashMap<Object, String>( entryList.size() );

      if( ftp instanceof IValuePropertyType )
      {
        final ITypeRegistry<IMarshallingTypeHandler> typeRegistry = MarshallingTypeRegistrySingleton.getTypeRegistry();
        final IMarshallingTypeHandler typeHandler = typeRegistry.getTypeHandlerFor( ftp );

        for( final Entry entry : entryList )
        {
          final String label = entry.getLabel();
          final String any = entry.getValue();
          try
          {
            final Object object = typeHandler.parseType( any );
            comboEntries.put( object, label );
          }
          catch( final ParseException e )
          {
            final IStatus status = StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.ogc.gml.featureview.control.FeatureComposite.parse" ) + any ); //$NON-NLS-1$
            KalypsoGisPlugin.getDefault().getLog().log( status );
          }
        }
      }

      final int comboStyle = SWTUtilities.createStyleFromString( comboType.getStyle() );

      final ComboFeatureControl cfc = new ComboFeatureControl( feature, ftp, comboEntries );

      final Control control = cfc.createControl( parent, comboStyle );

      addFeatureControl( cfc );

      return control;
    }
    else if( controlType instanceof Radiobutton )
    {
      final Radiobutton radioType = (Radiobutton) controlType;

      final Object valueToSet = radioType.getValueToSet();
      final String text = radioType.getText();
      final RadioFeatureControl rfc = new RadioFeatureControl( feature, ftp, valueToSet, text );

      final int radioStyle = SWTUtilities.createStyleFromString( radioType.getStyle() );
      final Control control = rfc.createControl( parent, radioStyle );

      addFeatureControl( rfc );

      return control;
    }
    else if( controlType instanceof SubcompositeType )
    {
      final SubcompositeType compoType = (SubcompositeType) controlType;

      final IFeatureControl fc = new SubFeatureControl( ftp, m_selectionManager, m_formToolkit, m_featureviewFactory );

      fc.setFeature( feature );

      final Control control = fc.createControl( parent, SWTUtilities.createStyleFromString( compoType.getStyle() ) );

      addFeatureControl( fc );

      return control;
    }
    else if( controlType instanceof Table )
    {
      final KalypsoGisPlugin plugin = KalypsoGisPlugin.getDefault();
      final IFeatureControl fc = new TableFeatureContol( ftp, plugin.createFeatureTypeCellEditorFactory(), m_selectionManager, this );
      fc.setFeature( feature );

      addFeatureControl( fc );

      final Control control = fc.createControl( parent, SWT.NONE );
      control.setLayoutData( new GridData() );
      return control;
    }

    final Label label = new Label( parent, SWT.NONE );
    label.setText( Messages.getString( "org.kalypso.ogc.gml.featureview.control.FeatureComposite.create" ) ); //$NON-NLS-1$

    /* If a toolkit is set, use it. */
    if( m_formToolkit != null )
      m_formToolkit.adapt( label, true, true );

    return label;
  }

  private Composite createCompositeFromCompositeType( final Composite parent, final int style, final CompositeType compositeType )
  {
    if( compositeType instanceof org.kalypso.template.featureview.Group )
    {
      final Group group = new org.eclipse.swt.widgets.Group( parent, style | SWTUtilities.createStyleFromString( compositeType.getStyle() ) );
      group.setText( ((org.kalypso.template.featureview.Group) compositeType).getText() );
      return group;
    }

    return new Composite( parent, style | SWTUtilities.createStyleFromString( compositeType.getStyle() ) );
  }

  private Layout createLayout( final LayoutType layoutType )
  {
    if( layoutType instanceof org.kalypso.template.featureview.GridLayout )
    {
      final org.kalypso.template.featureview.GridLayout gridLayoutType = (org.kalypso.template.featureview.GridLayout) layoutType;
      final GridLayout layout = new GridLayout();
      layout.horizontalSpacing = gridLayoutType.getHorizontalSpacing();
      layout.verticalSpacing = gridLayoutType.getVerticalSpacing();
      layout.makeColumnsEqualWidth = gridLayoutType.isMakeColumnsEqualWidth();
      layout.marginHeight = gridLayoutType.getMarginHeight();
      layout.marginWidth = gridLayoutType.getMarginWidth();
      layout.numColumns = gridLayoutType.getNumColumns();

      return layout;
    }

    return null;
  }

  private Object createLayoutData( final LayoutDataType layoutDataType )
  {
    if( layoutDataType instanceof GridDataType )
    {
      final GridDataType gridDataType = (GridDataType) layoutDataType;
      final GridData gridData = new GridData();

      gridData.grabExcessHorizontalSpace = gridDataType.isGrabExcessHorizontalSpace();
      gridData.grabExcessVerticalSpace = gridDataType.isGrabExcessVerticalSpace();

      gridData.heightHint = gridDataType.getHeightHint();
      gridData.widthHint = gridDataType.getWidthHint();
      gridData.horizontalAlignment = SWTUtilities.getGridData( gridDataType.getHorizontalAlignment() );
      gridData.verticalAlignment = SWTUtilities.getGridData( gridDataType.getVerticalAlignment() );
      gridData.horizontalIndent = gridDataType.getHorizontalIndent();

      gridData.horizontalSpan = gridDataType.getHorizontalSpan();
      gridData.verticalSpan = gridDataType.getVerticalSpan();

      return gridData;
    }

    return null;
  }

  private void addFeatureControl( final IFeatureControl fc )
  {
    m_featureControls.add( fc );
    fc.addChangeListener( this );
    fc.addModifyListener( this );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#addModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void addModifyListener( final ModifyListener l )
  {
    m_modifyListeners.add( l );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#removeModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void removeModifyListener( final ModifyListener l )
  {
    m_modifyListeners.remove( this );
  }

  @Override
  public void setFeature( final Feature feature )
  {
    super.setFeature( feature );
    for( final Iterator iter = m_featureControls.iterator(); iter.hasNext(); )
    {
      final IFeatureControl fc = (IFeatureControl) iter.next();
      fc.setFeature( feature );
    }
  }

  public void disposeControl( )
  {
    for( final Iterator iter = m_featureControls.iterator(); iter.hasNext(); )
    {
      final IFeatureControl fc = (IFeatureControl) iter.next();
      fc.dispose();
    }
    m_featureControls.clear();

    for( final Iterator iter = m_swtControls.iterator(); iter.hasNext(); )
    {
      final Control c = (Control) iter.next();
      c.dispose();
    }
    m_swtControls.clear();

    if( m_control != null )
    {
      m_control.dispose();
      m_control = null;
    }
  }

  public Control getControl( )
  {
    return m_control;
  }

  private void applyAnnotation( final Control label, final QName propertyName, final Feature feature )
  {
    if( propertyName != null )
    {
      final IPropertyType ftp = getPropertyTypeForQName( feature.getFeatureType(), propertyName );
      if( ftp != null )
      {
        final IAnnotation annotation = AnnotationUtilities.getAnnotation( ftp );

        if( annotation != null )
        {
          try
          {
            final Method method = label.getClass().getMethod( "setText", new Class[] { String.class } ); //$NON-NLS-1$
            if( method != null )
              method.invoke( label, annotation.getLabel() );
          }
          catch( final Exception e )
          {
            // ignore, this control has no 'setText'
          }

          label.setToolTipText( annotation.getTooltip() );
        }
      }
    }
  }

  private IPropertyType getProperty( final Feature feature, final PropertyControlType propertyControl )
  {
    final QName property = propertyControl.getProperty();
    return getPropertyTypeForQName( feature.getFeatureType(), property );
  }

  /**
   * Special method to retrieve a property from a feature for a special qname. Neeeded to have backward compability for
   * the feature-template. Before, the propertyName was given as xs:string (only the local part), now it is a xs:QName.
   * So old entries are interpreted against the namespace of the featuretemplate.
   */
  @SuppressWarnings("deprecation")//$NON-NLS-1$
  private IPropertyType getPropertyTypeForQName( final IFeatureType featureType, final QName property )
  {
    if( property == null )
      return null;

    final IPropertyType propertyType = featureType.getProperty( property );
    if( propertyType != null )
      return propertyType;

    if( property.getNamespaceURI().equals( FEATUREVIEW_NAMESPACE ) )
    {
      final String localPart = property.getLocalPart();
      PluginUtilities.logToPlugin( KalypsoGisPlugin.getDefault(), IStatus.WARNING, "Still using localPart for property-name '" + localPart + "'. Use QName instead.", null ); //$NON-NLS-1$ //$NON-NLS-2$
      return featureType.getProperty( localPart );
    }

    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureChangeListener#featureChanged(org.kalypso.ogc.gml.featureview.FeatureChange)
   */
  public void featureChanged( final FeatureChange change )
  {
    fireFeatureChange( change );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureChangeListener#openFeatureRequested(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypsodeegree.model.feature.IPropertyType)
   */
  public void openFeatureRequested( final Feature feature, final IPropertyType ftp )
  {
    fireOpenFeatureRequested( feature, ftp );
  }

  /**
   * @see org.eclipse.swt.events.ModifyListener#modifyText(org.eclipse.swt.events.ModifyEvent)
   */
  public void modifyText( final ModifyEvent e )
  {
    final ModifyListener[] listeners = m_modifyListeners.toArray( new ModifyListener[m_modifyListeners.size()] );
    for( int i = 0; i < listeners.length; i++ )
    {
      final ModifyListener listener = listeners[i];
      Platform.run( new SafeRunnable()
      {
        public void run( ) throws Exception
        {
          listener.modifyText( e );
        }
      } );
    }
  }

  /** Traverse the tree feature controls adds all found feature view types to the given collection */
  public void collectViewTypes( final Collection<FeatureviewType> types )
  {
    final Feature feature = getFeature();
    if( feature == null )
      return;

    final FeatureviewType type = m_featureviewFactory.get( feature.getFeatureType(), feature );
    types.add( type );

    for( final IFeatureControl control : m_featureControls )
    {
      if( control instanceof FeatureComposite )
        ((FeatureComposite) control).collectViewTypes( types );
      else if( control instanceof SubFeatureControl )
      {
        final IFeatureControl fc = ((SubFeatureControl) control).getFeatureControl();
        if( fc instanceof FeatureComposite )
          ((FeatureComposite) fc).collectViewTypes( types );
      }
    }
  }

  public FormToolkit getFormToolkit( )
  {
    return m_formToolkit;
  }

  public void setFormToolkit( final FormToolkit formToolkit )
  {
    m_formToolkit = formToolkit;
  }
}