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
package org.kalypso.ogc.gml.featureview;

import java.lang.reflect.Method;
import java.net.URL;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
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
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.adapter.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.ogc.gml.AnnotationUtilities;
import org.kalypso.ogc.gml.featureview.control.AbstractFeatureControl;
import org.kalypso.ogc.gml.featureview.control.ButtonFeatureControl;
import org.kalypso.ogc.gml.featureview.control.CheckboxFeatureControl;
import org.kalypso.ogc.gml.featureview.control.ComboFeatureControl;
import org.kalypso.ogc.gml.featureview.control.RadioFeatureControl;
import org.kalypso.ogc.gml.featureview.control.SubFeatureControl;
import org.kalypso.ogc.gml.featureview.control.TableFeatureContol;
import org.kalypso.ogc.gml.featureview.control.TextFeatureControl;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.template.featureview.Button;
import org.kalypso.template.featureview.Checkbox;
import org.kalypso.template.featureview.Combo;
import org.kalypso.template.featureview.CompositeType;
import org.kalypso.template.featureview.ControlType;
import org.kalypso.template.featureview.Featuretemplate;
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
import org.kalypso.template.featureview.Combo.Entry;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.swt.SWTUtilities;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author belger
 */
public class FeatureComposite extends AbstractFeatureControl implements IFeatureChangeListener, ModifyListener
{
  /* Used for the compability-hack. Is it possible to get this from the binding classes? */
  public static String FEATUREVIEW_NAMESPACE = "featureview.template.kalypso.org";

  private final Map<QName, FeatureviewType> m_viewMap = new HashMap<QName, FeatureviewType>();

  private final Collection<IFeatureControl> m_featureControls = new ArrayList<IFeatureControl>();

  private final Collection<Control> m_swtControls = new ArrayList<Control>();

  private final Collection<ModifyListener> m_modifyListeners = new ArrayList<ModifyListener>( 5 );

  private Control m_control = null;

  private final IFeatureSelectionManager m_selectionManager;

  public FeatureComposite( final Feature feature, final IFeatureSelectionManager selectionManager )
  {
    this( feature, selectionManager, new URL[] {} );
  }

  public FeatureComposite( final Feature feature, final IFeatureSelectionManager selectionManager, final URL[] templateURL )
  {
    super( feature, null );
    m_selectionManager = selectionManager;

    for( int i = 0; i < templateURL.length; i++ )
      addView( templateURL[i] );
  }

  public FeatureComposite( final Feature feature, final IFeatureSelectionManager selectionManager, final FeatureviewType[] views )
  {
    super( feature, null );

    m_selectionManager = selectionManager;

    for( int i = 0; i < views.length; i++ )
      addView( views[i] );
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
    m_viewMap.clear();
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

  /**
   * Gibt zu einem TypNamen eine FeatureView zurück. Existiert keine solche wird ein Default erzeugt.
   * 
   * @param featureType
   * @return featureview
   */
  public FeatureviewType getFeatureview( final IFeatureType featureType )
  {
    final QName typename = featureType.getQName();
    final FeatureviewType view = m_viewMap.get( typename );
    if( view != null )
      return view;

    // REMARK: this code section is for backwards compability. Before, for the typename, only
    // the local part was given in the featureViewType (type xs:string). Now it is of type xs:qname.
    // So old entries are interpretated against the namespace of the featureview, which allows us
    // to try against this namespace uri.

    final QName compabilityName = new QName( FEATUREVIEW_NAMESPACE, typename.getLocalPart(), typename.getPrefix() );
    final FeatureviewType compabilityView = m_viewMap.get( compabilityName );
    if( compabilityView != null )
      return compabilityView;
    // REMARK end

    final FeatureviewType newView = FeatureviewHelper.createFeatureviewFromFeatureType( featureType, getFeature() );

    m_viewMap.put( typename, newView );

    return newView;
  }

  public Control createControl( final Composite parent, final int style, final IFeatureType ft )
  {
    final FeatureviewType view = getFeatureview( ft );

    m_control = createControl( parent, style, view );
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

      for( final JAXBElement< ? extends ControlType> element : compositeType.getControl() )
        createControl( composite, SWT.NONE, element.getValue() );

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
      applyAnnotation( label, labelType.getProperty(), feature );

      return label;
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
    else if( controlType instanceof Combo )
    {
      final Combo comboType = (Combo) controlType;

      final List<Entry> entryList = comboType.getEntry();
      final String[] labels = new String[entryList.size()];
      final Object[] values = new Object[entryList.size()];

      final ITypeRegistry typeRegistry = MarshallingTypeRegistrySingleton.getTypeRegistry();
      final IMarshallingTypeHandler typeHandler = (IMarshallingTypeHandler) typeRegistry.getTypeHandlerFor( ftp );

      for( int i = 0; i < labels.length; i++ )
      {
        final Entry entry = entryList.get( i );
        labels[i] = entry.getLabel();

        final String any = entry.getValue();
        try
        {
          final Object object = typeHandler.parseType( any );
          values[i] = object;
        }
        catch( final ParseException e )
        {
          final IStatus status = StatusUtilities.statusFromThrowable( e, "Fehler beim Parsen eines Wertes auf der Feature-View Vorlage: " + any );
          KalypsoGisPlugin.getDefault().getLog().log( status );
        }
      }

      final int comboStyle = SWTUtilities.createStyleFromString( comboType.getStyle() );

      final ComboFeatureControl cfc = new ComboFeatureControl( feature, ftp, labels, values );

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

      final IFeatureControl fc = new SubFeatureControl( ftp, m_selectionManager, m_viewMap.values().toArray( new FeatureviewType[0] ) );
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
      return (Composite) control;
    }

    final Label label = new Label( parent, SWT.NONE );
    label.setText( "<could not create control>" );
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

  public void addView( final URL url )
  {
    try
    {
      final Unmarshaller unmarshaller = FeatureviewHelper.JC.createUnmarshaller();
      Object unmarshal = unmarshaller.unmarshal( url );
      if( unmarshal instanceof JAXBElement )
        unmarshal = ((JAXBElement) unmarshal).getValue();

      if( unmarshal instanceof FeatureviewType )
        addView( (FeatureviewType) unmarshal );
      else if( unmarshal instanceof Featuretemplate )
      {
        final Featuretemplate ftt = (Featuretemplate) unmarshal;
        final List view = ftt.getView();
        for( final Iterator vIt = view.iterator(); vIt.hasNext(); )
          addView( (FeatureviewType) vIt.next() );
      }
      else
        System.out.println( getClass().getName() + ": Unsupported type: " + unmarshal.getClass().getName() + " in " + url.toString() );
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();
    }
  }

  public void addView( final FeatureviewType view )
  {
    final QName typename = view.getTypename();

    m_viewMap.put( typename, view );
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
            final Method method = label.getClass().getMethod( "setText", new Class[] { String.class } );
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
  @SuppressWarnings("deprecation")
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
      PluginUtilities.logToPlugin( KalypsoGisPlugin.getDefault(), IStatus.WARNING, "Still using localPart for property-name '" + localPart + "'. Use QName instead.", null );
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
}