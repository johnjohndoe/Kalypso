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
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.xml.bind.JAXBElement;
import javax.xml.namespace.QName;

import org.apache.commons.lang.exception.ExceptionUtils;
import org.eclipse.core.runtime.IStatus;
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
import org.kalypso.gmlschema.annotation.AnnotationUtilities;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.maker.IFeatureviewFactory;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.template.featureview.Button;
import org.kalypso.template.featureview.Checkbox;
import org.kalypso.template.featureview.ColorLabelType;
import org.kalypso.template.featureview.ColumnDescriptor;
import org.kalypso.template.featureview.Combo;
import org.kalypso.template.featureview.CompositeType;
import org.kalypso.template.featureview.ControlType;
import org.kalypso.template.featureview.Extensioncontrol;
import org.kalypso.template.featureview.FeatureviewType;
import org.kalypso.template.featureview.GeometryLabelType;
import org.kalypso.template.featureview.GridDataType;
import org.kalypso.template.featureview.Image;
import org.kalypso.template.featureview.LabelType;
import org.kalypso.template.featureview.LayoutDataType;
import org.kalypso.template.featureview.LayoutType;
import org.kalypso.template.featureview.PropertyControlType;
import org.kalypso.template.featureview.Radiobutton;
import org.kalypso.template.featureview.Spinner;
import org.kalypso.template.featureview.SubcompositeType;
import org.kalypso.template.featureview.Table;
import org.kalypso.template.featureview.Text;
import org.kalypso.template.featureview.TupleResult;
import org.kalypso.template.featureview.ValidatorLabelType;
import org.kalypso.template.featureview.Combo.Entry;
import org.kalypso.template.featureview.Extensioncontrol.Param;
import org.kalypso.template.gistableview.Gistableview;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.KalypsoUIDebug;
import org.kalypso.ui.KalypsoUIExtensions;
import org.kalypso.util.swt.SWTUtilities;
import org.kalypsodeegree.filterencoding.FilterConstructionException;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.filterencoding.Operation;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.filterencoding.AbstractOperation;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * @author Gernot Belger
 */
public class FeatureComposite extends AbstractFeatureControl implements IFeatureChangeListener, ModifyListener
{
  private static final String DATA_LAYOUTDATA = "layoutData";

  private static final String DATA_CONTROL_TYPE = "controlType";

  private static final LayoutDataType NULL_LAYOUT_DATA_TYPE = new LayoutDataType();

  /**
   * The flag, indicating, if the green hook should be displayed.
   */
  private boolean m_showOk = false;

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
   *            If you want to add a feature directly at instantiation time, provide it here, otherwise leave it null.
   * @param selectionManager
   *            A selection manager, which provides functionality for adding and removing a feature from an selection
   *            and it handels the registration of listerners and so on. It has to implement IFeatureSelectionManager.
   *            You can get a default one for the features here
   *            <strong>KalypsoCorePlugin.getDefault().getSelectionManager()</strong>.
   * @param featureviewFactory
   *            A factory which delivers feature-view-templates (e.g. FeatureviewHelper).
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
    for( final IFeatureControl fc : m_featureControls )
      fc.updateControl();

    for( final Control control : m_swtControls )
    {
      updateLayoutData( control );

// if( control instanceof Composite )
// {
// // TODO: remove, not necessary
// final Composite composite = (Composite) control;
//
// composite.layout();
// composite.getParent().layout();
// }
    }

    if( m_control != null && !m_control.isDisposed() )
    {
      if( m_control instanceof Composite )
        ((Composite) m_control).layout();
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
    for( final Object element : m_featureControls )
    {
      final IFeatureControl fc = (IFeatureControl) element;

      if( !fc.isValid() )
      {
        return false;
      }
    }

    return true;
  }

  public Control createControl( final Composite parent, final int style, final IFeatureType ft )
  {
    final FeatureviewType view = m_featureviewFactory.get( ft, getFeature() );

    m_control = createControl( parent, style, view );

    /* If a toolkit is set, use it. */
    if( m_formToolkit != null )
    {
      m_formToolkit.adapt( m_control, true, true );
    }

    return m_control;
  }

  public Control createControl( final Composite parent, final int style )
  {
    try
    {
      return createControl( parent, style, getFeature().getFeatureType() );
    }
    catch( final Throwable t )
    {
      final org.eclipse.swt.widgets.Text text = new org.eclipse.swt.widgets.Text( parent, SWT.MULTI );
      text.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
      text.setEditable( false );
      final String trace = ExceptionUtils.getStackTrace( t );
      text.setText( trace );
      return text;
    }
  }

  private Control createControl( final Composite parent, final int style, final ControlType controlType )
  {
    final Control control = createControlFromControlType( parent, style, controlType );

    control.setData( DATA_CONTROL_TYPE, controlType );

    m_swtControls.add( control );

    // einen bereits gesetzten Tooltip nicht überschreiben
    if( control.getToolTipText() == null )
      control.setToolTipText( controlType.getTooltip() );

    final JAXBElement< ? extends LayoutDataType> jaxLayoutData = controlType.getLayoutData();
    final LayoutDataType layoutDataType;
    if( jaxLayoutData == null )
      layoutDataType = NULL_LAYOUT_DATA_TYPE;
    else
      layoutDataType = jaxLayoutData.getValue();

    control.setData( DATA_LAYOUTDATA, layoutDataType );
    updateLayoutData( control );

    return control;
  }

  private void updateLayoutData( final Control control )
  {
    final Feature feature = getFeature();

    /* Update the layout data */
    final LayoutDataType layoutDataType = (LayoutDataType) control.getData( DATA_LAYOUTDATA );
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

      final Object excludeType = gridDataType.getExcludeOperation();
      gridData.exclude = evaluateOperation( feature, excludeType, false );

      control.setLayoutData( gridData );
    }
    else if( layoutDataType == NULL_LAYOUT_DATA_TYPE )
      control.setLayoutData( new GridData() );

    /* Update visibility, eneblement, ... */
    final ControlType controlType = (ControlType) control.getData( DATA_CONTROL_TYPE );

    final Object visibleOperation = controlType.getVisibleOperation();
    final boolean visible = evaluateOperation( getFeature(), visibleOperation, controlType.isVisible() );
    control.setVisible( visible );

    final Object enabledOperation = controlType.getEnabledOperation();
    final boolean enabled = evaluateOperation( getFeature(), enabledOperation, controlType.isEnabled() );
    control.setEnabled( enabled );
  }

  private boolean evaluateOperation( final Feature feature, final Object operationElement, final boolean defaultValue )
  {
    try
    {
      if( operationElement instanceof Element )
      {
        KalypsoUIDebug.FEATUREVIEW_OPERATIONS.printf( "Found operation: %s%nfor feature: %s%n", operationElement, feature );

        final Element element = (Element) operationElement;
        final NodeList childNodes = element.getChildNodes();
        for( int i = 0; i < childNodes.getLength(); i++ )
        {
          final Node item = childNodes.item( i );
          if( item instanceof Element )
          {
            final Operation operation = AbstractOperation.buildFromDOM( (Element) item );
            final Boolean value = operation.evaluate( feature );
            final boolean result = value == null ? false : value.booleanValue();

            KalypsoUIDebug.FEATUREVIEW_OPERATIONS.printf( "Operation result: %s%n%n", result );

            return result;
          }
        }
      }
    }
    catch( final FilterConstructionException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( final FilterEvaluationException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

    return defaultValue;
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
      {
        composite.setLayout( createLayout( layoutType ) );
      }

      /* If a toolkit is set, use it. */
      if( m_formToolkit != null )
      {
        m_formToolkit.adapt( composite, true, true );
      }

      for( final JAXBElement< ? extends ControlType> element : compositeType.getControl() )
      {
        createControl( composite, SWT.NONE, element.getValue() );
      }

      return composite;
    }

    final IPropertyType ftp;
    if( controlType instanceof PropertyControlType )
    {
      ftp = getProperty( feature, (PropertyControlType) controlType );
    }
    else
    {
      ftp = null;
    }

    // control erzeugen!
    if( controlType instanceof LabelType )
    {
      final LabelType labelType = (LabelType) controlType;
      final Label label = new Label( parent, SWTUtilities.createStyleFromString( labelType.getStyle() ) );
      label.setText( labelType.getText() );

      final QName property = labelType.getProperty();
      if( property != null )
      {
        applyAnnotation( label, property, feature );
      }

      /* If a toolkit is set, use it. */
      if( m_formToolkit != null )
      {
        m_formToolkit.adapt( label, true, true );
      }

      return label;
    }
    else if( controlType instanceof ValidatorLabelType )
    {
      final ValidatorLabelType validatorLabelType = (ValidatorLabelType) controlType;
      if( ftp == null )
      {
        // TODO: should never happen. The error occurs while generating the ValidatorLabelType.
        System.out.println( "ValidatorLabelType without property" );
      }
      else
      {
        final ValidatorFeatureControl vfc = new ValidatorFeatureControl( feature, ftp, m_showOk );
        final Control control = vfc.createControl( parent, SWTUtilities.createStyleFromString( validatorLabelType.getStyle() ) );
        addFeatureControl( vfc );
        // System.out.println( this );

        /* If a toolkit is set, use it. */
        if( m_formToolkit != null )
        {
          m_formToolkit.adapt( control, true, true );
        }

        return control;
      }
    }
    else if( controlType instanceof GeometryLabelType )
    {
      final GeometryLabelType geometryLabelType = (GeometryLabelType) controlType;
      final GeometryFeatureControl vfc = new GeometryFeatureControl( feature, ftp );
      final Control control = vfc.createControl( parent, SWTUtilities.createStyleFromString( geometryLabelType.getStyle() ) );
      addFeatureControl( vfc );

      /* If a toolkit is set, use it. */
      if( m_formToolkit != null )
      {
        m_formToolkit.adapt( control, true, true );
      }

      return control;
    }
    else if( controlType instanceof ColorLabelType )
    {
      final ColorLabelType colorLabelType = (ColorLabelType) controlType;

      final ColorFeatureControl vfc = new ColorFeatureControl( feature, ftp );

      final Control control = vfc.createControl( parent, SWTUtilities.createStyleFromString( colorLabelType.getStyle() ) );

      addFeatureControl( vfc );

      /* If a toolkit is set, use it. */
      if( m_formToolkit != null )
      {
        m_formToolkit.adapt( control, true, true );
      }

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
      final TupleResultFeatureControl tfc = new TupleResultFeatureControl( feature, ftp );
      final List<ColumnDescriptor> columnDescriptors = editorType.getColumnDescriptor();
      final ColumnDescriptor[] cd = columnDescriptors.toArray( new ColumnDescriptor[columnDescriptors.size()] );
      tfc.setColumnDescriptors( cd );

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
    else if( controlType instanceof Spinner )
    {
      final Spinner spinnerType = (Spinner) controlType;
      final IValuePropertyType vpt = (IValuePropertyType) ftp;
      final SpinnerFeatureControl sfc = new SpinnerFeatureControl( feature, vpt );
      final int spinnerStyle = SWTUtilities.createStyleFromString( spinnerType.getStyle() );
      final org.eclipse.swt.widgets.Spinner control = sfc.createControl( parent, spinnerStyle );

      control.setIncrement( (int) spinnerType.getIncrement() );
      control.setPageIncrement( (int) spinnerType.getPageIncrement() );

      addFeatureControl( sfc );

      return control;
    }
    else if( controlType instanceof Image )
    {
      final Image imageType = (Image) controlType;

      final ImageFeatureControl ifc = new ImageFeatureControl( feature, ftp );

      final int imgStyle = SWTUtilities.createStyleFromString( imageType.getStyle() );
      final Control control = ifc.createControl( parent, imgStyle );

      addFeatureControl( ifc );

      return control;
    }
    else if( controlType instanceof Extensioncontrol )
    {
      final Extensioncontrol extControl = (Extensioncontrol) controlType;
      final String extensionId = extControl.getExtensionId();
      final int controlStyle = SWTUtilities.createStyleFromString( extControl.getStyle() );
      final List<Param> param = extControl.getParam();
      final Properties parameters = new Properties();
      for( final Param controlParam : param )
      {
        parameters.setProperty( controlParam.getName(), controlParam.getValue() );
      }

      final IFeatureviewControlFactory controlFactory = KalypsoUIExtensions.getFeatureviewControlFactory( extensionId );
      final IFeatureControl fc = controlFactory == null ? null : controlFactory.createFeatureControl( feature, ftp, parameters );
      if( fc == null )
      {
        final Label label = new Label( parent, SWT.NONE );
        label.setText( "Error: failed to create extension-control for id: " + extensionId );
        return label;
      }
      else
      {
        final Control control = fc.createControl( parent, controlStyle );
        addFeatureControl( fc );
        return control;
      }
    }
    else if( controlType instanceof SubcompositeType )
    {
      final SubcompositeType compoType = (SubcompositeType) controlType;

      final IFeatureControl fc = new SubFeatureControl( ftp, m_selectionManager, m_formToolkit, m_showOk, m_featureviewFactory, compoType.getSelector() );

      fc.setFeature( feature );

      final Control control = fc.createControl( parent, SWTUtilities.createStyleFromString( compoType.getStyle() ) );

      addFeatureControl( fc );

      return control;
    }
    else if( controlType instanceof Table )
    {
      final KalypsoGisPlugin plugin = KalypsoGisPlugin.getDefault();
      final TableFeatureContol fc = new TableFeatureContol( ftp, plugin.createFeatureTypeCellEditorFactory(), m_selectionManager, this );

      final Gistableview gistableview = ((Table) controlType).getGistableview();
      if( gistableview != null )
      {
        fc.setTableTemplate( gistableview );
      }

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
    {
      m_formToolkit.adapt( label, true, true );
    }

    return label;
  }

  private Composite createCompositeFromCompositeType( final Composite parent, final int style, final CompositeType compositeType )
  {
    final int compStyle = style | SWTUtilities.createStyleFromString( compositeType.getStyle() );
    if( compositeType instanceof org.kalypso.template.featureview.Group )
    {
      final Group group = new org.eclipse.swt.widgets.Group( parent, compStyle );
      group.setText( ((org.kalypso.template.featureview.Group) compositeType).getText() );
      return group;
    }

    return new Composite( parent, compStyle );
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
    for( final Object element : m_featureControls )
    {
      final IFeatureControl fc = (IFeatureControl) element;
      fc.setFeature( feature );
    }
  }

  public void disposeControl( )
  {
    for( final Object element : m_featureControls )
    {
      final IFeatureControl fc = (IFeatureControl) element;
      fc.dispose();
    }
    m_featureControls.clear();

    for( final Object element : m_swtControls )
    {
      final Control c = (Control) element;
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
            {
              method.invoke( label, annotation.getLabel() );
            }
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
    {
      return null;
    }

    final IPropertyType propertyType = featureType.getProperty( property );
    if( propertyType != null )
    {
      return propertyType;
    }

    if( property.getNamespaceURI().equals( FeatureComposite.FEATUREVIEW_NAMESPACE ) )
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
  public void featureChanged( final FeatureChange[] changes )
  {
    fireFeatureChange( changes );
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
    for( final ModifyListener listener : listeners )
    {
      SafeRunnable.run( new SafeRunnable()
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
    {
      return;
    }

    final FeatureviewType type = m_featureviewFactory.get( feature.getFeatureType(), feature );
    types.add( type );

    for( final IFeatureControl control : m_featureControls )
    {
      if( control instanceof FeatureComposite )
      {
        ((FeatureComposite) control).collectViewTypes( types );
      }
      else if( control instanceof SubFeatureControl )
      {
        final IFeatureControl fc = ((SubFeatureControl) control).getFeatureControl();
        if( fc instanceof FeatureComposite )
        {
          ((FeatureComposite) fc).collectViewTypes( types );
        }
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

  /**
   * This function sets, if the green hook on a ok validated feature should be displayed. The default is false. This
   * flag has only an effect, if the validator label is activated.
   * 
   * @param showOk
   *            The flag, indicating, if the green hook should be displayed.
   */
  public void setShowOk( final boolean showOk )
  {
    m_showOk = showOk;
  }

  /**
   * This function returns the flag for displaying the green hook on a ok validated feature.
   * 
   * @return The flag, indicating, if the green hook should be displayed.
   */
  public boolean isShowOk( )
  {
    return m_showOk;
  }
}