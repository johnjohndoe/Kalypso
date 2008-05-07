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

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.xml.bind.JAXBElement;
import javax.xml.namespace.QName;

import org.apache.commons.lang.exception.ExceptionUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.util.SafeRunnable;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.command.ICommand;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.swt.ColorUtilities;
import org.kalypso.gmlschema.annotation.AnnotationUtilities;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.control.comparators.IViewerComparator;
import org.kalypso.ogc.gml.featureview.maker.IFeatureviewFactory;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.template.featureview.Button;
import org.kalypso.template.featureview.Checkbox;
import org.kalypso.template.featureview.ColorLabelType;
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
import org.kalypso.template.featureview.TabFolder;
import org.kalypso.template.featureview.Table;
import org.kalypso.template.featureview.Text;
import org.kalypso.template.featureview.TupleResult;
import org.kalypso.template.featureview.ValidatorLabelType;
import org.kalypso.template.featureview.Combo.Entry;
import org.kalypso.template.featureview.Combo.Sorter;
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
import org.kalypsodeegree.xml.XMLTools;
import org.kalypsodeegree_impl.filterencoding.AbstractOperation;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * @author Gernot Belger
 */
public class FeatureComposite extends AbstractFeatureControl implements IFeatureChangeListener, ModifyListener
{
  private static final String DATA_LAYOUTDATA = "layoutData"; //$NON-NLS-1$

  private static final String DATA_CONTROL_TYPE = "controlType"; //$NON-NLS-1$

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
      updateLayoutData( control );

    if( m_control != null && !m_control.isDisposed() && m_control instanceof Composite )
      ((Composite) m_control).layout();
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
        return false;
    }

    return true;
  }

  public Control createControl( final Composite parent, final int style, final IFeatureType ft )
  {
    final FeatureviewType view = m_featureviewFactory.get( ft, getFeature() );

    if( m_formToolkit != null )
      m_formToolkit.adapt( parent );

    m_control = createControl( parent, style, view );

    /* If a toolkit is set, use it. */
    if( m_formToolkit != null )
      m_formToolkit.adapt( m_control, true, true );

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
    final Feature feature = getFeature();

    final IPropertyType ftp = getProperty( feature, controlType );

    // TODO: this is called much too often for the same ftp, we should cache it somehow
    // TODO: even better: process annotation while schema is loaded... then we can get it from the ftp itself
    final IAnnotation annotation = ftp == null ? null : AnnotationUtilities.getAnnotation( ftp );

    final Control control = createControlFromControlType( parent, style, controlType, ftp, annotation );

    // Set tooltip: an explicitly set tooltip always wins
    final String tooltipControlText = controlType.getTooltip();

    final String tooltipText = getAnnotation( annotation, tooltipControlText, IAnnotation.ANNO_TOOLTIP );
    control.setToolTipText( tooltipText );

    /* If a toolkit is set, use it. */
    if( m_formToolkit != null )
      m_formToolkit.adapt( control, true, true );

    control.setData( DATA_CONTROL_TYPE, controlType );

    m_swtControls.add( control );

    /* Set the background-color. */
    final Object backgroundColor = controlType.getBackgroundColor();
    if( backgroundColor != null )
    {
      RGB rgb = null;

      if( backgroundColor instanceof String )
        rgb = ColorUtilities.toRGBFromHTML( (String) backgroundColor );

      if( rgb != null )
        control.setBackground( new Color( control.getDisplay(), rgb ) );
    }

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

  /**
   * Return the desired annotation value. A given explicit value is preferred, if not empty.
   */
  private String getAnnotation( final IAnnotation annotation, final String explicitValue, final String annoElement )
  {
    if( annotation == null )
      return explicitValue == null ? "" : explicitValue; //$NON-NLS-1$

    if( explicitValue != null && explicitValue.length() > 0 )
      return explicitValue;

    return annotation.getValue( annoElement );
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

    /* Update visibility, enablement, ... */
    final ControlType controlType = (ControlType) control.getData( DATA_CONTROL_TYPE );

    // REMARK: Special case for direct children of Tab-Folders. Setting the visibility here
    // breaks the tab folder behavior. We assume, that the visibility of a
    // tab folder item is never changed depending on a value of a feature.
    if( !(control.getParent() instanceof org.eclipse.swt.widgets.TabFolder) )
    {
      final Object visibleOperation = controlType.getVisibleOperation();
      final boolean visible = evaluateOperation( getFeature(), visibleOperation, controlType.isVisible() );
      if( control.getVisible() != visible )
        control.setVisible( visible );
    }

    final Object enabledOperation = controlType.getEnabledOperation();
    final boolean enabled = evaluateOperation( getFeature(), enabledOperation, controlType.isEnabled() );
    if( control.getEnabled() != enabled )
      control.setEnabled( enabled );
  }

  private boolean evaluateOperation( final Feature feature, final Object operationElement, final boolean defaultValue )
  {
    try
    {
      if( operationElement instanceof String )
        return Boolean.parseBoolean( (String) operationElement );
      else if( operationElement instanceof Element )
      {
        KalypsoUIDebug.FEATUREVIEW_OPERATIONS.printf( Messages.getString("org.kalypso.ogc.gml.featureview.control.FeatureComposite.3"), operationElement, feature ); //$NON-NLS-1$

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

            KalypsoUIDebug.FEATUREVIEW_OPERATIONS.printf( Messages.getString("org.kalypso.ogc.gml.featureview.control.FeatureComposite.4"), result ); //$NON-NLS-1$

            return result;
          }
        }
      }
    }
    catch( final FilterConstructionException e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoGisPlugin.getDefault().getLog().log( status );
    }
    catch( final FilterEvaluationException e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoGisPlugin.getDefault().getLog().log( status );
    }

    return defaultValue;
  }

  private Control createControlFromControlType( final Composite parent, final int style, final ControlType controlType, final IPropertyType ftp, final IAnnotation annotation )
  {
    final Feature feature = getFeature();
    if( controlType instanceof CompositeType )
    {
      final CompositeType compositeType = (CompositeType) controlType;
      final Composite composite = createCompositeFromCompositeType( parent, style, compositeType, annotation );

      // Layout setzen
      final LayoutType layoutType = compositeType.getLayout().getValue();
      if( layoutType != null )
        composite.setLayout( createLayout( layoutType ) );

      for( final JAXBElement< ? extends ControlType> element : compositeType.getControl() )
        createControl( composite, SWT.NONE, element.getValue() );

      return composite;
    }

    if( controlType instanceof TabFolder )
    {
      final TabFolder tabFolderType = (TabFolder) controlType;

      final int tabStyle = SWTUtilities.createStyleFromString( tabFolderType.getStyle() );

      final org.eclipse.swt.widgets.TabFolder tabFolder = new org.eclipse.swt.widgets.TabFolder( parent, tabStyle );

      final List<org.kalypso.template.featureview.TabFolder.TabItem> tabItem = tabFolderType.getTabItem();
      for( final org.kalypso.template.featureview.TabFolder.TabItem tabItemType : tabItem )
      {
        final String label = tabItemType.getTabLabel();
        final String itemLabel = getAnnotation( annotation, label, IAnnotation.ANNO_LABEL );

        final ControlType control = tabItemType.getControl().getValue();

        final TabItem item = new TabItem( tabFolder, SWT.NONE );
        item.setText( itemLabel );

        final Control tabControl = createControl( tabFolder, SWT.NONE, control );

        item.setControl( tabControl );
      }

      return tabFolder;
    }

    if( controlType instanceof LabelType )
    {
      final LabelType labelType = (LabelType) controlType;
      final Label label = new Label( parent, SWTUtilities.createStyleFromString( labelType.getStyle() ) );

      final String labelControlText = labelType.getText();

      label.setText( getAnnotation( annotation, labelControlText, IAnnotation.ANNO_LABEL ) );

      return label;
    }
    else if( controlType instanceof ValidatorLabelType )
    {
      final ValidatorLabelType validatorLabelType = (ValidatorLabelType) controlType;
      if( ftp == null )
        // TODO: should never happen. The error occurs while generating the ValidatorLabelType.
        System.out.println( Messages.getString("org.kalypso.ogc.gml.featureview.control.FeatureComposite.5") ); //$NON-NLS-1$
      else
      {
        final ValidatorFeatureControl vfc = new ValidatorFeatureControl( feature, ftp, m_showOk );
        final Control control = vfc.createControl( parent, SWTUtilities.createStyleFromString( validatorLabelType.getStyle() ) );
        addFeatureControl( vfc );

        return control;
      }
    }
    else if( controlType instanceof GeometryLabelType )
    {
      final GeometryLabelType geometryLabelType = (GeometryLabelType) controlType;
      final GeometryFeatureControl vfc = new GeometryFeatureControl( feature, ftp );
      final Control control = vfc.createControl( parent, SWTUtilities.createStyleFromString( geometryLabelType.getStyle() ) );
      addFeatureControl( vfc );

      return control;
    }
    else if( controlType instanceof ColorLabelType )
    {
      final ColorLabelType colorLabelType = (ColorLabelType) controlType;

      final ColorFeatureControl vfc = new ColorFeatureControl( feature, ftp );

      final Control control = vfc.createControl( parent, SWTUtilities.createStyleFromString( colorLabelType.getStyle() ) );

      addFeatureControl( vfc );

      return control;
    }
    else if( controlType instanceof Text )
    {
      final Text editorType = (Text) controlType;

      final IValuePropertyType vpt = (IValuePropertyType) ftp;

      final KalypsoGisPlugin plugin = KalypsoGisPlugin.getDefault();

      final Object objFormat = editorType.getFormat();
      String format = null;
      if( objFormat instanceof String )
        format = (String) objFormat;
      else if( objFormat instanceof Node )
        format = XMLTools.getStringValue( ((Node) objFormat) );

      final TextFeatureControl tfc = new TextFeatureControl( feature, vpt, format, plugin.createFeatureTypeCellEditorFactory(), this, m_selectionManager );

      final Control control = tfc.createControl( parent, SWTUtilities.createStyleFromString( editorType.getStyle() ) );
      tfc.setEditable( editorType.isEditable() );

      addFeatureControl( tfc );

      return control;
    }
    else if( controlType instanceof Checkbox )
    {
      final Checkbox checkboxType = (Checkbox) controlType;

      final String checkboxControlText = checkboxType.getText();
      final String text = getAnnotation( annotation, checkboxControlText, IAnnotation.ANNO_LABEL );

      final IValuePropertyType vpt = (IValuePropertyType) ftp;
      final CheckboxFeatureControl cfc = new CheckboxFeatureControl( feature, vpt, text );

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

      final TupleResultFeatureControl tfc = TupleResultFeatureControl.create( editorType, feature, ftp );

      final Control control = tfc.createControl( parent, SWTUtilities.createStyleFromString( editorType.getStyle() ) );

      addFeatureControl( tfc );

      return control;
    }
    else if( controlType instanceof Combo )
    {
      final Combo comboType = (Combo) controlType;

      /* Look, if the user wants something sorted. */
      final Sorter sorter = comboType.getSorter();

      /* The comparator. */
      ViewerComparator comparator = null;

      /* If there is an sorter, look deeper. */
      if( sorter != null )
      {
        /* The id of the sorter. */
        String id = sorter.getId();
        if( id == null || id.length() == 0 )
          id = "org.kalypso.ui.featureview.comparators.defaultComparator"; //$NON-NLS-1$

        /* Get the sorter of the id. */
        final IExtensionRegistry registry = Platform.getExtensionRegistry();
        final IConfigurationElement[] elements = registry.getConfigurationElementsFor( "org.kalypso.core.featureviewComparator" ); //$NON-NLS-1$
        for( final IConfigurationElement element : elements )
        {
          final String elementId = element.getAttribute( "id" ); //$NON-NLS-1$
          if( id.equals( elementId ) )
            try
            {
              comparator = (ViewerComparator) element.createExecutableExtension( "class" ); //$NON-NLS-1$
            }
            catch( final CoreException e )
            {
              e.printStackTrace();
            }
        }

        /* If a valid id was given ... */
        if( comparator != null )
          /* The default-comparator doesn't use any parameter, so they are ignored, even, if they are supplied. */
          if( comparator instanceof IViewerComparator )
          {
            /* The parameter map. */
            final HashMap<String, String> params = new HashMap<String, String>();

            /* Get all parameter. */
            final List<org.kalypso.template.featureview.Combo.Sorter.Param> parameter = sorter.getParam();
            if( parameter != null )
              /* Collect all parameter. */
              for( final org.kalypso.template.featureview.Combo.Sorter.Param param : parameter )
                params.put( param.getName(), param.getValue() );

            ((IViewerComparator) comparator).init( feature, params );
          }
      }

      final List<Entry> entryList = comboType.getEntry();
      final Map<Object, String> comboEntries = new LinkedHashMap<Object, String>( entryList.size() );

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

      final ComboFeatureControl cfc = new ComboFeatureControl( feature, ftp, comboEntries, comparator );

      final Control control = cfc.createControl( parent, comboStyle );

      addFeatureControl( cfc );

      return control;
    }
    else if( controlType instanceof Radiobutton )
    {
      final Radiobutton radioType = (Radiobutton) controlType;

      final Object valueToSet = radioType.getValueToSet();

      final String text = getAnnotation( annotation, radioType.getText(), IAnnotation.ANNO_LABEL );

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
        parameters.setProperty( controlParam.getName(), controlParam.getValue() );

      try
      {
        final IFeatureviewControlFactory controlFactory = KalypsoUIExtensions.getFeatureviewControlFactory( extensionId );
        final IFeatureControl fc = controlFactory.createFeatureControl( feature, ftp, parameters );
        final Control control = fc.createControl( parent, controlStyle );
        addFeatureControl( fc );
        return control;
      }
      catch( final CoreException ce )
      {
        final Label label = new Label( parent, SWT.NONE );
        label.setText( ce.getStatus().getMessage() );
        return label;
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
      final Table tableType = (Table) controlType;

      final TableFeatureContol fc = new TableFeatureContol( ftp, plugin.createFeatureTypeCellEditorFactory(), m_selectionManager, this, tableType.isShowToolbar(), tableType.isShowContextMenu() );

      final Gistableview gistableview = tableType.getGistableview();
      if( gistableview != null )
        fc.setTableTemplate( gistableview );

      fc.setFeature( feature );

      addFeatureControl( fc );

      final Control control = fc.createControl( parent, SWT.NONE );
      control.setLayoutData( new GridData() );

      return control;
    }

    final Label label = new Label( parent, SWT.NONE );
    label.setText( Messages.getString( "org.kalypso.ogc.gml.featureview.control.FeatureComposite.create" ) ); //$NON-NLS-1$

    return label;
  }

  private Composite createCompositeFromCompositeType( final Composite parent, final int style, final CompositeType compositeType, final IAnnotation annotation )
  {
    final int compStyle = style | SWTUtilities.createStyleFromString( compositeType.getStyle() );
    if( compositeType instanceof org.kalypso.template.featureview.Group )
    {
      final Group group = new org.eclipse.swt.widgets.Group( parent, compStyle );

      final String groupControlText = ((org.kalypso.template.featureview.Group) compositeType).getText();

      final String groupText = getAnnotation( annotation, groupControlText, IAnnotation.ANNO_LABEL );
      group.setText( groupText );

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

  private IPropertyType getProperty( final Feature feature, final ControlType controlType )
  {
    if( controlType instanceof PropertyControlType )
      return getPropertyTypeForQName( feature.getFeatureType(), ((PropertyControlType) controlType).getProperty() );

    if( controlType instanceof CompositeType )
      return getPropertyTypeForQName( feature.getFeatureType(), ((CompositeType) controlType).getProperty() );

    return null;
  }

  /**
   * Special method to retrieve a property from a feature for a special qname. Neeeded to have backward compability for
   * the feature-template. Before, the propertyName was given as xs:string (only the local part), now it is a xs:QName.
   * So old entries are interpreted against the namespace of the featuretemplate.
   */
  @SuppressWarnings("deprecation") //$NON-NLS-1$
  private IPropertyType getPropertyTypeForQName( final IFeatureType featureType, final QName property )
  {
    if( property == null )
      return null;

    final IPropertyType propertyType = featureType.getProperty( property );
    if( propertyType != null )
      return propertyType;

    if( property.getNamespaceURI().equals( FeatureComposite.FEATUREVIEW_NAMESPACE ) )
    {
      final String localPart = property.getLocalPart();
      PluginUtilities.logToPlugin( KalypsoGisPlugin.getDefault(), IStatus.WARNING, "Still using localPart for property-name '" + localPart + "'. Use QName instead.", null ); //$NON-NLS-1$ //$NON-NLS-2$
      return featureType.getProperty( localPart );
    }

    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureChangeListener#featureChanged(org.kalypso.commons.command.ICommand)
   */
  public void featureChanged( final ICommand changeCommand )
  {
    fireFeatureChange( changeCommand );
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
      SafeRunnable.run( new SafeRunnable()
      {
        public void run( ) throws Exception
        {
          listener.modifyText( e );
        }
      } );
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
      if( control instanceof FeatureComposite )
        ((FeatureComposite) control).collectViewTypes( types );
      else if( control instanceof SubFeatureControl )
      {
        final IFeatureControl fc = ((SubFeatureControl) control).getFeatureControl();
        if( fc instanceof FeatureComposite )
          ((FeatureComposite) fc).collectViewTypes( types );
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