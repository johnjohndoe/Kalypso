package org.kalypso.ogc.gml.featureview;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Validator;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.kalypso.java.lang.reflect.ClassUtilities;
import org.kalypso.java.lang.reflect.ClassUtilities.ClassUtilityException;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.featureview.control.IFeatureControl;
import org.kalypso.ogc.gml.featureview.control.TextFeatureControl;
import org.kalypso.template.featureview.CompositeType;
import org.kalypso.template.featureview.ControlType;
import org.kalypso.template.featureview.Editor;
import org.kalypso.template.featureview.EditorType;
import org.kalypso.template.featureview.Featureview;
import org.kalypso.template.featureview.GridDataType;
import org.kalypso.template.featureview.GridLayoutType;
import org.kalypso.template.featureview.GroupType;
import org.kalypso.template.featureview.LabelType;
import org.kalypso.template.featureview.LayoutDataType;
import org.kalypso.template.featureview.LayoutType;
import org.kalypso.template.featureview.ObjectFactory;

/**
 * @author belger
 */
public class FeatureviewHelper
{
  // TODO: add mapping from outside
  /** FeatureType -> FeatureView */
  private final Map m_viewMap = new HashMap();

  /** Standardview erzeugen */
  public Featureview createFeatureviewFromFeatureType( final FeatureType type )
  {
    try
    {
      final ObjectFactory factory = new ObjectFactory();
      final Featureview featureview = factory.createFeatureview();
      featureview.setStyle( SWT.BORDER );
      
      final GridLayoutType gridLayout = factory.createGridLayout();
      gridLayout.setNumColumns( 2 );
      featureview.setLayout( gridLayout );
      
      final List controlList = featureview.getCompositeOrControl();
      
      final FeatureTypeProperty[] properties = type.getProperties();
      for( int i = 0; i < properties.length; i++ )
      {
        final FeatureTypeProperty ftp = properties[i];
        
        final LabelType label = factory.createLabel();
        label.setStyle( SWT.NONE );
        label.setText( ftp.getName() );
        label.setVisible( true );
        
        final GridDataType labelGridData = factory.createGridData();
        labelGridData.setGrabExcessHorizontalSpace( false );
        labelGridData.setHorizontalAlignment( GridData.BEGINNING );
        label.setLayoutData( labelGridData );
        
        controlList.add( label );

        final Editor editor = factory.createEditor();
        editor.setStyle( SWT.BORDER );
        editor.setEditable( true );
        editor.setProperty( ftp.getName() );

        final GridDataType editorGridData = factory.createGridData();
        editorGridData.setHorizontalAlignment( GridData.FILL );
        editorGridData.setGrabExcessHorizontalSpace( true );
        editor.setLayoutData( editorGridData );
        
        editor.setControl( getDefaultControlForType( ftp.getType() ) );
        
        controlList.add( editor );
      }
      
      final Validator validator = factory.createValidator();
      validator.validate( featureview );
      
      return featureview;
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();
      
      return null;
    }  
  }

  private String getDefaultControlForType( final String typeName )
  {
    if( typeName.equals( "java.lang.String" ) )
      return TextFeatureControl.class.getName();
    if( typeName.equals( "java.lang.Integer" ) )
      return TextFeatureControl.class.getName();
    if( typeName.equals( "java.lang.Long" ) )
      return TextFeatureControl.class.getName();
    if( typeName.equals( "java.lang.Float" ) )
      return TextFeatureControl.class.getName();
    if( typeName.equals( "java.lang.Double" ) )
      return TextFeatureControl.class.getName();
    if( typeName.equals( "java.lang.String" ) )
      return TextFeatureControl.class.getName();

    return "";
  }

  public Composite createFeatureControl( final Composite parent, final KalypsoFeatureLayer featureLayer, final Feature fe )
  {
    final Featureview view = getFeatureview( fe.getFeatureType() );    
    
    return createComposite( parent, view, featureLayer, fe );
  }

  /** 
   * Gibt zu einem TypNamen eine FeatureView zurück. Existiert keine solche
   * wird ein Default erzeugt.
   */
  public Featureview getFeatureview( final FeatureType featureType )
  {
    final Featureview view = (Featureview)m_viewMap.get( featureType );
    if( view != null )
      return view;
    
    final Featureview newView = createFeatureviewFromFeatureType( featureType );
    
    m_viewMap.put( featureType, newView );
    
    return newView;
  }

  private Composite createComposite( final Composite parent, final CompositeType compositeType, final KalypsoFeatureLayer featureLayer, final Feature feature )
  {
    Composite composite = null;
    if( compositeType instanceof GroupType )
    {
      final org.eclipse.swt.widgets.Group group = new org.eclipse.swt.widgets.Group( parent, compositeType.getStyle() );
      group.setText( ((GroupType)compositeType).getText() );
    }
    else
      composite = new Composite( parent, compositeType.getStyle() );

    composite.setToolTipText( compositeType.getTooltip() );
    
    final LayoutType layoutType = compositeType.getLayout();
    if( layoutType != null )
      composite.setLayout( createLayout( layoutType ) );
    final LayoutDataType layoutDataType = compositeType.getLayoutData();
    if( layoutDataType != null )
      composite.setLayoutData( createLayoutData( layoutDataType ) );
    
    // die Children einbauen
    final List compositeOrControlList = compositeType.getCompositeOrControl();
    for( final Iterator iter = compositeOrControlList.iterator(); iter.hasNext(); )
    {
      final Object compositeOrControl = iter.next();
      if( compositeOrControl instanceof CompositeType )
        createComposite( composite, (CompositeType)compositeOrControl, featureLayer, feature );
      else if( compositeOrControl instanceof ControlType )
      {
        final ControlType controlType = (ControlType)compositeOrControl;

        createControl( featureLayer, feature, composite, compositeOrControl, controlType );
      }
    }
    
    return composite;
  }

  private void createControl( final KalypsoFeatureLayer featureLayer, final Feature feature, final Composite composite, final Object compositeOrControl, final ControlType controlType )
  {
    final Control control = createControlInternal( featureLayer, feature, composite, compositeOrControl );
    
    if( control == null )
    {
      final Label label = new Label( composite, SWT.NONE );
      label.setText( "<could not create control>" );
    }
    else
    {
      control.setVisible( controlType.isVisible() );
      control.setToolTipText( controlType.getTooltip( ) );
      final LayoutDataType layoutData = controlType.getLayoutData();
      if( layoutData != null )
        control.setLayoutData( createLayoutData( layoutData ) );
    }
  }

  private Control createControlInternal( final KalypsoFeatureLayer featureLayer, final Feature feature, Composite composite, final Object compositeOrControl )
  {
    // control erzeugen!
    if( compositeOrControl instanceof LabelType )
    {
      final LabelType labelType = (LabelType)compositeOrControl;
      final Label label = new org.eclipse.swt.widgets.Label( composite, labelType.getStyle() );
      label.setText( labelType.getText() );
      return label;
    }
    else if( compositeOrControl instanceof EditorType )
    {
      final EditorType editorType = (EditorType)compositeOrControl;
      
      final String propertyName = editorType.getProperty();

      try
      {
        final String controlClassname = editorType.getControl();
        if( controlClassname == null || controlClassname.length() == 0 )
          return null;
        
        final IFeatureControl featureControl = (IFeatureControl)ClassUtilities.newInstance( controlClassname, IFeatureControl.class, getClass().getClassLoader() );
        
        featureControl.setFeature( featureLayer, feature );
        featureControl.setProperty( propertyName );
        
        final Control control = featureControl.createControl( composite, editorType.getStyle() );
        featureControl.setEnabled( editorType.isEditable() );
        
        return control;
      }
      catch( final ClassUtilityException e )
      {
        e.printStackTrace();
      }
    }
    
    return null;
  }

  private Layout createLayout( final LayoutType layoutType )
  {
    if( layoutType instanceof GridLayoutType )
    {
      final GridLayoutType gridLayoutType = (GridLayoutType)layoutType;
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
      final GridDataType gridDataType = (GridDataType)layoutDataType;
      final GridData gridData = new GridData(  );

      gridData.grabExcessHorizontalSpace = gridDataType.isGrabExcessHorizontalSpace();
      gridData.grabExcessVerticalSpace = gridDataType.isGrabExcessVerticalSpace();

      gridData.heightHint = gridDataType.getHeightHint();
      gridData.widthHint = gridDataType.getWidthHint();
      gridData.horizontalAlignment = gridDataType.getHorizontalAlignment();
      gridData.verticalAlignment = gridDataType.getVerticalAlignment();
      gridData.horizontalIndent = gridDataType.getHorizontalIndent();

      gridData.horizontalSpan = gridDataType.getHorizontalSpan();
      gridData.verticalSpan = gridDataType.getVerticalSpan();
      
      return gridData;
    }
    
    return null;
  }

  
}
