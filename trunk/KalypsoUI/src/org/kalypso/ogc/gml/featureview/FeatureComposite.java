package org.kalypso.ogc.gml.featureview;

import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBException;

import org.deegree.model.feature.Annotation;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.kalypso.ogc.gml.featureview.control.ButtonFeatureControl;
import org.kalypso.ogc.gml.featureview.control.TextFeatureControl;
import org.kalypso.template.featureview.ButtonType;
import org.kalypso.template.featureview.CompositeType;
import org.kalypso.template.featureview.ControlType;
import org.kalypso.template.featureview.Featureview;
import org.kalypso.template.featureview.GridDataType;
import org.kalypso.template.featureview.GridLayoutType;
import org.kalypso.template.featureview.GroupType;
import org.kalypso.template.featureview.LabelType;
import org.kalypso.template.featureview.LayoutDataType;
import org.kalypso.template.featureview.LayoutType;
import org.kalypso.template.featureview.TextType;

/**
 * @author belger
 */
public class FeatureComposite implements IFeatureControl
{
  /** FeatureType -> FeatureView */
  private final Map m_viewMap = new HashMap();

  private final Collection m_controls = new ArrayList();

  private final Feature m_feature;

  public FeatureComposite( final Feature feature )
  {
    this( feature, new URL[] {} );
  }

  public FeatureComposite( final Feature feature, final URL[] templateURL )
  {
    m_feature = feature;

    try
    {
      for( int i = 0; i < templateURL.length; i++ )
      {
        final Featureview view = (Featureview)FeatureviewHelper.UNMARSHALLER
            .unmarshal( templateURL[i] );
        m_viewMap.put( view.getTypename(), view );
      }
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#updateControl()
   */
  public void updateControl()
  {
    for( final Iterator iter = m_controls.iterator(); iter.hasNext(); )
    {
      final IFeatureControl fc = (IFeatureControl)iter.next();
      fc.updateControl();
    }
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#collectChanges(java.util.Collection)
   */
  public void collectChanges( Collection c )
  {
    for( final Iterator iter = m_controls.iterator(); iter.hasNext(); )
    {
      final IFeatureControl fc = (IFeatureControl)iter.next();
      fc.collectChanges( c );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#dispose()
   */
  public void dispose()
  {
    for( final Iterator iter = m_controls.iterator(); iter.hasNext(); )
    {
      final IFeatureControl fc = (IFeatureControl)iter.next();
      fc.dispose();
    }
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#getFeature()
   */
  public Feature getFeature()
  {
    return m_feature;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#isValid()
   */
  public boolean isValid()
  {
    boolean valid = true;
    final Iterator iter = m_controls.iterator();
    while( valid == true && iter.hasNext() )
    {
      final IFeatureControl fc = (IFeatureControl)iter.next();
      valid = valid & fc.isValid();
    }

    return valid;
  }

  /**
   * Gibt zu einem TypNamen eine FeatureView zur�ck. Existiert keine solche wird
   * ein Default erzeugt.
   */
  public Featureview getFeatureview( final FeatureType featureType )
  {
    final String typename = featureType.getName();
    final Featureview view = (Featureview)m_viewMap.get( typename );
    if( view != null )
      return view;

    final Featureview newView = FeatureviewHelper.createFeatureviewFromFeatureType( featureType );

    m_viewMap.put( typename, newView );

    return newView;
  }

  public Control createControl( final Composite parent, final int style )
  {
    final Featureview view = getFeatureview( getFeature().getFeatureType() );

    return createControl( parent, view );
  }

  public Control createControl( final Composite parent, final ControlType controlType )
  {
    final Control control = createControlFromControlType( parent, controlType );

    control.setVisible( controlType.isVisible() );

    // einen bereits gesetzten Tooltip nicht �berschreiben
    if( control.getToolTipText() == null )
      control.setToolTipText( controlType.getTooltip() );

    final LayoutDataType layoutData = controlType.getLayoutData();
    if( layoutData != null )
      control.setLayoutData( createLayoutData( layoutData ) );

    return control;
  }

  private Control createControlFromControlType( final Composite parent,
      final ControlType controlType )
  {
    if( controlType instanceof CompositeType )
    {
      final CompositeType compositeType = (CompositeType)controlType;
      final Composite composite = createCompositeFromCompositeType( parent, compositeType );

      // Layout setzen
      final LayoutType layoutType = compositeType.getLayout();
      if( layoutType != null )
        composite.setLayout( createLayout( layoutType ) );

      // die Children einbauen
      final List children = compositeType.getControl();
      for( final Iterator iter = children.iterator(); iter.hasNext(); )
        createControl( composite, (ControlType)iter.next() );

      return composite;
    }

    // control erzeugen!
    if( controlType instanceof LabelType )
    {
      final LabelType labelType = (LabelType)controlType;
      final Label label = new Label( parent, labelType.getStyle() );
      label.setText( labelType.getText() );

      // falls der evtl. angegebene propertyType eine annotation hat, diese
      // verwenden
      final String propertyName = labelType.getProperty();
      if( propertyName != null && propertyName.length() > 0 )
      {
        final FeatureTypeProperty ftp = getFeature().getFeatureType().getProperty( propertyName );
        final Annotation annotation = ftp.getAnnotation( "de" );
        if( annotation != null )
        {
          label.setText( annotation.getLabel() );
          label.setToolTipText( annotation.getTooltip() );
        }
      }

      return label;
    }
    else if( controlType instanceof TextType )
    {
      final TextType editorType = (TextType)controlType;

      final String propertyName = editorType.getProperty();

      final TextFeatureControl tfc = new TextFeatureControl( getFeature(), propertyName );

      final Control control = tfc.createControl( parent, editorType.getStyle() );
      tfc.setEnabled( editorType.isEditable() );

      addControl( tfc );

      return control;
    }
    else if( controlType instanceof ButtonType )
    {
      final ButtonType buttonType = (ButtonType)controlType;

      final String propertyName = buttonType.getProperty();

      final ButtonFeatureControl bfc = new ButtonFeatureControl( getFeature(), propertyName );

      final Control control = bfc.createControl( parent, buttonType.getStyle() );

      addControl( bfc );

      return control;
    }

    final Label label = new Label( parent, SWT.NONE );
    label.setText( "<could not create control>" );
    return label;
  }

  private Composite createCompositeFromCompositeType( final Composite parent,
      final CompositeType compositeType )
  {
    if( compositeType instanceof GroupType )
    {
      final org.eclipse.swt.widgets.Group group = new org.eclipse.swt.widgets.Group( parent,
          compositeType.getStyle() );
      group.setText( ( (GroupType)compositeType ).getText() );
      return group;
    }

    return new Composite( parent, compositeType.getStyle() );
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
      final GridData gridData = new GridData();

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

  private void addControl( final IFeatureControl fc )
  {
    m_controls.add( fc );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#addModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void addModifyListener( final ModifyListener l )
  {
    for( final Iterator iter = m_controls.iterator(); iter.hasNext(); )
    {
      final IFeatureControl fc = (IFeatureControl)iter.next();
      fc.addModifyListener( l );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#removeModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void removeModifyListener( final ModifyListener l )
  {
    for( final Iterator iter = m_controls.iterator(); iter.hasNext(); )
    {
      final IFeatureControl fc = (IFeatureControl)iter.next();
      fc.removeModifyListener( l );
    }

  }

}