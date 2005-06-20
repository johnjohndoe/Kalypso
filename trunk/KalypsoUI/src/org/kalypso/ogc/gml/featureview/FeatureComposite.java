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
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBException;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.kalypso.ogc.gml.featureview.control.ButtonFeatureControl;
import org.kalypso.ogc.gml.featureview.control.CheckboxFeatureControl;
import org.kalypso.ogc.gml.featureview.control.SubFeatureControl;
import org.kalypso.ogc.gml.featureview.control.TableFeatureContol;
import org.kalypso.ogc.gml.featureview.control.TextFeatureControl;
import org.kalypso.template.featureview.ButtonType;
import org.kalypso.template.featureview.CheckboxType;
import org.kalypso.template.featureview.CompositeType;
import org.kalypso.template.featureview.ControlType;
import org.kalypso.template.featureview.FeaturetemplateType;
import org.kalypso.template.featureview.FeatureviewType;
import org.kalypso.template.featureview.GridDataType;
import org.kalypso.template.featureview.GridLayoutType;
import org.kalypso.template.featureview.GroupType;
import org.kalypso.template.featureview.LabelType;
import org.kalypso.template.featureview.LayoutDataType;
import org.kalypso.template.featureview.LayoutType;
import org.kalypso.template.featureview.SubcompositeType;
import org.kalypso.template.featureview.TableType;
import org.kalypso.template.featureview.TextType;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.preferences.IKalypsoPreferences;
import org.kalypso.util.swt.SWTUtilities;
import org.kalypsodeegree.model.feature.Annotation;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author belger
 */
public class FeatureComposite implements IFeatureControl
{
  /** FeatureType -> FeatureView */
  private final Map m_viewMap = new HashMap();

  private final Collection m_featureControls = new ArrayList();

  private final Collection m_swtControls = new ArrayList();

  private Control m_control = null;

  private Feature m_feature;

  private GMLWorkspace m_workspace;

  public FeatureComposite( final GMLWorkspace workspace, final Feature feature )
  {
    this( workspace, feature, new URL[] {} );
  }

  public FeatureComposite( final GMLWorkspace workspace, final Feature feature, final URL[] templateURL )
  {
    m_workspace = workspace;
    m_feature = feature;

    for( int i = 0; i < templateURL.length; i++ )
      addView( templateURL[i] );
  }

  public FeatureComposite( final GMLWorkspace workspace, final Feature feature, final FeatureviewType[] views )
  {
    m_workspace = workspace;
    m_feature = feature;

    for( int i = 0; i < views.length; i++ )
      addView( views[i] );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#updateControl()
   */
  public void updateControl()
  {
    for( final Iterator iter = m_featureControls.iterator(); iter.hasNext(); )
    {
      final IFeatureControl fc = (IFeatureControl)iter.next();
      fc.updateControl();
    }
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#collectChanges(java.util.Collection)
   */
  public void collectChanges( final Collection c )
  {
    for( final Iterator iter = m_featureControls.iterator(); iter.hasNext(); )
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
    disposeControl();

    m_viewMap.clear();
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
    for( final Iterator iter = m_featureControls.iterator(); iter.hasNext(); )
    {
      final IFeatureControl fc = (IFeatureControl)iter.next();

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
  public FeatureviewType getFeatureview( final FeatureType featureType )
  {
    final String typename = featureType.getName();
    final FeatureviewType view = (FeatureviewType)m_viewMap.get( typename );
    if( view != null )
      return view;

    final FeatureviewType newView = FeatureviewHelper.createFeatureviewFromFeatureType( featureType );

    m_viewMap.put( typename, newView );

    return newView;
  }

  public Control createControl( final Composite parent, final int style, final FeatureType ft )
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

    // einen bereits gesetzten Tooltip nicht überschreiben
    if( control.getToolTipText() == null )
      control.setToolTipText( controlType.getTooltip() );

    final LayoutDataType layoutData = controlType.getLayoutData();
    if( layoutData != null )
      control.setLayoutData( createLayoutData( layoutData ) );

    return control;
  }

  private Control createControlFromControlType( final Composite parent, final int style, final ControlType controlType )
  {
    final Feature feature = getFeature();
    final GMLWorkspace workspace = getWorkspace();
    if( controlType instanceof CompositeType )
    {
      final CompositeType compositeType = (CompositeType)controlType;
      final Composite composite = createCompositeFromCompositeType( parent, style, compositeType );

      // Layout setzen
      final LayoutType layoutType = compositeType.getLayout();
      if( layoutType != null )
        composite.setLayout( createLayout( layoutType ) );

      // die Children einbauen
      final List children = compositeType.getControl();
      for( final Iterator iter = children.iterator(); iter.hasNext(); )
        createControl( composite, SWT.NONE, (ControlType)iter.next() );

      return composite;
    }

    // control erzeugen!
    if( controlType instanceof LabelType )
    {
      final LabelType labelType = (LabelType)controlType;
      final Label label = new Label( parent, SWTUtilities.createStyleFromString( labelType.getStyle() ) );
      label.setText( labelType.getText() );
      applyAnnotation( label, labelType.getProperty(), feature );

      return label;
    }
    else if( controlType instanceof TextType )
    {
      final TextType editorType = (TextType)controlType;

      final String propertyName = editorType.getProperty();

      final FeatureTypeProperty ftp = feature.getFeatureType().getProperty( propertyName );
      final TextFeatureControl tfc = new TextFeatureControl( workspace, feature, ftp );

      final Control control = tfc.createControl( parent, SWTUtilities.createStyleFromString( editorType.getStyle() ) );
      tfc.setEnabled( editorType.isEditable() );

      addFeatureControl( tfc );

      return control;
    }
    else if( controlType instanceof CheckboxType )
    {
      final CheckboxType checkboxType = (CheckboxType)controlType;

      final String propertyName = checkboxType.getProperty();

      final FeatureTypeProperty ftp = feature.getFeatureType().getProperty( propertyName );
      final CheckboxFeatureControl cfc = new CheckboxFeatureControl( workspace, feature, ftp );

      final Control control = cfc.createControl( parent, SWTUtilities.createStyleFromString( checkboxType.getStyle() ) );
      cfc.setEnabled( checkboxType.isEditable() );

      addFeatureControl( cfc );

      return control;
    }
    else if( controlType instanceof ButtonType )
    {
      final ButtonType buttonType = (ButtonType)controlType;

      final String propertyName = buttonType.getProperty();
      final FeatureTypeProperty ftp = feature.getFeatureType().getProperty( propertyName );
      final ButtonFeatureControl bfc = new ButtonFeatureControl( workspace, feature, ftp );

      final Control control = bfc.createControl( parent, SWTUtilities.createStyleFromString( buttonType.getStyle() ) );

      addFeatureControl( bfc );

      return control;
    }
    else if( controlType instanceof SubcompositeType )
    {
      final SubcompositeType compoType = (SubcompositeType)controlType;

      final String propertyName = compoType.getProperty();
      final FeatureTypeProperty ftp = feature.getFeatureType().getProperty( propertyName );

      final IFeatureControl fc = new SubFeatureControl( workspace, ftp, (FeatureviewType[])m_viewMap.values().toArray(
          new FeatureviewType[0] ) );
      fc.setFeature( workspace, feature );

      final Control control = fc.createControl( parent, SWTUtilities.createStyleFromString( compoType.getStyle() ) );

      addFeatureControl( fc );

      return control;
    }
    else if( controlType instanceof TableType )
    {
      final TableType tableType = (TableType)controlType;

      final String propertyName = tableType.getProperty();
      final FeatureTypeProperty ftp = feature.getFeatureType().getProperty( propertyName );

      final KalypsoGisPlugin plugin = KalypsoGisPlugin.getDefault();
      final IFeatureControl fc = new TableFeatureContol( workspace, ftp, plugin.createFeatureTypeCellEditorFactory(),
          plugin.getDefaultMapSelectionID() );
      fc.setFeature( workspace, feature );

      final Control control = fc.createControl( parent, SWTUtilities.createStyleFromString( tableType.getStyle() ) );

      addFeatureControl( fc );

      return control;
    }

    final Label label = new Label( parent, SWT.NONE );
    label.setText( "<could not create control>" );
    return label;
  }

  private Composite createCompositeFromCompositeType( final Composite parent, final int style,
      final CompositeType compositeType )
  {
    if( compositeType instanceof GroupType )
    {
      final Group group = new org.eclipse.swt.widgets.Group( parent, style
          | SWTUtilities.createStyleFromString( compositeType.getStyle() ) );
      group.setText( ( (GroupType)compositeType ).getText() );
      return group;
    }

    return new Composite( parent, style | SWTUtilities.createStyleFromString( compositeType.getStyle() ) );
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
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#addModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void addModifyListener( final ModifyListener l )
  {
    for( final Iterator iter = m_featureControls.iterator(); iter.hasNext(); )
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
    for( final Iterator iter = m_featureControls.iterator(); iter.hasNext(); )
    {
      final IFeatureControl fc = (IFeatureControl)iter.next();
      fc.removeModifyListener( l );
    }
  }

  public void setFeature( final GMLWorkspace workspace, final Feature feature )
  {
    m_feature = feature;
    m_workspace = workspace;

    for( final Iterator iter = m_featureControls.iterator(); iter.hasNext(); )
    {
      final IFeatureControl fc = (IFeatureControl)iter.next();
      fc.setFeature( workspace, feature );
    }
  }

  public void addView( final URL url )
  {
    try
    {
      final Object unmarshal = FeatureviewHelper.UNMARSHALLER.unmarshal( url );
      if( unmarshal instanceof FeatureviewType )
        addView( (FeatureviewType)unmarshal );
      else if( unmarshal instanceof FeaturetemplateType )
      {
        final FeaturetemplateType ftt = (FeaturetemplateType)unmarshal;
        final List view = ftt.getView();
        for( final Iterator vIt = view.iterator(); vIt.hasNext(); )
          addView( (FeatureviewType)vIt.next() );
      }
      else
        System.out.println( getClass().getName() + ": Unsupported type: " + unmarshal.getClass().getName() + " in "
            + url.toString() );
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();
    }
  }

  public void addView( final FeatureviewType view )
  {
    m_viewMap.put( view.getTypename(), view );
  }

  public void disposeControl()
  {
    for( final Iterator iter = m_featureControls.iterator(); iter.hasNext(); )
    {
      final IFeatureControl fc = (IFeatureControl)iter.next();
      fc.dispose();
    }
    m_featureControls.clear();

    for( final Iterator iter = m_swtControls.iterator(); iter.hasNext(); )
    {
      final Control c = (Control)iter.next();
      c.dispose();
    }
    m_swtControls.clear();

    if( m_control != null )
    {
      m_control.dispose();
      m_control = null;
    }
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#addChangeListener(org.kalypso.ogc.gml.featureview.IFeatureChangeListener)
   */
  public void addChangeListener( final IFeatureChangeListener l )
  {
    for( final Iterator iter = m_featureControls.iterator(); iter.hasNext(); )
      ( (IFeatureControl)iter.next() ).addChangeListener( l );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#removeChangeListener(org.kalypso.ogc.gml.featureview.IFeatureChangeListener)
   */
  public void removeChangeListener( final IFeatureChangeListener l )
  {
    for( final Iterator iter = m_featureControls.iterator(); iter.hasNext(); )
      ( (IFeatureControl)iter.next() ).removeChangeListener( l );
  }

  public Control getControl()
  {
    return m_control;
  }

  private void applyAnnotation( final Control label, final String propertyName, final Feature feature )
  {
    if( propertyName != null && propertyName.length() > 0 )
    {
      final FeatureTypeProperty ftp = feature.getFeatureType().getProperty( propertyName );
      if( ftp != null )
      {
        final Annotation annotation = ftp.getAnnotation( KalypsoGisPlugin.getDefault().getPluginPreferences()
            .getString( IKalypsoPreferences.LANGUAGE ) );

        if( annotation != null )
        {
          try
          {
            final Method method = label.getClass().getMethod( "setText", new Class[]
            { String.class } );
            if( method != null )
              method.invoke( label, new String[]
              { annotation.getLabel() } );
          }
          catch( final Exception e )
          {
            // ignore, this control has not text
          }

          label.setToolTipText( annotation.getTooltip() );
        }
      }
    }
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#getWorkspace()
   */
  public GMLWorkspace getWorkspace()
  {
    return m_workspace;
  }
}