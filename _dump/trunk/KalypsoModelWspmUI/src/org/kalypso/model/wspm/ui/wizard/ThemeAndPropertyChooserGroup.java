/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.ui.wizard;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IPropertyTypeFilter;
import org.kalypso.gmlschema.property.PropertyUtils;
import org.kalypso.model.wspm.ui.Messages;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.IKalypsoThemeFilter;
import org.kalypso.ogc.gml.filterdialog.model.FeatureTypeLabelProvider;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.MapModellHelper;

/**
 * @author Gernot Belger
 */
public class ThemeAndPropertyChooserGroup
{
  public static class PropertyDescriptor
  {
    public final boolean hideIfUnique;

    public final String label;

    public final IPropertyTypeFilter filter;

    public PropertyDescriptor( @SuppressWarnings("hiding")//$NON-NLS-1$
    final String label, @SuppressWarnings("hiding")//$NON-NLS-1$
    final IPropertyTypeFilter filter, @SuppressWarnings("hiding")//$NON-NLS-1$
    final boolean hideIfUnique )
    {
      this.label = label;
      this.filter = filter;
      this.hideIfUnique = hideIfUnique;
    }
  }

  private final static String SETTINGS_THEME = "settings.choosen.theme"; //$NON-NLS-1$

  private final static String SETTINGS_PROPERTIES = "settings.choosen.properties"; //$NON-NLS-1$

  private final IUpdateable m_updatable;

  private final IKalypsoThemeFilter m_themeFilter;

  private final IMapModell m_modell;

  private IDialogSettings m_dialogSettings;

  private IKalypsoFeatureTheme m_choosenTheme = null;

  private final Map<PropertyDescriptor, IPropertyType> m_properties = new LinkedHashMap<PropertyDescriptor, IPropertyType>();

  /**
   * @param filer
   *            only themes accepted by this filter are shown to the user
   */
  public ThemeAndPropertyChooserGroup( final IUpdateable updatable, final IMapModell modell, final IKalypsoThemeFilter themeFilter, final PropertyDescriptor[] properties )
  {
    m_updatable = updatable;
    m_modell = modell;
    m_themeFilter = themeFilter;

    for( final PropertyDescriptor descriptor : properties )
      m_properties.put( descriptor, null );
  }

  public void setDialogSettings( final IDialogSettings dialogSettings )
  {
    m_dialogSettings = dialogSettings;
  }

  public Group createControl( final Composite parent )
  {
    final Group group = new Group( parent, SWT.NONE );
    group.setLayout( new GridLayout( 2, false ) );

    /* theme chooser */
    new Label( group, SWT.NONE ).setText( Messages.ThemeAndPropertyChooserGroup_5 );
    final ComboViewer themeComboViewer = new ComboViewer( group, SWT.DROP_DOWN | SWT.READ_ONLY );
    themeComboViewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    themeComboViewer.setContentProvider( new ArrayContentProvider() );
    themeComboViewer.setLabelProvider( new LabelProvider() );
    themeComboViewer.setSorter( new ViewerSorter() );
    final IKalypsoTheme[] polygoneThemes = MapModellHelper.filterThemes( m_modell, m_themeFilter );
    themeComboViewer.setInput( polygoneThemes );
    themeComboViewer.getControl().setEnabled( polygoneThemes.length > 0 );

    /* Property choosers */
    final Map<PropertyDescriptor, Control[]> propertyControls = new HashMap<PropertyDescriptor, Control[]>();
    final Map<PropertyDescriptor, ComboViewer> propertyCombos = new HashMap<PropertyDescriptor, ComboViewer>();

    for( final PropertyDescriptor pd : m_properties.keySet() )
      createPropertyChooser( group, pd, propertyControls, propertyCombos );

    themeComboViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleSelectionChanged( (IStructuredSelection) event.getSelection(), propertyCombos, propertyControls );
      }
    } );

    IStructuredSelection themeSelection = polygoneThemes.length > 0 ? new StructuredSelection( polygoneThemes[0] ) : StructuredSelection.EMPTY;

    for( final PropertyDescriptor pd : propertyCombos.keySet() )
    {
      final ComboViewer viewer = propertyCombos.get( pd );

      viewer.addSelectionChangedListener( new ISelectionChangedListener()
      {
        public void selectionChanged( final SelectionChangedEvent event )
        {
          handlePropertyChanged( (IStructuredSelection) event.getSelection(), pd );
        }
      } );
    }

    // TRICKY: because setting the selection to the themes changes
    // the dialog-settings of the poly- and value property we have to read them immediately
    IDialogSettings propertySetting = null;
    if( m_dialogSettings != null )
    {
      final String themeSetting = m_dialogSettings.get( SETTINGS_THEME );

      propertySetting = m_dialogSettings.getSection( SETTINGS_PROPERTIES );

      if( themeSetting != null )
      {
        for( final IKalypsoTheme theme : polygoneThemes )
        {
          if( themeSetting.equals( theme.getName() ) )
          {
            themeSelection = new StructuredSelection( theme );
            break;
          }
        }
      }
    }

    themeComboViewer.setSelection( themeSelection );

    /* Apply dialog settings */
    final Map<PropertyDescriptor, IStructuredSelection> propertySelection = new HashMap<PropertyDescriptor, IStructuredSelection>();

    if( m_dialogSettings != null )
    {
      if( propertySetting != null )
      {
        for( final PropertyDescriptor pd : m_properties.keySet() )
        {
          final String lastProp = propertySetting.get( pd.label );
          if( lastProp != null )
          {
            final IPropertyType[] pts = (IPropertyType[]) propertyCombos.get( pd ).getInput();
            for( final IPropertyType pt : pts )
            {
              if( lastProp.equals( pt.getQName().toString() ) )
              {
                propertySelection.put( pd, new StructuredSelection( pt ) );
                break;
              }
            }
          }
        }
      }
    }

    for( final PropertyDescriptor pd : m_properties.keySet() )
    {
      final IStructuredSelection selection = propertySelection.get( pd );
      if( selection != null )
        propertyCombos.get( pd ).setSelection( selection );
    }

    return group;
  }

  private void createPropertyChooser( final Group group, final PropertyDescriptor pd, final Map<PropertyDescriptor, Control[]> propertyControls, final Map<PropertyDescriptor, ComboViewer> propertyCombos )
  {
    /* Geo-Property chooser */
    final Label label = new Label( group, SWT.NONE );
    label.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );
    label.setText( pd.label );

    final ComboViewer comboViewer = new ComboViewer( group, SWT.DROP_DOWN | SWT.READ_ONLY );
    comboViewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    comboViewer.setContentProvider( new ArrayContentProvider() );
    comboViewer.setLabelProvider( new FeatureTypeLabelProvider() );
    comboViewer.setSorter( new ViewerSorter() );

    propertyCombos.put( pd, comboViewer );
    propertyControls.put( pd, new Control[] { label, comboViewer.getControl() } );
  }

  protected void handlePropertyChanged( final IStructuredSelection selection, final PropertyDescriptor pd )
  {
    final Object firstElement = selection.getFirstElement();
    if( m_properties.get( pd ) == firstElement )
      return;

    m_properties.put( pd, (IPropertyType) firstElement );

    if( m_dialogSettings != null )
    {
      final IDialogSettings propSettings = m_dialogSettings.addNewSection( SETTINGS_PROPERTIES );
      for( final PropertyDescriptor desc : m_properties.keySet() )
      {
        final IPropertyType pt = m_properties.get( desc );
        if( pt != null )
          propSettings.put( desc.label, pt.getQName().toString() );
      }
    }

    m_updatable.update();
  }

  protected void handleSelectionChanged( final IStructuredSelection selection, final Map<PropertyDescriptor, ComboViewer> propertyCombos, final Map<PropertyDescriptor, Control[]> propertyControls )
  {
    final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme) selection.getFirstElement();
    if( theme == m_choosenTheme )
      return;

    m_choosenTheme = theme;

    if( theme == null )
    {
      for( final ComboViewer comboViewer : propertyCombos.values() )
        comboViewer.setInput( new Object[] {} );
    }
    else
    {
      final IFeatureType featureType = theme.getFeatureType();

      for( final PropertyDescriptor pd : propertyCombos.keySet() )
      {
        final ComboViewer comboViewer = propertyCombos.get( pd );
        final Control[] controls = propertyControls.get( pd );

        final IPropertyType[] pts = PropertyUtils.filterProperties( featureType, pd.filter );
        comboViewer.setInput( pts );
        if( pts.length == 0 )
          comboViewer.setSelection( StructuredSelection.EMPTY );
        else
          comboViewer.setSelection( new StructuredSelection( pts[0] ) );

        final boolean hideIfUnique = pd.hideIfUnique;
        for( final Control propertyControl : controls )
          ((GridData) propertyControl.getLayoutData()).exclude = hideIfUnique && pts.length < 2;
      }
    }

    if( m_dialogSettings != null )
      m_dialogSettings.put( SETTINGS_THEME, m_choosenTheme == null ? "" : m_choosenTheme.getName() ); //$NON-NLS-1$

    for( final ComboViewer comboViewer : propertyCombos.values() )
      comboViewer.getControl().setEnabled( theme != null );
  }

  public IPropertyType getProperty( final PropertyDescriptor pd )
  {
    return m_properties.get( pd );
  }

  public IKalypsoTheme getTheme( )
  {
    return m_choosenTheme;
  }
}
