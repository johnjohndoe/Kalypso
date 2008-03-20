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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.contribs.eclipse.jface.wizard.ResourceChooserGroup;
import org.kalypso.contribs.eclipse.ui.dialogs.KalypsoResourceSelectionDialog;
import org.kalypso.contribs.eclipse.ui.dialogs.ResourceSelectionValidator;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IPropertyTypeFilter;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.PropertyUtils;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.filter.IProfilePointFilter;
import org.kalypso.model.wspm.ui.Messages;
import org.kalypso.model.wspm.ui.wizard.ThemeAndPropertyChooserGroup.PropertyDescriptor;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.IKalypsoThemeFilter;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author Gernot Belger
 */
public class IntersectRoughnessPage extends WizardPage implements IUpdateable, IKalypsoThemeFilter
{
  private final static String SETTINGS_FILTER_IDS = "settings.filters.ids"; //$NON-NLS-1$

  private final ResourceChooserGroup m_assignmentGroup = new ResourceChooserGroup( this, Messages.IntersectRoughnessPage_1, Messages.IntersectRoughnessPage_2 );

  private final ThemeAndPropertyChooserGroup m_themeGroup;

  private final List<IProfilePointFilter> m_selectedFilters = new ArrayList<IProfilePointFilter>();

  private final IMapModell m_modell;

  private final PropertyDescriptor m_geoPd;

  private final PropertyDescriptor m_valuePd;

  public IntersectRoughnessPage( final IMapModell modell )
  {
    super( "intersectRoughnessPage", Messages.IntersectRoughnessPage_4, null ); //$NON-NLS-1$

    setMessage( Messages.IntersectRoughnessPage_5 );

    m_modell = modell;

    final IPropertyTypeFilter geoFilter = new IPropertyTypeFilter()
    {
      public boolean accept( final IPropertyType pt )
      {
        return pt instanceof IValuePropertyType && ((IValuePropertyType) pt).getValueQName().equals( GeometryUtilities.QN_POLYGON_PROPERTY );
      }
    };

    final IPropertyTypeFilter valueFilter = new IPropertyTypeFilter()
    {
      public boolean accept( IPropertyType pt )
      {
        return pt instanceof IValuePropertyType && !((IValuePropertyType) pt).isGeometry();
      }
    };

    m_geoPd = new PropertyDescriptor( Messages.IntersectRoughnessPage_6, geoFilter, true );
    m_valuePd = new PropertyDescriptor( Messages.IntersectRoughnessPage_7, valueFilter, false );

    final PropertyDescriptor[] pds = new PropertyDescriptor[] { m_geoPd, m_valuePd };
    m_themeGroup = new ThemeAndPropertyChooserGroup( this, m_modell, this, pds );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout() );

    /* Polygone Group */
    m_themeGroup.setDialogSettings( getDialogSettings() );
    final Group polygoneGroup = m_themeGroup.createControl( composite );
    polygoneGroup.setLayoutData( new GridData( SWT.FILL, SWT.BEGINNING, true, false ) );
    polygoneGroup.setText( Messages.IntersectRoughnessPage_8 );

    /* Assignment Group */
    final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();

    m_assignmentGroup.setDialogSettings( getDialogSettings() );
    final IResource initialSelection = getAssignmentPath() == null ? null : root.findMember( getAssignmentPath() );
    final KalypsoResourceSelectionDialog dialog = new KalypsoResourceSelectionDialog( getShell(), initialSelection, Messages.IntersectRoughnessPage_9, new String[] { "gml" }, root, new ResourceSelectionValidator() ); //$NON-NLS-2$ //$NON-NLS-1$
    m_assignmentGroup.setSelectionDialog( dialog );

    final Control assignmentGroup = m_assignmentGroup.createControl( composite );
    assignmentGroup.setLayoutData( new GridData( SWT.FILL, SWT.BEGINNING, true, false ) );

    createFilterGroup( composite );

    setControl( composite );
  }

  private void createFilterGroup( final Composite composite )
  {
    final IProfilePointFilter[] filters = KalypsoModelWspmCoreExtensions.getProfilePointFilters();

    final Group group = new Group( composite, SWT.NONE );
    group.setLayoutData( new GridData( SWT.FILL, SWT.BEGINNING, true, false ) );
    group.setLayout( new GridLayout( 1, false ) );
    group.setText( Messages.IntersectRoughnessPage_11 );

    /* theme chooser */
    new Label( group, SWT.NONE ).setText( Messages.IntersectRoughnessPage_12 );

    final Set<String> ids = new HashSet<String>();
    final IDialogSettings dialogSettings = getDialogSettings();
    if( dialogSettings != null )
    {
      final String[] idArray = dialogSettings.getArray( SETTINGS_FILTER_IDS );
      if( idArray != null )
        Collections.addAll( ids, idArray );
    }

    for( final IProfilePointFilter filter : filters )
    {
      final String id = filter.getId();
      final boolean select = ids.contains( id );
      addFilterCheckbox( group, filter, select );
    }
  }

  private void addFilterCheckbox( final Group group, final IProfilePointFilter filter, final boolean select )
  {
    final Button button = new Button( group, SWT.CHECK );
    button.setText( filter.getName() );
    button.setToolTipText( filter.getDescription() );
    button.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        selectFilter( filter, button.getSelection() );
      }
    } );

    button.setSelection( select );
    selectFilter( filter, select );
  }

  protected void selectFilter( final IProfilePointFilter filter, final boolean select )
  {
    if( select )
      m_selectedFilters.add( filter );
    else
      m_selectedFilters.remove( filter );

    final IDialogSettings dialogSettings = getDialogSettings();
    if( dialogSettings != null )
    {
      final String[] ids = new String[m_selectedFilters.size()];
      for( int i = 0; i < ids.length; i++ )
        ids[i] = m_selectedFilters.get( i ).getId();
      dialogSettings.put( SETTINGS_FILTER_IDS, ids );
    }

    update();
  }

  public IProfilePointFilter[] getSelectedPointFilter( )
  {
    return m_selectedFilters.toArray( new IProfilePointFilter[m_selectedFilters.size()] );
  }

  private IKalypsoFeatureTheme getPolygoneTheme( )
  {
    return (IKalypsoFeatureTheme) m_themeGroup.getTheme();
  }

  public FeatureList getPolygoneFeatures( )
  {
    final IKalypsoFeatureTheme polygoneTheme = getPolygoneTheme();
    if( polygoneTheme == null )
      return null;

    return polygoneTheme.getFeatureList();
  }

  public IPropertyType getPolygoneGeomProperty( )
  {
    return m_themeGroup.getProperty( m_geoPd );
  }

  public IPropertyType getPolygoneValueProperty( )
  {
    return m_themeGroup.getProperty( m_valuePd );
  }

  public IPath getAssignmentPath( )
  {
    return m_assignmentGroup.getPath();
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.wizard.IUpdateable#update()
   */
  public void update( )
  {
    final IPath assignmentPath = m_assignmentGroup.getPath();
    final IKalypsoTheme polygoneTheme = getPolygoneTheme();
    final IPropertyType polygoneGeomProperty = getPolygoneGeomProperty();
    final IPropertyType polygoneValueProperty = getPolygoneValueProperty();

    final boolean pageComplete = polygoneTheme != null && polygoneGeomProperty != null && polygoneValueProperty != null && assignmentPath != null;

    setPageComplete( pageComplete );

    if( polygoneTheme == null )
      setErrorMessage( Messages.IntersectRoughnessPage_13 );
    else if( polygoneValueProperty == null )
      setErrorMessage( Messages.IntersectRoughnessPage_14 );
    else if( assignmentPath == null )
      setErrorMessage( Messages.IntersectRoughnessPage_15 );
    else
    {
      setErrorMessage( null );
      setMessage( Messages.IntersectRoughnessPage_16 );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoThemeFilter#accept(org.kalypso.ogc.gml.IKalypsoTheme)
   */
  public boolean accept( final IKalypsoTheme theme )
  {
    if( theme instanceof IKalypsoFeatureTheme )
    {
      final IKalypsoFeatureTheme featureTheme = (IKalypsoFeatureTheme) theme;
      final IFeatureType featureType = featureTheme.getFeatureType();
      if( featureType != null )
      {
        final IPropertyType[] polygoneProperties = PropertyUtils.filterProperties( featureType, m_geoPd.filter );
        if( polygoneProperties.length > 0 )
          return true;
      }
    }

    return false;
  }

}
