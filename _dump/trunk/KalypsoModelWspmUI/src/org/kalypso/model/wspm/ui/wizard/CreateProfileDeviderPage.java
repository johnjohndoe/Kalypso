/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import javax.xml.namespace.QName;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IPropertyTypeFilter;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.PropertyUtils;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.ui.wizard.ThemeAndPropertyChooserGroup.PropertyDescriptor;
import org.kalypso.observation.result.IComponent;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.IKalypsoThemeFilter;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author Gernot Belger
 */
public class CreateProfileDeviderPage extends WizardPage implements IUpdateable, IKalypsoThemeFilter
{
  private static final String SETTINGS_DEVIDER = "settings.devider.type";

  private static final String SETTINGS_DELETE_EXISTING = "settings.delete.existing";

  private final ThemeAndPropertyChooserGroup m_themeGroup;

  private final PropertyDescriptor m_geoPd;

  private IComponent m_deviderType = null;

  private boolean m_deleteExisting = false;

  private final IKalypsoFeatureTheme m_profileTheme;

  public CreateProfileDeviderPage( final IKalypsoFeatureTheme profileTheme )
  {
    super( "createProfileDeviderPage", "Fließzonen erzeugen", null );

    m_profileTheme = profileTheme;

    setMessage( "Bitte wählen Sie aus, wie die Fließzonen erzeugt werden sollen." );

    final IPropertyTypeFilter geoFilter = new IPropertyTypeFilter()
    {
      public boolean accept( final IPropertyType pt )
      {
        if( pt instanceof IValuePropertyType )
        {
          final QName valueQName = ((IValuePropertyType) pt).getValueQName();
          return valueQName.equals( GeometryUtilities.QN_LINE_STRING_PROPERTY ) || valueQName.equals( GeometryUtilities.QN_POLYGON_PROPERTY )
              || valueQName.equals( GeometryUtilities.QN_MULTI_LINE_STRING_PROPERTY ) || valueQName.equals( GeometryUtilities.QN_MULTI_POLYGON_PROPERTY );
        }
        return false;
      }
    };

    m_geoPd = new PropertyDescriptor( "&Geometry", geoFilter, true );

    final PropertyDescriptor[] pds = new PropertyDescriptor[] { m_geoPd };
    m_themeGroup = new ThemeAndPropertyChooserGroup( this, profileTheme.getMapModell(), this, pds );
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
    final Group themeGroup = m_themeGroup.createControl( composite );
    themeGroup.setLayoutData( new GridData( SWT.FILL, SWT.BEGINNING, true, false ) );
    themeGroup.setText( "Linien oder Polygone" );

    /* Devider Group */
    final Group deviderGroup = createDeviderGroup( composite );
    deviderGroup.setLayoutData( new GridData( SWT.FILL, SWT.BEGINNING, true, false ) );
    deviderGroup.setText( "Art der Fließzonen-Begrenzung" );

    /* Options */
    createOptions( composite );

    setControl( composite );
  }

  private void createOptions( final Composite composite )
  {
    final Button deleteExistingCheckbox = new Button( composite, SWT.CHECK );
    deleteExistingCheckbox.setText( "vorhandene Begrenzer löschen" );
    deleteExistingCheckbox.setToolTipText( "falls aktiviert werden die vorhandenen Begrenzer des selektierten Typs aus den Profilen gelöscht" );
    deleteExistingCheckbox.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        handleDeleteExistingChanged( deleteExistingCheckbox.getSelection() );
      }
    } );

    final IDialogSettings dialogSettings = getDialogSettings();
    if( dialogSettings != null )
    {
      final boolean deleteExisting = dialogSettings.getBoolean( SETTINGS_DELETE_EXISTING );
      deleteExistingCheckbox.setSelection( deleteExisting );
      m_deleteExisting = deleteExisting;
    }
  }

  protected void handleDeleteExistingChanged( final boolean deleteExisting )
  {
    if( m_deleteExisting == deleteExisting )
      return;

    m_deleteExisting = deleteExisting;

    final IDialogSettings dialogSettings = getDialogSettings();
    if( dialogSettings != null )
      dialogSettings.put( SETTINGS_DELETE_EXISTING, deleteExisting );
  }

  private Group createDeviderGroup( final Composite composite )
  {
    final Group group = new Group( composite, SWT.NONE );
    group.setLayout( new GridLayout( 2, false ) );

    new Label( group, SWT.NONE ).setText( "&Art:" );

    final ComboViewer viewer = new ComboViewer( group, SWT.READ_ONLY | SWT.DROP_DOWN );
    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( final Object element )
      {
        final IComponent comp = (IComponent) element;
        return comp.getName();
      }
    } );
    viewer.setSorter( new ViewerSorter() );

    final Object object = m_profileTheme.getFeatureList().get( 0 );

    final IProfileFeature profile = (IProfileFeature) FeatureHelper.getFeature( m_profileTheme.getWorkspace(), object );
    final String type = profile.getProfil().getType();

    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( type );

    final String[] markerTypes = provider.getPointProperties();
    IComponent[] markerComponents = new IComponent[markerTypes.length];
    for( int i = 0; i < markerTypes.length; i++ )
      markerComponents[i] = provider.getPointProperty( markerTypes[i] );

    viewer.setInput( markerComponents );

    viewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        final Object firstElement = (selection).getFirstElement();
        handleDeviderChanged( (IComponent) firstElement );
      }
    } );

    final IDialogSettings dialogSettings = getDialogSettings();
    StructuredSelection deviderSelection = new StructuredSelection( markerComponents[0] );
    if( dialogSettings != null )
    {
      final String typeName = dialogSettings.get( SETTINGS_DEVIDER );
      for( IComponent component : markerComponents )
      {
        if( component.getId().equals( typeName ) )
          deviderSelection = new StructuredSelection( component );
      }
    }
    viewer.setSelection( deviderSelection );

    return group;
  }

  protected void handleDeviderChanged( final IComponent type )
  {
    if( m_deviderType == type )
      return;

    m_deviderType = type;

    final IDialogSettings dialogSettings = getDialogSettings();
    if( dialogSettings != null )
      dialogSettings.put( SETTINGS_DEVIDER, type.getId() );
  }

  private IKalypsoFeatureTheme getTheme( )
  {
    return (IKalypsoFeatureTheme) m_themeGroup.getTheme();
  }

  public FeatureList getFeatures( )
  {
    final IKalypsoFeatureTheme polygoneTheme = getTheme();
    if( polygoneTheme == null )
      return null;

    return polygoneTheme.getFeatureList();
  }

  public IPropertyType getGeomProperty( )
  {
    return m_themeGroup.getProperty( m_geoPd );
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.wizard.IUpdateable#update()
   */
  public void update( )
  {
    final IKalypsoTheme polygoneTheme = getTheme();
    final IPropertyType polygoneGeomProperty = getGeomProperty();

    final boolean pageComplete = polygoneTheme != null && polygoneGeomProperty != null;

    setPageComplete( pageComplete );

    if( polygoneTheme == null )
      setErrorMessage( "Es sind keine Polygon oder Linien-Themen in der Karte vorhanden. Zuweisung nicht möglich." );
    else
    {
      setErrorMessage( null );
      setMessage( "Bitte wählen Sie aus, wie die Rauheiten zugewiesen werden sollen." );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoThemeFilter#accept(org.kalypso.ogc.gml.IKalypsoTheme)
   */
  public boolean accept( final IKalypsoTheme theme )
  {
    if( theme instanceof IKalypsoFeatureTheme && theme != m_profileTheme )
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

  public IComponent getDeviderType( )
  {
    return m_deviderType;
  }

}
