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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.jface.window.Window;
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
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.ui.dialogs.KalypsoResourceSelectionDialog;
import org.kalypso.contribs.eclipse.ui.dialogs.ResourceSelectionValidator;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.filterdialog.model.FeatureTypeLabelProvider;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author Gernot Belger
 */
public class IntersectRoughnessPage extends WizardPage
{
  private static final String SETTINGS_ASSIGNMENT_PATH = "settings.assignment.path";

  private final IMapModell m_modell;

  private IKalypsoFeatureTheme m_polygoneTheme = null;

  private IPropertyType m_polygoneGeomProperty = null;

  private IPropertyType m_polygoneValueProperty = null;

  private IPath m_assignmentPath;

  public IntersectRoughnessPage( final IMapModell modell )
  {
    super( "intersectRoughnessPage", "Rauheiten zuweisen", null );

    setMessage( "Bitte wählen Sie aus, wie die Rauheiten zugewiesen werden sollen." );

    m_modell = modell;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout() );

    createPolygoneGroup( composite );
    createAssignmentGroup( composite );

    setControl( composite );
  }

  private void createAssignmentGroup( final Composite parent )
  {
    final IDialogSettings dialogSettings = getDialogSettings();
    final String lastPathName = dialogSettings == null ? null : dialogSettings.get( SETTINGS_ASSIGNMENT_PATH );

    final Group assignmentGroup = new Group( parent, SWT.NONE );
    assignmentGroup.setLayoutData( new GridData( SWT.FILL, SWT.BEGINNING, true, false ) );
    assignmentGroup.setLayout( new GridLayout( 3, false ) );
    assignmentGroup.setText( "Zuordnung" );

    /* theme chooser */
    new Label( assignmentGroup, SWT.NONE ).setText( "&Zuordnung" );
    final Text text = new Text( assignmentGroup, SWT.BORDER );
    text.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    text.setEditable( false );

    final Button button = new Button( assignmentGroup, SWT.NONE );
    button.setText( "&Datei..." );
    button.setToolTipText( "Datei aus dem Arbeitsbereich wählen" );
    button.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        assignmentButtonPressed( text );
      }
    } );

    if( lastPathName != null )
      setAssignmentPath( text, Path.fromPortableString( lastPathName ) );
  }

  protected void assignmentButtonPressed( final Text text )
  {
    final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();

    final IResource initialSelection = m_assignmentPath == null ? null : root.findMember( m_assignmentPath );
    final KalypsoResourceSelectionDialog dialog = new KalypsoResourceSelectionDialog( getShell(), initialSelection, "Zuordnungsdatei", new String[] { "gml" }, root, new ResourceSelectionValidator() );
    if( dialog.open() != Window.OK )
      return;

    final Object[] result = dialog.getResult();
    final IPath path = result.length == 0 ? null : (IPath) result[0];
    setAssignmentPath( text, path );
  }

  private void setAssignmentPath( final Text text, final IPath path )
  {
    m_assignmentPath = path;
    text.setText( m_assignmentPath == null ? "" : m_assignmentPath.toOSString() );

    final IDialogSettings dialogSettings = getDialogSettings();
    if( dialogSettings != null )
      dialogSettings.put( SETTINGS_ASSIGNMENT_PATH, path.toPortableString() );

    updatePageComplete();
  }

  private void createPolygoneGroup( final Composite parent )
  {
    final Group polygoneGroup = new Group( parent, SWT.NONE );
    polygoneGroup.setLayoutData( new GridData( SWT.FILL, SWT.BEGINNING, true, false ) );
    polygoneGroup.setLayout( new GridLayout( 2, false ) );
    polygoneGroup.setText( "Polygone" );

    /* theme chooser */
    new Label( polygoneGroup, SWT.NONE ).setText( "&Thema" );
    final ComboViewer themeComboViewer = new ComboViewer( polygoneGroup, SWT.DROP_DOWN | SWT.READ_ONLY );
    themeComboViewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    themeComboViewer.setContentProvider( new ArrayContentProvider() );
    themeComboViewer.setLabelProvider( new LabelProvider() );
    themeComboViewer.setSorter( new ViewerSorter() );
    final List<IKalypsoFeatureTheme> polygoneThemes = getPolygoneThemes();
    themeComboViewer.setInput( polygoneThemes );

    themeComboViewer.getControl().setEnabled( polygoneThemes.size() > 0 );

    /* Geo-Property chooser */
    final Label geoLabel = new Label( polygoneGroup, SWT.NONE );
    geoLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );
    geoLabel.setText( "&Geometrie" );
    final ComboViewer geoComboViewer = new ComboViewer( polygoneGroup, SWT.DROP_DOWN | SWT.READ_ONLY );
    geoComboViewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    geoComboViewer.setContentProvider( new ArrayContentProvider() );
    geoComboViewer.setLabelProvider( new FeatureTypeLabelProvider() );
    geoComboViewer.setSorter( new ViewerSorter() );

    /* Value-Property chooser */
    new Label( polygoneGroup, SWT.NONE ).setText( "&Wert" );
    final ComboViewer valueComboViewer = new ComboViewer( polygoneGroup, SWT.DROP_DOWN | SWT.READ_ONLY );
    valueComboViewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    valueComboViewer.setContentProvider( new ArrayContentProvider() );
    valueComboViewer.setLabelProvider( new FeatureTypeLabelProvider() );
    valueComboViewer.setSorter( new ViewerSorter() );

    /* event handlers */
    themeComboViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleSelectionChanged( (IStructuredSelection) event.getSelection(), geoComboViewer, valueComboViewer, geoLabel );
      }
    } );

    geoComboViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleGeoChanged( (IStructuredSelection) event.getSelection() );
      }
    } );

    valueComboViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleValueChanged( (IStructuredSelection) event.getSelection() );
      }
    } );

    if( !polygoneThemes.isEmpty() )
      themeComboViewer.setSelection( new StructuredSelection( polygoneThemes.get( 0 ) ) );
  }

  protected void handleGeoChanged( final IStructuredSelection selection )
  {
    final Object firstElement = selection.getFirstElement();
    if( m_polygoneGeomProperty == firstElement )
      return;

    m_polygoneGeomProperty = (IPropertyType) firstElement;

    updatePageComplete();
  }

  protected void handleValueChanged( final IStructuredSelection selection )
  {
    final Object firstElement = selection.getFirstElement();
    if( m_polygoneValueProperty == firstElement )
      return;

    m_polygoneValueProperty = (IPropertyType) firstElement;

    updatePageComplete();
  }

  protected void handleSelectionChanged( final IStructuredSelection selection, final ComboViewer geoComboViewer, final ComboViewer valueComboViewer, final Label geoLabel )
  {
    final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme) selection.getFirstElement();
    if( theme == m_polygoneTheme )
      return;

    m_polygoneTheme = theme;

    if( theme == null )
    {
      geoComboViewer.setInput( new Object[] {} );
      valueComboViewer.setInput( new Object[] {} );
    }
    else
    {
      final IFeatureType featureType = theme.getFeatureType();
      final IPropertyType[] geoPts = getPolygoneProperties( featureType );
      geoComboViewer.setInput( geoPts );
      geoComboViewer.setSelection( new StructuredSelection( geoPts[0] ) );

      ((GridData) geoLabel.getLayoutData()).exclude = geoPts.length < 2;
      ((GridData) geoComboViewer.getControl().getLayoutData()).exclude = geoPts.length < 2;

      final IPropertyType[] valuePts = getValueProperties( featureType );
      valueComboViewer.setInput( valuePts );
      if( valuePts.length > 0 )
        valueComboViewer.setSelection( new StructuredSelection( valuePts[0] ) );
    }

    geoComboViewer.getControl().setEnabled( theme != null );
    valueComboViewer.getControl().setEnabled( theme != null );
  }

  private List<IKalypsoFeatureTheme> getPolygoneThemes( )
  {
    final IKalypsoTheme[] allThemes = m_modell.getAllThemes();
    final List<IKalypsoFeatureTheme> themes = new ArrayList<IKalypsoFeatureTheme>( allThemes.length );
    for( final IKalypsoTheme theme : allThemes )
    {
      if( theme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme featureTheme = (IKalypsoFeatureTheme) theme;
        final IFeatureType featureType = featureTheme.getFeatureType();
        final IPropertyType[] polygoneProperties = getPolygoneProperties( featureType );
        if( polygoneProperties.length > 0 )
          themes.add( featureTheme );
      }
    }

    return themes;
  }

  private IPropertyType[] getPolygoneProperties( final IFeatureType featureType )
  {
    final List<IPropertyType> pts = new ArrayList<IPropertyType>();

    final IValuePropertyType[] allGeomteryProperties = featureType.getAllGeomteryProperties();
    for( final IValuePropertyType vpt : allGeomteryProperties )
    {
      if( vpt.getValueQName().equals( GeometryUtilities.QN_POLYGON_PROPERTY ) )
        pts.add( vpt );
    }

    return pts.toArray( new IPropertyType[pts.size()] );
  }

  private IPropertyType[] getValueProperties( IFeatureType featureType )
  {
    final List<IPropertyType> pts = new ArrayList<IPropertyType>();

    final IPropertyType[] allProperties = featureType.getProperties();
    for( final IPropertyType vpt : allProperties )
    {
      if( vpt instanceof IValuePropertyType && !((IValuePropertyType) vpt).isGeometry() )
        pts.add( vpt );
    }

    return pts.toArray( new IPropertyType[pts.size()] );
  }

  public FeatureList getPolygoneFeatures( )
  {
    if( m_polygoneTheme == null )
      return null;

    return m_polygoneTheme.getFeatureList();
  }

  public IPropertyType getPolygoneGeomProperty( )
  {
    return m_polygoneGeomProperty;
  }

  public IPropertyType getPolygoneValueProperty( )
  {
    return m_polygoneValueProperty;
  }

  public IPath getAssignmentPath( )
  {
    return m_assignmentPath;
  }

  private void updatePageComplete( )
  {
    final boolean pageComplete = m_polygoneTheme != null && m_polygoneGeomProperty != null && m_polygoneValueProperty != null && m_assignmentPath != null;

    setPageComplete( pageComplete );

    if( m_polygoneTheme == null )
      setErrorMessage( "Es sind keine Polygon-Themen in der Karte vorhanden. Zuweisung nicht möglich." );
    else if( m_polygoneValueProperty == null )
      setErrorMessage( "Das gewählte Thema hat keine Wert-Eigenschaften. Zuweisung nicht möglich." );
    else if( m_assignmentPath == null )
      setErrorMessage( "Ess muss ein Pfad auf eine Zuordnungsdatei angegeben werden." );
    else
    {
      setErrorMessage( null );
      setMessage( "Bitte wählen Sie aus, wie die Rauheiten zugewiesen werden sollen." );
    }
  }

}
