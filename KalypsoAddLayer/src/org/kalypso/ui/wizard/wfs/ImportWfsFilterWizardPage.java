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
package org.kalypso.ui.wizard.wfs;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.outline.GisMapOutlineViewer;
import org.kalypso.ui.editor.mapeditor.GisMapEditor;
import org.kalypsodeegree.filterencoding.Filter;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.filterencoding.ComplexFilter;
import org.kalypsodeegree_impl.filterencoding.OperationDefines;
import org.kalypsodeegree_impl.filterencoding.PropertyName;
import org.kalypsodeegree_impl.filterencoding.SpatialOperation;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author kuepfer
 */
public class ImportWfsFilterWizardPage extends WizardPage
{

  Text m_distance;

  Button m_bufferButton;

  GM_Object m_selectedGeom;

  private Button m_activeSelectionButton;

  GisMapOutlineViewer m_gisMapOutlineViewer;

  GM_Surface m_BBox;

  private Button m_BBoxButton;

  Button m_intersectButton;

  Button m_containsButton;

  Button m_touchesButton;

  private Combo m_spatialOpsCombo;

  String OPS_INTERSECTION = "Schneidet das Objekt";

  String OPS_CONTAINS = "Enthält das Objekt";

  String OPS_TOUCHES = "Berührt das Objekt";

  String m_themeName;

  public ImportWfsFilterWizardPage( String pageName, String title, ImageDescriptor titleImage, GisMapOutlineViewer viewer )
  {
    super( pageName, title, titleImage );
    m_gisMapOutlineViewer = viewer;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite parent )
  {
    Composite toptop = new Composite( parent, SWT.NONE );
    toptop.setLayout( new GridLayout() );
    toptop.setLayoutData( new GridData() );
    Group topGroup = new Group( toptop, SWT.NONE );
    topGroup.setLayout( new GridLayout( 2, false ) );
    topGroup.setLayoutData( new GridData() );
    m_bufferButton = new Button( topGroup, SWT.CHECK );
    m_bufferButton.setText( "Puffer:" );
    m_bufferButton.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        if( m_bufferButton.getSelection() )
        {
          m_distance.setVisible( true );
          updateMessage();
        }
        else
          m_distance.setVisible( false );
        setPageComplete( validate() );
      }
    } );
    m_distance = new Text( topGroup, SWT.SINGLE | SWT.BORDER );
    m_distance.setText( "Puffer in Meter:" );
    m_distance.setVisible( false );
    m_distance.addFocusListener( new FocusAdapter()
    {
      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusLost( FocusEvent e )
      {
        setPageComplete( validate() );
      }

    } );
    m_activeSelectionButton = new Button( topGroup, SWT.CHECK );
    m_activeSelectionButton.setText( "Active Selektion aus der Karte:" );
    m_activeSelectionButton.addSelectionListener( new SelectionAdapter()
    {

      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        final IMapModell mapModell = m_gisMapOutlineViewer.getMapModell();

        final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
        if( activeTheme instanceof IKalypsoFeatureTheme )
        {
          final Object firstElement = ((IKalypsoFeatureTheme) activeTheme).getSelectionManager().getFirstElement();
          if( firstElement instanceof Feature )
          {
            m_selectedGeom = ((Feature) firstElement).getDefaultGeometryProperty();
            m_themeName = activeTheme.getName();
          }
        }
        setPageComplete( validate() );
      }
    } );
    Label dummyLabel = new Label( topGroup, SWT.NONE );
    m_BBoxButton = new Button( topGroup, SWT.CHECK );
    m_BBoxButton.setText( "aktueller Kartenausschnitt (BBOX)" );
    m_BBoxButton.addSelectionListener( new SelectionAdapter()
    {

      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        IWorkbenchPage activePage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
        // if this Wizard is activated we assume there is always a map (GisMapEditor) open.
        IEditorPart activeEditor = activePage.getActiveEditor();
        if( activeEditor instanceof GisMapEditor )
        {
          final GisMapEditor gisMapEditor = (GisMapEditor) activeEditor;
          final MapPanel mapPanel = gisMapEditor.getMapPanel();
          try
          {
            m_BBox = GeometryFactory.createGM_Surface( mapPanel.getBoundingBox(), mapPanel.getMapModell().getCoordinatesSystem() );
          }
          catch( GM_Exception e1 )
          {
            e1.printStackTrace();
          }
          setPageComplete( validate() );
        }
      }
    } );
    Group spatialOpsGroup = new Group( toptop, SWT.NONE );
    spatialOpsGroup.setLayout( new GridLayout( 2, false ) );
    spatialOpsGroup.setLayoutData( new GridData() );
    spatialOpsGroup.setText( "Unterstütze Räumliche Operatoren" );
    Label opsLabel = new Label( spatialOpsGroup, SWT.NONE );
    opsLabel.setText( "Operatoren" );
    m_spatialOpsCombo = new Combo( spatialOpsGroup, SWT.READ_ONLY );
    m_spatialOpsCombo.setItems( new String[] { OPS_INTERSECTION, OPS_CONTAINS, OPS_TOUCHES } );
    m_spatialOpsCombo.select( 0 );
    setControl( parent );
    setPageComplete( false );

  }

  protected void updateMessage( )
  {
    String message = "";
    if( m_activeSelectionButton.getSelection() )
      message = message + "Die selektierte Geometrie vom aktiven Thema: " + m_themeName + " wird als räumlicher Operator verwendet.";
    if( m_BBoxButton.getSelection() )
      message = message + "Der aktive Kartenauschnitt wird als räumlicher Operator verwendet.";
    if( m_bufferButton.getSelection() )
      message = message + "\nMit einem Puffer von " + m_distance.getText() + " Metern";
    setMessage( message );

  }

  boolean validate( )
  {
    // the text window can only hold numbers
    if( m_distance.isVisible() && m_bufferButton.getSelection() )
    {
      try
      {
        String text = m_distance.getText();
        Double.parseDouble( text );
        Integer.parseInt( text );
      }
      catch( NumberFormatException e )
      {
        setErrorMessage( "Es können nur Zahlen in das Textfeld eingegeben werden" );
        m_distance.setText( "" );
        return false;
      }
      if( !m_BBoxButton.getSelection() && !m_activeSelectionButton.getSelection() )
      {
        setErrorMessage( "Der Puffer-Operator darf nie alleine gewählt sein" );
        return false;
      }
    }
    // the selected Object can not be null and must be of type GM_Object
    if( m_activeSelectionButton.getSelection() )
    {
      if( m_selectedGeom == null )
      {
        setErrorMessage( "Es ist kein Element selektiert" );
        return false;
      }
      else if( !(m_selectedGeom instanceof GM_Object) )
      {
        setErrorMessage( "Das selektierte Element ist keine bekannte Geometrie" );
        return false;
      }
    }
    // the bbox can not be null
    if( m_BBoxButton.getSelection() )
    {
      if( m_BBox == null )
      {
        setErrorMessage( "Die active bbox ist Null" );
        return false;
      }
    }

    // the selection bbox and active selection is not valid, all other combinations are OK.
    if( m_BBoxButton.getSelection() && m_activeSelectionButton.getSelection() )
    {
      setErrorMessage( "Es kann nur die BBOX-Option ODER eine die active Selektion-Option ausgewählt sein und nicht beides gleichzeitig" );
      return false;
    }
    updateMessage();
    setErrorMessage( null );
    return true;
  }

  Filter getFilter( IFeatureType ft )
  {
    int selectionIndex = m_spatialOpsCombo.getSelectionIndex();
    String item = m_spatialOpsCombo.getItem( selectionIndex );
    int ops = -1;
    if( item.equals( OPS_CONTAINS ) )
      ops = OperationDefines.CONTAINS;
    if( item.equals( OPS_INTERSECTION ) )
      ops = OperationDefines.INTERSECTS;
    if( item.equals( OPS_TOUCHES ) )
      ops = OperationDefines.TOUCHES;
    // TODO wählen des Poperties wenn mehrere gibt, zur Zeit wird nur das defautltGeometryProperty genommen
    final IValuePropertyType geom = ft.getDefaultGeometryProperty();
    final PropertyName propertyName = new PropertyName( geom.getQName().getLocalPart() );

    String val = m_distance.getText();
    if( val == null || val.length() == 0 )
      val = String.valueOf( 0 );

    if( m_selectedGeom != null )
    {

      final SpatialOperation operation = new SpatialOperation( ops, propertyName, m_selectedGeom, Double.parseDouble( val ) );
      return new ComplexFilter( operation );
    }
    else if( m_BBox != null )
    {
      final SpatialOperation operation = new SpatialOperation( ops, propertyName, m_BBox, Double.parseDouble( val ) );
      return new ComplexFilter( operation );
    }
    return null;
  }
}
