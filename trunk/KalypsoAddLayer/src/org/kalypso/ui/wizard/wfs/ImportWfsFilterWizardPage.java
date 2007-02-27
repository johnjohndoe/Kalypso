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

import java.util.ArrayList;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
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
import org.kalypso.ogc.gml.filterdialog.model.FeaturePropertyContentProvider;
import org.kalypso.ogc.gml.filterdialog.model.FeaturePropertyLabelProvider;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.outline.GisMapOutlineViewer;
import org.kalypso.ogc.wfs.IWFSLayer;
import org.kalypso.ui.editor.mapeditor.GisMapEditor;
import org.kalypsodeegree.filterencoding.Filter;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.filterencoding.ComplexFilter;
import org.kalypsodeegree_impl.filterencoding.OperationDefines;
import org.kalypsodeegree_impl.filterencoding.PropertyName;
import org.kalypsodeegree_impl.filterencoding.SpatialOperation;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

import com.vividsolutions.jts.geom.Geometry;

/**
 * @author kuepfer
 */
public class ImportWfsFilterWizardPage extends WizardPage
{

  Text m_bufferDistance;

  Button m_bufferButton;

  GM_Object m_selectedGeom;

  Button m_activeSelectionButton;

  IMapModell m_gisMapOutlineViewer;

  // GM_Surface m_BBox;

  private Button m_BBoxButton;

  private Combo m_spatialOpsCombo;

  String OPS_INTERSECTION = "Schneidet das Objekt";

  String OPS_CONTAINS = "Enthält das Objekt";

  String OPS_TOUCHES = "Berührt das Objekt";

  String m_themeName;

  boolean m_doFilterMaxFeature = false;

  String m_maxFeaturesAsString = "500";

  int m_maxFeaturesAsInt = 500;

  ComboViewer m_geomComboViewer;

  public ImportWfsFilterWizardPage( String pageName, String title, ImageDescriptor titleImage, IMapModell viewer )
  {
    super( pageName, title, titleImage );
    m_gisMapOutlineViewer = viewer;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite parent )
  {
    final Composite main = new Composite( parent, SWT.NONE );
    main.setLayout( new GridLayout( 2, true ) );
    main.setLayoutData( new GridData() );
    final Group topGroup = new Group( main, SWT.NONE );
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
          m_bufferDistance.setEditable( true );
          updateMessage();
        }
        else
          m_bufferDistance.setEditable( false );
        setPageComplete( validate() );
      }
    } );
    m_bufferDistance = new Text( topGroup, SWT.SINGLE | SWT.BORDER );
    m_bufferDistance.setText( "in Meter" );
    m_bufferDistance.setEditable( false );
    m_bufferDistance.addFocusListener( new FocusAdapter()
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
    m_activeSelectionButton = new Button( topGroup, SWT.RADIO );
    m_activeSelectionButton.setText( "Active Selektion aus der Karte:" );
    m_activeSelectionButton.addSelectionListener( new SelectionAdapter()
    {

      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        if( m_activeSelectionButton.getSelection() )
          m_geomComboViewer.getCombo().setEnabled( true );
        else
          m_geomComboViewer.getCombo().setEnabled( false );
        setPageComplete( validate() );
      }
    } );
    Combo geomCombo = new Combo( topGroup, SWT.FILL | SWT.DROP_DOWN | SWT.READ_ONLY );
    geomCombo.setEnabled( false );
    GridData data = new GridData( GridData.FILL_HORIZONTAL );
    // data.widthHint = STANDARD_WIDTH_FIELD;
    geomCombo.setLayoutData( data );
    m_geomComboViewer = new ComboViewer( geomCombo );
    m_geomComboViewer.setLabelProvider( new FeaturePropertyLabelProvider() );
    m_geomComboViewer.setContentProvider( new FeaturePropertyContentProvider() );
    m_geomComboViewer.addFilter( new GeometryPropertyFilter() );
    m_geomComboViewer.add( getSelectedFeatureProperties() );
    m_geomComboViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {

      public void selectionChanged( SelectionChangedEvent event )
      {
        ISelection selection = event.getSelection();
        if( selection instanceof IStructuredSelection )
        {
          IStructuredSelection ss = (IStructuredSelection) selection;
          m_selectedGeom = (GM_Object) ss.getFirstElement();
        }
        setPageComplete( validate() );
      }
    } );
    m_BBoxButton = new Button( topGroup, SWT.RADIO );
    m_BBoxButton.setText( "aktueller Kartenausschnitt (BBOX)" );
    m_BBoxButton.addSelectionListener( new SelectionAdapter()
    {

      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        getBBoxFromActiveMap();
        setPageComplete( validate() );
      }
    } );
    final GridData data1 = new GridData();
    data1.horizontalSpan = 2;
    m_BBoxButton.setLayoutData( data1 );

    final Button button = new Button( topGroup, SWT.CHECK );
    button.setText( "max. Feature" );
    button.setSelection( m_doFilterMaxFeature );

    final Text maxFeatureField = new Text( topGroup, SWT.BORDER );
    maxFeatureField.setText( m_maxFeaturesAsString );
    final GridData data2 = new GridData( GridData.FILL_HORIZONTAL );
    data2.grabExcessHorizontalSpace = true;
    maxFeatureField.setLayoutData( data2 );
    maxFeatureField.addFocusListener( new FocusAdapter()
    {

      @Override
      public void focusLost( FocusEvent e )
      {
        m_maxFeaturesAsString = maxFeatureField.getText();
        setPageComplete( validate() );
      }

    } );

    button.addSelectionListener( new SelectionAdapter()
    {

      @Override
      public void widgetSelected( SelectionEvent e )
      {
        m_doFilterMaxFeature = !m_doFilterMaxFeature;
        maxFeatureField.setEnabled( m_doFilterMaxFeature );
      }
    } );

    Group spatialOpsGroup = new Group( main, SWT.HORIZONTAL );
    spatialOpsGroup.setLayout( new GridLayout( 2, false ) );
    spatialOpsGroup.setLayoutData( new GridData() );
    spatialOpsGroup.setText( "Unterstütze Räumliche Operatoren" );
    Label opsLabel = new Label( spatialOpsGroup, SWT.NONE );
    opsLabel.setText( "Operatoren" );
    m_spatialOpsCombo = new Combo( spatialOpsGroup, SWT.READ_ONLY );
    m_spatialOpsCombo.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        setPageComplete( validate() );
      }
    } );
    m_spatialOpsCombo.setItems( new String[] { OPS_INTERSECTION, OPS_CONTAINS, OPS_TOUCHES } );
    m_spatialOpsCombo.select( 0 );
    setControl( main );
    setPageComplete( validate() );

  }

  protected void updateMessage( )
  {
    String message = "";
    if( m_activeSelectionButton.getSelection() )
      message = message + "Die selektierte Geometrie vom aktiven Thema: " + m_themeName + " wird als räumlicher Operator verwendet.";
    if( m_BBoxButton.getSelection() )
      message = message + "Der aktive Kartenauschnitt wird als räumlicher Operator verwendet.";
    if( m_bufferButton.getSelection() )
      message = message + "\nMit einem Puffer von " + m_bufferDistance.getText() + " Metern";
    setMessage( message );

  }

  boolean validate( )
  {
    // validate maxFeatures
    if( m_doFilterMaxFeature )
    {
      try
      {
        m_maxFeaturesAsInt = Integer.parseInt( m_maxFeaturesAsString );
        if( m_maxFeaturesAsInt < 1 )
        {
          setErrorMessage( "maximale Anzahl Feature muss größer 0 sein" );
          return false;
        }
      }
      catch( Exception ex )
      {
        setErrorMessage( "maximale Anzahl Feature muss ein Zahlenwert sein" );
        return false;
      }
    }
    // the text window can only hold numbers
    if( m_bufferDistance.isVisible() && m_bufferButton.getSelection() )
    {
      try
      {
        String text = m_bufferDistance.getText();
        Double.parseDouble( text );
        // Integer.parseInt( text );
      }
      catch( NumberFormatException e )
      {
        setErrorMessage( "Es können nur Zahlen in das Textfeld eingegeben werden" );
        m_bufferDistance.setText( "" );
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
    }
    // the bbox can not be null
    if( m_BBoxButton.getSelection() )
    {
      if( getBBoxFromActiveMap() == null )
      {
        setErrorMessage( "Die active bbox ist Null" );
        return false;
      }
    }

    // // the selection bbox and active selection is not valid, all other combinations are OK.
    // if( m_BBoxButton.getSelection() && m_activeSelectionButton.getSelection() )
    // {
    // // TODO use radio buttons (@christoph)
    // setErrorMessage( "Es kann nur die BBOX-Option ODER eine die active Selektion-Option ausgewählt sein und nicht
    // beides gleichzeitig" );
    // return false;
    // }
    updateMessage();
    setErrorMessage( null );
    return true;
  }

  Filter getFilter( final IWFSLayer layer ) throws GM_Exception
  {
    // TODO check checkboxes, maybe no filter at all is wanted
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
    // dies muss mit einer TreeView gelöst werden -> Label und ContentProvider anpassen 
    final IFeatureType ft = layer.getFeatureType();
    if( ft == null )
      return null;
    final IValuePropertyType geom = ft.getDefaultGeometryProperty();
    if( geom == null )
      return null;
    final PropertyName propertyName = new PropertyName( geom.getQName() );
    // check if buffer is selected
    final String val;
    if( !m_bufferButton.getSelection() )
      val = "0.0";
    else
      val = m_bufferDistance.getText();
    final double distance = Double.parseDouble( val );

    if( m_selectedGeom != null )
    {
      final Geometry jtsGeom = JTSAdapter.export( m_selectedGeom );
      final Geometry jtsBufferedGeom = jtsGeom.buffer( distance );
      final SpatialOperation operation = new SpatialOperation( ops, propertyName, JTSAdapter.wrap( jtsBufferedGeom ), distance );
      return new ComplexFilter( operation );
    }
    else if( m_BBoxButton.getSelection() )
    {

      final Geometry jtsGeom = JTSAdapter.export( getBBoxFromActiveMap() );
      final Geometry jtsBufferedGeom = jtsGeom.buffer( distance );
      final SpatialOperation operation = new SpatialOperation( ops, propertyName, JTSAdapter.wrap( jtsBufferedGeom ), distance );
      return new ComplexFilter( operation );
    }
    return null;
  }

  public boolean doFilterMaxFeatures( )
  {
    return m_doFilterMaxFeature;
  }

  public int getMaxFeatures( )
  {
    return m_maxFeaturesAsInt;
  }

  private Object[] getSelectedFeatureProperties( )
  {
    m_themeName = "Thema ohne Namen";
    if( m_gisMapOutlineViewer == null )
      return new Object[0];
    final IMapModell mapModell = m_gisMapOutlineViewer;

    final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
    if( activeTheme instanceof IKalypsoFeatureTheme )
    {
      final Object firstElement = ((IKalypsoFeatureTheme) activeTheme).getSelectionManager().getFirstElement();
      m_themeName = activeTheme.getName();
      if( firstElement instanceof Feature && firstElement != null )
      {
        final Feature feature = (Feature) firstElement;
        final Object[] properties = feature.getProperties();
        final ArrayList<Object> list = new ArrayList<Object>();
        for( Object prop : properties )
        {
          if( prop != null )
            list.add( prop );
        }
        return list.toArray();
      }
    }
    return new Object[0];
  }

  GM_Surface getBBoxFromActiveMap( )
  {
    IWorkbenchPage activePage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
    // if this Wizard is activated we assume there is always a map (GisMapEditor) open.
    IEditorPart activeEditor = activePage.getActiveEditor();
    if( activeEditor instanceof GisMapEditor )
    {
      final GisMapEditor gisMapEditor = (GisMapEditor) activeEditor;
      final MapPanel mapPanel = gisMapEditor.getMapPanel();
      GM_Envelope boundingBox = mapPanel.getBoundingBox();
      if( boundingBox != null )
        try
        {
          return GeometryFactory.createGM_Surface( boundingBox, mapPanel.getMapModell().getCoordinatesSystem() );
        }
        catch( GM_Exception ex )
        {
          ex.printStackTrace();
          setPageComplete( validate() );
        }
    }
    return null;
  }

  class GeometryPropertyFilter extends ViewerFilter
  {

    /**
     * @see org.eclipse.jface.viewers.ViewerFilter#select(org.eclipse.jface.viewers.Viewer, java.lang.Object,
     *      java.lang.Object)
     */
    @Override
    public boolean select( Viewer viewer, Object parentElement, Object element )
    {
      return GeometryUtilities.isGeometry( element );
    }

  }
}
