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
package org.kalypso.flows;

import java.util.ArrayList;
import java.util.HashMap;

import javax.xml.namespace.QName;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
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
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.filterdialog.model.FeaturePropertyContentProvider;
import org.kalypso.ogc.gml.filterdialog.model.FeaturePropertyLabelProvider;
import org.kalypso.ogc.gml.filterdialog.model.GeometryPropertyFilter;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.filterencoding.Filter;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.filterencoding.ComplexFilter;
import org.kalypsodeegree_impl.filterencoding.PropertyName;
import org.kalypsodeegree_impl.filterencoding.SpatialOperation;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;

/**
 * @author kuepfer
 */
public class SimpleFilterChooserDialog extends TitleAreaDialog
{

  private Composite m_top;

  GM_Object m_selectedGeom;

  Button m_activeSelectionButton;

  private final QName[] m_propertyName;

  String m_maxFeaturesAsString = "500";

  ComboViewer m_geomComboViewer;

  private final MapPanel m_mapPanel;

  private String m_themeName;

  boolean m_doFilterMaxFeature = false;

  private int m_maxFeaturesAsInt;

  private final int m_spatialOpsType;

  protected boolean m_bufferSelection;

  protected boolean m_bboxSelection;

  protected String m_bufferString;

  Text m_bufferDistance;

  private final IKalypsoFeatureTheme[] m_kfThemes;

  /**
   * @param parent
   *            the parent shell to host the dialog
   * @param kfThemes
   *            the themes to generate a filter for
   * @param propertyNames
   *            the list of geometry properties to apply the filter to
   * @param mapPanel
   *            the active map panel from the GisMapEditor
   * @param spatialOperationType
   *            the spatial operation as int from the OperationsDefines
   * @see org.kalypsodeegree_impl.filterencoding.OperationDefines
   */
  public SimpleFilterChooserDialog( final Shell parent, final IKalypsoFeatureTheme[] kfThemes, final QName[] propertyNames, final MapPanel mapPanel, final int spatialOperationType )
  {
    super( parent );
    m_kfThemes = kfThemes;
    m_propertyName = propertyNames;
    m_mapPanel = mapPanel;
    m_spatialOpsType = spatialOperationType;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createDialogArea( final Composite parent )
  {
    final Composite main = (Composite) super.createDialogArea( parent );
    m_top = new Composite( main, SWT.NONE );
    final GridLayout gridLayout = new GridLayout( 2, true );
    m_top.setLayout( gridLayout );
    final GridData data2 = new GridData( GridData.FILL_BOTH );
    data2.grabExcessHorizontalSpace = true;
    data2.grabExcessVerticalSpace = true;
    m_top.setLayoutData( data2 );

    final Group topGroup = new Group( main, SWT.NONE );
    topGroup.setLayout( new GridLayout( 2, false ) );
    topGroup.setLayoutData( new GridData() );
    final Button bufferButton = new Button( topGroup, SWT.CHECK );
    bufferButton.setText( "Puffer:" );
    bufferButton.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        m_bufferSelection = bufferButton.getSelection();
        if( m_bufferSelection )
          m_bufferDistance.setEditable( true );
        else
          m_bufferDistance.setEditable( false );
        m_bufferString = m_bufferDistance.getText();
        setPageComplete( validate() );
        updateMessage();
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
      public void focusLost( final FocusEvent e )
      {
        m_bufferString = m_bufferDistance.getText();
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
      public void widgetSelected( final SelectionEvent e )
      {
        if( m_activeSelectionButton.getSelection() )
          m_geomComboViewer.getCombo().setEnabled( true );
        else
          m_geomComboViewer.getCombo().setEnabled( false );
        setPageComplete( validate() );
      }
    } );
    final Combo geomCombo = new Combo( topGroup, SWT.FILL | SWT.DROP_DOWN | SWT.READ_ONLY );
    geomCombo.setEnabled( false );
    final GridData data = new GridData( GridData.FILL_HORIZONTAL );
    // data.widthHint = STANDARD_WIDTH_FIELD;
    geomCombo.setLayoutData( data );
    m_geomComboViewer = new ComboViewer( geomCombo );
    m_geomComboViewer.setLabelProvider( new FeaturePropertyLabelProvider() );
    m_geomComboViewer.setContentProvider( new FeaturePropertyContentProvider() );
    m_geomComboViewer.addFilter( new GeometryPropertyFilter() );
    m_geomComboViewer.add( getSelectedFeatureProperties() );
    m_geomComboViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {

      public void selectionChanged( final SelectionChangedEvent event )
      {
        final ISelection selection = event.getSelection();
        if( selection instanceof IStructuredSelection )
        {
          final IStructuredSelection ss = (IStructuredSelection) selection;
          m_selectedGeom = (GM_Object) ss.getFirstElement();
        }
        setPageComplete( validate() );
      }
    } );
    final Button bboxButton = new Button( topGroup, SWT.CHECK );
    bboxButton.setText( "aktueller Kartenausschnitt (BBOX)" );
    bboxButton.addSelectionListener( new SelectionAdapter()
    {

      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        m_bboxSelection = bboxButton.getSelection();
        getBBoxFromActiveMap();
        setPageComplete( validate() );
      }
    } );
    final GridData data1 = new GridData();
    data1.horizontalSpan = 2;
    bboxButton.setLayoutData( data1 );

    final Button button = new Button( topGroup, SWT.CHECK );
    button.setText( "max. Feature" );
    button.setSelection( m_doFilterMaxFeature );

    final Text maxFeatureField = new Text( topGroup, SWT.BORDER );
    maxFeatureField.setText( m_maxFeaturesAsString );
    final GridData data3 = new GridData( GridData.FILL_HORIZONTAL );
    data3.grabExcessHorizontalSpace = true;
    maxFeatureField.setLayoutData( data2 );
    maxFeatureField.addFocusListener( new FocusAdapter()
    {

      @Override
      public void focusLost( final FocusEvent e )
      {
        m_maxFeaturesAsString = maxFeatureField.getText();
        setPageComplete( validate() );
      }

    } );

    button.addSelectionListener( new SelectionAdapter()
    {

      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        m_doFilterMaxFeature = !m_doFilterMaxFeature;
        maxFeatureField.setEnabled( m_doFilterMaxFeature );
      }
    } );

    return main;
  }

  private Object[] getSelectedFeatureProperties( )
  {
    final IMapModell mapModell = m_mapPanel.getMapModell();

    final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
    if( activeTheme instanceof IKalypsoFeatureTheme )
    {
      final Object firstElement = ((IKalypsoFeatureTheme) activeTheme).getSelectionManager().getFirstElement();
      m_themeName = activeTheme.getLabel();
      if( firstElement instanceof Feature && firstElement != null )
      {
        final Feature feature = (Feature) firstElement;
        final Object[] properties = feature.getProperties();
        final ArrayList<Object> list = new ArrayList<Object>();
        for( final Object prop : properties )
        {
          if( prop != null )
            list.add( prop );
        }
        return list.toArray();
      }
    }
    return new Object[0];
  }

  protected void updateMessage( )
  {
    String message = "";
    if( m_activeSelectionButton.getSelection() )
      message = message + "Die selektierte Geometrie vom aktiven Thema: " + m_themeName + " wird als r‰umlicher Operator verwendet.";
    if( m_bboxSelection )
      message = message + "Der aktive Kartenauschnitt wird als r‰umlicher Operator verwendet.";
    if( m_bufferSelection )
      message = message + "\nMit einem Puffer von " + m_bufferString + " Metern";
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
          setErrorMessage( "maximale Anzahl Feature muss grˆﬂer 0 sein" );
          return false;
        }
      }
      catch( final Exception ex )
      {
        setErrorMessage( "maximale Anzahl Feature muss ein Zahlenwert sein" );
        return false;
      }
    }
    // the text window can only hold numbers
    if( m_bufferDistance.isVisible() && m_bufferSelection )
    {
      try
      {
        Double.parseDouble( m_bufferString );
        // Integer.parseInt( text );
      }
      catch( final NumberFormatException e )
      {
        setErrorMessage( "Es kˆnnen nur Zahlen in das Textfeld eingegeben werden" );
        m_bufferDistance.setText( "" );
        return false;
      }
      if( !m_bboxSelection && !m_activeSelectionButton.getSelection() )
      {
        setErrorMessage( "Der Puffer-Operator darf nie alleine gew‰hlt sein" );
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
    if( m_bboxSelection )
    {
      if( getBBoxFromActiveMap() == null )
      {
        setErrorMessage( "Die active bbox ist Null" );
        return false;
      }
    }

    // the selection bbox and active selection is not valid, all other combinations are OK.
    if( m_bboxSelection && m_activeSelectionButton.getSelection() )
    {
      setErrorMessage( "Es kann nur die BBOX-Option ODER eine die active Selektion-Option ausgew‰hlt sein und nicht beides gleichzeitig" );
      return false;
    }
    updateMessage();
    setErrorMessage( null );
    return true;
  }

  public HashMap<IKalypsoFeatureTheme, Filter> getFilter( ) throws GM_Exception
  {
    final HashMap<IKalypsoFeatureTheme, Filter> hashMap = new HashMap<IKalypsoFeatureTheme, Filter>();

    for( int i = 0; i < m_kfThemes.length; i++ )
    {
      final IKalypsoFeatureTheme kft = m_kfThemes[i];
      final QName qName = m_propertyName[i];

      final IPropertyType property = kft.getFeatureType().getProperty( qName );
      if( property == null )
        throw new GM_Exception( "No matching geometry proptery type found." );

      final PropertyName propertyName = new PropertyName( qName );
      // check if buffer is selected
      final String val;
      if( !m_bufferSelection )
        val = "0.0";
      else
        val = m_bufferString;
      final double distance = Double.parseDouble( val );

      if( m_selectedGeom != null )
      {
        final Geometry jtsGeom = JTSAdapter.export( m_selectedGeom );
        final Geometry jtsBufferedGeom = jtsGeom.buffer( distance );
        final SpatialOperation operation = new SpatialOperation( m_spatialOpsType, propertyName, JTSAdapter.wrap( jtsBufferedGeom ), distance );
        hashMap.put( kft, new ComplexFilter( operation ) );
      }
      else if( m_bboxSelection )
      {

        final Geometry jtsGeom = JTSAdapter.export( getBBoxFromActiveMap() );
        final Geometry jtsBufferedGeom = jtsGeom.buffer( distance );
        final SpatialOperation operation = new SpatialOperation( m_spatialOpsType, propertyName, JTSAdapter.wrap( jtsBufferedGeom ), distance );
        hashMap.put( kft, new ComplexFilter( operation ) );
      }

    }
    return hashMap;
  }

  public void setPageComplete( final boolean complete )
  {
    final Button okButton = getButton( IDialogConstants.OK_ID );
    okButton.setEnabled( complete );
  }

  GM_Object getBBoxFromActiveMap( )
  {
    try
    {
      return GeometryFactory.createGM_Surface( m_mapPanel.getBoundingBox(), m_mapPanel.getMapModell().getCoordinatesSystem() );
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }
}
