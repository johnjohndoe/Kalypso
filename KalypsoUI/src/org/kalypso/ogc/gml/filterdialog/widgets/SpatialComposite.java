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
package org.kalypso.ogc.gml.filterdialog.widgets;

import java.util.HashMap;
import java.util.Set;
import java.util.TreeSet;
import java.util.Map.Entry;

import javax.xml.namespace.QName;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.ogc.gml.filterdialog.dialog.IErrorMessageReciever;
import org.kalypso.ogc.gml.filterdialog.model.FeatureTypeContentProvider;
import org.kalypso.ogc.gml.filterdialog.model.FeatureTypeLabelProvider;
import org.kalypso.ogc.gml.filterdialog.model.GeometryPropertyFilter;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.filterencoding.OperationDefines;
import org.kalypsodeegree_impl.filterencoding.PropertyName;
import org.kalypsodeegree_impl.filterencoding.SpatialOperation;

class SpatialComposite extends AbstractFilterComposite
{

  Label m_spatialLabel;

  Label m_geomLable;

  Label m_supportedOpsLable;

  Combo m_supportedOpsCombo;

  // Combo m_combo;

  SpatialOperation m_operation;

  TreeSet<String> m_allsupportedSpatialOps = new TreeSet<String>();

  private String[] m_supportedOps;

  final Feature m_newGeometryOp;

  Combo m_geomOpsCombo;

  Button m_loadButton;

  Combo m_scrabLayerCombo;

  static private GM_Object m_oldGeometryOp = null;

  HashMap<String, GM_Object> m_hash = new HashMap<String, GM_Object>();

  private ComboViewer m_propViewer;

  public SpatialComposite( final Composite parent, final int style, final SpatialOperation operation, final IErrorMessageReciever errorMessageReciever, final IFeatureType ft, final Feature spatialOperators, final String[] supportedOps )
  {
    super( parent, style, errorMessageReciever, ft );
    m_operation = operation;
    m_newGeometryOp = spatialOperators;
    m_oldGeometryOp = m_operation.getGeometryLiteral();
    setSupportedOps( supportedOps );
    setControl();
  }

  private void setControl( )
  {
    if( m_oldGeometryOp == null && m_newGeometryOp == null )
    {
      m_errorMessageReciever.setErrorMessage( "Diese Operation ist nur mit einer in der Karte selektierten Geometrie möglich!" );
      return;
    }
    final String opsName = m_operation.getOperatorName();
    // Top-Group
    // possible oprations (they have been initialized when calling the factory)
    m_supportedOpsLable = new Label( this, SWT.NULL );
    // m_supportedOpsLable.setLayoutData( new GridData() );
    m_supportedOpsLable.setText( "Operation:" );
    m_supportedOpsCombo = new Combo( this, SWT.FILL | SWT.DROP_DOWN );
    // m_supportedOpsCombo.setLayout( new GridLayout() );
    final GridData data1 = new GridData( GridData.FILL_HORIZONTAL );
    data1.widthHint = STANDARD_WIDTH_FIELD;
    m_supportedOpsCombo.setLayoutData( data1 );
    String[] namesOps = null;
    if( m_supportedOps == null )
      namesOps = m_allsupportedSpatialOps.toArray( new String[m_allsupportedSpatialOps.size()] );
    else
      namesOps = m_supportedOps;
    m_supportedOpsCombo.setItems( namesOps );
    // set the selection to the current operation type, if not availabel a blank is selected by default (Combo)
    final int j = ArrayUtils.indexOf( namesOps, opsName );
    m_supportedOpsCombo.select( j );
    m_supportedOpsCombo.addSelectionListener( new SelectionAdapter()
    {

      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final String item = m_supportedOpsCombo.getItem( m_supportedOpsCombo.getSelectionIndex() );
        final int newOperationId = OperationDefines.getIdByName( item );
        m_operation.setOperatorId( newOperationId );
        refresh();
      }
    } );
    // set Geometry
    m_spatialLabel = new Label( this, SWT.NULL );
    m_spatialLabel.setText( "Geometrie:" );

    final Combo combo = new Combo( this, SWT.FILL | SWT.DROP_DOWN | SWT.READ_ONLY );
    final GridData data = new GridData( GridData.FILL_HORIZONTAL );
    data.widthHint = STANDARD_WIDTH_FIELD;
    combo.setLayoutData( data );
    m_propViewer = new ComboViewer( combo );
    m_propViewer.setContentProvider( new FeatureTypeContentProvider() );
    m_propViewer.setLabelProvider( new FeatureTypeLabelProvider() );
    m_propViewer.addFilter( new GeometryPropertyFilter() );
    m_propViewer.add( m_ft.getProperties() );
    m_propViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {

      public void selectionChanged( final SelectionChangedEvent event )
      {
        final Object firstElement = ((IStructuredSelection) event.getSelection()).getFirstElement();
        if( firstElement instanceof IValuePropertyType )
        {
          final QName propName = ((IValuePropertyType) firstElement).getQName();
          m_operation.setProperty( new PropertyName( propName ) );
          // fireModellEvent( new ModellEvent( SpatialComposite.this, ModellEvent.WIDGET_CHANGE ) );
          // updateOperation( null );

        }

      }
    } );
    m_propViewer.setSelection( new StructuredSelection( setPropertySelection( m_operation.getPropertyName() ) ) );
    // get
    m_geomLable = new Label( this, SWT.NONE );
    // m_geomLable.setLayoutData( new GridData() );
    m_geomLable.setText( "Operator:" );
    m_geomOpsCombo = new Combo( this, SWT.FILL | SWT.DROP_DOWN | SWT.READ_ONLY );
    m_geomOpsCombo.setLayoutData( new GridData() );
    if( m_oldGeometryOp != null )
    {
      final String id = "original_FID";
      m_geomOpsCombo.setItems( new String[] { id } );
      m_hash.put( id, m_oldGeometryOp );
      m_geomOpsCombo.select( 0 );
    }
    if( m_newGeometryOp != null )
    {
      String id = m_newGeometryOp.getId();
      if( id == null )
        id = "selektion_FID";
      final GM_Object[] geometryProperties = m_newGeometryOp.getGeometryProperties();
      for( int i = 0; i < geometryProperties.length; i++ )
      {
        final GM_Object geom = geometryProperties[i];
        m_geomOpsCombo.setItems( new String[] { id + i } );
        m_hash.put( id + i, geom );
      }
    }
    m_geomOpsCombo.addSelectionListener( new SelectionAdapter()
    {

      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {

        final int selectionIndex = m_geomOpsCombo.getSelectionIndex();
        final String item = m_geomOpsCombo.getItem( selectionIndex );
        final GM_Object geom = m_hash.get( item );
        updateOperation( geom );
      }
    } );
    m_geomOpsCombo.addFocusListener( new FocusAdapter()
    {

      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusLost( final FocusEvent e )
      {
        final int selectionIndex = m_geomOpsCombo.getSelectionIndex();
        final String item = m_geomOpsCombo.getItem( selectionIndex );
        final GM_Object geom = m_hash.get( item );
        updateOperation( geom );

      }

      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusGained(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusGained( final FocusEvent e )
      {
        focusLost( e );
      }
    } );

    m_geomOpsCombo.select( setGeomOperator() );
    // pack();
  }

  private int setGeomOperator( )
  {
    String key = null;
    final GM_Object geometryLiteral = m_operation.getGeometryLiteral();
    if( m_hash.containsValue( geometryLiteral ) )
    {
      final Set<Entry<String, GM_Object>> mapEntries = m_hash.entrySet();
      for( final Object element2 : mapEntries )
      {
        final Entry element = (Entry) element2;
        if( element.getValue().equals( geometryLiteral ) )
          key = (String) element.getKey();
      }
    }
    final String[] items = m_geomOpsCombo.getItems();
    final int index = ArrayUtils.indexOf( items, key );
    if( index == -1 )
      return 0;
    return index;
  }

  private void setSupportedOps( final String[] supportedOps )
  {
    if( supportedOps != null && supportedOps.length > 0 )
      m_supportedOps = supportedOps;
    else
    {
      m_allsupportedSpatialOps.add( OperationDefines.getNameById( OperationDefines.BBOX ) );
      m_allsupportedSpatialOps.add( OperationDefines.getNameById( OperationDefines.BEYOND ) );
      m_allsupportedSpatialOps.add( OperationDefines.getNameById( OperationDefines.CONTAINS ) );
      m_allsupportedSpatialOps.add( OperationDefines.getNameById( OperationDefines.CROSSES ) );
      m_allsupportedSpatialOps.add( OperationDefines.getNameById( OperationDefines.DISJOINT ) );
      m_allsupportedSpatialOps.add( OperationDefines.getNameById( OperationDefines.DWITHIN ) );
      m_allsupportedSpatialOps.add( OperationDefines.getNameById( OperationDefines.EQUALS ) );
      m_allsupportedSpatialOps.add( OperationDefines.getNameById( OperationDefines.INTERSECTS ) );
      m_allsupportedSpatialOps.add( OperationDefines.getNameById( OperationDefines.OVERLAPS ) );
      m_allsupportedSpatialOps.add( OperationDefines.getNameById( OperationDefines.TOUCHES ) );
      m_allsupportedSpatialOps.add( OperationDefines.getNameById( OperationDefines.WITHIN ) );
    }

  }

  boolean updateOperation( final GM_Object newGeometry )
  {
    try
    {
      if( newGeometry == null )
        return false;
      m_operation.setGeometry( newGeometry );
      final PropertyName propertyName = m_operation.getPropertyName();
      final IStructuredSelection selection = (IStructuredSelection) m_propViewer.getSelection();
      propertyName.setValue( ((IPropertyType) selection.getFirstElement()).getQName() );
      // PropertyName newPropertyName = new PropertyName( m_propViewer.getSelection() );
      // m_operation.setProperty( newPropertyName );
    }
    catch( final Exception e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      ErrorDialog.openError( getShell(), "Fehler beim erstellen des Geometrie-Operators", e.getMessage(), status );
      return false;
    }
    return true;
  }
}