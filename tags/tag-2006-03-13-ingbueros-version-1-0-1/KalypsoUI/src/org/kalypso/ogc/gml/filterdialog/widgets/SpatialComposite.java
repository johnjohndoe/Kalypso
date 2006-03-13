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
import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;
import java.util.Map.Entry;

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
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.ogc.gml.filterdialog.dialog.IErrorMessageReciever;
import org.kalypso.ogc.gml.filterdialog.model.FeatureTypeContentProvider;
import org.kalypso.ogc.gml.filterdialog.model.FeatureTypeLabelProvider;
import org.kalypso.ogc.gml.filterdialog.model.GeometryPropertyFilter;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.gml.GMLGeometry;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.filterencoding.OperationDefines;
import org.kalypsodeegree_impl.filterencoding.PropertyName;
import org.kalypsodeegree_impl.filterencoding.SpatialOperation;
import org.kalypsodeegree_impl.gml.GMLFactory;

class SpatialComposite extends AbstractFilterComposite
{

  // Text m_text;

  Label m_spatialLabel;

  Label m_geomLable;

  Label m_supportedOpsLable;

  Combo m_supportedOpsCombo;

  Combo m_combo;

  SpatialOperation m_operation;

  TreeSet<String> m_allsupportedSpatialOps = new TreeSet<String>();

  private TreeSet<String> m_supportedOps;

  final Feature m_newGeometryOp;

  Combo m_geomOpsCombo;

  Button m_loadButton;

  Combo m_scrabLayerCombo;

  static private GM_Object m_oldGeometryOp = null;

  HashMap<String, GM_Object> m_hash = new HashMap<String, GM_Object>();

  public SpatialComposite( final Composite parent, final int style, final SpatialOperation operation, final IErrorMessageReciever errorMessageReciever, final IFeatureType ft, Feature spatialOperators )
  {
    super( parent, style, errorMessageReciever, ft );
    m_operation = operation;
    m_newGeometryOp = spatialOperators;
    try
    {
      m_oldGeometryOp = m_operation.getGeometryLiteral();
    }
    catch( FilterEvaluationException e )
    {
      e.printStackTrace();
    }
    setSupportedOps( new TreeSet<String>() );
    setControl();
  }

  private void setControl( )
  {
    if( m_oldGeometryOp == null && m_newGeometryOp == null )
    {
      m_errorMessageReciever.setErrorMessage( "Diese Operation ist nur mit einer in der Karte selektierten Geometrie möglich!" );
      return;
    }
    String opsName = m_operation.getOperatorName();
    // Top-Group
    // possible oprations (they have been initialized when calling the factory)
    m_supportedOpsLable = new Label( this, SWT.NULL );
    m_supportedOpsLable.setText( "Operation:" );
    m_supportedOpsCombo = new Combo( this, SWT.FILL | SWT.DROP_DOWN );
    m_supportedOpsCombo.setLayout( new GridLayout() );
    GridData data1 = new GridData( GridData.FILL_HORIZONTAL );
    data1.widthHint = STANDARD_WIDTH_FIELD;
    m_supportedOpsCombo.setLayoutData( data1 );
    String[] namesOps = null;
    if( m_supportedOps == null )
      namesOps = m_allsupportedSpatialOps.toArray( new String[m_allsupportedSpatialOps.size()] );
    else
      namesOps = m_supportedOps.toArray( new String[m_supportedOps.size()] );
    m_supportedOpsCombo.setItems( namesOps );
    // set the selection to the current operation type, if not availabel a blank is selected by default (Combo)
    int j = ArrayUtils.indexOf( namesOps, opsName );
    m_supportedOpsCombo.select( j );
    m_supportedOpsCombo.addSelectionListener( new SelectionAdapter()
    {

      @Override
      public void widgetSelected( SelectionEvent e )
      {
        String item = m_supportedOpsCombo.getItem( m_supportedOpsCombo.getSelectionIndex() );
        int newOperationId = OperationDefines.getIdByName( item );
        m_operation.setOperatorId( newOperationId );
        fireModellEvent( new ModellEvent( SpatialComposite.this, ModellEvent.WIDGET_CHANGE ) );
      }
    } );
    // set Geometry
    GridData data = new GridData( GridData.FILL_HORIZONTAL );
    data.widthHint = STANDARD_WIDTH_FIELD;
    m_spatialLabel = new Label( this, SWT.NULL );
    m_spatialLabel.setText( "Geometrie:" );
    m_combo = new Combo( this, SWT.FILL | SWT.DROP_DOWN | SWT.READ_ONLY );
    m_combo.setLayoutData( data );
    ComboViewer propViewer = new ComboViewer( m_combo );
    propViewer.setContentProvider( new FeatureTypeContentProvider() );
    propViewer.setLabelProvider( new FeatureTypeLabelProvider() );
    propViewer.addFilter( new GeometryPropertyFilter() );
    propViewer.add( m_ft.getProperties() );
    propViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {

      public void selectionChanged( SelectionChangedEvent event )
      {
        Object firstElement = ((IStructuredSelection) event.getSelection()).getFirstElement();
        if( firstElement instanceof IValuePropertyType )
        {
          String propName = ((IValuePropertyType) firstElement).getQName().getLocalPart();
          m_operation.setProperty( new PropertyName( propName ) );
          // fireModellEvent( new ModellEvent( SpatialComposite.this, ModellEvent.WIDGET_CHANGE ) );
          // updateOperation( null );

        }

      }
    } );
    propViewer.setSelection( new StructuredSelection( setPropertySelection( m_operation.getPropertyName() ) ) );
    // get
    m_geomLable = new Label( this, SWT.NONE );
    m_geomLable.setText( "Operator:" );
    m_geomOpsCombo = new Combo( this, SWT.FILL | SWT.DROP_DOWN | SWT.READ_ONLY );
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
        GM_Object geom = geometryProperties[i];
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
      public void widgetSelected( SelectionEvent e )
      {

        int selectionIndex = m_geomOpsCombo.getSelectionIndex();
        String item = m_geomOpsCombo.getItem( selectionIndex );
        GM_Object geom = m_hash.get( item );
        updateOperation( geom );
      }
    } );
    m_geomOpsCombo.addFocusListener( new FocusAdapter()
    {

      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusLost( FocusEvent e )
      {
        int selectionIndex = m_geomOpsCombo.getSelectionIndex();
        String item = m_geomOpsCombo.getItem( selectionIndex );
        GM_Object geom = m_hash.get( item );
        updateOperation( geom );

      }

      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusGained(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusGained( FocusEvent e )
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
    try
    {
      GM_Object geometryLiteral = m_operation.getGeometryLiteral();
      if( m_hash.containsValue( geometryLiteral ) )
      {
        Set<Entry<String, GM_Object>> mapEntries = m_hash.entrySet();
        for( Iterator iter = mapEntries.iterator(); iter.hasNext(); )
        {
          Entry element = (Entry) iter.next();
          if( element.getValue().equals( geometryLiteral ) )
            key = (String) element.getKey();
        }
      }
    }
    catch( FilterEvaluationException e )
    {
      e.printStackTrace();
    }
    String[] items = m_geomOpsCombo.getItems();
    int index = ArrayUtils.indexOf( items, key );
    if( index == -1 )
      return 0;
    return index;
  }

  private void setSupportedOps( TreeSet<String> supportedOps )
  {
    if( supportedOps != null && !supportedOps.isEmpty() )
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

  boolean updateOperation( GM_Object newGeometry )
  {
    GMLGeometry gml = null;
    try
    {
      if( newGeometry == null )
        return false;
      gml = GMLFactory.createGMLGeometry( null, newGeometry );
      m_operation.setGeometry( gml );

      PropertyName newPropertyName = new PropertyName( m_combo.getItem( m_combo.getSelectionIndex() ) );
      m_operation.setProperty( newPropertyName );
    }
    catch( Exception e )
    {
      IStatus status = StatusUtilities.statusFromThrowable( e );
      ErrorDialog.openError( getShell(), "Fehler beim erstellen des Geometrie-Operators", e.getMessage(), status );
      return false;
    }
    return true;
  }
}