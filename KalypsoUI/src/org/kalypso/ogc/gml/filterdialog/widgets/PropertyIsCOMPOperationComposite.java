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
package org.kalypso.ogc.gml.filterdialog.widgets;

import java.util.TreeSet;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.ogc.gml.filterdialog.dialog.IErrorMessageReciever;
import org.kalypso.ogc.gml.filterdialog.model.FeatureTypeContentProvider;
import org.kalypso.ogc.gml.filterdialog.model.FeatureTypeLabelProvider;
import org.kalypso.ogc.gml.filterdialog.model.NonGeometryPropertyFilter;
import org.kalypsodeegree.filterencoding.Expression;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree_impl.filterencoding.ArithmeticExpression;
import org.kalypsodeegree_impl.filterencoding.Literal;
import org.kalypsodeegree_impl.filterencoding.OperationDefines;
import org.kalypsodeegree_impl.filterencoding.PropertyIsCOMPOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyName;

class PropertyIsCOMPOperationComposite extends AbstractFilterComposite
{

  Label m_firstRowLabel = null;

  // Combo m_firstRowCombo = null;

  Label m_secondRowLabel = null;

  Text m_secondRowText = null;

  Text m_errorMessage = null;

  Label m_errorLabel = null;

  Combo m_supportedOpsCombo;

  Label m_supportedOpsLable;

  PropertyIsCOMPOperation m_operation;

  final private TreeSet<String> m_allSupportedOps = new TreeSet<String>();

  private TreeSet<String> m_supportedOps;

  boolean m_secondRowTextModified;

  private ComboViewer m_propViewer;

  public PropertyIsCOMPOperationComposite( final Composite parent, final int style, final PropertyIsCOMPOperation operation, TreeSet<String> supportedOperations, final IErrorMessageReciever errorMessageReciever, final IFeatureType ft )
  {
    super( parent, style, errorMessageReciever, ft );
    m_operation = operation;
    m_supportedOps = supportedOperations;
    setSupportedOperations( supportedOperations );
    setControl();
  }

  private void setSupportedOperations( TreeSet<String> supportedOps )
  {
    if( supportedOps != null && !supportedOps.isEmpty() )
      m_supportedOps = supportedOps;
    else
    {
      // m_allSupportedCompOps.add( OperationDefines.getNameById( OperationDefines.PROPERTYISBETWEEN ) );
      m_allSupportedOps.add( OperationDefines.getNameById( OperationDefines.PROPERTYISGREATERTHAN ) );
      m_allSupportedOps.add( OperationDefines.getNameById( OperationDefines.PROPERTYISGREATERTHANOREQUALTO ) );
      m_allSupportedOps.add( OperationDefines.getNameById( OperationDefines.PROPERTYISLESSTHAN ) );
      m_allSupportedOps.add( OperationDefines.getNameById( OperationDefines.PROPERTYISLESSTHANOREQUALTO ) );
      // m_allSupportedCompOps.add( OperationDefines.getNameById( OperationDefines.PROPERTYISLIKE ) );
      // m_allSupportedCompOps.add( OperationDefines.getNameById( OperationDefines.PROPERTYISNULL ) );
    }
  }

  private void setControl( )
  {
    // This implementation is only for the Expression Literal.
    // TODO Functions and Arithmetics (according the OGC filterencoding Specs) are not supported!
    Expression firstExpression = null;
    Expression secondExpression = null;
    if( m_operation != null )
    {
      firstExpression = m_operation.getFirstExpression();
      secondExpression = m_operation.getSecondExpression();
    }// is the case when an new (empty) filter operation is to be displayed
    else if( m_operation == null )
    {
      final PropertyName propertyName = new PropertyName( EMPTY_VALUE );
      final Literal literal = new Literal( EMPTY_VALUE );
      m_operation = new PropertyIsCOMPOperation( m_operation.getOperatorId(), propertyName, literal );
    }
    final GridData data1 = new GridData( GridData.FILL_HORIZONTAL );
    data1.widthHint = STANDARD_WIDTH_FIELD;
    // possible oprations (they have been initialized when calling the factory)
    m_supportedOpsLable = new Label( this, SWT.NULL );
    m_supportedOpsLable.setText( "Operation:" );
    m_supportedOpsCombo = new Combo( this, SWT.FILL | SWT.DROP_DOWN | SWT.READ_ONLY );
    m_supportedOpsCombo.setLayoutData( data1 );
    String[] namesOps = null;
    if( m_supportedOps == null || m_supportedOps.isEmpty() )
      namesOps = m_allSupportedOps.toArray( new String[m_allSupportedOps.size()] );
    else
      namesOps = m_supportedOps.toArray( new String[m_allSupportedOps.size()] );

    int j = ArrayUtils.indexOf( namesOps, m_operation.getOperatorName() );
    m_supportedOpsCombo.setItems( namesOps );
    m_supportedOpsCombo.select( j );
    m_supportedOpsCombo.addSelectionListener( new SelectionAdapter()
    {

      @Override
      public void widgetSelected( SelectionEvent e )
      {
        final String item = m_supportedOpsCombo.getItem( m_supportedOpsCombo.getSelectionIndex() );
        int newOperationId = OperationDefines.getIdByName( item );
        final PropertyIsCOMPOperation comparisonOperation = m_operation;
        comparisonOperation.setOperatorId( newOperationId );
        fireModellEvent( new ModellEvent( PropertyIsCOMPOperationComposite.this, ModellEvent.WIDGET_CHANGE ) );
      }

    } );
    m_firstRowLabel = new Label( this, SWT.FILL );
    m_firstRowLabel.setText( "Feld:" );
    final Combo firstRowCombo = new Combo( this, SWT.FILL | SWT.READ_ONLY );
    final GridData data = new GridData( GridData.FILL_HORIZONTAL );
    data.widthHint = STANDARD_WIDTH_FIELD;
    firstRowCombo.setLayoutData( data );
    m_propViewer = new ComboViewer( firstRowCombo );
    m_propViewer.setContentProvider( new FeatureTypeContentProvider() );
    m_propViewer.setLabelProvider( new FeatureTypeLabelProvider() );
    m_propViewer.addFilter( new NonGeometryPropertyFilter() );
    m_propViewer.add( m_ft.getProperties() );
    m_propViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {

      public void selectionChanged( SelectionChangedEvent event )
      {
        Object firstElement = ((IStructuredSelection) event.getSelection()).getFirstElement();
        if( firstElement instanceof IValuePropertyType )
        {
          final IValuePropertyType vtp = ((IValuePropertyType) firstElement);
          final String content = m_secondRowText.getText();
          validate( vtp, content );
          updateOperation();
        }
      }
    } );
    m_secondRowLabel = new Label( this, SWT.FILL );
    m_secondRowLabel.setText( "Wert" );
    m_secondRowText = new Text( this, SWT.FILL | SWT.BORDER );
    m_secondRowText.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_secondRowText.addFocusListener( new FocusListener()
    {
      public void focusGained( FocusEvent e )
      {
        // do nothing
      }

      public void focusLost( FocusEvent e )
      {
        updateOperation();
        m_secondRowTextModified = false;
      }
    } );
    m_secondRowText.addKeyListener( new KeyListener()
    {

      public void keyPressed( KeyEvent e )
      {
        // do nothing
      }

      public void keyReleased( KeyEvent e )
      {
        if( e.keyCode == SWT.CR && m_secondRowTextModified )
        {
          updateOperation();
          m_secondRowTextModified = false;
        }

      }
    } );
    m_secondRowText.addModifyListener( new ModifyListener()
    {

      public void modifyText( ModifyEvent e )
      {
        m_secondRowTextModified = true;
      }
    } );

    if( firstExpression instanceof PropertyName && secondExpression instanceof Literal )
    {
      if( firstExpression != null && secondExpression != null )
      {
        final String value = ((Literal) secondExpression).getValue();
        m_secondRowText.setText( value );
        m_propViewer.setSelection( new StructuredSelection( setPropertySelection( m_operation.getFirstExpression() ) ) );
      }
    }
    else if( firstExpression instanceof PropertyName && secondExpression instanceof ArithmeticExpression )
    {
      // not implemented
    }// only for "expr1 instance PropertyName" and "expr2 instanceof Literal"
  }

  void updateOperation( )
  {
    final IStructuredSelection selection = (IStructuredSelection) m_propViewer.getSelection();
    final Object firstElement = selection.getFirstElement();
    if( firstElement instanceof IValuePropertyType )
    {
      final IValuePropertyType vpt = ((IValuePropertyType) firstElement);
      final IPropertyType ftp = m_ft.getProperty( vpt.getQName() );
      final String literalName = m_secondRowText.getText().trim();
      validate( (IValuePropertyType) ftp, literalName );
      m_operation.setFirstExperssion( new PropertyName( vpt.getQName() ) );
      m_operation.setSecondExperssion( new Literal( literalName ) );
    }
  }

}