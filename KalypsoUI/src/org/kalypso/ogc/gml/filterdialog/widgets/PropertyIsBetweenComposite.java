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

import javax.xml.namespace.QName;

import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.filterdialog.dialog.IErrorMessageReciever;
import org.kalypso.ogc.gml.filterdialog.model.FeatureTypeContentProvider;
import org.kalypso.ogc.gml.filterdialog.model.FeatureTypeLabelProvider;
import org.kalypso.ogc.gml.filterdialog.model.NonGeometryPropertyFilter;
import org.kalypsodeegree.filterencoding.Expression;
import org.kalypsodeegree_impl.filterencoding.BoundaryExpression;
import org.kalypsodeegree_impl.filterencoding.Literal;
import org.kalypsodeegree_impl.filterencoding.PropertyIsBetweenOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyName;

class PropertyIsBetweenComposite extends AbstractFilterComposite
{

  Label m_propertyComboLabel;

  // Combo propertyCombo;

  Label m_upperBComboLabel;

  Text m_upperBText;

  Label m_lowerBComboLabel;

  Text m_lowerBText;

  PropertyIsBetweenOperation m_operation;

  protected boolean m_upperBtextModified;

  protected boolean m_lowerBTextModified;

  private ComboViewer propViewer;

  public PropertyIsBetweenComposite( final Composite parent, final int style, final PropertyIsBetweenOperation operation, final IErrorMessageReciever errorMessageReciever, final IFeatureType ft )
  {
    super( parent, style, errorMessageReciever, ft );
    m_operation = operation;
    setControl();

  }

  private void setControl( )
  {
    PropertyName propertyName = m_operation.getPropertyName();
    Expression upperBoundary = m_operation.getUpperBoundary();
    Expression lowerBoundary = m_operation.getLowerBoundary();
    if( propertyName == null )
    {
      propertyName = new PropertyName( EMPTY_VALUE );
      m_operation.setPropertyName( propertyName );
    }
    if( upperBoundary == null )
    {
      upperBoundary = new BoundaryExpression( EMPTY_VALUE );
      m_operation.setUpperBoundary( upperBoundary );
    }
    if( lowerBoundary == null )
    {
      lowerBoundary = new BoundaryExpression( EMPTY_VALUE );
      m_operation.setLowerBoundary( lowerBoundary );
    }
    m_propertyComboLabel = new Label( this, SWT.NULL );
    m_propertyComboLabel.setText( Messages.getString("org.kalypso.ogc.gml.filterdialog.widgets.PropertyIsBetweenComposite.0") ); //$NON-NLS-1$
    Combo propertyCombo = new Combo( this, SWT.FILL | SWT.DROP_DOWN );
    GridData data = new GridData( GridData.FILL_HORIZONTAL );
    data.widthHint = STANDARD_WIDTH_FIELD;
    propertyCombo.setLayoutData( data );
    propViewer = new ComboViewer( propertyCombo );
    propViewer.setContentProvider( new FeatureTypeContentProvider() );
    propViewer.setLabelProvider( new FeatureTypeLabelProvider() );
    propViewer.addFilter( new NonGeometryPropertyFilter() );
    propViewer.add( m_ft.getProperties() );
    propViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {

      public void selectionChanged( SelectionChangedEvent event )
      {
        final Object firstElement = ((IStructuredSelection) event.getSelection()).getFirstElement();
        if( firstElement instanceof IValuePropertyType )
        {
          final QName propName = ((IValuePropertyType) firstElement).getQName();
          m_operation.setPropertyName( new PropertyName( propName ) );
        }
      }
    } );

    propViewer.setSelection( new StructuredSelection( setPropertySelection( m_operation.getPropertyName() ) ) );

    // lower boundary
    m_lowerBComboLabel = new Label( this, SWT.NULL );
    m_lowerBComboLabel.setText( Messages.getString("org.kalypso.ogc.gml.filterdialog.widgets.PropertyIsBetweenComposite.1") ); //$NON-NLS-1$
    m_lowerBText = new Text( this, SWT.FILL );
    final GridData data3 = new GridData( GridData.FILL_HORIZONTAL );
    data.widthHint = STANDARD_WIDTH_FIELD;
    m_lowerBText.setLayoutData( data3 );
    final Expression lB = m_operation.getLowerBoundary();
    if( lB instanceof BoundaryExpression )
      m_lowerBText.setText( ((BoundaryExpression) lB).getValue() );
    else if( lB instanceof Literal )
      m_lowerBText.setText( ((Literal) lB).getValue() );
    m_lowerBText.addModifyListener( new ModifyListener()
    {
      public void modifyText( ModifyEvent e )
      {
        m_lowerBTextModified = true;
      }
    } );
    m_lowerBText.addKeyListener( new KeyAdapter()
    {

      @Override
      public void keyReleased( KeyEvent e )
      {
        if( e.character == SWT.CR && m_lowerBTextModified )
        {
          final String lBText = m_lowerBText.getText();
          updateLowerBoundaryValue( lBText );
        }

      }
    } );
    m_lowerBText.addFocusListener( new FocusAdapter()
    {

      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusLost( FocusEvent e )
      {
        final String lBText = m_lowerBText.getText();
        updateLowerBoundaryValue( lBText );
      }
    } );
    // upper boundary
    m_upperBComboLabel = new Label( this, SWT.NULL );
    m_upperBComboLabel.setText( Messages.getString("org.kalypso.ogc.gml.filterdialog.widgets.PropertyIsBetweenComposite.2") ); //$NON-NLS-1$
    m_upperBText = new Text( this, SWT.FILL );
    final GridData data2 = new GridData( GridData.FILL_HORIZONTAL );
    data.widthHint = STANDARD_WIDTH_FIELD;
    m_upperBText.setLayoutData( data2 );
    final Expression uB = m_operation.getUpperBoundary();
    if( uB instanceof BoundaryExpression )
      m_upperBText.setText( ((BoundaryExpression) uB).getValue() );
    else if( uB instanceof Literal )
      m_upperBText.setText( ((Literal) uB).getValue() );
    m_upperBText.addModifyListener( new ModifyListener()
    {

      public void modifyText( ModifyEvent e )
      {
        m_upperBtextModified = true;
      }
    } );
    m_upperBText.addKeyListener( new KeyAdapter()
    {

      @Override
      public void keyReleased( KeyEvent e )
      {
        if( e.character == SWT.CR && m_upperBtextModified )
        {
          final String uBText = m_lowerBText.getText();
          updateUpperBoundaryValue( uBText );
        }

      }
    } );
    m_upperBText.addFocusListener( new FocusAdapter()
    {

      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusLost( FocusEvent e )
      {
        final String uBText = m_upperBText.getText();
        updateUpperBoundaryValue( uBText );
      }
    } );
  }

  void updateLowerBoundaryValue( final String lBText )
  {
    double value;
    try
    {
      value = Double.parseDouble( lBText );
      final Expression lb = m_operation.getLowerBoundary();
      if( lb instanceof BoundaryExpression )
      {
        ((BoundaryExpression) lb).setValue( String.valueOf( value ) );
        m_lowerBTextModified = false;
        setErrorMessage( null );
      }
    }
    catch( NumberFormatException ne )
    {
      final String exType = ne.getClass().getName().replaceAll( ".+\\.", "" ); //$NON-NLS-1$ //$NON-NLS-2$
      setErrorMessage( exType + "\t" + ne.getMessage() ); //$NON-NLS-1$
    }
  }

  void updateUpperBoundaryValue( final String lBText )
  {
    double value;
    try
    {
      value = Double.parseDouble( lBText );
      final Expression ub = m_operation.getUpperBoundary();
      if( ub instanceof BoundaryExpression )
      {
        ((BoundaryExpression) ub).setValue( String.valueOf( value ) );
        m_upperBtextModified = false;
        setErrorMessage( null );
      }
    }
    catch( NumberFormatException ne )
    {
      final String exType = ne.getClass().getName().replaceAll( ".+\\.", "" ); //$NON-NLS-1$ //$NON-NLS-2$
      setErrorMessage( exType + "\t" + ne.getMessage() ); //$NON-NLS-1$
    }
  }
}