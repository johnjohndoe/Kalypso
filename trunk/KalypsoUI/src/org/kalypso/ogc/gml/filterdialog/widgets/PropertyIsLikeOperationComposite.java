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

import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.ogc.gml.filterdialog.dialog.IErrorMessageReciever;
import org.kalypso.ogc.gml.filterdialog.model.FeatureTypeContentProvider;
import org.kalypso.ogc.gml.filterdialog.model.FeatureTypeLabelProvider;
import org.kalypso.ogc.gml.filterdialog.model.NonGeometryPropertyFilter;
import org.kalypsodeegree_impl.filterencoding.Literal;
import org.kalypsodeegree_impl.filterencoding.PropertyIsLikeOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyName;

class PropertyIsLikeOperationComposite extends AbstractFilterComposite
{
  Label m_firstRowLabel;

  // Combo m_firstRowCombo;

  Text m_secondRowText;

  Label m_errorLabel;

  Text m_errorMessage;

  Label m_secondRowLabel;

  Text m_thirdRowText;

  Text m_wildCard;

  Label m_wildCardLabel;

  Label m_singleCharLabel;

  Text m_singleChar;

  Label m_escpapeCharLabel;

  Text m_escpapeChar;

  PropertyIsLikeOperation m_operation;

  ComboViewer m_propViewer;

  public PropertyIsLikeOperationComposite( final Composite parent, final int style, final PropertyIsLikeOperation operation, final IErrorMessageReciever errorMessageReciever, final IFeatureType ft )
  {
    super( parent, style, errorMessageReciever, ft );
    m_operation = operation;
    setControl();

  }

  private void setControl( )
  {
    PropertyName firstExpression = m_operation.getPropertyName();
    Literal secondExpression = m_operation.getLiteral();
    if( firstExpression == null && secondExpression == null )
    {
      firstExpression = new PropertyName( EMPTY_VALUE );
      secondExpression = new Literal( EMPTY_VALUE );
    }
    else if( firstExpression == null && secondExpression != null )
    {
      firstExpression = new PropertyName( EMPTY_VALUE );
    }
    else if( firstExpression != null && secondExpression == null )
    {
      secondExpression = new Literal( EMPTY_VALUE );
    }
    m_firstRowLabel = new Label( this, SWT.FILL );
    m_firstRowLabel.setText( firstExpression.getExpressionName().trim() );
    Combo firstRowCombo = new Combo( this, SWT.FILL | SWT.READ_ONLY | SWT.DROP_DOWN );
    GridData data = new GridData( GridData.FILL_HORIZONTAL );
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
          String item = ((IValuePropertyType) firstElement).getQName().getLocalPart();
          PropertyName propertyName = m_operation.getPropertyName();
          if( propertyName == null )
            propertyName = new PropertyName( item );
          else
            propertyName.setValue( item );
          m_operation.setPropertyName( propertyName );
        }

      }
    } );
    m_propViewer.setSelection( new StructuredSelection( setPropertySelection( m_operation.getPropertyName() ) ) );

    m_secondRowLabel = new Label( this, SWT.FILL );
    m_secondRowLabel.setText( secondExpression.getExpressionName().trim() );
    m_secondRowText = new Text( this, SWT.FILL | SWT.BORDER );
    m_secondRowText.setText( secondExpression.getValue() );
    m_secondRowText.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_secondRowText.addFocusListener( new FocusListener()
    {

      public void focusGained( FocusEvent e )
      {
        // do nothing
      }

      public void focusLost( FocusEvent e )
      {
        IStructuredSelection selection = (IStructuredSelection) m_propViewer.getSelection();
        if( !selection.isEmpty() )
        {
          Object firstElement = selection.getFirstElement();
          if( firstElement instanceof IValuePropertyType )
          {
            IValuePropertyType vpt = (IValuePropertyType) firstElement;
            String str = m_secondRowText.getText().trim();
            validate( vpt, str );
            Literal literal = m_operation.getLiteral();
            if( literal == null )
              literal = new Literal( str );
            else
              literal.setValue( str );
            m_operation.setLiteral( literal );
            // fireModellEvent( new ModellEvent( PropertyIsLikeOperationComposite.this, ModellEvent.WIDGET_CHANGE ) );
          }
        }
        // String item = m_firstRowCombo.getItem( m_firstRowCombo.getSelectionIndex() );
        // IPropertyType ftp = m_ft.getProperty( item );
        // if( item != null && ftp != null )
        // {
        // TextFieldToPropertyTypeValidator validator = new TextFieldToPropertyTypeValidator( (IValuePropertyType) ftp
        // );
        // String test = validator.isValid( m_secondRowText.getText() );
        // m_errorMessageReciever.setErrorMessage( test );
        // String str = m_secondRowText.getText().trim();
        // Literal literal = m_operation.getLiteral();
        // if( literal == null )
        // literal = new Literal( str );
        // else
        // literal.setValue( str );
        // m_operation.setLiteral( literal );
        // fireModellEvent( new ModellEvent( FilterCompositeFactory.this, ModellEvent.WIDGET_CHANGE ) );
        // }
      }
    } );
    Group parameterGroup = new Group( this, SWT.LEFT );
    GridData data3 = new GridData( GridData.FILL_HORIZONTAL );
    data3.horizontalSpan = 2;
    parameterGroup.setLayoutData( data3 );
    parameterGroup.setLayout( new GridLayout( 2, true ) );
    parameterGroup.setText( "Spezial Zeichen" );
    m_wildCardLabel = new Label( parameterGroup, SWT.NONE );
    m_wildCardLabel.setText( "Wildcard:" );
    m_wildCard = new Text( parameterGroup, SWT.NONE | SWT.READ_ONLY );
    m_wildCard.setText( String.valueOf( m_operation.getWildCard() ) );
    m_singleCharLabel = new Label( parameterGroup, SWT.NONE );
    m_singleCharLabel.setText( "Einzelnes Zeichen:" );
    m_singleChar = new Text( parameterGroup, SWT.NONE | SWT.READ_ONLY );
    m_singleChar.setText( String.valueOf( m_operation.getSingleChar() ) );
    m_escpapeCharLabel = new Label( parameterGroup, SWT.NONE );
    m_escpapeCharLabel.setText( "Escape Zeichen:" );
    m_escpapeChar = new Text( parameterGroup, SWT.NONE | SWT.READ_ONLY );
    m_escpapeChar.setText( String.valueOf( m_operation.getEscapeChar() ) );
  }

  
}