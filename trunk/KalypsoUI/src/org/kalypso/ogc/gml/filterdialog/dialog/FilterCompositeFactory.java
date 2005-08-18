/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ogc.gml.filterdialog.dialog;

import java.util.TreeSet;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.filterencoding.Expression;
import org.kalypsodeegree.filterencoding.Operation;
import org.kalypsodeegree.gml.GMLException;
import org.kalypsodeegree.gml.GMLGeometry;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.filterencoding.ArithmeticExpression;
import org.kalypsodeegree_impl.filterencoding.Literal;
import org.kalypsodeegree_impl.filterencoding.OperationDefines;
import org.kalypsodeegree_impl.filterencoding.PropertyIsCOMPOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyIsLikeOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyIsNullOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyName;
import org.kalypsodeegree_impl.filterencoding.SpatialOperation;
import org.kalypsodeegree_impl.gml.GMLFactory;
import org.kalypsodeegree_impl.gml.schema.SpecialPropertyMapper;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author kuepfer
 */
public class FilterCompositeFactory
{
  private static FilterCompositeFactory m_factory = new FilterCompositeFactory();

  protected FeatureType m_ft;

  protected Operation m_operation;

  private static final int STANDARD_WIDTH_FIELD = 150;

  public static final String EMPTY_VALUE = "-NULL-";

  private TreeSet m_allsupportedSpatialOps = new TreeSet();

  public static TreeSet m_supportedOperations;

  private TreeSet m_allSupportedCompOps = new TreeSet();

  private FilterCompositeFactory()
  {
    if( m_allsupportedSpatialOps.size() == 0 )
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
    if( m_allSupportedCompOps.size() == 0 )
    {
      //      m_allSupportedCompOps.add( OperationDefines.getNameById( OperationDefines.PROPERTYISBETWEEN ) );
      m_allSupportedCompOps.add( OperationDefines.getNameById( OperationDefines.PROPERTYISGREATERTHAN ) );
      m_allSupportedCompOps.add( OperationDefines.getNameById( OperationDefines.PROPERTYISGREATERTHANOREQUALTO ) );
      m_allSupportedCompOps.add( OperationDefines.getNameById( OperationDefines.PROPERTYISLESSTHAN ) );
      m_allSupportedCompOps.add( OperationDefines.getNameById( OperationDefines.PROPERTYISLESSTHANOREQUALTO ) );
      //      m_allSupportedCompOps.add( OperationDefines.getNameById( OperationDefines.PROPERTYISLIKE ) );
      //      m_allSupportedCompOps.add( OperationDefines.getNameById( OperationDefines.PROPERTYISNULL ) );

    }
  }

  protected TreeSet getSupportedSpatialOperations()
  {
    return m_allsupportedSpatialOps;
  }

  protected TreeSet getSupportedCOMPOperations()
  {
    return m_allSupportedCompOps;
  }

  public static FilterCompositeFactory getInstance( TreeSet supportedOperations )
  {
    m_supportedOperations = supportedOperations;

    return m_factory;
  }

  protected Operation getOperation()
  {
    return m_operation;
  }

  public Composite createFilterElementComposite( Operation operation, Composite parent, FeatureType ft )
  {
    Composite c = null;
    m_ft = ft;
    m_operation = operation;
    if( m_operation != null )
    {
      String operatorName = m_operation.getOperatorName();
      if( operation instanceof PropertyIsCOMPOperation )
      {
        if( operatorName == null )
          operatorName = "Unbekannter Comperator";
        ( (Group)parent ).setText( "Eigenschaften-" + operatorName );
        c = new PropertyIsCOMPOperationComposite( parent, SWT.NULL );
      }
      else if( operation instanceof PropertyIsLikeOperation )
      {
        if( operatorName == null )
          operatorName = "Unbekannter IsLike Operator";
        ( (Group)parent ).setText( "Eigenschaften-" + operatorName );
        c = new PropertyIsCOMPOperationComposite( parent, SWT.NULL );

      }
      else if( operation instanceof SpatialOperation )
      {
        if( operatorName == null )
          operatorName = "Unbekannter Spatial Operator";
        ( (Group)parent ).setText( "Eigenschaften-" + operatorName );
        c = new SpatialComposite( parent, SWT.NULL );
      }
    }
    return c;
  }

  class SpatialComposite extends Composite
  {
    private Combo m_combo;

    protected Text m_text;

    private Label m_spatialLabel;

    private Label m_geomLable;

    private Button m_loadButton;

    protected Combo m_scrabLayerCombo;

    private Label m_supportedOpsLable;

    private Combo m_supportedOpsCombo;

    public SpatialComposite( Composite parent, int style )
    {
      super( parent, style );

      GMLGeometry geometry = ( (SpatialOperation)getOperation() ).getGeometry();
      PropertyName propertyName = ( (SpatialOperation)getOperation() ).getPropertyName();
      String opsName = getOperation().getOperatorName();
      //Top-Group
      setLayout( new GridLayout( 2, false ) );
      GridData data1 = new GridData( GridData.FILL_HORIZONTAL );
      data1.widthHint = STANDARD_WIDTH_FIELD;
      //possible oprations (they have been initialized when calling the factory)
      m_supportedOpsLable = new Label( this, SWT.NULL );
      m_supportedOpsLable.setText( "Operation" );
      m_supportedOpsCombo = new Combo( this, SWT.FILL | SWT.DROP_DOWN );
      m_supportedOpsCombo.setLayoutData( data1 );
      String[] namesOps = null;
      if( m_supportedOperations == null )
      {
        namesOps = (String[])getSupportedSpatialOperations().toArray(
            new String[getSupportedSpatialOperations().size()] );
      }
      else
        namesOps = (String[])getSupportedSpatialOperations().toArray();
      m_supportedOpsCombo.setItems( namesOps );
      //set the selection to the current operation type, if not availabel a blank is selected by default (Combo)
      int j = ArrayUtils.indexOf( namesOps, opsName );
      m_supportedOpsCombo.select( j );
      m_supportedOpsCombo.addSelectionListener( new SelectionAdapter()
      {

        public void widgetSelected( SelectionEvent e )
        {
          Combo c = ( (Combo)e.widget );
          String item = c.getItem( c.getSelectionIndex() );
          int newOperationId = OperationDefines.getIdByName( item );
          ( (SpatialOperation)m_operation ).setOperatorId( newOperationId );
        }

        public void widgetDefaultSelected( SelectionEvent e )
        {
          widgetSelected( e );
        }
      } );
      //set Geometry
      GridData data = new GridData( GridData.FILL_HORIZONTAL );
      data.widthHint = STANDARD_WIDTH_FIELD;
      m_spatialLabel = new Label( this, SWT.NULL );
      m_spatialLabel.setText( "Geometry" );
      m_combo = new Combo( this, SWT.FILL | SWT.DROP_DOWN );
      m_combo.setLayoutData( data );
      m_combo.addSelectionListener( new SelectionAdapter()
      {
        public void widgetSelected( SelectionEvent e )
        {
          Combo c = (Combo)e.widget;
          PropertyName pn = new PropertyName( c.getItem( c.getSelectionIndex() ) );
          ( (SpatialOperation)m_operation ).setProperty( pn );
        }
      } );

      FeatureTypeProperty[] properties = m_ft.getProperties();
      for( int i = 0; i < properties.length; i++ )
      {
        FeatureTypeProperty property = properties[i];
        if( property.isGeometryProperty() )
          m_combo.add( property.getName().trim() );
      }
      //sets the availabel property active, if the property name does not match an emty entry is created
      int index = -1;
      if( propertyName != null )
      {
        index = ArrayUtils.indexOf( m_combo.getItems(), propertyName.getValue() );
        m_combo.select( index );

      }
      else
      {
        m_combo.add( "...", 0 );
        m_combo.select( 0 );
      }
      m_geomLable = new Label( this, SWT.NULL );
      m_geomLable.setText( "Operator" );
      m_text = new Text( this, SWT.READ_ONLY );
      m_text.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
      if( geometry != null )
        m_text.setText( geometry.getName().trim() );
      else
        m_text.setText( "Geometrie ist noch leer!" );
      m_loadButton = new Button( this, SWT.CHECK );
      m_loadButton.setText( "neuer Operator laden" );
      m_loadButton
          .setToolTipText( "Ermöglicht das laden eines neuen geometrischen Operators aus dem Zeichungs-Thema der Karte" );
      m_loadButton.setSelection( false );
      m_loadButton.addSelectionListener( new SelectionListener()
      {

        public void widgetSelected( SelectionEvent e )
        {
          Button button = (Button)e.widget;
          if( button.getSelection() )
          {
            m_scrabLayerCombo.setVisible( true );
            MessageDialog.openInformation( getShell(), "Unimplemented Action", "Select the geometry from the list!" );
            //dummy geometry, just for testing
            GM_Object geom = GeometryFactory.createGM_Point( 100, 200, KalypsoGisPlugin.getDefault()
                .getCoordinatesSystem() );
            GMLGeometry gml = null;
            try
            {
              //update GMLGeometry
              gml = GMLFactory.createGMLGeometry( null, geom );
              ( (SpatialOperation)m_operation ).setGeometry( gml );
              m_text.setText( gml.getName() );
            }
            catch( GMLException e1 )
            {
              //TODO What shall we do? good question....
              e1.printStackTrace();
            }
            catch( GM_Exception e2 )
            {
              //TODO What shall we do? good question....
              e2.printStackTrace();
            }

          }
          else
            m_scrabLayerCombo.setVisible( false );

        }

        public void widgetDefaultSelected( SelectionEvent e )
        {
        // do nothing

        }
      } );
      m_scrabLayerCombo = new Combo( this, SWT.NULL );
      m_scrabLayerCombo.setItems( new String[]
      { "ScrabPolygon_1", "ScrabLine_2", "ScrabPoint_3" } );
      m_scrabLayerCombo.select( 0 );
      m_scrabLayerCombo.setVisible( false );
      this.setFocus();

    }

  }

  class LogicalOperatorComposite extends Composite
  {

    public LogicalOperatorComposite( Composite parent, int style )
    {
      super( parent, style );
      setLayout( new GridLayout( 2, false ) );

    }

  }

  class PropertyIsCOMPOperationComposite extends Composite
  {

    private Label m_firstRowLabel = null;

    protected Combo m_firstRowCombo = null;

    private Label m_secondRowLabel = null;

    protected Text m_secondRowText = null;

    protected Text m_errorMessage = null;

    protected Label m_errorLabel = null;

    private Combo m_supportedOpsCombo;

    private Label m_supportedOpsLable;

    public PropertyIsCOMPOperationComposite( Composite parent, int style )
    {
      super( parent, style );
      //This implementation is only for the Expression Literal.
      //TODO Functions and Arithmetics (according the OGC filterencoding Specs) are not supported!
      String opsName = m_operation.getOperatorName();
      Expression firstExpression = null;
      Expression secondExpression = null;
      if( m_operation != null )
      {
        firstExpression = ( (PropertyIsCOMPOperation)m_operation ).getFirstExpression();
        secondExpression = ( (PropertyIsCOMPOperation)m_operation ).getSecondExpression();
      }//is the case when an new (empty) filter operation is to be displayed
      else if( m_operation == null )
      {
        m_operation = new PropertyIsCOMPOperation( m_operation.getOperatorId(), new PropertyName( EMPTY_VALUE ),
            new Literal( EMPTY_VALUE ) );
      }
      setLayout( new GridLayout( 2, false ) );
      GridData data1 = new GridData( GridData.FILL_HORIZONTAL );
      data1.widthHint = STANDARD_WIDTH_FIELD;
      //possible oprations (they have been initialized when calling the factory)
      m_supportedOpsLable = new Label( this, SWT.NULL );
      m_supportedOpsLable.setText( "Operation" );
      m_supportedOpsCombo = new Combo( this, SWT.FILL | SWT.DROP_DOWN );
      m_supportedOpsCombo.setLayoutData( data1 );
      String[] namesOps = null;
      if( m_supportedOperations == null )
      {
        namesOps = (String[])getSupportedCOMPOperations().toArray( new String[getSupportedCOMPOperations().size()] );
      }
      else
        namesOps = (String[])getSupportedCOMPOperations().toArray();
      int j = ArrayUtils.indexOf( namesOps, opsName );
      m_supportedOpsCombo.setItems( namesOps );
      m_supportedOpsCombo.select( j );
      m_supportedOpsCombo.addSelectionListener( new SelectionAdapter()
      {

        public void widgetSelected( SelectionEvent e )
        {
          Combo c = ( (Combo)e.widget );
          String item = c.getItem( c.getSelectionIndex() );
          int newOperationId = OperationDefines.getIdByName( item );
          //TODO only implemented with propery name and liteal ?? is that all ??
          ( (PropertyIsCOMPOperation)m_operation ).setOperatorId( newOperationId );
          ( (PropertyIsCOMPOperation)m_operation ).setFirstExperssion( new PropertyName( m_firstRowCombo.getText()
              .trim() ) );
          ( (PropertyIsCOMPOperation)m_operation )
              .setSecondExperssion( new Literal( m_secondRowText.getText().trim() ) );
          System.out.println( "test" );
        }

        public void widgetDefaultSelected( SelectionEvent e )
        {
        //widgetSelected( e );
        }
      } );
      m_firstRowLabel = new Label( this, SWT.FILL );
      m_firstRowCombo = new Combo( this, SWT.FILL );
      GridData data = new GridData( GridData.FILL_HORIZONTAL );
      data.widthHint = STANDARD_WIDTH_FIELD;
      m_firstRowCombo.setLayoutData( data );
      m_firstRowCombo.addSelectionListener( new SelectionAdapter()
      {
        public void widgetSelected( SelectionEvent e )
        {
          Combo c = (Combo)e.widget;
          String item = c.getItem( c.getSelectionIndex() );
          FeatureTypeProperty ftp = m_ft.getProperty( item );
          TextFieldValidator validator = new TextFieldValidator( ftp );
          String test = validator.isValid( m_secondRowText.getText() );
          if( test != null )
          {
            Image image = getDisplay().getSystemImage( SWT.ICON_ERROR );
            m_errorLabel.setImage( image );
            m_errorMessage.setText( test );
            m_secondRowText.setFocus();
            m_secondRowText.setText( "Bitte eine Zahl eingeben" );
            m_secondRowText.selectAll();
            pack();
          }
          else
          {
            m_errorMessage.setText( "" );
            m_errorLabel.setImage( null );
            m_firstRowCombo.setFocus();
            //update model data
            PropertyName pn = new PropertyName( item );
            ( (PropertyIsCOMPOperation)m_operation ).setFirstExperssion( pn );
            Literal l = new Literal( m_secondRowText.getText().trim() );
            ( (PropertyIsCOMPOperation)m_operation ).setSecondExperssion( l );
            System.out.print( "" );
          }
        }
      } );
      m_secondRowLabel = new Label( this, SWT.FILL );
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
          String item = m_firstRowCombo.getItem( m_firstRowCombo.getSelectionIndex() );
          FeatureTypeProperty ftp = m_ft.getProperty( item );
          if( item != null && ftp != null )
          {
            TextFieldValidator validator = new TextFieldValidator( ftp );
            String test = null;
            if( m_secondRowText.getText() != "" )
              test = validator.isValid( m_secondRowText.getText() );
            if( test != null )
            {
              Image image = getDisplay().getSystemImage( SWT.ICON_ERROR );
              m_errorLabel.setImage( image );
              m_errorMessage.setText( test );
              m_secondRowText.setFocus();
              m_secondRowText.setText( "Bitte eine Zahl eingeben" );
              m_secondRowText.selectAll();
              pack();
            }
            else
            {
              m_errorMessage.setText( "" );
              m_errorLabel.setImage( null );
              m_firstRowCombo.setFocus();
              PropertyName pn = new PropertyName( item );
              ( (PropertyIsCOMPOperation)m_operation ).setFirstExperssion( pn );
              Literal l = new Literal( m_secondRowText.getText().trim() );
              ( (PropertyIsCOMPOperation)m_operation ).setSecondExperssion( l );
            }
          }
        }
      } );

      //Error Panel
      m_errorLabel = new Label( this, SWT.NULL );
      m_errorMessage = new Text( this, SWT.FILL | SWT.READ_ONLY | SWT.MULTI );
      GridData data2 = new GridData();
      data2.widthHint = 150;
      data2.heightHint = 30;
      m_errorMessage.setLayoutData( data2 );
      m_errorMessage.setText( "" );

      FeatureTypeProperty[] properties = m_ft.getProperties();
      for( int i = 0; i < properties.length; i++ )
      {
        FeatureTypeProperty property = properties[i];
        if( SpecialPropertyMapper.isValidMapping( String.class.getName(), property.getType() ) )
          m_firstRowCombo.add( property.getName() );
      }
      if( firstExpression instanceof PropertyName && secondExpression instanceof Literal )
      {
        m_firstRowLabel.setText( firstExpression.getExpressionName().trim() );
        m_secondRowLabel.setText( secondExpression.getExpressionName().trim() );
        m_secondRowText.setText( ( (Literal)secondExpression ).getValue() );
      }
      else if( firstExpression instanceof PropertyName && secondExpression instanceof ArithmeticExpression )
      {
        //not implemented
      }
    }//only for "expr1 instance PropertyName" and "expr2 instanceof Literal"

  }

  class PropertyIsLikeOperationComposite extends Composite
  {

    private Label m_firstRowLabel;

    protected Combo m_firstRowCombo;

    protected Text m_secondRowText;

    protected Label m_errorLabel;

    protected Text m_errorMessage;

    private Label m_secondRowLabel;

    private Text m_thirdRowText;

    public PropertyIsLikeOperationComposite( Composite parent, int style )
    {
      super( parent, style );
      PropertyName firstExpression = null;
      Literal secondExpression = null;
      if( m_operation != null )
      {
        firstExpression = ( (PropertyIsLikeOperation)m_operation ).getPropertyName();
        secondExpression = ( (PropertyIsLikeOperation)m_operation ).getLiteral();
      }
      else if( m_operation == null )
      {
        firstExpression = new PropertyName( EMPTY_VALUE );
        secondExpression = new Literal( EMPTY_VALUE );
      }
      setLayout( new GridLayout( 2, false ) );
      m_firstRowLabel = new Label( this, SWT.FILL );
      m_firstRowCombo = new Combo( this, SWT.FILL );
      m_firstRowLabel.setText( firstExpression.getExpressionName().trim() );
      GridData data = new GridData( GridData.FILL_HORIZONTAL );
      data.widthHint = STANDARD_WIDTH_FIELD;
      m_firstRowCombo.setLayoutData( data );
      m_firstRowCombo.addSelectionListener( new SelectionAdapter()
      {
        public void widgetSelected( SelectionEvent e )
        {
          Combo c = (Combo)e.widget;
          String item = c.getItem( c.getSelectionIndex() );
          FeatureTypeProperty ftp = m_ft.getProperty( item );
          TextFieldValidator validator = new TextFieldValidator( ftp );
          String test = validator.isValid( m_secondRowText.getText() );
          if( test != null )
          {
            Image image = getDisplay().getSystemImage( SWT.ICON_ERROR );
            m_errorLabel.setImage( image );
            m_errorMessage.setText( test );
            m_secondRowText.setFocus();
            m_secondRowText.setText( "Bitte eine Zahl eingeben" );
            m_secondRowText.selectAll();
            pack();
          }
          else
          {
            m_errorMessage.setText( "" );
            m_errorLabel.setImage( null );
            m_firstRowCombo.setFocus();
            //update model data
            PropertyName pn = new PropertyName( item );
            ( (PropertyIsCOMPOperation)m_operation ).setFirstExperssion( pn );
            Literal l = new Literal( m_secondRowText.getText().trim() );
            ( (PropertyIsCOMPOperation)m_operation ).setFirstExperssion( l );
          }
        }
      } );
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
          String item = m_firstRowCombo.getItem( m_firstRowCombo.getSelectionIndex() );
          FeatureTypeProperty ftp = m_ft.getProperty( item );
          if( item != null && ftp != null )
          {
            TextFieldValidator validator = new TextFieldValidator( ftp );
            String test = validator.isValid( m_secondRowText.getText() );
            if( test != null )
            {
              Image image = getDisplay().getSystemImage( SWT.ICON_ERROR );
              m_errorLabel.setImage( image );
              m_errorMessage.setText( test );
              m_secondRowText.setFocus();
              m_secondRowText.setText( "Bitte eine Zahl eingeben" );
              m_secondRowText.selectAll();
              pack();
            }
            else
            {
              m_errorMessage.setText( "" );
              m_errorLabel.setImage( null );
              m_firstRowCombo.setFocus();
            }
          }
        }
      } );
      //
      m_thirdRowText = new Text( this, SWT.FILL | SWT.READ_ONLY );
      m_thirdRowText.setText( "Wildcard: '" + ( (PropertyIsLikeOperation)m_operation ).getWildCard()
          + "'  SingleChar: '" + ( (PropertyIsLikeOperation)m_operation ).getSingleChar() + "'  Escape: '"
          + ( (PropertyIsLikeOperation)m_operation ).getEscapeChar() );
      GridData data3 = new GridData();
      data3.verticalSpan = 2;
      m_thirdRowText.setLayoutData( data3 );
      //Error panel
      m_errorLabel = new Label( this, SWT.NULL );
      m_errorMessage = new Text( this, SWT.FILL | SWT.READ_ONLY | SWT.MULTI );
      GridData data2 = new GridData();
      data2.widthHint = 150;
      data2.heightHint = 30;
      m_errorMessage.setLayoutData( data2 );
      m_errorMessage.setText( "" );

      FeatureTypeProperty[] properties = m_ft.getProperties();
      for( int i = 0; i < properties.length; i++ )
      {
        FeatureTypeProperty property = properties[i];
        if( SpecialPropertyMapper.isValidMapping( String.class.getName(), property.getType() ) )
          m_firstRowCombo.add( property.getName() );
      }

    }

  }

  class PropertyIsNullOperationComposite extends Composite
  {

    private Label m_firstRowLabel;

    private Combo m_fristRowCombo;

    public PropertyIsNullOperationComposite( Composite parent, int style )
    {
      super( parent, style );
      Expression expression = null;
      if( m_operation != null )
        expression = ( (PropertyIsNullOperation)m_operation ).getExpression();
      //TODO add new LiteralType's
      else if( m_operation == null )
        expression = new PropertyName( EMPTY_VALUE );

      //      String value = null;
      //      if( expression instanceof PropertyName )
      //        value = ( (PropertyName)expression ).getValue();
      //      if( expression instanceof Literal )
      //        value = ( (Literal)expression ).getValue();

      m_firstRowLabel = new Label( this, SWT.NULL );
      m_firstRowLabel.setText( expression.getExpressionName().trim() );
      m_fristRowCombo = new Combo( this, SWT.FILL | SWT.READ_ONLY );
      for( int i = 0; i < m_ft.getProperties().length; i++ )
      {
        FeatureTypeProperty ftp = m_ft.getProperties()[i];
        m_fristRowCombo.add( ftp.getName().trim() );
      }
      m_fristRowCombo.addSelectionListener( new SelectionListener()
      {
        public void widgetSelected( SelectionEvent e )
        {
          // empty
        }

        public void widgetDefaultSelected( SelectionEvent e )
        {
        // empty
        }
      } );
    }
  }

  class PropertyIsBetweenComposite extends Composite
  {
    public PropertyIsBetweenComposite( Composite parent, int style )
    {
      super( parent, style );
    }
  }

  protected class TextFieldValidator implements IInputValidator
  {
    private FeatureTypeProperty ftp = null;

    public TextFieldValidator( FeatureTypeProperty featureTypeProperty )
    {
      this.ftp = featureTypeProperty;
    }

    /**
     * @see org.eclipse.jface.dialogs.IInputValidator#isValid(java.lang.String)
     */
    public String isValid( String newText )
    {
      String type = this.ftp.getType();
      if( type != null )
      {
        try
        {
          if( type.equals( Double.class.getName() ) )
            Double.parseDouble( newText );
          if( type.equals( Float.class.getName() ) )
            Float.parseFloat( newText );
          if( type.equals( Integer.class.getName() ) )
            Integer.parseInt( newText );
        }
        catch( NumberFormatException e )
        {
          return "Format Fehler!";
        }
        return null;
      }

      return null;
    }
  }
}
