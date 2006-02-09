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
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.internal.ide.StatusUtil;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.ogc.gml.filterdialog.ShapeFileImportDialog;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.filterencoding.Expression;
import org.kalypsodeegree.filterencoding.Operation;
import org.kalypsodeegree.gml.GMLGeometry;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventProviderAdapter;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.filterencoding.ArithmeticExpression;
import org.kalypsodeegree_impl.filterencoding.BoundaryExpression;
import org.kalypsodeegree_impl.filterencoding.Literal;
import org.kalypsodeegree_impl.filterencoding.OperationDefines;
import org.kalypsodeegree_impl.filterencoding.PropertyIsBetweenOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyIsCOMPOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyIsLikeOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyIsNullOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyName;
import org.kalypsodeegree_impl.filterencoding.SpatialOperation;
import org.kalypsodeegree_impl.gml.GMLFactory;
import org.kalypsodeegree_impl.gml.schema.SpecialPropertyMapper;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author kuepfer
 */
public class FilterCompositeFactory extends ModellEventProviderAdapter
{
  private static FilterCompositeFactory m_factory = new FilterCompositeFactory();

  protected IFeatureType m_ft;

  protected Operation m_operation;

  private static final int STANDARD_WIDTH_FIELD = 150;

  public static final String EMPTY_VALUE = "-NULL-";

  private TreeSet m_allsupportedSpatialOps = new TreeSet();

  public static TreeSet m_supportedOperations;

  private TreeSet m_allSupportedCompOps = new TreeSet();

  private static IErrorMessageReciever m_errorMessageReciever;

  private FilterCompositeFactory( )
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
      // m_allSupportedCompOps.add( OperationDefines.getNameById( OperationDefines.PROPERTYISBETWEEN ) );
      m_allSupportedCompOps.add( OperationDefines.getNameById( OperationDefines.PROPERTYISGREATERTHAN ) );
      m_allSupportedCompOps.add( OperationDefines.getNameById( OperationDefines.PROPERTYISGREATERTHANOREQUALTO ) );
      m_allSupportedCompOps.add( OperationDefines.getNameById( OperationDefines.PROPERTYISLESSTHAN ) );
      m_allSupportedCompOps.add( OperationDefines.getNameById( OperationDefines.PROPERTYISLESSTHANOREQUALTO ) );
      // m_allSupportedCompOps.add( OperationDefines.getNameById( OperationDefines.PROPERTYISLIKE ) );
      // m_allSupportedCompOps.add( OperationDefines.getNameById( OperationDefines.PROPERTYISNULL ) );

    }
  }

  protected TreeSet getSupportedSpatialOperations( )
  {
    return m_allsupportedSpatialOps;
  }

  protected TreeSet getSupportedCOMPOperations( )
  {
    return m_allSupportedCompOps;
  }

  public static FilterCompositeFactory getInstance( IErrorMessageReciever errorMessageReciever, TreeSet supportedOperations )
  {
    m_supportedOperations = supportedOperations;
    m_errorMessageReciever = errorMessageReciever;
    return m_factory;
  }

  protected Operation getOperation( )
  {
    return m_operation;
  }

  public Composite createFilterElementComposite( Operation operation, Composite parent, IFeatureType ft )
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
        ((Group) parent).setText( "Eigenschaften-" + operatorName );
        c = new PropertyIsCOMPOperationComposite( parent, SWT.NULL );
      }
      else if( operation instanceof PropertyIsLikeOperation )
      {
        if( operatorName == null )
          operatorName = "Unbekannter IsLike Operator";
        ((Group) parent).setText( "Eigenschaften-" + operatorName );
        c = new PropertyIsLikeOperationComposite( parent, SWT.NULL );

      }
      else if( operation instanceof PropertyIsBetweenOperation )
      {
        if( operatorName == null )
          operatorName = "Unbekannter IsBetween Operator";
        ((Group) parent).setText( "Eigenschaften-" + operatorName );
        c = new PropertyIsBetweenComposite( parent, SWT.NULL );

      }
      else if( operation instanceof SpatialOperation )
      {
        if( operatorName == null )
          operatorName = "Unbekannter Spatial Operator";
        ((Group) parent).setText( "Eigenschaften-" + operatorName );
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

    private Button m_loadExternalGeom;

    public SpatialComposite( Composite parent, int style )
    {
      super( parent, style );

      GMLGeometry geometry = ((SpatialOperation) getOperation()).getGeometry();
      PropertyName propertyName = ((SpatialOperation) getOperation()).getPropertyName();
      String opsName = getOperation().getOperatorName();
      // Top-Group
      setLayout( new GridLayout( 2, false ) );
      GridData data1 = new GridData( GridData.FILL_HORIZONTAL );
      data1.widthHint = STANDARD_WIDTH_FIELD;
      // possible oprations (they have been initialized when calling the factory)
      m_supportedOpsLable = new Label( this, SWT.NULL );
      m_supportedOpsLable.setText( "Operation" );
      m_supportedOpsCombo = new Combo( this, SWT.FILL | SWT.DROP_DOWN );
      m_supportedOpsCombo.setLayoutData( data1 );
      String[] namesOps = null;
      if( m_supportedOperations == null )
      {
        namesOps = (String[]) getSupportedSpatialOperations().toArray( new String[getSupportedSpatialOperations().size()] );
      }
      else
        namesOps = (String[]) getSupportedSpatialOperations().toArray();
      m_supportedOpsCombo.setItems( namesOps );
      // set the selection to the current operation type, if not availabel a blank is selected by default (Combo)
      int j = ArrayUtils.indexOf( namesOps, opsName );
      m_supportedOpsCombo.select( j );
      m_supportedOpsCombo.addSelectionListener( new SelectionAdapter()
      {

        public void widgetSelected( SelectionEvent e )
        {
          String item = m_supportedOpsCombo.getItem( m_supportedOpsCombo.getSelectionIndex() );
          int newOperationId = OperationDefines.getIdByName( item );
          ((SpatialOperation) m_operation).setOperatorId( newOperationId );
          fireModellEvent( new ModellEvent( FilterCompositeFactory.this, ModellEvent.WIDGET_CHANGE ) );
        }
      } );
      // set Geometry
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
          String operationName = m_combo.getItem( m_combo.getSelectionIndex() );
          int operationID = OperationDefines.getIdByName( operationName );
          ((SpatialOperation) m_operation).setOperatorId( operationID );
          updateOperation( null );
        }
      } );

      IPropertyType[] properties = m_ft.getProperties();
      for( int i = 0; i < properties.length; i++ )
      {
        IPropertyType property = properties[i];
        if( GeometryUtilities.isGeometry( property ) )
          m_combo.add( property.getName().trim() );
      }
      // sets the availabel property active, if the property name does not match an emty entry is created
      int index = -1;
      if( propertyName != null )
      {
        index = ArrayUtils.indexOf( m_combo.getItems(), propertyName.getValue() );
        m_combo.select( index );

      }
      else
      {
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
      m_loadButton.setToolTipText( "Ermöglicht das laden eines neuen geometrischen Operators aus dem Zeichungs-Thema der Karte" );
      m_loadButton.setSelection( false );
      m_loadButton.addSelectionListener( new SelectionAdapter()
      {

        public void widgetSelected( SelectionEvent e )
        {
          if( m_loadButton.getSelection() )
          {
            m_scrabLayerCombo.setVisible( true );
            MessageDialog.openInformation( getShell(), "Unimplemented Action", "Select the geometry from the list!" );
            // dummy geometry, just for testing
            GM_Object geom = GeometryFactory.createGM_Point( 100, 200, KalypsoGisPlugin.getDefault().getCoordinatesSystem() );
            updateOperation( geom );
          }
          else
            m_scrabLayerCombo.setVisible( false );

        }
      } );
      m_scrabLayerCombo = new Combo( this, SWT.NULL );
      m_scrabLayerCombo.setItems( new String[] { "ScrabPolygon_1", "ScrabLine_2", "ScrabPoint_3" } );
      m_scrabLayerCombo.select( 0 );
      m_scrabLayerCombo.setVisible( false );
      m_loadExternalGeom = new Button( this, SWT.NONE );
      m_loadExternalGeom.setText( "Durchsuchen ..." );
      m_loadExternalGeom.setToolTipText( "Laden eines Geometry-Operatrors aus einer Datei aus dem Workspace" );
      m_loadExternalGeom.addSelectionListener( new SelectionAdapter()
      {
        /**
         * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
         */
        public void widgetSelected( SelectionEvent e )
        {
          GM_Object geom = null;
          ShapeFileImportDialog dialog = new ShapeFileImportDialog( getShell(), false );
          int open = dialog.open();
          if( open == Window.OK )
          {
            geom = dialog.getGeometry();
          }
          updateOperation( geom );
        }

      } );
    }

    private boolean updateOperation( GM_Object newGeometry )
    {
      SpatialOperation spatialOperation = (SpatialOperation) m_operation;
      GMLGeometry gml = null;
      try
      {
        GM_Object oldGeometry = spatialOperation.getGeometryLiteral();
        if( oldGeometry == null && newGeometry == null )
          return false;
        if( oldGeometry == null || !oldGeometry.equals( newGeometry ) )
        {
          gml = GMLFactory.createGMLGeometry( null, newGeometry );
          spatialOperation.setGeometry( gml );
          m_text.setText( gml.getName() );
        }
        PropertyName oldPropertyName = spatialOperation.getPropertyName();
        PropertyName newPropertyName = new PropertyName( m_combo.getItem( m_combo.getSelectionIndex() ) );
        ((SpatialOperation) m_operation).setProperty( newPropertyName );
      }
      catch( Exception e )
      {
        IStatus status = StatusUtil.newStatus( Status.WARNING, e.getLocalizedMessage(), e );
        ErrorDialog.openError( getShell(), "Fehler beim erstellen des Geometrie-Operators", e.getMessage(), status );
        return false;
      }
      fireModellEvent( new ModellEvent( FilterCompositeFactory.this, ModellEvent.WIDGET_CHANGE ) );
      return true;
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
      // This implementation is only for the Expression Literal.
      // TODO Functions and Arithmetics (according the OGC filterencoding Specs) are not supported!
      String opsName = m_operation.getOperatorName();
      Expression firstExpression = null;
      Expression secondExpression = null;
      if( m_operation != null )
      {
        firstExpression = ((PropertyIsCOMPOperation) m_operation).getFirstExpression();
        secondExpression = ((PropertyIsCOMPOperation) m_operation).getSecondExpression();
      }// is the case when an new (empty) filter operation is to be displayed
      else if( m_operation == null )
      {
        PropertyName propertyName = new PropertyName( FilterCompositeFactory.EMPTY_VALUE );
        Literal literal = new Literal( FilterCompositeFactory.EMPTY_VALUE );
        m_operation = new PropertyIsCOMPOperation( m_operation.getOperatorId(), propertyName, literal );
      }
      setLayout( new GridLayout( 2, false ) );
      GridData data1 = new GridData( GridData.FILL_HORIZONTAL );
      data1.widthHint = STANDARD_WIDTH_FIELD;
      // possible oprations (they have been initialized when calling the factory)
      m_supportedOpsLable = new Label( this, SWT.NULL );
      m_supportedOpsLable.setText( "Operation" );
      m_supportedOpsCombo = new Combo( this, SWT.FILL | SWT.DROP_DOWN | SWT.READ_ONLY );
      m_supportedOpsCombo.setLayoutData( data1 );
      String[] namesOps = null;
      if( m_supportedOperations == null )
      {
        namesOps = (String[]) getSupportedCOMPOperations().toArray( new String[getSupportedCOMPOperations().size()] );
      }
      else
        namesOps = (String[]) getSupportedCOMPOperations().toArray();
      int j = ArrayUtils.indexOf( namesOps, opsName );
      m_supportedOpsCombo.setItems( namesOps );
      m_supportedOpsCombo.select( j );
      m_supportedOpsCombo.addSelectionListener( new SelectionAdapter()
      {

        public void widgetSelected( SelectionEvent e )
        {
          String item = m_supportedOpsCombo.getItem( m_supportedOpsCombo.getSelectionIndex() );
          int newOperationId = OperationDefines.getIdByName( item );
          PropertyIsCOMPOperation comparisonOperation = (PropertyIsCOMPOperation) m_operation;
          comparisonOperation.setOperatorId( newOperationId );
          updateOperation( newOperationId );
        }

      } );
      m_firstRowLabel = new Label( this, SWT.FILL );
      m_firstRowCombo = new Combo( this, SWT.FILL | SWT.READ_ONLY );
      GridData data = new GridData( GridData.FILL_HORIZONTAL );
      data.widthHint = STANDARD_WIDTH_FIELD;
      m_firstRowCombo.setLayoutData( data );
      m_firstRowCombo.addSelectionListener( new SelectionAdapter()
      {
        public void widgetSelected( SelectionEvent e )
        {
          int selectionIndex = m_firstRowCombo.getSelectionIndex();
          String item = m_firstRowCombo.getItem( selectionIndex );
          IPropertyType ftp = m_ft.getProperty( item );
          String text = m_secondRowText.getText();
          TextFieldValidator validator = new TextFieldValidator( (IValuePropertyType) ftp );
          String test = validator.isValid( text );
          m_errorMessageReciever.setErrorMessage( test );
          updateOperation( m_operation.getOperatorId() );
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
          IPropertyType ftp = m_ft.getProperty( item );
          String text = m_secondRowText.getText();
          TextFieldValidator validator = new TextFieldValidator( (IValuePropertyType) ftp );
          m_errorMessageReciever.setErrorMessage( validator.isValid( text ) );
          int operatorId = m_operation.getOperatorId();
          updateOperation( operatorId );

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
          if( e.keyCode == SWT.CR )
          {
            String item = m_firstRowCombo.getItem( m_firstRowCombo.getSelectionIndex() );
            IPropertyType ftp = m_ft.getProperty( item );
            String text = m_secondRowText.getText();
            TextFieldValidator validator = new TextFieldValidator( (IValuePropertyType) ftp );
            m_errorMessageReciever.setErrorMessage( validator.isValid( text ) );
            int operatorId = m_operation.getOperatorId();
            updateOperation( operatorId );

          }

        }
      } );

      m_firstRowLabel.setText( "Feld" );
      m_secondRowLabel.setText( "Wert" );
      IPropertyType[] properties = m_ft.getProperties();
      for( int i = 0; i < properties.length; i++ )
      {
        final IPropertyType property = properties[i];
        if( property instanceof IValuePropertyType )
        {
          final Class valueClass = ((IValuePropertyType) property).getValueClass();
          if( SpecialPropertyMapper.isValidMapping( String.class, valueClass ) )
            m_firstRowCombo.add( property.getName() );
        }
      }
      if( firstExpression instanceof PropertyName && secondExpression instanceof Literal )
      {
        if( firstExpression != null && secondExpression != null )
        {
          String[] items = m_firstRowCombo.getItems();
          int index = ArrayUtils.indexOf( items, ((PropertyName) firstExpression).getValue() );
          if( index > 0 )
            m_firstRowCombo.select( index );
          String value = ((Literal) secondExpression).getValue();
          m_secondRowText.setText( value );
        }

        // m_firstRowLabel.setText( "Feld" );
        // m_secondRowLabel.setText( "Wert" );
        // m_secondRowText.setText( ( (Literal)secondExpression ).getValue() );
      }
      else if( firstExpression instanceof PropertyName && secondExpression instanceof ArithmeticExpression )
      {
        // not implemented
      }
    }// only for "expr1 instance PropertyName" and "expr2 instanceof Literal"

    private boolean updateOperation( int operationID )
    {
      PropertyIsCOMPOperation comparisonOperation = (PropertyIsCOMPOperation) m_operation;
      comparisonOperation.setOperatorId( operationID );
      String propertyName = m_firstRowCombo.getText().trim();
      String literalName = m_secondRowText.getText().trim();
      comparisonOperation.setFirstExperssion( new PropertyName( propertyName ) );
      comparisonOperation.setSecondExperssion( new Literal( literalName ) );
      fireModellEvent( new ModellEvent( FilterCompositeFactory.this, ModellEvent.WIDGET_CHANGE ) );
      return true;
    }

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

    private Text m_wildCard;

    private Label m_wildCardLabel;

    private Label m_singleCharLabel;

    private Text m_singleChar;

    private Label m_escpapeCharLabel;

    private Text m_escpapeChar;

    public PropertyIsLikeOperationComposite( Composite parent, int style )
    {
      super( parent, style );
      PropertyName firstExpression = ((PropertyIsLikeOperation) m_operation).getPropertyName();
      Literal secondExpression = ((PropertyIsLikeOperation) m_operation).getLiteral();
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
          String item = m_firstRowCombo.getItem( m_firstRowCombo.getSelectionIndex() );
          PropertyName propertyName = ((PropertyIsLikeOperation) m_operation).getPropertyName();
          if( propertyName == null )
            propertyName = new PropertyName( item );
          else
            propertyName.setValue( item );
          ((PropertyIsLikeOperation) m_operation).setPropertyName( propertyName );
          fireModellEvent( new ModellEvent( FilterCompositeFactory.this, ModellEvent.WIDGET_CHANGE ) );
        }
      } );
      IPropertyType[] properties = m_ft.getProperties();
      for( int i = 0; i < properties.length; i++ )
      {
        final IPropertyType property = properties[i];
        if( property instanceof IValuePropertyType )
        {
          final Class valueClass = ((IValuePropertyType) property).getValueClass();
          if( SpecialPropertyMapper.isValidMapping( String.class, valueClass ) )
            m_firstRowCombo.add( property.getName() );
        }
      }
      String value = firstExpression.getValue();
      int index = ArrayUtils.indexOf( m_firstRowCombo.getItems(), value );
      if( index >= 0 )
        m_firstRowCombo.select( index );
      else
        m_firstRowCombo.select( 0 );
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
          IPropertyType ftp = m_ft.getProperty( item );
          if( item != null && ftp != null )
          {
            TextFieldValidator validator = new TextFieldValidator( (IValuePropertyType) ftp );
            String test = validator.isValid( m_secondRowText.getText() );
            m_errorMessageReciever.setErrorMessage( test );
            String str = m_secondRowText.getText().trim();
            Literal literal = ((PropertyIsLikeOperation) m_operation).getLiteral();
            if( literal == null )
              literal = new Literal( str );
            else
              literal.setValue( str );
            ((PropertyIsLikeOperation) m_operation).setLiteral( literal );
            fireModellEvent( new ModellEvent( FilterCompositeFactory.this, ModellEvent.WIDGET_CHANGE ) );
          }
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
      m_wildCard.setText( String.valueOf( ((PropertyIsLikeOperation) m_operation).getWildCard() ) );
      m_singleCharLabel = new Label( parameterGroup, SWT.NONE );
      m_singleCharLabel.setText( "Einzelnes Zeichen:" );
      m_singleChar = new Text( parameterGroup, SWT.NONE | SWT.READ_ONLY );
      m_singleChar.setText( String.valueOf( ((PropertyIsLikeOperation) m_operation).getSingleChar() ) );
      m_escpapeCharLabel = new Label( parameterGroup, SWT.NONE );
      m_escpapeCharLabel.setText( "Escape Zeichen:" );
      m_escpapeChar = new Text( parameterGroup, SWT.NONE | SWT.READ_ONLY );
      m_escpapeChar.setText( String.valueOf( ((PropertyIsLikeOperation) m_operation).getEscapeChar() ) );

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
        expression = ((PropertyIsNullOperation) m_operation).getExpression();
      // TODO add new LiteralType's
      else if( m_operation == null )
        expression = new PropertyName( EMPTY_VALUE );

      // String value = null;
      // if( expression instanceof PropertyName )
      // value = ( (PropertyName)expression ).getValue();
      // if( expression instanceof Literal )
      // value = ( (Literal)expression ).getValue();

      m_firstRowLabel = new Label( this, SWT.NULL );
      m_firstRowLabel.setText( expression.getExpressionName().trim() );
      m_fristRowCombo = new Combo( this, SWT.FILL | SWT.READ_ONLY );
      for( int i = 0; i < m_ft.getProperties().length; i++ )
      {
        IPropertyType ftp = m_ft.getProperties()[i];
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
    private Label m_comboLabel;

    private Combo m_firstRowCombo;

    private Label m_comboLabel2;

    private Text m_secondRowText;

    private Label m_comboLabel3;

    private Text m_thirdRowText;

    public PropertyIsBetweenComposite( Composite parent, int style )
    {
      super( parent, style );
      PropertyName propertyName = ((PropertyIsBetweenOperation) m_operation).getPropertyName();
      Expression upperBoundary = ((PropertyIsBetweenOperation) m_operation).getUpperBoundary();
      Expression lowerBoundary = ((PropertyIsBetweenOperation) m_operation).getLowerBoundary();
      if( propertyName == null )
        propertyName = new PropertyName( EMPTY_VALUE );
      if( upperBoundary == null )
        upperBoundary = new BoundaryExpression( EMPTY_VALUE );
      if( lowerBoundary == null )
        lowerBoundary = new BoundaryExpression( EMPTY_VALUE );
      setLayout( new GridLayout( 2, false ) );
      GridData data1 = new GridData( GridData.FILL_HORIZONTAL );
      data1.widthHint = STANDARD_WIDTH_FIELD;
      m_comboLabel = new Label( this, SWT.NULL );
      m_comboLabel.setText( "Property Name" );
      m_firstRowCombo = new Combo( this, SWT.FILL | SWT.DROP_DOWN );
      GridData data = new GridData( GridData.FILL_HORIZONTAL );
      data.widthHint = STANDARD_WIDTH_FIELD;
      m_firstRowCombo.setLayoutData( data );
      m_firstRowCombo.addSelectionListener( new SelectionAdapter()
      {
        public void widgetSelected( SelectionEvent e )
        {
          String operationName = m_firstRowCombo.getItem( m_firstRowCombo.getSelectionIndex() );
          int operationID = OperationDefines.getIdByName( operationName );
          ((SpatialOperation) m_operation).setOperatorId( operationID );
          // updateOperation( null );
        }
      } );

      IPropertyType[] properties = m_ft.getProperties();
      for( int i = 0; i < properties.length; i++ )
      {
        IPropertyType property = properties[i];
        if( GeometryUtilities.isGeometry( property ) )
          m_firstRowCombo.add( property.getName().trim() );
      }
      String[] items = m_firstRowCombo.getItems();
      int index = ArrayUtils.indexOf( items, propertyName.getValue() );
      if( index >= 0 )
        m_firstRowCombo.select( index );
      else
        m_firstRowCombo.select( 0 );
      m_comboLabel2 = new Label( this, SWT.NULL );
      m_comboLabel2.setText( "Obere Grenze" );
      m_secondRowText = new Text( this, SWT.FILL );
      GridData data2 = new GridData( GridData.FILL_HORIZONTAL );
      data.widthHint = STANDARD_WIDTH_FIELD;
      m_secondRowText.setLayoutData( data2 );
      // m_secondRowText.setText();

      m_comboLabel3 = new Label( this, SWT.NULL );
      m_comboLabel3.setText( "Untere Grenze" );
      m_thirdRowText = new Text( this, SWT.FILL );
      GridData data3 = new GridData( GridData.FILL_HORIZONTAL );
      data.widthHint = STANDARD_WIDTH_FIELD;
      m_thirdRowText.setLayoutData( data3 );

    }
  }

  protected class TextFieldValidator implements IInputValidator
  {
    private IValuePropertyType i_ftp = null;

    public TextFieldValidator( IValuePropertyType featureTypeProperty )
    {
      i_ftp = featureTypeProperty;
    }

    /**
     * @see org.eclipse.jface.dialogs.IInputValidator#isValid(java.lang.String)
     */
    public String isValid( String newText )
    {
      Class clazz = i_ftp.getValueClass();
      if( clazz != null )
      {
        try
        {
          if( clazz == Double.class )
            Double.parseDouble( newText );
          if( clazz == Float.class )
            Float.parseFloat( newText );
          if( clazz == Integer.class )
            Integer.parseInt( newText );
          if( GeometryUtilities.isGeometry( i_ftp ) )
          {
            // TODO Christoph was macht das ?
            String geomString = clazz.getName().replaceAll( ".+\\.", "" );
            if( newText.equals( geomString ) )
            {
              return "Falscher Geometerie-Typ gewählt!";
            }
          }
          if( clazz == String.class )
          {
            if( newText == null || newText.length() == 0 )
              return "Das Werte Feld darf nicht leer sein, bitte Text eingeben";
          }
        }
        catch( NumberFormatException e )
        {
          return "Format Fehler! Es wird ein Wert vom Typ: " + clazz.getName().replaceAll( ".+\\.", "" ) + " erwartet";
        }
        return null;
      }

      return null;
    }
  }
}
