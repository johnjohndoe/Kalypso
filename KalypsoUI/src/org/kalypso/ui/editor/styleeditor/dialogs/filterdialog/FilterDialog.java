/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.editor.styleeditor.dialogs.filterdialog;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;

import javax.swing.event.EventListenerList;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableTreeViewer;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.kalypso.contribs.eclipse.core.resources.ProjectUtilities;
import org.kalypso.contribs.eclipse.ui.dialogs.KalypsoResourceSelectionDialog;
import org.kalypso.contribs.eclipse.ui.dialogs.ResourceSelectionValidator;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.styleeditor.MessageBundle;
import org.kalypso.ui.editor.styleeditor.panels.ComparisonFilterComboPanel;
import org.kalypso.ui.editor.styleeditor.panels.LogicalFilterComboPanel;
import org.kalypso.ui.editor.styleeditor.panels.SpatialOperationPanel;
import org.kalypsodeegree.filterencoding.ElseFilter;
import org.kalypsodeegree.filterencoding.Expression;
import org.kalypsodeegree.filterencoding.Filter;
import org.kalypsodeegree.filterencoding.Operation;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree_impl.filterencoding.AbstractFilter;
import org.kalypsodeegree_impl.filterencoding.BoundaryExpression;
import org.kalypsodeegree_impl.filterencoding.ComparisonOperation;
import org.kalypsodeegree_impl.filterencoding.ComplexFilter;
import org.kalypsodeegree_impl.filterencoding.FeatureFilter;
import org.kalypsodeegree_impl.filterencoding.FeatureId;
import org.kalypsodeegree_impl.filterencoding.Literal;
import org.kalypsodeegree_impl.filterencoding.LogicalOperation;
import org.kalypsodeegree_impl.filterencoding.OperationDefines;
import org.kalypsodeegree_impl.filterencoding.PropertyIsBetweenOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyIsCOMPOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyIsLikeOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyIsNullOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyName;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

public class FilterDialog extends Dialog implements ISelectionChangedListener
{

  private TableTreeViewer m_viewer;

  private FilterDialogTreeNode mRoot = null;

  private LogicalFilterComboPanel logicalCombo = null;

  private Label logicalButton = null;

  private ComparisonFilterComboPanel comparisonCombo = null;

  private Label compButton = null;

  private Label featureIdButton = null;

  private IFeatureType featureType = null;

  private Group configureGroup = null;

  private FilterDialogTreeNode currentNode = mRoot;

  private Label elseFilterButton = null;

  private Composite globalConfigureComposite = null;

  private Composite innerConfigureComposite = null;

  private Button validateFilterButton = null;

  private boolean isValidated = false;

  private Filter returnFilter = null;

  private Rule rule = null;

  private Label errorLabel = null;

  private EventListenerList listenerList = new EventListenerList();

  private Rule historyRule = null;

  private SpatialOperationPanel m_spatialCombo = null;

  boolean m_loadGeomSelection = false;

  boolean m_drawGeomSelection = false;

  private Label spatialButton;

  public FilterDialog( Shell parent, IFeatureType m_featureType, Rule m_rule )
  {
    super( parent );
    setFeatureType( m_featureType );
    setRule( m_rule );
    Filter filter = rule.getFilter();
    if( filter != null && filter instanceof AbstractFilter )
    {
      FilterDialogTreeNode filterTree = parseFilterIntoTree( (AbstractFilter) filter );
      if( filterTree != null )
      {
        mRoot = new FilterDialogTreeNode( "Filter                                                              ", FilterDialogTreeNode.ROOT_TYPE );
        mRoot.getParent().addNode( filterTree );
      }
    }
    historyRule = StyleFactory.createRule( rule.getSymbolizers(), rule.getName(), rule.getTitle(), rule.getAbstract(), rule.getLegendGraphic(), rule.getFilter(), rule.hasElseFilter(), rule.getMinScaleDenominator(), rule.getMaxScaleDenominator() );
  }

  @Override
  protected void configureShell( Shell shell )
  {
    super.configureShell( shell );
    shell.setText( MessageBundle.STYLE_EDITOR_FILTER );
    shell.setSize( 500, 450 );
  }

  @Override
  protected void okPressed( )
  {
    Object[] children = ((FilterDialogTreeNode) mRoot.getChildren()[0]).getChildren();
    if( children.length != 0 )
    {
      boolean validFilter = false;
      try
      {
        validFilter = validateFilter( children );
      }
      catch( FilterDialogException e1 )
      {
        m_viewer.setSelection( new StructuredSelection( e1.getError().getNode() ) );
        m_viewer.expandAll();
        errorLabel.setText( e1.getError().getFaultCode() );
      }
      if( validFilter )
      {
        returnFilter = generateFilter( children );
        if( returnFilter instanceof ElseFilter )
        {
          rule.setElseFilter( true );
          rule.setFilter( null );
        }
        else
        {
          rule.setFilter( getFilter() );
          rule.setElseFilter( false );
        }
        historyRule = StyleFactory.createRule( rule.getSymbolizers(), rule.getName(), rule.getTitle(), rule.getAbstract(), rule.getLegendGraphic(), rule.getFilter(), rule.hasElseFilter(), rule.getMinScaleDenominator(), rule.getMaxScaleDenominator() );
        fire();
        super.okPressed();
      }
    }
    // no filter -> set empty filter
    else
    {
      returnFilter = null;
      rule.setFilter( null );
      rule.setElseFilter( false );
      historyRule = StyleFactory.createRule( rule.getSymbolizers(), rule.getName(), rule.getTitle(), rule.getAbstract(), rule.getLegendGraphic(), rule.getFilter(), rule.hasElseFilter(), rule.getMinScaleDenominator(), rule.getMaxScaleDenominator() );
      fire();
      super.okPressed();
    }
  }

  @Override
  protected void cancelPressed( )
  {
    returnFilter = historyRule.getFilter();
    rule.setFilter( historyRule.getFilter() );
    rule.setElseFilter( historyRule.hasElseFilter() );
    FilterDialogTreeNode filterTree = parseFilterIntoTree( (AbstractFilter) historyRule.getFilter() );
    if( filterTree != null )
    {
      mRoot = new FilterDialogTreeNode( "Filter                                                              ", FilterDialogTreeNode.ROOT_TYPE );
      mRoot.getParent().addNode( filterTree );
    }
    fire();
    super.cancelPressed();
  }

  public void addFilterDialogListener( FilterDialogListener pl )
  {
    listenerList.add( FilterDialogListener.class, pl );
  }

  @Override
  protected Control createDialogArea( Composite parent )
  {
    returnFilter = null;
    Composite main = (Composite) super.createDialogArea( parent );
    main.setSize( 500, 300 );
    main.setLayout( new GridLayout( 1, true ) );
    main.layout();
    applyDialogFont( main );

    // **** TITLE/NAME of rule
    Label nameLabel = new Label( main, 0 );
    if( rule.getTitle() != null )
      nameLabel.setText( MessageBundle.STYLE_EDITOR_FILTER_FOR_RULE + rule.getTitle() );
    else
      nameLabel.setText( MessageBundle.STYLE_EDITOR_FILTER_FOR_RULE + rule.getName() );
    nameLabel.setFont( new Font( null, "Arial", 10, SWT.BOLD ) );

    // **** Titel
    Label titleLabel = new Label( main, SWT.NULL );
    titleLabel.setText( MessageBundle.STYLE_EDITOR_FILTER_AVAILABLE );
    titleLabel.setFont( new Font( null, "Arial", 8, SWT.BOLD ) );
    GridData titleLabelData = new GridData();
    titleLabelData.horizontalAlignment = GridData.CENTER;
    titleLabelData.verticalAlignment = GridData.END;
    titleLabel.setLayoutData( titleLabelData );

    // **** THIRD ROW - FUNCTION MENU
    Composite functionComposite = new Composite( main, SWT.NULL );
    functionComposite.setLayout( new GridLayout( 2, false ) );
    createOGCFilterPanel( functionComposite );
    createSLDFilterPanel( functionComposite );

    // **** FOURTH ROW
    Composite secondRowComposite = new Composite( main, SWT.NULL );
    secondRowComposite.setLayout( new GridLayout( 2, true ) );

    Label treeLabel = new Label( secondRowComposite, SWT.NULL );
    treeLabel.setText( MessageBundle.STYLE_EDITOR_FILTER_STRUKTUR );
    treeLabel.setFont( new Font( null, "Arial", 8, SWT.BOLD ) );
    GridData treeLabelData = new GridData();
    treeLabelData.horizontalAlignment = GridData.CENTER;
    treeLabelData.verticalAlignment = GridData.END;
    treeLabel.setLayoutData( treeLabelData );

    Label inputLabel = new Label( secondRowComposite, SWT.NULL );
    inputLabel.setText( MessageBundle.STYLE_EDITOR_FILTER_CONFIG );
    inputLabel.setFont( new Font( null, "Arial", 8, SWT.BOLD ) );
    GridData inputLabelData = new GridData();
    inputLabelData.horizontalAlignment = GridData.CENTER;
    inputLabelData.verticalAlignment = GridData.END;
    inputLabel.setLayoutData( inputLabelData );

    // **** FIFTH ROW - TREE
    final Tree tree = new Tree( secondRowComposite, SWT.SINGLE | SWT.FULL_SELECTION | SWT.H_SCROLL | SWT.BORDER );
    GridData tableTreeData = new GridData();
    tableTreeData.widthHint = 224;
    tableTreeData.heightHint = 97;
    tree.setLayoutData( tableTreeData );
    m_viewer = new TableTreeViewer( tree );
    m_viewer.addSelectionChangedListener( this );
    m_viewer.setContentProvider( new FilterDialogTreeContentProvider() );
    m_viewer.setLabelProvider( new FilterDialogLabelProvider() );
    if( mRoot == null )
      mRoot = new FilterDialogTreeNode( "Filter                                                              ", FilterDialogTreeNode.ROOT_TYPE );
    m_viewer.setInput( mRoot );
    m_viewer.setSelection( new StructuredSelection( mRoot.getParent() ) );
    m_viewer.expandAll();
    createContextMenu( m_viewer.getControl() );

    globalConfigureComposite = new Composite( secondRowComposite, SWT.NULL );
    globalConfigureComposite.setLayout( new GridLayout() );
    GridData globalConfigureCompositeData = new GridData();
    globalConfigureCompositeData.widthHint = 224;
    globalConfigureCompositeData.heightHint = 127;
    globalConfigureComposite.setLayoutData( globalConfigureCompositeData );

    innerConfigureComposite = new Composite( globalConfigureComposite, SWT.NULL );

    // **** THIRD ROW - SAVE FILTER
    Composite thirdRowComposite = new Composite( main, SWT.NULL );
    thirdRowComposite.setLayout( new GridLayout( 2, false ) );
    validateFilterButton = new Button( thirdRowComposite, SWT.NULL );
    validateFilterButton.setText( MessageBundle.STYLE_EDITOR_FILTER_VALIDATE );
    validateFilterButton.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        Object[] children = ((FilterDialogTreeNode) getMRoot().getChildren()[0]).getChildren();
        if( children.length != 0 )
        {
          // generate Filter
          if( isValidated() )
          {
            getErrorLabel().setText( "" );
            getValidateFilterButton().setText( MessageBundle.STYLE_EDITOR_FILTER_APPLY );
            setReturnFilter( generateFilter( children ) );
            if( getReturnFilter() instanceof ElseFilter )
            {
              getRule().setElseFilter( true );
              getRule().setFilter( null );
            }
            else
            {
              getRule().setFilter( getFilter() );
              getRule().setElseFilter( false );
            }
            fire();
          }
          else
          {
            boolean validFilter = false;
            try
            {
              validFilter = validateFilter( children );
            }
            catch( FilterDialogException e1 )
            {
              getM_viewer().setSelection( new StructuredSelection( e1.getError().getNode() ) );
              getM_viewer().expandAll();
              getErrorLabel().setText( e1.getError().getFaultCode() );
            }

            if( validFilter )
            {
              getValidateFilterButton().setText( MessageBundle.STYLE_EDITOR_FILTER_APPLY );
              setValidated( true );
            }
          }
        }
        // no filter -> set empty filter
        else
        {
          if( isValidated() )
          {
            getErrorLabel().setText( "" );
            getValidateFilterButton().setText( MessageBundle.STYLE_EDITOR_FILTER_APPLY );
            setReturnFilter( null );
            getRule().setFilter( null );
            getRule().setElseFilter( false );
            fire();
          }
          else
          {
            getValidateFilterButton().setText( MessageBundle.STYLE_EDITOR_FILTER_APPLY );
            setValidated( true );
          }
        }
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );

    errorLabel = new Label( thirdRowComposite, SWT.NULL );
    GridData errorLabelData = new GridData();
    errorLabelData.widthHint = 350;
    errorLabel.setLayoutData( errorLabelData );

    return main;
  }

  private FilterDialogTreeNode parseFilterIntoTree( AbstractFilter filter )
  {
    if( filter instanceof FeatureFilter )
    {
      FeatureFilter featureFilter = (FeatureFilter) filter;
      ArrayList featureIds = featureFilter.getFeatureIds();
      FilterDialogTreeNode tmpNode = null;
      FeatureIDData tmpData = null;
      for( int i = 0; i < featureIds.size(); i++ )
      {
        tmpNode = new FilterDialogTreeNode( "FT_ID", FilterDialogTreeNode.FEATUREID_NODE_TYPE );
        tmpData = new FeatureIDData();
        tmpData.setFeatureId( featureIds.get( i ).toString() );
        tmpNode.setData( tmpData );
      }
      return tmpNode;
    }
    else if( filter instanceof ComplexFilter )
    {
      ComplexFilter complexFilter = (ComplexFilter) filter;
      Operation operation = complexFilter.getOperation();
      if( operation instanceof LogicalOperation )
      {
        LogicalOperation logOp = (LogicalOperation) operation;
        FilterDialogTreeNode tmpNode = null;
        switch( logOp.getOperatorId() )
        {
          case OperationDefines.AND:
          {
            tmpNode = new FilterDialogTreeNode( "AND", FilterDialogTreeNode.LOGICAL_NODE_TYPE );
            break;
          }
          case OperationDefines.OR:
          {
            tmpNode = new FilterDialogTreeNode( "OR", FilterDialogTreeNode.LOGICAL_NODE_TYPE );
            break;
          }
          case OperationDefines.NOT:
          {
            tmpNode = new FilterDialogTreeNode( "NOT", FilterDialogTreeNode.LOGICAL_NODE_TYPE );
            break;
          }
        }
        // ArrayList with Operations (logic, comp, spatial)
        ArrayList arguments = logOp.getArguments();
        for( int i = 0; i < arguments.size(); i++ )
        {
          ComplexFilter tmpFilter = new ComplexFilter( (Operation) arguments.get( i ) );
          tmpNode.addNode( parseFilterIntoTree( tmpFilter ) );
        }
        return tmpNode;
      }
      else if( operation instanceof ComparisonOperation )
      {
        ComparisonOperation compOp = (ComparisonOperation) operation;
        FilterDialogTreeNode tmpNode = null;
        switch( compOp.getOperatorId() )
        {
          case OperationDefines.PROPERTYISLIKE:
          {
            tmpNode = new FilterDialogTreeNode( "LIKE", FilterDialogTreeNode.COMPARISON_NODE_TYPE );
            PropertyIsLikeOperation isLikeOp = (PropertyIsLikeOperation) compOp;
            LikeComparisonData data = new LikeComparisonData();
            data.setLiteral( isLikeOp.getLiteral().getValue() );
            data.setPropertyName( isLikeOp.getPropertyName().getValue() );
            data.setEscapeChar( isLikeOp.getEscapeChar() );
            data.setSingleChar( isLikeOp.getSingleChar() );
            data.setWildCard( isLikeOp.getWildCard() );
            tmpNode.setData( data );
            break;
          }
          case OperationDefines.PROPERTYISNULL:
          {
            tmpNode = new FilterDialogTreeNode( "NULL", FilterDialogTreeNode.COMPARISON_NODE_TYPE );
            PropertyIsNullOperation isNullOp = (PropertyIsNullOperation) compOp;
            NullComparisonData data = new NullComparisonData();
            Expression exp = isNullOp.getExpression();
            if( exp instanceof PropertyName )
              data.setPropertyName( ((PropertyName) exp).getValue() );
            tmpNode.setData( data );
            break;
          }
          case OperationDefines.PROPERTYISBETWEEN:
          {
            tmpNode = new FilterDialogTreeNode( "BETWEEN", FilterDialogTreeNode.COMPARISON_NODE_TYPE );
            PropertyIsBetweenOperation isBetweenOp = (PropertyIsBetweenOperation) compOp;
            BetweenComparisonData data = new BetweenComparisonData();
            data.setPropertyName( isBetweenOp.getPropertyName().getValue() );
            data.setLower( ((BoundaryExpression) isBetweenOp.getLowerBoundary()).getValue() );
            data.setUpper( ((BoundaryExpression) isBetweenOp.getUpperBoundary()).getValue() );
            tmpNode.setData( data );
            break;
          }
          case OperationDefines.PROPERTYISEQUALTO:
          {
            tmpNode = new FilterDialogTreeNode( "EQUAL_TO", FilterDialogTreeNode.COMPARISON_NODE_TYPE );
            PropertyIsCOMPOperation isCompOp = (PropertyIsCOMPOperation) compOp;
            BinaryComparisonData data = new BinaryComparisonData();
            Expression exp = isCompOp.getFirstExpression();
            if( exp instanceof PropertyName )
              data.setPropertyName( ((PropertyName) exp).getValue() );
            exp = isCompOp.getSecondExpression();
            if( exp instanceof Literal )
              data.setLiteral( ((Literal) exp).getValue() );
            tmpNode.setData( data );
            break;
          }
          case OperationDefines.PROPERTYISLESSTHAN:
          {
            tmpNode = new FilterDialogTreeNode( "LESS_THAN", FilterDialogTreeNode.COMPARISON_NODE_TYPE );
            PropertyIsCOMPOperation isCompOp = (PropertyIsCOMPOperation) compOp;
            BinaryComparisonData data = new BinaryComparisonData();
            Expression exp = isCompOp.getFirstExpression();
            if( exp instanceof PropertyName )
              data.setPropertyName( ((PropertyName) exp).getValue() );
            exp = isCompOp.getSecondExpression();
            if( exp instanceof Literal )
              data.setLiteral( ((Literal) exp).getValue() );
            tmpNode.setData( data );
            break;
          }
          case OperationDefines.PROPERTYISGREATERTHAN:
          {
            tmpNode = new FilterDialogTreeNode( "GREATER_THAN", FilterDialogTreeNode.COMPARISON_NODE_TYPE );
            PropertyIsCOMPOperation isCompOp = (PropertyIsCOMPOperation) compOp;
            BinaryComparisonData data = new BinaryComparisonData();
            Expression exp = isCompOp.getFirstExpression();
            if( exp instanceof PropertyName )
              data.setPropertyName( ((PropertyName) exp).getValue() );
            exp = isCompOp.getSecondExpression();
            if( exp instanceof Literal )
              data.setLiteral( ((Literal) exp).getValue() );
            tmpNode.setData( data );
            break;
          }
          case OperationDefines.PROPERTYISLESSTHANOREQUALTO:
          {
            tmpNode = new FilterDialogTreeNode( "LESS_THAN_OR_EQUAL_TO", FilterDialogTreeNode.COMPARISON_NODE_TYPE );
            PropertyIsCOMPOperation isCompOp = (PropertyIsCOMPOperation) compOp;
            BinaryComparisonData data = new BinaryComparisonData();
            Expression exp = isCompOp.getFirstExpression();
            if( exp instanceof PropertyName )
              data.setPropertyName( ((PropertyName) exp).getValue() );
            exp = isCompOp.getSecondExpression();
            if( exp instanceof Literal )
              data.setLiteral( ((Literal) exp).getValue() );
            tmpNode.setData( data );
            break;
          }
          case OperationDefines.PROPERTYISGREATERTHANOREQUALTO:
          {
            tmpNode = new FilterDialogTreeNode( "GREATER_THAN_OR_EQUAL_TO", FilterDialogTreeNode.COMPARISON_NODE_TYPE );
            PropertyIsCOMPOperation isCompOp = (PropertyIsCOMPOperation) compOp;
            BinaryComparisonData data = new BinaryComparisonData();
            Expression exp = isCompOp.getFirstExpression();
            if( exp instanceof PropertyName )
              data.setPropertyName( ((PropertyName) exp).getValue() );
            exp = isCompOp.getSecondExpression();
            if( exp instanceof Literal )
              data.setLiteral( ((Literal) exp).getValue() );
            tmpNode.setData( data );
            break;
          }
        }
        return tmpNode;
      }
    }
    return null;
  }

  Filter generateFilter( Object[] children )
  {
    if( ((FilterDialogTreeNode) children[0]).getType() == FilterDialogTreeNode.FEATUREID_NODE_TYPE )
    {
      FeatureFilter filter = new FeatureFilter();
      for( int i = 0; i < children.length; i++ )
      {
        FilterDialogTreeNode currentChild = (FilterDialogTreeNode) children[i];
        if( currentChild.getType() == FilterDialogTreeNode.FEATUREID_NODE_TYPE )
        {
          FeatureIDData data = (FeatureIDData) currentChild.getData();
          filter.addFeatureId( new FeatureId( data.getFeatureId() ) );
        }
      }
      return filter;
    }
    else if( ((FilterDialogTreeNode) children[0]).getType() == FilterDialogTreeNode.ELSEFILTER_TYPE )
    {
      return new ElseFilter();
    }
    else if( ((FilterDialogTreeNode) children[0]).getType() == FilterDialogTreeNode.COMPARISON_NODE_TYPE )
    {
      FilterDialogTreeNode child = (FilterDialogTreeNode) children[0];

      switch( child.getSubType() )
      {
        case FilterDialogTreeNode.COMPARISON_BETWEEN:
        {
          BetweenComparisonData data = (BetweenComparisonData) child.getData();
          BoundaryExpression upperBoundary = new BoundaryExpression( data.getUpper() );
          BoundaryExpression lowerBoundary = new BoundaryExpression( data.getLower() );
          PropertyName propertyName = new PropertyName( data.getPropertyName() );
          PropertyIsBetweenOperation operation = new PropertyIsBetweenOperation( propertyName, lowerBoundary, upperBoundary );
          return new ComplexFilter( operation );
        }
        case FilterDialogTreeNode.COMPARISON_LIKE:
        {
          LikeComparisonData data = (LikeComparisonData) child.getData();
          PropertyName propertyName = new PropertyName( data.getPropertyName() );
          Literal literal = new Literal( data.getLiteral() );
          PropertyIsLikeOperation operation = new PropertyIsLikeOperation( propertyName, literal, data.getWildCard(), data.getSingleChar(), data.getEscapeChar() );
          return new ComplexFilter( operation );
        }
        case FilterDialogTreeNode.COMPARISON_NULL:
        {
          NullComparisonData data = (NullComparisonData) child.getData();
          PropertyName propertyName = new PropertyName( data.getPropertyName() );
          PropertyIsNullOperation operation = new PropertyIsNullOperation( propertyName );
          return new ComplexFilter( operation );
        }
        case FilterDialogTreeNode.COMPARISON_EQUALTO:
        case FilterDialogTreeNode.COMPARISON_LESSTHAN:
        case FilterDialogTreeNode.COMPARISON_GREATERTHAN:
        case FilterDialogTreeNode.COMPARISON_LESSTHANOREQUALTO:
        case FilterDialogTreeNode.COMPARISON_GREATERTHANOREQUALTO:
        {
          Literal literal = null;
          PropertyName propertyName = null;

          if( child.getData() instanceof BinaryComparisonData )
          {
            BinaryComparisonData data = (BinaryComparisonData) child.getData();
            propertyName = new PropertyName( data.getPropertyName() );
            literal = new Literal( data.getLiteral() );
          }
          else if( child.getData() instanceof BinaryComparisonNumericData )
          {
            BinaryComparisonNumericData data = (BinaryComparisonNumericData) child.getData();
            propertyName = new PropertyName( data.getPropertyName() );
            literal = new Literal( data.getLiteral() );
          }
          PropertyIsCOMPOperation operation = new PropertyIsCOMPOperation( child.getSubType(), propertyName, literal );
          return new ComplexFilter( operation );
        }
      }
    }
    else if( ((FilterDialogTreeNode) children[0]).getType() == FilterDialogTreeNode.SPATIAL_NODE_TYPE )
    {
      FilterDialogTreeNode child = (FilterDialogTreeNode) children[0];
      if( child.getSubType() == FilterDialogTreeNode.SPATIAL_INTERSECTS )
      {

      }

    }
    else if( ((FilterDialogTreeNode) children[0]).getType() == FilterDialogTreeNode.LOGICAL_NODE_TYPE )
    {
      FilterDialogTreeNode child = (FilterDialogTreeNode) children[0];
      if( child.getSubType() == FilterDialogTreeNode.LOCICAL_NOT )
      {
        ArrayList<Operation> arguments = new ArrayList<Operation>();
        Object[] innerElement = { ((FilterDialogTreeNode) child.getChildren()[0]) };
        Filter filter = generateFilter( innerElement );
        if( filter instanceof ComplexFilter )
        {
          arguments.add( ((ComplexFilter) filter).getOperation() );
        }
        LogicalOperation operation = new LogicalOperation( OperationDefines.NOT, arguments );
        return new ComplexFilter( operation );
      }
      else if( child.getSubType() == FilterDialogTreeNode.LOCICAL_AND )
      {
        ArrayList<Operation> arguments = new ArrayList<Operation>();
        for( int i = 0; i < child.getChildren().length; i++ )
        {
          Object[] innerElement = { ((FilterDialogTreeNode) child.getChildren()[i]) };
          Filter filter = generateFilter( innerElement );
          if( filter instanceof ComplexFilter )
          {
            arguments.add( ((ComplexFilter) filter).getOperation() );
          }
        }
        LogicalOperation operation = new LogicalOperation( OperationDefines.AND, arguments );
        return new ComplexFilter( operation );
      }
      else if( child.getSubType() == FilterDialogTreeNode.LOCICAL_OR )
      {
        ArrayList<Operation> arguments = new ArrayList<Operation>();
        for( int i = 0; i < child.getChildren().length; i++ )
        {
          Object[] innerElement = { ((FilterDialogTreeNode) child.getChildren()[i]) };
          Filter filter = generateFilter( innerElement );
          if( filter instanceof ComplexFilter )
          {
            arguments.add( ((ComplexFilter) filter).getOperation() );
          }
        }
        LogicalOperation operation = new LogicalOperation( OperationDefines.OR, arguments );
        return new ComplexFilter( operation );
      }

    }
    return null;
  }

  // parses the Filter to check whether valid structure
  boolean validateFilter( Object[] children ) throws FilterDialogException
  {
    // element has to have at least one child
    if( children.length == 0 )
      return false;

    if( ((FilterDialogTreeNode) children[0]).getType() == FilterDialogTreeNode.FEATUREID_NODE_TYPE )
    {
      // check every FeatureIDElement whether valid
      for( int i = 0; i < children.length; i++ )
      {
        FilterDialogTreeNode currentChild = (FilterDialogTreeNode) children[i];
        // if one is not valid
        if( !currentChild.validate() )
          return false;
      }
      return true;
    }
    else if( ((FilterDialogTreeNode) children[0]).getType() == FilterDialogTreeNode.ELSEFILTER_TYPE )
    {
      return true;
    }
    else if( ((FilterDialogTreeNode) children[0]).getType() == FilterDialogTreeNode.COMPARISON_NODE_TYPE )
    {
      FilterDialogTreeNode currentChild = (FilterDialogTreeNode) children[0];
      if( !currentChild.validate() )
        return false;
      return true;
    }
    // ck 28.7.05
    else if( ((FilterDialogTreeNode) children[0]).getType() == FilterDialogTreeNode.SPATIAL_NODE_TYPE )
    {
      FilterDialogTreeNode currentChild = (FilterDialogTreeNode) children[0];
      if( !currentChild.validate() )
        return false;
      return true;
    }
    else if( ((FilterDialogTreeNode) children[0]).getType() == FilterDialogTreeNode.LOGICAL_NODE_TYPE )
    {
      FilterDialogTreeNode child = (FilterDialogTreeNode) children[0];
      if( child.getSubType() == FilterDialogTreeNode.LOCICAL_NOT )
      {
        boolean evaluation = false;
        // NOT needs to have exactly one child
        if( child.getChildren().length != 1 )
          throw new FilterDialogException( new FilterDialogError( child, MessageBundle.STYLE_EDITOR_FILTER_ERROR_CHILD ) );

        if( ((FilterDialogTreeNode) child.getChildren()[0]).getType() == FilterDialogTreeNode.LOGICAL_NODE_TYPE )
        {
          FilterDialogTreeNode[] tmpArray = { (FilterDialogTreeNode) child.getChildren()[0] };
          evaluation = validateFilter( tmpArray );
          if( !evaluation )
            throw new FilterDialogException( new FilterDialogError( (FilterDialogTreeNode) child.getChildren()[0], MessageBundle.STYLE_EDITOR_FILTER_ERROR_CHILD ) );
        }
        else
        {
          if( !((FilterDialogTreeNode) child.getChildren()[0]).validate() )
            return false;
        }
        return true;
      }
      else if( child.getSubType() == FilterDialogTreeNode.LOCICAL_AND )
      {
        boolean evaluation = false;
        // AND needs to have more than 1 child
        if( child.getChildren().length < 2 )
          throw new FilterDialogException( new FilterDialogError( child, MessageBundle.STYLE_EDITOR_FILTER_ERROR_CHILDREN ) );
        for( int i = 0; i < child.getChildren().length; i++ )
        {
          if( ((FilterDialogTreeNode) child.getChildren()[i]).getType() == FilterDialogTreeNode.LOGICAL_NODE_TYPE )
          {
            FilterDialogTreeNode[] tmpArray = { (FilterDialogTreeNode) child.getChildren()[i] };
            evaluation = validateFilter( tmpArray );
            if( !evaluation )
              return false;
          }
          else if( !((FilterDialogTreeNode) child.getChildren()[i]).validate() )
            return false;
        }
        return true;
      }
      else if( child.getSubType() == FilterDialogTreeNode.LOCICAL_OR )
      {
        boolean evaluation = false;
        // OR needs to have more than 1 child
        if( child.getChildren().length < 2 )
          throw new FilterDialogException( new FilterDialogError( child, MessageBundle.STYLE_EDITOR_FILTER_ERROR_CHILDREN ) );
        for( int i = 0; i < child.getChildren().length; i++ )
        {
          if( ((FilterDialogTreeNode) child.getChildren()[i]).getType() == FilterDialogTreeNode.LOGICAL_NODE_TYPE )
          {
            FilterDialogTreeNode[] tmpArray = { (FilterDialogTreeNode) child.getChildren()[i] };
            evaluation = validateFilter( tmpArray );
            if( !evaluation )
              return false;
          }
          else if( !((FilterDialogTreeNode) child.getChildren()[i]).validate() )
            return false;
        }
        return true;
      }
    }
    return false;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void selectionChanged( SelectionChangedEvent event )
  {
    TableTreeViewer ttv = (TableTreeViewer) event.getSource();
    IStructuredSelection s = (IStructuredSelection) ttv.getSelection();
    if( s.getFirstElement() instanceof FilterDialogTreeNode )
    {
      currentNode = (FilterDialogTreeNode) s.getFirstElement();
      Object[] children = currentNode.getChildren();
      if( globalConfigureComposite != null && !globalConfigureComposite.isDisposed() )
        globalConfigureComposite.setVisible( false );
      // always disabled -> only root enables it if nothing added yet
      disableElseFilter();

      switch( currentNode.getType() )
      {
        case FilterDialogTreeNode.ROOT_TYPE:

        {
          // no first element -> all options available
          if( children.length == 0 )
          {
            enableComparisonOperations();
            enableFeatureOperations();
            enableLogicalOperations();
            enableElseFilter();
            enableSpatialOperations();
          }
          // first element: featureId -> only featureId available
          else if( ((FilterDialogTreeNode) children[0]).getType() == FilterDialogTreeNode.FEATUREID_NODE_TYPE )
          {
            disableComparisonOperations();
            enableFeatureOperations();
            disableLogicalOperations();
            disableSpatialOperations();
          }
          // first element: logic or comparision -> no options
          else
          {
            disableComparisonOperations();
            disableFeatureOperations();
            disableLogicalOperations();
            disableSpatialOperations();
          }
          break;
        }
        case FilterDialogTreeNode.ELSEFILTER_TYPE:
        {
          disableComparisonOperations();
          disableFeatureOperations();
          disableLogicalOperations();
          disableSpatialOperations();
          break;
        }
        case FilterDialogTreeNode.LOGICAL_NODE_TYPE:
        {
          // no feature id
          disableFeatureOperations();
          // if NOT -> only one child possible
          if( currentNode.getSubType() == FilterDialogTreeNode.LOCICAL_NOT )
          {
            if( children.length == 0 )
            {
              enableComparisonOperations();
              enableLogicalOperations();
              enableSpatialOperations();
            }
            else
            {
              disableComparisonOperations();
              disableLogicalOperations();
              enableSpatialOperations();
            }
          }
          // else (AND,OR)-> unbounded number of children possible
          else
          {
            enableComparisonOperations();
            enableLogicalOperations();
            enableSpatialOperations();
          }
          break;
        }
        case FilterDialogTreeNode.COMPARISON_NODE_TYPE:
        {
          disableComparisonOperations();
          disableFeatureOperations();
          disableLogicalOperations();
          disableSpatialOperations();
          if( FilterDialogTreeNode.isBinaryComparisonType( currentNode.getSubType() ) )
            drawBinaryComparisonOpTypeGroup( currentNode.getData(), currentNode.getSubType() );
          else if( currentNode.getSubType() == FilterDialogTreeNode.COMPARISON_BETWEEN )
            drawPropertyIsBetweenTypeGroup( (BetweenComparisonData) currentNode.getData() );
          else if( currentNode.getSubType() == FilterDialogTreeNode.COMPARISON_LIKE )
            drawPropertyIsLikeTypeGroup( (LikeComparisonData) currentNode.getData() );
          else if( currentNode.getSubType() == FilterDialogTreeNode.COMPARISON_NULL )
            drawPropertyIsNullTypeGroup( (NullComparisonData) currentNode.getData() );
          break;
        }
        case FilterDialogTreeNode.FEATUREID_NODE_TYPE:
        {
          disableComparisonOperations();
          disableFeatureOperations();
          disableLogicalOperations();
          disableSpatialOperations();
          drawFeatureIdTypeGroup( (FeatureIDData) currentNode.getData() );
          break;
        }
        case FilterDialogTreeNode.SPATIAL_NODE_TYPE:
        {
          enableComparisonOperations();
          disableFeatureOperations();
          enableLogicalOperations();
          drawSpatialOpsTypeGroup( currentNode.getData(), currentNode.getSubType() );

        }

        default:
        {
          System.out.println( "Error selectionChanged in FilterDialog" );
        }
      }
    }
  }

  /**
   *  
   */
  private void enableSpatialOperations( )
  {
    m_spatialCombo.enable();
    spatialButton.setVisible( true );
  }

  /**
   *  
   */
  private void disableSpatialOperations( )
  {
    m_spatialCombo.disable();
    spatialButton.setVisible( false );
  }

  private void disableLogicalOperations( )
  {
    logicalCombo.disable();
    logicalButton.setVisible( false );
  }

  private void enableLogicalOperations( )
  {
    logicalCombo.enable();
    logicalButton.setVisible( true );
  }

  private void disableComparisonOperations( )
  {
    comparisonCombo.disable();
    compButton.setVisible( false );
  }

  private void enableComparisonOperations( )
  {
    comparisonCombo.enable();
    compButton.setVisible( true );
  }

  private void disableFeatureOperations( )
  {
    featureIdButton.setVisible( false );
  }

  private void enableFeatureOperations( )
  {
    featureIdButton.setVisible( true );
  }

  private void disableElseFilter( )
  {
    if( elseFilterButton != null )
      elseFilterButton.setVisible( false );
  }

  private void enableElseFilter( )
  {
    if( elseFilterButton != null )
      elseFilterButton.setVisible( true );
  }

  private void createContextMenu( Control menuControl )
  {
    MenuManager menuMgr = new MenuManager( "#PopUp" );
    menuMgr.setRemoveAllWhenShown( true );
    menuMgr.addMenuListener( new IMenuListener()
    {
      public void menuAboutToShow( IMenuManager manager )
      {
        manager.add( new MenuAction( MessageBundle.STYLE_EDITOR_DELETE ) );
      }
    } );

    Menu menu = menuMgr.createContextMenu( menuControl );
    menuControl.setMenu( menu );
  }

  private void drawSpatialOpsTypeGroup( AbstractData data, final int subType )
  {
    String propertyName = null;

    if( data instanceof BinarySpatialData )
    {
      propertyName = ((BinarySpatialData) data).getGeometryPropertyName();
      ((BinarySpatialData) data).getGeomType();
    }
    else if( data instanceof BBoxSpatialData )
    {
      propertyName = ((BBoxSpatialData) data).getGeometryPropertyName();
      ((BBoxSpatialData) data).getGeomType();
    }
    if( innerConfigureComposite != null )
      innerConfigureComposite.dispose();
    innerConfigureComposite = new Composite( globalConfigureComposite, SWT.NULL );
    innerConfigureComposite.setLayout( new GridLayout() );
    GridData formData = new GridData( 224, 127 );
    innerConfigureComposite.setLayoutData( formData );

    // **** Configuration
    configureGroup = new Group( innerConfigureComposite, SWT.NULL );
    configureGroup.setLayout( new GridLayout( 2, false ) );
    configureGroup.setText( MessageBundle.STYLE_EDITOR_FILTER_CONFIG_FILTER );
    GridData groupCompositeData = new GridData();
    groupCompositeData.widthHint = 200;
    groupCompositeData.heightHint = 100;
    configureGroup.setLayoutData( groupCompositeData );

    Label propertyLabel = new Label( configureGroup, SWT.NULL );
    propertyLabel.setText( MessageBundle.STYLE_EDITOR_GEOM_PROPERTY );
    final Combo propertyNameCombo = new Combo( configureGroup, SWT.NULL );
    GridData propertyNameComboData = new GridData( 75, 30 );
    propertyNameCombo.setLayoutData( propertyNameComboData );
    // get all PropertyNames to use for filter
    ArrayList<String> labelStringItems = new ArrayList<String>();
    IPropertyType[] ftp = featureType.getProperties();
    for( int i = 0; i < ftp.length; i++ )
    {
      IPropertyType geomProperty = ftp[i];
      if( GeometryUtilities.isGeometry( geomProperty ) )
        labelStringItems.add( geomProperty.getName() );
    }
    final String[] items = new String[labelStringItems.size()];
    for( int j = 0; j < items.length; j++ )
      items[j] = labelStringItems.get( j );
    propertyNameCombo.setItems( items );
    propertyNameCombo.setText( "..." );
    Button geomMapButton = new Button( configureGroup, SWT.RADIO );
    geomMapButton.setSelection( false );
    geomMapButton.setText( MessageBundle.STYLE_EDITOR_GEOMETRY_IN_MAP );
    geomMapButton.addSelectionListener( new SelectionListener()
    {

      public void widgetSelected( SelectionEvent e )
      {
        Object o = e.widget;
        if( o instanceof Button )
        {
          Button radio = (Button) o;
          m_drawGeomSelection = radio.getSelection();
          if( m_drawGeomSelection )
            MessageDialog.openInformation( getShell(), "Dummy Dialog", "Draw a Geometry in the Map" );
        }
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );

      }
    } );

    Button geomFileButton = new Button( configureGroup, SWT.RADIO );
    geomFileButton.setSelection( false );
    geomFileButton.setText( MessageBundle.STYLE_EDITOR_CHOOSE_EXTERNAL_GEOMETRY );
    geomFileButton.addSelectionListener( new SelectionListener()
    {

      public void widgetSelected( SelectionEvent e )
      {
        Object o = e.widget;
        if( o instanceof Button )
        {
          Button radio = (Button) o;
          m_loadGeomSelection = radio.getSelection();
          if( m_loadGeomSelection )
          {
            IProject project = ProjectUtilities.getSelectedProjects()[0];
            KalypsoResourceSelectionDialog dialog = new KalypsoResourceSelectionDialog( getShell(), project, "Ausw�hlen der Geometry f�r den R�umlichen Filter", new String[] { "shp", "gml" }, project, new ResourceSelectionValidator() );
            int open = dialog.open();
            if( open == Window.OK )
            {
              IPath result = (IPath) dialog.getResult()[0];
              // if( result.getFileExtension().equals( "shp" ) )
              // ;
              // if( result.getFileExtension().equals( "gml" ) )
              // ;
              System.out.println( "test geometry aus file " + result );
            }
          }
        }
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );

      }
    } );

    if( propertyName != null && propertyName.trim().length() > 0 )
      propertyNameCombo.select( labelStringItems.indexOf( propertyName ) );

    Button addButton = new Button( configureGroup, SWT.NULL );
    addButton.setText( MessageBundle.STYLE_EDITOR_SET );
    if( labelStringItems.size() == 0 )
      addButton.setEnabled( false );
    if( labelStringItems.size() == 0 )
      addButton.setEnabled( false );
    if( labelStringItems.size() == 0 )
      addButton.setEnabled( false );
    addButton.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        AbstractData addata = null;
        int index = propertyNameCombo.getSelectionIndex();
        if( subType != FilterDialogTreeNode.SPATIAL_CROSSES && subType != FilterDialogTreeNode.SPATIAL_OVERLAPS && subType != FilterDialogTreeNode.SPATIAL_TOUCHES )
        {
          addata = new BinarySpatialData();
          if( index >= 0 && index < items.length )
            ((BinarySpatialData) addata).setGeometryPropertyName( items[index] );
          ((BinarySpatialData) addata).setGeomType( GeometryFactory.createGM_Point( 120, 200, KalypsoGisPlugin.getDefault().getCoordinatesSystem() ) );
        }
        else
        {
          MessageDialog.openError( getShell(), "Filter Fehler", "Diese gew�nschete Operation (" + OperationDefines.getNameById( subType ) + ") ist nicht verf�gbar!" );
          return;
        }

        boolean validInput = false;
        try
        {
          validInput = addata.verify();
          if( validInput && ((m_drawGeomSelection && !m_loadGeomSelection) || (!m_drawGeomSelection && m_loadGeomSelection)) )
          {
            getCurrentNode().setData( addata );
            setFilterInvalid();
          }
          getErrorLabel().setText( "" );
        }
        catch( FilterDialogException e1 )
        {
          getErrorLabel().setText( e1.getError().getFaultCode() );
          if( getCurrentNode().getData() != null )
          {
            // TODO was passiert hier
            System.out.println( "Error !!!" );
          }
        }
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );

    innerConfigureComposite.pack( true );
    globalConfigureComposite.setVisible( true );
  }

  private void drawFeatureIdTypeGroup( FeatureIDData data )
  {
    if( innerConfigureComposite != null )
      innerConfigureComposite.dispose();
    innerConfigureComposite = new Composite( globalConfigureComposite, SWT.NULL );
    innerConfigureComposite.setLayout( new GridLayout() );
    GridData formData = new GridData( 224, 127 );
    innerConfigureComposite.setLayoutData( formData );

    // **** Configuration
    configureGroup = new Group( innerConfigureComposite, SWT.NULL );
    configureGroup.setLayout( new GridLayout( 2, false ) );
    configureGroup.setText( MessageBundle.STYLE_EDITOR_FILTER_CONFIG_FILTER );
    GridData groupCompositeData = new GridData();
    groupCompositeData.widthHint = 200;
    groupCompositeData.heightHint = 100;
    configureGroup.setLayoutData( groupCompositeData );

    Label propertyLabel = new Label( configureGroup, SWT.NULL );
    propertyLabel.setText( MessageBundle.STYLE_EDITOR_FILTER_FEATURE_ID );
    final Text featureIdText = new Text( configureGroup, SWT.BORDER );
    featureIdText.setBackground( new org.eclipse.swt.graphics.Color( null, new RGB( 255, 255, 255 ) ) );
    GridData textData = new GridData();
    textData.widthHint = 90;
    textData.heightHint = 10;
    featureIdText.setLayoutData( textData );

    if( data != null && data.getFeatureId() != null )
    {
      featureIdText.setText( data.getFeatureId() );
    }

    Button addButton = new Button( configureGroup, SWT.NULL );
    addButton.setText( MessageBundle.STYLE_EDITOR_SET );
    addButton.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        FeatureIDData fiddata = new FeatureIDData();
        fiddata.setFeatureId( featureIdText.getText() );
        boolean validInput = false;
        try
        {
          validInput = fiddata.verify();
          if( validInput )
          {
            getCurrentNode().setData( fiddata );
            setFilterInvalid();
          }
          getErrorLabel().setText( MessageBundle.STYLE_EDITOR_SET );
        }
        catch( FilterDialogException e1 )
        {
          getErrorLabel().setText( e1.getError().getFaultCode() );
          if( getCurrentNode().getData() != null )
            featureIdText.setText( ((FeatureIDData) getCurrentNode().getData()).getFeatureId() );
        }
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );

    innerConfigureComposite.pack( true );
    globalConfigureComposite.setVisible( true );

  }

  private void drawPropertyIsNullTypeGroup( NullComparisonData data )
  {
    if( innerConfigureComposite != null )
      innerConfigureComposite.dispose();
    innerConfigureComposite = new Composite( globalConfigureComposite, SWT.NULL );
    innerConfigureComposite.setLayout( new GridLayout() );
    GridData formData = new GridData( 224, 127 );
    innerConfigureComposite.setLayoutData( formData );

    // **** Configuration
    configureGroup = new Group( innerConfigureComposite, SWT.NULL );
    configureGroup.setLayout( new GridLayout( 2, false ) );
    configureGroup.setText( MessageBundle.STYLE_EDITOR_FILTER_CONFIG_FILTER );
    GridData groupCompositeData = new GridData();
    groupCompositeData.widthHint = 200;
    groupCompositeData.heightHint = 100;
    configureGroup.setLayoutData( groupCompositeData );

    Label propertyLabel = new Label( configureGroup, SWT.NULL );
    propertyLabel.setText( MessageBundle.STYLE_EDITOR_PROPERTY );
    final Combo propertyNameCombo = new Combo( configureGroup, SWT.NULL );
    GridData propertyNameComboData = new GridData( 75, 30 );
    propertyNameCombo.setLayoutData( propertyNameComboData );
    // get all PropertyNames to use for filter
    ArrayList<String> labelStringItems = new ArrayList<String>();
    IPropertyType[] ftp = featureType.getProperties();
    for( int i = 0; i < ftp.length; i++ )
      if( !GeometryUtilities.isGeometry( ftp[i] ) )
        labelStringItems.add( ftp[i].getName() );
    final String[] items = new String[labelStringItems.size()];
    for( int j = 0; j < items.length; j++ )
      items[j] = labelStringItems.get( j );
    propertyNameCombo.setItems( items );
    propertyNameCombo.setText( "..." );

    if( data != null && data.getPropertyName() != null )
    {
      propertyNameCombo.select( labelStringItems.indexOf( data.getPropertyName() ) );
    }

    Button addButton = new Button( configureGroup, SWT.NULL );
    addButton.setText( MessageBundle.STYLE_EDITOR_SET );
    if( labelStringItems.size() == 0 )
      addButton.setEnabled( false );
    addButton.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        NullComparisonData ncdata = new NullComparisonData();
        int index = propertyNameCombo.getSelectionIndex();
        if( index >= 0 && index < items.length )
          ncdata.setPropertyName( items[index] );
        boolean validInput = false;
        try
        {
          validInput = ncdata.verify();
          if( validInput )
          {
            getCurrentNode().setData( ncdata );
            setFilterInvalid();
          }
          getErrorLabel().setText( "" );
        }
        catch( FilterDialogException e1 )
        {
          getErrorLabel().setText( e1.getError().getFaultCode() );
        }
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );

    innerConfigureComposite.pack( true );
    globalConfigureComposite.setVisible( true );
  }

  private void drawBinaryComparisonOpTypeGroup( AbstractData data, final int subType )
  {
    String propertyName = null;
    String literal = null;

    if( data instanceof BinaryComparisonData )
    {
      propertyName = ((BinaryComparisonData) data).getPropertyName();
      literal = ((BinaryComparisonData) data).getLiteral();
    }
    else if( data instanceof BinaryComparisonNumericData )
    {
      propertyName = ((BinaryComparisonNumericData) data).getPropertyName();
      literal = ((BinaryComparisonNumericData) data).getLiteral();
    }

    if( innerConfigureComposite != null )
      innerConfigureComposite.dispose();
    innerConfigureComposite = new Composite( globalConfigureComposite, SWT.NULL );
    innerConfigureComposite.setLayout( new GridLayout() );
    GridData formData = new GridData( 224, 127 );
    innerConfigureComposite.setLayoutData( formData );

    // **** Configuration
    configureGroup = new Group( innerConfigureComposite, SWT.NULL );
    configureGroup.setLayout( new GridLayout( 2, false ) );
    configureGroup.setText( MessageBundle.STYLE_EDITOR_FILTER_CONFIG_FILTER );
    GridData groupCompositeData = new GridData();
    groupCompositeData.widthHint = 200;
    groupCompositeData.heightHint = 100;
    configureGroup.setLayoutData( groupCompositeData );

    Label propertyLabel = new Label( configureGroup, SWT.NULL );
    propertyLabel.setText( MessageBundle.STYLE_EDITOR_PROPERTY );
    final Combo propertyNameCombo = new Combo( configureGroup, SWT.NULL );
    GridData propertyNameComboData = new GridData( 75, 30 );
    propertyNameCombo.setLayoutData( propertyNameComboData );
    // get all PropertyNames to use for filter
    ArrayList<String> labelStringItems = new ArrayList<String>();
    final IPropertyType[] ftp = featureType.getProperties();
    for( int i = 0; i < ftp.length; i++ )
    {
      // if only numeric types should be selectable
      final String name = ftp[i].getName();
      if( subType != FilterDialogTreeNode.COMPARISON_EQUALTO )
      {
        if( ftp[i] instanceof IValuePropertyType )
        {
          final IValuePropertyType vpt = (IValuePropertyType) ftp[i];
          final Class valueClass = vpt.getValueClass();
          if( valueClass == Double.class )
            labelStringItems.add( name );
          else if( valueClass == BigInteger.class )
            labelStringItems.add( name );
          else if( valueClass == Byte.class )
            labelStringItems.add( name );
          else if( valueClass == BigDecimal.class )
            labelStringItems.add( name );
          else if( valueClass == Float.class )
            labelStringItems.add( name );
          else if( valueClass == Integer.class )
            labelStringItems.add( name );
          else if( valueClass == Long.class )
            labelStringItems.add( name );
          else if( valueClass == Short.class )
            labelStringItems.add( name );
        }
      }
      // any type except for a geometry object
      else
      {
        if( !GeometryUtilities.isGeometry( ftp[i] ) )
          labelStringItems.add( name );
      }
    }
    final String[] items = new String[labelStringItems.size()];
    for( int j = 0; j < items.length; j++ )
      items[j] = labelStringItems.get( j );
    propertyNameCombo.setItems( items );
    propertyNameCombo.setText( "..." );

    Label literalLabel = new Label( configureGroup, SWT.NULL );
    literalLabel.setText( MessageBundle.STYLE_EDITOR_FILTER_VALUE );
    final Text literalText = new Text( configureGroup, SWT.BORDER );
    literalText.setBackground( new org.eclipse.swt.graphics.Color( null, new RGB( 255, 255, 255 ) ) );
    GridData textData = new GridData();
    textData.widthHint = 90;
    textData.heightHint = 10;
    literalText.setLayoutData( textData );

    if( propertyName != null && propertyName.trim().length() > 0 )
      propertyNameCombo.select( labelStringItems.indexOf( propertyName ) );
    if( literal != null && literal.trim().length() > 0 )
      literalText.setText( literal );

    Button addButton = new Button( configureGroup, SWT.NULL );
    addButton.setText( MessageBundle.STYLE_EDITOR_SET );
    if( labelStringItems.size() == 0 )
      addButton.setEnabled( false );
    addButton.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        AbstractData addata = null;
        int index = propertyNameCombo.getSelectionIndex();
        if( subType == FilterDialogTreeNode.COMPARISON_EQUALTO )
        {
          addata = new BinaryComparisonData();
          if( index >= 0 && index < items.length )
            ((BinaryComparisonData) addata).setPropertyName( items[index] );
          ((BinaryComparisonData) addata).setLiteral( literalText.getText() );
        }
        else
        {
          addata = new BinaryComparisonNumericData();
          if( index >= 0 && index < items.length )
            ((BinaryComparisonNumericData) addata).setPropertyName( items[index] );
          ((BinaryComparisonNumericData) addata).setLiteral( literalText.getText() );
        }

        boolean validInput = false;
        try
        {
          validInput = addata.verify();
          if( validInput )
          {
            getCurrentNode().setData( addata );
            setFilterInvalid();
          }
          getErrorLabel().setText( "" );
        }
        catch( FilterDialogException e1 )
        {
          getErrorLabel().setText( e1.getError().getFaultCode() );
          if( getCurrentNode().getData() != null )
          {
            if( subType == FilterDialogTreeNode.COMPARISON_EQUALTO )
              literalText.setText( ((BinaryComparisonData) getCurrentNode().getData()).getLiteral() );
            else
              literalText.setText( ((BinaryComparisonNumericData) getCurrentNode().getData()).getLiteral() );
          }
        }
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );

    innerConfigureComposite.pack( true );
    globalConfigureComposite.setVisible( true );
  }

  private void drawPropertyIsLikeTypeGroup( LikeComparisonData data )
  {
    if( innerConfigureComposite != null )
      innerConfigureComposite.dispose();
    innerConfigureComposite = new Composite( globalConfigureComposite, SWT.NULL );
    innerConfigureComposite.setLayout( new GridLayout() );
    GridData formData = new GridData( 224, 127 );
    innerConfigureComposite.setLayoutData( formData );

    // **** Configuration
    configureGroup = new Group( innerConfigureComposite, SWT.NULL );
    configureGroup.setLayout( new GridLayout( 2, false ) );
    configureGroup.setText( MessageBundle.STYLE_EDITOR_FILTER_CONFIG_FILTER );
    GridData groupCompositeData = new GridData();
    groupCompositeData.widthHint = 200;
    groupCompositeData.heightHint = 100;
    configureGroup.setLayoutData( groupCompositeData );

    Label propertyLabel = new Label( configureGroup, SWT.NULL );
    propertyLabel.setText( MessageBundle.STYLE_EDITOR_PROPERTY );
    final Combo propertyNameCombo = new Combo( configureGroup, SWT.NULL );
    GridData propertyNameComboData = new GridData( 75, 30 );
    propertyNameCombo.setLayoutData( propertyNameComboData );
    // get all PropertyNames to use for filter
    ArrayList<String> labelStringItems = new ArrayList<String>();
    IPropertyType[] ftp = featureType.getProperties();
    for( int i = 0; i < ftp.length; i++ )
      if( !GeometryUtilities.isGeometry( ftp[i] ) )
        labelStringItems.add( ftp[i].getName() );
    final String[] items = new String[labelStringItems.size()];
    for( int j = 0; j < items.length; j++ )
      items[j] = labelStringItems.get( j );
    propertyNameCombo.setItems( items );
    propertyNameCombo.setText( "..." );

    Label literalLabel = new Label( configureGroup, SWT.NULL );
    literalLabel.setText( MessageBundle.STYLE_EDITOR_FILTER_VALUE );
    final Text literalText = new Text( configureGroup, SWT.BORDER );
    literalText.setBackground( new org.eclipse.swt.graphics.Color( null, new RGB( 255, 255, 255 ) ) );
    GridData textData = new GridData();
    textData.widthHint = 90;
    textData.heightHint = 10;
    literalText.setLayoutData( textData );

    if( data != null && data.getPropertyName() != null )
    {
      propertyNameCombo.select( labelStringItems.indexOf( data.getPropertyName() ) );
    }
    if( data != null && data.getLiteral() != null )
    {
      literalText.setText( data.getLiteral() );
    }
    if( data == null )
      data = new LikeComparisonData();
    Label wildCard = new Label( configureGroup, SWT.NULL );
    wildCard.setText( MessageBundle.STYLE_EDITOR_FILTER_WILDCARD + "" + data.getWildCard() + "'" );
    Label otherInfo = new Label( configureGroup, SWT.NULL );
    otherInfo.setText( MessageBundle.STYLE_EDITOR_FILTER_SINGLECHAR + data.getSingleChar() + MessageBundle.STYLE_EDITOR_FILTER_ESCAPE + data.getEscapeChar() + "'" );

    Button addButton = new Button( configureGroup, SWT.NULL );
    addButton.setText( MessageBundle.STYLE_EDITOR_SET );
    if( labelStringItems.size() == 0 )
      addButton.setEnabled( false );
    addButton.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        LikeComparisonData lcdata = new LikeComparisonData();
        int index = propertyNameCombo.getSelectionIndex();
        if( index >= 0 && index < items.length )
          lcdata.setPropertyName( items[index] );
        lcdata.setLiteral( literalText.getText() );
        boolean validInput = false;
        try
        {
          validInput = lcdata.verify();
          if( validInput )
          {
            getCurrentNode().setData( lcdata );
            setFilterInvalid();
          }
          getErrorLabel().setText( "" );
        }
        catch( FilterDialogException e1 )
        {
          getErrorLabel().setText( e1.getError().getFaultCode() );
          if( getCurrentNode().getData() != null )
            literalText.setText( ((LikeComparisonData) getCurrentNode().getData()).getLiteral() );
        }
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );

    innerConfigureComposite.pack( true );
    globalConfigureComposite.setVisible( true );
  }

  private void drawPropertyIsBetweenTypeGroup( BetweenComparisonData data )
  {
    if( innerConfigureComposite != null )
      innerConfigureComposite.dispose();
    innerConfigureComposite = new Composite( globalConfigureComposite, SWT.NULL );
    innerConfigureComposite.setLayout( new GridLayout() );
    GridData formData = new GridData( 224, 127 );
    innerConfigureComposite.setLayoutData( formData );

    // **** Configuration
    configureGroup = new Group( innerConfigureComposite, SWT.NULL );
    configureGroup.setLayout( new GridLayout( 2, false ) );
    configureGroup.setText( MessageBundle.STYLE_EDITOR_FILTER_CONFIG_FILTER );
    GridData groupCompositeData = new GridData();
    groupCompositeData.widthHint = 200;
    groupCompositeData.heightHint = 100;
    configureGroup.setLayoutData( groupCompositeData );

    Label propertyLabel = new Label( configureGroup, SWT.NULL );
    propertyLabel.setText( MessageBundle.STYLE_EDITOR_PROPERTY );
    final Combo propertyNameCombo = new Combo( configureGroup, SWT.NULL );
    GridData propertyNameComboData = new GridData( 75, 30 );
    propertyNameCombo.setLayoutData( propertyNameComboData );
    // get all PropertyNames to use for filter
    ArrayList<String> labelStringItems = new ArrayList<String>();
    IPropertyType[] ftp = featureType.getProperties();
    for( int i = 0; i < ftp.length; i++ )
    {
      if( ftp[i] instanceof IValuePropertyType )
      {
        final IValuePropertyType vpt = (IValuePropertyType) ftp[i];
        final Class valueClass = vpt.getValueClass();
        final String name = ftp[i].getName();
        if( valueClass == Double.class )
          labelStringItems.add( name );
        else if( valueClass == BigInteger.class )
          labelStringItems.add( name );
        else if( valueClass == Byte.class )
          labelStringItems.add( name );
        else if( valueClass == BigDecimal.class )
          labelStringItems.add( name );
        else if( valueClass == Float.class )
          labelStringItems.add( name );
        else if( valueClass == Integer.class )
          labelStringItems.add( name );
        else if( valueClass == Long.class )
          labelStringItems.add( name );
        else if( valueClass == Short.class )
          labelStringItems.add( name );
      }
    }
    final String[] items = new String[labelStringItems.size()];
    for( int j = 0; j < items.length; j++ )
      items[j] = labelStringItems.get( j );
    propertyNameCombo.setItems( items );
    propertyNameCombo.setText( "..." );
    Label literalLabel = new Label( configureGroup, SWT.NULL );
    literalLabel.setText( MessageBundle.STYLE_EDITOR_LOWER_BOUNDARY );
    final Text literalText = new Text( configureGroup, SWT.BORDER );
    literalText.setBackground( new org.eclipse.swt.graphics.Color( null, new RGB( 255, 255, 255 ) ) );
    GridData textData = new GridData();
    textData.widthHint = 90;
    textData.heightHint = 10;
    literalText.setLayoutData( textData );

    Label literalLabel2 = new Label( configureGroup, SWT.NULL );
    literalLabel2.setText( MessageBundle.STYLE_EDITOR_UPPER_BOUNDARY );
    final Text literalText2 = new Text( configureGroup, SWT.BORDER );
    literalText2.setBackground( new org.eclipse.swt.graphics.Color( null, new RGB( 255, 255, 255 ) ) );
    GridData textData2 = new GridData();
    textData2.widthHint = 90;
    textData2.heightHint = 10;
    literalText2.setLayoutData( textData2 );

    if( data != null && data.getPropertyName() != null )
    {
      propertyNameCombo.select( labelStringItems.indexOf( data.getPropertyName() ) );
    }
    if( data != null && data.getLower() != null )
    {
      literalText.setText( data.getLower() );
    }
    if( data != null && data.getUpper() != null )
    {
      literalText2.setText( data.getUpper() );
    }

    Button addButton = new Button( configureGroup, SWT.NULL );
    addButton.setText( MessageBundle.STYLE_EDITOR_SET );
    if( labelStringItems.size() == 0 )
      addButton.setEnabled( false );
    addButton.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        BetweenComparisonData bcdata = new BetweenComparisonData();
        int index = propertyNameCombo.getSelectionIndex();
        if( index >= 0 && index < items.length )
          bcdata.setPropertyName( items[index] );
        bcdata.setLower( literalText.getText() );
        bcdata.setUpper( literalText2.getText() );
        boolean validInput = false;
        try
        {
          validInput = bcdata.verify();
          if( validInput )
          {
            getCurrentNode().setData( bcdata );
            setFilterInvalid();
          }
          getErrorLabel().setText( "" );
        }
        catch( FilterDialogException e1 )
        {
          getErrorLabel().setText( e1.getError().getFaultCode() );
          if( getCurrentNode().getData() != null )
          {
            literalText.setText( ((BetweenComparisonData) getCurrentNode().getData()).getLower() );
            literalText2.setText( ((BetweenComparisonData) getCurrentNode().getData()).getUpper() );
          }
        }
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );

    innerConfigureComposite.pack( true );
    globalConfigureComposite.setVisible( true );
  }

  private void createOGCFilterPanel( Composite top )
  {
    // ----- OGC Filter
    GridData ogcGroupData = new GridData();
    ogcGroupData.widthHint = 235;
    ogcGroupData.heightHint = 100;
    Group ogcFilter = new Group( top, SWT.NULL );
    ogcFilter.setLayout( new GridLayout( 3, false ) );
    ogcFilter.setText( MessageBundle.STYLE_EDITOR_FILTER_OGC );
    ogcFilter.setLayoutData( ogcGroupData );
    // ++++ Logical filter line
    Label logicalLabel = new Label( ogcFilter, SWT.NULL );
    logicalLabel.setText( MessageBundle.STYLE_EDITOR_FILTER_LOGICAL );
    logicalCombo = new LogicalFilterComboPanel( ogcFilter );
    logicalButton = new Label( ogcFilter, SWT.NULL );
    logicalButton.setImage( ImageProvider.IMAGE_STYLEEDITOR_ADD_RULE.createImage() );
    logicalButton.setToolTipText( MessageBundle.STYLE_EDITOR_ADD );
    logicalButton.addMouseListener( new MouseListener()
    {
      public void mouseDoubleClick( MouseEvent e )
      {
        FilterDialogTreeNode childNode = new FilterDialogTreeNode( getLogicalCombo().getSelectionName( getLogicalCombo().getSelection() ), FilterDialogTreeNode.LOGICAL_NODE_TYPE );
        getCurrentNode().addNode( childNode );
        getM_viewer().expandAll();
        if( childNode != null )
          getM_viewer().setSelection( new StructuredSelection( childNode ) );
        getM_viewer().refresh( true );
        setFilterInvalid();
      }

      public void mouseDown( MouseEvent e )
      {
        mouseDoubleClick( e );
      }

      public void mouseUp( MouseEvent e )
      {
        // nothing
      }
    } );

    // ++++ Comparison filter line
    Label comparisonLabel = new Label( ogcFilter, SWT.NULL );
    comparisonLabel.setText( MessageBundle.STYLE_EDITOR_FILTER_COMPARISON );
    comparisonCombo = new ComparisonFilterComboPanel( ogcFilter );
    compButton = new Label( ogcFilter, SWT.NULL );
    compButton.setImage( ImageProvider.IMAGE_STYLEEDITOR_ADD_RULE.createImage() );
    compButton.setToolTipText( MessageBundle.STYLE_EDITOR_ADD );
    compButton.addMouseListener( new MouseListener()
    {
      public void mouseDoubleClick( MouseEvent e )
      {
        FilterDialogTreeNode childNode = new FilterDialogTreeNode( getComparisonCombo().getSelectionName( getComparisonCombo().getSelection() ), FilterDialogTreeNode.COMPARISON_NODE_TYPE );
        getCurrentNode().addNode( childNode );
        getM_viewer().expandAll();
        if( getCurrentNode() != null )
          getM_viewer().setSelection( new StructuredSelection( getCurrentNode() ) );
        getM_viewer().refresh( true );
        setFilterInvalid();
      }

      public void mouseDown( MouseEvent e )
      {
        mouseDoubleClick( e );
      }

      public void mouseUp( MouseEvent e )
      {
        // nothing
      }
    } );

    // ++++ Spatial filter line
    Label spatialFilterLabel = new Label( ogcFilter, SWT.NULL );
    spatialFilterLabel.setText( MessageBundle.STYLE_EDITOR_FILTER_SPATIAL );
    m_spatialCombo = new SpatialOperationPanel( ogcFilter );
    spatialButton = new Label( ogcFilter, SWT.NULL );
    spatialButton.setImage( ImageProvider.IMAGE_STYLEEDITOR_ADD_RULE.createImage() );
    spatialButton.setToolTipText( MessageBundle.STYLE_EDITOR_ADD );
    spatialButton.addMouseListener( new MouseListener()
    {
      public void mouseDoubleClick( MouseEvent e )
      {
        FilterDialogTreeNode childNode = new FilterDialogTreeNode( getSpatialCombo().getSelectionName( getSpatialCombo().getSelection() ), FilterDialogTreeNode.SPATIAL_NODE_TYPE );
        getCurrentNode().addNode( childNode );
        getM_viewer().expandAll();
        if( getCurrentNode() != null )
          getM_viewer().setSelection( new StructuredSelection( getCurrentNode() ) );
        getM_viewer().refresh( true );
        setFilterInvalid();
      }

      public void mouseDown( MouseEvent e )
      {
        mouseDoubleClick( e );
      }

      public void mouseUp( MouseEvent e )
      {
        // nothing
      }
    } );

    // ++++ Comparison FilterFilter line
    Label featureFilterLabel = new Label( ogcFilter, SWT.NULL );
    featureFilterLabel.setText( MessageBundle.STYLE_EDITOR_FILTER_FEATURE );
    featureIdButton = new Label( ogcFilter, SWT.NULL );
    featureIdButton.setImage( ImageProvider.IMAGE_STYLEEDITOR_ADD_RULE.createImage() );
    featureIdButton.setToolTipText( MessageBundle.STYLE_EDITOR_ADD );
    featureIdButton.addMouseListener( new MouseListener()
    {
      public void mouseDoubleClick( MouseEvent e )
      {
        FilterDialogTreeNode childNode = new FilterDialogTreeNode( "FT_ID", FilterDialogTreeNode.FEATUREID_NODE_TYPE );
        getCurrentNode().addNode( childNode );
        getM_viewer().expandAll();
        if( getCurrentNode() != null )
          getM_viewer().setSelection( new StructuredSelection( getCurrentNode() ) );
        getM_viewer().refresh( true );
        setFilterInvalid();
      }

      public void mouseDown( MouseEvent e )
      {
        mouseDoubleClick( e );
      }

      public void mouseUp( MouseEvent e )
      {
        // nothing
      }
    } );
  }

  private void createSLDFilterPanel( Composite top )
  {
    // ----- SLD Filter
    Group sldFilter = new Group( top, SWT.NULL );
    GridData sldGroupData = new GridData();
    sldGroupData.widthHint = 210;
    sldGroupData.heightHint = 100;
    sldFilter.setText( MessageBundle.STYLE_EDITOR_FILTER_SLD );
    sldFilter.setLayoutData( sldGroupData );
    sldFilter.setLayout( new GridLayout( 2, false ) );
    // ++++ Comparison ElseFilter line
    Label elseFilterLabel = new Label( sldFilter, SWT.NULL );
    elseFilterLabel.setText( MessageBundle.STYLE_EDITOR_FILTER_ELSE );
    elseFilterButton = new Label( sldFilter, SWT.NULL );
    elseFilterButton.setImage( ImageProvider.IMAGE_STYLEEDITOR_ADD_RULE.createImage() );
    elseFilterButton.setToolTipText( MessageBundle.STYLE_EDITOR_ADD );
    elseFilterButton.addMouseListener( new MouseListener()
    {
      public void mouseDoubleClick( MouseEvent e )
      {
        FilterDialogTreeNode childNode = new FilterDialogTreeNode( "ElseFilter", FilterDialogTreeNode.ELSEFILTER_TYPE );
        getCurrentNode().addNode( childNode );
        getM_viewer().expandAll();
        if( getCurrentNode() != null )
          getM_viewer().setSelection( new StructuredSelection( getCurrentNode() ) );
        getM_viewer().refresh( true );
        setFilterInvalid();
      }

      public void mouseDown( MouseEvent e )
      {
        mouseDoubleClick( e );
      }

      public void mouseUp( MouseEvent e )
      {
        // nothing
      }
    } );
  }

  Filter getFilter( )
  {
    return returnFilter;
  }

  void setFilterInvalid( )
  {
    isValidated = false;
    validateFilterButton.setText( MessageBundle.STYLE_EDITOR_FILTER_VALIDATE );
    returnFilter = null;
  }

  protected void fire( )
  {
    Object[] listeners = listenerList.getListenerList();
    for( int i = listeners.length - 2; i >= 0; i -= 2 )
    {
      if( listeners[i] == FilterDialogListener.class )
      {
        FilterDialogEvent event = new FilterDialogEvent( this );
        ((FilterDialogListener) listeners[i + 1]).filterUpdated( event );
      }
    }
  }

  public Filter getReturnFilter( )
  {
    return returnFilter;
  }

  public void setReturnFilter( Filter m_returnFilter )
  {
    this.returnFilter = m_returnFilter;
  }

  public Button getValidateFilterButton( )
  {
    return validateFilterButton;
  }

  public void setValidateFilterButton( Button m_validateFilterButton )
  {
    this.validateFilterButton = m_validateFilterButton;
  }

  public Label getErrorLabel( )
  {
    return errorLabel;
  }

  public void setErrorLabel( Label m_errorLabel )
  {
    this.errorLabel = m_errorLabel;
  }

  public boolean isValidated( )
  {
    return isValidated;
  }

  public void setValidated( boolean m_isValidated )
  {
    this.isValidated = m_isValidated;
  }

  public FilterDialogTreeNode getMRoot( )
  {
    return mRoot;
  }

  public void setMRoot( FilterDialogTreeNode root )
  {
    mRoot = root;
  }

  public ComparisonFilterComboPanel getComparisonCombo( )
  {
    return comparisonCombo;
  }

  public void setComparisonCombo( ComparisonFilterComboPanel m_comparisonCombo )
  {
    this.comparisonCombo = m_comparisonCombo;
  }

  public SpatialOperationPanel getSpatialCombo( )
  {
    return m_spatialCombo;
  }

  public void setSpatialCombo( SpatialOperationPanel spatialCombo )
  {
    this.m_spatialCombo = spatialCombo;
  }

  public TableTreeViewer getM_viewer( )
  {
    return m_viewer;
  }

  public void setM_viewer( TableTreeViewer m_m_viewer )
  {
    this.m_viewer = m_m_viewer;
  }

  public FilterDialogTreeNode getCurrentNode( )
  {
    return currentNode;
  }

  public void setCurrentNode( FilterDialogTreeNode m_currentNode )
  {
    this.currentNode = m_currentNode;
  }

  public LogicalFilterComboPanel getLogicalCombo( )
  {
    return logicalCombo;
  }

  public void setLogicalCombo( LogicalFilterComboPanel m_logicalCombo )
  {
    this.logicalCombo = m_logicalCombo;
  }

  public IFeatureType getFeatureType( )
  {
    return featureType;
  }

  public void setFeatureType( IFeatureType m_featureType )
  {
    this.featureType = m_featureType;
  }

  public Rule getRule( )
  {
    return rule;
  }

  public void setRule( Rule m_rule )
  {
    this.rule = m_rule;
  }

  protected class MenuAction extends Action
  {
    public MenuAction( String text )
    {
      super( text );
    }

    @Override
    public void run( )
    {
      getErrorLabel().setText( "" );
      setFilterInvalid();
      IStructuredSelection selection = (IStructuredSelection) getM_viewer().getSelection();

      if( selection != null )
      {
        Object selectedElement = selection.getFirstElement();
        FilterDialogTreeNode node = (FilterDialogTreeNode) selectedElement;
        if( node.getType() == FilterDialogTreeNode.ROOT_TYPE )
        {
          Object[] children = node.getChildren();
          for( int i = 0; i < children.length; i++ )
            node.removeNode( (FilterDialogTreeNode) children[i] );
          setCurrentNode( node );
        }
        else
        {
          FilterDialogTreeNode parent = node.getParent();
          if( parent != null )
            parent.removeNode( node );
          setCurrentNode( parent );
        }
        getM_viewer().setInput( getMRoot() );
        if( getCurrentNode() != null )
          getM_viewer().setSelection( new StructuredSelection( getCurrentNode() ) );
        getM_viewer().expandAll();
      }
    }
  }

}